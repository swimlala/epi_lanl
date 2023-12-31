CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-05-07T15:38:22Z creation;2020-05-07T15:38:26Z conversion to V3.1;2022-11-21T05:27:07Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20200507153822  20221123114512  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_209                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @��9D�1   @����� @;��ߤ�dy��ڹ�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?fD?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @$z�@qG�@���@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>��D?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH��DIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��yA��A��A��A��A��A��/A��RA�bNA���A�ffA�?}A���A�bA���A�jA�=qA�;dA�/A�A��#A�ȴA���A���A���A��RA�(�A���A�\)A��yA��#A��TA�33A���A���A��+A�  A���A��FA��uA�I�A��
A�33A�^5A�=qA��mA�;dA�ƨA�ZA���A�dZA�
=A�
=A�bNA���A��
A���A�`BA���A�x�A� �A�(�A�dZA�hsA���A�&�A��A��;A��!A��A�ȴA�$�A���A��;A�t�A��;A�r�A�=qA��A��A�`BA��A��A���A��A�v�A�XA�JA�t�A�5?A��HA��A��uA|�DAx�+Aw�7Av�RAux�At��Atn�Aq��AmXAl �Ak�7AkAjz�AjbNAjM�Aj�Ai&�Ah^5AgXAex�Ad�!Ad-AcAc"�Ab�jAb{A`1A]G�A[��A[
=AY�^AX�/AW&�AV��AV9XAU�FAS�AQK�AP��AO�#AM�^AK��AJbNAH��AG�AF��AFĜAF�AE��AE��AE/AD^5ACVABv�AA��A@��A?t�A> �A=�A=�TA=�FA=�A=?}A<�/A<�A:�yA9t�A7l�A6ȴA65?A5�A5��A5p�A5;dA5VA4$�A2��A2�DA1%A/ƨA/��A/�7A/;dA.r�A-�A,ĜA,=qA+��A+�FA+��A+K�A*r�A)7LA(�A'S�A&�RA&A%?}A$A#S�A"��A!��A!XA jAAhsA��A1'A�^A�PA�A��A��Az�A1'A  A��Al�A�#A�AK�A~�A�#A�!An�AM�A�FA��A?}A��Az�A��A�A�RA�A|�A
�HA
^5A	�7A�\A�A�PA�A�A �A��A1Av�A=qA��A&�A �\A �@��m@���@���@���@�$�@���@��h@�`B@��j@���@���@�7L@��/@�ƨ@�!@� �@��@���@�&�@�ƨ@�33@�\@��#@�p�@�u@�+@��@�h@�j@�\)@�E�@�X@ߥ�@�@�1@�ȴ@ى7@��;@��y@�ff@պ^@���@���@җ�@�V@�E�@�@�hs@Ͼw@͑h@̼j@�A�@�l�@�@ɡ�@��@ǍP@��@�-@���@Õ�@§�@�`B@�z�@��!@�z�@�|�@���@��@���@��@��@��;@��@�M�@�A�@�~�@��-@�j@���@��^@�hs@��@��@���@��R@��@�`B@�(�@��+@�M�@��@�`B@��@��F@��R@�{@�O�@���@�1'@�  @���@���@�$�@�hs@���@�A�@��@�l�@�C�@���@�E�@�x�@�r�@�9X@��;@��@�33@���@���@��j@�bN@�9X@�1@�|�@���@�{@��@��-@��7@�?}@�%@�j@��m@���@�|�@�K�@�"�@�
=@���@�@��@�/@�A�@�1@��m@��
@��
@�ƨ@��@���@�S�@���@�ȴ@��+@�M�@�$�@��@��T@��-@�x�@��@�z�@�bN@�I�@��@��F@�|�@��@���@�x�@�x�@��@��@��@�x�@�x�@�p�@��@��@�Ĝ@�bN@�I�@�Q�@�Q�@�A�@�1'@� �@�b@��
@�{@���@��^@���@���@��h@�`B@�O�@�/@���@�z�@�A�@�@~�y@}��@}�@|�/@|��@|9X@{�F@{��@{S�@{C�@{"�@{@z~�@zM�@z�@y��@x�@x  @wK�@v�y@v�+@vE�@v{@u�T@u�-@u`B@u�@t�/@t�@tj@t9X@t1@s�@s33@r�@r��@r��@rn�@q�#@qX@qG�@q7L@p�9@pQ�@o�;@o\)@o�@n��@nv�@n5?@m@m�@lj@k�F@kt�@k"�@j�!@jM�@i��@i��@i�7@i%@h��@h�@hb@g|�@g+@f�@fE�@e��@ep�@d��@c��@c��@cC�@co@c@b��@bJ@a��@ax�@`  @_�P@_|�@_+@^ȴ@^v�@^V@^$�@]�h@]?}@\�/@\��@\��@\j@\(�@[��@[S�@[33@["�@[@Z�!@Z-@YG�@XA�@W��@W|�@W\)@V�@V5?@U�T@U��@U�@Up�@Up�@U`B@UV@TI�@S�F@SS�@S@R~�@R=q@R=q@Q�@Q�7@Qhs@Q7L@Q�@Q%@P��@Pr�@PA�@P1'@P  @O��@O��@O�@Nȴ@N5?@M�T@M�@M?}@M�@MV@L��@K��@K�@Kt�@KdZ@K"�@J��@J�\@Jn�@J-@J�@I��@H��@HĜ@H�u@H �@G�w@G�P@G;d@G+@G
=@F��@Fȴ@Fv�@F5?@E�@E��@E��@E�@EO�@E?}@D��@D��@DI�@C��@C��@CdZ@C��@Ct�@B�H@B��@BM�@B�@A�@A�^@A��@A��@@Ĝ@@  @?�w@?\)@>ȴ@>��@>E�@=��@=p�@<�/@<Z@<(�@;��@;�m@;�F@;��@;t�@;dZ@;C�@;"�@:��@:�\@:~�@:M�@:J@9��@9�7@9x�@9hs@9X@9G�@9�@8�9@8bN@8 �@8 �@8  @7�w@7|�@7;d@7+@7
=@6��@6�y@6ȴ@6�+@6@5�T@5@5p�@5/@4�/@4��@3�m@3dZ@333@3"�@3@2��@2��@2-@1��@1G�@0�`@0��@0�u@0�u@0r�@0bN@0Q�@0Q�@01'@0  @/��@/��@/|�@/;d@/�@.�y@.v�@.$�@.{@-�-@-/@,�/@,�@,Z@,�@+�m@+�F@+��@+��@+�@+33@+o@*�@*�H@*n�@)��@)X@(�`@(bN@'�@'�P@'l�@';d@'+@&�y@&��@&ff@&5?@%�T@%@%�-@%O�@$��@$�D@$9X@#��@#dZ@"�@"��@"M�@"�@!��@!�^@!��@!G�@!%@ �u@ bN@  �@   @   @�@��@�@��@�P@�P@;d@��@��@v�@ff@E�@E�@��@`B@?}@V@��@Z@I�@��@ƨ@��@�@dZ@C�@"�@@�H@��@��@^5@-@J@��@��@��@�7@�7@�7@x�@X@�@�`@Ĝ@A�@�w@�P@l�@+@�y@�y@�R@ff@{@��@�-@�h@p�@/@�@�@�D@j@Z@9X@9X@�@�
@��@�@dZ@C�@@��@�!@n�@-@��@��@&�@��@�u@r�@A�@�@��@l�@\)@K�@;d@
=@�y@ȴ@��@��@@@`B@V@��@��@�j@�D@j@I�@�
@��@S�@@
��@
��@
�!@
�\@
M�@
�@	�^@	7L@	&�@	�@��@��@�9@�9@�9@��@�@�@Q�@Q�@A�@1'@ �@b@  @�w@�@�P@|�@\)@K�@;d@+@
=@��@�@�@�@��@v�@ff@5?@�@��@�-@p�@��@�/@��@�@��@��@�D@j@9X@(�@�@��@ƨ@�F@��@t�@S�@C�@33@33@33@"�@�@��@�!@�!@��@��@��@�\@~�@-@�@�@�@�#@��@��@�7@x�@X@7L@�@ ��@ ��@ ��@ �9@ r�@ bN@ bN@ Q�@  �@ b@   @   ?��;?��w?��w?��w?���?�\)?���?��?��?��?��R?��R?���?�v�?�v�?�v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��yA��A��A��A��A��A��/A��RA�bNA���A�ffA�?}A���A�bA���A�jA�=qA�;dA�/A�A��#A�ȴA���A���A���A��RA�(�A���A�\)A��yA��#A��TA�33A���A���A��+A�  A���A��FA��uA�I�A��
A�33A�^5A�=qA��mA�;dA�ƨA�ZA���A�dZA�
=A�
=A�bNA���A��
A���A�`BA���A�x�A� �A�(�A�dZA�hsA���A�&�A��A��;A��!A��A�ȴA�$�A���A��;A�t�A��;A�r�A�=qA��A��A�`BA��A��A���A��A�v�A�XA�JA�t�A�5?A��HA��A��uA|�DAx�+Aw�7Av�RAux�At��Atn�Aq��AmXAl �Ak�7AkAjz�AjbNAjM�Aj�Ai&�Ah^5AgXAex�Ad�!Ad-AcAc"�Ab�jAb{A`1A]G�A[��A[
=AY�^AX�/AW&�AV��AV9XAU�FAS�AQK�AP��AO�#AM�^AK��AJbNAH��AG�AF��AFĜAF�AE��AE��AE/AD^5ACVABv�AA��A@��A?t�A> �A=�A=�TA=�FA=�A=?}A<�/A<�A:�yA9t�A7l�A6ȴA65?A5�A5��A5p�A5;dA5VA4$�A2��A2�DA1%A/ƨA/��A/�7A/;dA.r�A-�A,ĜA,=qA+��A+�FA+��A+K�A*r�A)7LA(�A'S�A&�RA&A%?}A$A#S�A"��A!��A!XA jAAhsA��A1'A�^A�PA�A��A��Az�A1'A  A��Al�A�#A�AK�A~�A�#A�!An�AM�A�FA��A?}A��Az�A��A�A�RA�A|�A
�HA
^5A	�7A�\A�A�PA�A�A �A��A1Av�A=qA��A&�A �\A �@��m@���@���@���@�$�@���@��h@�`B@��j@���@���@�7L@��/@�ƨ@�!@� �@��@���@�&�@�ƨ@�33@�\@��#@�p�@�u@�+@��@�h@�j@�\)@�E�@�X@ߥ�@�@�1@�ȴ@ى7@��;@��y@�ff@պ^@���@���@җ�@�V@�E�@�@�hs@Ͼw@͑h@̼j@�A�@�l�@�@ɡ�@��@ǍP@��@�-@���@Õ�@§�@�`B@�z�@��!@�z�@�|�@���@��@���@��@��@��;@��@�M�@�A�@�~�@��-@�j@���@��^@�hs@��@��@���@��R@��@�`B@�(�@��+@�M�@��@�`B@��@��F@��R@�{@�O�@���@�1'@�  @���@���@�$�@�hs@���@�A�@��@�l�@�C�@���@�E�@�x�@�r�@�9X@��;@��@�33@���@���@��j@�bN@�9X@�1@�|�@���@�{@��@��-@��7@�?}@�%@�j@��m@���@�|�@�K�@�"�@�
=@���@�@��@�/@�A�@�1@��m@��
@��
@�ƨ@��@���@�S�@���@�ȴ@��+@�M�@�$�@��@��T@��-@�x�@��@�z�@�bN@�I�@��@��F@�|�@��@���@�x�@�x�@��@��@��@�x�@�x�@�p�@��@��@�Ĝ@�bN@�I�@�Q�@�Q�@�A�@�1'@� �@�b@��
@�{@���@��^@���@���@��h@�`B@�O�@�/@���@�z�@�A�@�@~�y@}��@}�@|�/@|��@|9X@{�F@{��@{S�@{C�@{"�@{@z~�@zM�@z�@y��@x�@x  @wK�@v�y@v�+@vE�@v{@u�T@u�-@u`B@u�@t�/@t�@tj@t9X@t1@s�@s33@r�@r��@r��@rn�@q�#@qX@qG�@q7L@p�9@pQ�@o�;@o\)@o�@n��@nv�@n5?@m@m�@lj@k�F@kt�@k"�@j�!@jM�@i��@i��@i�7@i%@h��@h�@hb@g|�@g+@f�@fE�@e��@ep�@d��@c��@c��@cC�@co@c@b��@bJ@a��@ax�@`  @_�P@_|�@_+@^ȴ@^v�@^V@^$�@]�h@]?}@\�/@\��@\��@\j@\(�@[��@[S�@[33@["�@[@Z�!@Z-@YG�@XA�@W��@W|�@W\)@V�@V5?@U�T@U��@U�@Up�@Up�@U`B@UV@TI�@S�F@SS�@S@R~�@R=q@R=q@Q�@Q�7@Qhs@Q7L@Q�@Q%@P��@Pr�@PA�@P1'@P  @O��@O��@O�@Nȴ@N5?@M�T@M�@M?}@M�@MV@L��@K��@K�@Kt�@KdZ@K"�@J��@J�\@Jn�@J-@J�@I��@H��@HĜ@H�u@H �@G�w@G�P@G;d@G+@G
=@F��@Fȴ@Fv�@F5?@E�@E��@E��@E�@EO�@E?}@D��@D��@DI�@C��@C��@CdZ@C��@Ct�@B�H@B��@BM�@B�@A�@A�^@A��@A��@@Ĝ@@  @?�w@?\)@>ȴ@>��@>E�@=��@=p�@<�/@<Z@<(�@;��@;�m@;�F@;��@;t�@;dZ@;C�@;"�@:��@:�\@:~�@:M�@:J@9��@9�7@9x�@9hs@9X@9G�@9�@8�9@8bN@8 �@8 �@8  @7�w@7|�@7;d@7+@7
=@6��@6�y@6ȴ@6�+@6@5�T@5@5p�@5/@4�/@4��@3�m@3dZ@333@3"�@3@2��@2��@2-@1��@1G�@0�`@0��@0�u@0�u@0r�@0bN@0Q�@0Q�@01'@0  @/��@/��@/|�@/;d@/�@.�y@.v�@.$�@.{@-�-@-/@,�/@,�@,Z@,�@+�m@+�F@+��@+��@+�@+33@+o@*�@*�H@*n�@)��@)X@(�`@(bN@'�@'�P@'l�@';d@'+@&�y@&��@&ff@&5?@%�T@%@%�-@%O�@$��@$�D@$9X@#��@#dZ@"�@"��@"M�@"�@!��@!�^@!��@!G�@!%@ �u@ bN@  �@   @   @�@��@�@��@�P@�P@;d@��@��@v�@ff@E�@E�@��@`B@?}@V@��@Z@I�@��@ƨ@��@�@dZ@C�@"�@@�H@��@��@^5@-@J@��@��@��@�7@�7@�7@x�@X@�@�`@Ĝ@A�@�w@�P@l�@+@�y@�y@�R@ff@{@��@�-@�h@p�@/@�@�@�D@j@Z@9X@9X@�@�
@��@�@dZ@C�@@��@�!@n�@-@��@��@&�@��@�u@r�@A�@�@��@l�@\)@K�@;d@
=@�y@ȴ@��@��@@@`B@V@��@��@�j@�D@j@I�@�
@��@S�@@
��@
��@
�!@
�\@
M�@
�@	�^@	7L@	&�@	�@��@��@�9@�9@�9@��@�@�@Q�@Q�@A�@1'@ �@b@  @�w@�@�P@|�@\)@K�@;d@+@
=@��@�@�@�@��@v�@ff@5?@�@��@�-@p�@��@�/@��@�@��@��@�D@j@9X@(�@�@��@ƨ@�F@��@t�@S�@C�@33@33@33@"�@�@��@�!@�!@��@��@��@�\@~�@-@�@�@�@�#@��@��@�7@x�@X@7L@�@ ��@ ��@ ��@ �9@ r�@ bN@ bN@ Q�@  �@ b@   @   ?��;?��w?��w?��w?���?�\)?���?��?��?��?��R?��R?���?�v�?�v�?�v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B�
B�#B�TB�fB�mB�sB�sB�yB�sB�mB�mB�fB�`B�/B��B��BǮBĜB��B�RB�9B�B��B��B�B�B�B�B��B��B��B��B��B�VB�B|�Bu�Bn�Be`B[#BR�BQ�BM�BQ�BR�BP�BL�B?}B%�BJB��B�B�NB��BǮB�!B��B��B�PB|�Bm�BB�B1'B+B �B�B�B{BPB1BB
��B
�B
�HB
�B
�B
��B
��B
ŢB
�}B
�'B
��B
o�B
T�B
L�B
F�B
>wB
9XB
49B
#�B

=B
B	��B	��B	��B	��B	��B	�B	�B	�sB	�NB	�B	��B	��B	��B	��B	ɺB	ŢB	�^B	��B	��B	��B	�{B	�hB	�=B	�%B	�B	~�B	q�B	gmB	cTB	^5B	R�B	J�B	E�B	?}B	:^B	7LB	5?B	49B	1'B	/B	-B	'�B	"�B	�B	�B	�B	oB	PB	JB	DB	DB	
=B	1B	%B	B��B��B�B�B�mB�fB�`B�ZB�TB�NB�;B�)B�B��B��B��B��B��BƨB��B�}B�}B�qB�jB�dB�XB�FB�!B�B��B��B��B��B��B��B��B�oB�bB�PB�DB�7B�%B�B�B�B� B|�Bz�Bx�Bw�Bv�Bu�Br�Bo�Bk�BhsBffBcTBbNB`BB_;B]/B[#BXBW
BVBT�BS�BQ�BP�BO�BM�BL�BJ�BH�BG�BF�BE�BD�BC�BA�B?}B=qB=qB<jB;dB:^B:^B9XB7LB7LB5?B5?B5?B49B49B33B2-B2-B2-B1'B0!B/B0!B/B/B.B.B.B-B-B,B)�B)�B)�B(�B(�B(�B)�B(�B'�B&�B'�B'�B'�B'�B(�B'�B'�B(�B)�B)�B)�B)�B)�B(�B)�B+B+B,B,B+B,B-B-B-B-B.B/B0!B1'B2-B33B6FB7LB8RB9XB:^B=qB=qB=qB?}B>wBB�BF�BE�BF�BJ�BM�BM�BN�BO�BQ�BS�BT�BVBXB\)B]/B_;BcTBe`BhsBjBl�Bo�Bp�Br�Br�Bt�Bx�Bz�B}�B�B�B�+B�1B�7B�=B�PB�hB��B��B��B��B��B��B��B�B�B�B�B�-B�LB�^B�dB�jB�qB�}B��BĜBǮBɺB��B��B��B��B��B��B�
B�#B�BB�NB�TB�TB�ZB�ZB�`B�`B�mB�B�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	"�B	#�B	#�B	#�B	$�B	$�B	$�B	%�B	%�B	5?B	8RB	8RB	:^B	:^B	:^B	<jB	=qB	>wB	@�B	E�B	F�B	J�B	O�B	S�B	W
B	W
B	XB	XB	ZB	ZB	[#B	\)B	\)B	]/B	^5B	_;B	`BB	bNB	gmB	hsB	k�B	l�B	n�B	o�B	p�B	p�B	q�B	r�B	s�B	s�B	t�B	u�B	v�B	v�B	y�B	{�B	|�B	|�B	}�B	~�B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�PB	�PB	�VB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�?B	�FB	�^B	�dB	�jB	�}B	�}B	�}B	ÖB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�5B	�;B	�;B	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
	7B

=B
DB
DB
JB
JB
PB
PB
VB
\B
bB
hB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'�B
(�B
)�B
+B
+B
+B
+B
+B
+B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
1'B
2-B
33B
49B
49B
49B
49B
5?B
5?B
5?B
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
K�B
L�B
L�B
M�B
N�B
N�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
R�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
|�B
}�B
}�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
� B
� B
� B
� B
� B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B� B� B� B� B� B� B�NBуB��B�hBөBյB�+B��B�@B��B��B�B�B��B��B�B��B�mB�mB�!B�FB�BȴB�%B��B��B�FB��B�XB��B�"B��B�]B��B��B�2B��B��B�QB�B��B~(Bw2Bp�BhXB^BU2BS@BN�BRoBS�BQ�BOBBAB(�B�B  B��B�&B�
BʦB��B��B��B�B� Bs�BD�B2|B,WB!�B=B+B�BVB	lBuB
��B
�oB
�NB
ںB
��B
�FB
��B
�B
�AB
��B
�pB
s�B
VmB
N"B
H1B
?�B
:�B
7�B
(
B
�B
�B	��B	�xB	�B	�+B	�`B	��B	��B	�B	�ZB	�7B	��B	ңB	��B	͹B	�DB	�fB	�qB	��B	��B	�CB	�B	�@B	�)B	�B	��B	��B	s�B	h�B	d�B	`�B	U�B	L~B	G_B	@�B	;B	7�B	5�B	5B	1�B	0B	.cB	)�B	#�B	 �B	�B	B	�B	�B	�B	�B	�B	
�B		B	zB	�B�B��B�B�QB�
B��B��B��B�B�B��B�IB�	B�aB�\B�<B͟B�B�1B�AB�OB� B��B��B�6B��B�B��B�CB��B�B�&B�HB��B��B��B��B��B�VB�B�=B��B��B��B�B��B~B{�ByrBxRBw�Bv�Bt�BqvBm]Bi�Bg�Bd�Bb�B`�B`vB^�B\�BX�BW�BV�BU�BU2BR�BQ�BP�BN�BN"BLBI�BH�BGzBF�BE�BD�BC�BAUB>B>(B=�B<PB;B:�B:*B9$B8�B5�B5�B5�B4�B5B4�B2�B2�B2�B2-B1AB0�B1B/�B0UB/B.�B.�B-�B-�B,�B+QB*�B*�B)�B*B)�B+B*eB)DB(XB)*B)B)*B(�B)�B(�B)*B)�B*eB*eB*KB*�B*�B*eB+�B+�B+�B,�B,�B,"B-CB-�B-�B-�B./B/B0B1AB2GB3�B4�B72B8B8�B9�B;B=�B>(B>]B@OB@BC�BGzBF�BG�BKxBN<BNVBOvBP�BR�BT�BU�BW$BY1B\�B]�B_�BdBf�BiDBk6BmCBpUBq'BsBshBu�ByXB{�B~�B��B��B�zB��B��B��B�"B� B��B��B�B�)B�dB��B��B��B�]B�}B��B��B��B��B��B��B��B� B� B�B�B�	B�B�B�B�PB�}B�{BרB��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B�B�3B�RB�B�6B�PB�BB	 iB	�B	B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	"4B	"4B	#B	#�B	$B	$B	%,B	%,B	%,B	&fB	'B	5tB	8�B	8�B	:�B	:�B	:�B	<�B	=�B	>�B	A B	FB	GB	KDB	PbB	TFB	W?B	WsB	X_B	X_B	ZQB	ZQB	[WB	\]B	\]B	]�B	^jB	_�B	`�B	b�B	g�B	h�B	k�B	l�B	n�B	o�B	p�B	p�B	q�B	r�B	s�B	tB	t�B	u�B	wB	wB	zB	|6B	}"B	}<B	~(B	cB	�[B	�MB	�MB	�gB	�tB	��B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�&B	�2B	�DB	�DB	�KB	�WB	�cB	�}B	�vB	��B	�tB	�zB	��B	��B	��B	��B	��B	�4B	��B	��B	��B	��B	�	B	�B	��B	�B	�B	�B	�B	�B	�4B	� B	�:B	�,B	�2B	�2B	�2B	�SB	�mB	ؓB	چB	�~B	�jB	�pB	ߊB	�B	�B	�B	�B	�B	�tB	�B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�	B	�	B	�>B	�DB	�<B	�(B	�.B	�HB
 4B
;B
AB
AB
AB
[B
{B
MB
gB
mB
mB
YB
zB
_B
fB
fB
fB
fB
	lB

rB
xB
xB
~B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
�B
 'B
!B
#B
#B
#�B
$B
$&B
%,B
%,B
%B
%B
%B
&B
'B
'B
($B
)*B
*0B
+B
+B
+QB
+6B
+6B
+QB
,=B
-CB
-)B
-]B
-]B
-CB
-CB
-CB
-CB
-CB
.IB
.cB
.IB
.}B
/OB
/OB
/OB
0UB
0UB
1vB
2�B
3�B
4�B
4�B
4nB
4nB
5tB
5�B
5�B
7�B
7�B
8�B
8lB
9�B
9�B
9rB
9rB
9rB
9�B
9�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
=�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
IB
H�B
IB
J	B
KB
KB
LB
MB
MB
NB
OB
OB
PB
PB
Q4B
QB
R B
R:B
R:B
R B
SB
SB
SB
S&B
S@B
TB
TB
TB
S&B
TFB
T,B
U2B
UB
U2B
V9B
VmB
VSB
V9B
V9B
WYB
W?B
XEB
XEB
XEB
YKB
YKB
YKB
YeB
YKB
YKB
ZQB
ZQB
ZQB
ZkB
[WB
[WB
[WB
\]B
\CB
\]B
\CB
\CB
\]B
\]B
\]B
]dB
]~B
]~B
^�B
^jB
_�B
_pB
_pB
_VB
_pB
`�B
`vB
a|B
a|B
a|B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
dtB
dtB
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
g�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
tB
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
t�B
t�B
uB
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
xB
wB
xB
xB
xB
xB
x�B
x�B
y	B
y�B
y�B
y�B
zB
z*B
zB
y�B
zB
{B
z�B
{B
{B
{B
|B
|B
|B
|B
}B
}"B
}"B
}B
}"B
}"B
~B
}B
~B
~(B
}"B
~BB
~B
~B
~B
~(B
.B
HB
�B
�4B
�4B
�4B
�4B
�;B
�4B
�;B
�;B
� B
�;B
�;B
�;B
�'B
�'B
�'B
�'B
�'B
�'B
�AB
�AB
�AB
�[B
�GB
�-B
�-B
�-B
�-B
�-B
�-B
�-B
�-B
�-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.23(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202005180035292020051800352920200518003529202211182143032022111821430320221118214303202005190017072020051900170720200519001707  JA  ARFMdecpA19c                                                                20200508003745  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200507153822  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200507153825  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200507153825  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200507153826  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200507153826  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200507153826  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200507153826  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200507153826  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200507153826  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200507153826  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200507153826                      G�O�G�O�G�O�                JA  ARUP                                                                        20200507155337                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200507153438  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200507153413  CV  JULD            G�O�G�O�FȽ                JM  ARCAJMQC2.0                                                                 20200517153529  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200517153529  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200518151707  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124303  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114512                      G�O�G�O�G�O�                