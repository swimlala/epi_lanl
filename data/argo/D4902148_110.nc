CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-08-21T15:35:57Z creation;2017-08-21T15:36:00Z conversion to V3.1;2019-12-18T07:28:41Z update;2022-11-21T05:32:07Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]T   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
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
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170821153557  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               nA   JA  I1_0397_110                     2C  Dd(�NAVIS_A                         0397                            ARGO 011514                     863 @� �^o�1   @�  �m� @;��u%�d(��*01   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ D�|�D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�FfD�s311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��=@�p�@���@���AQ�A<Q�A\Q�A|Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|qHD|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�uqDŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D帤D���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�8�D�x�D鸤D���D�8�D�x�D긤D���D�5qD�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D��qD���D�8�D�x�D���D���D�?
D�k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�|�A�K�A�ZA�ffA�ffA�ffA�ffA�^5A�hsA�hsA�dZA�ffA�dZA�K�A���A���A��mAړuA�(�Aҝ�A�A�VA���A��mAìAÇ+A�dZA�oA��^A�?}A�bNA�%A�l�A�|�A��A�XA�"�A�I�A�t�A��A�  A��uA���A��hA��A�C�A��FA�1'A�JA��-A�O�A���A�ffA���A���A��A�1A�x�A�ĜA���A��7A��A� �A�O�A��PA�Q�A��mA�{A��TA��A��-A���A���A�ZA��9A�ffA�  A��RA�ȴA�I�A�t�A��A��7A��A���A��wA�\)A�=qA��A�K�A���A�&�A���A�VA�n�A�/A��mA�A�G�A��mA��hA�|�A7LA~5?A}��A}VA|�\A|bNA|JA{7LAxVAu�7Atr�As�PAp��Ao��Ao�hAo/An��AnI�Am�;Am�Al�Al^5Al$�Ak�hAk+Aj�!Ai��AhĜAg�Af�DAe��Ad��Ac��Aa`BA`(�A_�^A_�7A_�A^-A]K�AZ�9AY�AX��AW�hAWK�AV�ATZAS�^AS;dAR��AP�AO�AOhsAN��ANM�AM��AM��AL��ALbAJ�AH��AG��AF�jAE��AB��A@��A@$�A?�A?ƨA?;dA>z�A=��A<��A;��A;+A:1'A9dZA9A8z�A7�A6��A69XA3��A2n�A2�A2bA1�-A/�TA-��A-t�A-C�A,��A,��A+�A)7LA'�-A'�hA'/A&�RA&�DA&E�A%A$n�A$I�A#��A#��A#%A"1A!�-A!O�A ��A?}A(�AXA�yA��A�hA�Az�AA�+A�A��A|�A\)A;dA�AI�A"�AhsA�A�/A��A9XA�A��A
Q�A	p�A	%A�-A�AA�Ax�Al�A\)A;dA��A5?A��AA�A ��A n�A M�A 9XA =qA A�A I�A I�A 9XA �@��@���@�l�@��\@�G�@���@��D@���@���@�$�@���@�+@�z�@��@�j@�@��@�ȴ@�ff@�5?@�J@��@�p�@���@��@ߕ�@�V@� �@���@۶F@ۍP@�@؋D@��@�ȴ@�7L@�ƨ@��@�hs@�C�@ͩ�@̋D@˅@�n�@�@�G�@�(�@�"�@Ɨ�@���@�bN@�A�@�C�@¸R@\@�G�@���@�~�@�%@�|�@��!@���@��@�"�@��h@���@�@��@�Z@��R@��#@�hs@�x�@��@��
@���@�M�@��7@���@��@�Q�@�  @�ƨ@��w@��w@��w@�ƨ@���@��
@��
@��@�V@��^@��@��u@��\@�V@���@��P@�@���@��T@��@��D@�z�@�r�@�r�@�r�@�j@�r�@�  @���@��P@�C�@�`B@���@�|�@�C�@��@�o@��!@�{@���@���@��@�o@��y@�ȴ@���@���@�5?@�hs@�/@��/@��D@�I�@��
@��F@���@�l�@�C�@�+@��@�@��y@���@�J@���@�O�@��`@��@�1@��
@���@�;d@���@���@�V@��@��^@���@��@�b@���@�S�@�o@��@��R@���@�~�@�n�@�=q@�J@���@�p�@��@��j@�b@��F@��P@�o@��@���@�`B@�`B@���@���@��@��u@��j@��`@���@��@�r�@�I�@�9X@�1@�;@�P@~�R@~v�@~V@}�h@|�/@{dZ@{dZ@{33@{@z�H@zn�@y��@y�7@x�9@w;d@v��@u��@up�@up�@u�-@u�h@u�@u`B@u?}@uV@tz�@t1@s�F@s@r�\@r-@q�7@qhs@qhs@qhs@p�9@pbN@o��@o\)@n�y@o\)@o��@oK�@o;d@o
=@n@l�j@l�D@lj@lj@lj@lj@lZ@lI�@lI�@l1@k�m@k��@j=q@i�@i��@j�\@j�!@j��@jn�@j-@i�#@i�^@i�7@i�@h�9@h��@h �@g�@gl�@f�y@e�@e�@d��@d�D@d(�@d1@c��@c��@cS�@c"�@b�\@bn�@bn�@b=q@bM�@b=q@b�@a��@a��@a%@`�9@`1'@`b@_�@_�w@_�P@_;d@^�@\�D@\I�@\9X@[ƨ@Z�@Z��@Y�@X��@Xr�@X �@Xb@W�@W�@W��@W|�@W�@V��@V$�@U�T@U��@Up�@UO�@U/@UV@T��@Tj@T�@S"�@R�!@Q��@QG�@P�u@PbN@PbN@PA�@O�@O��@O�w@O�w@O�@O|�@OK�@O�@N�y@N�@N�@Nȴ@N��@Nv�@N{@M��@M��@Mp�@MO�@M/@MV@L�j@L��@L�D@L�D@Lj@L(�@L�@K��@K��@J�H@J�@I��@I&�@H�@HA�@G��@G\)@GK�@GK�@G+@F�R@Fv�@F$�@F5?@E�@E@Ep�@D�@D�@D�D@Dj@D9X@D(�@D�@D1@C�m@Ct�@B=q@Ax�@AX@@�9@@r�@@b@?�w@?\)@>��@>5?@=�@=�T@=��@=�-@=�@=O�@=/@<��@<��@<��@<(�@<�@<1@;��@;ƨ@;dZ@;"�@:�!@:=q@9��@9X@9�@9%@8Ĝ@8��@8��@8�@8Q�@81'@8  @7��@7�@6ȴ@6v�@6E�@6{@6{@5�@5�-@5��@5p�@5�@4�/@4�@49X@3��@3�
@3�
@3��@3�@3dZ@3t�@3t�@3t�@3t�@3"�@2��@2M�@2J@1�@1��@17L@0��@0  @/�P@/+@/�@/
=@.��@.v�@.E�@.$�@.{@-�@-��@-�-@-�h@-`B@,�@,z�@,(�@,1@+�m@+ƨ@+��@+S�@*��@*M�@*-@*�@*J@)��@)�@)��@)�7@)7L@)�@)%@(��@(�9@(Q�@( �@(  @'�;@'K�@&v�@%�@%?}@%V@$�@$�@#ƨ@#"�@"��@"�!@"��@"��@"�\@"^5@"M�@!��@!�7@!G�@!�@ �`@ �u@ r�@ Q�@ Q�@  �@K�@�R@��@��@�+@ff@{@��@`B@`B@V@�@z�@z�@z�@Z@(�@�@1@��@��@�m@�
@t�@@��@n�@^5@M�@=q@�@G�@��@�u@1'@�@�w@�P@K�@;d@�@�y@ȴ@��@�+@ff@V@E�@$�@�T@��@�@O�@V@�/@�@��@�D@z�@Z@(�@��@�F@�F@dZ@@�\@�@�^@�7@�7@hs@X@7L@��@bN@ �@�@�@�;@�w@��@K�@��@�+@v�@V@�T@��@?}@�@V@�@�j@��@Z@Z@I�@9X@(�@��@��@t�@t�@t�@dZ@C�@o@
��@
�H@
��@
�!@
�!@
��@
n�@
�@	��@	�^@	X@�`@�u@Q�@  @�P@\)@
=@�@��@v�@{@��@p�@p�@p�@`B@/@�@V@�@��@Z@(�@1@��@�m@�F@�F@��@��@�@dZ@33@o@��@^5@-@�@�^@��@hs@�@ �`@ ��@ �u@ �@ bN@ 1'@   ?��;?��;?��;?��w?���?�|�?�\)?��?��?��?��R?�v�?�5??�5??�{?��h?�/?��?���?���?��?��D?��D?��D?�j?�j?�j?��D?��D?��D?��D?��D?��D?��D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�|�A�K�A�ZA�ffA�ffA�ffA�ffA�^5A�hsA�hsA�dZA�ffA�dZA�K�A���A���A��mAړuA�(�Aҝ�A�A�VA���A��mAìAÇ+A�dZA�oA��^A�?}A�bNA�%A�l�A�|�A��A�XA�"�A�I�A�t�A��A�  A��uA���A��hA��A�C�A��FA�1'A�JA��-A�O�A���A�ffA���A���A��A�1A�x�A�ĜA���A��7A��A� �A�O�A��PA�Q�A��mA�{A��TA��A��-A���A���A�ZA��9A�ffA�  A��RA�ȴA�I�A�t�A��A��7A��A���A��wA�\)A�=qA��A�K�A���A�&�A���A�VA�n�A�/A��mA�A�G�A��mA��hA�|�A7LA~5?A}��A}VA|�\A|bNA|JA{7LAxVAu�7Atr�As�PAp��Ao��Ao�hAo/An��AnI�Am�;Am�Al�Al^5Al$�Ak�hAk+Aj�!Ai��AhĜAg�Af�DAe��Ad��Ac��Aa`BA`(�A_�^A_�7A_�A^-A]K�AZ�9AY�AX��AW�hAWK�AV�ATZAS�^AS;dAR��AP�AO�AOhsAN��ANM�AM��AM��AL��ALbAJ�AH��AG��AF�jAE��AB��A@��A@$�A?�A?ƨA?;dA>z�A=��A<��A;��A;+A:1'A9dZA9A8z�A7�A6��A69XA3��A2n�A2�A2bA1�-A/�TA-��A-t�A-C�A,��A,��A+�A)7LA'�-A'�hA'/A&�RA&�DA&E�A%A$n�A$I�A#��A#��A#%A"1A!�-A!O�A ��A?}A(�AXA�yA��A�hA�Az�AA�+A�A��A|�A\)A;dA�AI�A"�AhsA�A�/A��A9XA�A��A
Q�A	p�A	%A�-A�AA�Ax�Al�A\)A;dA��A5?A��AA�A ��A n�A M�A 9XA =qA A�A I�A I�A 9XA �@��@���@�l�@��\@�G�@���@��D@���@���@�$�@���@�+@�z�@��@�j@�@��@�ȴ@�ff@�5?@�J@��@�p�@���@��@ߕ�@�V@� �@���@۶F@ۍP@�@؋D@��@�ȴ@�7L@�ƨ@��@�hs@�C�@ͩ�@̋D@˅@�n�@�@�G�@�(�@�"�@Ɨ�@���@�bN@�A�@�C�@¸R@\@�G�@���@�~�@�%@�|�@��!@���@��@�"�@��h@���@�@��@�Z@��R@��#@�hs@�x�@��@��
@���@�M�@��7@���@��@�Q�@�  @�ƨ@��w@��w@��w@�ƨ@���@��
@��
@��@�V@��^@��@��u@��\@�V@���@��P@�@���@��T@��@��D@�z�@�r�@�r�@�r�@�j@�r�@�  @���@��P@�C�@�`B@���@�|�@�C�@��@�o@��!@�{@���@���@��@�o@��y@�ȴ@���@���@�5?@�hs@�/@��/@��D@�I�@��
@��F@���@�l�@�C�@�+@��@�@��y@���@�J@���@�O�@��`@��@�1@��
@���@�;d@���@���@�V@��@��^@���@��@�b@���@�S�@�o@��@��R@���@�~�@�n�@�=q@�J@���@�p�@��@��j@�b@��F@��P@�o@��@���@�`B@�`B@���@���@��@��u@��j@��`@���@��@�r�@�I�@�9X@�1@�;@�P@~�R@~v�@~V@}�h@|�/@{dZ@{dZ@{33@{@z�H@zn�@y��@y�7@x�9@w;d@v��@u��@up�@up�@u�-@u�h@u�@u`B@u?}@uV@tz�@t1@s�F@s@r�\@r-@q�7@qhs@qhs@qhs@p�9@pbN@o��@o\)@n�y@o\)@o��@oK�@o;d@o
=@n@l�j@l�D@lj@lj@lj@lj@lZ@lI�@lI�@l1@k�m@k��@j=q@i�@i��@j�\@j�!@j��@jn�@j-@i�#@i�^@i�7@i�@h�9@h��@h �@g�@gl�@f�y@e�@e�@d��@d�D@d(�@d1@c��@c��@cS�@c"�@b�\@bn�@bn�@b=q@bM�@b=q@b�@a��@a��@a%@`�9@`1'@`b@_�@_�w@_�P@_;d@^�@\�D@\I�@\9X@[ƨ@Z�@Z��@Y�@X��@Xr�@X �@Xb@W�@W�@W��@W|�@W�@V��@V$�@U�T@U��@Up�@UO�@U/@UV@T��@Tj@T�@S"�@R�!@Q��@QG�@P�u@PbN@PbN@PA�@O�@O��@O�w@O�w@O�@O|�@OK�@O�@N�y@N�@N�@Nȴ@N��@Nv�@N{@M��@M��@Mp�@MO�@M/@MV@L�j@L��@L�D@L�D@Lj@L(�@L�@K��@K��@J�H@J�@I��@I&�@H�@HA�@G��@G\)@GK�@GK�@G+@F�R@Fv�@F$�@F5?@E�@E@Ep�@D�@D�@D�D@Dj@D9X@D(�@D�@D1@C�m@Ct�@B=q@Ax�@AX@@�9@@r�@@b@?�w@?\)@>��@>5?@=�@=�T@=��@=�-@=�@=O�@=/@<��@<��@<��@<(�@<�@<1@;��@;ƨ@;dZ@;"�@:�!@:=q@9��@9X@9�@9%@8Ĝ@8��@8��@8�@8Q�@81'@8  @7��@7�@6ȴ@6v�@6E�@6{@6{@5�@5�-@5��@5p�@5�@4�/@4�@49X@3��@3�
@3�
@3��@3�@3dZ@3t�@3t�@3t�@3t�@3"�@2��@2M�@2J@1�@1��@17L@0��@0  @/�P@/+@/�@/
=@.��@.v�@.E�@.$�@.{@-�@-��@-�-@-�h@-`B@,�@,z�@,(�@,1@+�m@+ƨ@+��@+S�@*��@*M�@*-@*�@*J@)��@)�@)��@)�7@)7L@)�@)%@(��@(�9@(Q�@( �@(  @'�;@'K�@&v�@%�@%?}@%V@$�@$�@#ƨ@#"�@"��@"�!@"��@"��@"�\@"^5@"M�@!��@!�7@!G�@!�@ �`@ �u@ r�@ Q�@ Q�@  �@K�@�R@��@��@�+@ff@{@��@`B@`B@V@�@z�@z�@z�@Z@(�@�@1@��@��@�m@�
@t�@@��@n�@^5@M�@=q@�@G�@��@�u@1'@�@�w@�P@K�@;d@�@�y@ȴ@��@�+@ff@V@E�@$�@�T@��@�@O�@V@�/@�@��@�D@z�@Z@(�@��@�F@�F@dZ@@�\@�@�^@�7@�7@hs@X@7L@��@bN@ �@�@�@�;@�w@��@K�@��@�+@v�@V@�T@��@?}@�@V@�@�j@��@Z@Z@I�@9X@(�@��@��@t�@t�@t�@dZ@C�@o@
��@
�H@
��@
�!@
�!@
��@
n�@
�@	��@	�^@	X@�`@�u@Q�@  @�P@\)@
=@�@��@v�@{@��@p�@p�@p�@`B@/@�@V@�@��@Z@(�@1@��@�m@�F@�F@��@��@�@dZ@33@o@��@^5@-@�@�^@��@hs@�@ �`@ ��@ �u@ �@ bN@ 1'@   ?��;?��;?��;?��w?���?�|�?�\)?��?��?��?��R?�v�?�5??�5??�{?��h?�/?��?���?���?��?��D?��D?��D?�j?�j?�j?��D?��D?��D?��D?��D?��D?��D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111ǮB�B�B�B�B�B�B�B�B�B�B�B�B�BbB\B\B1B�B�B�B�B�B�B�B�B�
B��B�B�B�B��B��BǮBɺBĜB�qB�?B�B��B��B�uB�\B�7B�B~�B{�By�Bx�Bv�Bt�Br�Bo�BjBgmBjBhsBdZB]/BR�BF�B@�B:^B2-B'�B�B%B��B�`B��BÖB�9B��B�Bz�Bv�Bq�BdZBYBQ�BH�BD�B@�B<jB6FB'�B"�B!�B�BbB
=B%BB
��B
�B
�)B
��B
�}B
�XB
�FB
�'B
��B
��B
�{B
�bB
�JB
�7B
�+B
�B
}�B
q�B
cTB
\)B
T�B
F�B
A�B
>wB
<jB
9XB
6FB
33B
1'B
-B
(�B
&�B
!�B
�B
�B
uB

=B
B	��B	��B	�B	�sB	�B	��B	��B	ɺB	ŢB	�}B	�dB	�3B	�!B	�B	��B	��B	��B	�JB	�7B	�+B	�B	w�B	o�B	k�B	ffB	cTB	ffB	dZB	_;B	[#B	R�B	A�B	8RB	1'B	#�B	�B	uB	hB	bB	\B	JB	+B	B	B	B	  B��B��B��B��B�B�B�B�B�fB�`B�ZB�BB��BŢBÖBB��B�}B�dB�?B�!B�'B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�\B�VB�JB�+B�B|�Bx�Bs�Bo�Bl�Bk�BjBiyBgmBe`BaHB\)BW
BVBVBT�BS�BQ�BN�BL�BJ�BG�BE�BC�BC�BC�BC�BB�BA�B@�B=qB;dB9XB8RB8RB7LB7LB7LB7LB7LB7LB7LB6FB5?B33B2-B1'B0!B/B.B.B-B,B)�B(�B&�B$�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBoBoBhBhBbBbB\B\B\BbBbBhBhBoBoBoBuBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B$�B%�B+B-B1'B49B6FB9XB;dB;dB<jB@�B@�B@�BA�BB�BC�BC�BD�BE�BE�BE�BD�BG�BI�BJ�BM�BQ�BQ�BS�BVBW
BVBW
BYBbNBdZBdZBe`Be`Be`BffBhsBt�B|�B� B~�B�1B�VB�hB�uB��B��B��B��B��B��B��B��B��B�B�B�B�B�-B�3B�?B�LB�RB�dB�jB�jB�qB�wB�}B�}B��B��BBƨBȴB��B��B��B�B�
B�B�#B�;B�BB�NB�TB�`B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	B	%B	1B	1B	
=B	
=B	JB	PB	VB	\B	bB	hB	oB	�B	�B	�B	�B	�B	$�B	'�B	)�B	,B	.B	/B	/B	0!B	2-B	49B	6FB	7LB	9XB	;dB	=qB	?}B	B�B	D�B	E�B	E�B	F�B	H�B	I�B	J�B	K�B	L�B	M�B	N�B	N�B	N�B	O�B	P�B	Q�B	VB	XB	YB	\)B	_;B	_;B	aHB	cTB	dZB	ffB	hsB	iyB	l�B	q�B	u�B	v�B	w�B	x�B	|�B	~�B	� B	� B	� B	� B	� B	� B	�B	�B	�B	�B	�B	�1B	�PB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�3B	�?B	�?B	�?B	�FB	�FB	�LB	�RB	�}B	��B	��B	B	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�#B	�5B	�BB	�NB	�ZB	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
DB
DB
PB
PB
VB
\B
\B
hB
oB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
+B
+B
,B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
>wB
?}B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
F�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
M�B
M�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
bNB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
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
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
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
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��ByB�B�B�B�B�B�B�B�B�B�B�BB�B}BTBHB��B��B��B�BBٚBںBٴB��B�B��BچB��B�sB�SB�[B�JB�"BǮB� B��B��B� B��B��B�TB�dB��B�4B}BzxBy�Bw�Bu�BtnBr-Bn/BjBk�Bi�BfLB_�BU�BH�BBuB<jB4nB+B�B�B��B��B̘B�%B��B�B��B|BxlBt�Bf�BZ�BS�BI�BFBA�B=�B88B(�B#nB"�B)B BxB_B�B
��B
�B
�!B
��B
�;B
��B
��B
��B
��B
��B
�MB
�4B
�B
��B
�B
��B
�oB
t�B
d�B
]�B
W�B
G�B
B'B
>�B
=B
:B
6�B
3�B
2B
-�B
)yB
'�B
"�B
�B
�B
�B
�B
�B	�B	�FB	�5B	�B	�kB	ЗB	�JB	ʦB	�B	�;B	�BB	��B	�[B	�]B	��B	�bB	�eB	�PB	�#B	�fB	��B	y	B	poB	lWB	gRB	dB	g8B	e�B	`�B	]/B	UMB	B�B	9�B	3MB	'8B	�B	aB	�B	�B	HB	jB	KB	mB	�B	'B	UB�B��B��B�fB�B��B�AB�CB�B��B�zB�B�2B�YB�B�GBB�UB�BB��B��B��B��B��B��B�B��B�mB��B��B��B�B��B��B�B��B�B��B�.B�\B�B��B��B~BBz�Bu�Bq'Bm)Bk�BkBjKBh�BgRBc�B^�BW�BV�BV�BU�BT�BS�BP.BM�BL�BI7BF�BDBC�BC�BC�BCBB[BA�B?cB=B:^B9	B8�B7�B7�B7�B7�B7�B7�B7�B6�B6�B4�B33B1�B1B/�B.�B.�B-�B,�B+�B*KB(�B&fB#�B �B!B~BBBB/B)B�B=B�B$BB�B�B�B�BuB&BoB�BhBNB�B�B}BNB4BTBBBuB[B,B�BB�BYB+BB�BBB�B�B�BB�B~BB B 'B"�B%�B'8B+�B-�B1vB4�B7LB:*B<B<6B=B@�B@�B@�BA�BB�BC�BC�BD�BE�BE�BE�BESBHfBJXBKxBN�BSuBS&BT�BV�BW�BV�BW�BY�Bb�Bd�Bd�Be�Be�Be�Bf�Bh�Bu%B}<B��B�iB�7B��B��B��B��B�B�?B��B�|B�zB�RB�KB�KB�QB�WB��B��B�|B��B��B��B��B��B��B��B��B��B��B��B��B��B�-B�B�7B�JB�BB�oB�SB�sBخBیBߤB��B�B��B�B�B�B�-B�B�B�>B�B�B�B�B�VB�BB�cB	UB	{B	�B	�B	�B	�B	�B	B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B		B	�B	%B	(>B	*KB	,qB	.}B	/OB	/iB	0�B	2�B	4�B	6zB	7�B	9�B	;�B	=�B	?�B	B�B	E9B	F?B	FB	G+B	H�B	I�B	J�B	K�B	MB	N"B	OB	O(B	O(B	P.B	Q4B	RTB	VSB	X_B	YeB	\]B	_pB	_pB	a�B	c�B	d�B	f�B	h�B	iyB	l�B	q�B	u�B	wB	xRB	yXB	}"B	.B	�B	�B	�B	�4B	�4B	�4B	�;B	�;B	�UB	��B	�SB	�KB	�jB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	� B	�B	� B	�B	�B	�0B	�B	�QB	�6B	�WB	�]B	�iB	�|B	��B	�tB	�tB	��B	�zB	��B	��B	�$B	��B	��B	��B	��B	��B	�B	�1B	��B	��B	�B	�B	�B	�B	�(B	�.B	�4B	�@B	�MB	�9B	�?B	�?B	�EB	�EB	�eB	�kB	�qB	ۦB	ޞB	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�+B	�B	�$B	�DB	�B	�VB	�BB	�B	�BB	�(B	�HB	�.B
 4B
 B
 OB
;B
UB
[B
GB
MB
MB
MB
SB
SB
SB
SB
�B
�B
	�B
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
 'B
 B
 �B
!�B
# B
#B
"�B
#B
$B
$&B
$B
$B
%B
&B
&2B
'B
'B
'B
'8B
($B
(
B
($B
(
B
(
B
($B
(XB
)_B
*0B
*KB
+6B
+6B
+QB
,WB
.cB
/iB
/OB
0;B
0oB
0oB
1vB
1[B
2|B
2|B
2aB
2aB
2aB
2|B
3�B
3�B
4�B
4nB
5tB
5tB
5�B
6�B
6�B
6�B
7�B
8�B
8lB
8�B
8lB
8�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
<�B
<�B
=�B
>�B
?�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
GB
HB
IB
I�B
I�B
J	B
I�B
I�B
KB
K�B
K�B
LB
K�B
MB
NB
M�B
NB
NB
M�B
M�B
M�B
OB
N"B
NB
N<B
O(B
PB
PB
Q B
Q4B
QB
QNB
Q4B
R B
S@B
T,B
T,B
UMB
U2B
V9B
VB
V9B
VSB
W?B
WYB
W?B
X+B
X+B
XEB
XEB
X_B
YKB
YeB
YKB
ZQB
ZQB
ZQB
[=B
[WB
[WB
[WB
[WB
[WB
[WB
[qB
[qB
\�B
\xB
]~B
^jB
^jB
_VB
_pB
_pB
_�B
_�B
`�B
a�B
a|B
bhB
a|B
b�B
b�B
b�B
b�B
c�B
c�B
d�B
d�B
d�B
d�B
e�B
ezB
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
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
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
tB
tB
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
wB
wB
w�B
w�B
xB
xB
xB
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y$B
y	B
x�B
x�B
y�B
zB
y�B
zB
zB
zB
{B
z�B
{B
{B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{B
{B
{B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.23(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709010033112017090100331120170901003311202211182131332022111821313320221118213133201804031936592018040319365920180403193659  JA  ARFMdecpA19c                                                                20170822003506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170821153557  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170821153558  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170821153559  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170821153559  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170821153559  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170821153559  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170821153559  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170821153600  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170821153600                      G�O�G�O�G�O�                JA  ARUP                                                                        20170821155540                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170821153245  CV  JULD            G�O�G�O�F� �                JM  ARCAJMQC2.0                                                                 20170831153311  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170831153311  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103659  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171527                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123133  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                