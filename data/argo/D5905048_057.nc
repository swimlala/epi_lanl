CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-11T00:35:22Z creation;2016-11-11T00:35:24Z conversion to V3.1;2019-12-19T08:22:02Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161111003522  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               9A   JA  I2_0577_057                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��2��[ 1   @��36��@3?U�=��d��S��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ D�|�D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D�|�D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@qG�@���@���AQ�A<Q�A\Q�A|Q�A�(�A���A�(�A�(�A�(�A�(�A�(�A�\)B{B{B{B{B'{B/{B7{B?{BG{BO{BW{B_{Bg{Bo{Bw{B{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��qB��=B��=BÊ=BǊ=Bˊ=Bϊ=Bӊ=B׊=Bۊ=Bߊ=B�=B�=B�=B�=B�=B��=B��=B��=C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3��C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs��Cu�Cw�Cy�C{�C}�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��\C��C��C��C��\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qHD �HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD	qHD	�HD
qHD
�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDw�D�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HDqHD�HD qHD �HD!qHD!�HD"qHD"�HD#qHD#�HD$qHD$�HD%qHD%�HD&qHD&�HD'qHD'�HD(qHD(�HD)qHD)�HD*qHD*�HD+qHD+�HD,qHD,�HD-qHD-�HD.qHD.�HD/qHD/�HD0qHD0�HD1qHD1�HD2qHD2�HD3qHD3�HD4qHD4�HD5qHD5�HD6qHD6�HD7qHD7�HD8qHD8�HD9qHD9�HD:qHD:�HD;qHD;�HD<qHD<�HD=qHD=�HD>qHD>�HD?qHD?�HD@qHD@�HDAqHDA�HDBqHDB�HDCqHDC�HDDqHDD�HDEqHDE�HDFqHDF�HDGqHDG�HDHqHDH�HDIqHDI�HDJqHDJ�HDKqHDK�HDLqHDL�HDMqHDM�HDNqHDN�HDOqHDO�HDPqHDP�HDQqHDQ�HDRqHDR�HDSqHDS�HDTqHDT�HDUqHDU�HDVqHDV�HDWqHDW�HDXqHDX�HDYqHDY�HDZqHDZ�HD[qHD[�HD\qHD\�HD]qHD]�HD^qHD^�HD_qHD_�HD`qHD`�HDaqHDa�HDbqHDb�HDcqHDc�HDdqHDd�HDeqHDe�HDfqHDf�HDgqHDg�HDhqHDh�HDiqHDi�HDjqHDj�HDkqHDk�HDlqHDl�HDmqHDm�HDnqHDn�HDoqHDo�HDpqHDp�HDqqHDq�HDrqHDr�HDsqHDs�HDtqHDt�HDuqHDu�HDvqHDv�HDwqHDw�HDxqHDx�HDyqHDy�HDzqHDz�HD{qHD{�HD|w�D|�HD}qHD}�HD~qHD~�HDqHD�HD�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�uqD���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�;�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�uqDƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�uqDܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�DฤD���D�8�D�x�DḤD���D�8�D�x�D⸤D���D�8�D�x�D㸤D���D�8�D�x�D两D���D�8�D�x�D�qD���D�8�D�x�D渤D���D�8�D�x�D縤D���D�8�D�x�D踤D���D�;�D�x�D鸤D���D�8�D�x�D긤D���D�8�D�x�D븤D���D�8�D�x�D츤D���D�8�D�x�D���D���D�8�D�x�DD���D�8�D�x�D︤D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D�D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aן�Aס�Aס�Aף�Aץ�Aק�Aש�Aש�A׬A׬A׬A׬A׬A׬Aף�A�^5A�ĜAհ!AծAէ�A�~�A�K�A�=qA�;dA�9XA�7LA�33A�&�A��A�{A�
=A���A��A��A���A�z�A�%A�`BAћ�A�|�A��A�bNA�p�A�5?A��A��A�O�A�  A���A�(�A��A�ffA�ȴA��RA��mA�&�A��A�  A�l�A�hsA�I�A��A���A�?}A�l�A���A��-A�1'A��\A�"�A�Q�A��DA�"�A���A��`A�A�A�=qA��/A�jA�O�A��
A��
A�A�ĜA�ffA�ZA�{A�M�A��9A�oA��-A�7LA�{A��mA��/A���A�-A��#A�(�A�l�A�A��A��A�ȴA��-A��FA��#A~��A{
=Aw�PAv-Atz�As%ApjAoAkK�Ag�wAe�PAc?}Aa�#A]��A]+A\��A\ffAZz�AXAW�
AW��AW��AW��AW��AVn�AUG�AT��AT(�AR�`AQ�TAPE�AO��AN�HANbAL��AL$�AKƨAJ�`AI��AI"�AHz�AGAG�PAG`BAF��AF  AD-AB�yA@�`A@bNA@1A>��A>1'A="�A:$�A5�wA3�A1ƨA.��A,��A+S�A)��A'��A&�DA%�A#��A"�HA"JA!�A�AA�A��A��Ap�A�A�\A�FAn�Al�A�^An�A�A`BAM�AXA�;AJA&�A
�RA	x�An�A�;AXA��AVA9XA-A�A\)A"�A��A~�A{A�-A|�@��`@�bN@��@�ƨ@��\@�p�@���@�-@�(�@�S�@��#@�Ĝ@�n�@�Q�@�ff@�A�@�?}@�
=@���@ݲ-@ݡ�@�V@܋D@���@��@�/@ش9@�I�@���@�~�@�7L@�&�@�(�@Ӆ@�@�^5@�G�@�Q�@�|�@���@·+@�v�@�M�@͡�@̬@ʟ�@�/@Ȭ@�A�@�(�@ǅ@���@ƸR@�ff@�?}@���@Ĵ9@�K�@��#@���@�G�@�V@���@���@�t�@�C�@���@�V@�$�@��T@���@�?}@��@��F@�"�@���@���@��/@�(�@��F@�\)@��H@��\@�V@�@�p�@�t�@�ff@�/@��@�Q�@��;@�
=@�@���@�
=@�$�@��7@�O�@�1@���@�=q@�@�O�@���@�z�@�1'@��@��@��
@���@�|�@�S�@�+@�@���@��@��!@��^@��`@��@���@�  @�|�@�S�@�S�@��P@��F@���@���@��;@�A�@��j@�J@���@���@�V@�ff@��+@�v�@�E�@�{@��@��-@�x�@�/@��j@�bN@�1'@��w@��F@��P@�l�@�ȴ@���@���@��7@��7@�%@�Ĝ@���@�A�@�r�@�r�@�Z@��m@�
=@��@�~�@�$�@��@��h@�x�@�O�@��@���@��`@���@��j@��u@�9X@� �@�b@��;@�ƨ@��w@��w@��F@�|�@�;d@��@���@�ȴ@��R@��!@�x�@��u@�1'@�b@��m@��;@��
@�ƨ@���@�l�@�+@���@��\@�n�@�M�@�J@���@��@��@��@��#@��-@���@��@�`B@�7L@���@���@��@�bN@��@��@�ƨ@���@�l�@�"�@���@��H@���@�ff@�5?@�J@���@��#@�@���@���@��h@�x�@�?}@�%@�Ĝ@���@��@�Z@�9X@��@��w@���@�t�@�K�@�ȴ@�$�@��@�@�x�@��@���@��u@�Z@�A�@�1'@� �@��@��F@��@���@��P@��@�|�@��\@�{@��@��#@���@�hs@��@���@��@��`@��/@���@�Ĝ@� �@��P@�K�@���@�ȴ@�~�@��@���@��@�@���@���@�hs@�&�@��`@�Ĝ@���@�j@��@�ƨ@���@�C�@���@��\@��+@�V@�J@��^@��@�hs@�&�@��j@���@���@���@���@���@���@��u@�z�@�@~�+@~{@}�T@}�-@}O�@|�@|I�@|1@{�
@{��@{33@z�H@z��@z�\@z=q@y��@y��@yhs@yX@y&�@x�`@x�9@x �@w�w@w�P@w\)@v�y@vE�@v$�@u�T@u@u�h@u�@uO�@t��@tj@t�@s�F@s33@so@r=q@q��@p��@p�@pQ�@p1'@pb@p  @o�@o\)@n�R@nv�@nv�@nV@m�@m��@l��@l9X@k��@k�@jM�@i�^@i7L@h��@g�@fE�@f{@e�T@eO�@d�@dI�@d�@c��@c"�@b�\@bn�@b=q@b�@a��@a�7@a&�@`��@`A�@_l�@_
=@^�@^@]�@\�/@\9X@[�
@[�F@[��@[��@[��@[�@[33@Zn�@ZJ@Y�@Y�7@Y�@X  @W�P@WK�@W
=@Vȴ@VV@V$�@T��@TZ@T�@R��@Q��@Q7L@Q�@P��@Pr�@P1'@O��@OK�@O;d@O�@O
=@N�y@N�R@N��@N��@N��@N�+@N{@Mp�@MO�@LI�@Kt�@K@J�H@J�!@J~�@J~�@Jn�@JM�@J-@JJ@I�#@I�7@I%@H �@G��@G|�@G\)@G;d@G�@F��@F��@FV@F{@E��@E�-@E�h@E�@Ep�@EO�@D�/@D(�@C�m@C�F@C�@B�H@B�@A�#@A��@A��@@��@?��@?�@?|�@?\)@?;d@?+@>�@>5?@=�@=@=p�@=p�@=p�@=O�@<�@<�j@<��@<j@<I�@<�@;�
@;ƨ@;��@;�@;C�@;@:��@:��@:�\@:�\@:^5@:-@9��@9�#@9��@9�^@9��@9x�@9G�@9�@8��@8�9@8bN@81'@8b@7|�@7
=@6�@6�R@6��@6V@6{@5�T@5@5@5�-@5�-@5��@5�@5`B@5?}@4�/@4��@4�D@4z�@4I�@4(�@3��@3�F@3dZ@3o@2n�@1�#@1X@1�@1%@0��@0�`@0�`@0Ĝ@0�9@0�u@0bN@/�w@.�R@.E�@.E�@.E�@-�T@-O�@,�j@,1@+��@+t�@+dZ@*�H@*�\@*n�@*J@)x�@)X@)G�@)&�@(��@(bN@(A�@(b@'�@'��@'K�@&ȴ@&v�@&V@&5?@%�@%@%��@%�@%V@$z�@$�@#�F@#C�@#@"�@"��@"~�@"M�@"-@!��@!��@!�@!x�@ �9@ �@ 1'@��@l�@;d@�y@��@V@E�@E�@$�@�@`B@V@�@��@j@Z@I�@�@��@��@t�@S�@C�@"�@@�H@M�@J@�^@�7@x�@X@&�@%@�@  @�@K�@�@��@�R@��@ff@E�@$�@$�@$�@@��@�h@p�@�@j@I�@9X@(�@�@��@�
@��@��@�@�@t�@t�@S�@��@�!@~�@~�@n�@n�@=q@�@�@��@��@��@��@�7@x�@X@��@�u@�u@�@r�@r�@r�@bN@bN@bN@bN@bN@Q�@A�@A�@1'@1'@1'@b@  @��@�P@�@ȴ@�R@��@v�@ff@ff@ff@ff@V@E�@5?@@��@@�-@�-@��@�@`B@/@V@�@��@�j@�@z�@Z@9X@9X@�@ƨ@��@�@t�@S�@"�@@
�H@
��@
��@
�\@
n�@
^5@
�@	�@	��@	��@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aן�Aס�Aס�Aף�Aץ�Aק�Aש�Aש�A׬A׬A׬A׬A׬A׬Aף�A�^5A�ĜAհ!AծAէ�A�~�A�K�A�=qA�;dA�9XA�7LA�33A�&�A��A�{A�
=A���A��A��A���A�z�A�%A�`BAћ�A�|�A��A�bNA�p�A�5?A��A��A�O�A�  A���A�(�A��A�ffA�ȴA��RA��mA�&�A��A�  A�l�A�hsA�I�A��A���A�?}A�l�A���A��-A�1'A��\A�"�A�Q�A��DA�"�A���A��`A�A�A�=qA��/A�jA�O�A��
A��
A�A�ĜA�ffA�ZA�{A�M�A��9A�oA��-A�7LA�{A��mA��/A���A�-A��#A�(�A�l�A�A��A��A�ȴA��-A��FA��#A~��A{
=Aw�PAv-Atz�As%ApjAoAkK�Ag�wAe�PAc?}Aa�#A]��A]+A\��A\ffAZz�AXAW�
AW��AW��AW��AW��AVn�AUG�AT��AT(�AR�`AQ�TAPE�AO��AN�HANbAL��AL$�AKƨAJ�`AI��AI"�AHz�AGAG�PAG`BAF��AF  AD-AB�yA@�`A@bNA@1A>��A>1'A="�A:$�A5�wA3�A1ƨA.��A,��A+S�A)��A'��A&�DA%�A#��A"�HA"JA!�A�AA�A��A��Ap�A�A�\A�FAn�Al�A�^An�A�A`BAM�AXA�;AJA&�A
�RA	x�An�A�;AXA��AVA9XA-A�A\)A"�A��A~�A{A�-A|�@��`@�bN@��@�ƨ@��\@�p�@���@�-@�(�@�S�@��#@�Ĝ@�n�@�Q�@�ff@�A�@�?}@�
=@���@ݲ-@ݡ�@�V@܋D@���@��@�/@ش9@�I�@���@�~�@�7L@�&�@�(�@Ӆ@�@�^5@�G�@�Q�@�|�@���@·+@�v�@�M�@͡�@̬@ʟ�@�/@Ȭ@�A�@�(�@ǅ@���@ƸR@�ff@�?}@���@Ĵ9@�K�@��#@���@�G�@�V@���@���@�t�@�C�@���@�V@�$�@��T@���@�?}@��@��F@�"�@���@���@��/@�(�@��F@�\)@��H@��\@�V@�@�p�@�t�@�ff@�/@��@�Q�@��;@�
=@�@���@�
=@�$�@��7@�O�@�1@���@�=q@�@�O�@���@�z�@�1'@��@��@��
@���@�|�@�S�@�+@�@���@��@��!@��^@��`@��@���@�  @�|�@�S�@�S�@��P@��F@���@���@��;@�A�@��j@�J@���@���@�V@�ff@��+@�v�@�E�@�{@��@��-@�x�@�/@��j@�bN@�1'@��w@��F@��P@�l�@�ȴ@���@���@��7@��7@�%@�Ĝ@���@�A�@�r�@�r�@�Z@��m@�
=@��@�~�@�$�@��@��h@�x�@�O�@��@���@��`@���@��j@��u@�9X@� �@�b@��;@�ƨ@��w@��w@��F@�|�@�;d@��@���@�ȴ@��R@��!@�x�@��u@�1'@�b@��m@��;@��
@�ƨ@���@�l�@�+@���@��\@�n�@�M�@�J@���@��@��@��@��#@��-@���@��@�`B@�7L@���@���@��@�bN@��@��@�ƨ@���@�l�@�"�@���@��H@���@�ff@�5?@�J@���@��#@�@���@���@��h@�x�@�?}@�%@�Ĝ@���@��@�Z@�9X@��@��w@���@�t�@�K�@�ȴ@�$�@��@�@�x�@��@���@��u@�Z@�A�@�1'@� �@��@��F@��@���@��P@��@�|�@��\@�{@��@��#@���@�hs@��@���@��@��`@��/@���@�Ĝ@� �@��P@�K�@���@�ȴ@�~�@��@���@��@�@���@���@�hs@�&�@��`@�Ĝ@���@�j@��@�ƨ@���@�C�@���@��\@��+@�V@�J@��^@��@�hs@�&�@��j@���@���@���@���@���@���@��u@�z�@�@~�+@~{@}�T@}�-@}O�@|�@|I�@|1@{�
@{��@{33@z�H@z��@z�\@z=q@y��@y��@yhs@yX@y&�@x�`@x�9@x �@w�w@w�P@w\)@v�y@vE�@v$�@u�T@u@u�h@u�@uO�@t��@tj@t�@s�F@s33@so@r=q@q��@p��@p�@pQ�@p1'@pb@p  @o�@o\)@n�R@nv�@nv�@nV@m�@m��@l��@l9X@k��@k�@jM�@i�^@i7L@h��@g�@fE�@f{@e�T@eO�@d�@dI�@d�@c��@c"�@b�\@bn�@b=q@b�@a��@a�7@a&�@`��@`A�@_l�@_
=@^�@^@]�@\�/@\9X@[�
@[�F@[��@[��@[��@[�@[33@Zn�@ZJ@Y�@Y�7@Y�@X  @W�P@WK�@W
=@Vȴ@VV@V$�@T��@TZ@T�@R��@Q��@Q7L@Q�@P��@Pr�@P1'@O��@OK�@O;d@O�@O
=@N�y@N�R@N��@N��@N��@N�+@N{@Mp�@MO�@LI�@Kt�@K@J�H@J�!@J~�@J~�@Jn�@JM�@J-@JJ@I�#@I�7@I%@H �@G��@G|�@G\)@G;d@G�@F��@F��@FV@F{@E��@E�-@E�h@E�@Ep�@EO�@D�/@D(�@C�m@C�F@C�@B�H@B�@A�#@A��@A��@@��@?��@?�@?|�@?\)@?;d@?+@>�@>5?@=�@=@=p�@=p�@=p�@=O�@<�@<�j@<��@<j@<I�@<�@;�
@;ƨ@;��@;�@;C�@;@:��@:��@:�\@:�\@:^5@:-@9��@9�#@9��@9�^@9��@9x�@9G�@9�@8��@8�9@8bN@81'@8b@7|�@7
=@6�@6�R@6��@6V@6{@5�T@5@5@5�-@5�-@5��@5�@5`B@5?}@4�/@4��@4�D@4z�@4I�@4(�@3��@3�F@3dZ@3o@2n�@1�#@1X@1�@1%@0��@0�`@0�`@0Ĝ@0�9@0�u@0bN@/�w@.�R@.E�@.E�@.E�@-�T@-O�@,�j@,1@+��@+t�@+dZ@*�H@*�\@*n�@*J@)x�@)X@)G�@)&�@(��@(bN@(A�@(b@'�@'��@'K�@&ȴ@&v�@&V@&5?@%�@%@%��@%�@%V@$z�@$�@#�F@#C�@#@"�@"��@"~�@"M�@"-@!��@!��@!�@!x�@ �9@ �@ 1'@��@l�@;d@�y@��@V@E�@E�@$�@�@`B@V@�@��@j@Z@I�@�@��@��@t�@S�@C�@"�@@�H@M�@J@�^@�7@x�@X@&�@%@�@  @�@K�@�@��@�R@��@ff@E�@$�@$�@$�@@��@�h@p�@�@j@I�@9X@(�@�@��@�
@��@��@�@�@t�@t�@S�@��@�!@~�@~�@n�@n�@=q@�@�@��@��@��@��@�7@x�@X@��@�u@�u@�@r�@r�@r�@bN@bN@bN@bN@bN@Q�@A�@A�@1'@1'@1'@b@  @��@�P@�@ȴ@�R@��@v�@ff@ff@ff@ff@V@E�@5?@@��@@�-@�-@��@�@`B@/@V@�@��@�j@�@z�@Z@9X@9X@�@ƨ@��@�@t�@S�@"�@@
�H@
��@
��@
�\@
n�@
^5@
�@	�@	��@	��@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�#B�#B�#B�#B�#B�#B�#B�#B�#B�B�#B�#B�B�B�B8RBjBo�Bo�Bo�Bl�Bl�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bp�Bp�Bq�Bq�Bo�Bo�Bo�Bs�Bu�B�B�uB�uB�hB��B��B�B�?B�^BÖB��B��B��B��B��B��B��BB�qB�XB�9B�B��B��B�uB�PB�DB{�Bo�BhsBdZB\)B?}B8RB5?B2-B%�B�BhBDB
=BB��B�B�B�BB��B�qB�LB��B��B��B��B��B�Bo�BZBP�BF�B6FB�B\B
=B
��B
�B
�BB
��B
B
�9B
��B
~�B
_;B
P�B
C�B
7LB
 �B
{B
  B	�mB	�B	ȴB	ŢB	�!B	��B	��B	��B	��B	�7B	�B	�B	�B	�B	�B	~�B	z�B	w�B	u�B	p�B	hsB	bNB	^5B	ZB	T�B	N�B	I�B	F�B	A�B	9XB	6FB	49B	0!B	-B	+B	%�B	"�B	�B	oB	1B	B	B��B��B�B�NB��BB�RB�!B��B��B��B��B��B�uB�\B�JB�DB�1B�B�B� B|�By�Bw�Bw�Bv�Bt�Bq�Br�Bn�Bm�Bl�Bl�BiyBffBaHB^5B^5B`BBaHBbNBcTBcTBe`BdZBdZBe`BgmBffBffBffBdZBcTBgmB_;B^5B]/B]/B]/B[#B[#BT�BR�BQ�BT�B`BBdZBcTBbNBbNBgmBjBp�Bt�Bt�Bu�Bv�Bw�Bv�Bw�Bz�B|�B{�B~�B�=B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�3B�3B�3B�RB�^B�dB�dB�jB�}B��B��B��BBŢBŢBƨBǮBɺB��B��B��B��B�B�5B�HB�ZB�fB�sB�B�B�B�B��B��B��B��B��B��B��B��B	
=B	hB	oB	{B	{B	�B	�B	�B	�B	�B	!�B	$�B	'�B	'�B	+B	-B	.B	.B	/B	5?B	7LB	9XB	:^B	;dB	=qB	C�B	F�B	J�B	L�B	S�B	W
B	ZB	^5B	cTB	ffB	gmB	hsB	k�B	n�B	y�B	�B	�DB	�PB	�\B	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�FB	�LB	�XB	�^B	�wB	�}B	��B	��B	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�#B	�)B	�;B	�NB	�ZB	�sB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
VB
PB
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
%�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
5?B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
:^B
9XB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
W
B
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
XB
XB
XB
YB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
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
e`B
e`B
e`B
e`B
ffB
ffB
ffB
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
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
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
p�B
p�B
p�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
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
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�=B�=B�=B�=B�=B�=B�=B�=B�=B�7B�=B�WB�kB��B�/B9XBjKBo�Bo�BpBl�Bl�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bp�Bp�Bq�Bq�Bp!BqBr�BvBzB��B��B�KB�SB�'B�B�9B�8B��B�+B�&B�B��B�oB��B��B�B�B��B�B��B�cB�4B�5B��B��B�pB~�BqABj�BhXBabBAoB9�B6�B4B'�B�B�B�B�B�B��B�B�/B�:B�:B�.B��B��B�&B�B�OB�EB��Bq�B[WBR�BJ	B9�B�B B�B iB
��B
�B
уB
�SB
�B
�-B
��B
aHB
S&B
E�B
:^B
#�B
B
B	�B	�	B	�^B	ɺB	��B	��B	�2B	�`B	�B	��B	�SB	�SB	�gB	��B	��B	�iB	{�B	x�B	wfB	r-B	jKB	cTB	_pB	[qB	V�B	O�B	J�B	G�B	B�B	:DB	72B	5%B	0�B	-�B	+�B	'�B	%B	�B	�B		B	B	�B�"B�DB��B�RB��BňB��B��B��B�B��B��B��B�MB��B��B��B�rB�+B�gB�AB~�Bz�Bx�ByXBx�BvzBs�BtnBo�Bn�Bn/BnIBk�Bh�Bb�B_VB_�Ba�Bb4Bc BdBdZBe�Bd�Bd�Bf2Bg�BgBgBgmBe�Bf�BjB_�B^�B]�B^OB^jB]/B^jBVSBS�BS&BVSBa�Be�BeBd&BdZBh�BkQBqBuBuZBvzBxBx�Bw�BxRB{dB}�B}B� B��B�@B�?B�KB�QB��B�qB�dB�pB�B�B�-B��B��B�mB�B��B��B�WB��B��B��B��B�B��B��B�TB�>B��B��B��B�<B��B��B��B��B�B��B�B�B�1B�XB˒B�}B҉B��BںB��B��B��B��B��B��B�B�cB�B��B��B�FB�2B�FB�lB��B�B	
�B	 B	B	�B	�B	sB	B	
B	B	CB	"4B	%,B	(>B	(XB	+6B	-wB	.cB	.}B	/�B	5tB	7�B	9�B	:�B	<6B	>(B	C�B	GB	K^B	M6B	T,B	W?B	ZB	^OB	c�B	f�B	gRB	hXB	k6B	m�B	y�B	�MB	�xB	�jB	�vB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	� B	�2B	��B	��B	�>B	�>B	�KB	��B	��B	��B	��B	�xB	��B	��B	�B	�UB	��B	�B	�)B	�(B	�4B	�B	�B	�4B	�B	�B	�B	� B	�:B	�:B	�&B	�FB	�FB	�2B	�B	�9B	�9B	�sB	�eB	�qB	�]B	�VB	�B	��B	�_B	�0B	��B	�B	�B	�B	��B	�B	��B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�B	�	B	�	B	�*B	�DB	�0B	�6B	�6B	�<B	�BB	�BB	�HB	�cB	�cB
 4B
 4B
 OB
;B
UB
AB
AB
AB
GB
GB
GB
GB
GB
gB
�B
�B
tB
tB
tB
_B
zB
zB
�B
fB
�B
�B
	�B

�B

�B

�B
�B
�B
�B
�B
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
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
IB
5B
B
�B
�B
B
!B
�B
�B
 B
�B
 'B
 �B
 �B
 �B
!B
"B
!�B
!�B
!�B
!�B
!�B
!�B
"B
#B
#B
#B
$@B
$&B
%,B
%B
%,B
%B
%B
%B
%,B
&B
&B
&2B
'RB
'B
&LB
(>B
'RB
(>B
(>B
($B
($B
(>B
($B
(>B
(>B
)*B
)B
)*B
)DB
)*B
)_B
*KB
*eB
*eB
*B
+QB
+QB
,�B
,�B
-�B
.IB
/OB
/iB
/iB
/iB
0UB
0UB
0�B
1vB
1[B
1[B
1[B
1[B
1vB
1�B
1vB
2|B
2�B
3�B
3�B
3�B
4�B
5tB
4�B
5tB
5tB
5ZB
5ZB
5tB
5tB
5�B
5�B
6zB
6zB
6�B
6�B
6�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
:�B
9�B
:�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
@ B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
GB
F�B
F�B
F�B
G�B
HB
H�B
H�B
IB
IB
J#B
J�B
J�B
J�B
J�B
KB
KB
LB
K�B
MB
MB
L�B
L�B
MB
MB
MB
MB
N"B
N"B
NB
NB
NB
NB
NB
N"B
OB
OB
O(B
N�B
N�B
OB
OB
OB
PB
O�B
O�B
PB
PB
PB
PB
PB
P.B
QB
Q4B
Q4B
Q4B
R B
R B
R B
R B
R B
S&B
S&B
S&B
SB
SB
SB
SB
S@B
S&B
S&B
S&B
T,B
TB
TFB
T,B
T,B
T,B
U2B
U2B
UgB
UMB
VSB
WYB
W?B
W$B
W?B
X+B
XEB
XEB
XEB
XEB
XEB
XyB
XyB
YKB
Z7B
ZQB
ZkB
Z�B
[qB
[qB
\]B
\]B
\]B
\xB
]dB
]dB
]~B
^�B
^�B
^jB
^jB
^jB
_pB
_pB
_pB
_pB
_�B
_�B
`�B
`vB
`�B
`vB
a|B
a�B
a�B
a|B
a�B
b�B
b�B
c�B
c�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
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
h�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
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
p�B
p�B
p�B
q�B
p�B
rB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
tB
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
wB
wB
v�B
wB
w�B
w�B
w�B
w�B
w�B
xB
w�B
xB
xB
xB
w�B
w�B
w�B
w�B
xB
y	B
y	B
y	B
y	B
y	B
x�B
y	B
z*B
zB
zB
y�B
zB
zB
{B
{0B
z�B
{0B
{B
{B
|B
|6B
|B
|B
|B
|6B
|B
}"B
}"B
}"B
~(B
~11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<4;@<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.23(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611150036012016111500360120161115003601201806221304502018062213045020180622130450201804050704472018040507044720180405070447  JA  ARFMdecpA19c                                                                20161111093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161111003522  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161111003522  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161111003522  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161111003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161111003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161111003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161111003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161111003524  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161111003524                      G�O�G�O�G�O�                JA  ARUP                                                                        20161111013212                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161111153300  CV  JULD            G�O�G�O�F�ɕ                JM  ARCAJMQC2.0                                                                 20161114153601  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161114153601  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220447  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040450  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                