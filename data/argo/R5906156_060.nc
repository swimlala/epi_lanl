CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-12-05T10:00:59Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20201205100059  20201205100059  5906156 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               <A   AO  7912                            2B  A   NAVIS_A                         1020                            170425                          863 @�L�R���1   @�L��>�F@8�ě��T�cC��%1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         <A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��R@��RA\)A;\)A[\)A{\)A��A��A��A��AͮAݮA��A��B�
B�
B�
B�
B&�
B.�
B6�
B>�
BF�
BN�
BV�
B^�
Bf�
Bn�
Bv�
B~�
B�k�B���B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D mqD �qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD	mqD	�qD
mqD
�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD mqD �D!mqD!�qD"mqD"�qD#mqD#�qD$mqD$�qD%mqD%�qD&mqD&�qD'mqD'�qD(mqD(�qD)mqD)�qD*mqD*�qD+mqD+�qD,mqD,�qD-mqD-�qD.mqD.�qD/mqD/�qD0mqD0�qD1mqD1�qD2mqD2�qD3mqD3�qD4mqD4�qD5mqD5�qD6mqD6�qD7mqD7�qD8mqD8�qD9mqD9�qD:mqD:�qD;mqD;�qD<mqD<�qD=mqD=�qD>mqD>�qD?mqD?�qD@mqD@�qDAmqDA�qDBmqDB�qDCmqDC�qDDmqDD�qDEmqDE�qDFmqDF�DGmqDG�qDHmqDH�qDImqDI�qDJmqDJ�qDKmqDK�qDLmqDL�qDMmqDM�qDNmqDN�qDOmqDO�qDPmqDP�qDQmqDQ�qDRmqDR�qDSmqDS�qDTmqDT�qDUmqDU�qDVmqDV�qDWmqDW�qDXmqDX�qDYmqDY�qDZmqDZ�qD[mqD[�qD\mqD\�qD]mqD]�qD^mqD^�qD_mqD_�qD`mqD`�qDamqDa�qDbmqDb�qDcmqDc�qDdmqDd�qDemqDe�qDfmqDf�qDgmqDg�qDhmqDh�qDimqDi�qDjmqDj�qDkmqDk�qDlmqDl�qDmmqDm�qDnmqDn�qDomqDo�qDpmqDp�qDqmqDq�qDrmqDr�qDsmqDs�qDtmqDt�qDumqDu�qDvmqDv�qDwmqDw�qDxmqDx�qDymqDy�qDzmqDz�qD{mqD{�qD|mqD|�qD}mqD}�qD~mqD~�qDmqD�qD�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�s�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�y�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�9�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D¶�D���D�6�D�v�Dö�D���D�6�D�v�DĶ�D���D�6�D�v�DŶ�D���D�6�D�v�Dƶ�D���D�6�D�v�DǶ�D���D�6�D�v�Dȶ�D���D�6�D�v�Dɶ�D���D�6�D�v�Dʶ�D���D�6�D�v�D˶�D���D�6�D�v�D̶�D���D�6�D�v�DͶ�D���D�6�D�v�Dζ�D���D�6�D�v�D϶�D���D�6�D�v�Dж�D���D�6�D�v�DѶ�D���D�6�D�v�DҶ�D���D�6�D�v�DӶ�D���D�6�D�v�DԶ�D���D�6�D�v�Dն�D���D�6�D�v�Dֶ�D���D�6�D�v�D׶�D���D�6�D�v�Dض�D���D�6�D�v�Dٶ�D���D�6�D�v�Dڶ�D���D�6�D�v�D۶�D���D�6�D�v�Dܶ�D���D�6�D�v�Dݶ�D���D�6�D�v�D޶�D���D�6�D�v�D߶�D���D�6�D�v�DමD���D�6�D�v�DᶸD���D�6�D�v�DⶸD���D�6�D�v�D㶸D���D�6�D�v�D䶸D���D�6�D�v�D嶸D���D�6�D�v�D涸D���D�6�D�v�D綸D���D�6�D�v�D趸D���D�6�D�v�D鶸D���D�6�D�v�D궸D���D�6�D�v�D붸D���D�6�D�v�D춸D���D�6�D�v�D���D���D�6�D�v�DD���D�6�D�v�DﶸD���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�9�D�pR11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AȃAȉ7AȍPAȗ�Aș�Aș�Aȗ�Aȗ�AȑhAȑhAȍPA�~�A�ffA�7LA���A��/A���AǼjAǴ9AǮAǬAǥ�AǙ�AǓuAǍPAǉ7AǃA�O�A�(�A�/A� �A��A�$�A�+A��A��A���Aƥ�A�9XA�33A�~�A�M�A��9A�"�A�l�A�M�A�\)A��A�VA�ZA���A���A��A���A�bNA�n�A��A���A��
A�{A�1'A��PA��A��uA��FA�JA�A���A�dZA���A�M�A���A���A�=qA�JA��TA���A�M�A�p�A��DA���A�7LA��A�A��;A��A��A��FA�jA�A��#A��A�=qA���A�z�A���A�=qA��A�A~�A};dA}
=A|5?Az��AyhsAxE�Au��At(�AsAqO�Ap{AnbNAl��Aj{AhbAg�Aet�Ab�yA`��A^A�A\�9A[
=AY&�AW��AU��AS�-ARI�AP��AOp�AN1'AMVAKK�AJ �AIp�AG+AE\)AC�mAA�A?�-A>A<�DA;�#A:�DA97LA8bNA7�A5��A4�A2��A1p�A0ffA.��A-%A+�A*��A)p�A(��A(9XA'��A';dA&�9A& �A%��A$5?A#A#?}A"�!A!�A!�wA!dZA ��A 1'A�TA��A�`A�DAbNA1AS�A�jA�A�7A��AA�A��A��A�FA`BA��A�wA�yA��AVA�hA��AG�A�AjAt�A��An�A��AO�A
=A
 �A	XA�/A��AjA�-A�A��A�
AĜAE�A��A �HA A�@�ƨ@��@���@�v�@��w@��\@��`@�@�=q@@�X@���@�R@�@旍@��#@�h@�F@�x�@�bN@�K�@�$�@��@�r�@ە�@���@�Ĝ@�|�@�
=@�n�@�/@���@��H@���@�l�@��@ΰ!@�=q@͡�@�V@�A�@ʰ!@�@�/@�ƨ@�
=@��@ě�@�K�@�n�@��@��h@���@�(�@�33@�M�@��h@�  @���@��^@��u@���@�^5@��-@�V@��9@��@���@��P@��y@��@��-@��@�V@�b@��y@�v�@�J@�&�@�j@��;@�o@�V@�J@��@���@���@�
=@�$�@��^@�hs@��/@�b@�t�@��@��!@�^5@�J@��h@�`B@��@��`@�z�@���@�ƨ@��F@�t�@��@�n�@�ff@�ff@��+@���@��T@�`B@���@�o@��7@�I�@���@�K�@���@�;d@���@�-@��!@��y@�v�@���@��u@�l�@�^5@�E�@�$�@���@���@�@���@�p�@��@���@�dZ@��H@�ff@��@���@�J@�5?@���@��@�dZ@��@��H@�ȴ@��;@��-@���@���@�^5@��H@�n�@��\@��@�V@��h@�{@�V@�ff@��^@�%@���@�  @�Z@���@�j@�"�@�^5@��^@�n�@�"�@�+@��y@�J@���@�`B@�&�@���@���@�Q�@� �@��;@��w@���@�K�@���@��\@���@���@�v�@�{@���@�`B@�7L@��@��/@��j@�r�@� �@�1@��w@���@���@��w@���@�|�@�C�@�@���@��@���@���@�v�@���@��^@���@��@���@�I�@�I�@� �@� �@�  @�b@���@��F@��@�|�@�"�@�ff@��@��^@��@�hs@�O�@�&�@���@���@���@��j@��9@��j@�Ĝ@���@��@�A�@;d@
=@~�y@}��@|�@|�@}V@|�j@{��@{�@z��@z-@y��@yG�@y�#@z^5@y��@xbN@w�;@w;d@v��@vE�@u�T@u�T@u�T@u�@up�@u��@u�@uO�@t�/@t�@tZ@t(�@t(�@t�@st�@sdZ@sdZ@s�@s��@sS�@so@s@r�H@r��@q��@q�#@q��@qG�@p�`@p1'@o�;@o|�@o;d@nV@m�T@m��@m��@m�@m`B@m�@l��@l��@lI�@lZ@l1@kt�@j��@j�\@j�\@j�@iG�@h��@h�9@h�@hQ�@h  @g�@g��@g��@gl�@g+@f��@fȴ@f��@f@e�-@e@eO�@e/@eV@d�/@d�D@dj@c�m@ct�@b��@b-@aX@`��@`Ĝ@`r�@`Q�@_�;@_�@_K�@_+@_+@^�@^�+@^V@]��@]O�@\�@\��@\z�@\�@[��@[S�@[@Z��@Z�@Yhs@Yhs@YG�@XĜ@XQ�@X  @W��@W|�@WK�@W;d@Vȴ@V�+@V�+@V�+@V�+@Vv�@Vff@V$�@U�-@Up�@U?}@U?}@T��@T��@Tz�@T9X@S�F@St�@S"�@R�!@Rn�@R=q@R�@Q�^@Qhs@QG�@P��@P�`@PbN@O��@OK�@O;d@O;d@O;d@N�y@N��@N5?@N@M�@M�-@MO�@L�@L(�@K�@Kt�@Ko@J��@J��@J�!@J�\@J^5@I��@I�@I��@Ix�@I&�@H�`@H�9@H�@HA�@Hb@G�w@Gl�@G+@G
=@F��@F�R@F$�@E@E�-@E`B@EV@D�j@DZ@D�@C��@Cƨ@C�F@C��@CS�@Co@B��@B^5@B�@A�7@A%@A%@@�`@@�`@@��@@��@@Ĝ@@�u@@  @?|�@?+@?�@>�y@>��@>�+@>ff@>@=�T@=�-@=�@=V@<�@<�@<�/@<�j@<�@<z�@<1@;�m@;�F@;��@;dZ@;o@:�@:��@:^5@:=q@:=q@:-@9�^@9&�@8��@8�9@8Q�@8Q�@8b@7��@7|�@7
=@6�@6v�@6E�@65?@6@5�T@5��@5p�@5�@4�@4��@4��@4I�@3��@3�m@3dZ@3dZ@3"�@2�@2��@2-@2�@1��@1�7@1hs@1&�@0��@0�`@0�`@0r�@0A�@0 �@0b@/�;@/��@/|�@/K�@/�@.ȴ@.�+@.5?@-�@-?}@-?}@-`B@-?}@-V@,�/@,�D@,9X@+ƨ@+��@+"�@*��@*~�@*n�@*^5@*^5@*-@)��@)�7@)7L@)%@(��@( �@'�P@'K�@'K�@'�@&�@&�@&ȴ@&�+@&V@&5?@%�@%��@%�@%p�@%O�@%/@$�D@$9X@$�@$(�@$�@$1@#��@#��@#��@#�@#S�@#@"�!@"�\@"�\@"M�@"M�@"=q@!��@!��@!G�@ �`@ ��@ �u@ 1'@ b@��@�@��@��@\)@��@�R@v�@ff@$�@��@@��@`B@�@�@�j@�D@Z@�
@ƨ@��@t�@C�@"�@�@��@�\@-@�@7L@��@�9@�u@A�@b@  @�;@�w@��@|�@K�@+@�@�R@�+@5?@�T@�-@�@?}@�@V@�@�@��@j@9X@�m@�F@t�@33@�@��@�\@n�@-@J@�@��@X@&�@�`@Ĝ@�9@�u@A�@�@�;@�;@�P@|�@l�@K�@ȴ@��@�+@v�@E�@5?@$�@�T@�-@�@p�@`B@?}@��@��@�j@�@��@z�@j@I�@9X@(�@1@��@��@�@dZ@C�@@
�\@
M�@
-@
J@	�@	�^@	�7@	hs@	X@	X@	G�@	&�@�`@��@A�@�@�;@l�@�@
=@��@ȴ@�R@�R@��@�+@�+@�+@v�@V@$�@�@�h@p�@?}@/@�@�@��@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AȃAȉ7AȍPAȗ�Aș�Aș�Aȗ�Aȗ�AȑhAȑhAȍPA�~�A�ffA�7LA���A��/A���AǼjAǴ9AǮAǬAǥ�AǙ�AǓuAǍPAǉ7AǃA�O�A�(�A�/A� �A��A�$�A�+A��A��A���Aƥ�A�9XA�33A�~�A�M�A��9A�"�A�l�A�M�A�\)A��A�VA�ZA���A���A��A���A�bNA�n�A��A���A��
A�{A�1'A��PA��A��uA��FA�JA�A���A�dZA���A�M�A���A���A�=qA�JA��TA���A�M�A�p�A��DA���A�7LA��A�A��;A��A��A��FA�jA�A��#A��A�=qA���A�z�A���A�=qA��A�A~�A};dA}
=A|5?Az��AyhsAxE�Au��At(�AsAqO�Ap{AnbNAl��Aj{AhbAg�Aet�Ab�yA`��A^A�A\�9A[
=AY&�AW��AU��AS�-ARI�AP��AOp�AN1'AMVAKK�AJ �AIp�AG+AE\)AC�mAA�A?�-A>A<�DA;�#A:�DA97LA8bNA7�A5��A4�A2��A1p�A0ffA.��A-%A+�A*��A)p�A(��A(9XA'��A';dA&�9A& �A%��A$5?A#A#?}A"�!A!�A!�wA!dZA ��A 1'A�TA��A�`A�DAbNA1AS�A�jA�A�7A��AA�A��A��A�FA`BA��A�wA�yA��AVA�hA��AG�A�AjAt�A��An�A��AO�A
=A
 �A	XA�/A��AjA�-A�A��A�
AĜAE�A��A �HA A�@�ƨ@��@���@�v�@��w@��\@��`@�@�=q@@�X@���@�R@�@旍@��#@�h@�F@�x�@�bN@�K�@�$�@��@�r�@ە�@���@�Ĝ@�|�@�
=@�n�@�/@���@��H@���@�l�@��@ΰ!@�=q@͡�@�V@�A�@ʰ!@�@�/@�ƨ@�
=@��@ě�@�K�@�n�@��@��h@���@�(�@�33@�M�@��h@�  @���@��^@��u@���@�^5@��-@�V@��9@��@���@��P@��y@��@��-@��@�V@�b@��y@�v�@�J@�&�@�j@��;@�o@�V@�J@��@���@���@�
=@�$�@��^@�hs@��/@�b@�t�@��@��!@�^5@�J@��h@�`B@��@��`@�z�@���@�ƨ@��F@�t�@��@�n�@�ff@�ff@��+@���@��T@�`B@���@�o@��7@�I�@���@�K�@���@�;d@���@�-@��!@��y@�v�@���@��u@�l�@�^5@�E�@�$�@���@���@�@���@�p�@��@���@�dZ@��H@�ff@��@���@�J@�5?@���@��@�dZ@��@��H@�ȴ@��;@��-@���@���@�^5@��H@�n�@��\@��@�V@��h@�{@�V@�ff@��^@�%@���@�  @�Z@���@�j@�"�@�^5@��^@�n�@�"�@�+@��y@�J@���@�`B@�&�@���@���@�Q�@� �@��;@��w@���@�K�@���@��\@���@���@�v�@�{@���@�`B@�7L@��@��/@��j@�r�@� �@�1@��w@���@���@��w@���@�|�@�C�@�@���@��@���@���@�v�@���@��^@���@��@���@�I�@�I�@� �@� �@�  @�b@���@��F@��@�|�@�"�@�ff@��@��^@��@�hs@�O�@�&�@���@���@���@��j@��9@��j@�Ĝ@���@��@�A�@;d@
=@~�y@}��@|�@|�@}V@|�j@{��@{�@z��@z-@y��@yG�@y�#@z^5@y��@xbN@w�;@w;d@v��@vE�@u�T@u�T@u�T@u�@up�@u��@u�@uO�@t�/@t�@tZ@t(�@t(�@t�@st�@sdZ@sdZ@s�@s��@sS�@so@s@r�H@r��@q��@q�#@q��@qG�@p�`@p1'@o�;@o|�@o;d@nV@m�T@m��@m��@m�@m`B@m�@l��@l��@lI�@lZ@l1@kt�@j��@j�\@j�\@j�@iG�@h��@h�9@h�@hQ�@h  @g�@g��@g��@gl�@g+@f��@fȴ@f��@f@e�-@e@eO�@e/@eV@d�/@d�D@dj@c�m@ct�@b��@b-@aX@`��@`Ĝ@`r�@`Q�@_�;@_�@_K�@_+@_+@^�@^�+@^V@]��@]O�@\�@\��@\z�@\�@[��@[S�@[@Z��@Z�@Yhs@Yhs@YG�@XĜ@XQ�@X  @W��@W|�@WK�@W;d@Vȴ@V�+@V�+@V�+@V�+@Vv�@Vff@V$�@U�-@Up�@U?}@U?}@T��@T��@Tz�@T9X@S�F@St�@S"�@R�!@Rn�@R=q@R�@Q�^@Qhs@QG�@P��@P�`@PbN@O��@OK�@O;d@O;d@O;d@N�y@N��@N5?@N@M�@M�-@MO�@L�@L(�@K�@Kt�@Ko@J��@J��@J�!@J�\@J^5@I��@I�@I��@Ix�@I&�@H�`@H�9@H�@HA�@Hb@G�w@Gl�@G+@G
=@F��@F�R@F$�@E@E�-@E`B@EV@D�j@DZ@D�@C��@Cƨ@C�F@C��@CS�@Co@B��@B^5@B�@A�7@A%@A%@@�`@@�`@@��@@��@@Ĝ@@�u@@  @?|�@?+@?�@>�y@>��@>�+@>ff@>@=�T@=�-@=�@=V@<�@<�@<�/@<�j@<�@<z�@<1@;�m@;�F@;��@;dZ@;o@:�@:��@:^5@:=q@:=q@:-@9�^@9&�@8��@8�9@8Q�@8Q�@8b@7��@7|�@7
=@6�@6v�@6E�@65?@6@5�T@5��@5p�@5�@4�@4��@4��@4I�@3��@3�m@3dZ@3dZ@3"�@2�@2��@2-@2�@1��@1�7@1hs@1&�@0��@0�`@0�`@0r�@0A�@0 �@0b@/�;@/��@/|�@/K�@/�@.ȴ@.�+@.5?@-�@-?}@-?}@-`B@-?}@-V@,�/@,�D@,9X@+ƨ@+��@+"�@*��@*~�@*n�@*^5@*^5@*-@)��@)�7@)7L@)%@(��@( �@'�P@'K�@'K�@'�@&�@&�@&ȴ@&�+@&V@&5?@%�@%��@%�@%p�@%O�@%/@$�D@$9X@$�@$(�@$�@$1@#��@#��@#��@#�@#S�@#@"�!@"�\@"�\@"M�@"M�@"=q@!��@!��@!G�@ �`@ ��@ �u@ 1'@ b@��@�@��@��@\)@��@�R@v�@ff@$�@��@@��@`B@�@�@�j@�D@Z@�
@ƨ@��@t�@C�@"�@�@��@�\@-@�@7L@��@�9@�u@A�@b@  @�;@�w@��@|�@K�@+@�@�R@�+@5?@�T@�-@�@?}@�@V@�@�@��@j@9X@�m@�F@t�@33@�@��@�\@n�@-@J@�@��@X@&�@�`@Ĝ@�9@�u@A�@�@�;@�;@�P@|�@l�@K�@ȴ@��@�+@v�@E�@5?@$�@�T@�-@�@p�@`B@?}@��@��@�j@�@��@z�@j@I�@9X@(�@1@��@��@�@dZ@C�@@
�\@
M�@
-@
J@	�@	�^@	�7@	hs@	X@	X@	G�@	&�@�`@��@A�@�@�;@l�@�@
=@��@ȴ@�R@�R@��@�+@�+@�+@v�@V@$�@�@�h@p�@?}@/@�@�@��@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B#�B#�B$�B%�B&�B'�B-B33B:^B=qBC�BD�BG�BL�BP�BQ�BP�BP�BN�BN�BJ�BC�BG�BXBjBu�Bl�BhsBffBjB`BBZBYBXBW
BT�BK�BG�B;dB49B2-B+B#�B!�B �B�BVBB��B��B�B�B�yB�fB�NB��B��B�FB��B�1By�Bm�BcTBZBN�BA�B:^B2-B)�B$�B�B
=B
��B
�B
�dB
�B
t�B
ffB
P�B
5?B
 �B
{B
hB
VB
+B	��B	��B	�sB	�5B	�B	��B	��B	ɺB	�}B	�B	��B	��B	�\B	~�B	q�B	dZB	YB	O�B	D�B	;dB	33B	%�B	�B	�B	hB		7B	B��B�B�B�NB�
B��BÖB�FB�B��B��B��B�hB�VB�\B�Bz�Bu�Bp�Bo�BiyBiyBffB_;B]/BZBZB[#B]/B]/B[#B\)BYBYBYBYBXBW
BXBVBT�BS�BR�BR�BP�BP�BP�BN�BO�BL�BL�BJ�BI�BI�BG�BD�BC�BA�B>wB<jB9XB8RB6FB6FB5?B33B33B2-B0!B/B.B-B,B,B(�B'�B&�B#�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B{BoBoBoBoBhBbBbBbB\BVBbB\BbB\BbBbBbBbBbBhBoBhBoBoBuB{B�B�B�B�B�B�B�B�B�B�B�B �B �B"�B$�B'�B(�B)�B+B,B/B1'B2-B33B49B5?B6FB8RB;dB=qB?}BA�BC�BE�BF�BG�BJ�BM�BO�BO�BQ�BT�BXBZBZB\)B^5B`BBbNBdZBdZBffBiyBn�Bp�Bt�Bu�Bw�By�B~�B�B�%B�1B�7B�DB�VB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B�B�FB�^BȴB��B��BȴBƨBŢB��B��B��B��B�#B�HB�HB�HB�HB�/B�/B�BB�NB�HB�TB�mB�B�B�B�B�B�B�B�B��B��B��B	B		7B	\B	�B	�B	�B	�B	'�B	)�B	+B	0!B	+B	+B	/B	/B	5?B	5?B	:^B	?}B	E�B	F�B	G�B	F�B	E�B	H�B	N�B	O�B	O�B	N�B	N�B	S�B	[#B	[#B	\)B	]/B	`BB	aHB	cTB	dZB	ffB	iyB	iyB	jB	k�B	l�B	n�B	o�B	r�B	u�B	z�B	z�B	z�B	z�B	z�B	z�B	{�B	}�B	� B	� B	� B	� B	�B	�%B	�JB	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�FB	�FB	�LB	�LB	�RB	�dB	�jB	�qB	�qB	�wB	�}B	��B	B	B	ÖB	ÖB	ǮB	��B	��B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�)B	�;B	�;B	�/B	�/B	�/B	�5B	�BB	�BB	�HB	�HB	�NB	�TB	�`B	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
	7B

=B

=B

=B
DB
JB
JB
PB
PB
VB
VB
\B
\B
\B
hB
hB
oB
uB
uB
uB
{B
�B
�B
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
�B
�B
�B
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
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
,B
-B
.B
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
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
8RB
8RB
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
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
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
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
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
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
W
B
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
]/B
]/B
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
_;B
_;B
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
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
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
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
k�B
k�B
k�B
k�B
k�B
l�B
k�B
l�B
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
n�B
n�B
n�B
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
q�B
r�B
q�B
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
t�B
t�B
t�B
t�B
t�B
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
x�B
x�B
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
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B#�B#�B$�B%�B&�B'�B-B33B:^B=qBC�BD�BG�BL�BP�BQ�BP�BP�BN�BN�BJ�BC�BG�BXBjBu�Bl�BhsBffBjB`BBZBYBXBW
BT�BK�BG�B;dB49B2-B+B#�B!�B �B�BVBB��B��B�B�B�yB�fB�NB��B��B�FB��B�1By�Bm�BcTBZBN�BA�B:^B2-B)�B$�B�B
=B
��B
�B
�dB
�B
t�B
ffB
P�B
5?B
 �B
{B
hB
VB
+B	��B	��B	�sB	�5B	�B	��B	��B	ɺB	�}B	�B	��B	��B	�\B	~�B	q�B	dZB	YB	O�B	D�B	;dB	33B	%�B	�B	�B	hB		7B	B��B�B�B�NB�
B��BÖB�FB�B��B��B��B�hB�VB�\B�Bz�Bu�Bp�Bo�BiyBiyBffB_;B]/BZBZB[#B]/B]/B[#B\)BYBYBYBYBXBW
BXBVBT�BS�BR�BR�BP�BP�BP�BN�BO�BL�BL�BJ�BI�BI�BG�BD�BC�BA�B>wB<jB9XB8RB6FB6FB5?B33B33B2-B0!B/B.B-B,B,B(�B'�B&�B#�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B{BoBoBoBoBhBbBbBbB\BVBbB\BbB\BbBbBbBbBbBhBoBhBoBoBuB{B�B�B�B�B�B�B�B�B�B�B�B �B �B"�B$�B'�B(�B)�B+B,B/B1'B2-B33B49B5?B6FB8RB;dB=qB?}BA�BC�BE�BF�BG�BJ�BM�BO�BO�BQ�BT�BXBZBZB\)B^5B`BBbNBdZBdZBffBiyBn�Bp�Bt�Bu�Bw�By�B~�B�B�%B�1B�7B�DB�VB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B�B�FB�^BȴB��B��BȴBƨBŢB��B��B��B��B�#B�HB�HB�HB�HB�/B�/B�BB�NB�HB�TB�mB�B�B�B�B�B�B�B�B��B��B��B	B		7B	\B	�B	�B	�B	�B	'�B	)�B	+B	0!B	+B	+B	/B	/B	5?B	5?B	:^B	?}B	E�B	F�B	G�B	F�B	E�B	H�B	N�B	O�B	O�B	N�B	N�B	S�B	[#B	[#B	\)B	]/B	`BB	aHB	cTB	dZB	ffB	iyB	iyB	jB	k�B	l�B	n�B	o�B	r�B	u�B	z�B	z�B	z�B	z�B	z�B	z�B	{�B	}�B	� B	� B	� B	� B	�B	�%B	�JB	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�FB	�FB	�LB	�LB	�RB	�dB	�jB	�qB	�qB	�wB	�}B	��B	B	B	ÖB	ÖB	ǮB	��B	��B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�)B	�;B	�;B	�/B	�/B	�/B	�5B	�BB	�BB	�HB	�HB	�NB	�TB	�`B	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
	7B

=B

=B

=B
DB
JB
JB
PB
PB
VB
VB
\B
\B
\B
hB
hB
oB
uB
uB
uB
{B
�B
�B
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
�B
�B
�B
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
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
,B
-B
.B
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
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
8RB
8RB
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
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
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
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
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
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
W
B
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
]/B
]/B
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
_;B
_;B
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
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
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
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
k�B
k�B
k�B
k�B
k�B
l�B
k�B
l�B
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
n�B
n�B
n�B
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
q�B
r�B
q�B
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
t�B
t�B
t�B
t�B
t�B
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
x�B
x�B
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
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20201205100059                              AO  ARCAADJP                                                                    20201205100059    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20201205100059  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20201205100059  QCF$                G�O�G�O�G�O�0               