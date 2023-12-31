CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-26T00:35:27Z creation;2016-11-26T00:35:29Z conversion to V3.1;2019-12-19T08:24:36Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20161126003527  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               =A   JA  I2_0576_061                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @���@�1   @���Q�n @:�Ov_خ�d��b��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�33A��AffA@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ Dȼ�D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D���D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D��3D� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@'
=@mp�@��@��AA;\)AYA{\)A��A��A��A��AͮAݮA��A��B�
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
Bfp�Bn�
Bv�
B~�
B�k�B�k�B�k�B�k�B���B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D mqD �qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD	mqD	�qD
mqD
�qDmqD�qDmqD�qDg
D�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD mqD �qD!mqD!�qD"mqD"�qD#mqD#�qD$mqD$�qD%mqD%�qD&mqD&�qD'mqD'�qD(mqD(�qD)mqD)�qD*mqD*�qD+mqD+�qD,mqD,�qD-mqD-�qD.mqD.�qD/mqD/�qD0mqD0�qD1mqD1�qD2mqD2�qD3mqD3�qD4mqD4�qD5mqD5�qD6mqD6�qD7mqD7�qD8mqD8�qD9mqD9�qD:mqD:�qD;mqD;�qD<mqD<�qD=s�D=�qD>mqD>�qD?mqD?�qD@mqD@�qDAmqDA�qDBmqDB�qDCmqDC�qDDmqDD�qDEmqDE�qDFmqDF�qDGmqDG�qDHmqDH�qDImqDI�qDJmqDJ�qDKmqDK�qDLmqDL�qDMmqDM�qDNmqDN�qDOmqDO�qDPmqDP�qDQmqDQ�qDRmqDR�qDSmqDS�qDTmqDT�qDUmqDU�qDVmqDV�qDWmqDW�qDXmqDX�qDYmqDY�qDZmqDZ�qD[mqD[�qD\mqD\�qD]mqD]�qD^mqD^�qD_mqD_�qD`mqD`�qDamqDa�qDbmqDb�qDcmqDc�qDdmqDd�qDemqDe�qDfmqDf�qDgmqDg�qDhmqDh�qDimqDi�qDjmqDj�qDkmqDk�qDlmqDl�qDmmqDm�qDnmqDn�qDomqDo�qDpmqDp�qDqmqDq�qDrmqDr�qDsmqDs�qDtmqDt��DumqDu�qDvmqDv�qDwmqDw�qDxmqDx�qDymqDy�qDzmqDz�qD{mqD{�qD|mqD|�qD}mqD}�qD~mqD~�qDmqD�qD�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D¶�D���D�6�D�v�Dö�D���D�6�D�v�DĶ�D���D�6�D�v�DŶ�D���D�6�D�v�Dƶ�D���D�6�D�v�DǶ�D���D�6�D�v�Dȳ�D���D�6�D�v�Dɶ�D���D�6�D�v�Dʶ�D���D�6�D�v�D˶�D���D�6�D�v�D̶�D���D�6�D�v�DͶ�D���D�6�D�v�Dζ�D���D�6�D�v�D϶�D���D�6�D�v�Dж�D���D�6�D�v�DѶ�D���D�6�D�v�DҶ�D���D�3�D�v�DӶ�D���D�6�D�v�DԶ�D���D�6�D�v�Dն�D���D�6�D�v�Dֶ�D���D�6�D�v�D׶�D���D�6�D�v�Dض�D���D�6�D�v�Dٶ�D���D�6�D�v�Dڶ�D���D�6�D�v�D۶�D���D�6�D�v�Dܶ�D��D�6�D�v�Dݶ�D���D�6�D�v�D޶�D���D�6�D�v�D߶�D���D�6�D�v�DමD���D�9�D�v�DᶸD���D�6�D�v�DⶸD���D�6�D�v�D㶸D���D�6�D�v�D䶸D���D�6�D�v�D嶸D���D�6�D�v�D涸D���D�6�D�v�D綸D���D�6�D�v�D趸D���D�6�D�v�D鶸D���D�6�D�v�D궸D���D�6�D�v�D붸D���D�6�D�v�D춸D���D�6�D�v�D���D���D�6�D�v�DD���D�6�D�v�DﶸD���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D�D��D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��#A��/A��;A��HA��/A��/A��;A��`A��A��A��A��A��A��A��A��A���A��A��A���A���A���A��A���A��A���A���A���A���A���A���A���A���A�A�A�A�A�A�A�A�A���A�  A��A��A���A��;A�VA���A��A���A�ȴA�hsA��A�{A�"�A�VA��A�5?A��A��#A�|�A�ƨA�7LA��jA���A�|�A�Q�A�ȴA��/A��A��A�z�A�XA���A���A�1'A�x�A�`BA���A���A�A�A��A�%A�=qA�XA�"�A�bA��#A�G�A��A�=qA��A���A��\A��;A�%A��7A�ȴA�33A�^5A�hsA� �A��A��RA���A�n�A�A�O�A���A�z�A���A�\)A�+A�JA��A��hA�dZA��A�JA�K�A��jA���A�XA�ĜA�jA�G�A�A~ �A}�-A}\)A{�mAy�Av�`AvbAu�-At��As\)Ap�Ao�An�HAn��An�Am�Ak�wAi|�Ag%Ae;dAb��A_�
A]�A[�7AZ�AZ��AY|�AX�!AXr�AW��AW\)AV1AT�!AS7LAQ�AP�+AO33AM/AL^5AK?}AJ��AJ-AH��AG��AG��AGVAF�+AE�hAD�jAD�uAC��AA�hA@  A?�A>�A>�A=�A=�A<�DA;�#A:��A:-A9��A9`BA9+A8�9A7%A6$�A5��A4�uA3��A2ȴA0��A0�\A0$�A/��A/;dA.5?A,��A+�mA*v�A)`BA(v�A'�^A'O�A'/A'%A&bNA%�PA$  A"��A!��A!��A!�A ��A�^A��A��AM�A�A�^AdZA��A^5A��A�A��A�yA~�A��A33AZA��A{A�
A�hA7LA��A  Ax�A?}A�Av�AE�A �AJA��A�A`BA
�A��A�AbNA�A�A�-AȴAA+AA �jA �@�"�@��y@�=q@�O�@��;@�n�@��@��;@���@�(�@�ƨ@�J@���@�\)@�D@��@�@��@�-@�&�@��m@�
=@��/@ݡ�@��
@�5?@�?}@�9X@���@�t�@�ȴ@�  @��H@�&�@�1@�l�@��T@�  @�1'@�S�@ƸR@�V@�9X@�t�@�hs@��F@��H@�E�@�`B@��m@��-@��@��m@�n�@��/@���@���@�^5@��@�9X@�|�@���@�V@���@�V@��m@��@���@���@�X@���@��F@��!@��@�%@�z�@�(�@��@��m@��F@�l�@���@�-@���@��-@���@���@�-@��@��9@�r�@�Q�@�  @�K�@��@�M�@��@�p�@�&�@��@�Ĝ@�bN@���@�K�@��y@���@�ff@�J@�@��@��@�b@�ƨ@��P@�dZ@�@�^5@��@�hs@�Ĝ@�r�@�bN@�I�@�1'@�(�@��@��F@�t�@�\)@��@��R@���@��@��@���@��9@��@�l�@�+@��@�
=@���@��H@���@�J@��#@�x�@���@�j@�I�@�9X@��w@�|�@�l�@�C�@��@���@��@��@���@�=q@��#@��7@�hs@�O�@�7L@���@�I�@�  @��m@��;@��w@��@��F@���@�"�@�@��@���@��R@���@��\@�v�@�-@�@���@�x�@�V@���@�Ĝ@���@�z�@�9X@�  @�;@�w@�@K�@
=@~��@}�@}��@}�@}?}@}V@|�@|Z@|�@{�m@{��@{C�@z�@z��@z��@z��@zJ@y��@y�^@y�7@yhs@yX@y7L@y7L@y7L@yG�@y7L@y%@x��@x��@xĜ@x��@x�@xA�@w�@w�P@wl�@w\)@w�@w�@w+@w+@w
=@v�@v�R@v��@vv�@u�T@u@u�@tz�@tj@tZ@tZ@tZ@sƨ@sS�@sC�@r��@r�@q�^@q��@q�7@q�7@qX@q%@pbN@o�w@o;d@n�R@n�+@m�T@m@m�@mO�@lZ@k"�@j��@j=q@i��@i&�@hĜ@hbN@hA�@hQ�@h1'@h �@g�@g+@fȴ@fv�@fV@fV@f{@e��@e`B@d��@d�j@dj@d9X@c�F@c"�@c@b=q@a��@a��@a7L@`r�@`bN@` �@_K�@^�@^�+@^V@^$�@]�T@]/@\�@\Z@[�@[o@Z�H@ZM�@Y��@Yx�@Yx�@YX@Y&�@X�`@X�u@Xr�@W�@Wl�@W
=@V��@V$�@V$�@V$�@U�T@U�@T�j@T�D@Tz�@Tj@Tj@Tj@T�@S�
@S33@R��@R~�@RM�@R�@Qhs@Q7L@Q�@Q%@P��@PĜ@P�u@Pr�@PQ�@P1'@O�@O�@Ol�@O+@O
=@O
=@O
=@N��@N�y@N�R@N��@N��@NE�@N{@N@M�@M��@L��@K��@K��@KdZ@K33@K33@K"�@J��@J�@I��@I�7@IX@IG�@I&�@I�@I�@H��@H��@HQ�@H  @G��@G��@Gl�@G\)@GK�@F�+@E�-@E?}@E/@D�@Dz�@D(�@C��@C�
@C�F@C�@CS�@B�!@A�^@A��@A��@A�@@��@@r�@@  @?;d@?
=@>��@>V@=�h@=O�@=�@=V@<�/@<Z@<9X@<9X@<(�@;�
@;t�@;dZ@;C�@;"�@:�!@:~�@:�@9�@9�#@9��@9�7@9G�@9&�@9�@9%@8bN@8  @7�P@7+@6ȴ@6V@6{@6@5�T@5�-@5�@4�j@4(�@3��@3S�@3C�@3C�@333@333@3"�@3@2�@2�H@2��@2M�@1��@1�^@1��@1��@1x�@1&�@0��@0r�@0  @/�w@/|�@/K�@/+@.E�@-�@-?}@,��@,�/@,��@,�@,�@,��@,�@,��@,�D@,z�@,z�@,j@+�
@*��@*~�@*=q@*-@*J@)��@)�#@)x�@)�@)�@)%@(��@(�9@(��@(�@(A�@( �@'�;@'��@'K�@&v�@%�@$��@$�@$��@$z�@$I�@$9X@$(�@#��@#�m@#ƨ@#�@#C�@#@#@"��@"��@"n�@"�@!��@!��@!X@!�@!%@ ��@ Ĝ@ �@ bN@ 1'@   @��@\)@�@��@�R@ff@V@V@V@$�@{@{@{@�T@/@�@V@��@�j@�D@�D@z�@�@�@1@1@�m@��@S�@"�@�@�H@�H@�H@��@��@�\@J@��@��@&�@�@�@�`@��@��@�u@Q�@�;@l�@;d@�@
=@��@ff@E�@{@/@�@�@�@�j@Z@Z@I�@9X@(�@��@��@�
@ƨ@�F@��@��@��@dZ@S�@33@��@�\@~�@n�@M�@=q@=q@=q@=q@-@-@��@��@�7@hs@hs@X@X@G�@7L@&�@�@�`@�9@��@�u@r�@bN@�@�w@�@��@�P@|�@|�@l�@\)@;d@
=@�@��@$�@@�-@�@p�@?}@V@��@��@��@I�@�@�m@�F@��@S�@
�H@
��@
~�@
^5@
=q@
=q@
-@
�@	�#@	�^@	��@	�@��@�9@��@�u@r�@Q�@1'@b@�@�;@\)@K�@�@��@v�@$�@�T@@�-@�-@��@��@O�@/@/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��#A��/A��;A��HA��/A��/A��;A��`A��A��A��A��A��A��A��A��A���A��A��A���A���A���A��A���A��A���A���A���A���A���A���A���A���A�A�A�A�A�A�A�A�A���A�  A��A��A���A��;A�VA���A��A���A�ȴA�hsA��A�{A�"�A�VA��A�5?A��A��#A�|�A�ƨA�7LA��jA���A�|�A�Q�A�ȴA��/A��A��A�z�A�XA���A���A�1'A�x�A�`BA���A���A�A�A��A�%A�=qA�XA�"�A�bA��#A�G�A��A�=qA��A���A��\A��;A�%A��7A�ȴA�33A�^5A�hsA� �A��A��RA���A�n�A�A�O�A���A�z�A���A�\)A�+A�JA��A��hA�dZA��A�JA�K�A��jA���A�XA�ĜA�jA�G�A�A~ �A}�-A}\)A{�mAy�Av�`AvbAu�-At��As\)Ap�Ao�An�HAn��An�Am�Ak�wAi|�Ag%Ae;dAb��A_�
A]�A[�7AZ�AZ��AY|�AX�!AXr�AW��AW\)AV1AT�!AS7LAQ�AP�+AO33AM/AL^5AK?}AJ��AJ-AH��AG��AG��AGVAF�+AE�hAD�jAD�uAC��AA�hA@  A?�A>�A>�A=�A=�A<�DA;�#A:��A:-A9��A9`BA9+A8�9A7%A6$�A5��A4�uA3��A2ȴA0��A0�\A0$�A/��A/;dA.5?A,��A+�mA*v�A)`BA(v�A'�^A'O�A'/A'%A&bNA%�PA$  A"��A!��A!��A!�A ��A�^A��A��AM�A�A�^AdZA��A^5A��A�A��A�yA~�A��A33AZA��A{A�
A�hA7LA��A  Ax�A?}A�Av�AE�A �AJA��A�A`BA
�A��A�AbNA�A�A�-AȴAA+AA �jA �@�"�@��y@�=q@�O�@��;@�n�@��@��;@���@�(�@�ƨ@�J@���@�\)@�D@��@�@��@�-@�&�@��m@�
=@��/@ݡ�@��
@�5?@�?}@�9X@���@�t�@�ȴ@�  @��H@�&�@�1@�l�@��T@�  @�1'@�S�@ƸR@�V@�9X@�t�@�hs@��F@��H@�E�@�`B@��m@��-@��@��m@�n�@��/@���@���@�^5@��@�9X@�|�@���@�V@���@�V@��m@��@���@���@�X@���@��F@��!@��@�%@�z�@�(�@��@��m@��F@�l�@���@�-@���@��-@���@���@�-@��@��9@�r�@�Q�@�  @�K�@��@�M�@��@�p�@�&�@��@�Ĝ@�bN@���@�K�@��y@���@�ff@�J@�@��@��@�b@�ƨ@��P@�dZ@�@�^5@��@�hs@�Ĝ@�r�@�bN@�I�@�1'@�(�@��@��F@�t�@�\)@��@��R@���@��@��@���@��9@��@�l�@�+@��@�
=@���@��H@���@�J@��#@�x�@���@�j@�I�@�9X@��w@�|�@�l�@�C�@��@���@��@��@���@�=q@��#@��7@�hs@�O�@�7L@���@�I�@�  @��m@��;@��w@��@��F@���@�"�@�@��@���@��R@���@��\@�v�@�-@�@���@�x�@�V@���@�Ĝ@���@�z�@�9X@�  @�;@�w@�@K�@
=@~��@}�@}��@}�@}?}@}V@|�@|Z@|�@{�m@{��@{C�@z�@z��@z��@z��@zJ@y��@y�^@y�7@yhs@yX@y7L@y7L@y7L@yG�@y7L@y%@x��@x��@xĜ@x��@x�@xA�@w�@w�P@wl�@w\)@w�@w�@w+@w+@w
=@v�@v�R@v��@vv�@u�T@u@u�@tz�@tj@tZ@tZ@tZ@sƨ@sS�@sC�@r��@r�@q�^@q��@q�7@q�7@qX@q%@pbN@o�w@o;d@n�R@n�+@m�T@m@m�@mO�@lZ@k"�@j��@j=q@i��@i&�@hĜ@hbN@hA�@hQ�@h1'@h �@g�@g+@fȴ@fv�@fV@fV@f{@e��@e`B@d��@d�j@dj@d9X@c�F@c"�@c@b=q@a��@a��@a7L@`r�@`bN@` �@_K�@^�@^�+@^V@^$�@]�T@]/@\�@\Z@[�@[o@Z�H@ZM�@Y��@Yx�@Yx�@YX@Y&�@X�`@X�u@Xr�@W�@Wl�@W
=@V��@V$�@V$�@V$�@U�T@U�@T�j@T�D@Tz�@Tj@Tj@Tj@T�@S�
@S33@R��@R~�@RM�@R�@Qhs@Q7L@Q�@Q%@P��@PĜ@P�u@Pr�@PQ�@P1'@O�@O�@Ol�@O+@O
=@O
=@O
=@N��@N�y@N�R@N��@N��@NE�@N{@N@M�@M��@L��@K��@K��@KdZ@K33@K33@K"�@J��@J�@I��@I�7@IX@IG�@I&�@I�@I�@H��@H��@HQ�@H  @G��@G��@Gl�@G\)@GK�@F�+@E�-@E?}@E/@D�@Dz�@D(�@C��@C�
@C�F@C�@CS�@B�!@A�^@A��@A��@A�@@��@@r�@@  @?;d@?
=@>��@>V@=�h@=O�@=�@=V@<�/@<Z@<9X@<9X@<(�@;�
@;t�@;dZ@;C�@;"�@:�!@:~�@:�@9�@9�#@9��@9�7@9G�@9&�@9�@9%@8bN@8  @7�P@7+@6ȴ@6V@6{@6@5�T@5�-@5�@4�j@4(�@3��@3S�@3C�@3C�@333@333@3"�@3@2�@2�H@2��@2M�@1��@1�^@1��@1��@1x�@1&�@0��@0r�@0  @/�w@/|�@/K�@/+@.E�@-�@-?}@,��@,�/@,��@,�@,�@,��@,�@,��@,�D@,z�@,z�@,j@+�
@*��@*~�@*=q@*-@*J@)��@)�#@)x�@)�@)�@)%@(��@(�9@(��@(�@(A�@( �@'�;@'��@'K�@&v�@%�@$��@$�@$��@$z�@$I�@$9X@$(�@#��@#�m@#ƨ@#�@#C�@#@#@"��@"��@"n�@"�@!��@!��@!X@!�@!%@ ��@ Ĝ@ �@ bN@ 1'@   @��@\)@�@��@�R@ff@V@V@V@$�@{@{@{@�T@/@�@V@��@�j@�D@�D@z�@�@�@1@1@�m@��@S�@"�@�@�H@�H@�H@��@��@�\@J@��@��@&�@�@�@�`@��@��@�u@Q�@�;@l�@;d@�@
=@��@ff@E�@{@/@�@�@�@�j@Z@Z@I�@9X@(�@��@��@�
@ƨ@�F@��@��@��@dZ@S�@33@��@�\@~�@n�@M�@=q@=q@=q@=q@-@-@��@��@�7@hs@hs@X@X@G�@7L@&�@�@�`@�9@��@�u@r�@bN@�@�w@�@��@�P@|�@|�@l�@\)@;d@
=@�@��@$�@@�-@�@p�@?}@V@��@��@��@I�@�@�m@�F@��@S�@
�H@
��@
~�@
^5@
=q@
=q@
-@
�@	�#@	�^@	��@	�@��@�9@��@�u@r�@Q�@1'@b@�@�;@\)@K�@�@��@v�@$�@�T@@�-@�-@��@��@O�@/@/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�B�#B�#B�#B�#B�#B�#B�)B�#B�#B�#B�)B�#B�#B�)B�)B�)B�)B�)B�#B�#B�B��BoB�B�sB�B�B�B�`B�ZB�;B�B�B��B��B��BɺB�LB�3B��B��B��B�uB�PB~�Bx�Bk�B_;BQ�BF�B9XB,B �B{B1B��B�/B��B��B��B�VB�7B�+B�BhsBQ�B?}B8RB&�B+B
�B1B,B?}B;dBE�Bk�Bl�BbNBH�BS�BR�BL�BC�B:^B5?B/B"�B�B�B�BVB
��B
�B
�fB
�B
��B
ŢB
�RB
�B
��B
��B
�1B
�B
|�B
v�B
iyB
\)B
E�B
@�B
=qB
5?B
)�B
�B
	7B
1B
%B
B	��B	�sB	��B	�dB	��B	�hB	�B	t�B	iyB	e`B	cTB	^5B	VB	T�B	P�B	L�B	J�B	C�B	<jB	6FB	0!B	&�B	bB		7B	B	B	B	B��B��B��B��B��B��B�B�B�yB�BB�#B�B�B�B��B��B��B��B��B��B��B�B�#B��B��BȴBŢBÖBǮBȴBǮBŢBBB�wB�dB�LB�-B�B��B��B��B��B��B��B��B��B��B�oB�hB�\B�VB�PB�+B�B� B|�B{�B{�B|�B{�B{�Bz�Bx�Bw�Bu�Br�Bp�Bp�Br�Bm�Bl�Bk�BjBhsBdZBbNBaHB`BB`BBaHBbNBbNBbNBbNBbNB\)BO�BH�BG�BF�BG�BG�BE�B?}B@�BA�BA�BA�B?}B@�BA�BA�B>wB:^B7LB5?B33B.B-B-B+B)�B+B(�B'�B'�B'�B&�B%�B$�B%�B&�B$�B"�B"�B"�B!�B"�B#�B�B�B�B�B �B#�B �B%�B(�B)�B-B-B,B.B/B0!B1'B1'B/B0!B.B-B.B2-B49B5?B49B9XB;dB=qB@�BC�BE�BF�BI�BI�BJ�BK�BK�BK�BM�BO�BP�BR�BT�BVBXBYBYBZB^5B^5B_;B_;BbNBbNBcTBe`BffBffBffBhsBjBk�Bl�Bm�Bs�Bv�Bw�By�B{�B}�B~�B~�B� B� B�B�B�B�%B�JB�VB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�3B�LB�^B�qB�wB��BÖB��B��B��B��B��B��B��B��B��B�
B�#B�/B�HB�NB�fB�sB�sB�yB�B�B�B�B�B��B��B��B��B	B	B	B	%B	1B		7B	
=B	JB	PB	\B	bB	oB	uB	�B	�B	�B	�B	�B	�B	"�B	%�B	&�B	,B	1'B	2-B	49B	5?B	7LB	9XB	<jB	<jB	=qB	@�B	C�B	D�B	G�B	J�B	K�B	L�B	O�B	Q�B	T�B	XB	ZB	]/B	bNB	cTB	e`B	e`B	ffB	gmB	iyB	k�B	k�B	m�B	o�B	q�B	r�B	s�B	r�B	r�B	r�B	u�B	v�B	w�B	x�B	z�B	z�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�=B	�=B	�=B	�DB	�JB	�bB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�'B	�-B	�3B	�?B	�XB	�dB	�dB	�jB	�jB	�qB	�}B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�#B	�)B	�5B	�;B	�BB	�HB	�NB	�ZB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B

=B

=B
DB
DB
JB
DB
JB
JB
PB
PB
PB
VB
VB
VB
VB
VB
\B
bB
hB
hB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
#�B
$�B
#�B
#�B
"�B
"�B
#�B
$�B
&�B
&�B
&�B
&�B
'�B
(�B
)�B
(�B
)�B
)�B
+B
,B
,B
,B
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
33B
49B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
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
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
B�B
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
F�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
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
T�B
VB
VB
W
B
W
B
W
B
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
YB
YB
YB
YB
ZB
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
\)B
]/B
]/B
^5B
^5B
^5B
^5B
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
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
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
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
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
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
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
p�B
p�B
p�B
q�B
q�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�=B�7B�=B�=B�=B�=B�=B�=B�CB�=B�=B�=B�CB�=B�=B�CB�CB�CB�]B�xB��B��B��B�NB�B�'B�B�]B��B�qB�*B�B�bB��B�BևB�9B��B�6B�XB�FB�B�QB��B��B��B��B{dBn�Ba�BS�BH�B;JB.B#TB�B
�B��B�|B�B��B�pB�B��B��B��Bk6BS�BAUB;�B+6B
#B
��BzB,B@OB;BF?BlWBm�Bc�BI�BT�BTFBN�BD�B;dB6�B0oB#�B vBB�B B
��B
�hB
�>B
ٴB
ѷB
�EB
��B
�qB
�RB
�KB
�lB
��B
}�B
y	B
l�B
^jB
F�B
AoB
>�B
7fB
,�B
+B
	�B
�B
EB
�B	�	B	�kB	�B	�(B	�FB	��B	�B	v�B	jKB	fB	d�B	_VB	V�B	U�B	R:B	N�B	L�B	E�B	>�B	7�B	2B	)*B	�B	
�B	%B	3B	�B	�B��B�B��B�*B��B��B�?B�B�QB�bB��B��B�B��B�mB�B�@B��B�vBοBѝB�$B�B�MB��B�=B�B�9BɺB�lBȀBƎBÖB�3B�iB��B�>B��B��B�B��B�fB�zB�B�nB��B�OB�mB�&B�oB�HB��B��B��B��B��B}qB|�B|�B}�B|�B|�B{�ByXBx�BwBs�Br�BtTBs�BnBmCBlWBk�Bi�Be,BcBa�B`�B`�Ba�Bb�Bb�Bc Bd@BfLB_BQ4BIlBH�BHfBIRBIBF�B@iBA BB[BB�BB[B@ BAUBB�BB�B?�B;�B8�B6�B4TB.�B.�B.�B+�B+�B,"B)�B)B)�B(�B(
B'B&�B(
B(>B&2B#�B$B$�B#B#�B%�B�BB�B�B!�B$�B!HB&�B)�B+6B-�B.B-�B/OB0B0�B2GB2�B0�B1B.�B.cB/�B3hB4�B5�B5?B:*B<B>BA;BDMBFtBG�BJ�BJ=BK�BLdBL~BL�BN�BP�BQ�BSuBUgBVmBX_BYBY�BZ�B^�B^�B_�B`\Bc�Bc Bd&Be�Bf�Bf�Bf�Bi*BkBl"BmBnBtBwBx8BzxB|�B~wB}BcB��B��B�oB�uB��B��B��B��B��B��B�?B�CB�OB�bB�:B�,B�B�8B�B�RB�_B�eB�qB��B��B�B��B��B��B��B� B�gB�B��B��B��B�0B�PB�vB�[B�{BרB��B�~B�|B��B�B�B��B��B��B��B� B� B�-B�2B�0B�<B�BB	UB	�B	�B	�B	fB		�B	
rB	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B		B	B	# B	&2B	'mB	,�B	1[B	2�B	4�B	5�B	7�B	9�B	<�B	<�B	=�B	@�B	C�B	D�B	HB	J�B	K�B	M6B	P.B	RTB	UgB	X_B	ZQB	]�B	b�B	c�B	e�B	e�B	f�B	g�B	i�B	k�B	k�B	m�B	o�B	q�B	r�B	s�B	r�B	r�B	r�B	u�B	wB	xB	y	B	{B	{0B	~]B	�OB	�[B	�[B	�[B	�-B	�GB	�GB	�aB	�aB	�MB	�MB	�SB	�mB	�mB	��B	��B	�rB	�rB	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�4B	�&B	�,B	�B	�RB	�QB	�WB	�cB	��B	��B	�vB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�.B	�FB	�SB	�SB	�yB	�kB	�xB	�qB	ܬB	ޞB	�pB	��B	�|B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�2B	�2B	�$B	�	B	�B	�0B	�B	�(B	�HB	�HB	�cB
 iB
 OB
;B
;B
uB
MB
MB
MB
SB
mB
mB
YB
_B
_B
fB
	lB

�B

rB
xB
^B
dB
xB
dB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#B
B
B
�B
�B
B
B
�B
�B
B
�B
�B
!B
!HB
!�B
#B
$@B
$@B
%B
$@B
$@B
# B
#:B
$@B
%`B
'B
'8B
'8B
'B
(XB
)*B
*B
)*B
*0B
*KB
+QB
,WB
,WB
,WB
-]B
-]B
.cB
.IB
.IB
.cB
.IB
/iB
/iB
/OB
/�B
0�B
1�B
1�B
1�B
2�B
3�B
3hB
3�B
3�B
3�B
4�B
5�B
6�B
6�B
7fB
7�B
7fB
7�B
7fB
7�B
7fB
7�B
7�B
7�B
8�B
9rB
9�B
9�B
9�B
9�B
:�B
:�B
;�B
;�B
;�B
;�B
;�B
;�B
<�B
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
>�B
>�B
>�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
EB
F%B
GEB
H�B
IB
H�B
IB
H�B
I�B
I�B
J	B
J	B
I�B
I�B
KB
J�B
J�B
KB
LB
LB
LB
MB
MB
MB
N"B
M�B
NB
N"B
NB
OB
OB
OB
O(B
PB
P.B
QB
Q4B
R B
R B
RB
R B
R:B
R B
RB
R B
R:B
S[B
TB
T,B
TFB
TFB
T,B
TB
T,B
UMB
U2B
U2B
UB
UMB
V9B
V9B
WYB
WYB
W?B
W$B
W$B
W?B
W?B
XEB
X_B
X_B
XEB
YKB
Y1B
YKB
YKB
ZQB
Z7B
ZkB
ZQB
ZkB
[qB
[qB
\]B
\xB
\xB
\]B
\]B
]~B
]�B
^jB
^jB
^jB
^jB
^jB
^jB
^jB
^OB
^�B
_pB
_VB
_�B
_VB
_pB
_VB
_pB
_�B
`vB
`vB
`�B
`vB
a|B
a|B
abB
a|B
abB
a|B
a|B
a|B
a|B
abB
a|B
b�B
b�B
b�B
bhB
b�B
bhB
bhB
bhB
b�B
b�B
b�B
c�B
c�B
cnB
c�B
c�B
c�B
d�B
d�B
d�B
e�B
ezB
e�B
e�B
ezB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
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
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o B
n�B
n�B
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
q�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.29(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201611300032502016113000325020161130003250201806221217232018062212172320180622121723201804050410232018040504102320180405041023  JA  ARFMdecpA19c                                                                20161126093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161126003527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161126003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161126003528  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161126003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161126003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161126003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161126003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161126003529  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161126003529                      G�O�G�O�G�O�                JA  ARUP                                                                        20161126013009                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161126153550  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20161126153550  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20161126153550  CV  LONGITUDE       G�O�G�O��%                JM  ARCAJMQC2.0                                                                 20161129153250  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161129153250  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191023  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031723  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                