CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-06-18T09:00:36Z creation      
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
resolution        =���   axis      Z        L  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  n�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210618090036  20210618090036  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               mA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @�}V$��1   @�}V�k�@:�C��%�c�A�7K�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         mA   A   F   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@��R@�A\)A;\)A[\)A{\)A��A��A��A��AͮAݮA��A��B�
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
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C��C��C�]C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D mqD �qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD	mqD	�qD
mqD
�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qDmqD�qD mqD �qD!mqD!�qD"mqD"�qD#mqD#�qD$mqD$�qD%mqD%�qD&mqD&�qD'mqD'�qD(mqD(�qD)mqD)�qD*mqD*�qD+mqD+�qD,mqD,�qD-mqD-�qD.mqD.�qD/mqD/�qD0mqD0�qD1mqD1�qD2mqD2�qD3mqD3�qD4mqD4�qD5mqD5�qD6mqD6�qD7mqD7�qD8mqD8�qD9mqD9�qD:mqD:�qD;mqD;�qD<mqD<�qD=mqD=�qD>mqD>�qD?mqD?�qD@mqD@�qDAmqDA�qDBmqDB�qDCmqDC�qDDmqDD�qDEmqDE�qDFmqDF�qDGmqDG�qDHmqDH�qDImqDI�qDJmqDJ�qDKmqDK�qDLmqDL�qDMmqDM�qDNmqDN�qDOmqDO�qDPmqDP�qDQmqDQ�qDRmqDR�qDSmqDS�qDTmqDT�qDUmqDU�qDVmqDV�qDWmqDW�qDXmqDX�qDYmqDY�qDZmqDZ�qD[mqD[�qD\mqD\�qD]mqD]�qD^mqD^�qD_mqD_�qD`mqD`�qDamqDa�qDbmqDb�qDcmqDc�qDdmqDd�qDemqDe�qDfmqDf�qDgmqDg�qDhmqDh�qDimqDi�qDjmqDj�qDkmqDk�qDlmqDl�qDmmqDm�qDnmqDn�qDomqDo�qDpmqDp�qDqmqDq�qDrmqDr�qDsmqDs�qDtmqDt�qDumqDu�qDvmqDv�qDwmqDw�qDxmqDx�qDymqDy�qDzmqDz�qD{mqD{�qD|mqD|�qD}s�D}�qD~mqD~�qDmqD�qD�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�3�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�s�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D��D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D���D���D�6�D�v�D¶�D���D�6�D�v�Dö�D���D�6�D�v�DĶ�D���D�6�D�v�DŶ�D���D�6�D�v�Dƶ�D���D�6�D�v�DǶ�D���D�6�D�v�Dȶ�D���D�6�D�v�Dɶ�D���D�6�D�v�Dʶ�D���D�6�D�v�D˶�D���D�6�D�v�D̶�D���D�6�D�v�DͶ�D���D�6�D�v�Dζ�D���D�6�D�v�D϶�D���D�6�D�v�Dж�D���D�6�D�v�DѶ�D���D�6�D�v�DҶ�D���D�6�D�v�DӶ�D���D�6�D�v�DԶ�D���D�6�D�v�Dն�D���D�6�D�v�Dֶ�D���D�6�D�v�D׶�D���D�6�D�v�Dض�D���D�6�D�v�Dٶ�D���D�6�D�v�Dڶ�D���D�6�D�v�D۶�D���D�6�D�v�Dܶ�D���D�6�D�v�Dݶ�D���D�6�D�v�D޶�D���D�6�D�v�D߶�D���D�6�D�v�DමD���D�6�D�v�DᶸD���D�6�D�v�DⶸD���D�6�D�v�D㶸D���D�6�D�v�D䶸D���D�6�D�v�D嶸D���D�6�D�v�D涸D���D�6�D�v�D綸D���D�6�D�s�D趸D���D�6�D�v�D鶸D���D�6�D�v�D궸D���D�6�D�v�D붸D���D�6�D�v�D춸D���D�6�D�v�D���D���D�6�D�v�DD���D�6�D�v�DﶸD���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�v�D�D���D�6�D�y�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��A��A��A��yA��`A��;A���A�ȴA���AǶFAǬAǧ�Aǥ�Aǡ�Aǟ�AǙ�A�v�A�hsAÏ\A��PA�(�A��A�v�A�=qA�z�A���A��mA��A��A���A�(�A��TA��A�1'A���A�C�A�~�A���A��A�p�A�Q�A���A��^A�dZA�ffA�&�A��A�hsA�x�A��
A�  A���A���A�&�A���A���A�;dA�JA��/A�p�A��yA�/A�bA��+A���A��A�v�A�bA�bNA�33A��A�~�A�^5A�1A�v�A�t�A��mA�t�A��A�A��A�A���A�|�A�/A�5?A�K�A�1'A��A�K�A�7LA�A��yA���A�hsA��A~-A|9XAz�Ay��Ax�Aw��Av�Au
=As��AqAl�Ak��Aj�9Ai�AioAh1'Ad�yA_hsA]�mA\bAXĜAU�FAS��AR�/ARbNAR{AQ��AQ;dAQVAP�AP-AO�ANr�AM��AMG�AJ��AI�AH�AF��AE��AEC�AD~�AC�^ACG�AB��AAC�A@{A>�A>{A=ƨA=\)A<��A<VA;�-A:�A:v�A9��A9"�A8�9A8ffA7ƨA6��A5&�A3A3G�A1�^A0��A0��A0��A0~�A0�A/%A.�!A.M�A.  A-��A-K�A-oA+�FA*Q�A)�A)��A)�wA)�FA)�-A)��A)�A)t�A)`BA)?}A&��A&�DA&  A%"�A$bA#��A#��A"n�A!�-A ��A��A&�A�\Ap�A�A�HAQ�A7LAK�A�uA�TA��A�DAp�A�A1'A�7A�A?}A �AdZA�!A�
A�uAE�A��A
$�A��A�jA�A�\A5?A��AƨAl�A+A�uA�FAl�A\)A
=A��A�RAbNA�A
=@��w@��@�I�@��w@�$�@�`B@��m@�
=@�ȴ@���@�E�@�%@�Q�@�t�@���@�ȴ@�=q@�^@�;d@��y@���@߾w@�S�@�@۾w@���@ڟ�@�V@�@���@�\)@�~�@�^5@�E�@�{@�G�@Դ9@��@�(�@Χ�@�=q@�5?@��@��`@˅@ʰ!@��m@�v�@���@���@��@���@��u@��\@���@���@�v�@���@�p�@�V@��@�1'@���@���@���@��\@�@��;@��
@��
@��@��@���@�$�@��7@�7L@��u@�ƨ@��@���@�X@�Z@��@��w@���@�
=@��@�hs@�%@�/@�O�@�/@��`@� �@���@�=q@���@��/@��D@�Z@�  @���@�t�@�"�@��@��@���@�$�@��-@��@��@���@�Ĝ@��9@��@�Z@�33@���@��@��;@�K�@�o@��@��+@�5?@��T@��^@��7@�?}@��/@�j@�9X@�  @���@��@��@�;d@��@��R@�5?@��7@�7L@�%@�j@�A�@�(�@��;@���@�\)@�"�@�
=@��y@���@��+@�V@�{@���@�x�@�&�@��D@��@��@�\)@�o@��y@��@�ȴ@��R@��\@�E�@�-@��@��^@�O�@��@���@�A�@��w@�\)@�33@��y@���@�ff@��@��^@���@�?}@���@�(�@���@���@��@��y@��y@��y@���@��+@�v�@�n�@�^5@�E�@�@��^@���@�hs@�X@�?}@�/@�&�@�V@���@��/@��D@�(�@�w@;d@
=@~��@~{@}�T@}/@|�@|1@{"�@z�\@z�@y��@y�@zJ@y�#@yx�@x�@w�P@w+@w
=@v��@v5?@v@u?}@tZ@sƨ@sS�@r��@r��@q��@qhs@q7L@q%@p��@p �@o�@oK�@n�y@n��@nE�@m�T@m��@mp�@l��@l��@l��@lz�@lI�@lI�@lI�@l9X@l�@l1@k�m@k�
@k�F@k��@kt�@kC�@k"�@j�\@ihs@hĜ@h��@g�@g;d@g
=@f��@e�T@f{@fE�@e?}@dZ@c�m@c��@cdZ@c@b~�@ahs@aG�@`��@`Ĝ@`r�@`��@_��@^�y@^�@^�@^�@^�@^ȴ@^�R@^��@^�+@]�@]?}@\�j@\(�@[�
@[�@[C�@[@Z��@Z�!@Zn�@Z=q@Z-@Y��@Y��@Z�@Y��@Zn�@Z-@Z�@Z�@ZJ@Y��@Y��@Y��@Y�7@Yx�@Y&�@X��@X�9@X�9@X�u@Xr�@W��@W�@X  @W�@W�@W�@V��@V5?@U�@U�-@U`B@Tj@S�F@S�@S�@S"�@R�!@Q�@Q&�@Pr�@PbN@PQ�@PQ�@P1'@Pb@O�@N�R@N�+@Nff@N{@M��@M�-@M��@M�@M/@L�/@L�j@LI�@K�m@K��@Kt�@KdZ@KC�@K"�@K@J�!@J��@J�\@Jn�@J^5@JM�@J=q@J�@I�@I�^@I��@Ix�@I7L@I�@H��@H�`@H��@HĜ@H��@G�@F�y@F��@F��@F�+@Fff@F$�@E`B@D�@Dj@D�@C��@C�F@C�@CdZ@C33@Co@B�H@B�\@A�@A��@A%@@�u@@b@?�P@?+@>�y@>��@>V@>5?@=�@=�T@=��@=�@=?}@<�D@<j@;�
@:�H@:~�@:^5@:�@9��@9�7@8�`@81'@7�w@7|�@7l�@7K�@7;d@6��@6�@6�R@6��@6��@6�+@65?@5�T@5p�@5/@4�/@4��@4I�@41@3��@3��@3�m@3��@3t�@3t�@3dZ@3C�@3"�@3o@3@2�@2��@2�\@2n�@2n�@2�@1�^@1hs@1%@0��@0��@0�9@0�u@0r�@0 �@/�w@/|�@/\)@/\)@/\)@/K�@/+@.�y@.ȴ@.v�@.V@.E�@.5?@.5?@.@-�T@-�T@-��@-��@,�/@,��@,j@,(�@+��@+�m@+ƨ@+"�@*-@)�#@)�^@)X@(��@(b@'�;@'�w@'l�@&ȴ@&ff@&E�@&{@%�@&@%��@%�-@%��@%�@%p�@%O�@%/@$�@$��@$�j@$��@$�D@$Z@$I�@$1@#�F@#dZ@#"�@#o@"��@"�\@"n�@"M�@"-@"�@"J@!��@!x�@!&�@!�@!%@ �`@ ��@ ��@ Ĝ@ ��@ bN@�w@+@��@��@E�@@�-@O�@V@�/@Z@ƨ@�@t�@S�@C�@"�@�H@��@��@�!@��@~�@�@��@X@�9@bN@Q�@1'@1'@1'@ �@  @�@�;@��@��@|�@;d@��@�@��@v�@E�@{@@�@�@��@p�@?}@�@�j@z�@Z@I�@I�@�@1@�m@ƨ@�H@��@�!@M�@-@J@J@J@��@��@��@�@�@�#@��@�^@��@��@��@�7@�7@x�@G�@�`@��@�u@ �@�@��@l�@;d@��@@�T@��@�-@�@�@p�@`B@O�@/@��@�/@�@�D@(�@��@�@�@dZ@S�@C�@
�@
�\@
-@
�@	�#@	��@	7L@	�@	%@�`@�`@�9@�9@r�@Q�@b@�@�w@|�@\)@
=@�y@ȴ@�R@�R@��@��@��@��@�+@v�@v�@E�@�T@@�-@�h@?}@��@�/@�j@�@��@�D@z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��TA��A��A��A��yA��`A��;A���A�ȴA���AǶFAǬAǧ�Aǥ�Aǡ�Aǟ�AǙ�A�v�A�hsAÏ\A��PA�(�A��A�v�A�=qA�z�A���A��mA��A��A���A�(�A��TA��A�1'A���A�C�A�~�A���A��A�p�A�Q�A���A��^A�dZA�ffA�&�A��A�hsA�x�A��
A�  A���A���A�&�A���A���A�;dA�JA��/A�p�A��yA�/A�bA��+A���A��A�v�A�bA�bNA�33A��A�~�A�^5A�1A�v�A�t�A��mA�t�A��A�A��A�A���A�|�A�/A�5?A�K�A�1'A��A�K�A�7LA�A��yA���A�hsA��A~-A|9XAz�Ay��Ax�Aw��Av�Au
=As��AqAl�Ak��Aj�9Ai�AioAh1'Ad�yA_hsA]�mA\bAXĜAU�FAS��AR�/ARbNAR{AQ��AQ;dAQVAP�AP-AO�ANr�AM��AMG�AJ��AI�AH�AF��AE��AEC�AD~�AC�^ACG�AB��AAC�A@{A>�A>{A=ƨA=\)A<��A<VA;�-A:�A:v�A9��A9"�A8�9A8ffA7ƨA6��A5&�A3A3G�A1�^A0��A0��A0��A0~�A0�A/%A.�!A.M�A.  A-��A-K�A-oA+�FA*Q�A)�A)��A)�wA)�FA)�-A)��A)�A)t�A)`BA)?}A&��A&�DA&  A%"�A$bA#��A#��A"n�A!�-A ��A��A&�A�\Ap�A�A�HAQ�A7LAK�A�uA�TA��A�DAp�A�A1'A�7A�A?}A �AdZA�!A�
A�uAE�A��A
$�A��A�jA�A�\A5?A��AƨAl�A+A�uA�FAl�A\)A
=A��A�RAbNA�A
=@��w@��@�I�@��w@�$�@�`B@��m@�
=@�ȴ@���@�E�@�%@�Q�@�t�@���@�ȴ@�=q@�^@�;d@��y@���@߾w@�S�@�@۾w@���@ڟ�@�V@�@���@�\)@�~�@�^5@�E�@�{@�G�@Դ9@��@�(�@Χ�@�=q@�5?@��@��`@˅@ʰ!@��m@�v�@���@���@��@���@��u@��\@���@���@�v�@���@�p�@�V@��@�1'@���@���@���@��\@�@��;@��
@��
@��@��@���@�$�@��7@�7L@��u@�ƨ@��@���@�X@�Z@��@��w@���@�
=@��@�hs@�%@�/@�O�@�/@��`@� �@���@�=q@���@��/@��D@�Z@�  @���@�t�@�"�@��@��@���@�$�@��-@��@��@���@�Ĝ@��9@��@�Z@�33@���@��@��;@�K�@�o@��@��+@�5?@��T@��^@��7@�?}@��/@�j@�9X@�  @���@��@��@�;d@��@��R@�5?@��7@�7L@�%@�j@�A�@�(�@��;@���@�\)@�"�@�
=@��y@���@��+@�V@�{@���@�x�@�&�@��D@��@��@�\)@�o@��y@��@�ȴ@��R@��\@�E�@�-@��@��^@�O�@��@���@�A�@��w@�\)@�33@��y@���@�ff@��@��^@���@�?}@���@�(�@���@���@��@��y@��y@��y@���@��+@�v�@�n�@�^5@�E�@�@��^@���@�hs@�X@�?}@�/@�&�@�V@���@��/@��D@�(�@�w@;d@
=@~��@~{@}�T@}/@|�@|1@{"�@z�\@z�@y��@y�@zJ@y�#@yx�@x�@w�P@w+@w
=@v��@v5?@v@u?}@tZ@sƨ@sS�@r��@r��@q��@qhs@q7L@q%@p��@p �@o�@oK�@n�y@n��@nE�@m�T@m��@mp�@l��@l��@l��@lz�@lI�@lI�@lI�@l9X@l�@l1@k�m@k�
@k�F@k��@kt�@kC�@k"�@j�\@ihs@hĜ@h��@g�@g;d@g
=@f��@e�T@f{@fE�@e?}@dZ@c�m@c��@cdZ@c@b~�@ahs@aG�@`��@`Ĝ@`r�@`��@_��@^�y@^�@^�@^�@^�@^ȴ@^�R@^��@^�+@]�@]?}@\�j@\(�@[�
@[�@[C�@[@Z��@Z�!@Zn�@Z=q@Z-@Y��@Y��@Z�@Y��@Zn�@Z-@Z�@Z�@ZJ@Y��@Y��@Y��@Y�7@Yx�@Y&�@X��@X�9@X�9@X�u@Xr�@W��@W�@X  @W�@W�@W�@V��@V5?@U�@U�-@U`B@Tj@S�F@S�@S�@S"�@R�!@Q�@Q&�@Pr�@PbN@PQ�@PQ�@P1'@Pb@O�@N�R@N�+@Nff@N{@M��@M�-@M��@M�@M/@L�/@L�j@LI�@K�m@K��@Kt�@KdZ@KC�@K"�@K@J�!@J��@J�\@Jn�@J^5@JM�@J=q@J�@I�@I�^@I��@Ix�@I7L@I�@H��@H�`@H��@HĜ@H��@G�@F�y@F��@F��@F�+@Fff@F$�@E`B@D�@Dj@D�@C��@C�F@C�@CdZ@C33@Co@B�H@B�\@A�@A��@A%@@�u@@b@?�P@?+@>�y@>��@>V@>5?@=�@=�T@=��@=�@=?}@<�D@<j@;�
@:�H@:~�@:^5@:�@9��@9�7@8�`@81'@7�w@7|�@7l�@7K�@7;d@6��@6�@6�R@6��@6��@6�+@65?@5�T@5p�@5/@4�/@4��@4I�@41@3��@3��@3�m@3��@3t�@3t�@3dZ@3C�@3"�@3o@3@2�@2��@2�\@2n�@2n�@2�@1�^@1hs@1%@0��@0��@0�9@0�u@0r�@0 �@/�w@/|�@/\)@/\)@/\)@/K�@/+@.�y@.ȴ@.v�@.V@.E�@.5?@.5?@.@-�T@-�T@-��@-��@,�/@,��@,j@,(�@+��@+�m@+ƨ@+"�@*-@)�#@)�^@)X@(��@(b@'�;@'�w@'l�@&ȴ@&ff@&E�@&{@%�@&@%��@%�-@%��@%�@%p�@%O�@%/@$�@$��@$�j@$��@$�D@$Z@$I�@$1@#�F@#dZ@#"�@#o@"��@"�\@"n�@"M�@"-@"�@"J@!��@!x�@!&�@!�@!%@ �`@ ��@ ��@ Ĝ@ ��@ bN@�w@+@��@��@E�@@�-@O�@V@�/@Z@ƨ@�@t�@S�@C�@"�@�H@��@��@�!@��@~�@�@��@X@�9@bN@Q�@1'@1'@1'@ �@  @�@�;@��@��@|�@;d@��@�@��@v�@E�@{@@�@�@��@p�@?}@�@�j@z�@Z@I�@I�@�@1@�m@ƨ@�H@��@�!@M�@-@J@J@J@��@��@��@�@�@�#@��@�^@��@��@��@�7@�7@x�@G�@�`@��@�u@ �@�@��@l�@;d@��@@�T@��@�-@�@�@p�@`B@O�@/@��@�/@�@�D@(�@��@�@�@dZ@S�@C�@
�@
�\@
-@
�@	�#@	��@	7L@	�@	%@�`@�`@�9@�9@r�@Q�@b@�@�w@|�@\)@
=@�y@ȴ@�R@�R@��@��@��@��@�+@v�@v�@E�@�T@@�-@�h@?}@��@�/@�j@�@��@�D@z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�HB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��BT�B��B�hB�DB�B�Bz�Bp�Bl�Bo�Bo�Bu�Bv�Bu�Bt�Bs�Bt�Bp�Bo�Bl�Be`B_;BT�BL�BG�BD�B@�B9XB�B�BoB
=BB��B��B�#B��B��BǮB��B�B�uB�DB�Bw�B\)BK�BD�B6FB)�B�B1BB  B��B�B�B��BB�^B�-B�B��B�VBx�BdZBQ�BM�BB�B49B1'B-B$�B�BuBhBPB
=BB��B�B�
B�B��B��BĜB�dB�3B��B�VB�B}�By�Bs�Bk�BaHB>wB33B(�B�B1B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�fB
�HB
�BB
�5B
�
B
��B
��B
ƨB
��B
�wB
�jB
�XB
�LB
�?B
�-B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�hB
�PB
�1B
�B
{�B
x�B
v�B
o�B
n�B
l�B
k�B
jB
e`B
cTB
aHB
`BB
^5B
]/B
ZB
W
B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
S�B
R�B
K�B
H�B
E�B
@�B
=qB
>wB
>wB
>wB
;dB
8RB
33B
1'B
0!B
,B
+B
)�B
(�B
&�B
"�B
�B
�B
�B
�B
�B
uB
hB
VB
DB
	7B
B
B
B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�sB	�mB	�fB	�`B	�ZB	�HB	�NB	�/B	�/B	�)B	�#B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�;B	�HB	�ZB	�`B	�fB	�fB	�mB	�fB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
	7B
JB
JB
VB
\B
bB
oB
oB
uB
�B
�B
�B
&�B
.B
0!B
33B
6FB
6FB
7LB
7LB
;dB
@�B
A�B
D�B
F�B
G�B
I�B
K�B
J�B
K�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
S�B
S�B
S�B
R�B
P�B
Q�B
W
B
ZB
[#B
\)B
]/B
^5B
_;B
`BB
aHB
dZB
iyB
k�B
m�B
m�B
m�B
n�B
o�B
p�B
r�B
u�B
|�B
�B
�%B
�JB
�bB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�3B
�FB
�LB
�qB
��B
��B
B
B
ÖB
ĜB
ƨB
ƨB
ƨB
ɺB
��B
��B
��B
��B
�B
�#B
�;B
�TB
�`B
�fB
�sB
�B
�B
�B
��B
��B
��B
��B  BBBBB+B+B1B1B	7BDBPB\BhBoBuB{B{B�B�B�B�B�B�B"�B&�B'�B)�B+B.B0!B33B7LB9XB=qB>wB@�BA�BC�BE�BH�BJ�BK�BK�BL�BM�BM�BN�BP�BQ�BR�BS�BS�BVBVBVBW
BXBZB[#B\)B]/B^5B_;B`BBaHBbNBdZBe`BffBgmBgmBgmBgmBhsBhsBiyBiyBjBjBk�Bl�Bm�Bn�Br�Bw�By�By�B}�B� B�B�B�1B�=B�JB�JB�JB�JB�JB�JB�DB�JB�VB�bB�hB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�9B�9B�?B�?B�FB�LB�RB�RB�RB�RB�RB�RB�RB�XB�XB�XB�XB�XB�XB�^B�dB�dB�dB�jB�jB�qB�wB�wB�}B��B��BBÖBŢBƨBƨBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�
B�
B�
B�B�B�B�B�B�B�#B�#B�#B�/B�5B�;B�BB�HB�NB�`B�fB�fB�fB�fB�fB�mB�mB�sB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  B  B  BBBBBBBBBBBBBBBBBB%B%B%B%B+B+B+B+B1B1B	7B	7B	7B	7B	7B
=B
=BDBDBDBDBJBDBJBJBJBPBPBPBPBPBVBVBVBVBVB\BbBbBbBbBbBhBhBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B �B �B �B �B!�B"�B"�B#�B#�B$�B$�B%�B%�B%�B&�B'�B'�B'�B'�B'�B(�B(�B(�B(�B(�B(�B(�B)�B+B+B,B,B-B-B-B-B-B-B-B-B-B.B.B.B.B/B/B/B/B0!B0!B0!B0!B1'B1'B1'B2-B2-B2-B2-B2-B2-B33B33B33B33B49B49B5?B5?B5?B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB6FB7LB7LB7LB7LB8RB8RB9XB9XB9XB9XB:^B;dB;dB;dB<jB<jB<jB<jB<jB<jB<jB=qB=qB=qB=qB>wB?}B?}B?}B?}B?}B?}B@�B@�BA�BA�BA�BA�BB�BB�BB�BB�BB�BC�BC�BC�BC�BD�BD�BD�BD�BE�BE�BE�BE�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BG�BG�BG�BG�BH�BH�BH�BI�BI�BI�BI�BI�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B�HB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��BT�B��B�hB�DB�B�Bz�Bp�Bl�Bo�Bo�Bu�Bv�Bu�Bt�Bs�Bt�Bp�Bo�Bl�Be`B_;BT�BL�BG�BD�B@�B9XB�B�BoB
=BB��B��B�#B��B��BǮB��B�B�uB�DB�Bw�B\)BK�BD�B6FB)�B�B1BB  B��B�B�B��BB�^B�-B�B��B�VBx�BdZBQ�BM�BB�B49B1'B-B$�B�BuBhBPB
=BB��B�B�
B�B��B��BĜB�dB�3B��B�VB�B}�By�Bs�Bk�BaHB>wB33B(�B�B1B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�fB
�HB
�BB
�5B
�
B
��B
��B
ƨB
��B
�wB
�jB
�XB
�LB
�?B
�-B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�hB
�PB
�1B
�B
{�B
x�B
v�B
o�B
n�B
l�B
k�B
jB
e`B
cTB
aHB
`BB
^5B
]/B
ZB
W
B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
S�B
R�B
K�B
H�B
E�B
@�B
=qB
>wB
>wB
>wB
;dB
8RB
33B
1'B
0!B
,B
+B
)�B
(�B
&�B
"�B
�B
�B
�B
�B
�B
uB
hB
VB
DB
	7B
B
B
B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�sB	�mB	�fB	�`B	�ZB	�HB	�NB	�/B	�/B	�)B	�#B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�;B	�HB	�ZB	�`B	�fB	�fB	�mB	�fB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
	7B
JB
JB
VB
\B
bB
oB
oB
uB
�B
�B
�B
&�B
.B
0!B
33B
6FB
6FB
7LB
7LB
;dB
@�B
A�B
D�B
F�B
G�B
I�B
K�B
J�B
K�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
S�B
S�B
S�B
R�B
P�B
Q�B
W
B
ZB
[#B
\)B
]/B
^5B
_;B
`BB
aHB
dZB
iyB
k�B
m�B
m�B
m�B
n�B
o�B
p�B
r�B
u�B
|�B
�B
�%B
�JB
�bB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�3B
�FB
�LB
�qB
��B
��B
B
B
ÖB
ĜB
ƨB
ƨB
ƨB
ɺB
��B
��B
��B
��B
�B
�#B
�;B
�TB
�`B
�fB
�sB
�B
�B
�B
��B
��B
��B
��B  BBBBB+B+B1B1B	7BDBPB\BhBoBuB{B{B�B�B�B�B�B�B"�B&�B'�B)�B+B.B0!B33B7LB9XB=qB>wB@�BA�BC�BE�BH�BJ�BK�BK�BL�BM�BM�BN�BP�BQ�BR�BS�BS�BVBVBVBW
BXBZB[#B\)B]/B^5B_;B`BBaHBbNBdZBe`BffBgmBgmBgmBgmBhsBhsBiyBiyBjBjBk�Bl�Bm�Bn�Br�Bw�By�By�B}�B� B�B�B�1B�=B�JB�JB�JB�JB�JB�JB�DB�JB�VB�bB�hB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�9B�9B�?B�?B�FB�LB�RB�RB�RB�RB�RB�RB�RB�XB�XB�XB�XB�XB�XB�^B�dB�dB�dB�jB�jB�qB�wB�wB�}B��B��BBÖBŢBƨBƨBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�
B�
B�
B�B�B�B�B�B�B�#B�#B�#B�/B�5B�;B�BB�HB�NB�`B�fB�fB�fB�fB�fB�mB�mB�sB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  B  B  BBBBBBBBBBBBBBBBBB%B%B%B%B+B+B+B+B1B1B	7B	7B	7B	7B	7B
=B
=BDBDBDBDBJBDBJBJBJBPBPBPBPBPBVBVBVBVBVB\BbBbBbBbBbBhBhBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B �B �B �B �B �B!�B"�B"�B#�B#�B$�B$�B%�B%�B%�B&�B'�B'�B'�B'�B'�B(�B(�B(�B(�B(�B(�B(�B)�B+B+B,B,B-B-B-B-B-B-B-B-B-B.B.B.B.B/B/B/B/B0!B0!B0!B0!B1'B1'B1'B2-B2-B2-B2-B2-B2-B33B33B33B33B49B49B5?B5?B5?B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB6FB7LB7LB7LB7LB8RB8RB9XB9XB9XB9XB:^B;dB;dB;dB<jB<jB<jB<jB<jB<jB<jB=qB=qB=qB=qB>wB?}B?}B?}B?}B?}B?}B@�B@�BA�BA�BA�BA�BB�BB�BB�BB�BB�BC�BC�BC�BC�BD�BD�BD�BD�BE�BE�BE�BE�BF�BF�BF�BF�BF�BF�BF�BF�BF�BF�BG�BG�BG�BG�BH�BH�BH�BI�BI�BI�BI�BI�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210618090036                              AO  ARCAADJP                                                                    20210618090036    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210618090036  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210618090036  QCF$                G�O�G�O�G�O�8000            