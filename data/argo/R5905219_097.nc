CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-12-09T18:40:22Z creation;2020-12-09T18:40:23Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ͱ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20201209184022  20201209185219  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               aA   JA                                  2B  A   APEX                            7906                            051216                          846 @�M�����1   @�M�5��@3�/��w�d}/��w1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D���D�@ D�� D�� D���D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�C3D̀ D�� D�  D�@ D΀ D�� D���D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D���D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߃3D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�3D�C3D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Mp�@��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB�B=qB=qB!=qB)=qB1=qB8�BA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B�k�BĞ�BȞ�B̞�B�k�BԞ�B؞�Bܞ�B���B䞸B螸B잸B���B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&h�C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>5�C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�4{C�'�C�'�C��C�'�C�'�C��C��C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�4{C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��DqD��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D=D�=D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,qD,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8�=D9=D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DFqDF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM=DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY�=DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do=Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�F�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D��D���D��D�I�D���D���D��D�I�D���D���D��D�F�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D�ƹD�	�D�I�D���D���D�	�D�I�D���D���D��D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D��D��D�D�I�D���D���D�	�D�I�D���D���D��D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D��D��D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D��D�I�D���D���D�	�D�I�D���D���D��D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D�D���D�	�D�I�DÉ�D���D�	�D�I�Dĉ�D���D�	�D�I�Dŉ�D���D�	�D�I�DƉ�D���D�	�D�I�Dǉ�D���D�	�D�I�Dȉ�D���D�	�D�I�Dɉ�D���D�	�D�I�Dʉ�D���D�	�D�I�Dˉ�D���D�	�D�I�D̉�D���D�	�D�MD͉�D���D�	�D�I�DΉ�D���D��D�I�Dω�D���D�	�D�I�DЉ�D���D�	�D�I�Dщ�D���D�	�D�I�D҉�D���D�	�D�I�DӉ�D���D�	�D�I�Dԉ�D���D�	�D�I�DՉ�D���D��D�I�D։�D���D�	�D�I�D׉�D���D�	�D�I�D؉�D���D�	�D�I�Dى�D���D�	�D�I�Dډ�D���D�	�D�I�Dۉ�D���D�	�D�I�D܉�D���D�	�D�I�D݉�D���D�	�D�I�Dމ�D���D�	�D�I�DߍD���D�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D��D���D�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D�D��D�D�MD��D���D��D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�MD��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D���D���D��D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D�D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ZA�dZA�bNA�dZA�n�A�p�A�x�A�z�A�z�A�|�A�z�A�z�A�~�AمAمAه+AكAفA�~�AفA�~�AمAكAى7Aى7Aى7AٍPAّhAٓuAٕ�Aٕ�Aٗ�Aٕ�Aٗ�A٣�Aٟ�AټjA�ƨA���A���A���A�ƨA���Aٴ9AفA�-A���A��Aї�A��A��A�1A�E�A��Aŉ7A�JA�dZA�G�A��mA´9A��-A�A�A��uA���A�ƨA���A��+A��#A���A���A��A� �A�l�A���A��RA�hsA��A�`BA���A��A��/A�
=A��A���A�$�A��7A�5?A��A���A���A��uA���A�1'A�VA���A��!A�G�A��uA��A��TA��yA��RA��A�^5A��A�$�A��mA�+A��A���A��RA��A� �A��A�S�A�G�A��FA�{A?}A~(�A}�A|��A{\)AzVAyC�AvbNArĜAqhsAq�AqXAqS�Ap1Ak��Ah�`AgG�Ae��AdJAbn�A`��A^bNA^-A]��A]�A]7LA[�;AZz�AXjAW+AT��ASVAR��AQoAO/AM�AJ�AH$�AG�AG�;AG�PAE��ABZAA��A?�TA>�yA>9XA;��A;x�A9�-A7�A69XA5oA4r�A3/A1�;A1�A/��A.=qA-33A,��A+�wA*z�A)��A)?}A'dZA&�`A&$�A%`BA%
=A$�DA$(�A#�A#�-A#dZA!��Ap�A�A�!A��AoA�A&�A�jA�A��A=qAK�A�AVA�A-A�7A7LA-At�A�A��A1AA�wA
��A
9XA	�A	t�A��AM�Ar�A��A��A+A�9A�
A%A �DA 1'@��\@�x�@�r�@�\)@��y@�M�@�Z@��P@�;d@��y@�{@��@��@���@���@���@��@��@��@���@�dZ@��@��@���@柾@�@�t�@�!@�hs@�Z@ޏ\@ݑh@�b@ڧ�@��@�?}@ؼj@�(�@��@�V@� �@�v�@�/@���@���@ͺ^@ͩ�@��@�1'@ʏ\@ȣ�@���@�l�@�@�=q@�hs@�(�@Ý�@�|�@�@��@���@�X@�V@�Ĝ@�(�@�dZ@��y@�M�@���@�`B@��9@�1@�33@��@��R@��@��/@�r�@�b@�S�@��!@�=q@��@���@�V@�z�@��
@���@�dZ@���@��+@�$�@���@��h@�p�@��@���@�Q�@� �@��;@��P@�C�@��@�M�@���@��T@��#@��^@��#@�J@�ff@���@�V@���@�A�@� �@�b@�b@�dZ@�33@��@���@���@�ff@�M�@�-@��#@���@���@�?}@�V@��@��D@��m@��@�l�@�33@�+@�o@�ȴ@�v�@�ff@�~�@���@���@���@�n�@�=q@��@���@�`B@�7L@�&�@�%@���@�(�@��@��F@�t�@�S�@�;d@�@��R@�5?@��7@�%@���@�Ĝ@��9@�r�@���@��
@���@�|�@�l�@�\)@�o@��!@���@���@��\@��+@��\@�V@��@�{@�@�7L@�z�@�I�@�b@�ƨ@���@���@���@���@�t�@�\)@�33@�
=@��@�ff@�$�@��@�x�@�G�@��@���@���@�1'@��@�b@�1@�  @��;@���@�+@��@���@�$�@��@��@��h@�%@��D@�1'@��@��@�l�@�C�@�
=@���@��H@��R@�v�@��@��@���@���@��j@��u@�1'@���@�t�@�\)@��@�~�@�ff@�5?@�@�x�@�V@��/@��9@�bN@��@��m@���@�|�@�\)@�33@��@��H@���@�~�@�{@���@�hs@��@��9@�j@�A�@� �@�1@�  @���@���@�l�@�C�@�+@�o@��y@��R@�n�@�M�@���@��h@�?}@���@��@�z�@�z�@�r�@�I�@�1@��@�S�@�;d@�"�@�ȴ@�n�@�n�@�V@�J@��@���@�9X@��@�1@�@��@\)@K�@~�y@~E�@}�T@}/@|��@|��@|1@{33@z��@z��@z��@z�@zM�@y��@y�7@x�@w�@w|�@w;d@v�@vff@v@u�@v@v5?@v@u�-@u�@uO�@uV@t9X@s�F@sC�@r��@r=q@q�@q��@qhs@q&�@pb@o|�@o;d@n��@nv�@nv�@n$�@m��@m�@l�@ko@j��@i%@hb@g�w@g�;@h�9@h�u@g�@g;d@g��@h �@h  @g|�@f��@f��@e�@e�T@fV@fff@fff@fV@e�T@e�-@e�h@e�@eO�@eO�@e?}@d��@dj@c�
@cdZ@c33@b�!@bn�@b-@a�#@a��@ax�@`��@`�9@`1'@_�;@_��@_\)@_�@^�R@^�+@]�T@]��@]�h@]�@]p�@]V@\9X@\(�@\1@[�m@[�
@[�F@[��@[��@[��@[�@[S�@Y�#@Y&�@Y�@Y%@X��@X��@XbN@X1'@X �@Xb@W�@W�@W;d@V�@VV@V@U�-@U/@TI�@SS�@S@R�@R��@Rn�@R=q@R�@Q�^@Qx�@QX@Q7L@Q�@P�`@Pr�@O�w@O�P@O�P@O��@O|�@O;d@N�@N�+@NE�@M��@MO�@L�@L��@L�@K�@KC�@J��@J-@I�@IX@H��@H �@G��@G|�@G+@G�@F�+@F5?@F{@E��@E�h@E�@E?}@D��@DI�@D9X@D1@Ct�@CC�@C"�@B�@B��@B^5@B-@A�#@A��@Ax�@AX@@�u@@  @?�w@?��@?|�@?\)@?;d@?+@?+@>��@>ff@=@=O�@=/@<�@<��@<Z@<Z@<9X@<1@;ƨ@;t�@:��@:�!@:n�@:J@97L@8�`@8��@8��@8bN@8 �@7�@7l�@7;d@7�@6�y@6��@6�+@6V@6@5�-@5O�@5�@4��@4�/@4��@4�j@4�D@4j@4Z@49X@3�
@3��@3�@3"�@2��@2M�@1�@1��@1�@0��@0Ĝ@0bN@01'@0  @/�@/�@/;d@/
=@.�y@.�@.�R@.�+@.v�@.$�@.@-�-@-?}@,�/@,�j@,��@,(�@+��@+o@*�\@*^5@*-@)��@)7L@(r�@(Q�@(A�@(A�@(A�@(A�@(A�@(A�@( �@(b@(b@(  @'�@'�;@'�;@'��@'�w@'��@'|�@';d@&��@&�R@&v�@&E�@&$�@%�T@%�-@%`B@%V@$�@$�D@$z�@$j@$Z@#�m@#ƨ@#�F@#dZ@#C�@#o@"�H@"�!@"��@"~�@"-@!��@!�^@!�7@!hs@!X@!G�@!&�@ ��@ �9@ r�@ Q�@ 1'@�;@�@|�@+@�@
=@��@ȴ@v�@5?@��@��@p�@V@��@��@j@I�@�F@�@C�@�H@��@��@~�@M�@=q@J@��@��@�7@x�@G�@%@��@�9@�@A�@ �@  @�@�;@��@�@�P@l�@+@�@��@$�@��@@��@p�@?}@��@��@��@j@Z@(�@�m@ƨ@�@S�@C�@C�@33@"�@@��@�\@n�@^5@=q@J@�#@��@�7@x�@G�@&�@�@%@��@r�@bN@Q�@ �@b@�@��@�@�P@|�@l�@\)@;d@
=@�@�R@V@5?@�@��@�-@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ZA�dZA�bNA�dZA�n�A�p�A�x�A�z�A�z�A�|�A�z�A�z�A�~�AمAمAه+AكAفA�~�AفA�~�AمAكAى7Aى7Aى7AٍPAّhAٓuAٕ�Aٕ�Aٗ�Aٕ�Aٗ�A٣�Aٟ�AټjA�ƨA���A���A���A�ƨA���Aٴ9AفA�-A���A��Aї�A��A��A�1A�E�A��Aŉ7A�JA�dZA�G�A��mA´9A��-A�A�A��uA���A�ƨA���A��+A��#A���A���A��A� �A�l�A���A��RA�hsA��A�`BA���A��A��/A�
=A��A���A�$�A��7A�5?A��A���A���A��uA���A�1'A�VA���A��!A�G�A��uA��A��TA��yA��RA��A�^5A��A�$�A��mA�+A��A���A��RA��A� �A��A�S�A�G�A��FA�{A?}A~(�A}�A|��A{\)AzVAyC�AvbNArĜAqhsAq�AqXAqS�Ap1Ak��Ah�`AgG�Ae��AdJAbn�A`��A^bNA^-A]��A]�A]7LA[�;AZz�AXjAW+AT��ASVAR��AQoAO/AM�AJ�AH$�AG�AG�;AG�PAE��ABZAA��A?�TA>�yA>9XA;��A;x�A9�-A7�A69XA5oA4r�A3/A1�;A1�A/��A.=qA-33A,��A+�wA*z�A)��A)?}A'dZA&�`A&$�A%`BA%
=A$�DA$(�A#�A#�-A#dZA!��Ap�A�A�!A��AoA�A&�A�jA�A��A=qAK�A�AVA�A-A�7A7LA-At�A�A��A1AA�wA
��A
9XA	�A	t�A��AM�Ar�A��A��A+A�9A�
A%A �DA 1'@��\@�x�@�r�@�\)@��y@�M�@�Z@��P@�;d@��y@�{@��@��@���@���@���@��@��@��@���@�dZ@��@��@���@柾@�@�t�@�!@�hs@�Z@ޏ\@ݑh@�b@ڧ�@��@�?}@ؼj@�(�@��@�V@� �@�v�@�/@���@���@ͺ^@ͩ�@��@�1'@ʏ\@ȣ�@���@�l�@�@�=q@�hs@�(�@Ý�@�|�@�@��@���@�X@�V@�Ĝ@�(�@�dZ@��y@�M�@���@�`B@��9@�1@�33@��@��R@��@��/@�r�@�b@�S�@��!@�=q@��@���@�V@�z�@��
@���@�dZ@���@��+@�$�@���@��h@�p�@��@���@�Q�@� �@��;@��P@�C�@��@�M�@���@��T@��#@��^@��#@�J@�ff@���@�V@���@�A�@� �@�b@�b@�dZ@�33@��@���@���@�ff@�M�@�-@��#@���@���@�?}@�V@��@��D@��m@��@�l�@�33@�+@�o@�ȴ@�v�@�ff@�~�@���@���@���@�n�@�=q@��@���@�`B@�7L@�&�@�%@���@�(�@��@��F@�t�@�S�@�;d@�@��R@�5?@��7@�%@���@�Ĝ@��9@�r�@���@��
@���@�|�@�l�@�\)@�o@��!@���@���@��\@��+@��\@�V@��@�{@�@�7L@�z�@�I�@�b@�ƨ@���@���@���@���@�t�@�\)@�33@�
=@��@�ff@�$�@��@�x�@�G�@��@���@���@�1'@��@�b@�1@�  @��;@���@�+@��@���@�$�@��@��@��h@�%@��D@�1'@��@��@�l�@�C�@�
=@���@��H@��R@�v�@��@��@���@���@��j@��u@�1'@���@�t�@�\)@��@�~�@�ff@�5?@�@�x�@�V@��/@��9@�bN@��@��m@���@�|�@�\)@�33@��@��H@���@�~�@�{@���@�hs@��@��9@�j@�A�@� �@�1@�  @���@���@�l�@�C�@�+@�o@��y@��R@�n�@�M�@���@��h@�?}@���@��@�z�@�z�@�r�@�I�@�1@��@�S�@�;d@�"�@�ȴ@�n�@�n�@�V@�J@��@���@�9X@��@�1@�@��@\)@K�@~�y@~E�@}�T@}/@|��@|��@|1@{33@z��@z��@z��@z�@zM�@y��@y�7@x�@w�@w|�@w;d@v�@vff@v@u�@v@v5?@v@u�-@u�@uO�@uV@t9X@s�F@sC�@r��@r=q@q�@q��@qhs@q&�@pb@o|�@o;d@n��@nv�@nv�@n$�@m��@m�@l�@ko@j��@i%@hb@g�w@g�;@h�9@h�u@g�@g;d@g��@h �@h  @g|�@f��@f��@e�@e�T@fV@fff@fff@fV@e�T@e�-@e�h@e�@eO�@eO�@e?}@d��@dj@c�
@cdZ@c33@b�!@bn�@b-@a�#@a��@ax�@`��@`�9@`1'@_�;@_��@_\)@_�@^�R@^�+@]�T@]��@]�h@]�@]p�@]V@\9X@\(�@\1@[�m@[�
@[�F@[��@[��@[��@[�@[S�@Y�#@Y&�@Y�@Y%@X��@X��@XbN@X1'@X �@Xb@W�@W�@W;d@V�@VV@V@U�-@U/@TI�@SS�@S@R�@R��@Rn�@R=q@R�@Q�^@Qx�@QX@Q7L@Q�@P�`@Pr�@O�w@O�P@O�P@O��@O|�@O;d@N�@N�+@NE�@M��@MO�@L�@L��@L�@K�@KC�@J��@J-@I�@IX@H��@H �@G��@G|�@G+@G�@F�+@F5?@F{@E��@E�h@E�@E?}@D��@DI�@D9X@D1@Ct�@CC�@C"�@B�@B��@B^5@B-@A�#@A��@Ax�@AX@@�u@@  @?�w@?��@?|�@?\)@?;d@?+@?+@>��@>ff@=@=O�@=/@<�@<��@<Z@<Z@<9X@<1@;ƨ@;t�@:��@:�!@:n�@:J@97L@8�`@8��@8��@8bN@8 �@7�@7l�@7;d@7�@6�y@6��@6�+@6V@6@5�-@5O�@5�@4��@4�/@4��@4�j@4�D@4j@4Z@49X@3�
@3��@3�@3"�@2��@2M�@1�@1��@1�@0��@0Ĝ@0bN@01'@0  @/�@/�@/;d@/
=@.�y@.�@.�R@.�+@.v�@.$�@.@-�-@-?}@,�/@,�j@,��@,(�@+��@+o@*�\@*^5@*-@)��@)7L@(r�@(Q�@(A�@(A�@(A�@(A�@(A�@(A�@( �@(b@(b@(  @'�@'�;@'�;@'��@'�w@'��@'|�@';d@&��@&�R@&v�@&E�@&$�@%�T@%�-@%`B@%V@$�@$�D@$z�@$j@$Z@#�m@#ƨ@#�F@#dZ@#C�@#o@"�H@"�!@"��@"~�@"-@!��@!�^@!�7@!hs@!X@!G�@!&�@ ��@ �9@ r�@ Q�@ 1'@�;@�@|�@+@�@
=@��@ȴ@v�@5?@��@��@p�@V@��@��@j@I�@�F@�@C�@�H@��@��@~�@M�@=q@J@��@��@�7@x�@G�@%@��@�9@�@A�@ �@  @�@�;@��@�@�P@l�@+@�@��@$�@��@@��@p�@?}@��@��@��@j@Z@(�@�m@ƨ@�@S�@C�@C�@33@"�@@��@�\@n�@^5@=q@J@�#@��@�7@x�@G�@&�@�@%@��@r�@bN@Q�@ �@b@�@��@�@�P@|�@l�@\)@;d@
=@�@�R@V@5?@�@��@�-@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�'B
�-B
�-B
�^B  B��B�DBI�B �B
��B
ɺB
�LB
�jB
�}B
��B
ǮB
�B
�fBB �B(�B)�B+B+B1'B;dBH�BW
B�uB�wB��B�BuB�B+B5?BJ�B8RB2-B0!B-B/B.B9XB33B!�B\BB�B�B�jB�FB�B��B�hB�JB�Bp�BW
BC�B33B�B�B{BPBB
��B
�B
�)B
�XB
��B
�oB
gmB
A�B
7LB
6FB
1'B
.B
.B
8RB
?}B
8RB
%�B

=B	��B
  B
B
B
B	�fB	ȴB	�RB	�3B	��B	�PB	�B	iyB	iyB	r�B	�PB	��B	�VB	�7B	~�B	u�B	m�B	`BB	]/B	W
B	L�B	G�B	:^B	+B	(�B	(�B	%�B	�B	bB		7B	%B	B	  B��B�B�B�fB�HB�/B�B��B��B��BǮBŢB��B�wB�jB�XB�FB�9B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B�hB�hB�bB�JB�JB�=B�=B�7B�1B�%B�B�B�B�B� B� B|�Bz�Bz�By�Bz�By�B{�Bw�Bv�B|�B{�B{�Bx�Bs�Bs�Bt�Bt�Bt�Bu�Bw�By�Bx�By�B{�B{�B|�B}�B}�B� B�B�B�+B�1B�B�B�1B�{B��B��B��B��B�B�B�B�!B�-B�RB�XB�XB�RB�RB�qB�jB�}B�wB�qB�jB�jB�dB�^B�qB�jB�jB�dBÖBŢBȴB��B�B�B�;B�HB�BB�NB�`B�mB�yB�B�B�B��B��B��B��B��B��B	B	%B		7B	PB	\B	hB	{B	�B	�B	�B	�B	"�B	&�B	(�B	+B	0!B	49B	7LB	9XB	:^B	?}B	G�B	J�B	L�B	P�B	R�B	VB	W
B	XB	[#B	]/B	aHB	cTB	dZB	e`B	ffB	iyB	jB	l�B	q�B	s�B	v�B	y�B	y�B	� B	�1B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�9B	�9B	�?B	�FB	�FB	�FB	�XB	�jB	�jB	�}B	B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�TB	�ZB	�ZB	�mB	�sB	�yB	�yB	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
B
B
B
B
B
%B
%B
%B
1B
1B
1B
+B
1B
	7B
	7B
	7B
DB
DB
DB
DB
JB
JB
JB
PB
VB
VB
VB
VB
VB
\B
bB
hB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
+B
+B
+B
+B
,B
,B
.B
.B
-B
-B
-B
-B
.B
.B
.B
-B
-B
.B
-B
/B
1'B
49B
33B
49B
5?B
49B
33B
2-B
1'B
2-B
33B
6FB
8RB
8RB
7LB
9XB
;dB
;dB
:^B
:^B
:^B
9XB
9XB
<jB
<jB
<jB
<jB
=qB
=qB
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
?}B
>wB
?}B
?}B
?}B
?}B
@�B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
B�B
C�B
C�B
D�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
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
N�B
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
W
B
W
B
W
B
W
B
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
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
bNB
bNB
bNB
bNB
bNB
bNB
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
gmB
gmB
gmB
hsB
iyB
iyB
jB
jB
k�B
k�B
k�B
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
q�B
q�B
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
v�B
w�B
w�B
w�B
w�B
w�B
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
y�B
y�B
y�B
y�B
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
{�B
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
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�'B
�-B
�-B
�^B  B��B�DBI�B �B
��B
ɺB
�LB
�jB
�}B
��B
ǮB
�B
�fBB �B(�B)�B+B+B1'B;dBH�BW
B�uB�wB��B�BuB�B+B5?BJ�B8RB2-B0!B-B/B.B9XB33B!�B\BB�B�B�jB�FB�B��B�hB�JB�Bp�BW
BC�B33B�B�B{BPBB
��B
�B
�)B
�XB
��B
�oB
gmB
A�B
7LB
6FB
1'B
.B
.B
8RB
?}B
8RB
%�B

=B	��B
  B
B
B
B	�fB	ȴB	�RB	�3B	��B	�PB	�B	iyB	iyB	r�B	�PB	��B	�VB	�7B	~�B	u�B	m�B	`BB	]/B	W
B	L�B	G�B	:^B	+B	(�B	(�B	%�B	�B	bB		7B	%B	B	  B��B�B�B�fB�HB�/B�B��B��B��BǮBŢB��B�wB�jB�XB�FB�9B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B�hB�hB�bB�JB�JB�=B�=B�7B�1B�%B�B�B�B�B� B� B|�Bz�Bz�By�Bz�By�B{�Bw�Bv�B|�B{�B{�Bx�Bs�Bs�Bt�Bt�Bt�Bu�Bw�By�Bx�By�B{�B{�B|�B}�B}�B� B�B�B�+B�1B�B�B�1B�{B��B��B��B��B�B�B�B�!B�-B�RB�XB�XB�RB�RB�qB�jB�}B�wB�qB�jB�jB�dB�^B�qB�jB�jB�dBÖBŢBȴB��B�B�B�;B�HB�BB�NB�`B�mB�yB�B�B�B��B��B��B��B��B��B	B	%B		7B	PB	\B	hB	{B	�B	�B	�B	�B	"�B	&�B	(�B	+B	0!B	49B	7LB	9XB	:^B	?}B	G�B	J�B	L�B	P�B	R�B	VB	W
B	XB	[#B	]/B	aHB	cTB	dZB	e`B	ffB	iyB	jB	l�B	q�B	s�B	v�B	y�B	y�B	� B	�1B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�9B	�9B	�?B	�FB	�FB	�FB	�XB	�jB	�jB	�}B	B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�TB	�ZB	�ZB	�mB	�sB	�yB	�yB	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
B
B
B
B
B
%B
%B
%B
1B
1B
1B
+B
1B
	7B
	7B
	7B
DB
DB
DB
DB
JB
JB
JB
PB
VB
VB
VB
VB
VB
\B
bB
hB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
$�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
+B
+B
+B
+B
,B
,B
.B
.B
-B
-B
-B
-B
.B
.B
.B
-B
-B
.B
-B
/B
1'B
49B
33B
49B
5?B
49B
33B
2-B
1'B
2-B
33B
6FB
8RB
8RB
7LB
9XB
;dB
;dB
:^B
:^B
:^B
9XB
9XB
<jB
<jB
<jB
<jB
=qB
=qB
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
?}B
>wB
?}B
?}B
?}B
?}B
@�B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
B�B
C�B
C�B
D�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
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
N�B
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
W
B
W
B
W
B
W
B
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
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
bNB
bNB
bNB
bNB
bNB
bNB
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
gmB
gmB
gmB
hsB
iyB
iyB
jB
jB
k�B
k�B
k�B
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
q�B
q�B
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
v�B
w�B
w�B
w�B
w�B
w�B
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
y�B
y�B
y�B
y�B
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
{�B
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
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20201210033954  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201209184022  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20201209184022  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201209184022  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201209184023  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20201209184023  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201209184023  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20201209184023  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20201209184023  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201209184023                      G�O�G�O�G�O�                JA  ARUP                                                                        20201209185219                      G�O�G�O�G�O�                