CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-07T00:35:24Z creation;2018-11-07T00:35:29Z conversion to V3.1;2019-12-19T07:24:46Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20181107003524  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              +A   JA  I2_0577_299                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @؎�""" 1   @؎� @4bGE8�5�dm�g��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D�|�D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB��B ��B(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�~�D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D�~�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HA��#A���AظRAضFAش9AضFAضFAظRAظRAظRAغ^Aغ^AؼjAؾwAظRAء�A�;dA���A�p�A�VÁA�C�A��A���AžwAº^A���A��A��A�$�A�Q�A���A�ZA��HA��FA�1A�p�A�~�A��!A�G�A���A�1A��^A�z�A��A�
=A���A�$�A�VA�ĜA�Q�A��`A�bA�ZA�A�A���A��-A��HA���A��FA���A���A�Q�A�;dA���A�9XA�S�A�&�A�$�A�oA�"�A���A��A�r�A��A��mA���A�C�A���A��wA���A��A�|�A���A�33A��TA�/A���A��hA��Az�yAxM�Aw�PAu��As�hAo�7An��An9XAl�+Ai�hAf�Ac��AcS�AaƨA`��A_VA]XA[l�AY�mAYK�AU��AS/AQ�AN9XAL$�AH�DAE�PACƨAA�PA?�A=�A<�9A<VA;?}A: �A9�TA9dZA8��A8��A7��A6��A5�A3�-A2v�A/�A.�A-��A,I�A)�mA(�/A'XA&9XA$�`A$(�A"�uA!�A =qA��A
=AZA{A|�AK�AAC�A��AA�\A�^AȴA�wA  A�FAC�A$�A��AbA"�A�+A�
A?}A
��A	��A-AdZAjAbNA�A$�A`BA�+A�A��A�9@�+@��#@�`B@��@�r�@��@�z�@�Z@��#@�V@�\)@���@�p�@�Ĝ@��H@���@���@���@�1'@�V@��@�G�@۶F@ّh@�
=@�1@�;d@�\)@��@�r�@�"�@���@�ȴ@֗�@��@���@���@ՙ�@Լj@��m@ӕ�@�@җ�@���@�@�`B@���@�Ĝ@���@Ѓ@�ƨ@�S�@��@˝�@�t�@�|�@�C�@��@�o@�~�@��/@�z�@�r�@�r�@� �@��;@�l�@�M�@�/@�7L@�&�@�z�@��@�M�@�~�@�+@��@���@�x�@�hs@�/@��@�j@�  @���@��\@���@���@�j@��@�t�@�\)@��!@���@�bN@�l�@��\@�p�@��j@��@�|�@�S�@��@��\@�M�@�J@��@�G�@��@���@�j@� �@��@�@��H@�=q@���@�p�@���@��`@�Ĝ@��@�(�@��
@�S�@�E�@�-@�J@�=q@���@�7L@�%@��D@�Z@�Z@��@��;@���@�K�@�"�@��H@��+@�=q@�X@�(�@�b@��m@��
@��m@���@�1@��@��@��@�1@���@�l�@�
=@���@��T@��h@�p�@�7L@��@���@�Ĝ@�z�@�Z@�1@�S�@�
=@���@�^5@�^5@�v�@��+@���@���@�~�@��@���@��`@�z�@�1'@�  @���@��@���@���@��@�C�@���@��+@�V@��@��7@�G�@���@��`@���@��9@�z�@�(�@��@��;@��P@�dZ@��@���@�-@���@�?}@���@��j@�Z@�b@��
@��P@�S�@�@��R@�v�@�{@���@�hs@�7L@�V@��`@�Ĝ@���@�Z@�(�@�  @��@��;@��
@��w@�K�@�"�@�o@��H@��R@��\@�~�@�n�@�M�@�J@���@���@�O�@��@��`@�Ĝ@��u@��D@��@�Z@�b@��m@��w@���@�33@��@�
=@���@��+@�@��#@��^@�hs@�V@���@�Q�@�(�@�b@�  @��;@��P@�\)@�33@��@���@�E�@���@���@��-@���@�x�@�`B@�&�@�V@��9@�1'@���@��P@��P@��P@�;d@��@��!@�J@�p�@�X@�O�@�G�@��@��9@�(�@��@�P@l�@K�@~�y@~V@~{@}�@}�@}��@}/@|�/@|��@|z�@|j@|9X@{�m@{ƨ@{dZ@{33@{@z�H@z��@z�\@z~�@zJ@yx�@x��@x�@x1'@w�;@wl�@v��@v�y@v�R@v{@u�@uV@t�@tj@s��@st�@s33@r�@r��@rn�@r�@q�#@q��@q&�@p��@p�u@p �@o��@ol�@oK�@n��@n5?@m�T@m�-@m�@mO�@mV@l�@l9X@l�@k�F@j�@j�\@j=q@j�@i��@i�#@i�^@i��@i7L@i&�@i�@i�@h��@h�`@hĜ@h�u@hA�@h  @g�@fȴ@e�@eO�@eV@d��@d�/@d��@d�j@d��@dj@d1@c��@cS�@b�H@b��@b�\@bn�@b-@ahs@aG�@a7L@a7L@a&�@a�@`��@`��@`b@_�;@_��@_|�@_|�@_l�@_\)@_;d@_
=@^��@^$�@]�T@]�T@]�T@]��@\�@\��@\I�@\9X@\1@[��@[t�@Z~�@ZJ@Y�^@Y&�@X��@X�u@XQ�@W�@W�w@W\)@V�R@V�+@VV@V5?@V5?@V@U�@UV@T�j@TZ@T9X@T(�@T�@S��@S�
@St�@S@R��@R=q@Q��@Qx�@Q7L@P��@Pr�@O�@O�w@O��@O;d@N�y@N��@M��@M`B@M�@L��@L�j@L��@Lz�@L1@K��@KS�@J�!@I�#@IX@I7L@I�@H��@HĜ@Hr�@H1'@H  @GK�@G+@F�R@Fv�@F5?@E�T@E��@E�h@E?}@D�@D��@D9X@C��@C��@B�H@Bn�@A��@Ahs@A�@@�`@@��@@ �@?�;@?|�@?\)@?�@>�y@>��@>ff@>5?@=p�@=�@=�@<�@;��@;�F@;�@;"�@:�@9��@9��@9X@9%@8bN@7�w@7\)@7;d@6ȴ@6��@6��@6v�@6ff@5�@5�-@5��@5V@4�@4(�@4I�@49X@4(�@3��@3�
@3��@3�@3t�@3S�@3C�@2�@2�\@2n�@1�@1��@1X@17L@0��@0�u@0A�@/�P@/K�@/+@/
=@.�y@.�R@.v�@.V@.V@.E�@.@-�@-/@,�/@,�D@,�D@,�D@,�D@,�D@,z�@,j@+�m@+dZ@+"�@*��@*��@*�\@*�\@*~�@*~�@*M�@*J@)�@)��@)��@)x�@)X@)G�@)�@(�9@(A�@(b@(  @'��@'�P@'|�@'l�@'+@'
=@'
=@&��@&�@&�R@&v�@&ff@&E�@&@%�h@$��@$�/@$�@$��@$�D@$j@$Z@$I�@$(�@$1@#��@#�m@#�m@#�
@#�F@#�@#t�@#33@"�H@"�!@"�\@"=q@!��@!X@!&�@!%@ �`@ �9@ �u@ bN@ Q�@ b@�;@�;@�@l�@\)@;d@�y@�@�R@��@�+@{@�T@��@�@`B@?}@V@��@�@z�@I�@1@�m@�m@�
@ƨ@�@C�@@��@��@~�@n�@n�@=q@�@�@��@�7@x�@hs@X@7L@%@��@��@Ĝ@�9@�@bN@A�@ �@  @��@�@��@|�@+@�y@�+@V@E�@$�@�T@�@`B@/@��@��@��@�D@�D@z�@Z@j@j@j@I�@�@��@�
@dZ@@�H@��@��@n�@-@��@��@��@�7@hs@hs@X@&�@�`@�9@�u@1'@  @��@�P@\)@\)@K�@K�@;d@
=@�R@v�@@�h@`B@�@�@�@�D@z�@9X@(�@1@�
@�F@�F@��@t�@S�@33@
�H@
^5@
=q@
=q@
=q@
-@
-@
-@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HA��#A���AظRAضFAش9AضFAضFAظRAظRAظRAغ^Aغ^AؼjAؾwAظRAء�A�;dA���A�p�A�VÁA�C�A��A���AžwAº^A���A��A��A�$�A�Q�A���A�ZA��HA��FA�1A�p�A�~�A��!A�G�A���A�1A��^A�z�A��A�
=A���A�$�A�VA�ĜA�Q�A��`A�bA�ZA�A�A���A��-A��HA���A��FA���A���A�Q�A�;dA���A�9XA�S�A�&�A�$�A�oA�"�A���A��A�r�A��A��mA���A�C�A���A��wA���A��A�|�A���A�33A��TA�/A���A��hA��Az�yAxM�Aw�PAu��As�hAo�7An��An9XAl�+Ai�hAf�Ac��AcS�AaƨA`��A_VA]XA[l�AY�mAYK�AU��AS/AQ�AN9XAL$�AH�DAE�PACƨAA�PA?�A=�A<�9A<VA;?}A: �A9�TA9dZA8��A8��A7��A6��A5�A3�-A2v�A/�A.�A-��A,I�A)�mA(�/A'XA&9XA$�`A$(�A"�uA!�A =qA��A
=AZA{A|�AK�AAC�A��AA�\A�^AȴA�wA  A�FAC�A$�A��AbA"�A�+A�
A?}A
��A	��A-AdZAjAbNA�A$�A`BA�+A�A��A�9@�+@��#@�`B@��@�r�@��@�z�@�Z@��#@�V@�\)@���@�p�@�Ĝ@��H@���@���@���@�1'@�V@��@�G�@۶F@ّh@�
=@�1@�;d@�\)@��@�r�@�"�@���@�ȴ@֗�@��@���@���@ՙ�@Լj@��m@ӕ�@�@җ�@���@�@�`B@���@�Ĝ@���@Ѓ@�ƨ@�S�@��@˝�@�t�@�|�@�C�@��@�o@�~�@��/@�z�@�r�@�r�@� �@��;@�l�@�M�@�/@�7L@�&�@�z�@��@�M�@�~�@�+@��@���@�x�@�hs@�/@��@�j@�  @���@��\@���@���@�j@��@�t�@�\)@��!@���@�bN@�l�@��\@�p�@��j@��@�|�@�S�@��@��\@�M�@�J@��@�G�@��@���@�j@� �@��@�@��H@�=q@���@�p�@���@��`@�Ĝ@��@�(�@��
@�S�@�E�@�-@�J@�=q@���@�7L@�%@��D@�Z@�Z@��@��;@���@�K�@�"�@��H@��+@�=q@�X@�(�@�b@��m@��
@��m@���@�1@��@��@��@�1@���@�l�@�
=@���@��T@��h@�p�@�7L@��@���@�Ĝ@�z�@�Z@�1@�S�@�
=@���@�^5@�^5@�v�@��+@���@���@�~�@��@���@��`@�z�@�1'@�  @���@��@���@���@��@�C�@���@��+@�V@��@��7@�G�@���@��`@���@��9@�z�@�(�@��@��;@��P@�dZ@��@���@�-@���@�?}@���@��j@�Z@�b@��
@��P@�S�@�@��R@�v�@�{@���@�hs@�7L@�V@��`@�Ĝ@���@�Z@�(�@�  @��@��;@��
@��w@�K�@�"�@�o@��H@��R@��\@�~�@�n�@�M�@�J@���@���@�O�@��@��`@�Ĝ@��u@��D@��@�Z@�b@��m@��w@���@�33@��@�
=@���@��+@�@��#@��^@�hs@�V@���@�Q�@�(�@�b@�  @��;@��P@�\)@�33@��@���@�E�@���@���@��-@���@�x�@�`B@�&�@�V@��9@�1'@���@��P@��P@��P@�;d@��@��!@�J@�p�@�X@�O�@�G�@��@��9@�(�@��@�P@l�@K�@~�y@~V@~{@}�@}�@}��@}/@|�/@|��@|z�@|j@|9X@{�m@{ƨ@{dZ@{33@{@z�H@z��@z�\@z~�@zJ@yx�@x��@x�@x1'@w�;@wl�@v��@v�y@v�R@v{@u�@uV@t�@tj@s��@st�@s33@r�@r��@rn�@r�@q�#@q��@q&�@p��@p�u@p �@o��@ol�@oK�@n��@n5?@m�T@m�-@m�@mO�@mV@l�@l9X@l�@k�F@j�@j�\@j=q@j�@i��@i�#@i�^@i��@i7L@i&�@i�@i�@h��@h�`@hĜ@h�u@hA�@h  @g�@fȴ@e�@eO�@eV@d��@d�/@d��@d�j@d��@dj@d1@c��@cS�@b�H@b��@b�\@bn�@b-@ahs@aG�@a7L@a7L@a&�@a�@`��@`��@`b@_�;@_��@_|�@_|�@_l�@_\)@_;d@_
=@^��@^$�@]�T@]�T@]�T@]��@\�@\��@\I�@\9X@\1@[��@[t�@Z~�@ZJ@Y�^@Y&�@X��@X�u@XQ�@W�@W�w@W\)@V�R@V�+@VV@V5?@V5?@V@U�@UV@T�j@TZ@T9X@T(�@T�@S��@S�
@St�@S@R��@R=q@Q��@Qx�@Q7L@P��@Pr�@O�@O�w@O��@O;d@N�y@N��@M��@M`B@M�@L��@L�j@L��@Lz�@L1@K��@KS�@J�!@I�#@IX@I7L@I�@H��@HĜ@Hr�@H1'@H  @GK�@G+@F�R@Fv�@F5?@E�T@E��@E�h@E?}@D�@D��@D9X@C��@C��@B�H@Bn�@A��@Ahs@A�@@�`@@��@@ �@?�;@?|�@?\)@?�@>�y@>��@>ff@>5?@=p�@=�@=�@<�@;��@;�F@;�@;"�@:�@9��@9��@9X@9%@8bN@7�w@7\)@7;d@6ȴ@6��@6��@6v�@6ff@5�@5�-@5��@5V@4�@4(�@4I�@49X@4(�@3��@3�
@3��@3�@3t�@3S�@3C�@2�@2�\@2n�@1�@1��@1X@17L@0��@0�u@0A�@/�P@/K�@/+@/
=@.�y@.�R@.v�@.V@.V@.E�@.@-�@-/@,�/@,�D@,�D@,�D@,�D@,�D@,z�@,j@+�m@+dZ@+"�@*��@*��@*�\@*�\@*~�@*~�@*M�@*J@)�@)��@)��@)x�@)X@)G�@)�@(�9@(A�@(b@(  @'��@'�P@'|�@'l�@'+@'
=@'
=@&��@&�@&�R@&v�@&ff@&E�@&@%�h@$��@$�/@$�@$��@$�D@$j@$Z@$I�@$(�@$1@#��@#�m@#�m@#�
@#�F@#�@#t�@#33@"�H@"�!@"�\@"=q@!��@!X@!&�@!%@ �`@ �9@ �u@ bN@ Q�@ b@�;@�;@�@l�@\)@;d@�y@�@�R@��@�+@{@�T@��@�@`B@?}@V@��@�@z�@I�@1@�m@�m@�
@ƨ@�@C�@@��@��@~�@n�@n�@=q@�@�@��@�7@x�@hs@X@7L@%@��@��@Ĝ@�9@�@bN@A�@ �@  @��@�@��@|�@+@�y@�+@V@E�@$�@�T@�@`B@/@��@��@��@�D@�D@z�@Z@j@j@j@I�@�@��@�
@dZ@@�H@��@��@n�@-@��@��@��@�7@hs@hs@X@&�@�`@�9@�u@1'@  @��@�P@\)@\)@K�@K�@;d@
=@�R@v�@@�h@`B@�@�@�@�D@z�@9X@(�@1@�
@�F@�F@��@t�@S�@33@
�H@
^5@
=q@
=q@
=q@
-@
-@
-@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
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
�B-BJ�B�=B��B�wBĜB�dB�5BBBB�BPB%�BH�BffBt�Be`Bp�Bv�Be`BgmBx�B�B� Bw�BdZBp�BbNBO�B9XB;dB#�B�B'�B5?B&�BBbB�BB�TB�yB�BBŢB��B��B�{B|�Bl�BffBYBYBA�B�BB
��B
��B
��B
ĜB
ÖB
��B
ŢB
�-B
�B
�B
��B
��B
� B
iyB
2-B
B
DB
�B
B	�sB	ȴB	��B	��B	�FB	�VB	�B	n�B	�+B	u�B	dZB	ffB	YB	T�B	I�B	F�B	�B	B	bB�sB�`B��B�jBǮB�dB�XB�XB�FB��B�?B�-B�qB�XB�LB�-B��B��B��B�+B�DBq�B�B�VB~�BhsBz�Bs�Bu�Bq�Bs�BjBk�BjBs�Bs�Bp�Bq�BhsBO�BXBn�Bk�BcTB\)Be`BdZB`BBXBp�BjB_;B]/BgmBffBjBl�BgmBhsB[#BR�BZB\)BjBr�Bs�BhsBk�BhsBdZBS�B1'B$�B-BC�BG�B@�B8RBJ�BD�BM�BJ�BM�BN�BK�BB�B@�BC�BL�BL�BG�BZBW
BR�BR�Bk�By�By�B�B� Bs�By�B�%B�=B�DB�VB�oB��B��B�uB�hB��B��B��B��B��B��B��B�B�B��B��B��B��B��B�qBĜBĜBŢBŢB��B�XBƨB��B��BȴBȴBǮBĜBŢB��B��B��B��B��B�;B�NB�NB�B�fB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B�B��B	B	+B	VB	uB	�B	 �B	!�B	&�B	(�B	)�B	+B	2-B	33B	5?B	8RB	7LB	5?B	7LB	8RB	7LB	7LB	=qB	=qB	B�B	A�B	B�B	A�B	B�B	A�B	>wB	K�B	J�B	P�B	O�B	J�B	P�B	P�B	VB	[#B	_;B	`BB	_;B	_;B	_;B	^5B	\)B	\)B	ZB	\)B	n�B	o�B	r�B	u�B	x�B	y�B	z�B	z�B	{�B	|�B	}�B	�B	�B	�B	� B	�%B	�1B	�1B	�7B	�7B	�7B	�7B	�=B	�=B	�1B	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�'B	�9B	�LB	�LB	�RB	�RB	�RB	�jB	�jB	�jB	�wB	�wB	�wB	�qB	��B	B	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�B	�)B	�/B	�5B	�5B	�5B	�/B	�)B	�;B	�HB	�BB	�HB	�NB	�NB	�TB	�NB	�HB	�NB	�NB	�TB	�ZB	�fB	�mB	�mB	�sB	�sB	�fB	�fB	�sB	�sB	�sB	�mB	�B	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B	��B
  B	��B	��B	��B
B
%B
%B
%B
B
B
B
1B
	7B
1B
	7B
1B
DB
JB
JB
JB
DB
PB
PB
VB
\B
VB
VB
bB
\B
bB
hB
hB
hB
hB
hB
bB
bB
hB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
"�B
%�B
%�B
%�B
&�B
'�B
&�B
%�B
%�B
%�B
$�B
#�B
"�B
&�B
(�B
+B
+B
,B
,B
,B
+B
+B
+B
,B
,B
.B
.B
.B
-B
,B
0!B
1'B
0!B
0!B
0!B
/B
/B
.B
/B
0!B
1'B
1'B
1'B
1'B
1'B
0!B
/B
/B
1'B
2-B
2-B
0!B
/B
1'B
2-B
33B
2-B
0!B
0!B
.B
0!B
2-B
1'B
5?B
49B
5?B
5?B
6FB
5?B
5?B
7LB
8RB
8RB
8RB
7LB
6FB
7LB
8RB
8RB
:^B
:^B
:^B
:^B
9XB
8RB
8RB
8RB
9XB
:^B
9XB
:^B
:^B
:^B
:^B
<jB
=qB
<jB
<jB
<jB
;dB
<jB
=qB
?}B
?}B
?}B
?}B
>wB
>wB
>wB
=qB
=qB
?}B
B�B
B�B
B�B
B�B
A�B
B�B
B�B
@�B
C�B
C�B
C�B
D�B
D�B
E�B
D�B
D�B
D�B
C�B
D�B
D�B
C�B
B�B
C�B
D�B
D�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
H�B
I�B
K�B
J�B
I�B
K�B
K�B
J�B
J�B
L�B
N�B
M�B
L�B
K�B
K�B
M�B
N�B
N�B
O�B
P�B
Q�B
Q�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
T�B
S�B
T�B
T�B
VB
VB
W
B
VB
T�B
VB
T�B
YB
YB
YB
YB
YB
YB
ZB
[#B
ZB
YB
XB
ZB
ZB
[#B
]/B
]/B
]/B
\)B
\)B
[#B
YB
ZB
\)B
\)B
]/B
^5B
^5B
^5B
^5B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
^5B
_;B
`BB
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
`BB
`BB
`BB
`BB
`BB
_;B
_;B
_;B
bNB
cTB
cTB
cTB
cTB
dZB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
cTB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
ffB
ffB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
ffB
gmB
hsB
hsB
iyB
iyB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
jB
jB
jB
jB
k�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
l�B
l�B
m�B
n�B
m�B
m�B
m�B
o�B
o�B
o�B
o�B
o�B
q�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
o�B
p�B
p�B
o�B
o�B
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
s�B
s�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
t�B
s�B
t�B
s�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
v�B
v�B
u�B
u�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
��B
��B
�[B
�iB
��B
�B3�BP�B�pB��B�uB�1B��B��BB�B_B�B�B(�BK�Bh�BvzBh�Br�Bx�Bh�Bj�BzB��B��Bx�Bf�BqvBdBRoB<6B=B'8B7B)DB5�B(�B�B B�B�B�8B�QB�BȚB��B��B��B�Bo Bh�B[=BZ7BDB�B�B
��B
�>B
��B
��B
żB
�:B
��B
��B
��B
�B
��B
�?B
�B
l�B
7�B

�B
�B
�B
�B	�kB	̳B	�B	��B	��B	� B	�fB	r-B	��B	w�B	e�B	hsB	[qB	W?B	K�B	H�B	$B	�B	�B�B�B�:B�OB��B�(B��B�0B��B�;B��B��B��B�B�B��B�0B�;B�_B��B�Bu?B��B�\B�;Bk�B|PBu�BwLBshBt�BlqBl�Bl"BtnBt�BqvBrGBi�BR�BY�BoOBlqBd�B]�Bf�Be�Ba�BZ7BqBkkB`�B^�BhsBg�BkQBmwBhXBi_B\�BT�B[=B]IBj�Br�BtTBi�Bl�BiDBeFBU�B5tB(>B/iBC�BH1BA�B9�BK)BF?BN�BK�BN�BO\BL~BC�BA�BEBM�BM�BH�BZ�BW�BS�BTBj�ByXBzxB�-B��BuZBz�B�YB�XB�xB��B��B��B��B�B��B��B��B��B��B��B�B�8B�B�/B�eB��B��B��B�yB�qBĜBĶB��B��B�'B�DB��B��B��B�B�B�1B�mB�YB�B�,B�}B��B�@B�!B�B�B�	B�B�B��B��B��B��B�WB�B�UB�MB�B�`B�B�6B�RB�zB��B��B	�B	�B	�B	�B	�B	 �B	"4B	'B	)B	*0B	+QB	2aB	3�B	5�B	8�B	7�B	5�B	7�B	8�B	7�B	7�B	=�B	=�B	B�B	A�B	B�B	A�B	B�B	A�B	?.B	K�B	J�B	P�B	P.B	K^B	Q B	QB	V9B	[=B	_pB	`vB	_VB	_VB	_pB	^OB	\xB	\�B	Z�B	\�B	n�B	o�B	r�B	u�B	x�B	y�B	z�B	z�B	{�B	}B	~(B	�9B	�{B	�aB	��B	�YB	�KB	�fB	�RB	�RB	�lB	�lB	�XB	�rB	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�5B	��B	�B	�B	�B	��B	�B	�B	�B	�>B	�>B	�6B	�CB	�CB	�qB	�[B	�nB	�fB	�LB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�B	�B	� B	� B	�,B	�FB	�+B	�1B	�B	�=B	�#B	�QB	�]B	�/B	�5B	�OB	�OB	�dB	�xB	�VB	�bB	�\B	�bB	�hB	�hB	�nB	�hB	�bB	�hB	�B	�B	�tB	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�>B	�0B
  B
�B
B	�HB
 4B	�]B	�jB	�cB
B
?B
?B
?B
{B
{B
SB
B
	7B
KB
	RB
KB
^B
JB
dB
dB
^B
jB
PB
pB
\B
pB
pB
}B
vB
}B
�B
�B
�B
hB
hB
�B
}B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#B
%�B
%�B
%�B
'B
'�B
&�B
%�B
%�B
%�B
%B
$&B
#B
'B
)B
+B
+B
,B
,B
,"B
+B
+6B
+6B
,"B
,"B
./B
./B
./B
-B
,WB
0B
1B
0!B
0;B
0;B
/5B
/B
.IB
/B
0;B
1B
1'B
1'B
1'B
1AB
0;B
/OB
/OB
1AB
2-B
2-B
0UB
/iB
1AB
2GB
33B
2GB
0UB
0!B
.cB
0UB
2-B
1AB
5ZB
4TB
5ZB
5tB
6`B
5ZB
5tB
7fB
8RB
8RB
8RB
7LB
6zB
7fB
8RB
8RB
:DB
:^B
:xB
:^B
9rB
8�B
8�B
8�B
9rB
:^B
9rB
:xB
:�B
:�B
:�B
<�B
=qB
<�B
<�B
<�B
;�B
<�B
=qB
?�B
?�B
?�B
?�B
>�B
>�B
>�B
=�B
=�B
?�B
BuB
B�B
B�B
B�B
A�B
B�B
B�B
@�B
C�B
C�B
C�B
D�B
D�B
E�B
D�B
D�B
D�B
C�B
D�B
D�B
C�B
B�B
C�B
D�B
D�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
IB
I�B
K�B
J�B
J	B
K�B
K�B
J�B
KB
L�B
N�B
M�B
MB
K�B
K�B
M�B
N�B
OB
O�B
P�B
RB
RB
Q B
P�B
RB
QB
RB
RB
S�B
T�B
UB
T�B
T�B
T�B
VB
VB
VB
VB
UB
T,B
UB
U2B
VB
VB
W
B
VB
U2B
VB
UMB
Y1B
YB
Y1B
Y1B
Y1B
Y1B
ZB
[	B
ZB
Y1B
XEB
ZB
Z7B
[=B
]/B
]/B
]/B
\)B
\B
[=B
YKB
ZQB
\CB
\CB
]IB
^5B
^5B
^5B
^B
]/B
]/B
^OB
^OB
^5B
^5B
^5B
^OB
^5B
]IB
^OB
_;B
`BB
_VB
_VB
`'B
`\B
`BB
`\B
aHB
aHB
`\B
`\B
`\B
`\B
`BB
_VB
_pB
_pB
b4B
cTB
c:B
cTB
cTB
d@B
c:B
cnB
dZB
dZB
dZB
dZB
dZB
cnB
cnB
cnB
cnB
cTB
cnB
cnB
cnB
b�B
c�B
ezB
f�B
f�B
ffB
ffB
f�B
f�B
ffB
f�B
gmB
f�B
f�B
gRB
g�B
f�B
gmB
gmB
g�B
gmB
f�B
gmB
hsB
h�B
i�B
iyB
h�B
h�B
i�B
i�B
i�B
i�B
jeB
jB
jB
iyB
i�B
i�B
iyB
j�B
j�B
j�B
kkB
k�B
j�B
j�B
jB
jB
k�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
l�B
l�B
m�B
n�B
m�B
m�B
m�B
o�B
o�B
o�B
o�B
o�B
q�B
p�B
p�B
p�B
q�B
q�B
p�B
p�B
o�B
p�B
p�B
o�B
o�B
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
s�B
s�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
t�B
s�B
t�B
s�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
v�B
v�B
u�B
u�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.06(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811110037012018111100370120181111003701201811110200282018111102002820181111020028201811120030142018111200301420181112003014  JA  ARFMdecpA19c                                                                20181107093514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181107003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181107003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181107003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181107003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181107003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181107003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181107003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181107003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181107003529                      G�O�G�O�G�O�                JA  ARUP                                                                        20181107005651                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181107153159  CV  JULD            G�O�G�O�F�u�                JM  ARCAJMQC2.0                                                                 20181110153701  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181110153701  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181110170028  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181111153014  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                