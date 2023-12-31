CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-13T21:35:16Z creation;2018-10-13T21:35:21Z conversion to V3.1;2019-12-19T07:30:28Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20181013213516  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              "A   JA  I2_0576_290                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؈����1   @؈��}( @9jW���'�d8Ov_�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ D�|�D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@(�CB(�CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�=D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh�=Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�>�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�>�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D���D�A�D���D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�A�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�D�~�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D���D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�>�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D�D�K�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�I�A�G�A�G�A�G�A�G�A�I�A�I�A�G�A�
=Aџ�AжFA��HA�ȴA��TA�9XA̸RA�ĜAʬA�ZA�1'A�7LA��`A���AċDA�{A�VA�  A��A��
A��7A�JA���A��;A��
A��#A�z�A�1A���A��;A�"�A�x�A���A�9XA���A�x�A���A���A���A��A���A�(�A��
A��+A�C�A���A��RA�bA��A�JA���A�`BA�$�A���A�M�A���A�ffA��HA�Q�A���A�`BA���A��A�bNA�?}A��A��A��7A�K�A�VA���A��jA�l�A�-A�bA��!A�5?A��A�bNA�A���A�A��DA�&�A�-A
=A~ �A}"�A|��A|  A{;dAzr�Ay�Ay?}AxVAv9XAs33ArȴAq��Ap�HAoO�AnVAm
=AlZAk�7Ak;dAkoAj�/Aj�RAjr�Ai�Ai
=Af��AfVAe�AeƨAex�Adr�AcXAb�`AaƨA`��A`jA`$�A_oA]��A]�PA]"�A[dZAW�hAV-AUXAU/AT�RATn�AS�;AR�yAP�RAOt�AN�\AM��AL�+AK�AJv�AJr�AJn�AJ5?AIG�AHA�AH$�AGO�AFr�AFn�AEl�ADQ�AC��ACABr�AB�AA��AAt�AA\)AA7LAA�A@�A@��A@A�A?�;A?`BA>JA=%A<z�A;��A;O�A:9XA9;dA7��A7K�A6bA5%A3C�A2�\A2JA05?A/t�A/"�A.9XA,v�A*�/A)�
A(��A'��A'7LA&��A%�mA%��A%�7A%hsA$  A"�A"=qA!�
A!`BA jA bNA��A��A?}A~�A�Av�AJA?}A5?AƨAXAbNA�A�A^5AAhsA9XAG�AȴA?}A�DA  Al�AȴAbA7LA�wA�A
�A
��A
��A
��A
1A	�A��A��AAx�AȴAoA��A%A�AQ�A�7@��@��@�j@�?}@�I�@�@�@�V@��@�Z@� �@睲@�J@�bN@�R@�7L@߶F@ާ�@��@݁@��@�  @�-@ٺ^@�p�@�/@�V@ؓu@��
@���@Չ7@ӝ�@�~�@��#@�`B@���@мj@Л�@�r�@�bN@�Q�@� �@���@Ώ\@�-@�ƨ@�~�@�hs@�l�@���@�7L@���@���@�7L@��@��!@�z�@��\@�1@�o@�M�@���@���@��
@�o@��@�G�@��@��`@�(�@���@�J@���@�G�@��@��D@��P@��\@�V@��@��@��h@���@�I�@�1@��@�ƨ@���@�t�@�@��R@�^5@�J@��^@��7@�X@�/@��@�%@���@���@�bN@���@�t�@���@��@��h@�?}@���@� �@�ƨ@�l�@��y@��@���@� �@�o@�@��7@�V@��D@��P@�ȴ@��\@�V@�@��^@�p�@�?}@���@���@��j@���@�9X@��F@�|�@�ȴ@��7@�/@��D@�1'@��@�  @��m@��F@���@���@�l�@�33@���@�X@��@�Ĝ@�j@���@���@�+@�V@�J@�@��@��#@���@���@��7@�`B@�O�@�&�@��9@�z�@�A�@��@�1@��m@��;@��
@���@�dZ@�C�@�;d@�33@�33@�33@�+@�;d@�t�@�|�@��P@���@���@��P@��H@���@��+@�~�@�V@�E�@���@�&�@���@��u@�Z@�A�@�  @�@|�@;d@
=@~�@~�R@~�+@}/@|�@|1@y��@x��@x��@xbN@xA�@xĜ@y��@y%@w�@w|�@w\)@v��@v$�@v@v5?@v�+@vv�@vff@v5?@u@uV@tz�@t1@s��@sS�@sC�@s33@so@r��@rM�@q�#@q�#@q�^@q�7@q7L@qX@q��@q��@rM�@q��@q��@qG�@p��@p�u@pbN@p �@o��@o�w@ol�@n��@n��@o�@o;d@o�@n�y@nff@m��@m��@mp�@l�D@lI�@k��@k33@ko@j�@i&�@i&�@i7L@i&�@h�`@hQ�@hb@g�@g��@g�@g�P@g|�@gK�@f@e�h@e/@d�@d��@d�j@d�j@d�j@d�@d(�@cƨ@c��@cS�@b��@ahs@`r�@` �@_��@^�y@^v�@^5?@]�T@]O�@\�/@\9X@[��@[�m@[��@[dZ@[C�@Z�@Z�!@Zn�@Z=q@Z-@Z�@ZJ@ZJ@ZJ@Y�@Y�#@Y��@Y�^@Y��@Y&�@X��@X�@XQ�@Xb@W�;@WK�@Vȴ@VE�@U�@U�@UV@T��@Tz�@S�m@S��@S�@SdZ@SC�@S33@S33@S"�@R�@QG�@PĜ@Pr�@PA�@Pb@O�;@O�w@O�P@Ol�@OK�@N�y@N��@M�-@L��@L�j@Lj@L9X@Kƨ@K�F@K��@KS�@J�@J��@J~�@J-@I��@H��@HA�@G�;@Gl�@G\)@G
=@G
=@G;d@GK�@F��@F��@FE�@E�-@E��@E��@E/@DZ@CdZ@B�!@B-@A��@A��@Ahs@@�`@@�u@@ �@?�;@?�;@?|�@?\)@?�@>��@>5?@=�h@<�/@<�@;�m@;ƨ@;�F@;�F@;��@;�@;o@:-@9��@9��@9X@9&�@9&�@9&�@9%@8��@9%@8�`@8Ĝ@8�9@8�9@8��@8��@8�9@8�9@8�9@8�9@8�u@8r�@8bN@81'@7�@8  @7�@7�;@7�w@7�@7��@7|�@7|�@7K�@6�@5�@5�h@5�h@4��@4�/@4��@49X@4�@3�
@3ƨ@3��@3"�@2��@2-@1�7@1x�@1x�@1hs@1X@17L@1�@0��@0��@0��@0��@0��@0r�@0Q�@0A�@/�w@/l�@/l�@/\)@/;d@/+@/
=@.ȴ@.v�@.$�@.{@-�T@-�h@-�@-/@,�@,(�@+�F@+"�@*�@*��@*�\@*^5@)��@)��@)�@(b@'�@&��@&��@&�@&�+@&{@%O�@$��@$�/@$z�@#ƨ@#ƨ@#�F@#t�@#"�@#o@"�H@"�!@"n�@"=q@"J@!��@!&�@ ��@ �9@ �9@ �9@ �u@ �@ r�@ bN@ Q�@ 1'@  �@�;@;d@�@��@��@�y@�y@�y@�@ff@$�@@@�@�@�-@?}@V@��@��@�@�D@j@Z@1@ƨ@��@C�@"�@"�@o@��@��@�!@�\@^5@=q@-@�@�@�^@��@X@��@��@�u@bN@1'@b@�@��@�P@l�@\)@\)@K�@K�@K�@�@��@ff@V@V@V@V@{@�@�T@��@`B@�@�@�/@��@�j@j@�m@S�@"�@�@^5@M�@M�@=q@�@J@��@�7@�7@X@&�@��@��@�`@�`@�`@�`@�`@Ĝ@��@�@�@��@l�@
=@�y@�@��@v�@V@5?@�@��@@�-@�-@�-@�-@�-@��@`B@/@V@�@�D@�@�@1@��@�
@C�@33@
�H@
^5@	��@	��@	hs@	X@	G�@	&�@	�@	%@�`@��@Ĝ@�9@�9@�u@bN@�@�w@|�@+@��@�y@�@�R@v�@E�@$�@{@{@{@�@�-@�h@�@p�@`B@`B@O�@/@V@��@z�@I�@9X@(�@1@�m@��@�@t�@dZ@C�@��@�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�I�A�G�A�G�A�G�A�G�A�I�A�I�A�G�A�
=Aџ�AжFA��HA�ȴA��TA�9XA̸RA�ĜAʬA�ZA�1'A�7LA��`A���AċDA�{A�VA�  A��A��
A��7A�JA���A��;A��
A��#A�z�A�1A���A��;A�"�A�x�A���A�9XA���A�x�A���A���A���A��A���A�(�A��
A��+A�C�A���A��RA�bA��A�JA���A�`BA�$�A���A�M�A���A�ffA��HA�Q�A���A�`BA���A��A�bNA�?}A��A��A��7A�K�A�VA���A��jA�l�A�-A�bA��!A�5?A��A�bNA�A���A�A��DA�&�A�-A
=A~ �A}"�A|��A|  A{;dAzr�Ay�Ay?}AxVAv9XAs33ArȴAq��Ap�HAoO�AnVAm
=AlZAk�7Ak;dAkoAj�/Aj�RAjr�Ai�Ai
=Af��AfVAe�AeƨAex�Adr�AcXAb�`AaƨA`��A`jA`$�A_oA]��A]�PA]"�A[dZAW�hAV-AUXAU/AT�RATn�AS�;AR�yAP�RAOt�AN�\AM��AL�+AK�AJv�AJr�AJn�AJ5?AIG�AHA�AH$�AGO�AFr�AFn�AEl�ADQ�AC��ACABr�AB�AA��AAt�AA\)AA7LAA�A@�A@��A@A�A?�;A?`BA>JA=%A<z�A;��A;O�A:9XA9;dA7��A7K�A6bA5%A3C�A2�\A2JA05?A/t�A/"�A.9XA,v�A*�/A)�
A(��A'��A'7LA&��A%�mA%��A%�7A%hsA$  A"�A"=qA!�
A!`BA jA bNA��A��A?}A~�A�Av�AJA?}A5?AƨAXAbNA�A�A^5AAhsA9XAG�AȴA?}A�DA  Al�AȴAbA7LA�wA�A
�A
��A
��A
��A
1A	�A��A��AAx�AȴAoA��A%A�AQ�A�7@��@��@�j@�?}@�I�@�@�@�V@��@�Z@� �@睲@�J@�bN@�R@�7L@߶F@ާ�@��@݁@��@�  @�-@ٺ^@�p�@�/@�V@ؓu@��
@���@Չ7@ӝ�@�~�@��#@�`B@���@мj@Л�@�r�@�bN@�Q�@� �@���@Ώ\@�-@�ƨ@�~�@�hs@�l�@���@�7L@���@���@�7L@��@��!@�z�@��\@�1@�o@�M�@���@���@��
@�o@��@�G�@��@��`@�(�@���@�J@���@�G�@��@��D@��P@��\@�V@��@��@��h@���@�I�@�1@��@�ƨ@���@�t�@�@��R@�^5@�J@��^@��7@�X@�/@��@�%@���@���@�bN@���@�t�@���@��@��h@�?}@���@� �@�ƨ@�l�@��y@��@���@� �@�o@�@��7@�V@��D@��P@�ȴ@��\@�V@�@��^@�p�@�?}@���@���@��j@���@�9X@��F@�|�@�ȴ@��7@�/@��D@�1'@��@�  @��m@��F@���@���@�l�@�33@���@�X@��@�Ĝ@�j@���@���@�+@�V@�J@�@��@��#@���@���@��7@�`B@�O�@�&�@��9@�z�@�A�@��@�1@��m@��;@��
@���@�dZ@�C�@�;d@�33@�33@�33@�+@�;d@�t�@�|�@��P@���@���@��P@��H@���@��+@�~�@�V@�E�@���@�&�@���@��u@�Z@�A�@�  @�@|�@;d@
=@~�@~�R@~�+@}/@|�@|1@y��@x��@x��@xbN@xA�@xĜ@y��@y%@w�@w|�@w\)@v��@v$�@v@v5?@v�+@vv�@vff@v5?@u@uV@tz�@t1@s��@sS�@sC�@s33@so@r��@rM�@q�#@q�#@q�^@q�7@q7L@qX@q��@q��@rM�@q��@q��@qG�@p��@p�u@pbN@p �@o��@o�w@ol�@n��@n��@o�@o;d@o�@n�y@nff@m��@m��@mp�@l�D@lI�@k��@k33@ko@j�@i&�@i&�@i7L@i&�@h�`@hQ�@hb@g�@g��@g�@g�P@g|�@gK�@f@e�h@e/@d�@d��@d�j@d�j@d�j@d�@d(�@cƨ@c��@cS�@b��@ahs@`r�@` �@_��@^�y@^v�@^5?@]�T@]O�@\�/@\9X@[��@[�m@[��@[dZ@[C�@Z�@Z�!@Zn�@Z=q@Z-@Z�@ZJ@ZJ@ZJ@Y�@Y�#@Y��@Y�^@Y��@Y&�@X��@X�@XQ�@Xb@W�;@WK�@Vȴ@VE�@U�@U�@UV@T��@Tz�@S�m@S��@S�@SdZ@SC�@S33@S33@S"�@R�@QG�@PĜ@Pr�@PA�@Pb@O�;@O�w@O�P@Ol�@OK�@N�y@N��@M�-@L��@L�j@Lj@L9X@Kƨ@K�F@K��@KS�@J�@J��@J~�@J-@I��@H��@HA�@G�;@Gl�@G\)@G
=@G
=@G;d@GK�@F��@F��@FE�@E�-@E��@E��@E/@DZ@CdZ@B�!@B-@A��@A��@Ahs@@�`@@�u@@ �@?�;@?�;@?|�@?\)@?�@>��@>5?@=�h@<�/@<�@;�m@;ƨ@;�F@;�F@;��@;�@;o@:-@9��@9��@9X@9&�@9&�@9&�@9%@8��@9%@8�`@8Ĝ@8�9@8�9@8��@8��@8�9@8�9@8�9@8�9@8�u@8r�@8bN@81'@7�@8  @7�@7�;@7�w@7�@7��@7|�@7|�@7K�@6�@5�@5�h@5�h@4��@4�/@4��@49X@4�@3�
@3ƨ@3��@3"�@2��@2-@1�7@1x�@1x�@1hs@1X@17L@1�@0��@0��@0��@0��@0��@0r�@0Q�@0A�@/�w@/l�@/l�@/\)@/;d@/+@/
=@.ȴ@.v�@.$�@.{@-�T@-�h@-�@-/@,�@,(�@+�F@+"�@*�@*��@*�\@*^5@)��@)��@)�@(b@'�@&��@&��@&�@&�+@&{@%O�@$��@$�/@$z�@#ƨ@#ƨ@#�F@#t�@#"�@#o@"�H@"�!@"n�@"=q@"J@!��@!&�@ ��@ �9@ �9@ �9@ �u@ �@ r�@ bN@ Q�@ 1'@  �@�;@;d@�@��@��@�y@�y@�y@�@ff@$�@@@�@�@�-@?}@V@��@��@�@�D@j@Z@1@ƨ@��@C�@"�@"�@o@��@��@�!@�\@^5@=q@-@�@�@�^@��@X@��@��@�u@bN@1'@b@�@��@�P@l�@\)@\)@K�@K�@K�@�@��@ff@V@V@V@V@{@�@�T@��@`B@�@�@�/@��@�j@j@�m@S�@"�@�@^5@M�@M�@=q@�@J@��@�7@�7@X@&�@��@��@�`@�`@�`@�`@�`@Ĝ@��@�@�@��@l�@
=@�y@�@��@v�@V@5?@�@��@@�-@�-@�-@�-@�-@��@`B@/@V@�@�D@�@�@1@��@�
@C�@33@
�H@
^5@	��@	��@	hs@	X@	G�@	&�@	�@	%@�`@��@Ĝ@�9@�9@�u@bN@�@�w@|�@+@��@�y@�@�R@v�@E�@$�@{@{@{@�@�-@�h@�@p�@`B@`B@O�@/@V@��@z�@I�@9X@(�@1@�m@��@�@t�@dZ@C�@��@�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B~�By�B~�B�+B��B�jB�NB��BPBPB�B-B0!B�B1B0!B2-B1'B;dBR�BdZBT�B,B(�B8RBo�Bt�B`BBH�BVB]/BP�BT�BN�BA�B,B)�B�B �B�B
=B�B�B�}B�!BB�!B�9B�XB�B��B��B��B��B�oB�1Bx�BI�BZBR�BG�B=qB49B�BB�B�BoBDB
��B
�mB
�#B
�HB
�;B
�B
�B
��B
ȴB
�dB
��B
��B
��B
��B
�uB
�VB
�DB
�1B
�B
{�B
t�B
t�B
o�B
jB
cTB
`BB
W
B
K�B
7LB
 �B
1'B
%�B
!�B
�B
\B
JB
JB

=B
DB
DB
	7B
+B
B	��B	�B	�/B	�yB	�sB	�mB	�HB	�B	��B	��B	ƨB	B	��B	��B	�FB	��B	�3B	��B	�PB	q�B	�B	�B	�=B	�B	� B	w�B	k�B	VB	[#B	XB	R�B	K�B	E�B	G�B	L�B	K�B	F�B	<jB	49B	9XB	49B	+B	1'B	(�B	"�B	"�B	 �B	�B	 �B	 �B	"�B	"�B	!�B	 �B	�B	�B	�B	{B	VB	B��B	B��B��B�B�B�NB�HB�
B��BȴBȴBƨB�3B�?B�3B��B��B�bB�{B�bB�hB��B��B��B��B��B��B�DB�7B�bB�bB�\B�+B�bB�DB�+B�Bx�Bn�Bq�Bs�Bl�Be`BjBiyB_;Be`B^5B]/BZB[#BN�BI�BL�BB�BG�BJ�BG�BD�B@�B=qB8RBC�BI�BJ�BH�BF�B?}B?}B:^B<jB8RB<jB8RB(�B,B6FB8RB49B'�BbB  B��B�B&�B(�B(�B&�B#�B%�B&�B#�B�B�B�B�B�B �B$�B&�B%�B �B�B(�B)�B+B)�B&�B"�B�B�B�B$�B(�B,B-B/B0!B0!B0!B.B,B&�B�B%B�B$�B'�B!�B0!B)�B,B.B-B/B,B(�B+B,BB�BD�BG�BG�BL�BO�BO�BS�BXBW
BS�BP�B_;BbNBcTBdZBdZBaHBcTBm�Bm�Bm�Bl�BiyBq�Bs�Bt�Bt�Bt�Bt�Bs�Bu�Bu�Bw�Bx�Bz�B{�B|�B}�B}�B}�B{�Bz�Bz�B|�B{�B}�B�B�B�B�1B�DB�JB�DB�=B�JB�{B��B��B��B��B��B��B�B�RB�XB�XB�jB�wB��BBĜBƨBŢBÖBĜBȴBǮBǮB�B�
B�5B�HB�NB�NB�NB�ZB�ZB�TB�TB�TB�TB�B��B��B��B��B��B	  B	
=B	\B	\B	\B	hB	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	%�B	,B	0!B	1'B	1'B	1'B	33B	5?B	8RB	9XB	9XB	9XB	9XB	8RB	8RB	@�B	C�B	D�B	F�B	G�B	E�B	D�B	G�B	K�B	O�B	S�B	S�B	YB	[#B	]/B	^5B	`BB	aHB	bNB	aHB	gmB	iyB	cTB	l�B	r�B	w�B	z�B	�B	�B	�B	�%B	�1B	�PB	�JB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�FB	�LB	�XB	�jB	�jB	�jB	�wB	�wB	�wB	�}B	��B	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�5B	�;B	�5B	�5B	�/B	�;B	�HB	�HB	�NB	�NB	�NB	�NB	�;B	�TB	�`B	�fB	�sB	�sB	�yB	�sB	�sB	�fB	�sB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
+B
+B
	7B
JB
JB
JB
PB
JB
DB
1B
	7B
PB
\B
bB
bB
hB
oB
oB
oB
oB
hB
oB
bB
hB
uB
uB
uB
uB
{B
{B
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
�B
�B
�B
 �B
!�B
#�B
$�B
#�B
"�B
$�B
%�B
&�B
(�B
&�B
'�B
&�B
%�B
%�B
#�B
&�B
'�B
,B
-B
-B
-B
-B
,B
+B
+B
/B
0!B
1'B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
33B
33B
49B
33B
49B
5?B
5?B
5?B
49B
49B
5?B
49B
49B
33B
2-B
2-B
5?B
7LB
6FB
7LB
7LB
7LB
8RB
8RB
9XB
8RB
7LB
7LB
7LB
8RB
<jB
<jB
<jB
<jB
;dB
<jB
<jB
<jB
=qB
<jB
<jB
<jB
<jB
<jB
;dB
<jB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
@�B
?}B
?}B
@�B
@�B
?}B
@�B
A�B
A�B
C�B
C�B
C�B
B�B
B�B
B�B
@�B
>wB
@�B
C�B
D�B
D�B
D�B
C�B
B�B
E�B
F�B
F�B
D�B
G�B
H�B
G�B
G�B
H�B
H�B
I�B
H�B
I�B
H�B
H�B
H�B
J�B
J�B
K�B
L�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
M�B
O�B
O�B
O�B
O�B
N�B
N�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
T�B
T�B
T�B
T�B
VB
VB
T�B
VB
VB
W
B
VB
VB
VB
W
B
VB
VB
XB
YB
XB
XB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
ZB
ZB
YB
[#B
\)B
\)B
\)B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
\)B
\)B
\)B
_;B
_;B
^5B
aHB
bNB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
cTB
cTB
cTB
bNB
bNB
`BB
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
hsB
gmB
hsB
jB
jB
jB
iyB
gmB
jB
iyB
iyB
jB
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
n�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
q�B
r�B
r�B
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
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�B�B�B�B�'B.Bz�B�4B�RB��B��B�B�HB�B\B�B-�B0�BxB�B2aB5%B4�B>�BVBg�BZ�B5�B2-B?cBr�Bw�Bd�BM�BYB_pBS[BV�BP�BC�B/�B,�B�B!�B�B~B�B�qB�MB��B��B��B�%B��B��B��B��B��B�B�&B�lBz�BN�B[	BT,BIB>�B5�B/BtB�B�B�B�B iB
�B
�B
�B
��B
��B
֡B
�{B
��B
��B
�kB
�fB
��B
��B
��B
�vB
�0B
�B
��B
}B
u�B
utB
pUB
kkB
dZB
aB
X+B
MPB
9�B
#�B
1�B
'RB
"�B
EB
�B
�B
B
)B
�B
xB
	�B
zB
�B	��B	�B	ߤB	��B	��B	��B	��B	�?B	�"B	бB	��B	ÖB	�'B	�'B	��B	��B	��B	�
B	�B	u�B	�{B	�B	��B	��B	��B	x�B	l�B	X�B	\�B	Y1B	TFB	M6B	G+B	HfB	L�B	K�B	GEB	=�B	5tB	9�B	5%B	+�B	1�B	*KB	$&B	#�B	!�B	 \B	!HB	!bB	# B	#B	!�B	 �B	!B	)B	1B	2B	BB	�B�.B	�B��B��B�'B��B��B�hBخB�[BʦBɺBǮB�?B�FB��B�yB��B�TB��B��B��B�1B�sB�YB�B��B�+B�B��B�NB�B�B�fB�}B��B��B��BzBpUBr�BtnBm�Bf�Bk6Bj0B`�BfB_VB^BZ�B[�BPbBJ�BM�BD�BH�BK�BH�BE�BA�B>�B:BD3BI�BJ�BIBGB@OB@ B;dB=B9�B="B9�B+QB-�B7B8�B5B)�B,B�B B+B'�B)�B)yB'mB$�B&fB'B$ZB�B�B�B�B�B!bB%`B'RB&LB!�B�B)DB*KB+QB*0B'mB#�B �B�BB%�B)yB,WB-]B/OB0UB0UB0;B./B,WB'�BB�BB%�B(�B# B0�B+B-B/ B.B0B-CB*�B,�B-�BB�BE9BHKBHfBMjBP}BP�BTaBXEBWYBT�BQ�B_�Bb�Bc�Bd�Bd�BbBdBm�Bm�Bm�Bl�Bj0Bq�Bs�Bt�Bt�Bt�Bt�BtBu�BvBxBy$B{B|B}B~B~B~B|6B{JB{JB}VB|�B~]B�mB�mB��B��B�xB��B��B��B�B�B�9B�CB�@B�RB�yB��B��B�lB��B��B��B��B��B��BĶB��B��B��B�B�B�KBȀB�SB�sB�OB�bB�NB�hB�B�tB�tB�B�B��B�&B��B�B�2B�8B�(B�BB	 �B	
XB	BB	vB	vB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	&B	,"B	0;B	1'B	1'B	1AB	3B	5%B	88B	9XB	9>B	9>B	9rB	8�B	8�B	@�B	C�B	D�B	F�B	G�B	FB	EB	G�B	K�B	O�B	TB	TB	YB	[#B	]/B	^5B	`BB	abB	bhB	a�B	g�B	i�B	d&B	l�B	r�B	w�B	z�B	��B	��B	�mB	�tB	�fB	�PB	��B	��B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	��B	�B	�FB	�2B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�"B	��B	�&B	�:B	�B	�,B	�9B	�$B	�9B	�eB	�B	�!B	�5B	�OB	�dB	�VB	�bB	�bB	�hB	�hB	�hB	�B	ߊB	�nB	�zB	�B	�sB	�B	�_B	�sB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�(B	�.B
 B
-B
MB
9B
EB
EB
_B
	RB
0B
dB
dB
PB
dB
^B
�B
	lB
�B
vB
}B
}B
�B
oB
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
�B
B
B
 �B
!�B
#�B
$�B
#�B
"�B
$�B
&B
&�B
(�B
'B
(
B
&�B
&B
&B
$&B
'B
(>B
,"B
-)B
,�B
-B
-B
,"B
+B
+6B
/5B
0!B
1'B
2GB
3B
33B
3MB
33B
33B
3MB
3MB
3B
4B
49B
4B
49B
49B
49B
4B
33B
33B
4B
3MB
4TB
5?B
5?B
5%B
49B
49B
5?B
49B
4B
33B
2aB
2|B
5ZB
7LB
6zB
7fB
7fB
7fB
8lB
8lB
9XB
8lB
7fB
7fB
7�B
8�B
<jB
<jB
<jB
<PB
;B
<�B
<jB
<�B
=VB
<jB
<�B
<�B
<�B
<�B
;�B
<jB
>]B
>wB
>wB
>�B
>�B
>wB
>�B
>wB
@iB
?�B
?�B
@�B
@�B
?�B
@�B
A�B
A�B
C�B
C�B
C�B
B�B
B�B
B�B
@�B
>�B
@�B
C�B
D�B
D�B
D�B
C�B
B�B
E�B
F�B
F�B
D�B
G�B
H�B
G�B
G�B
H�B
H�B
I�B
H�B
I�B
H�B
H�B
H�B
J�B
J�B
K�B
L�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
J�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
NB
M�B
O�B
O�B
O�B
O�B
N�B
N�B
Q B
Q�B
RB
RB
Q�B
Q�B
RB
Q�B
RB
SB
SB
T�B
T�B
T�B
UB
U�B
VB
T�B
VB
VB
W
B
VB
VB
VB
W$B
VB
V9B
XB
YB
X+B
X+B
YB
Y1B
Y1B
ZB
Z7B
[#B
[	B
[#B
[#B
Z7B
Z7B
YKB
[#B
\)B
\B
\)B
\)B
[=B
\CB
\CB
\)B
\CB
\CB
]/B
^5B
^5B
^5B
\CB
\]B
\CB
_VB
_VB
^OB
aHB
b4B
aHB
abB
aHB
abB
abB
bNB
bhB
bhB
bhB
cTB
cTB
dZB
cTB
cTB
cTB
bhB
bNB
`�B
cnB
cnB
cTB
cTB
dZB
dtB
dZB
ezB
e`B
e`B
ezB
ffB
gmB
gmB
gmB
gmB
gmB
gRB
gmB
g�B
g�B
h�B
h�B
g�B
hsB
jB
jB
jeB
iyB
g�B
jB
i�B
i�B
j�B
k�B
l�B
l�B
m�B
m�B
m�B
mwB
m�B
mwB
m�B
n}B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
p�B
q�B
q�B
r�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
q�B
r�B
r�B
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
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.06(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810180037062018101800370620181018003706201810180200182018101802001820181018020018201810190033502018101900335020181019003350  JA  ARFMdecpA19c                                                                20181014063514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181013213516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181013213519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181013213519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181013213520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181013213520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181013213520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181013213520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181013213520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181013213521                      G�O�G�O�G�O�                JA  ARUP                                                                        20181013215516                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181014153543  CV  JULD            G�O�G�O�F�E�                JM  ARCAJMQC2.0                                                                 20181017153706  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181017153706  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181017170018  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181018153350  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                