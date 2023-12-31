CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-02-01T18:42:02Z creation;2023-02-01T18:42:03Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230201184202  20230201185840  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               yA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @���OC�1   @��ƻZ�@;WKƧ��d �j~��1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�  A�  A�  A���A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C�C�C  C  C  C  C  C�C   C!�fC$  C&  C(�C*�C,  C-�fC0  C2  C3�fC6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  Dy�D��Dy�D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D8��D9� D:  D:� D;  D;� D<  D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DW��DX� DY  DY� DZ  DZ�fD[  D[�fD\fD\�fD]fD]�fD^  D^� D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dqy�Dq��Dry�Dr��Ds� Dt  Dt� Du  Du� Du��Dvy�Dw  Dw� Dx  Dx� Dy  Dy� DzfDz� D{  D{� D|  D|y�D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��3D�� D���D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D D��3D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�3D�@ Dƀ D�� D�3D�C3Dǀ D�� D�  D�@ D�|�D�� D�  D�@ Dɀ D�� D���D�@ Dʀ D�� D�  D�@ Dˀ D�� D�3D�@ D�|�D�� D�  D�@ D̀ D�� D�  D�@ D΃3D�� D�  D�@ Dπ Dϼ�D�  D�@ DЀ D�� D�  D�C3Dр D�� D�  D�<�DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�C3Dփ3D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D���D�@ D�3D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��
@�
=@�
=A�A?�A_�A�A�A�A�A��\AЏ\A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC޸C�RC	�RC�RC�RC�C�C�RC�RC�RC�RC�RC�C�RC!޸C#�RC%�RC(�C*�C+�RC-޸C/�RC1�RC3޸C5�RC7޸C9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCT�CU�RCW�RCY�RC[�RC]�RC_�RCa�RCc޸Ce�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC|�C}�RC�RC��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��\C��)C��)C��)C��)C��\C��)C��)C��)C��)C��C��C��)C��C��)C��)C��C��)C��\C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��\C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��\C��)C��)C��)C��C��)C��\C��\C��\C��)C��)C��)C��)C��)C��)D ~D �D~D�D�zD�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�Dw�D��Dw�D�D~D�D~D��D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#�zD#�D$~D$�D%~D%�D&~D&�D'~D'��D(~D(�D)~D)�D*~D*�D+~D+�D,w�D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8��D9~D9�D:~D:�D;~D;�D<~D<�D=~D>zD>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DJzDJ~DJ�DK~DK�DL�zDL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DRw�DR�DS~DS�DT~DT�DU~DU�DV~DV�DWw�DW��DX~DX�DY~DY�DZ�zDZ�D[�zD\zD\�zD]zD]�zD]�D^~D^��D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh��Di~Di�Dj~Dj�Dk�zDk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp��Dqw�Dq��Drw�Dr��Ds~Ds�Dt~Dt�Du~Du��Dvw�Dv�Dw~Dw�Dx~Dx�Dy~DzzDz~Dz�D{~D{�D|w�D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�B=D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�{�D���D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�{�D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�B=D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D��=D��
D��
D�?
D�
D��
D�=D�?
D�
D��
D��
D�B=D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D���D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D��=D��
D��
D�?
D��=D��=D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D���D�?
D��=D��
D���D�?
D�{�D���D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�B=D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D��=D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D�=D�?
D�
D��
D��
D�?
D�
D��
D�=D�B=D��=D��
D��
D�?
D�
D���D��
D�?
D�
D��
D��
D�?
D�{�D��
D��
D�?
D�
D��
D��
D�?
D�
D��=D��
D�?
D�
Dÿ
D��
D�?
D�
DĿ
D��
D�?
D�
Dſ
D�=D�?
D�
Dƿ
D�=D�B=D�
Dǿ
D��
D�?
D�{�Dȿ
D��
D�?
D�
Dɿ
D���D�?
D�
Dʿ
D��
D�?
D�
D˿
D�=D�?
D�{�D̿
D��
D�?
D�
DͿ
D��
D�?
D΂=Dο
D��
D�?
D�
Dϻ�D��
D�?
D�
Dп
D��
D�B=D�
Dѿ
D��
D�;�D�
Dҿ
D��
D�?
D�
Dӿ
D��
D�?
D�
DԿ
D��
D�?
D�
Dտ
D��
D�B=Dւ=Dֿ
D��
D�?
D�
D׿
D��
D�?
D�
Dؿ
D��
D�?
D�
Dٿ
D��
D�?
D�
Dڿ
D��
D�?
D�
Dۿ
D��
D�?
D�
Dܿ
D��
D�?
D�
Dݿ
D��
D�?
D�
D޿
D��
D�?
D�
D߿
D��
D�?
D�
D�
D��
D�?
D�
D��D���D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�=D��=D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�;�D�
D�
D��
D�?
D�
D��D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��
D��
D�B=D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��=D��
D�?
D�
D�
D���D�?
D�=D��
D��
D�?
D�
D��
D�=D�?
D�
D��
D��
D�?
D�
D��
D��
D�;�D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��1A���A���A���A���A��A��7A���A��A���A��(A��xA��A���A��MA���A���A���A��_A���A���A��bA��-A���A���A��hA��:A���A��nA���A���A���A��:A���A���A��$A���A���A���A���A��{A�(�A�
rA��A��A��fA���A�+�A���A���A��oA���A��ZA�)�A��AA�VmA�%�A�HA�/�A���A�Y�A���A�a�A���A��A��A��{A�s�A��0A��FA���A��'A�xA��A�P�A��A��fA�U�A���A��PA��~A���A�OvA��A���A��A�=�A��HA���A���A���A��A���A�$A��A֡AW�Ay�>AwV�AvƨAv8�Ats�AsVArS�Aq�pAqG�Ap�;ApSAo\�Am�Al��AkK^Aje�Ai��AiAh&�Ae6zA_[WA^m]A\ںAY�HAW�AT�AS�.AS�$ASdZASG�AS�AR�}AROvAR�AQ˒AQ4nAP��AO��AO^�AO�AN{JAM��AMo�AL�AK�BAK�AJ� AH�sAF��AE��AE�7AD҉AD=qAB��ABU�AAbA>}�A=A<��A;�NA;E9A;�A:�A:��A:��A9�A9$A8��A81�A8YA7��A7�A5a|A3��A3B[A2�A1]dA0\)A0�A/�6A/XyA/$tA.�zA,�A,�A+�A+W�A*�FA)��A)JA(kQA&c A$��A#n/A#@OA#6A#�A"�A"��A"8�A!�~A ��A��Aw�A}�A�A/�A��A�A�RA�A��A:*A\�A�A7LA�`A�Ae�A6A�xA~�A�6AK^A��A�}AԕA�]AA	lA
֡A
G�A	��A	�4A	hsA�A��AjA�wA�|A��A�A �@�[W@�U2@�s@�j�@��m@�{�@�M�@�/�@���@�&�@���@��z@�E�@��y@��@�}�@�.I@�6@�U�@��@�%F@�@�c @���@��@��p@��m@�ƨ@�t�@��@�֡@ڃ@�~�@���@՗�@�e@ӌ~@��f@�	�@�`B@�V@��'@Л�@Ј�@�j@�L0@��@Ϡ'@���@��D@�V@˛=@�S@�M�@ȟ�@ƂA@Ô�@�u�@�P�@���@��z@��I@��Q@�/@�@��z@��+@���@���@���@�~�@���@���@�/�@�(@���@��_@�($@���@�!�@���@�C-@��P@���@���@�e�@�� @��=@�@O@���@��@���@���@�~�@�*0@��@���@�5?@��@���@��@��C@���@�A�@���@�_@�L�@���@��q@�a@�/@��@�Ta@�3�@�ԕ@�9�@���@��@�>B@��S@��@�z�@��@�F�@��L@��o@�Xy@��@��*@�hs@�6z@���@��@�Ov@���@�2a@��`@���@�{�@�/�@��]@���@�<�@�e,@��@���@���@�v`@�`B@�(�@���@�z�@�G@�`B@��F@��@��"@���@�-w@��@��@�YK@�@���@��n@�P�@��@���@�Q�@��@���@��Y@���@���@�s�@�2�@���@���@���@�S�@�+@���@�l"@��#@��K@��3@���@��+@�1@���@��@��@��k@�~�@�y�@�g�@�6z@��@�S�@�6@��@��@�Vm@�\�@�j�@�J�@�ff@���@�:�@��`@���@�ی@���@��@�B[@�%�@���@��K@��V@�j@�/�@���@��x@�p;@��@~}V@}�@|��@|��@|'R@z҉@y��@x�j@x�@xu�@xm�@x2�@w�
@w�@@w�	@wqv@wMj@wJ#@w4�@w�@v�@v�@uԕ@u+@t��@t��@t��@t��@ttT@te�@t_@tM@tD�@tx@s��@s{J@s_p@sH�@sK�@sJ#@s>�@s.I@r=q@r�@q��@qS&@p��@p�@pH@pb@o>�@nZ�@n@m��@m�#@k�@ks@kH�@k>�@k.I@kS@jn�@j5?@i��@h�@g�
@gj�@gH�@g$t@g"�@g@g
=@f��@f��@fL0@e�D@e�@e�@e�T@e��@e�@d��@c�+@c@b��@b^5@b-@b �@a�o@a�j@a�H@aN<@`��@_��@_��@_Z�@_Z�@_X�@_O@_E9@_C�@_A�@_;d@_/�@_+@_�@_�@^�c@^��@^��@^Q@]��@]��@]�M@]^�@]+�@\�p@\�@\?�@\�@Zں@Y�@Y|@YY�@YL�@X��@X��@X��@X�z@X1@Wx@W�@V}V@U�#@UX@TĜ@TN�@S�0@S@Q�9@Q�@Qu�@QL�@Q�@P~(@P�o@P_@Ob�@N��@N�L@Mϫ@MVm@M%@L��@L�@L��@L�.@L�D@L�D@Lw�@Lh�@LQ�@K�m@Ks@K9�@J��@J��@JkQ@JL0@J:*@JO@Iϫ@I�H@I��@I��@IVm@H��@HI�@G�Q@Gs@G�@F��@F�F@F~�@F+k@F
�@E�j@E�@Ex�@Ef�@Ec�@E<6@E�@D]d@Cl�@Bl�@B($@A�^@@u�@@_@@[�@@N�@@S�@@bN@@m�@@l"@@��@@w�@@/�@@�@@�@?��@@b@@�@@~@@4n@@K^@?ݘ@?��@?b�@?"�@?"�@>��@>��@>�r@>L0@>8�@>($@>4@=��@=�@=�@=N<@= \@<��@<��@<Z@<C-@<!@<�@;��@;n/@;�@:��@:z@:6�@:�@9�D@9�>@9�9@9�@9�z@9�3@9@9@9��@9j@9�@8��@8��@8l"@8>B@7��@7'�@7P�@6z@5�-@5s�@4��@4r�@4�@3��@3;d@2��@2�@1��@1�T@1�@1k�@1q@0H@/�W@/�Q@/��@/RT@/A�@/"�@/S@.�B@.��@.{�@.}V@.{�@.z@.��@.i�@.L0@.O@._@.@-��@-�d@-�n@-p�@-p�@-?}@,�Y@+خ@+iD@+C�@+�@+Y@+�@*�<@*s�@)��@)f�@)=�@(ѷ@(�@(��@(��@(1'@'��@'��@&�@&a|@&C�@&=q@&:*@&-@&�@%��@%��@%Y�@$��@#��@#Z�@#E9@#&@"��@!�@ tT@ Ft@ (�@ �@ �@��@�@ƨ@��@��@Z�@+@��@ff@0U@@IR@�O@��@l�@S�@��@��@�!@��@d�@;�@�@
�@�d@ \@��@��@u�@`�@1'@	�@��@�@��@�s@�@� @v�@W�@�9@�S@��@L�@B�@-w@�@�u@_@/�@��@��@�@�[@�k@U�@�8@҉@�}@�r@��@n�@_�@M�@M�@H�@($@u@@�`@6@~@�@�@@M@@@�@G@�@خ@��@��@y�@$t@ߤ@�!@��@p;@_�@L0@@�@GE@J�@C�@5?@;�@#:@��@�#@��@��@�@�H@�^@��@�@��@��@�n@X@�f@�v@�[@Ĝ@�Y@'R@�]@��@�g@�0@��@�:@��@l�@a@]�@o@
��@
�L@
V@
e@	�o@	��@	�@	��@	�S@	��@	u�@	(�@	+@	V@	;@�f@�5@�|@�K@�v@�/@�`@�@�|@�|@��@ѷ@�j@��@r�@V�@H@Ft@9X@�@�r@�+@��@�m@�@��@�*@��@+@�@�M@�@��@_�@�@��@�#@��@�=@}�@Q�@-w@@�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��1A���A���A���A���A��A��7A���A��A���A��(A��xA��A���A��MA���A���A���A��_A���A���A��bA��-A���A���A��hA��:A���A��nA���A���A���A��:A���A���A��$A���A���A���A���A��{A�(�A�
rA��A��A��fA���A�+�A���A���A��oA���A��ZA�)�A��AA�VmA�%�A�HA�/�A���A�Y�A���A�a�A���A��A��A��{A�s�A��0A��FA���A��'A�xA��A�P�A��A��fA�U�A���A��PA��~A���A�OvA��A���A��A�=�A��HA���A���A���A��A���A�$A��A֡AW�Ay�>AwV�AvƨAv8�Ats�AsVArS�Aq�pAqG�Ap�;ApSAo\�Am�Al��AkK^Aje�Ai��AiAh&�Ae6zA_[WA^m]A\ںAY�HAW�AT�AS�.AS�$ASdZASG�AS�AR�}AROvAR�AQ˒AQ4nAP��AO��AO^�AO�AN{JAM��AMo�AL�AK�BAK�AJ� AH�sAF��AE��AE�7AD҉AD=qAB��ABU�AAbA>}�A=A<��A;�NA;E9A;�A:�A:��A:��A9�A9$A8��A81�A8YA7��A7�A5a|A3��A3B[A2�A1]dA0\)A0�A/�6A/XyA/$tA.�zA,�A,�A+�A+W�A*�FA)��A)JA(kQA&c A$��A#n/A#@OA#6A#�A"�A"��A"8�A!�~A ��A��Aw�A}�A�A/�A��A�A�RA�A��A:*A\�A�A7LA�`A�Ae�A6A�xA~�A�6AK^A��A�}AԕA�]AA	lA
֡A
G�A	��A	�4A	hsA�A��AjA�wA�|A��A�A �@�[W@�U2@�s@�j�@��m@�{�@�M�@�/�@���@�&�@���@��z@�E�@��y@��@�}�@�.I@�6@�U�@��@�%F@�@�c @���@��@��p@��m@�ƨ@�t�@��@�֡@ڃ@�~�@���@՗�@�e@ӌ~@��f@�	�@�`B@�V@��'@Л�@Ј�@�j@�L0@��@Ϡ'@���@��D@�V@˛=@�S@�M�@ȟ�@ƂA@Ô�@�u�@�P�@���@��z@��I@��Q@�/@�@��z@��+@���@���@���@�~�@���@���@�/�@�(@���@��_@�($@���@�!�@���@�C-@��P@���@���@�e�@�� @��=@�@O@���@��@���@���@�~�@�*0@��@���@�5?@��@���@��@��C@���@�A�@���@�_@�L�@���@��q@�a@�/@��@�Ta@�3�@�ԕ@�9�@���@��@�>B@��S@��@�z�@��@�F�@��L@��o@�Xy@��@��*@�hs@�6z@���@��@�Ov@���@�2a@��`@���@�{�@�/�@��]@���@�<�@�e,@��@���@���@�v`@�`B@�(�@���@�z�@�G@�`B@��F@��@��"@���@�-w@��@��@�YK@�@���@��n@�P�@��@���@�Q�@��@���@��Y@���@���@�s�@�2�@���@���@���@�S�@�+@���@�l"@��#@��K@��3@���@��+@�1@���@��@��@��k@�~�@�y�@�g�@�6z@��@�S�@�6@��@��@�Vm@�\�@�j�@�J�@�ff@���@�:�@��`@���@�ی@���@��@�B[@�%�@���@��K@��V@�j@�/�@���@��x@�p;@��@~}V@}�@|��@|��@|'R@z҉@y��@x�j@x�@xu�@xm�@x2�@w�
@w�@@w�	@wqv@wMj@wJ#@w4�@w�@v�@v�@uԕ@u+@t��@t��@t��@t��@ttT@te�@t_@tM@tD�@tx@s��@s{J@s_p@sH�@sK�@sJ#@s>�@s.I@r=q@r�@q��@qS&@p��@p�@pH@pb@o>�@nZ�@n@m��@m�#@k�@ks@kH�@k>�@k.I@kS@jn�@j5?@i��@h�@g�
@gj�@gH�@g$t@g"�@g@g
=@f��@f��@fL0@e�D@e�@e�@e�T@e��@e�@d��@c�+@c@b��@b^5@b-@b �@a�o@a�j@a�H@aN<@`��@_��@_��@_Z�@_Z�@_X�@_O@_E9@_C�@_A�@_;d@_/�@_+@_�@_�@^�c@^��@^��@^Q@]��@]��@]�M@]^�@]+�@\�p@\�@\?�@\�@Zں@Y�@Y|@YY�@YL�@X��@X��@X��@X�z@X1@Wx@W�@V}V@U�#@UX@TĜ@TN�@S�0@S@Q�9@Q�@Qu�@QL�@Q�@P~(@P�o@P_@Ob�@N��@N�L@Mϫ@MVm@M%@L��@L�@L��@L�.@L�D@L�D@Lw�@Lh�@LQ�@K�m@Ks@K9�@J��@J��@JkQ@JL0@J:*@JO@Iϫ@I�H@I��@I��@IVm@H��@HI�@G�Q@Gs@G�@F��@F�F@F~�@F+k@F
�@E�j@E�@Ex�@Ef�@Ec�@E<6@E�@D]d@Cl�@Bl�@B($@A�^@@u�@@_@@[�@@N�@@S�@@bN@@m�@@l"@@��@@w�@@/�@@�@@�@?��@@b@@�@@~@@4n@@K^@?ݘ@?��@?b�@?"�@?"�@>��@>��@>�r@>L0@>8�@>($@>4@=��@=�@=�@=N<@= \@<��@<��@<Z@<C-@<!@<�@;��@;n/@;�@:��@:z@:6�@:�@9�D@9�>@9�9@9�@9�z@9�3@9@9@9��@9j@9�@8��@8��@8l"@8>B@7��@7'�@7P�@6z@5�-@5s�@4��@4r�@4�@3��@3;d@2��@2�@1��@1�T@1�@1k�@1q@0H@/�W@/�Q@/��@/RT@/A�@/"�@/S@.�B@.��@.{�@.}V@.{�@.z@.��@.i�@.L0@.O@._@.@-��@-�d@-�n@-p�@-p�@-?}@,�Y@+خ@+iD@+C�@+�@+Y@+�@*�<@*s�@)��@)f�@)=�@(ѷ@(�@(��@(��@(1'@'��@'��@&�@&a|@&C�@&=q@&:*@&-@&�@%��@%��@%Y�@$��@#��@#Z�@#E9@#&@"��@!�@ tT@ Ft@ (�@ �@ �@��@�@ƨ@��@��@Z�@+@��@ff@0U@@IR@�O@��@l�@S�@��@��@�!@��@d�@;�@�@
�@�d@ \@��@��@u�@`�@1'@	�@��@�@��@�s@�@� @v�@W�@�9@�S@��@L�@B�@-w@�@�u@_@/�@��@��@�@�[@�k@U�@�8@҉@�}@�r@��@n�@_�@M�@M�@H�@($@u@@�`@6@~@�@�@@M@@@�@G@�@خ@��@��@y�@$t@ߤ@�!@��@p;@_�@L0@@�@GE@J�@C�@5?@;�@#:@��@�#@��@��@�@�H@�^@��@�@��@��@�n@X@�f@�v@�[@Ĝ@�Y@'R@�]@��@�g@�0@��@�:@��@l�@a@]�@o@
��@
�L@
V@
e@	�o@	��@	�@	��@	�S@	��@	u�@	(�@	+@	V@	;@�f@�5@�|@�K@�v@�/@�`@�@�|@�|@��@ѷ@�j@��@r�@V�@H@Ft@9X@�@�r@�+@��@�m@�@��@�*@��@+@�@�M@�@��@_�@�@��@�#@��@�=@}�@Q�@-w@@�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B4B4B3�B4TB49B4nB4�B4nB4�B4�B4�B4�B5ZB4nB4�B5?B5?B5ZB5?B5�B5ZB5�B5�B5�B5�B5�B5�B5�B5�B5�B5�B5�B6B6B6`B6`B5�B5�B5�B5?B4TB3�B3MB*0B&�B%B$�B"B�B.BuB�bB�'B�CB�B�[B`�BA�B1�BWB	lBuB��B�B� BȀB�B��B��B��Bw�BO�BGEB@�B7�B*KB4B��B�B��BÖB�MB��B�
B��B��B}VBp�Be�BN�B;B5B"�B B	B�B
�cB
�"B
��B
�B
��B
�B
�DB
��B
��B
��B
��B
��B
��B
�DB
��B
��B
��B
��B
�JB
�1B
�B
~B
s�B
V�B
I�B
E�B
6�B
+kB
�B
?B
B
:B
B
.B
"B

�B
	B
KB
�B
'B	��B	��B	��B	��B	��B	�GB	�B	�=B	�_B	�B	��B	��B	ΥB	�CB	��B	�)B	ԕB	ЗB	˒B	�zB	��B	�QB	�B	��B	�~B	�B	��B	��B	��B	� B	��B	��B	��B	��B	��B	|B	t�B	q�B	n/B	kB	c�B	b�B	aB	_�B	]�B	]B	VSB	S�B	RB	N�B	L�B	H�B	E�B	C�B	=�B	8B	3�B	2B	1[B	1'B	0B	/ B	./B	+B	(sB	%,B	"NB	�B	CB	QB	eB	�B	�B	gB	�B	NB	vB	B	
�B		�B		7B	�B	�B	SB	�B��B��B�"B��B��B��B�B��B�B��B�B��B�CB�B�B�mB�B�B�BB��BߊB��B�5B��BݘBܬB�]B�)BۦB�WB��B�B��B�2B�9B�B��B՛B�MB�9B��B��B�2B�7B��B�KB�	BڠBڠB�	BںB�kB�7B�)B��BݘB�!B޸BߤB�\B��B��B�B�B�B�B��B��B�B�hB�TB��B�2B�zB�B�sB��B�!B��B�B��B�TB��B��B��B�LB�RB��B�^B�^B�xB�*B��B�(B��B��B��B	 iB	 B	;B	B	�B		�B	DB	~B	�B	6B	�B	B	vB	}B	hB	SB	jB	B	 BB	!�B	!�B	#nB	$�B	%FB	%zB	%�B	&B	&�B	(�B	)B	-�B	0B	3B	2�B	3hB	5�B	6zB	6�B	8�B	:�B	DMB	HKB	JXB	L�B	OBB	PHB	Q�B	UgB	X�B	X�B	Y�B	[#B	]/B	^OB	_!B	`BB	`\B	c�B	f�B	i�B	j�B	l=B	mB	n�B	o�B	qvB	y�B	}�B	�4B	��B	��B	��B	�B	�XB	�dB	�pB	��B	�gB	�QB	�VB	�bB	��B	�&B	�FB	��B	�*B	�kB	��B	�oB	��B	�zB	�PB	��B	��B	�gB	�EB	�KB	��B	�lB	�xB	�PB	�(B	�.B	�TB	��B	�B	�)B	ߊB	�\B	��B	�TB	�8B	�yB	�"B	�cB	��B	��B	��B	�B	�|B	�B	�RB	�^B	�B	��B	��B
oB
 �B
 B
 �B
B
0B
\B
B
hB
�B
�B
�B
{B
B
B
$B
yB
eB
kB
B
�B
~B
!�B
%FB
&�B
'�B
(�B
)�B
,�B
/�B
2aB
3MB
3�B
4B
5?B
6zB
72B
7�B
8B
8�B
8�B
8�B
9>B
:xB
?}B
@4B
C�B
E9B
E�B
FB
F%B
F�B
F�B
F�B
G+B
G+B
G�B
IB
I�B
JrB
K)B
K)B
K^B
K�B
K�B
N"B
OvB
Q�B
TFB
TB
UB
V�B
X_B
ZB
[=B
\xB
\�B
]/B
b�B
c:B
cTB
cnB
c�B
d&B
fB
ffB
g8B
kkB
m�B
o5B
o�B
p!B
p;B
pUB
p�B
p�B
q�B
r�B
tB
t9B
t9B
tTB
u�B
v�B
w�B
z�B
~B
}B
�B
��B
�;B
�oB
��B
�B
�-B
�3B
��B
��B
��B
��B
��B
��B
�	B
�#B
�#B
�#B
�rB
��B
��B
��B
�DB
��B
�B
��B
�"B
��B
�BB
��B
�.B
�4B
��B
�:B
��B
��B
��B
��B
�QB
��B
��B
��B
�B
��B
�;B
��B
��B
�nB
�,B
�LB
�
B
�B
�B
�_B
�qB
��B
�B
�wB
�}B
�oB
��B
��B
�zB
��B
�8B
�^B
�JB
��B
�6B
�jB
��B
��B
��B
��B
�B
�B
�"B
��B
��B
��B
��B
��B
�4B
�OB
�iB
��B
�UB
�UB
�UB
�UB
��B
ªB
�{B
��B
��B
ňB
�?B
�YB
��B
ǔB
ǮB
�B
ȀB
�B
�B
�B
�B
�RB
��B
��B
��B
�dB
��B
�vB
�B
�(B
�\B
�\B
�BB
�vB
ϑB
�vB
ϫB
��B
ѷB
��B
� B
��B
��B
��B
��B
�B
�B
�&B
�uB
��B
��B
��B
��B
�gB
��B
�B
�mB
֡B
ּB
ּB
�?B
�+B
�_B
�yB
ٚB
�B
�7B
��B
�	B
ܬB
�B
�OB
�B
��B
�|B
��B
�4B
�4B
�B
�B
�B
�B
�B
�B
��B
�B
��B
�B
�B
�,B
��B
�2B
�B
�B
�
B
��B
�*B
��B
�B
�6B
�kB
�=B
��B
��B
��B
�B
��B
�IB
�5B
�B
�'B
�'B
�[B
�B
�-B
�|B
��B
�MB
��B
��B
��B
��B
��B
��B
��B
�TB
�B
��B
��B
��B
�%B
�tB
��B
��B
�B
��B
��B
�$B
�$B
�>B
�XB
�rB
�B
�^B
�B
��B
�B
��B
�B
�"B
�<B
��B
��B
�B
��B
��B
�.B
�.B
�.B
�cB
�}B
��B
��B OBB�B�B3BgBmB�BKB�B�B�B�B�B�B	B	B	B	�B	�B
�B
�B
�B
�BBB�B�BB�BB�B.B}B�B�B�B4B:B�B�B�B&B[B�B�B�BBgBgB�B�B�B�B�B�B$B$B$B?B�BEByB�B�BBBeB�BQB�B�B�B�B#B=B=B=BWBqB�B�BIBB5BOBOBOBOBjBOBjBjB�B�B�BB!B�B B BB 'B \B vB �B �B �B �B �B �B �B �B!HB!|B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B"4B"�B"�B"�B"�B#TB#�B$B$&B$tB$�B$�B$�B$�B$�B%B$�B%�B%�B%�B&�B&�B&�B&�B'B'B'B'8B'RB'�B'�B'�B'�B'�B(
B(
B($B(XB(XB(sB(>B(XB(XB(sB(�B(�B)B)B)DB)DB)DB)_B)�B)�B)�B)�B)�B)�B)�B)�B)�B*eB*B*�B+6B+�B,B,�B,�B,�B-)B-CB-wB-�B-�B-�B-�B-�B-�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B4B4B3�B4TB49B4nB4�B4nB4�B4�B4�B4�B5ZB4nB4�B5?B5?B5ZB5?B5�B5ZB5�B5�B5�B5�B5�B5�B5�B5�B5�B5�B5�B6B6B6`B6`B5�B5�B5�B5?B4TB3�B3MB*0B&�B%B$�B"B�B.BuB�bB�'B�CB�B�[B`�BA�B1�BWB	lBuB��B�B� BȀB�B��B��B��Bw�BO�BGEB@�B7�B*KB4B��B�B��BÖB�MB��B�
B��B��B}VBp�Be�BN�B;B5B"�B B	B�B
�cB
�"B
��B
�B
��B
�B
�DB
��B
��B
��B
��B
��B
��B
�DB
��B
��B
��B
��B
�JB
�1B
�B
~B
s�B
V�B
I�B
E�B
6�B
+kB
�B
?B
B
:B
B
.B
"B

�B
	B
KB
�B
'B	��B	��B	��B	��B	��B	�GB	�B	�=B	�_B	�B	��B	��B	ΥB	�CB	��B	�)B	ԕB	ЗB	˒B	�zB	��B	�QB	�B	��B	�~B	�B	��B	��B	��B	� B	��B	��B	��B	��B	��B	|B	t�B	q�B	n/B	kB	c�B	b�B	aB	_�B	]�B	]B	VSB	S�B	RB	N�B	L�B	H�B	E�B	C�B	=�B	8B	3�B	2B	1[B	1'B	0B	/ B	./B	+B	(sB	%,B	"NB	�B	CB	QB	eB	�B	�B	gB	�B	NB	vB	B	
�B		�B		7B	�B	�B	SB	�B��B��B�"B��B��B��B�B��B�B��B�B��B�CB�B�B�mB�B�B�BB��BߊB��B�5B��BݘBܬB�]B�)BۦB�WB��B�B��B�2B�9B�B��B՛B�MB�9B��B��B�2B�7B��B�KB�	BڠBڠB�	BںB�kB�7B�)B��BݘB�!B޸BߤB�\B��B��B�B�B�B�B��B��B�B�hB�TB��B�2B�zB�B�sB��B�!B��B�B��B�TB��B��B��B�LB�RB��B�^B�^B�xB�*B��B�(B��B��B��B	 iB	 B	;B	B	�B		�B	DB	~B	�B	6B	�B	B	vB	}B	hB	SB	jB	B	 BB	!�B	!�B	#nB	$�B	%FB	%zB	%�B	&B	&�B	(�B	)B	-�B	0B	3B	2�B	3hB	5�B	6zB	6�B	8�B	:�B	DMB	HKB	JXB	L�B	OBB	PHB	Q�B	UgB	X�B	X�B	Y�B	[#B	]/B	^OB	_!B	`BB	`\B	c�B	f�B	i�B	j�B	l=B	mB	n�B	o�B	qvB	y�B	}�B	�4B	��B	��B	��B	�B	�XB	�dB	�pB	��B	�gB	�QB	�VB	�bB	��B	�&B	�FB	��B	�*B	�kB	��B	�oB	��B	�zB	�PB	��B	��B	�gB	�EB	�KB	��B	�lB	�xB	�PB	�(B	�.B	�TB	��B	�B	�)B	ߊB	�\B	��B	�TB	�8B	�yB	�"B	�cB	��B	��B	��B	�B	�|B	�B	�RB	�^B	�B	��B	��B
oB
 �B
 B
 �B
B
0B
\B
B
hB
�B
�B
�B
{B
B
B
$B
yB
eB
kB
B
�B
~B
!�B
%FB
&�B
'�B
(�B
)�B
,�B
/�B
2aB
3MB
3�B
4B
5?B
6zB
72B
7�B
8B
8�B
8�B
8�B
9>B
:xB
?}B
@4B
C�B
E9B
E�B
FB
F%B
F�B
F�B
F�B
G+B
G+B
G�B
IB
I�B
JrB
K)B
K)B
K^B
K�B
K�B
N"B
OvB
Q�B
TFB
TB
UB
V�B
X_B
ZB
[=B
\xB
\�B
]/B
b�B
c:B
cTB
cnB
c�B
d&B
fB
ffB
g8B
kkB
m�B
o5B
o�B
p!B
p;B
pUB
p�B
p�B
q�B
r�B
tB
t9B
t9B
tTB
u�B
v�B
w�B
z�B
~B
}B
�B
��B
�;B
�oB
��B
�B
�-B
�3B
��B
��B
��B
��B
��B
��B
�	B
�#B
�#B
�#B
�rB
��B
��B
��B
�DB
��B
�B
��B
�"B
��B
�BB
��B
�.B
�4B
��B
�:B
��B
��B
��B
��B
�QB
��B
��B
��B
�B
��B
�;B
��B
��B
�nB
�,B
�LB
�
B
�B
�B
�_B
�qB
��B
�B
�wB
�}B
�oB
��B
��B
�zB
��B
�8B
�^B
�JB
��B
�6B
�jB
��B
��B
��B
��B
�B
�B
�"B
��B
��B
��B
��B
��B
�4B
�OB
�iB
��B
�UB
�UB
�UB
�UB
��B
ªB
�{B
��B
��B
ňB
�?B
�YB
��B
ǔB
ǮB
�B
ȀB
�B
�B
�B
�B
�RB
��B
��B
��B
�dB
��B
�vB
�B
�(B
�\B
�\B
�BB
�vB
ϑB
�vB
ϫB
��B
ѷB
��B
� B
��B
��B
��B
��B
�B
�B
�&B
�uB
��B
��B
��B
��B
�gB
��B
�B
�mB
֡B
ּB
ּB
�?B
�+B
�_B
�yB
ٚB
�B
�7B
��B
�	B
ܬB
�B
�OB
�B
��B
�|B
��B
�4B
�4B
�B
�B
�B
�B
�B
�B
��B
�B
��B
�B
�B
�,B
��B
�2B
�B
�B
�
B
��B
�*B
��B
�B
�6B
�kB
�=B
��B
��B
��B
�B
��B
�IB
�5B
�B
�'B
�'B
�[B
�B
�-B
�|B
��B
�MB
��B
��B
��B
��B
��B
��B
��B
�TB
�B
��B
��B
��B
�%B
�tB
��B
��B
�B
��B
��B
�$B
�$B
�>B
�XB
�rB
�B
�^B
�B
��B
�B
��B
�B
�"B
�<B
��B
��B
�B
��B
��B
�.B
�.B
�.B
�cB
�}B
��B
��B OBB�B�B3BgBmB�BKB�B�B�B�B�B�B	B	B	B	�B	�B
�B
�B
�B
�BBB�B�BB�BB�B.B}B�B�B�B4B:B�B�B�B&B[B�B�B�BBgBgB�B�B�B�B�B�B$B$B$B?B�BEByB�B�BBBeB�BQB�B�B�B�B#B=B=B=BWBqB�B�BIBB5BOBOBOBOBjBOBjBjB�B�B�BB!B�B B BB 'B \B vB �B �B �B �B �B �B �B �B!HB!|B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B"4B"�B"�B"�B"�B#TB#�B$B$&B$tB$�B$�B$�B$�B$�B%B$�B%�B%�B%�B&�B&�B&�B&�B'B'B'B'8B'RB'�B'�B'�B'�B'�B(
B(
B($B(XB(XB(sB(>B(XB(XB(sB(�B(�B)B)B)DB)DB)DB)_B)�B)�B)�B)�B)�B)�B)�B)�B)�B*eB*B*�B+6B+�B,B,�B,�B,�B-)B-CB-wB-�B-�B-�B-�B-�B-�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230201184201  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230201184202  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230201184203  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230201184203                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230201184203  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230201184203  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230201185840                      G�O�G�O�G�O�                