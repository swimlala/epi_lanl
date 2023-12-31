CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       $Woods Hole Oceanographic Institution   source        
Argo float     history       92021-02-17T03:39:42Z creation; 2022-05-04T12:55:30Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         iPRIMARY | https://orcid.org/0000-0001-5113-1068 | Deborah West-Mack, Woods Hole Oceanographic Institution         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7d   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7t   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7x   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7|   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  84   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8d   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8h   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8l   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8p   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9    POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9$   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9,   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :,   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :0   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :4   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :8   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :<   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  a�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ϼ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ߨ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20210217033942  20220504085530  1901663 US ARGO PROJECT                                                 BRECK OWENS, STEVE JAYNE, P.E. ROBBINS                          PRES            TEMP            PSAL               �A   AO  5137                            2C  D   S2A                             7179                            SBE602 V1.3                     854 @��X-�s1   @��I����C�|0��@DSy��1   GPS     Primary sampling: averaged [nominal 2 dbar binned data sampled at 0.5 Hz from a SBE41CP]                                                                                                                                                                           A   A   A   ?�  ?��@B�\@�G�@�G�@�  @�  AG�A��A   A?\)A_\)A���A���A���A��A��A�Q�A߮A�\)A��B�
B  B  B   B(Q�B0Q�B8  B@  BH(�BPQ�BX  B`  Bh  Bo�Bw�
B�  B��B�  B�{B�  B�  B�  B�  B�  B�  B��B�  B�  B�  B�{B�  B�  B�  B��B��B�  B�  B��B�  B�  B�  B��B�  B�  B�  B�  B��C   C  C  C
=C
=C
  C��C��C  C��C��C��C  C  C  C  C   C"  C$
=C&  C'��C)��C,  C.  C0  C2
=C4
=C6
=C8
=C:
=C<
=C>  C@  CB  CD  CF
=CH  CJ  CL  CN  CP  CQ��CT  CV  CX
=CZ
=C\  C^  C`  Cb  Cd  Ce��Ch  Cj  Cl  Cm��Co��Cq��Ct  Cu��Cx  Cz  C|
=C~  C�  C�  C�  C�  C�  C���C�  C�C�C�  C���C���C�  C�  C���C�  C�C�C���C���C�  C�C�C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�  C�C�  C�  C�  C�  C�C�C�  C�  C�C�C�  C���C�  C�  C�  C�  C���C���C�  C�C�  C���C���C���C���C�C�  C���C���C�  C�C�C�C�  C���C�  C�  C���C�  C�  C�C�  C���C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C���C�  C�C�  C���C���C���C���C���C���C�  C�C�C�C�  C���C���C���C�  C�  C�  C�  C�  C�  C�C�C�C�  C���C�  C�C�  C�  C�  C�  C���C���C�  D �D � D ��D� D�D��D  D��D  D}qD  D��D  D� D�D��D�D��D	�D	� D	�qD
� D  D� D  D��D�D� D�qD� D�D� D�D��D�D��D  D}qD  D� D�qD� D  D� D  D� D�D� D�qD� D  D� D�D��D  D� D�qD� D�D� D  D� D  D��D   D }qD!  D!� D!�qD"� D#�D#}qD#�qD$� D%�D%� D&�D&� D'  D'� D(  D(��D)D)��D*�D*��D+�D+��D,�D,}qD,�qD-� D.  D.� D/  D/� D0  D0}qD1  D1��D2  D2}qD2�qD3}qD4  D4� D4�qD5� D6  D6}qD7  D7� D8�D8�D9  D9}qD:  D:� D;�D;��D<�D<� D=�D=�D>  D>}qD?  D?� D@�D@��DA�DA� DA�qDB� DC  DC� DD  DD��DE�DE� DF  DFz�DG  DG� DH  DH}qDI�DI��DI�qDJ}qDK�DK� DK�qDL� DM�DM� DN  DN� DO�DO��DP  DP� DQ  DQ� DR  DR� DS�DS��DT�DT� DU  DU}qDU�qDV��DW�DW� DX  DX��DY  DY��DZ  DZ� D[  D[}qD[�qD\}qD]  D]}qD]�qD^}qD^�qD_� D`  D`� Da  Da}qDb  Db��Dc  Dc� Dd�Dd� De  De� Df  Df� Df�qDg}qDh  Dh� Dh�qDi}qDi�qDj� Dk�Dk� Dk�qDl}qDl�qDm}qDm�qDn� Do  Do� Do�qDp� Dq  Dq� Dq�qDr� Ds�Ds� Dt  Dt� Du  Du}qDu�qDv��Dw  Dw}qDw�qDx� Dy�Dy��Dz�Dz� Dz�qD{� D|�D|��D}  D}� D}�qD~}qD~�qD� D�HD�@ D�~�D��HD�  D�>�D��HD��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D�HD�B�D��HD���D�  D�AHD�� D��HD�HD�@ D�� D�� D�  D�AHD�� D�� D���D�@ D�� D���D���D�>�D�� D�� D���D�>�D�~�D���D�  D�@ D��HD�� D���D�>�D�~�D���D�  D�@ D�� D�� D���D�@ D��HD��HD�  D�>�D�~�D�� D�  D�AHD�� D�� D�  D�>�D�� D�� D���D�>�D�� D��HD�  D�@ D��HD�� D���D�>�D�~�D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�@ D��HD��HD�  D�AHD�� D���D�HD�AHD�� D�� D�  D�AHD�� D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�AHD�� D���D���D�@ D�� D���D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�AHD�� D���D�  D�@ D�~�D�� D�  D�@ D��HD�� D�  D�@ D�~�D�� D�  D�>�D�� D�� D���D�>�D�~�D���D���D�>�D�� D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD�� D���D���D�>�D��HD�� D���D�>�D�� D�� D�  D�@ D�� D���D���D�@ D�~�D�� D�  D�@ D��HD�� D�  D�@ D��HD��HD�HD�AHD�� D�� D���D�@ D��HD�� D�  D�@ D�~�D�� D���D�=qD�~�D�� D���D�@ D��HD�� D���D�@ D�� D�� D�HD�@ D�� D��HD�HD�AHD�� D���D��qD�>�D�� D��HD�HD�AHD�~�D���D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�HD�@ D D��HD�  D�@ D�~�D�� D�  D�@ DĀ D�� D���D�>�D�~�D��HD�  D�>�D�~�D�� D�HD�@ Dǀ DǾ�D�  D�AHDȀ D�� D�  D�>�Dɀ Dɾ�D���D�@ Dʀ D��HD�  D�@ Dˀ D�� D���D�@ D́HD�� D�  D�>�D̀ D��HD�  D�@ D΁HD��HD�HD�@ Dπ D��HD�HD�AHDЀ Dо�D�  D�@ D�~�DѾ�D���D�AHDҀ D�� D�  D�@ D�~�D�� D�  D�>�DԀ D�� D�HD�>�D�}qDվ�D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�>�D؀ D�� D���D�>�D�~�D�� D�  D�AHDڀ Dھ�D�  D�@ D�~�D�� D�HD�AHD܁HD�� D�  D�>�D݀ D��HD�  D�@ Dހ D�� D�  D�@ D߀ D�� D���D�@ D��HD��HD�  D�>�D� D�� D���D�>�D�~�D�� D�  D�@ D�HD�� D�  D�@ D� D�� D�  D�>�D�~�D�� D���D�@ D悏D�� D���D�>�D� D�� D�  D�@ D�~�D辸D�  D�@ D� D�� D�  D�@ D�~�D꾸D���D�@ D� D�� D�  D�>�D�~�D쾸D�  D�AHD� D��HD�  D�@ D� D�� D�  D�@ D�~�D�� D�  D�@ D�� D��HD�HD�@ D� D��HD�  D�>�D� D�� D�  D�AHD�HD��HD�HD�AHD� D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�AHD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�  ?��@B�\@�G�@�G�@�  @�  AG�A��A   A?\)A_\)A���A���A���A��A��A�Q�A߮A�\)A��B�
B  B  B   B(Q�B0Q�B8  B@  BH(�BPQ�BX  B`  Bh  Bo�Bw�
B�  B��B�  B�{B�  B�  B�  B�  B�  B�  B��B�  B�  B�  B�{B�  B�  B�  B��B��B�  B�  B��B�  B�  B�  B��B�  B�  B�  B�  B��C   C  C  C
=C
=C
  C��C��C  C��C��C��C  C  C  C  C   C"  C$
=C&  C'��C)��C,  C.  C0  C2
=C4
=C6
=C8
=C:
=C<
=C>  C@  CB  CD  CF
=CH  CJ  CL  CN  CP  CQ��CT  CV  CX
=CZ
=C\  C^  C`  Cb  Cd  Ce��Ch  Cj  Cl  Cm��Co��Cq��Ct  Cu��Cx  Cz  C|
=C~  C�  C�  C�  C�  C�  C���C�  C�C�C�  C���C���C�  C�  C���C�  C�C�C���C���C�  C�C�C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�  C�C�  C�  C�  C�  C�C�C�  C�  C�C�C�  C���C�  C�  C�  C�  C���C���C�  C�C�  C���C���C���C���C�C�  C���C���C�  C�C�C�C�  C���C�  C�  C���C�  C�  C�C�  C���C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C���C�  C�C�  C���C���C���C���C���C���C�  C�C�C�C�  C���C���C���C�  C�  C�  C�  C�  C�  C�C�C�C�  C���C�  C�C�  C�  C�  C�  C���C���C�  D �D � D ��D� D�D��D  D��D  D}qD  D��D  D� D�D��D�D��D	�D	� D	�qD
� D  D� D  D��D�D� D�qD� D�D� D�D��D�D��D  D}qD  D� D�qD� D  D� D  D� D�D� D�qD� D  D� D�D��D  D� D�qD� D�D� D  D� D  D��D   D }qD!  D!� D!�qD"� D#�D#}qD#�qD$� D%�D%� D&�D&� D'  D'� D(  D(��D)D)��D*�D*��D+�D+��D,�D,}qD,�qD-� D.  D.� D/  D/� D0  D0}qD1  D1��D2  D2}qD2�qD3}qD4  D4� D4�qD5� D6  D6}qD7  D7� D8�D8�D9  D9}qD:  D:� D;�D;��D<�D<� D=�D=�D>  D>}qD?  D?� D@�D@��DA�DA� DA�qDB� DC  DC� DD  DD��DE�DE� DF  DFz�DG  DG� DH  DH}qDI�DI��DI�qDJ}qDK�DK� DK�qDL� DM�DM� DN  DN� DO�DO��DP  DP� DQ  DQ� DR  DR� DS�DS��DT�DT� DU  DU}qDU�qDV��DW�DW� DX  DX��DY  DY��DZ  DZ� D[  D[}qD[�qD\}qD]  D]}qD]�qD^}qD^�qD_� D`  D`� Da  Da}qDb  Db��Dc  Dc� Dd�Dd� De  De� Df  Df� Df�qDg}qDh  Dh� Dh�qDi}qDi�qDj� Dk�Dk� Dk�qDl}qDl�qDm}qDm�qDn� Do  Do� Do�qDp� Dq  Dq� Dq�qDr� Ds�Ds� Dt  Dt� Du  Du}qDu�qDv��Dw  Dw}qDw�qDx� Dy�Dy��Dz�Dz� Dz�qD{� D|�D|��D}  D}� D}�qD~}qD~�qD� D�HD�@ D�~�D��HD�  D�>�D��HD��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D�HD�B�D��HD���D�  D�AHD�� D��HD�HD�@ D�� D�� D�  D�AHD�� D�� D���D�@ D�� D���D���D�>�D�� D�� D���D�>�D�~�D���D�  D�@ D��HD�� D���D�>�D�~�D���D�  D�@ D�� D�� D���D�@ D��HD��HD�  D�>�D�~�D�� D�  D�AHD�� D�� D�  D�>�D�� D�� D���D�>�D�� D��HD�  D�@ D��HD�� D���D�>�D�~�D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�@ D��HD��HD�  D�AHD�� D���D�HD�AHD�� D�� D�  D�AHD�� D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�AHD�� D���D���D�@ D�� D���D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�AHD�� D���D�  D�@ D�~�D�� D�  D�@ D��HD�� D�  D�@ D�~�D�� D�  D�>�D�� D�� D���D�>�D�~�D���D���D�>�D�� D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD�� D���D���D�>�D��HD�� D���D�>�D�� D�� D�  D�@ D�� D���D���D�@ D�~�D�� D�  D�@ D��HD�� D�  D�@ D��HD��HD�HD�AHD�� D�� D���D�@ D��HD�� D�  D�@ D�~�D�� D���D�=qD�~�D�� D���D�@ D��HD�� D���D�@ D�� D�� D�HD�@ D�� D��HD�HD�AHD�� D���D��qD�>�D�� D��HD�HD�AHD�~�D���D�  D�AHD��HD��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�HD�@ D D��HD�  D�@ D�~�D�� D�  D�@ DĀ D�� D���D�>�D�~�D��HD�  D�>�D�~�D�� D�HD�@ Dǀ DǾ�D�  D�AHDȀ D�� D�  D�>�Dɀ Dɾ�D���D�@ Dʀ D��HD�  D�@ Dˀ D�� D���D�@ D́HD�� D�  D�>�D̀ D��HD�  D�@ D΁HD��HD�HD�@ Dπ D��HD�HD�AHDЀ Dо�D�  D�@ D�~�DѾ�D���D�AHDҀ D�� D�  D�@ D�~�D�� D�  D�>�DԀ D�� D�HD�>�D�}qDվ�D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�>�D؀ D�� D���D�>�D�~�D�� D�  D�AHDڀ Dھ�D�  D�@ D�~�D�� D�HD�AHD܁HD�� D�  D�>�D݀ D��HD�  D�@ Dހ D�� D�  D�@ D߀ D�� D���D�@ D��HD��HD�  D�>�D� D�� D���D�>�D�~�D�� D�  D�@ D�HD�� D�  D�@ D� D�� D�  D�>�D�~�D�� D���D�@ D悏D�� D���D�>�D� D�� D�  D�@ D�~�D辸D�  D�@ D� D�� D�  D�@ D�~�D꾸D���D�@ D� D�� D�  D�>�D�~�D쾸D�  D�AHD� D��HD�  D�@ D� D�� D�  D�@ D�~�D�� D�  D�@ D�� D��HD�HD�@ D� D��HD�  D�>�D� D�� D�  D�AHD�HD��HD�HD�AHD� D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�AHD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Az�AzbAz�Az�AzJAy��AzJAz  Az�Az�Az�Az{Ay��AzbAz-Az-Az1'AzJAy�Ay��AzJAz{Az1'Az9XAz$�Az�Az{AzAz  Ay��Ay��Ay��Ay��Ay�Ay�Ay�Ay�Ay�TAy�Az  Ay�TAy��Ay�
AyƨAyƨAy��Ay`BAy
=Ax�yAw�
Av�AtȴAs?}Aq��Aq?}Ap��Ap�Ap�+Ap��ApȴAp�uAo��AoC�Ao33AooAn�`An�`Ao%An5?Amt�Am+AlȴAm33Al{Ak��AkK�Aj��Aj�!Aj(�Ai�AhbNAg`BAf-Ae7LAd�!Ad�Ac&�Ab�Aa�Aa�A`z�A` �A_�mA_��A_�PA_p�A_C�A_&�A^�`A]�^A[�7AZz�AZI�AZ=qAZAY�
AY��AY&�AX~�AW��AV��AUƨAU"�AU�AU%AT��AT1'AT-AT$�AT{AS��AS��AS�^AS��AS/AQl�AP{AO��AO�TAO�;AO�
AO�
AO�wAO��AO33AN�+AM�AMG�AM
=AL��AL�\AL  AKdZAJ~�AJ�AI�AI�FAH�!AHAG�AE�AD �AD$�ADI�ADJAC�
ACt�AC�ABVAA�-AA"�A@��A?\)A>��A>��A>�!A>�\A>ZA>9XA>bA=�A<��A;��A:��A9�A9XA8�`A8ĜA8�A7�A7�;A7��A7A7A7�PA6�yA5�A5%A4��A4�HA4�\A3�mA3��A3S�A2Q�A1�TA1�A0��A0�jA0  A/VA.�+A.-A-��A,bNA+��A*��A)�wA)t�A)hsA(�HA(��A(�uA(5?A'��A'G�A&�A&M�A%��A%�A%C�A$��A$(�A#�
A#�A#"�A"�9A!�A!
=A A�A��A��A�A��A$�A�-A?}AA�An�A^5A=qAM�AA�A �A��A�;At�Ar�At�A�A��A~�AffAA�A�AA�A�TA��A��AC�A�A�\AVAbA�-Ax�A/A&�A��A�RA�uA�DAz�AbNAM�A�TA�AK�AoA�HA��A~�A��A�uA�
AA
�A	��A	K�A�yA�\AM�A�#A�hAK�AAz�A �A�#AoAAC�A1'A�7A �j@��@��\@��@��@�t�@��H@�E�@�p�@�  @�o@�b@�@���@�@�1'@�S�@�@��`@땁@���@���@��@�(�@���@�X@� �@އ+@�j@�A�@֗�@��/@�@�=q@�M�@Ұ!@�dZ@��m@�C�@�^5@�n�@Ұ!@��@�@�
=@�;d@�l�@�S�@��@���@��@�p�@�Ĝ@��@ΰ!@�J@���@�A�@�33@�
=@���@�X@ȃ@��m@ǝ�@�o@��#@�V@Ĭ@�9X@���@�t�@�33@°!@���@��@�33@���@��@���@�r�@�K�@�J@��T@��-@�%@���@�S�@�|�@��@���@�1@�dZ@�E�@���@��y@��@��\@�^5@�E�@�-@�-@�{@�{@�{@�$�@��@���@���@�p�@�O�@�%@�(�@�$�@�&�@�Ĝ@��D@�Q�@�1@���@�dZ@��@���@�7L@��@��@�dZ@�S�@�C�@�C�@�33@�@��H@�-@���@��@�O�@�Ĝ@� �@���@���@��@�&�@��@�bN@�A�@��@��F@���@���@�"�@��!@��-@�/@��/@���@�z�@�Z@�A�@�9X@�1@��P@�l�@�+@��+@�@���@���@�hs@�G�@�7L@��`@�Ĝ@�r�@���@�dZ@�;d@�"�@��R@��+@�^5@�=q@�@�O�@��/@�Ĝ@���@�r�@�j@�bN@�1'@�1@�ƨ@��@�|�@�l�@�K�@��@��@��@�X@�7L@���@���@�r�@�(�@���@��w@��@���@�l�@��@���@�@�O�@�V@��9@���@��@�r�@�Q�@�dZ@���@��^@�O�@���@�Z@�9X@��F@���@�C�@��@��y@��R@�^5@�-@��@��@���@���@���@��h@�hs@��@�Ĝ@�z�@�Q�@�9X@�@~��@~��@~5?@}�-@|�j@|I�@{t�@z=q@v�y@sƨ@so@rJ@pĜ@ol�@n�@n5?@m@m?}@l��@l�D@l9X@k�m@kdZ@k@j�\@j^5@j-@i�#@ihs@h��@h��@h�@h1'@g��@g�@g�;@h�@k�F@k�
@k�
@lI�@l�@l�@jM�@j=q@j^5@i�@jJ@hA�@fv�@e�@e?}@d�D@c��@b�@bn�@bJ@b�@b�@b��@c��@a�#@`��@a%@a��@b�H@c�
@c�
@c��@d9X@d�/@e��@e@f@f{@f@e�@eV@dZ@cS�@b��@bM�@a�@aX@`��@_�@^��@^5?@^@^@]��@]@]�-@]�-@]�@]/@\�@[�m@[@Z�H@Z=q@Yx�@YX@Yhs@YG�@Y%@X�`@X��@Xr�@X  @W��@W|�@WK�@V��@U�T@U��@U��@Up�@UO�@U/@UV@T��@T�@T�/@T�@T��@T��@Tz�@T�@S�F@SdZ@S"�@R^5@Q��@Q�7@Qhs@Q�@P�`@P�@Ol�@Nff@N@LZ@K�m@K�F@Kt�@K"�@J�H@J�!@J^5@J�@JJ@I�#@Ix�@IG�@IG�@I7L@I7L@IG�@I�7@I��@I�7@H�`@H�@HQ�@H1'@HA�@HA�@H1'@H1'@Hb@G�;@G�w@G�P@G\)@GK�@G;d@G�@F�@FV@E��@EO�@D�/@DI�@D(�@C�
@CS�@C"�@C"�@C"�@Co@Co@C@C@B�H@B�H@B�H@B�@@�`@@�@@�@@r�@@bN@@A�@@A�@@Q�@@Q�@@Q�@@A�@@A�@@A�@@Q�@@A�@@ �@?;d@>��@>�y@>�y@>�R@>��@>��@>�+@>ff@>V@>V@>5?@=�@=p�@=/@=/@=�@=�@=�@=V@<��@<�/@<Z@;ƨ@;o@;o@:�@:��@:��@:=q@9��@9�@9�#@9��@9�^@9�7@9x�@9hs@9X@97L@8��@81'@7�@7�@7�;@7�;@7�w@7�@7�@7�P@7�P@7l�@7�@6�y@6�R@6��@6�+@6�+@6v�@6v�@6v�@6�+@6�+@6�+@6��@6��@6��@6��@6�+@6v�@6v�@6v�@6�+@6ff@6E�@6$�@5�@5�T@5�T@5�@6@5�@5�T@5��@5@5�-@5�h@5�@5p�@5O�@5�@4��@4�@4��@4�D@4j@3��@3�
@3�F@3�F@3�F@3��@3��@3�@3�@3t�@3S�@333@3o@3@2�H@2��@2��@2��@2�!@2~�@2^5@2J@1�@1��@1x�@1x�@1x�@1x�@1hs@1&�@0��@0��@0Ĝ@0�9@0�9@0�9@0��@0��@0�u@0�@0�u@0�u@0�@0�@0r�@0A�@01'@0 �@0 �@0b@0b@0  @/�@/�;@/�;@/��@/�@/�P@/l�@/l�@/l�@/l�@/\)@/;d@/+@/�@/
=@/�@/
=@.��@.�@.�R@.��@.�+@.v�@.v�@.v�@.v�@.v�@.ff@.ff@.E�@.$�@.$�@.$�@.$�@.{@.@.@-�@-�T@-��@-��@-�T@-�T@-��@-�-@-�h@-�@-�@,�j@,�D@,��@,�@,�@,�@,�@,�@,�@,�@,�@,��@,z�@,Z@,�@+�
@+��@+��@+�@+��@+��@+�@+t�@+�@+t�@+C�@*�@*�H@*��@*�H@*��@*�!@*�\@*^5@*=q@*=q@*-@*-@*-@*-@*=q@*=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Az�AzbAz�Az�AzJAy��AzJAz  Az�Az�Az�Az{Ay��AzbAz-Az-Az1'AzJAy�Ay��AzJAz{Az1'Az9XAz$�Az�Az{AzAz  Ay��Ay��Ay��Ay��Ay�Ay�Ay�Ay�Ay�TAy�Az  Ay�TAy��Ay�
AyƨAyƨAy��Ay`BAy
=Ax�yAw�
Av�AtȴAs?}Aq��Aq?}Ap��Ap�Ap�+Ap��ApȴAp�uAo��AoC�Ao33AooAn�`An�`Ao%An5?Amt�Am+AlȴAm33Al{Ak��AkK�Aj��Aj�!Aj(�Ai�AhbNAg`BAf-Ae7LAd�!Ad�Ac&�Ab�Aa�Aa�A`z�A` �A_�mA_��A_�PA_p�A_C�A_&�A^�`A]�^A[�7AZz�AZI�AZ=qAZAY�
AY��AY&�AX~�AW��AV��AUƨAU"�AU�AU%AT��AT1'AT-AT$�AT{AS��AS��AS�^AS��AS/AQl�AP{AO��AO�TAO�;AO�
AO�
AO�wAO��AO33AN�+AM�AMG�AM
=AL��AL�\AL  AKdZAJ~�AJ�AI�AI�FAH�!AHAG�AE�AD �AD$�ADI�ADJAC�
ACt�AC�ABVAA�-AA"�A@��A?\)A>��A>��A>�!A>�\A>ZA>9XA>bA=�A<��A;��A:��A9�A9XA8�`A8ĜA8�A7�A7�;A7��A7A7A7�PA6�yA5�A5%A4��A4�HA4�\A3�mA3��A3S�A2Q�A1�TA1�A0��A0�jA0  A/VA.�+A.-A-��A,bNA+��A*��A)�wA)t�A)hsA(�HA(��A(�uA(5?A'��A'G�A&�A&M�A%��A%�A%C�A$��A$(�A#�
A#�A#"�A"�9A!�A!
=A A�A��A��A�A��A$�A�-A?}AA�An�A^5A=qAM�AA�A �A��A�;At�Ar�At�A�A��A~�AffAA�A�AA�A�TA��A��AC�A�A�\AVAbA�-Ax�A/A&�A��A�RA�uA�DAz�AbNAM�A�TA�AK�AoA�HA��A~�A��A�uA�
AA
�A	��A	K�A�yA�\AM�A�#A�hAK�AAz�A �A�#AoAAC�A1'A�7A �j@��@��\@��@��@�t�@��H@�E�@�p�@�  @�o@�b@�@���@�@�1'@�S�@�@��`@땁@���@���@��@�(�@���@�X@� �@އ+@�j@�A�@֗�@��/@�@�=q@�M�@Ұ!@�dZ@��m@�C�@�^5@�n�@Ұ!@��@�@�
=@�;d@�l�@�S�@��@���@��@�p�@�Ĝ@��@ΰ!@�J@���@�A�@�33@�
=@���@�X@ȃ@��m@ǝ�@�o@��#@�V@Ĭ@�9X@���@�t�@�33@°!@���@��@�33@���@��@���@�r�@�K�@�J@��T@��-@�%@���@�S�@�|�@��@���@�1@�dZ@�E�@���@��y@��@��\@�^5@�E�@�-@�-@�{@�{@�{@�$�@��@���@���@�p�@�O�@�%@�(�@�$�@�&�@�Ĝ@��D@�Q�@�1@���@�dZ@��@���@�7L@��@��@�dZ@�S�@�C�@�C�@�33@�@��H@�-@���@��@�O�@�Ĝ@� �@���@���@��@�&�@��@�bN@�A�@��@��F@���@���@�"�@��!@��-@�/@��/@���@�z�@�Z@�A�@�9X@�1@��P@�l�@�+@��+@�@���@���@�hs@�G�@�7L@��`@�Ĝ@�r�@���@�dZ@�;d@�"�@��R@��+@�^5@�=q@�@�O�@��/@�Ĝ@���@�r�@�j@�bN@�1'@�1@�ƨ@��@�|�@�l�@�K�@��@��@��@�X@�7L@���@���@�r�@�(�@���@��w@��@���@�l�@��@���@�@�O�@�V@��9@���@��@�r�@�Q�@�dZ@���@��^@�O�@���@�Z@�9X@��F@���@�C�@��@��y@��R@�^5@�-@��@��@���@���@���@��h@�hs@��@�Ĝ@�z�@�Q�@�9X@�@~��@~��@~5?@}�-@|�j@|I�@{t�@z=q@v�y@sƨ@so@rJ@pĜ@ol�@n�@n5?@m@m?}@l��@l�D@l9X@k�m@kdZ@k@j�\@j^5@j-@i�#@ihs@h��@h��@h�@h1'@g��@g�@g�;@h�@k�F@k�
@k�
@lI�@l�@l�@jM�@j=q@j^5@i�@jJ@hA�@fv�@e�@e?}@d�D@c��@b�@bn�@bJ@b�@b�@b��@c��@a�#@`��@a%@a��@b�H@c�
@c�
@c��@d9X@d�/@e��@e@f@f{@f@e�@eV@dZ@cS�@b��@bM�@a�@aX@`��@_�@^��@^5?@^@^@]��@]@]�-@]�-@]�@]/@\�@[�m@[@Z�H@Z=q@Yx�@YX@Yhs@YG�@Y%@X�`@X��@Xr�@X  @W��@W|�@WK�@V��@U�T@U��@U��@Up�@UO�@U/@UV@T��@T�@T�/@T�@T��@T��@Tz�@T�@S�F@SdZ@S"�@R^5@Q��@Q�7@Qhs@Q�@P�`@P�@Ol�@Nff@N@LZ@K�m@K�F@Kt�@K"�@J�H@J�!@J^5@J�@JJ@I�#@Ix�@IG�@IG�@I7L@I7L@IG�@I�7@I��@I�7@H�`@H�@HQ�@H1'@HA�@HA�@H1'@H1'@Hb@G�;@G�w@G�P@G\)@GK�@G;d@G�@F�@FV@E��@EO�@D�/@DI�@D(�@C�
@CS�@C"�@C"�@C"�@Co@Co@C@C@B�H@B�H@B�H@B�@@�`@@�@@�@@r�@@bN@@A�@@A�@@Q�@@Q�@@Q�@@A�@@A�@@A�@@Q�@@A�@@ �@?;d@>��@>�y@>�y@>�R@>��@>��@>�+@>ff@>V@>V@>5?@=�@=p�@=/@=/@=�@=�@=�@=V@<��@<�/@<Z@;ƨ@;o@;o@:�@:��@:��@:=q@9��@9�@9�#@9��@9�^@9�7@9x�@9hs@9X@97L@8��@81'@7�@7�@7�;@7�;@7�w@7�@7�@7�P@7�P@7l�@7�@6�y@6�R@6��@6�+@6�+@6v�@6v�@6v�@6�+@6�+@6�+@6��@6��@6��@6��@6�+@6v�@6v�@6v�@6�+@6ff@6E�@6$�@5�@5�T@5�T@5�@6@5�@5�T@5��@5@5�-@5�h@5�@5p�@5O�@5�@4��@4�@4��@4�D@4j@3��@3�
@3�F@3�F@3�F@3��@3��@3�@3�@3t�@3S�@333@3o@3@2�H@2��@2��@2��@2�!@2~�@2^5@2J@1�@1��@1x�@1x�@1x�@1x�@1hs@1&�@0��@0��@0Ĝ@0�9@0�9@0�9@0��@0��@0�u@0�@0�u@0�u@0�@0�@0r�@0A�@01'@0 �@0 �@0b@0b@0  @/�@/�;@/�;@/��@/�@/�P@/l�@/l�@/l�@/l�@/\)@/;d@/+@/�@/
=@/�@/
=@.��@.�@.�R@.��@.�+@.v�@.v�@.v�@.v�@.v�@.ff@.ff@.E�@.$�@.$�@.$�@.$�@.{@.@.@-�@-�T@-��@-��@-�T@-�T@-��@-�-@-�h@-�@-�@,�j@,�D@,��@,�@,�@,�@,�@,�@,�@,�@,�@,��@,z�@,Z@,�@+�
@+��@+��@+�@+��@+��@+�@+t�@+�@+t�@+C�@*�@*�H@*��@*�H@*��@*�!@*�\@*^5@*=q@*=q@*-@*-@*-@*-@*=q@*=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�LB�LB�LB�LB�FB�?B�FB�FB�FB�LB�LB�FB�?B�FB�RB�RB�LB�?B�9B�9B�?B�FB�LB�RB�FB�FB�?B�9B�9B�3B�9B�9B�3B�3B�3B�3B�3B�-B�3B�3B�-B�'B�'B�!B�!B�B��B��B��B��B�DBv�BgmB^5BYBYBZB`BBn�Bq�Bq�Br�Bq�Br�Bq�Br�Bt�Bx�Bx�Bp�Bl�Bp�B�JB�hB�hB�uB�hB�bB�JB�%B}�Bu�Bn�BiyBgmBcTB\)BYBR�BM�BH�BF�BE�BB�BA�B@�B=qB<jB8RB,B�B�BuBoBhB\BJB	7BB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�BB�
B��B��B��B��B��B��B��B��BɺBĜB�}B�qB�dB�RB�3B�B��B��B��B��B�oB�=B�BiyB_;BcTBffBffBhsBgmBcTB\)BVBO�BH�B>wB:^B8RB7LB5?B49B49B2-B.B%�B�B�BDB+BB  B��B��B��B��B��B��B�B�B�NB�5B�/B�)B�B��B��B��BB�wB�dB�FB�3B�B��B��B��B��B�7B�Bz�Bq�Bo�Bo�Bn�Bp�Bm�BjBe`BaHB[#BXBT�BR�BN�BI�BC�B?}B;dB6FB2-B+B!�B�BoB
=BB
��B
�B
�B
�mB
�ZB
�;B
�5B
�5B
�/B
�)B
�#B
�B
��B
ĜB
��B
�jB
�RB
�LB
�FB
�FB
�?B
�?B
�?B
�?B
�9B
�9B
�3B
�3B
�-B
�'B
�'B
�'B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�{B
�\B
�=B
�%B
�B
�B
� B
~�B
|�B
z�B
y�B
w�B
u�B
s�B
q�B
n�B
iyB
cTB
]/B
W
B
R�B
M�B
H�B
G�B
D�B
B�B
@�B
>wB
<jB
8RB
33B
.B
'�B
%�B
#�B
!�B
�B
�B
�B
hB

=B
B	��B	�B	�B	�mB	�HB	�)B	��B	��B	�}B	�XB	�9B	�'B	�!B	�?B	�XB	�wB	B	B	B	ǮB	ɺB	��B	��B	��B	��B	�B	�)B	�)B	�#B	�B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	ǮB	ƨB	ŢB	ĜB	ÖB	B	��B	��B	�}B	�wB	�qB	�dB	�RB	�FB	�3B	�-B	�'B	�!B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�\B	�7B	�7B	�7B	�7B	�=B	�DB	�DB	�DB	�JB	�JB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�-B	�-B	�'B	�-B	�-B	�3B	�9B	�9B	�9B	�9B	�9B	�9B	�FB	�RB	�RB	�XB	�XB	�^B	�dB	�dB	�jB	�jB	�jB	�dB	�dB	�jB	�jB	�jB	�qB	�qB	�jB	�jB	�jB	�dB	�dB	�^B	�dB	�^B	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�qB	�qB	�}B	�}B	��B	��B	��B	��B	��B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ŢB	ŢB	ĜB	ŢB	ŢB	ƨB	ȴB	ȴB	ƨB	ĜB	ÖB	ĜB	ÖB	��B	B	B	B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	�BB	�NB	�ZB	�mB	�yB	�yB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
B
%B
1B
VB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
%�B
&�B
(�B
(�B
(�B
+B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
5?B
6FB
6FB
7LB
8RB
:^B
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
D�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
I�B
M�B
O�B
Q�B
R�B
R�B
S�B
W
B
YB
YB
ZB
ZB
ZB
ZB
[#B
]/B
^5B
_;B
`BB
bNB
cTB
dZB
ffB
ffB
hsB
iyB
jB
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
l�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
p�B
r�B
s�B
t�B
u�B
w�B
x�B
x�B
y�B
z�B
z�B
z�B
{�B
}�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�1B
�DB
�bB
�bB
�bB
�hB
�hB
�uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�'B
�-B
�-B
�3B
�3B
�3B
�9B
�FB
�LB
�LB
�XB
�XB
�XB
�XB
�^B
�^B
�dB
�dB
�jB
�jB
�qB
�qB
�wB
�}B
��B
B
ÖB
ÖB
ĜB
ŢB
ƨB
ǮB
ȴB
ȴB
ȴB
ɺB
ɺB
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
�B
�B
�B
�B
�#B
�)B
�/B
�5B
�5B
�5B
�HB
�NB
�TB
�ZB
�ZB
�ZB
�ZB
�ZB
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�fB
�mB
�mB
�mB
�sB
�sB
�sB
�sB
�yB
�yB
�yB
�B
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
�B
�B
�B
�B
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
��B  BBBBBBBBBBBBB%B%B+B+B+B+B+B1B	7B	7B	7B
=B
=BDBDBDBJBJBJBJBJBPBPBPBVBVBVB\B\B\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�fB�+B�pB��B��B�B�[B��B�=B�LB�iB��B�	B�	B�RB�PB��B��B�6B�B�(B�B�@B��B�lB�XB�pB�NB�PB�5B�;B�IB�DB�DB�:B�0B�aB�B�B��B�\B�,B�YB�.B�|B��B��B�lB��B��B�ZB{
Bj�B`^BZ�BY\BZB_�Bn�BrHBs�Bt2Bq�BsBr+Br�Bt�Bz�Bz�Bq�Bm�Bo�B�B��B�PB�SB�=B��B�%B�0B��ByBqDBkBiBe�B]>B[�BU1BO�BI�BGOBFTBB�BA�B@�B=�B=;B;�B1�B�B*B�BB�BB�BB�B�B�,B��B��B�)B�B��B��B��B�B�B�<B�
B�B�B�[B��B�nB�BB�
B�B��B�:B�9B�:BϲB�sB�dB�3B�B�'B��B��B��B�B�\B��B�bB�`B��B�nBl~B_jBb�BgBgBi�BhrBeyB]�BW�BQxBLB?�B:�B8�B7�B5�B4�B4�B3MB0nB(TB!�BBOBaB�B�B�kB�B��B��B��B�gB��B�_B�B�bB݂B�B��B��BϼB�\B��B��B��B�B�LB��B�ZB��B��B�CB��B�qB}�Br�Bo�BqBn�BqXBn�BlBfBb�B\DBYBU�BT*BP8BK�BD�B@wB<}B7�B4bB-�B$B�B�B�B*B
��B
�B
��B
�8B
�B
ߐB
�lB
ގB
�
B
�TB
ۨB
�bB
��B
��B
�uB
�PB
��B
�%B
��B
��B
��B
��B
��B
�rB
�qB
�|B
��B
�JB
�nB
� B
��B
��B
�8B
��B
��B
�AB
��B
��B
�zB
�+B
�8B
�RB
�NB
�4B
�B
��B
��B
�qB
��B
�[B
�cB
��B
��B
��B
��B
��B
�B
�7B
�B
�B
~=B
{�B
z�B
x�B
wIB
t�B
r�B
p�B
l�B
e�B
`RB
YB
UZB
Q�B
I�B
I�B
FB
C�B
AdB
?kB
=�B
:qB
4�B
2jB
*B
'/B
%�B
"�B
!B
�B
KB
rB
�B
XB	�0B	��B	�B	�B	�4B	ޔB	�FB	�B	�8B	�B	�B	�]B	�*B	��B	�`B	��B	ÖB	��B	B	�LB	�eB	˴B	��B	ϤB	ӶB	�:B	�tB	ܥB	�<B	�2B	�#B	�\B	��B	�B	ԑB	�B	�jB	� B	�]B	��B	�B	ɬB	�1B	ǎB	�nB	��B	�@B	�KB	�4B	�B	��B	�PB	��B	��B	��B	�B	�tB	��B	��B	��B	��B	�OB	�^B	�B	�B	�XB	��B	��B	��B	��B	��B	�XB	�B	��B	�`B	��B	��B	�qB	�oB	�OB	�vB	�RB	�VB	�KB	��B	��B	��B	��B	��B	�#B	�B	��B	� B	�9B	��B	��B	�B	�KB	�
B	��B	�KB	��B	��B	�EB	�HB	��B	��B	��B	��B	�B	�B	��B	��B	�B	�$B	��B	��B	��B	�dB	��B	�B	��B	�NB	�B	�]B	�8B	��B	� B	��B	��B	�cB	��B	�gB	�VB	�"B	�%B	�B	�B	�DB	��B	�+B	�`B	��B	��B	�OB	�XB	�NB	�8B	�&B	��B	�BB	��B	�HB	�lB	�WB	�DB	��B	�fB	�\B	�TB	��B	�,B	��B	�ZB	�fB	�|B	�DB	�DB	�B	�wB	��B	��B	�NB	�\B	�wB	��B	��B	��B	�>B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�:B	��B	�)B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�nB	��B	��B	�7B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�B	��B	��B	�>B	�6B	��B	�B	�B	�iB	�B	�dB	ɿB	�ZB	�B	�CB	�pB	ĠB	B	�B	�B	��B	�B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�
B	�B	��B	��B	��B	�B	ʶB	��B	�RB	ѲB	�B	�NB	�
B	�.B	��B	��B	�B	�gB	��B	�B	�B	��B	�	B	�B	� B	�?B	� B	��B	��B	�B	�B	�MB	�#B	�.B	��B	��B	�bB	�B
YB
B

B
�B
�B
�B
VB
WB
}B
�B
�B
HB
2B
hB
B
�B
�B
B
B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
(B
 aB
!uB
!�B
!TB
"fB
"�B
"�B
"�B
#
B
#�B
$�B
%5B
&BB
'>B
)B
)%B
)�B
+�B
0,B
1PB
1TB
1KB
2JB
2LB
2@B
2?B
2AB
3^B
4JB
4BB
4]B
5�B
6�B
6�B
7�B
8�B
:�B
<�B
<�B
<�B
=�B
=�B
>NB
?JB
>�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
D�B
D�B
E�B
F�B
G�B
G�B
G�B
G�B
I�B
M�B
O�B
RnB
SCB
S B
TB
WB
YB
Y+B
ZB
Z:B
ZGB
Z@B
[RB
]YB
^FB
_OB
`fB
b�B
c�B
d�B
f�B
f�B
h�B
i�B
j�B
k�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
m5B
m�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
q^B
r�B
s�B
t�B
u�B
w�B
x�B
x�B
y�B
z�B
z�B
{B
|&B
~]B
�;B
�B
�!B
�B
�B
� B
�'B
�>B
��B
��B
��B
�^B
�B
��B
�qB
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
�EB
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
�1B
�B
�#B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�$B
�*B
�7B
�:B
�=B
�2B
�4B
�+B
�ZB
�_B
�fB
�vB
�aB
�XB
�LB
�PB
�pB
�qB
�pB
�vB
�wB
��B
�~B
��B
��B
��B
��B
©B
äB
çB
ļB
��B
��B
��B
ȴB
ȶB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�
B
��B
��B
�
B
�B
�'B
�B
�IB
�,B
�OB
�JB
�-B
�0B
�6B
�EB
�qB
�qB
�jB
�eB
�gB
�\B
�[B
�gB
�`B
�nB
�nB
�VB
�aB
�nB
�dB
�tB
�B
�zB
�~B
�oB
�B
�uB
�B
�B
�B
�|B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
�B
�B
�B
�B
�B
��B
��B
�B
��B
��B
�B
�B
�B
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
�B
�
B
�B
�HB OB/BBBBBBBBBB.B;BCB\B^BaB0B:B#B4B	BB	DB	2B
KB
gB�BRBUB;B[BfBiBvBhBRB]BXBYBZBQB^B^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�<#�]<#��<#�<#�4<#�N<#�c<#��<#�I<#�
<#ٛ<#�4<#��<#�e<#�
<#�<#�)<#�M<#�<#�8<#ا<#��<#�{<#�E<#�r<#�<#�^<#�c<#ا<#�<#�<#��<#��<#��<#�0<#�<#�<#�C<#ڑ<#�<#��<#�<#ޫ<#׎<#�N<$L<$y�<$a<)�N<+�<6�<1j7<-*�<'r#<&?><#�<#ף<$/%<#�
<$#(<'<%�M<#�l<#�m<$	�<#�X<#ܯ<'T�<&��<$m,<$�J<$XX<)��<%:{<$z�<$m,<$aD<%�l<&�<*��<)��<,.<)SQ<%ȧ<%��<(�,<$��<(�<'�-<&/<$��<$,<$7�<#�<#�"<$p<#�<$\"<,S<<��<+�<$.<#�<$$<$	�<$9�<% <&v�<)�<)W[<'��<&7"<#�<#�<$�<$��<#ף<#��<#��<#�M<#�a<#�l<#�)<%G<5*<-�]<#��<#�&<#�<#ا<#�0<#�U<#�l<%8j<&�}<&$h<&L�<$<<$(<$J�<%��<&U"<(y�<$�w<$�<$H�<)c�<&�2<&)�<A�<*Ѝ<#��<#��<$%<$+<$�J<$�j<'d�<&q<%��<%�y<,��<$�<$�<#�5<#��<$Z<#�<$a<$�h<(�<(n�<*�<-s�<$�	<$�L<$ �<&!�<$F<#�^<#��<#��<#�0<$)
<&��<.�!<%\\<#�8<#�<$��<&W�<$r�<$t <(��<%8j<$��<%��<$C�<'><)//<%��<$�7<%'<.e<(�<(B�<*,�<$�V<#�<%m�<#�&<$9�<$�;<%�6<$ѩ<&�<$�(<$�Q<$ub<$�<%MY<&h�<$��<$�X<$�R<%>�<'�.<)�<'�O<&�k<'޽<(��<+_j<&�<%MY<%8j<$T�<%Z2<#�<#�E<#�5<#�8<#ܯ<$�<,<�<+�<%Q�<*{�<*F�<%��<$f�<$�<#�<#�N<#��<#��<#��<#��<#�<$�<$��<%S<$f�<$4e<$]h<$��<$6�<$_�<#�8<$
�<$E<#��<#��<#�<#�<#�<$�<$��<$<<$1:<$�<$.<$F<%�~</�:<'��<(�<)G9<%��<$|d<$��<$�	<$XX<%,#<$y�<$_�<$�<%��<$�J<$��<'��<+n<(<+Z�<&��<(M}<.G<$aD<'1;<%�l<$�<$q@<$�V<%(<'Qf<%�`<1�-<'�<%&<&�<$Z�<%2?<&�+<&A�<'�<(c�<,S<,.<'�|<'W�<'��<&�^<(X~<,�<?Ȼ<)��<)�<)��<$�L<#�I<$'<$�Q<$5w<$��<%,#<#�I<#�W<#�<#�$<#�<<#�<#�U<#��<#�4<$�<$�R<$��<$��<%&<&�%<$�2<%�`<$�J<%��<#�!<$F<'�<%4L<$�Q<$a<$x+<&W�<%
�<$/%<$B�<$0.<$�<#��<$f�<%�<(��<%v�<&9�<#��<$Z<&A�<&`8<&��<#�!<#�W<$�<'�<$	�<#ף<'�<&�J<%F<$�;<&�
<.C<%v�<#�+<$<<#�<#�J<#ܯ<#�i<#ޫ<#�<<#�{<#�<#�<#�U<#�<#�l<#�<$G<%m�<*ٜ<%�L<$1:<#�<#��<$p<$6�<#��<$<<%�y<$�J<&�R<$ <$�<#�8<#��<#׺<#ۮ<#�<#��<$�<$y�<#�E<#�5<$y�<$��<$\"<%�`<$��<%'<$O�<$�<#�&<$�<#�<#��<#�+<$?[<$MO<%�Z<$c�<$.<#�a<#��<#�U<#��<#��<#�N<$H�<#�U<$<<$��<$]h<#�5<#�W<#�<#�<#�l<$.<#��<$�<%&<#��<#�M<#��<$0.<#�<#��<#�<#�H<$Ş<$7�<#��<#�&<#�<#ا<#ا<#�<#�<$ �<#��<#�c<#��<#��<#�<#��<%��<$�Q<#�e<#�(<#�<$+<$/<#�"<#�W<#��<#��<#��<$
<$b�<% <$F9<$p<$G<#�8<#��<#�8<#�<%p<%$<%S�<$?[<$�7<$p<#�<$^�<#�<$�<#�M<#��<#�"<$�<#�<#��<#�i<#��<#�c<#׎<#�)<#�<$�<$�<$�<#�<#��<$�<$�<#�<#�	<#��<$O�<#��<$5w<$�	<)k�<(�_<$2G<$_�<$�k<$�7<$	<$<#��<$ <#��<#�<#�<#�<#�N<#�<#�m<#�<#�<#�<#�W<#�)<#�<#�8<#�<#�<#�i<#׎<$	�<'�O<#�^<#�
<#�<#�&<$/<%G<#��<#�{<#�<#�i<%e<%MY<$.<$f<$ K<$@|<$ K<#��<#�<#�<#�<#�m<$k<%@�<$A�<#�<$�<$o�<$8�<#׎<#�c<#��<#�H<$'<#�8<#�l<#�X<#�C<#�<$:�<$}<$aD<$	�<#�m<#�"<$<<#��<$B�<$�k<#��<#�l<#ף<#��<#�C<#ף<#�i<#��<#�<#�N<$(<$5w<#�r<$f<$ K<#��<#�<#�]<#��<#�*<#�C<#�<#�<#�<#�*<#��<$k<$Z<#�i<#�+<#�8<#��<#ٛ<#��<#�$<#�<#�C<#ܯ<#��<#�I<#��<#�<#�<#�<#�&<$'<$F<#��<#��<#�l<#��<#��<$k�<$^�<#�g<%,#<#��<#�8<#�N<#�M<#��<#�^<#�l<#�E<#�$<#��<#�<#ܯ<#�<#��<#�&<#�I<#�8<#׺<#��<$
�<#�<#�<#��<#�I<#�<#�C<#�<#ٛ<#�l<#��<#��<#�l<#��<#�C<#��<#��<#��<$�<#�<#��<$ <#��<#�<#��<#��<#�
<#�<#��<#�<#ף<#�&<#ٛ<#�<#ף<$/%<$��<#�<#�<#��<#��<#��<#�<#�X<#�<#�<#�C<#�<#�<#�{<#؄<#�r<$@|<#��<#��<#�<#�<#ף<#�<#��<#��<#��<#�<#�8<#�&<#��<#�<#�<#׺<#�<#�<#ף<#ף<#�8<#�N<$<<$f<#�<#ٛ<#�&<#�I<#�<#ߜ<#�X<#�C<#׺<#؄<#ۮ<#׺<#��<#��<#�]<#�<$k<#�J<#�<#׺<#�<#ٛ<#ף<#�0<#�*<#�<#��<#��<#��<#ۮ<#�*<#�X<#�<#�{<#�<#�
<#�<<#�&<#�
<#�$<#�<#�I<#��<#׎<#��<#�<#�<#�<<#�]<#��<#�<#�l<#�I<#�
<#�{<#�<<#�<#�$<#�{<#�<#׎<#�*<#׎<#��<#��<#�l<#�l<#�<#ף<#��<#�*<#�N<#�D<#�o<#�
<#�<#�<#�{<#��<#�
<#�$<#�*<#ٛ<#�<#�i<#��<#׎<#�<<#�<#�$<#�+<#�<#�<#�o<#��<#ۮ<#�<#�<#�<#��<#�<#�+<#�o<#��<#׎<#�<#�<#׎<#�&<#ף<#ף<#�X<#�<#ף<#�<#�C<#��<#׎<#��<#�<#�{<#�<#�{<#׺<#׎<#�<#��<#��<#��<#�D<#�<#�<#�<#�<#ٛ<#�c<#�<#�X<#�i<#׺<#�c<#��<#�o<#ף<#�D<#׎<#�<#�<#�<#�<#ף<#�<#ٛ<#�o<#�<#�<#�<#׎<#�{<#�<#׺<#׺<#�X<#�<#�i<#�<#�<#�o<#�o<#��<#�<#�<#�+<#�<<#�i<#�<#�<#�<#�<#�<#�<#�<#׺<#�o<#��<#�E<#��<#��<#�<#׺<#�<<#�<#�i<#׎<#�<#ף<#�l<#�e<#ף<#��<#׺<#��<#�o<#��<#��<#��<#�<#׎<#�<#�<#�<#�i<#�<#�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = CTM_ADJ_PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                              None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment;OW: r =1(+/-0.0001), vertically averaged dS =-0.005(+/-0.002),                                                                                                             SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT.; No significant drift detected in conductivity                                                                                                                                   202205040000002022050400000020220504000000  AO  ARGQQCPL                                                                    20210217033942  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210217033942  QCF$                G�O�G�O�G�O�0               WHOIARSQWHQCV0.5                                                                20211004000000  QC                  G�O�G�O�G�O�                WHOIARSQCTM V1.0                                                                20220503000000  IP                  G�O�G�O�G�O�                WHOIARCAOWC V2.0ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     20220504000000  IP                  G�O�G�O�G�O�                