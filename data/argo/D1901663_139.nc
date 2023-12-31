CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       $Woods Hole Oceanographic Institution   source        
Argo float     history       92021-02-16T20:48:08Z creation; 2022-05-04T12:55:31Z DMQC;      
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
resolution        :�o     �  q\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ߜ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20210216204808  20220504085531  1901663 US ARGO PROJECT                                                 BRECK OWENS, STEVE JAYNE, P.E. ROBBINS                          PRES            TEMP            PSAL               �A   AO  5137                            2C  D   S2A                             7179                            SBE602 V1.3                     854 @���K7�1   @��O��p�DG�#@E	��h��1   GPS     Primary sampling: averaged [nominal 2 dbar binned data sampled at 0.5 Hz from a SBE41CP]                                                                                                                                                                           A   A   A   ?�\)@�\@@  @}p�@�  @�G�@�  @��RA\)A   A@  A^�RA~�RA��A�  A�  A�  A�\)A߮A�Q�B (�B(�B(�B  B   B(  B0(�B8  B?�
BH  BP  BX(�B`  Bh  Bp  Bx  B�  B�  B�  B�{B�  B��B�  B�  B�  B�  B�(�B�{B�  B�  B�{B�  B�  B�  B�  B�(�B�{B��B��B��B�  B�{B�{B�{B�  B�  B�  B�  C   C  C��C��C�C	��C��C  C  C  C��C  C  C  C��C  C   C!��C$  C&
=C(  C*  C,
=C.  C/��C2  C3��C5��C7��C:  C<  C>
=C@
=CB
=CD  CE��CG��CJ  CL  CN
=CP  CR
=CT
=CV
=CX
=CZ  C\
=C^
=C`  Ca��Cc��Cf  Ch  Cj  Cl  Cn  Cp  Cr
=Ct
=Cv  Cx  Cz
=C|  C~  C�C�C�  C�  C�C�C�  C�  C�  C�  C���C���C���C���C���C�  C�  C�C�  C�  C���C�  C�C�  C���C�  C�C�C�  C�C�
=C�C���C�  C�C���C���C���C���C���C���C���C�  C�  C���C�  C�  C�C�
=C�C�C�C�  C���C���C�  C�C�  C���C���C���C�  C�C�  C�  C���C�  C�  C�  C���C�  C�  C�  C�  C�C�  C�  C�  C�C�C�  C���C���C�  C���C�  C�  C�C�C�  C���C���C�  C�  C���C���C���C�  C���C�  C�C�  C���C���C�  C�  C�  C���C���C�  C�  C�C�C�  C�  C�C�C�  C�  C�C�  C�  C���C���C�  C�  C���C���C���D }qD �qD}qD  D��D  D� D�D� D  D� D�qD� D  D� D�D��D	�D	��D
�D
��D  D}qD�qD}qD  D��D  D}qD  D��D�D� D  D� D�qD}qD�qD� D�D��D�D� D  D��D�D��D�D� D�qD}qD  D}qD  D� D�qD� D  D}qD  D� D  D� D   D }qD �qD!� D"  D"��D#�D#}qD$  D$��D%  D%� D&  D&� D'�D'��D(�D(��D)  D)}qD*  D*� D+  D+� D,  D,� D-  D-� D.�D.��D/�D/��D0  D0��D1  D1� D2  D2��D3�D3��D4�D4��D5�D5��D6  D6� D7  D7� D8  D8� D9  D9}qD:  D:��D;�D;� D<  D<}qD=  D=� D>  D>}qD>�qD?� D@  D@� DA�DA��DB�DB��DC�DC� DD  DD��DE�DE��DF  DF� DG  DG� DH  DH� DI  DI� DI�qDJ}qDJ�qDK� DL�DL� DL�qDM� DN  DN� DO  DO��DP  DP}qDP�qDQ� DR  DR}qDR�qDSz�DS�qDT}qDU  DU� DV  DV��DW�DW�DX  DX� DY  DY}qDZ  DZ}qDZ�qD[� D\  D\� D]  D]� D]�qD^� D_  D_��D_�qD`��Da�Da� Db  Db}qDb�qDc}qDd  Dd}qDe  De��Df  Df}qDf�qDg��Dh  Dh}qDi  Di� Dj  Dj��Dk  Dk� Dl  Dl� Dl�qDm� Dn  Dn��DoDo��Dp  Dp� Dq�Dq�DrDr��Ds�Ds� Dt�Dt��Du  Du� Du�qDv� Dw�Dw� Dw�qDx� Dx�qDy}qDy�qDz}qD{  D{��D|  D|� D}  D}� D~  D~}qD  D� D�qD�@ D�� D���D���D�@ D�� D�� D�HD�AHD�� D�� D�HD�@ D�� D��HD�HD�@ D�� D���D�  D�@ D�� D��HD���D�@ D�� D�� D�HD�AHD��HD��HD�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D�~�D�� D�  D�>�D��HD��HD�  D�AHD�� D�� D�  D�>�D�� D�� D���D�>�D�� D�� D�  D�AHD��HD��HD�HD�>�D�� D��HD�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��HD��HD���D�>�D�~�D�� D�  D�>�D�~�D�� D��qD�>�D��HD��HD�  D�>�D�~�D��HD�HD�@ D��HD�� D���D�>�D�� D�� D���D�>�D��HD��HD�  D�@ D�� D���D�  D�AHD��HD��HD�  D�@ D�� D���D�  D�@ D��HD�� D���D�AHD�� D�� D�HD�@ D�~�D�� D���D�@ D�� D�� D�  D�@ D���D�D�HD�AHD�� D���D���D�>�D�� D�� D���D�>�D�� D�� D���D�@ D��HD��HD���D�>�D�� D�� D��qD�>�D�~�D�� D�HD�AHD�~�D���D���D�AHD�� D�� D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�~�D���D���D�@ D�� D��HD�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D��HD���D���D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D���D�>�D D��HD�  D�@ DÀ Dþ�D���D�AHDāHD�� D�  D�AHDŀ D�� D�  D�@ D�~�Dƾ�D���D�@ DǁHD��HD�  D�>�DȀ D�� D�  D�>�D�~�D��HD�  D�@ D�~�Dʾ�D�  D�>�D�~�D˾�D���D�@ D�~�D̾�D���D�>�D̀ D;�D���D�@ D΁HD��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�AHDсHD�� D�  D�@ DҁHD�� D�HD�@ DӀ D��HD�  D�>�DԀ D�� D�  D�@ DՀ Dվ�D���D�@ Dր D�� D�HD�@ D׀ D�� D�  D�@ D؀ D�� D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�@ Dۀ D۾�D���D�@ D܀ DܽqD�  D�AHD݀ Dݾ�D�HD�@ Dހ D޾�D���D�>�D߁HD�� D�  D�@ D��HD�� D�  D�AHDႏD��HD�HD�AHD�HD��HD�HD�@ D� D��HD�  D�>�D� D�� D�  D�AHD� D��HD�HD�@ D�~�D澸D�  D�@ D� D�� D�  D�@ D�HD��HD�  D�AHD�HD�� D�  D�>�D�~�D꾸D���D�@ D� D�� D�  D�>�D� D�� D���D�>�D�~�D���D�  D�@ D� D�� D�  D�@ D�HD��HD�  D�>�D�� D�� D�  D�AHD� D�D���D�>�D� D��HD�HD�AHD�D�D�  D�@ D� D�� D�  D�@ D��HD��HD�  D�@ D�� D���D���D�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�~�D�� D�  D�AHD��HD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�\)@�\@@  @}p�@�  @�G�@�  @��RA\)A   A@  A^�RA~�RA��A�  A�  A�  A�\)A߮A�Q�B (�B(�B(�B  B   B(  B0(�B8  B?�
BH  BP  BX(�B`  Bh  Bp  Bx  B�  B�  B�  B�{B�  B��B�  B�  B�  B�  B�(�B�{B�  B�  B�{B�  B�  B�  B�  B�(�B�{B��B��B��B�  B�{B�{B�{B�  B�  B�  B�  C   C  C��C��C�C	��C��C  C  C  C��C  C  C  C��C  C   C!��C$  C&
=C(  C*  C,
=C.  C/��C2  C3��C5��C7��C:  C<  C>
=C@
=CB
=CD  CE��CG��CJ  CL  CN
=CP  CR
=CT
=CV
=CX
=CZ  C\
=C^
=C`  Ca��Cc��Cf  Ch  Cj  Cl  Cn  Cp  Cr
=Ct
=Cv  Cx  Cz
=C|  C~  C�C�C�  C�  C�C�C�  C�  C�  C�  C���C���C���C���C���C�  C�  C�C�  C�  C���C�  C�C�  C���C�  C�C�C�  C�C�
=C�C���C�  C�C���C���C���C���C���C���C���C�  C�  C���C�  C�  C�C�
=C�C�C�C�  C���C���C�  C�C�  C���C���C���C�  C�C�  C�  C���C�  C�  C�  C���C�  C�  C�  C�  C�C�  C�  C�  C�C�C�  C���C���C�  C���C�  C�  C�C�C�  C���C���C�  C�  C���C���C���C�  C���C�  C�C�  C���C���C�  C�  C�  C���C���C�  C�  C�C�C�  C�  C�C�C�  C�  C�C�  C�  C���C���C�  C�  C���C���C���D }qD �qD}qD  D��D  D� D�D� D  D� D�qD� D  D� D�D��D	�D	��D
�D
��D  D}qD�qD}qD  D��D  D}qD  D��D�D� D  D� D�qD}qD�qD� D�D��D�D� D  D��D�D��D�D� D�qD}qD  D}qD  D� D�qD� D  D}qD  D� D  D� D   D }qD �qD!� D"  D"��D#�D#}qD$  D$��D%  D%� D&  D&� D'�D'��D(�D(��D)  D)}qD*  D*� D+  D+� D,  D,� D-  D-� D.�D.��D/�D/��D0  D0��D1  D1� D2  D2��D3�D3��D4�D4��D5�D5��D6  D6� D7  D7� D8  D8� D9  D9}qD:  D:��D;�D;� D<  D<}qD=  D=� D>  D>}qD>�qD?� D@  D@� DA�DA��DB�DB��DC�DC� DD  DD��DE�DE��DF  DF� DG  DG� DH  DH� DI  DI� DI�qDJ}qDJ�qDK� DL�DL� DL�qDM� DN  DN� DO  DO��DP  DP}qDP�qDQ� DR  DR}qDR�qDSz�DS�qDT}qDU  DU� DV  DV��DW�DW�DX  DX� DY  DY}qDZ  DZ}qDZ�qD[� D\  D\� D]  D]� D]�qD^� D_  D_��D_�qD`��Da�Da� Db  Db}qDb�qDc}qDd  Dd}qDe  De��Df  Df}qDf�qDg��Dh  Dh}qDi  Di� Dj  Dj��Dk  Dk� Dl  Dl� Dl�qDm� Dn  Dn��DoDo��Dp  Dp� Dq�Dq�DrDr��Ds�Ds� Dt�Dt��Du  Du� Du�qDv� Dw�Dw� Dw�qDx� Dx�qDy}qDy�qDz}qD{  D{��D|  D|� D}  D}� D~  D~}qD  D� D�qD�@ D�� D���D���D�@ D�� D�� D�HD�AHD�� D�� D�HD�@ D�� D��HD�HD�@ D�� D���D�  D�@ D�� D��HD���D�@ D�� D�� D�HD�AHD��HD��HD�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D�~�D�� D�  D�>�D��HD��HD�  D�AHD�� D�� D�  D�>�D�� D�� D���D�>�D�� D�� D�  D�AHD��HD��HD�HD�>�D�� D��HD�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��HD��HD���D�>�D�~�D�� D�  D�>�D�~�D�� D��qD�>�D��HD��HD�  D�>�D�~�D��HD�HD�@ D��HD�� D���D�>�D�� D�� D���D�>�D��HD��HD�  D�@ D�� D���D�  D�AHD��HD��HD�  D�@ D�� D���D�  D�@ D��HD�� D���D�AHD�� D�� D�HD�@ D�~�D�� D���D�@ D�� D�� D�  D�@ D���D�D�HD�AHD�� D���D���D�>�D�� D�� D���D�>�D�� D�� D���D�@ D��HD��HD���D�>�D�� D�� D��qD�>�D�~�D�� D�HD�AHD�~�D���D���D�AHD�� D�� D�HD�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�~�D���D���D�@ D�� D��HD�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D��HD���D���D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D���D�>�D D��HD�  D�@ DÀ Dþ�D���D�AHDāHD�� D�  D�AHDŀ D�� D�  D�@ D�~�Dƾ�D���D�@ DǁHD��HD�  D�>�DȀ D�� D�  D�>�D�~�D��HD�  D�@ D�~�Dʾ�D�  D�>�D�~�D˾�D���D�@ D�~�D̾�D���D�>�D̀ D;�D���D�@ D΁HD��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�AHDсHD�� D�  D�@ DҁHD�� D�HD�@ DӀ D��HD�  D�>�DԀ D�� D�  D�@ DՀ Dվ�D���D�@ Dր D�� D�HD�@ D׀ D�� D�  D�@ D؀ D�� D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�@ Dۀ D۾�D���D�@ D܀ DܽqD�  D�AHD݀ Dݾ�D�HD�@ Dހ D޾�D���D�>�D߁HD�� D�  D�@ D��HD�� D�  D�AHDႏD��HD�HD�AHD�HD��HD�HD�@ D� D��HD�  D�>�D� D�� D�  D�AHD� D��HD�HD�@ D�~�D澸D�  D�@ D� D�� D�  D�@ D�HD��HD�  D�AHD�HD�� D�  D�>�D�~�D꾸D���D�@ D� D�� D�  D�>�D� D�� D���D�>�D�~�D���D�  D�@ D� D�� D�  D�@ D�HD��HD�  D�>�D�� D�� D�  D�AHD� D�D���D�>�D� D��HD�HD�AHD�D�D�  D�@ D� D�� D�  D�@ D��HD��HD�  D�@ D�� D���D���D�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�~�D�� D�  D�AHD��HD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AWhsAWx�AWx�AW|�AW|�AW|�AW|�AW|�AW�AW�AW�AW|�AW|�AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW�AW��AW��AW�
AW�
AW�mAW�mAW�AW�AW�mAW��AX  AW��AW��AW��AW��AW��AW��AW��AW�AW�AW�AW�AW�AW�AW�AW�mAW�TAW�;AW�;AW�mAW�^AW��AW�PAW�PAW�AW�AW?}AW+AW"�AW"�AW"�AW�AW�AW�AW�AWoAV��AV�9AV��AV��AV�\AV�uAV�uAV��AV��AV��AV��AV��AV�uAV�DAV~�AVv�AVjAV=qAV5?AVbAUƨAU�^AU�^AU�AU��AU�PAUt�AU33AU�AT5?AR�jAQ�AP�+ANz�AM+AL^5AK/AIS�AH�AH��AH��AGx�AFr�AE�AD�AC�#AC�AC�-ACl�AB�AB��ABr�AA�AA�^AA�FAA�PAAoA@��A@9XA?��A?�A?dZA>��A>(�A=�A<��A<r�A<A;�hA;p�A;�A:�uA:ZA:�A9�wA9�A9+A8JA61A5oA3�A3�7A2��A2�A1��A1�;A1�PA1C�A0��A0��A0��A1;dA1C�A0��A/x�A.�A-�FA,��A,��A,z�A,E�A,bA+�wA+dZA+�A*�\A*^5A*A�A)�A)S�A(�!A'�-A&-A%��A$9XA#7LA#+A"��A"��A"E�A!�mA!��A!�A!oA�#A�PA��A��A��A�DAE�AƨA�A/A�yA��A �A�7A��A��AA�A�A��A��A?}A�A�/AĜA{A�A��A&�A�Ar�AZAA�AA�+AA&�AĜA9XA�wA33Av�A5?AA�PAVA�/Ar�A(�A�wAt�A;dA
�/A
�RA
^5A
=qA
9XA
A�A
A�A
$�A	�A	A	�7A	C�A��A��A1A�wA�A��Az�A��A9XA{A�TA�-A��A�7A�A�9A��A��Ar�AZAJAp�A bN@�l�@�ȴ@�{@��@�S�@�ȴ@��7@�A�@��R@�@�1'@��#@�"�@�@�@��;@ꟾ@�D@�ƨ@��@�7@��H@�@���@߶F@�dZ@�;d@�=q@�K�@�&�@ץ�@�@ӕ�@җ�@��@�j@�1'@��`@�"�@�n�@ɺ^@�  @�C�@�|�@��@��j@�1@���@�C�@�-@���@��^@��7@��@� �@��@��@��@���@�S�@��@���@��@���@���@�X@�&�@��`@�  @��^@�K�@��+@��@�1@�n�@��^@�hs@��9@��
@�dZ@�K�@�@��H@���@���@��R@�^5@�@�b@��@�-@��@�V@���@�x�@�&�@�j@�|�@��@���@���@��@��@���@�Q�@��@�b@��@��F@�\)@���@�@�Q�@���@�@��-@�hs@�hs@�hs@��@�hs@�?}@��@��j@��m@�;d@�v�@��h@�\)@���@���@���@��@��/@�n�@���@�I�@��!@�A�@�n�@��F@�1'@���@�ƨ@��@�|�@�33@��@��@��@���@�ff@��@�@��@���@��D@�j@�1'@��m@�dZ@�
=@�~�@��@�7L@���@�Ĝ@�bN@� �@���@��@�t�@�l�@�S�@�K�@�+@�
=@���@��@���@�v�@�=q@�$�@�{@��7@��/@�Z@�l�@�+@�@��R@��@��7@�O�@��`@��@���@���@�X@���@�
=@���@�-@��@��#@�V@�bN@�  @�t�@�K�@�;d@�@�~�@�M�@��#@���@���@���@���@�hs@�/@�/@�/@�7L@�G�@�`B@�G�@��@��/@�9X@�(�@�P@~�y@}�@|��@|Z@|�@{ƨ@{o@z�H@z�!@z�!@z��@z�\@z~�@z�@{S�@y�@zJ@z-@y�^@xQ�@x  @w�@w+@vv�@u�T@u��@u/@t�@t�j@t�D@tZ@s�m@st�@sS�@sC�@s@s@r�H@q��@q�^@qX@pbN@o�P@n�@n��@n�@n�y@n�@n��@m�@k��@h  @g�P@e�@c��@cƨ@b��@b^5@b�!@e�@f�+@g�@i�@i��@ihs@i7L@i&�@i7L@i7L@iG�@i7L@i�@h�9@hQ�@hA�@hA�@g�@g��@gl�@g;d@g+@f�y@f��@f{@e`B@d�@dI�@d�@d(�@c�F@c33@`��@^{@^@]�@[�F@Z��@Y�^@W\)@V��@Vv�@S��@SdZ@SS�@S�@S�@SdZ@TZ@XA�@YX@Yhs@X��@XĜ@X�u@Xr�@XbN@X �@W�@W�;@W�w@W��@W�P@W\)@V��@V�y@V�@Vȴ@Vȴ@Vȴ@V�R@V��@Vv�@V5?@V5?@V$�@V$�@U�T@U��@Up�@UO�@U�@T��@TI�@S�F@SS�@R��@R~�@R�@Q��@Q�^@Qx�@QG�@Q&�@P�`@PĜ@P�9@P�@Pb@Ol�@Nȴ@N5?@M�@M�T@M��@M�-@M`B@L��@L��@L��@L�D@L�D@Lz�@Lj@LI�@K��@Kt�@Ko@J��@J��@J��@J��@J�\@J~�@Jn�@J-@I��@I��@Ihs@H�`@H��@H�9@HQ�@H1'@H1'@H �@H  @Gl�@F��@F��@FE�@FE�@F5?@F{@F@F@E�T@E�h@D��@D�@Dj@Dj@Dz�@DI�@D1@C�m@CC�@B�H@B��@B��@B�!@B�\@Bn�@B=q@B�@A��@A�@A��@A�7@Ahs@AX@AG�@A&�@@��@@A�@@ �@?�;@?�P@?;d@?+@?�@>��@>�@>�R@>ȴ@>�R@>��@>v�@>V@>ff@>ff@>ff@>V@=�@=��@=�@=�@=O�@=?}@=?}@=�@<�/@<�/@<�j@<��@<z�@<9X@<(�@;�m@;��@;t�@;t�@;t�@;dZ@;"�@:�H@:��@:�\@:�\@:�\@:�\@:n�@:=q@:�@9�@9��@9��@9�7@9�7@9�7@9x�@9x�@9x�@9x�@9x�@9X@9&�@9�@9&�@9&�@9&�@9%@8�`@8�`@8Ĝ@8A�@7�@7��@7�P@7�P@7��@7��@7�P@7�P@7�P@7|�@7+@6�y@6�@6��@6V@6$�@5�@5�T@5��@5��@5@5�-@5�h@5�h@5�@5p�@5V@4�@4��@4��@4�D@4�D@4Z@49X@3��@3�m@3�
@3��@3t�@3S�@3S�@3C�@333@3o@2�@2��@2��@2��@2��@2�!@2�!@2��@2�\@2~�@2^5@2M�@2M�@2=q@2J@1�#@1�^@1�^@1�^@1�^@1�^@1��@1�7@1X@17L@1&�@1&�@1&�@1�@0��@0��@0�9@0�u@0�@0r�@0Q�@0A�@0 �@0b@0  @/�;@/��@/�w@/�@/��@/�P@/|�@/l�@/K�@/+@/�@/�@/
=@/
=@.��@.�@.��@.v�@.v�@.v�@.ff@.E�@.$�@.$�@.$�@.$�@.{@.{@-�T@-��@-@-��@-�h@-p�@-`B@-`B@-`B@-O�@-?}@-?}@-/@-�@-V@-V@-V@-V@,��@,�/@,��@,��@,�j@,�@,z�@,Z@,I�@,9X@,�@,1@+�m@+�F@+��@+��@+t�@+S�@+o@+@+@*��@*��@*~�@*^5@*M�@*-@)��@)��@)��@)��@)�7@)�7@)x�@)hs@)X@)G�@)7L@)&�@(�`@(��@(Ĝ@(��@(�@(�@(�@(�@(�@(�@(Q�@(1'@(b@( �@(b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AWhsAWx�AWx�AW|�AW|�AW|�AW|�AW|�AW�AW�AW�AW|�AW|�AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW��AW�AW��AW��AW�
AW�
AW�mAW�mAW�AW�AW�mAW��AX  AW��AW��AW��AW��AW��AW��AW��AW�AW�AW�AW�AW�AW�AW�AW�mAW�TAW�;AW�;AW�mAW�^AW��AW�PAW�PAW�AW�AW?}AW+AW"�AW"�AW"�AW�AW�AW�AW�AWoAV��AV�9AV��AV��AV�\AV�uAV�uAV��AV��AV��AV��AV��AV�uAV�DAV~�AVv�AVjAV=qAV5?AVbAUƨAU�^AU�^AU�AU��AU�PAUt�AU33AU�AT5?AR�jAQ�AP�+ANz�AM+AL^5AK/AIS�AH�AH��AH��AGx�AFr�AE�AD�AC�#AC�AC�-ACl�AB�AB��ABr�AA�AA�^AA�FAA�PAAoA@��A@9XA?��A?�A?dZA>��A>(�A=�A<��A<r�A<A;�hA;p�A;�A:�uA:ZA:�A9�wA9�A9+A8JA61A5oA3�A3�7A2��A2�A1��A1�;A1�PA1C�A0��A0��A0��A1;dA1C�A0��A/x�A.�A-�FA,��A,��A,z�A,E�A,bA+�wA+dZA+�A*�\A*^5A*A�A)�A)S�A(�!A'�-A&-A%��A$9XA#7LA#+A"��A"��A"E�A!�mA!��A!�A!oA�#A�PA��A��A��A�DAE�AƨA�A/A�yA��A �A�7A��A��AA�A�A��A��A?}A�A�/AĜA{A�A��A&�A�Ar�AZAA�AA�+AA&�AĜA9XA�wA33Av�A5?AA�PAVA�/Ar�A(�A�wAt�A;dA
�/A
�RA
^5A
=qA
9XA
A�A
A�A
$�A	�A	A	�7A	C�A��A��A1A�wA�A��Az�A��A9XA{A�TA�-A��A�7A�A�9A��A��Ar�AZAJAp�A bN@�l�@�ȴ@�{@��@�S�@�ȴ@��7@�A�@��R@�@�1'@��#@�"�@�@�@��;@ꟾ@�D@�ƨ@��@�7@��H@�@���@߶F@�dZ@�;d@�=q@�K�@�&�@ץ�@�@ӕ�@җ�@��@�j@�1'@��`@�"�@�n�@ɺ^@�  @�C�@�|�@��@��j@�1@���@�C�@�-@���@��^@��7@��@� �@��@��@��@���@�S�@��@���@��@���@���@�X@�&�@��`@�  @��^@�K�@��+@��@�1@�n�@��^@�hs@��9@��
@�dZ@�K�@�@��H@���@���@��R@�^5@�@�b@��@�-@��@�V@���@�x�@�&�@�j@�|�@��@���@���@��@��@���@�Q�@��@�b@��@��F@�\)@���@�@�Q�@���@�@��-@�hs@�hs@�hs@��@�hs@�?}@��@��j@��m@�;d@�v�@��h@�\)@���@���@���@��@��/@�n�@���@�I�@��!@�A�@�n�@��F@�1'@���@�ƨ@��@�|�@�33@��@��@��@���@�ff@��@�@��@���@��D@�j@�1'@��m@�dZ@�
=@�~�@��@�7L@���@�Ĝ@�bN@� �@���@��@�t�@�l�@�S�@�K�@�+@�
=@���@��@���@�v�@�=q@�$�@�{@��7@��/@�Z@�l�@�+@�@��R@��@��7@�O�@��`@��@���@���@�X@���@�
=@���@�-@��@��#@�V@�bN@�  @�t�@�K�@�;d@�@�~�@�M�@��#@���@���@���@���@�hs@�/@�/@�/@�7L@�G�@�`B@�G�@��@��/@�9X@�(�@�P@~�y@}�@|��@|Z@|�@{ƨ@{o@z�H@z�!@z�!@z��@z�\@z~�@z�@{S�@y�@zJ@z-@y�^@xQ�@x  @w�@w+@vv�@u�T@u��@u/@t�@t�j@t�D@tZ@s�m@st�@sS�@sC�@s@s@r�H@q��@q�^@qX@pbN@o�P@n�@n��@n�@n�y@n�@n��@m�@k��@h  @g�P@e�@c��@cƨ@b��@b^5@b�!@e�@f�+@g�@i�@i��@ihs@i7L@i&�@i7L@i7L@iG�@i7L@i�@h�9@hQ�@hA�@hA�@g�@g��@gl�@g;d@g+@f�y@f��@f{@e`B@d�@dI�@d�@d(�@c�F@c33@`��@^{@^@]�@[�F@Z��@Y�^@W\)@V��@Vv�@S��@SdZ@SS�@S�@S�@SdZ@TZ@XA�@YX@Yhs@X��@XĜ@X�u@Xr�@XbN@X �@W�@W�;@W�w@W��@W�P@W\)@V��@V�y@V�@Vȴ@Vȴ@Vȴ@V�R@V��@Vv�@V5?@V5?@V$�@V$�@U�T@U��@Up�@UO�@U�@T��@TI�@S�F@SS�@R��@R~�@R�@Q��@Q�^@Qx�@QG�@Q&�@P�`@PĜ@P�9@P�@Pb@Ol�@Nȴ@N5?@M�@M�T@M��@M�-@M`B@L��@L��@L��@L�D@L�D@Lz�@Lj@LI�@K��@Kt�@Ko@J��@J��@J��@J��@J�\@J~�@Jn�@J-@I��@I��@Ihs@H�`@H��@H�9@HQ�@H1'@H1'@H �@H  @Gl�@F��@F��@FE�@FE�@F5?@F{@F@F@E�T@E�h@D��@D�@Dj@Dj@Dz�@DI�@D1@C�m@CC�@B�H@B��@B��@B�!@B�\@Bn�@B=q@B�@A��@A�@A��@A�7@Ahs@AX@AG�@A&�@@��@@A�@@ �@?�;@?�P@?;d@?+@?�@>��@>�@>�R@>ȴ@>�R@>��@>v�@>V@>ff@>ff@>ff@>V@=�@=��@=�@=�@=O�@=?}@=?}@=�@<�/@<�/@<�j@<��@<z�@<9X@<(�@;�m@;��@;t�@;t�@;t�@;dZ@;"�@:�H@:��@:�\@:�\@:�\@:�\@:n�@:=q@:�@9�@9��@9��@9�7@9�7@9�7@9x�@9x�@9x�@9x�@9x�@9X@9&�@9�@9&�@9&�@9&�@9%@8�`@8�`@8Ĝ@8A�@7�@7��@7�P@7�P@7��@7��@7�P@7�P@7�P@7|�@7+@6�y@6�@6��@6V@6$�@5�@5�T@5��@5��@5@5�-@5�h@5�h@5�@5p�@5V@4�@4��@4��@4�D@4�D@4Z@49X@3��@3�m@3�
@3��@3t�@3S�@3S�@3C�@333@3o@2�@2��@2��@2��@2��@2�!@2�!@2��@2�\@2~�@2^5@2M�@2M�@2=q@2J@1�#@1�^@1�^@1�^@1�^@1�^@1��@1�7@1X@17L@1&�@1&�@1&�@1�@0��@0��@0�9@0�u@0�@0r�@0Q�@0A�@0 �@0b@0  @/�;@/��@/�w@/�@/��@/�P@/|�@/l�@/K�@/+@/�@/�@/
=@/
=@.��@.�@.��@.v�@.v�@.v�@.ff@.E�@.$�@.$�@.$�@.$�@.{@.{@-�T@-��@-@-��@-�h@-p�@-`B@-`B@-`B@-O�@-?}@-?}@-/@-�@-V@-V@-V@-V@,��@,�/@,��@,��@,�j@,�@,z�@,Z@,I�@,9X@,�@,1@+�m@+�F@+��@+��@+t�@+S�@+o@+@+@*��@*��@*~�@*^5@*M�@*-@)��@)��@)��@)��@)�7@)�7@)x�@)hs@)X@)G�@)7L@)&�@(�`@(��@(Ĝ@(��@(�@(�@(�@(�@(�@(�@(Q�@(1'@(b@( �@(b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�!B�'B�'B�'B�'B�'B�'B�-B�-B�-B�-B�-B�3B�3B�9B�FB�RB�LB�FB�FB�FB�?B�?B�3B�-B�-B�-B�-B�-B�-B�-B�-B�-B�!B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�JB�Bz�Bq�BffB^5BZBW
BT�BS�BR�BP�BI�BC�B=qB:^B8RB;dB=qB=qB:^B9XB7LB49B33B2-B1'B.B)�B&�B$�B"�B�B�BuBhB	7B%BB  B��B��B��B�B�B�B�B�B�HB��BŢB�qB�^B�3B�B�B�B��B��B��B��B��B�B�!B��B��B��B�DB�B�B� B}�B{�Bw�Bs�Bq�Bl�BjBiyBdZB^5BVBK�B<jB5?B(�B�B�B�B�B�BuBhB\BDBBB
��B
��B
�B
�B
�B
�sB
�`B
�TB
�NB
�BB
�/B
�B
��B
��B
��B
��B
��B
��B
ȴB
��B
��B
ȴB
ǮB
ǮB
ƨB
ĜB
ǮB
ȴB
ȴB
ȴB
ŢB
�XB
�9B
�!B
�!B
�B
�B
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
�bB
�\B
�JB
�JB
�DB
�DB
�DB
�JB
�PB
�PB
�PB
�JB
�=B
�7B
�+B
�B
�B
�B
� B
|�B
x�B
q�B
n�B
m�B
l�B
jB
jB
iyB
gmB
ffB
ffB
ffB
e`B
dZB
bNB
_;B
YB
T�B
R�B
O�B
K�B
H�B
F�B
C�B
?}B
;dB
8RB
2-B
,B
%�B
"�B
�B
�B
�B
{B
hB
PB
	7B
B	��B	��B	��B	�B	�B	�B	�ZB	�5B	�B	��B	��B	ɺB	��B	�wB	�dB	�!B	��B	��B	��B	��B	�+B	z�B	u�B	s�B	q�B	o�B	n�B	l�B	k�B	jB	iyB	ffB	cTB	bNB	^5B	ZB	YB	XB	W
B	VB	T�B	S�B	R�B	Q�B	Q�B	O�B	K�B	F�B	C�B	C�B	B�B	?}B	;dB	9XB	8RB	6FB	49B	33B	33B	33B	49B	49B	33B	33B	49B	5?B	2-B	/B	-B	-B	-B	,B	33B	49B	2-B	/B	.B	0!B	1'B	0!B	/B	.B	-B	-B	-B	.B	/B	0!B	.B	.B	,B	+B	,B	,B	/B	0!B	1'B	2-B	33B	2-B	2-B	2-B	0!B	/B	1'B	/B	-B	0!B	49B	C�B	N�B	YB	cTB	hsB	r�B	� B	�1B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�9B	�3B	�?B	�LB	�RB	�XB	�^B	�^B	�dB	�qB	�qB	�wB	�wB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	��B	B	ĜB	ŢB	ĜB	ĜB	ǮB	ɺB	ɺB	ȴB	ǮB	ǮB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�HB	�NB	�TB	�ZB	�ZB	�`B	�HB	�/B	�)B	�B	�B	�B	�B	�B	�5B	�sB	�B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
+B
1B
	7B

=B
DB
JB
PB
PB
PB
VB
bB
hB
hB
hB
hB
hB
oB
oB
bB
bB
\B
VB
PB
JB

=B
1B
+B
%B
B
%B
+B
1B
1B
	7B
bB
�B
"�B
"�B
&�B
'�B
'�B
(�B
(�B
+B
-B
.B
/B
/B
0!B
1'B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
:^B
:^B
;dB
;dB
=qB
=qB
=qB
?}B
A�B
A�B
D�B
F�B
F�B
H�B
I�B
J�B
J�B
K�B
L�B
M�B
M�B
N�B
O�B
O�B
O�B
P�B
R�B
S�B
VB
W
B
W
B
W
B
XB
YB
[#B
[#B
\)B
]/B
]/B
]/B
]/B
^5B
`BB
bNB
cTB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
hsB
iyB
iyB
jB
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
q�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
y�B
{�B
{�B
|�B
|�B
}�B
� B
� B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�1B
�7B
�7B
�=B
�=B
�DB
�DB
�JB
�JB
�PB
�\B
�\B
�bB
�hB
�uB
�uB
�uB
�{B
�{B
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
�B
�B
�B
�B
�B
�!B
�'B
�'B
�'B
�-B
�3B
�9B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�FB
�XB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�qB
�qB
�wB
�wB
��B
��B
��B
��B
��B
��B
��B
B
ÖB
B
ÖB
ÖB
ƨB
ǮB
ǮB
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
�B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�)B
�)B
�/B
�/B
�5B
�BB
�BB
�HB
�HB
�HB
�HB
�NB
�NB
�ZB
�ZB
�`B
�`B
�fB
�fB
�mB
�mB
�sB
�yB
�yB
�yB
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
��B  B  BB  BBBBBBBBBBBBBBBB%B+B+B1B1B	7B	7B
=BDBDBJBJBPBVBVB\B\BbBbBhBhBoBuBuB{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�B�B� B��B�B�-B�"B� B�#B�/B�B�5B�1B�)B�6B�*B�.B�.B�8B�9B�>B�BB�;B�7B��B��B�lB�JB�XB�YB��B�pB�EB�/B�/B�:B�9B�7B�"B�FB�rB��B�>B�!B�"B��B�B��B�B��B�	B�B�)B�B�#B�B�B�jB�B�HB��B��B��B��B��B��B��B�FB��B��B�:B�$B~*Bw4Bj B`�B]�B[�BV-BS�BTBTBL�BF.B?.B=OB8HB<B>3B>�B;OB9�B8�B4�B3HB2�B2wB/jB+B'�B%B$^B eB[BABoB
]B\BNB sB��B�WB�lB�tB�B�QB�B��B��B��BȳB��B�lB�-B�dB�RB��B��B�B��B�dB�<B�B�B�
B�zB��B��B��B�`B��B~�B|�Bx�Bt�Bs,Bm)Bj�BjdBfB`BX�BPB>SB9SB+�BB?BbB�B�B6B�B�B�BB�B�B
�6B
�nB
�oB
��B
�BB
�MB
�"B
�HB
�B
��B
ڰB
ԻB
�]B
��B
�MB
�mB
��B
ɣB
�B
�B
ʪB
��B
��B
�B
�	B
�XB
��B
�B
ɗB
��B
��B
��B
�^B
��B
��B
��B
�B
��B
�xB
�(B
�1B
�RB
��B
�{B
��B
�TB
�B
�tB
��B
�TB
��B
�SB
�6B
�RB
��B
��B
��B
��B
�B
�B
�*B
��B
��B
��B
��B
~�B
}�B
sfB
oB
n'B
mB
j�B
j�B
j�B
h�B
f�B
f�B
f�B
e�B
eLB
d+B
b>B
[B
VB
T!B
R�B
L�B
I�B
H�B
E�B
A�B
<�B
;B
5�B
/�B
'�B
$�B
 	B
�B
�B
�B
�B
fB
B
�B	�tB	�sB	�NB	�B	�>B	��B	�B	��B	��B	�BB	�iB	�&B	�XB	�B	�B	��B	�)B	�B	��B	�hB	��B	~(B	wB	t�B	rhB	p:B	pDB	m[B	k�B	j�B	j4B	g�B	c�B	dB	aiB	[DB	ZB	X�B	W�B	V�B	U�B	TGB	SkB	RJB	RfB	Q`B	OfB	JLB	D�B	D�B	EoB	A�B	<�B	9�B	9rB	7�B	4�B	3pB	3�B	3vB	4[B	4ZB	3SB	3�B	5HB	7�B	3�B	0�B	.'B	-�B	-�B	*�B	3�B	5aB	3�B	0'B	.CB	0tB	2NB	1rB	/�B	.�B	-pB	-,B	-RB	.�B	/�B	1JB	/'B	0�B	.�B	,B	,�B	,�B	/(B	0.B	1B	2`B	3~B	2xB	2�B	3zB	1;B	0lB	2�B	2jB	-�B	0LB	1B	@B	LB	V�B	b�B	e�B	oB	}yB	��B	��B	��B	�B	�B	��B	�B	�7B	��B	�B	��B	�B	�hB	�RB	�kB	��B	�(B	�uB	�B	�5B	�QB	��B	�rB	��B	��B	��B	�HB	�.B	�uB	�LB	�dB	�TB	�B	��B	�	B	��B	�B	�B	� B	�B	�B	�TB	�AB	�B	�B	��B	��B	��B	�]B	�fB	�BB	�uB	�/B	��B	�gB	��B	��B	��B	��B	�B	�0B	��B	��B	�B	��B	�hB	�xB	�QB	��B	�.B	��B	��B	��B	�<B	��B	�&B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	�wB	�rB	��B	��B	�B	ŢB	��B	�IB	�LB	��B	�HB	�B	��B	��B	�:B	��B	��B	ǷB	��B	��B	��B	�tB	͜B	��B	��B	��B	�6B	��B	�B	��B	�oB	�nB	�ZB	�*B	�[B	�;B	�.B	�4B	�8B	�gB	�lB	�.B	�&B	�KB	�B	�?B	��B	�\B	�{B	��B	��B	��B	�nB	�(B	�HB	�iB	�B	�TB	��B	��B	ݿB	��B	�PB	�YB	��B	�|B	��B	�|B	�YB	�B	�B	�lB	�&B	�'B
 B	��B
B
B
.B
?B
sB
yB
@B
:B
	|B

�B
sB
xB
bB
�B
�B
�B
�B
�B
�B
�B
iB
�B
�B
NB
yB
�B
!B
yB
1B
!B

B
�B
�B
B
�B
5B
B
6B
FB
vB
~B
�B
"�B
#5B
'B
(B
(B
)B
)4B
+1B
-!B
.5B
/:B
/0B
0MB
1~B
4KB
5QB
5PB
5FB
6KB
6[B
6[B
7vB
8�B
:aB
:rB
;kB
;�B
=�B
=�B
=�B
?�B
A�B
BB
EB
F�B
GB
H�B
J
B
J�B
J�B
K�B
L�B
M�B
NB
N�B
O�B
PB
PFB
QnB
StB
TvB
V;B
WB
WB
W,B
XZB
YlB
[JB
[PB
\9B
]2B
]?B
]@B
]NB
^~B
`�B
b�B
c�B
eaB
e[B
fuB
f�B
fwB
fxB
f�B
h�B
i�B
i�B
j�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
pB
rB
t�B
vB
v�B
v�B
v�B
v�B
v�B
v�B
xB
xJB
z%B
|B
{�B
|�B
}B
~.B
�B
��B
�^B
�,B
�.B
�1B
�BB
�CB
�UB
�OB
�SB
�JB
�pB
�XB
�\B
�SB
�YB
�lB
��B
��B
�|B
��B
��B
��B
��B
��B
��B
��B
��B
�zB
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
� B
�B
��B
��B
��B
��B
�B
�B
�B
�B
��B
��B
��B
�B
�)B
�B
�(B
�!B
�1B
�B
�B
�B
�)B
�%B
�,B
�)B
�.B
�IB
�ZB
�IB
�6B
�@B
�BB
�[B
�[B
�GB
�fB
��B
��B
��B
�tB
�fB
�ZB
�fB
�sB
�fB
�gB
�zB
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
¨B
ÖB
¤B
çB
��B
��B
��B
��B
��B
ɸB
��B
��B
��B
��B
��B
�B
�	B
�B
��B
�B
�	B
�B
� B
�(B
�!B
�B
�B
�%B
�B
�*B
�,B
�0B
�;B
�8B
�1B
�?B
�^B
�bB
�ZB
�HB
�HB
�MB
�KB
�eB
�\B
�{B
�pB
�gB
�bB
�dB
�sB
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
�B
�B
�B
�B
�B
�B
��B
��B
�B
�B
��B
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
�B
��B
�B
�B
�B
�B B BB BBBBBBBBB#B0B'BB/B/BKB@B:B;BIB@B	RB	bB
KBOBeBgB�B_BXB�B�BzBzBuB�B�B�BxB�B�B}B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�<#ף<#�$<#�<#�<#�<#�<#�<#�c<#�X<#�0<#�<#�<#��<#�<#�
<#�
<#�X<#�
<#�<<#�{<#�X<#�X<#�i<#�
<#�<#׎<#�<#�
<#؄<#�<#�$<#�<#�<#��<#�&<#�i<#�i<#�<#�<#�c<#�{<#�<#�<#�<#�<<#�X<#ף<#�X<#�<#׺<#�<#�<#�<#�i<#�{<#�i<#׺<#�<#׺<#��<#�N<#�r<#�<#�<#�<$+<#�e<#��<#�<#�<#׎<#�{<#�X<#�i<#��<#�<$=<#�<#��<#�*<#�i<#�<#�I<#�<#؄<#�<#�&<#�]<#�<#�]<#؄<#�r<$p<#�]<#��<$H�<#�l<#�
<#��<#�<#��<#�<$1:<#��<(��</��<+�<,�<:F<.Z)<(4<-<5��<$�J<#�<$ѩ<+Q]<*
c<)�<&/<*w<#�X<$%<$I�<%<$��<$�<%`�<$�<#�c<$�<%.+<%:{<$ʾ<$1:<#�<%�L<&�<&L�<$U�<*ٜ<$��<$�-<$�k<#�a<$y�<%rN<$5w<$L<$�w<$2G<$�<+v�<:<*S�<+><%G<'<&�A<#��<#�<$v�<$o�<$��<#�<#�a<$2G<#�X<&�,<+	<&L�<,sq<)K?<$Z<#��<$�<$$<$t <$�<$g�<%�M<$#(<#�<$<<&A�<&y�<*9�<1�M<&��<0Z�<*<#��<$�<$<<%<�<$�R<$H�<$�<%6Z<,ix<$�Q<&4p<*{�<,��<$Sa<$c�<%\\<$Y�<$�<$XX<$�X<%Z2<&y<%�y<$J�<%Z2<$v�<$p<$+<$��<$� <#�<#�<&�,<$��<#�<%t�<%k�<$/%<#�4<#�<$t <1�z<(\,<&D�<%�<%͍<%�J<%�!<'�<$aD<$&<%I<%y<$+<$�J<$z�<% �<$n�<$=<$Ş<$r<$�k<#��<#׺<#ף<#�<<#�5<$"2<$G<$3U<$^�<$t <$�<&W�<$}�<$=<%��<%��<5*?<&,f<$p<$�<$
<#�<#�<$�-<$�-<#�<#��<#��<#��<$�b<&��<*ǂ<&��<$��<$�J<)Ɩ<$��<$z�<&�A<&�/<'��<%b<)�5<-T*</`�<&W�<&�<%04<&�U<*�<%S<%{@<'4l<.�X<&ke<%�L<%��<$�<#��<%�6<1�z<,nt<(b<*F�<,2�<%�<(\,<$Y�<$%<47a<)�<$��<$�t<)�N<D�`<<MZ<,�<%"<$��<$E<$!><&e<$Z�<#�
<#�N<$A�<%v�<$ <&7"<+�)<$��<$�<$*<$�<$Y�<$�<#�<$�<#��<$p<%��<-�G<-�<%Oz<$�;<*,�<(_�<$�J<$Z<$�h<%B�<$E<#�e<$p<#�<#ڑ<#�]<#�*<$"2<$��<(�)<%�#<%v�<$�R<$L<$Z<$�(<#��<$�<%t�<$��<#��<#�<$��<%04<$B�<$�<#�W<#��<#�&<#��<$&<$�`<$�2<)#=<)K?<$�2<$�<$v<#׎<#׎<#؄<#��<#�4<#�4<$6�<%(<$�<%04<%�<,.<$}�<#ܯ<+v�<-�<)�e<(�<$	<(��<-��<(��<+�<&�,<$L<#�<#�"<#ޫ<#�<$�<#�<#�M<#�<#��<$�<$	�<$Z<$��<#�"<$)
<#��<#��<$	<$\"<$&<$o�<$�b<$��<#��<#�"<$#(<$ �<$�<$<<#�r<#�<#��<#�D<#�N<#�N<#��<#�e<#�J<$<<#��<#�^<#�l<$r�<$��<$n�<%{@<#�N<#�<$<<%s<$0.<#�)<$2G<$5w<%�d<%�@<&��<'[)<%rN<$.<$��<#�H<#��<%'<$��<$/%<$j|<#�4<#ڑ<#��<$e.<#��<$:�<#�<#�c<#�<#ڑ<#�<#��<#�<<#�<<#�<#�{<#ا<#��<#�<#��<$�<#��<$ K<$
<$�;<$�<#�	<#�<#�<$
<#�<#�<#�I<#�C<#�<#�<#�<#�E<$��<#�0<#׺<#��<$��<#�4<#��<$!><$�<$/<#��<#��<#�e<#�l<#�<#�J<#�m<#�W<#��<#؄<#�<#�<<#��<$A�<#�&<#�<$R'<$5w<$�<#��<#�*<#�&<#�c<#�J<$�X<%��<)3-<$F<%��<% �<#�U<$L<#�<#�<&$h<$�<$�Q<$��<$ �<#ޫ<#�8<#��<#�<#�0<#�<#�c<#�*<#�<#�<#�c<#�I<#�<#�<#��<#�<#�<#��<#��<$	<$
<$�<#�<#��<#�<#��<$<<&�}<'7�<#�8<$MO<$ح<$q@<$c�<&Z�<#�"<$a<&�R<$�<#��<#�o<#�<#�c<$H�<*>'<$�b<#ף<#��<#��<#�+<#��<#��<#��<#��<#�$<#�]<#��<#�c<#��<#�"<#�<#�<#��<#�0<#�<#�c<#�c<#�l<#�<#�<#�C<#�0<#�<#�$<#�<#�+<#�+<#�r<$�<$<<#�<#��<#��<#�<#�]<#�e<#��<#��<#�*<#��<#ٛ<#��<#��<#�g<$G<$
�<$v<#�E<#ף<#�c<#ڑ<#�<#�<#ۮ<#�8<#��<#�<#��<#��<#��<#�M<#�N<#�<#��<#�<#�<#׺<#ܯ<#��<#�<#��<#�8<#�+<#�<#�H<#׺<#�]<#�<#�<#�0<#��<#��<$�<#�	<#�<#�<#�<#��<#�D<#׺<#�0<#�]<#�&<$/<#�<#�<#�<#�0<#�8<#�N<#��<$�<#�<#�$<#׺<#�<#��<#��<#�l<#��<#ڑ<#�$<#��<#�D<#��<#׺<#�c<#ڑ<#��<#�<#�*<#�N<#��<#�&<#ף<#��<#��<#�<#�o<#�0<#�{<#�o<#ٛ<#�o<#�I<#�<#�<#�c<#�<#�<#�*<#�<#��<#ף<#�<<#�*<#��<#�<#�*<#�<#��<#�E<#�<#�<#�<#�o<#�<#�<#؄<#�E<#�E<#ا<#�8<#�<#�<#�<#��<#�8<#�*<#��<#��<#�+<#�{<#�<#�<#ף<#�<#�<#�<#�0<#�o<#ۮ<#��<#�I<#�<#�<#�o<#�o<#�<#�*<#��<#�U<#�U<#��<#�<#�X<#�<#׺<#�<#�<#؄<#�<#ߜ<#ا<#��<#�l<#ۮ<#��<#׎<#��<#�&<#��<#�c<#��<#�
<#�c<#��<#�<#��<#�D<#��<#׺<#�<#�+<#ٛ<#��<#�X<#�<#��<#�l<#�D<#�<#�X<#��<#ٛ<#�o<#��<#��<#�
<#�<#ף<#�<#�$<#׺<#�$<#��<#׺<#�<<#��<#��<#�8<#��<#�&<#�
<#�<#�<#ٛ<#ף<#�8<#؄<#׎<#�<#�<#׎<#�*<#��<#ڑ<#�<#׎<#׺<#ٛ<#��<#ا<#��<#׎<#�o<#׺<#�{<#׺<#�X<#�<#׺<#׎<#��<#ا<#׺<#�<#׺<#�<#�{<#��<#ߜ<#�]<#�<#�<#�X<#��<#�<#�<#�
<#�<#׎<#�<#�+<#ף<#ף<#ٛ<#׎<#��<#�{<#�<#�<#�C<#�{<#�<#�X<#ף<#��<#�
<#�<#�<#��<#��<#ף<#�<#��<#��<#��<#�D<#׺<#��<#��<#׺<#�D<#ܯ<#ף<#�i<#�]<#ٛ<#��<#׺<#�<#ܯ<#�8<#��<#��<#�$<#��<#�<#�l<#�<#��<#�{<#�<#׎<#׺<#�i<#�$<#�X<#�$<#��<#�<#׎<#�o<#�o<#�<#�<#�<#�<#�<#ۮ<#�o<#ٛ<#�{<#��<#�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = CTM_ADJ_PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                              None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment;OW: r =1(+/-0.0001), vertically averaged dS =-0.005(+/-0.002),                                                                                                             SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT.; No significant drift detected in conductivity                                                                                                                                   202205040000002022050400000020220504000000  AO  ARGQQCPL                                                                    20210216204808  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210216204808  QCF$                G�O�G�O�G�O�0               WHOIARSQWHQCV0.5                                                                20211004000000  QC                  G�O�G�O�G�O�                WHOIARSQCTM V1.0                                                                20220503000000  IP                  G�O�G�O�G�O�                WHOIARCAOWC V2.0ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     20220504000000  IP                  G�O�G�O�G�O�                