CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       $Woods Hole Oceanographic Institution   source        
Argo float     history       92021-02-17T02:02:09Z creation; 2022-05-04T12:55:30Z DMQC;      
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
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20210217020209  20220504085530  1901663 US ARGO PROJECT                                                 BRECK OWENS, STEVE JAYNE, P.E. ROBBINS                          PRES            TEMP            PSAL               �A   AO  5137                            2C  D   S2A                             7179                            SBE602 V1.3                     854 @�EOtq$1   @�E�����Dmq�bc@D�m���u1   GPS     Primary sampling: averaged [nominal 2 dbar binned data sampled at 0.5 Hz from a SBE41CP]                                                                                                                                                                           A   A   A   ?��?��H@@  @�  @�G�@�G�@�  A ��A��A\)A?\)A`��A�Q�A�  A�  A�  A�  A�  A�  A�  B   B  B(�B(�B (�B((�B0  B8  B@  BH  BP  BX(�B`  Bh  Bp(�Bx(�B�  B��B�  B�  B�  B�{B�{B��B�  B��B�  B�  B�  B�  B�  B�  B�{B�{B�  B�  B�  B�{B�{B��B��B�  B��B�  B�{B�{B�{B�{C   C  C
=C  C��C	��C
=C
=C  C  C  C
=C  C��C  C  C��C"  C$  C%��C'��C*  C,  C.  C0
=C2
=C4  C6  C8  C:  C<
=C>  C?��CB  CD
=CF
=CH  CJ  CK��CM��CP  CR
=CT  CV  CX  CZ
=C\  C^  C`
=Cb  Cc��Ce��Cg��Ci��Cl  Cm��Co��Cr
=Ct  Cu��Cw��Cy��C|  C~  C�C�  C�  C�C�C���C�  C�C�C�  C�  C�C�  C�  C���C���C�  C�  C�  C�  C�  C���C�  C�  C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�C���C�  C���C�  C�  C���C�  C���C���C�  C�  C�  C�  C�C�  C���C���C���C�  C�  C���C�  C�  C���C�  C�  C�  C�  C�  C�  C�  C�C�C�C�  C�  C�  C�  C�  C�C�C�  C���C���C�  C���C�  C�C���C�  C�C�C�C�C�  C�  C�  C���C�  C���C���C���C�  C�  C�  C�C�  C���C���C�  C�  C�  C�  C���C���C���C���C���C���C���C���C�  C���C�  C�  C�  C���D }qD �qD}qD  D��D  D� D  D� D�D��D�D��D�D� D  D��D	  D	}qD	�qD
� D  D��D  D� D  D}qD  D��D�qD}qD  D}qD�qD� D  D}qD�qD}qD�qD}qD  D��D  D� D�qD}qD  D� D  D��D�D�D�D� D  D� D�qD� D  D}qD�qD� D   D }qD!  D!� D"�D"� D"�qD#��D$D$� D$��D%� D&  D&� D'�D'}qD'�qD(� D(�qD)� D*�D*��D+  D+}qD+�qD,� D-  D-� D.�D.��D/  D/� D0  D0��D1  D1� D2  D2� D3�D3� D3�qD4}qD4�qD5� D6  D6� D7  D7}qD8  D8� D8�qD9}qD:  D:��D;�D;� D;�qD<}qD=�D=��D>�D>� D>�qD?}qD@  D@� DA  DA� DB�DB��DC  DC}qDD  DD� DEDE� DE�qDF}qDF�qDG� DH�DH��DI�DI� DI�qDJ}qDJ�qDK� DL  DL� DL�qDM� DN  DN}qDN�qDO}qDO�qDP� DP�qDQ}qDR  DR��DS  DS��DT  DT� DU  DU� DV  DV� DW�DW� DW�qDX� DY  DY� DY�qDZ}qDZ�qD[� D\  D\}qD]  D]��D^�D^� D_  D_��D`�D`��Da�Da��Db  Db� Dc  Dc� Dd  Dd� Dd�qDe� Df�Df��Dg�Dg� Dh  Dh� Di  Di��Dj�Dj��Dk�Dk� Dk�qDl� Dm  Dm� Dn  Dn}qDn�qDo� Dp�Dp��Dq�Dq� Dr  Dr}qDr�qDs� Dt  Dt}qDu  Du� Dv  Dv��Dw  Dw� Dx�Dx� Dx�qDy� Dz  Dz}qDz�qD{}qD{�qD|}qD}  D}� D~  D~��D  D� D�qD�>�D�~�D�� D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�HD�>�D�� D�� D���D�@ D�� D�� D���D�@ D��HD�� D���D�@ D��HD�� D���D�>�D�� D�� D�HD�@ D��HD��HD�HD�AHD�~�D�� D�HD�@ D�� D��HD�  D�@ D��HD�� D��qD�=qD�� D��HD�HD�AHD�� D���D���D�@ D�� D���D�  D�AHD�~�D�� D�  D�>�D��HD�� D�  D�AHD��HD�� D���D�@ D�� D�� D���D�@ D�� D�� D�  D�AHD�� D��HD��D�AHD�� D�� D�  D�>�D�~�D�� D���D�>�D�� D���D�  D�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D��HD�� D�  D�AHD�� D���D��qD�>�D�� D�� D�  D�>�D�� D��HD�HD�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD�� D�� D�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD��HD�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D��HD�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�@ D��HD�� D�  D�AHD�� D�� D�HD�@ D�� D��HD�  D�>�D�� D��HD�  D�@ D��HD��HD�  D�>�D�~�D�� D���D�>�D��HD��HD�  D�@ D��HD��HD���D�>�D�~�D��HD�HD�@ D�� D���D�  D�AHD��HD�� D�  D�AHD�� D�� D�  D�AHD��HD�� D���D�AHD��HD��HD�  D�@ D�~�D���D���D�>�D D��HD�HD�AHDÀ D��HD�HD�AHDĀ D�� D�  D�@ Dŀ Dž�D���D�@ D�~�Dƾ�D���D�>�DǁHD��HD�HD�AHDȀ D�� D�  D�@ Dɀ D��HD�  D�@ Dʀ D�� D���D�>�Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D�~�D;�D�  D�@ D΀ D�� D���D�@ DρHD��HD�  D�AHDЁHD�� D���D�@ Dр DѾ�D���D�AHDҀ D�� D�  D�@ DӀ D��HD�HD�@ D�~�DԾ�D���D�@ DՁHD��HD�HD�@ Dր D־�D�  D�@ D׀ D��HD�  D�@ D؀ D�� D�  D�@ D�~�D�� D�  D�>�Dڀ D��HD�  D�@ D�~�D۾�D�  D�AHD܁HD�� D���D�>�D�~�D�� D�  D�@ Dހ D��HD�  D�>�D߀ D�� D���D�>�D�� D�� D�  D�AHD�HD��HD�  D�>�D� D�� D�  D�@ D�~�D�� D�  D�B�D� D侸D���D�@ D� D徸D�  D�B�D�HD�� D�  D�@ D� D�� D�  D�AHD� D�� D�  D�AHD� D�� D�HD�@ D�~�D꾸D�  D�@ D� D�� D�HD�AHD�HD��HD�HD�AHD� D���D���D�@ D� D�� D���D�>�D�~�D�� D�  D�@ D�� D�D�  D�AHD�HD�� D�HD�@ D�~�D�D�  D�@ D�~�D�� D�  D�AHD�HD��HD�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�AHD�p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?��?��H@@  @�  @�G�@�G�@�  A ��A��A\)A?\)A`��A�Q�A�  A�  A�  A�  A�  A�  A�  B   B  B(�B(�B (�B((�B0  B8  B@  BH  BP  BX(�B`  Bh  Bp(�Bx(�B�  B��B�  B�  B�  B�{B�{B��B�  B��B�  B�  B�  B�  B�  B�  B�{B�{B�  B�  B�  B�{B�{B��B��B�  B��B�  B�{B�{B�{B�{C   C  C
=C  C��C	��C
=C
=C  C  C  C
=C  C��C  C  C��C"  C$  C%��C'��C*  C,  C.  C0
=C2
=C4  C6  C8  C:  C<
=C>  C?��CB  CD
=CF
=CH  CJ  CK��CM��CP  CR
=CT  CV  CX  CZ
=C\  C^  C`
=Cb  Cc��Ce��Cg��Ci��Cl  Cm��Co��Cr
=Ct  Cu��Cw��Cy��C|  C~  C�C�  C�  C�C�C���C�  C�C�C�  C�  C�C�  C�  C���C���C�  C�  C�  C�  C�  C���C�  C�  C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�C���C�  C���C�  C�  C���C�  C���C���C�  C�  C�  C�  C�C�  C���C���C���C�  C�  C���C�  C�  C���C�  C�  C�  C�  C�  C�  C�  C�C�C�C�  C�  C�  C�  C�  C�C�C�  C���C���C�  C���C�  C�C���C�  C�C�C�C�C�  C�  C�  C���C�  C���C���C���C�  C�  C�  C�C�  C���C���C�  C�  C�  C�  C���C���C���C���C���C���C���C���C�  C���C�  C�  C�  C���D }qD �qD}qD  D��D  D� D  D� D�D��D�D��D�D� D  D��D	  D	}qD	�qD
� D  D��D  D� D  D}qD  D��D�qD}qD  D}qD�qD� D  D}qD�qD}qD�qD}qD  D��D  D� D�qD}qD  D� D  D��D�D�D�D� D  D� D�qD� D  D}qD�qD� D   D }qD!  D!� D"�D"� D"�qD#��D$D$� D$��D%� D&  D&� D'�D'}qD'�qD(� D(�qD)� D*�D*��D+  D+}qD+�qD,� D-  D-� D.�D.��D/  D/� D0  D0��D1  D1� D2  D2� D3�D3� D3�qD4}qD4�qD5� D6  D6� D7  D7}qD8  D8� D8�qD9}qD:  D:��D;�D;� D;�qD<}qD=�D=��D>�D>� D>�qD?}qD@  D@� DA  DA� DB�DB��DC  DC}qDD  DD� DEDE� DE�qDF}qDF�qDG� DH�DH��DI�DI� DI�qDJ}qDJ�qDK� DL  DL� DL�qDM� DN  DN}qDN�qDO}qDO�qDP� DP�qDQ}qDR  DR��DS  DS��DT  DT� DU  DU� DV  DV� DW�DW� DW�qDX� DY  DY� DY�qDZ}qDZ�qD[� D\  D\}qD]  D]��D^�D^� D_  D_��D`�D`��Da�Da��Db  Db� Dc  Dc� Dd  Dd� Dd�qDe� Df�Df��Dg�Dg� Dh  Dh� Di  Di��Dj�Dj��Dk�Dk� Dk�qDl� Dm  Dm� Dn  Dn}qDn�qDo� Dp�Dp��Dq�Dq� Dr  Dr}qDr�qDs� Dt  Dt}qDu  Du� Dv  Dv��Dw  Dw� Dx�Dx� Dx�qDy� Dz  Dz}qDz�qD{}qD{�qD|}qD}  D}� D~  D~��D  D� D�qD�>�D�~�D�� D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�HD�>�D�� D�� D���D�@ D�� D�� D���D�@ D��HD�� D���D�@ D��HD�� D���D�>�D�� D�� D�HD�@ D��HD��HD�HD�AHD�~�D�� D�HD�@ D�� D��HD�  D�@ D��HD�� D��qD�=qD�� D��HD�HD�AHD�� D���D���D�@ D�� D���D�  D�AHD�~�D�� D�  D�>�D��HD�� D�  D�AHD��HD�� D���D�@ D�� D�� D���D�@ D�� D�� D�  D�AHD�� D��HD��D�AHD�� D�� D�  D�>�D�~�D�� D���D�>�D�� D���D�  D�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D��HD�� D�  D�AHD�� D���D��qD�>�D�� D�� D�  D�>�D�� D��HD�HD�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD�� D�� D�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD��HD�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D��HD�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�@ D��HD�� D�  D�AHD�� D�� D�HD�@ D�� D��HD�  D�>�D�� D��HD�  D�@ D��HD��HD�  D�>�D�~�D�� D���D�>�D��HD��HD�  D�@ D��HD��HD���D�>�D�~�D��HD�HD�@ D�� D���D�  D�AHD��HD�� D�  D�AHD�� D�� D�  D�AHD��HD�� D���D�AHD��HD��HD�  D�@ D�~�D���D���D�>�D D��HD�HD�AHDÀ D��HD�HD�AHDĀ D�� D�  D�@ Dŀ Dž�D���D�@ D�~�Dƾ�D���D�>�DǁHD��HD�HD�AHDȀ D�� D�  D�@ Dɀ D��HD�  D�@ Dʀ D�� D���D�>�Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D�~�D;�D�  D�@ D΀ D�� D���D�@ DρHD��HD�  D�AHDЁHD�� D���D�@ Dр DѾ�D���D�AHDҀ D�� D�  D�@ DӀ D��HD�HD�@ D�~�DԾ�D���D�@ DՁHD��HD�HD�@ Dր D־�D�  D�@ D׀ D��HD�  D�@ D؀ D�� D�  D�@ D�~�D�� D�  D�>�Dڀ D��HD�  D�@ D�~�D۾�D�  D�AHD܁HD�� D���D�>�D�~�D�� D�  D�@ Dހ D��HD�  D�>�D߀ D�� D���D�>�D�� D�� D�  D�AHD�HD��HD�  D�>�D� D�� D�  D�@ D�~�D�� D�  D�B�D� D侸D���D�@ D� D徸D�  D�B�D�HD�� D�  D�@ D� D�� D�  D�AHD� D�� D�  D�AHD� D�� D�HD�@ D�~�D꾸D�  D�@ D� D�� D�HD�AHD�HD��HD�HD�AHD� D���D���D�@ D� D�� D���D�>�D�~�D�� D�  D�@ D�� D�D�  D�AHD�HD�� D�HD�@ D�~�D�D�  D�@ D�~�D�� D�  D�AHD�HD��HD�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�AHD�p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AH��AH��AH��AH��AH��AH��AH��AH��AH��AH��AH��AH��AH��AH�/AH�/AH�HAH�`AH�HAH�`AH�`AH�`AH�`AH�HAH�/AH�`AH�AH��AH��AH�/AH�`AH�`AH�`AH�yAI/AIp�AI|�AIt�AIt�AI|�AI��AI��AI��AI��AIƨAJ1AJJAI�AJ1AJ1'AJ5?AJZAJbNAJr�AJ�AJr�AJjAJ�AJ�uAJ�9AJĜAJ�jAJ��AJ��AJ�AJ��AK�AK�AKG�AKO�AKp�AJ��AK��AM��AOO�AOAO��APbAP$�AP9XAP$�AP(�AP$�APAO��ANĜANffAN=qANJAN1AM�AM?}AM�AM%AL�AKAK�AK�-AKXAJz�AJz�AJ~�AI�AIp�AIG�AH�/AH�9AH�AH�jAGƨAEx�ADĜAC�AC��ACl�AB��AB-A@��A@Q�A@9XA@1'A?�mA=��A=G�A=p�A=hsA=+A<�`A;�#A:��A:5?A9�A9VA8ȴA8�\A8��A8~�A8=qA7�A7�A7�7A7p�A7G�A6��A6n�A6 �A5�TA5�7A4��A3l�A3
=A2Q�A1�#A1�A17LA0=qA/hsA/�A.z�A-�PA,��A,I�A+A+oA*Q�A*bA)�#A)�hA)`BA)"�A(��A'��A&~�A%�A%hsA$��A$Q�A#��A#�PA#�A"9XA ȴA �9A ��A ��A ��A �A n�A JA�AM�A��A�A�jA�DAn�AjAffAffAbNAM�AbA�wA�PA?}A%A
=A
=A%A�A��Av�AA�A{A�#AC�Av�AA�#A�A�`A1'A��Al�A�A�+AI�A  A�
A�FAt�A;dA"�A�AVA��A&�A�\Av�A�TAx�AhsA/A��A��A�mA�A�A�
A��AXAoAn�A�^A�A"�A
�A
$�A	��A	33A	�A��A�A��A=qA�FA�A�yA~�AAC�A�RAbA/A�jA��A�
A�^Ap�A ��A $�A {A @��w@��@�5?@�O�@��;@�~�@��D@�  @�  @�1@���@���@��@�X@� �@�b@�1@��m@�~�@�+@��m@�X@�Q�@��@�w@���@�+@�@��D@�ƨ@�"�@݉7@ׅ@֟�@�S�@��@֟�@���@�Ĝ@Ӯ@�\)@��@җ�@ѡ�@���@���@�v�@���@�@ɲ-@ɉ7@�O�@�&�@ț�@�  @ǥ�@�
=@Ƨ�@�@š�@�/@���@��/@���@Ĵ9@�I�@��
@�C�@�@��@��w@�=q@�bN@�@���@�j@��@�E�@��@�1'@��
@�S�@�M�@�-@�-@���@�&�@��j@��@��@�S�@�C�@��H@�~�@�5?@��@���@�x�@��j@���@�+@�@�n�@�{@���@�G�@��@��@�l�@���@���@��m@�C�@��@�ff@��@��@��D@�j@��@�|�@�C�@�"�@�
=@��@�@�
=@��y@���@�M�@�$�@��@��@���@��@�V@���@�  @���@�"�@�@���@��@�V@��@���@�%@��j@��j@��@���@�z�@�A�@��@��@�o@���@���@��@��@��@�ȴ@���@�v�@�^5@�-@���@��#@��-@��h@�hs@��9@�Q�@�1'@���@�K�@�+@�"�@��@�o@�@�n�@�7L@�V@�%@���@��j@��u@�j@�1@��P@�K�@��@�V@�{@��T@�@���@�hs@�%@�bN@�(�@��@��@��@��@�|�@�|�@��@�=q@�@�G�@�z�@�b@�1@���@��m@���@�\)@��@�n�@��@���@��`@���@���@��/@��@��@�Ĝ@��@��@�b@�1@���@�~�@�V@�@��T@���@���@�?}@�%@���@��u@�;@~��@~�@~ff@}�T@}p�@|�j@|��@|��@|�@|�j@|��@|�@|��@|�/@|�D@|�@{��@z�@z^5@z�@z�@z�@y��@x�`@w�@v5?@s�m@s��@t��@r�H@r^5@q%@p �@n��@n5?@nE�@nv�@nȴ@o|�@o��@p  @p  @o�w@o�;@pbN@p��@q�7@q�#@q��@rn�@r^5@rM�@q�@q�7@q�7@q&�@pĜ@p �@o�;@n�y@n��@m�@m?}@l��@l�/@lz�@kt�@ko@k@j��@j�\@j~�@j=q@i��@i�7@i��@i�#@j^5@k@j�!@j��@j��@j~�@j�\@j�\@j~�@i�^@iG�@iX@ihs@i%@g�@g|�@f��@fV@f��@h��@h�9@h�@g�w@g|�@e�T@d(�@d9X@d1@d�/@d��@dj@c�@a�7@^E�@\I�@[�@Y7L@Y%@Y%@XA�@X  @V��@Up�@T�D@S�@R^5@N��@O��@R-@R=q@Q�^@S�
@Rn�@QX@U�@W�P@W�;@W��@W��@W\)@WK�@V��@U�@U��@V�y@Vff@Vv�@Vv�@VV@V5?@U��@U`B@U/@T�/@T1@SdZ@S"�@So@S@R~�@Qx�@P��@PA�@P �@Q%@Q%@Q%@P��@PQ�@O�@Nv�@O��@N�R@M�@M��@NE�@Nv�@Nv�@M?}@L�@J��@F�R@F�R@H1'@J^5@K33@Kt�@I�^@HĜ@EO�@B�@Bn�@Bn�@A��@A�#@A7L@@��@A7L@A�7@Ax�@A�7@A�7@A�@B��@C33@D(�@D�/@EV@EV@E/@E?}@E?}@Ep�@E�@E�@Ep�@D��@D�@D�D@DI�@D�@C�m@C��@CC�@C@B�\@BJ@A�@A�#@Ax�@AX@A7L@@��@@Ĝ@@�9@@��@@r�@?�;@?�w@?�@?|�@?+@>�y@>ȴ@>v�@>V@>E�@>E�@>E�@>@=�T@=��@=�@=p�@=`B@=O�@=/@=/@=V@<��@<Z@<9X@<9X@<(�@;��@;�@;dZ@;C�@;@:��@:~�@:^5@:J@9�@9�^@9�^@9��@9hs@9X@97L@9&�@9�@9%@8�`@8��@8�u@8A�@81'@8  @7��@7�w@7�@7�P@7�P@7l�@7\)@7\)@7K�@7
=@6�@6ȴ@6�R@6��@6�+@6ff@65?@6@6@5�@5�@5�T@5��@5@5�-@5�@5`B@5/@4�@4��@4�j@4�j@4�@4�D@4z�@4j@4Z@4(�@4�@4�@41@3�m@3�
@3�
@3ƨ@3ƨ@3ƨ@3ƨ@3ƨ@3ƨ@3�@333@3"�@3o@3o@2�@2��@2��@2��@2�\@2~�@2n�@2^5@2=q@2�@1��@1�#@1�#@1��@1�^@1��@1�7@1x�@1hs@1X@17L@1&�@0��@0��@0��@0��@0�9@0�u@0bN@0Q�@01'@01'@0 �@0  @/�@/�;@/��@/�@/l�@/;d@/
=@.�y@.�@.�@.ȴ@.�R@.�R@.��@.��@.��@.�+@.�+@.�+@.�+@.v�@.V@.@-�h@-/@-�@,��@,�@-V@,�/@,�@,�D@,j@,I�@,9X@,(�@,�@,�@,1@+�m@+�
@+�m@+�m@+�m@,1@,�@,(�@,I�@,9X@,(�@,�@+��@+�m@+ƨ@+�F@+�F@+��@+��@+��@+dZ@+S�@+33@+"�@+"�@+o@+@+@*�@*�H@*��@*�!@*�!@*�!@*��@*~�@*^5@*M�@*-@*�@*J@)��@)�@)�#@)��@)��@)�^@)��@)��@)��@)�7@)x�@)7L@)�@)%@)%@(��@(��@(�`@(��@(��@(��@(��@(��@(��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AH��AH��AH��AH��AH��AH��AH��AH��AH��AH��AH��AH��AH��AH�/AH�/AH�HAH�`AH�HAH�`AH�`AH�`AH�`AH�HAH�/AH�`AH�AH��AH��AH�/AH�`AH�`AH�`AH�yAI/AIp�AI|�AIt�AIt�AI|�AI��AI��AI��AI��AIƨAJ1AJJAI�AJ1AJ1'AJ5?AJZAJbNAJr�AJ�AJr�AJjAJ�AJ�uAJ�9AJĜAJ�jAJ��AJ��AJ�AJ��AK�AK�AKG�AKO�AKp�AJ��AK��AM��AOO�AOAO��APbAP$�AP9XAP$�AP(�AP$�APAO��ANĜANffAN=qANJAN1AM�AM?}AM�AM%AL�AKAK�AK�-AKXAJz�AJz�AJ~�AI�AIp�AIG�AH�/AH�9AH�AH�jAGƨAEx�ADĜAC�AC��ACl�AB��AB-A@��A@Q�A@9XA@1'A?�mA=��A=G�A=p�A=hsA=+A<�`A;�#A:��A:5?A9�A9VA8ȴA8�\A8��A8~�A8=qA7�A7�A7�7A7p�A7G�A6��A6n�A6 �A5�TA5�7A4��A3l�A3
=A2Q�A1�#A1�A17LA0=qA/hsA/�A.z�A-�PA,��A,I�A+A+oA*Q�A*bA)�#A)�hA)`BA)"�A(��A'��A&~�A%�A%hsA$��A$Q�A#��A#�PA#�A"9XA ȴA �9A ��A ��A ��A �A n�A JA�AM�A��A�A�jA�DAn�AjAffAffAbNAM�AbA�wA�PA?}A%A
=A
=A%A�A��Av�AA�A{A�#AC�Av�AA�#A�A�`A1'A��Al�A�A�+AI�A  A�
A�FAt�A;dA"�A�AVA��A&�A�\Av�A�TAx�AhsA/A��A��A�mA�A�A�
A��AXAoAn�A�^A�A"�A
�A
$�A	��A	33A	�A��A�A��A=qA�FA�A�yA~�AAC�A�RAbA/A�jA��A�
A�^Ap�A ��A $�A {A @��w@��@�5?@�O�@��;@�~�@��D@�  @�  @�1@���@���@��@�X@� �@�b@�1@��m@�~�@�+@��m@�X@�Q�@��@�w@���@�+@�@��D@�ƨ@�"�@݉7@ׅ@֟�@�S�@��@֟�@���@�Ĝ@Ӯ@�\)@��@җ�@ѡ�@���@���@�v�@���@�@ɲ-@ɉ7@�O�@�&�@ț�@�  @ǥ�@�
=@Ƨ�@�@š�@�/@���@��/@���@Ĵ9@�I�@��
@�C�@�@��@��w@�=q@�bN@�@���@�j@��@�E�@��@�1'@��
@�S�@�M�@�-@�-@���@�&�@��j@��@��@�S�@�C�@��H@�~�@�5?@��@���@�x�@��j@���@�+@�@�n�@�{@���@�G�@��@��@�l�@���@���@��m@�C�@��@�ff@��@��@��D@�j@��@�|�@�C�@�"�@�
=@��@�@�
=@��y@���@�M�@�$�@��@��@���@��@�V@���@�  @���@�"�@�@���@��@�V@��@���@�%@��j@��j@��@���@�z�@�A�@��@��@�o@���@���@��@��@��@�ȴ@���@�v�@�^5@�-@���@��#@��-@��h@�hs@��9@�Q�@�1'@���@�K�@�+@�"�@��@�o@�@�n�@�7L@�V@�%@���@��j@��u@�j@�1@��P@�K�@��@�V@�{@��T@�@���@�hs@�%@�bN@�(�@��@��@��@��@�|�@�|�@��@�=q@�@�G�@�z�@�b@�1@���@��m@���@�\)@��@�n�@��@���@��`@���@���@��/@��@��@�Ĝ@��@��@�b@�1@���@�~�@�V@�@��T@���@���@�?}@�%@���@��u@�;@~��@~�@~ff@}�T@}p�@|�j@|��@|��@|�@|�j@|��@|�@|��@|�/@|�D@|�@{��@z�@z^5@z�@z�@z�@y��@x�`@w�@v5?@s�m@s��@t��@r�H@r^5@q%@p �@n��@n5?@nE�@nv�@nȴ@o|�@o��@p  @p  @o�w@o�;@pbN@p��@q�7@q�#@q��@rn�@r^5@rM�@q�@q�7@q�7@q&�@pĜ@p �@o�;@n�y@n��@m�@m?}@l��@l�/@lz�@kt�@ko@k@j��@j�\@j~�@j=q@i��@i�7@i��@i�#@j^5@k@j�!@j��@j��@j~�@j�\@j�\@j~�@i�^@iG�@iX@ihs@i%@g�@g|�@f��@fV@f��@h��@h�9@h�@g�w@g|�@e�T@d(�@d9X@d1@d�/@d��@dj@c�@a�7@^E�@\I�@[�@Y7L@Y%@Y%@XA�@X  @V��@Up�@T�D@S�@R^5@N��@O��@R-@R=q@Q�^@S�
@Rn�@QX@U�@W�P@W�;@W��@W��@W\)@WK�@V��@U�@U��@V�y@Vff@Vv�@Vv�@VV@V5?@U��@U`B@U/@T�/@T1@SdZ@S"�@So@S@R~�@Qx�@P��@PA�@P �@Q%@Q%@Q%@P��@PQ�@O�@Nv�@O��@N�R@M�@M��@NE�@Nv�@Nv�@M?}@L�@J��@F�R@F�R@H1'@J^5@K33@Kt�@I�^@HĜ@EO�@B�@Bn�@Bn�@A��@A�#@A7L@@��@A7L@A�7@Ax�@A�7@A�7@A�@B��@C33@D(�@D�/@EV@EV@E/@E?}@E?}@Ep�@E�@E�@Ep�@D��@D�@D�D@DI�@D�@C�m@C��@CC�@C@B�\@BJ@A�@A�#@Ax�@AX@A7L@@��@@Ĝ@@�9@@��@@r�@?�;@?�w@?�@?|�@?+@>�y@>ȴ@>v�@>V@>E�@>E�@>E�@>@=�T@=��@=�@=p�@=`B@=O�@=/@=/@=V@<��@<Z@<9X@<9X@<(�@;��@;�@;dZ@;C�@;@:��@:~�@:^5@:J@9�@9�^@9�^@9��@9hs@9X@97L@9&�@9�@9%@8�`@8��@8�u@8A�@81'@8  @7��@7�w@7�@7�P@7�P@7l�@7\)@7\)@7K�@7
=@6�@6ȴ@6�R@6��@6�+@6ff@65?@6@6@5�@5�@5�T@5��@5@5�-@5�@5`B@5/@4�@4��@4�j@4�j@4�@4�D@4z�@4j@4Z@4(�@4�@4�@41@3�m@3�
@3�
@3ƨ@3ƨ@3ƨ@3ƨ@3ƨ@3ƨ@3�@333@3"�@3o@3o@2�@2��@2��@2��@2�\@2~�@2n�@2^5@2=q@2�@1��@1�#@1�#@1��@1�^@1��@1�7@1x�@1hs@1X@17L@1&�@0��@0��@0��@0��@0�9@0�u@0bN@0Q�@01'@01'@0 �@0  @/�@/�;@/��@/�@/l�@/;d@/
=@.�y@.�@.�@.ȴ@.�R@.�R@.��@.��@.��@.�+@.�+@.�+@.�+@.v�@.V@.@-�h@-/@-�@,��@,�@-V@,�/@,�@,�D@,j@,I�@,9X@,(�@,�@,�@,1@+�m@+�
@+�m@+�m@+�m@,1@,�@,(�@,I�@,9X@,(�@,�@+��@+�m@+ƨ@+�F@+�F@+��@+��@+��@+dZ@+S�@+33@+"�@+"�@+o@+@+@*�@*�H@*��@*�!@*�!@*�!@*��@*~�@*^5@*M�@*-@*�@*J@)��@)�@)�#@)��@)��@)�^@)��@)��@)��@)�7@)x�@)7L@)�@)%@)%@(��@(��@(�`@(��@(��@(��@(��@(��@(��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�LB�RB�LB�RB�RB�RB�RB�RB�LB�LB�LB�LB�LB�FB�FB�FB�LB�LB�LB�LB�RB�jB�}B��B��B��B��BÖBÖBŢBBƨBɺBɺBǮB��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�
B�#B�TB�ZB�fB�mB�B�yB��B(�BO�Bo�Bz�B�B�1B�VB�uB�{B�{B�oB�VB�+B�B�B�7B�VB�PB�JB�JB�DB�=B�+B�%B�1B�B~�B�B�B}�By�Bw�Bs�Bt�Bt�Bs�BffBN�BG�B?}B:^B8RB49B,B�B�B�B�BuBBB+B	7B1BB��B�B�B�B�ZB�NB�BB�HB�HB�;B�/B�B�B�
B�B��B��BȴBŢB��B�RB�B��B��B��B��B�uB�DB�B� By�Bq�Bk�BhsBcTB]/BW
BT�BR�BP�BN�BL�BH�BB�B8RB33B0!B)�B$�B�B�B�BuBDBJBVBoBoBhBPB1B  B
��B
��B
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
�B
�B
�B
�B
�mB
�`B
�BB
�/B
�B
�B
��B
��B
��B
��B
��B
ɺB
ȴB
ƨB
ŢB
ĜB
B
�}B
�dB
�RB
�FB
�?B
�-B
�'B
�!B
�B
�B
�B
�B
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
��B
��B
��B
��B
�uB
�bB
�PB
�=B
�1B
�B
�B
}�B
z�B
u�B
q�B
n�B
k�B
jB
iyB
ffB
cTB
`BB
_;B
^5B
]/B
[#B
XB
T�B
P�B
L�B
G�B
E�B
E�B
D�B
A�B
;dB
49B
0!B
.B
-B
,B
)�B
$�B
�B
�B
VB
JB
DB
	7B
+B
B
B	��B	��B	��B	�B	�)B	�B	�BB	�TB	�NB	�BB	�/B	�#B	�B	�B	��B	��B	��B	ĜB	�wB	�qB	�qB	�jB	�dB	�dB	�dB	�dB	�^B	�^B	�XB	�RB	�LB	�RB	�RB	�RB	�RB	�RB	�LB	�LB	�LB	�FB	�?B	�-B	�!B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	�uB	�uB	�{B	��B	��B	��B	��B	�{B	�{B	�{B	�uB	��B	��B	��B	��B	��B	��B	�uB	�hB	�\B	�VB	�VB	�PB	�JB	�VB	�\B	�\B	�VB	�bB	�hB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�-B	�-B	�-B	�-B	�-B	�-B	�-B	�3B	�3B	�3B	�-B	�9B	�?B	�?B	�FB	�FB	�FB	�FB	�LB	�FB	�LB	�XB	�^B	�^B	�^B	�jB	�jB	�wB	�wB	�}B	�}B	��B	��B	B	ÖB	ĜB	ĜB	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ŢB	ÖB	B	ĜB	ĜB	ĜB	ÖB	B	��B	��B	��B	��B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�/B	�BB	�BB	�NB	�TB	�`B	�ZB	�`B	�fB	�sB	�sB	�yB	�yB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B

=B
DB
JB
VB
VB
\B
\B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
uB
hB
\B
JB
bB
�B
�B
�B
�B
�B
�B
'�B
/B
1'B
49B
5?B
6FB
6FB
5?B
33B
7LB
?}B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
D�B
D�B
C�B
B�B
B�B
D�B
H�B
J�B
M�B
O�B
N�B
M�B
N�B
Q�B
P�B
P�B
Q�B
T�B
T�B
T�B
S�B
R�B
O�B
J�B
K�B
O�B
VB
W
B
W
B
T�B
S�B
M�B
K�B
K�B
K�B
K�B
L�B
M�B
O�B
Q�B
T�B
VB
W
B
W
B
ZB
_;B
aHB
iyB
n�B
o�B
p�B
q�B
q�B
r�B
s�B
s�B
s�B
t�B
v�B
w�B
x�B
z�B
{�B
|�B
}�B
~�B
� B
�B
�B
�B
�B
�+B
�+B
�1B
�7B
�=B
�=B
�=B
�DB
�PB
�VB
�VB
�VB
�bB
�oB
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
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�!B
�!B
�'B
�-B
�3B
�?B
�FB
�LB
�XB
�XB
�^B
�^B
�^B
�dB
�dB
�dB
�jB
�qB
�}B
��B
��B
��B
B
ÖB
ŢB
ƨB
ǮB
ǮB
ǮB
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
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�
B
�B
�B
�B
�#B
�#B
�)B
�/B
�5B
�;B
�;B
�;B
�BB
�HB
�NB
�NB
�TB
�ZB
�ZB
�ZB
�`B
�`B
�fB
�mB
�mB
�sB
�sB
�sB
�yB
�B
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
��B  B  BBBBBBBBBBBBB%B%B%B%B+B+B1B1B1B	7B	7B	7B
=BDBDBDBJBJBJBJBJBPBPBPBVBVBVB\B\B\BbBbBbBhBhBhBhBoBoBoBuBuB{B{B{B{B�B�B�B�B�B�B�B�B�B�B�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�^B�OB�NB�LB�NB�LB�JB�GB�8B�AB�LB�LB�<B�KB�IB�BB�YB�EB�QB�MB�OB�WB�WB�:B�jB�TB�DB�1B�4B�LB�JB�9B��B��B�YB��B��B�lB�*B�B�CB�B�B��BɬB�B�qB�TBͺB�tB��B϶BмB�B��BХBѽBӝB��B�B�EB�B��BֱB�IB�SB��B�DB�&B�B�jB��B$kBN�Bn�Bz�B��B��B��B�^B��B��B��B��B�1B��B��B�QB�>B�{B��B��B��B�EB�xB�2B�+B�`B%B�B�zBUBzfBx�Bt,Bt�Bt�Bv�BlnBQABI�B@�B:�B:	B6B/EB!�BB�B�B�B9B �B>B	�B	B!B�TB��B�eB��B�-B��B�(B�B� B�B��BڅB�bBבB�iB��B̬B�kBƷB�B��B�DB��B�#B��B��B�B��B�B��B|nBtgBl{Bi�BeNB_HBW�BU�BS�BQzBO�BNXBK�BE�B:�B4B1�B+jB&LB�B!
B HBcB�B�B�BoB�B�B�B
�BeB
��B
�jB
��B
�:B
��B
�B
�B
�B
�B
��B
�KB
�{B
�$B
�mB
�9B
�B
�B
��B
��B
��B
�OB
�^B
�GB
�iB
�eB
��B
��B
��B
�tB
��B
�B
�BB
�2B
��B
҇B
ΖB
̝B
�>B
�%B
�uB
�QB
��B
�FB
�KB
�B
�JB
��B
��B
��B
�^B
�jB
��B
��B
�B
�B
�B
� B
�WB
��B
��B
��B
��B
��B
�rB
��B
�|B
��B
�!B
��B
��B
��B
��B
�>B
��B
��B
��B
��B
�kB
��B
�DB
�B
|�B
xUB
sB
p�B
k�B
j�B
jTB
g�B
e�B
`�B
_rB
^�B
^&B
\�B
YyB
W B
SB
O�B
H�B
E�B
E�B
ELB
D�B
?�B
6FB
1�B
.AB
-$B
,WB
,2B
)�B
"~B
9B
�B
�B
�B

ZB
�B
WB
�B	�&B	��B	�fB	�B	ݱB	�KB	��B	��B	�B	��B	��B	۱B	يB	��B	։B	�jB	�,B	�`B	�UB	��B	��B	��B	��B	��B	�:B	�VB	��B	�HB	�B	�OB	��B	�B	��B	��B	�vB	��B	��B	�B	�<B	�9B	�B	�{B	�yB	�B	��B	�.B	��B	��B	��B	��B	��B	�!B	�gB	�B	��B	��B	�%B	��B	�:B	��B	�0B	��B	��B	�B	�B	��B	��B	��B	�$B	��B	�4B	�5B	��B	�bB	�B	�HB	�&B	�)B	�EB	�CB	��B	�JB	��B	�]B	��B	�dB	�B	��B	�B	��B	� B	�B	��B	��B	��B	��B	�kB	��B	��B	�B	�+B	��B	��B	��B	�B	��B	��B	��B	��B	�6B	��B	�B	��B	��B	��B	�|B	�XB	��B	�JB	��B	��B	��B	�B	�4B	�_B	�JB	��B	�ZB	��B	��B	��B	��B	��B	�B	�+B	�	B	�+B	�.B	�B	�B	�B	�)B	��B	�vB	�B	�~B	��B	�B	��B	��B	��B	�B	��B	��B	�(B	��B	�B	�<B	�)B	�)B	�B	��B	�`B	��B	��B	�bB	�@B	�(B	�B	�\B	��B	��B	�SB	�B	�B	��B	�>B	�B	�B	��B	�B	��B	��B	�DB	��B	�%B	�0B	�1B	�BB	��B	��B	��B	��B	��B	�BB	�[B	�,B	�)B	�B	�9B	�xB	��B	��B	�KB	�SB	� B	��B	��B	��B	�zB	�tB	��B	��B	��B	�wB	��B	�PB	�
B	��B	��B	��B	��B	��B	��B	��B	�xB	�}B	�}B	�zB	B	øB	��B	��B	� B	�+B	�B	��B	ūB	ƱB	�B	�BB	�xB	�B	�hB	B	�:B	��B	�B	ħB	�UB	�vB	�#B	��B	�lB	�\B	�0B	ˎB	͸B	��B	�B	��B	ЈB	ҟB	�~B	��B	�B	��B	�JB	�^B	�B	�B	�cB	�B	�B	��B	�B	�<B	�B	�
B	�B	�B	�B	��B	�IB	��B	��B	��B	�B	�B	��B	� B	�B	�B	��B	�MB	�YB	�(B	��B	�B	�B	��B	��B	�B	��B	�XB	��B
B
cB
�B
qB
|B
�B
�B
�B

$B
uB
�B
�B
�B
�B
^B
�B
�B
�B
-B
~B
 RB
!GB
IB
UB
FB
�B
�B
'B
�B
�B
zB
9B
MB
tB
�B
�B
}B
fB
�B
B
�B
_B
�B
& B
.�B
1<B
4cB
5vB
6^B
6�B
6iB
2�B
6cB
?�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
DDB
EB
D�B
E�B
E�B
E	B
EmB
DB
CB
B�B
C�B
H�B
J�B
M�B
POB
O�B
NVB
NB
R�B
Q�B
QB
Q�B
T�B
UB
U�B
T�B
TB
R�B
J�B
J�B
N1B
UNB
V�B
X]B
U�B
V�B
O�B
L7B
K�B
L%B
K�B
MNB
NB
O�B
Q�B
UB
U�B
WB
V�B
YzB
^�B
`�B
h�B
npB
o�B
p�B
q�B
q�B
r�B
s�B
s�B
s�B
uAB
v�B
w�B
yB
{B
|B
}'B
~GB
6B
�aB
�wB
�<B
�4B
�rB
�EB
�LB
�iB
�`B
�LB
�OB
�kB
��B
�qB
�gB
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
�B
��B
��B
�B
�B
�	B
��B
�&B
�B
�B
��B
�
B
�8B
�B
�0B
�*B
�/B
�1B
�AB
�9B
�cB
�tB
�OB
�nB
�uB
�dB
�kB
�xB
�fB
�}B
�sB
�eB
�xB
��B
��B
��B
��B
��B
��B
¬B
��B
��B
ƮB
ǼB
ǲB
ǾB
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
��B
��B
��B
�B
�B
��B
�	B
�B
�B
�B
�B
�B
�@B
�SB
�,B
�.B
�"B
�AB
�HB
�?B
�PB
�JB
�JB
�MB
�QB
�fB
�hB
�kB
�oB
�\B
�fB
�lB
�{B
�pB
�uB
�zB
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
��B
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
�#B
�BB
�6B
��B
�B
� B
��B
�B
�!B
�B
�B
�B B BBBB&BBBBBBBBB3B5B4BBB6BHB>B4B?B	5B	IB	bB
KB_BSBIBXBZBOBZB]BfB^BPBVBfBtBuBnBxBnBpBsBxBwBwBlB{B~BuBtB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�<#�<#�<#�
<#�<#�
<#�<#�<#�C<#�i<#�
<#�
<#��<#�0<#�<#��<#�0<#׎<#�<#�<#�<#�i<#�i<#�<#��<#ף<#�<#�c<#��<#�
<#�<#�$<$@|<$8�<#��<#׎<#�<#ا<#�<#ا<#�<$�<#�a<$6�<#ף<#�M<#�e<#�H<#��<#�<#��<#�+<#�+<#�l<#��<#�<#��<#�N<#��<#؄<#��<#��<#�r<#�5<$g�<#�<$v<#ڑ<#�l<$��<'5<8��<3=x<%@�<$k�<#��<#�&<#��<#�<#ا<#׺<#�m<$��<'�O<$�<$�<$�<#�<$z�<$��<$p<#��<%y<'F<#�!<#׎<$�X<'�<#ܯ<#�X<%m�<%Q�<$�<$ѩ<$�<#�c<#�<<)�O<>%�<(M}<'޽<$��<$
<&<&�^<+�^<&�*<#�W<#��<$�3<6��<'A><#��<#�$<$/%<$q@<*�&<,�<%
�<$o�<'��<$^�<$"2<#�<#�<$><<$i&<$C�<#�	<#�4<$�<%Z2<$�<$v�<$=<$��<(��<,A�<%�<&��<%K:<$�k<$�V<)G9<'�U<$�Q<&W�<(�<)��<$�Q<%��<&�A<'><$b�<$%<$aD<$�<$Gd<%��<*�<+��<(!�<$ub<&1�<%m�<%p<$<<%&<(��</}~<#��<#�4<#��<#�
<#��<%MY<$��<)�6<(B�<&��<%��<$��<$F<#�<#��<#ף<#�<#׎<#�<$8�<$}�<$k<$r�<$&<#�&<#�<#ا<#��<$n�<$�<$ K<$�<$8�<&)�<'�U<%K:<$'<'.<$/%<&�?<$��<$�t<$�Q<%��<$J�<$b�<$�<#��<$H�<$.<#�<$/%<&,f<%�d<&��<%��<#�N<%��<$��<#�<$1:<$6�<$��<&��<#�&<#�<<#�<$<$� <$r�<&��<&��<$9�<$��<&1�<$�.<%�!<%0<#�<#�)<#�	<$C�<$ѩ<%��<&
(<$5w<%�<%�l<'�Q<%�!<&��<(�)<%}�<'d<$�<#�<$i&<%�M<(7�<#�<#�E<$�<$��<%MY<%b�<'[)<'uq<)�<$aD<#�I<#�<$5w<+'�<2m<'�<&U"<#�8<#؄<#�<'��<6�M<4��<.O:<%��<$)
<#��<$ح<$E<$�<&Gi<$��<$��<)//<U�<<%�d<$]h<$Z<$<%0<%��<%�@<$�<#�a<$]h<%��<(��<,��<4��<$m,<#��<#ڑ<#�<#�	<#�<$b�<$�b<$#(<$}�<$1:<$��<$,<$7�<#�<#�N<#��<#��<$3U<$A�<$�t<&�+<'��<%*<(�<*nL</Dj<%b�<$� <'��<$�7<'$�<$�<$Z<$o�<%�<#��<#�<$#(<$�B<$?[<$��<$4e<#�"<#�<$%<$&<$�<#��<#�U<$"2<$�k<&$h<$@|<#�<$�<$"2<$A�<$�<$�<$E<&<�<(��<&�%<$�`<$��<#�N<$��<$]h<%�M<$+<#�l<$Sa<$><<#�W<#��<#�<#��<#�X<#�<#�e<$�<$�<#��<#�$<#׺<#�e<$P�<$H�<$:�<$��<#�<$��<#�<#؄<#��<$�t<$*<$�<$�B<$�<#�i<#��<#؄<#�<#��<$�<$�<$��<$a<#�i<#�<#�0<#�I<#ڑ<#�<#�<#ޫ<#�<#�"<#�&<#��<#�<#�<$Ş<$0.<#�<$,<$W<#�U<#�C<#�c<#�c<#�8<$��<&q<#�4<#��<#ۮ<#�<#�<#�<$'<$P�<#�N<$B�<$]h<#��<#�<#�&<#ޫ<#�a<$1:<$��<#��<#�*<#׺<$1:<#��<#�c<#�<$x+<$Ş<$W<$f�<%F<$2G<#ا<#ڑ<#��<#�J<$=<$?[<$aD<$�<$g�<$�(<#�<#�<#�<#��<#�{<#�4<$ <$+<#��<#�*<$W<&�<#�!<$	<#�<#�<#�<$-<#�<#ܯ<$0.<$��<$7�<#��<#��<$ <#��<$�<#�]<#�i<#�<#�
<#�&<#׺<#�&<#ڑ<#�<#�(<#�Q<$�<$/<#�N<#�I<#�I<#��<$Z<$Z�<%k�<&h�<#׺<#�W<%B�<$v<$��<$N�<$�<$%<#�<#ٛ<#�N<$�<#��<#�D<#�<<#�E<#�<#�m<#�<$	<#�<#�D<#��<#�<<#�o<#�<#�<#�<#�<#�5<$�<#�&<$R'<#�U<$'<$�<#�<#�r<#�m<$Z�<#�<#؄<#�<#�$<#ا<#��<$r<#׺<#��<#ڑ<#��<#�N<#��<#�<#�]<#�D<#�<#�<<#�o<$&<#��<#�<#�<#�m<$i&<#�)<#�N<$v<$
�<$�L<#��<#�^<$%<#�<% <%4L<#�<#�]<$�<#��<$/<$N�<%��<(��<%�b<$A�<&�<#�<#׎<$�<#�4<$�B<$�<$L<$e.<$��<(ܠ<#��<&��<#ۮ<#�<%�V<$��<$?[<)_u<&�8<#�&<#�c<#�l<#�E<#��<$�<$�.<#�<$|d<#�<#�0<#�0<#��<#��<#�<#��<#��<#�<$3U<$<<#��<#�<#�c<#�H<$\"<#��<$F<#�<$*<#�<<#�I<#��<#�N<$��<$a<$W<$*<$ K<#�*<#�<#ܯ<#��<$�Q<$�V<$��<+"�<#�&<$�h<&�<$<<#�*<%4L<$y�<)?0<&�R<#�N<#�I<#��<#�8<$	�<#��<#��<#��<#�I<#�<<#�<#�!<$(<#�N<$<<$G<#��<#�<#�C<#�X<#�<#�*<#�{<#�<#��<$�<#��<#��<#�N<#�l<#��<#��<#�<#�<#�<#��<#ٛ<#�c<#�<#�<#�]<#��<#�+<#׺<#�<#�<$ <#�]<#��<#��<#��<#��<#��<#�<#�D<#׺<#�0<#�<#ߜ<#��<#��<#ٛ<#��<#׺<#�C<#�*<#�<#��<#�e<#��<#��<#�<#�C<#��<#�(<#��<#��<#�e<#��<#��<#�o<#�<#��<#ܯ<#�<#ף<#�<#�i<#ڑ<#׺<#�C<#��<#�*<#�<#��<#��<#��<#��<#�+<#�{<#�$<#�<#�<<#��<#׺<#�<#�C<#ߜ<#�l<#ף<#��<#ף<#�*<#ٛ<#�l<#��<#�&<#ף<#�<#��<#�<#׺<#��<#��<#�D<#�l<#ߜ<#��<#�i<#�<#�C<#ٛ<#�c<#׎<#��<#��<#��<#�<#�$<#�D<#׎<#�<#�i<#�&<#�<#�
<#�<#�I<#��<#�<#׺<#��<#�<#��<#��<#��<#�D<#׺<#׺<#�<#׺<#��<#�<#ٛ<#�D<#�<#�{<#�<#�D<#��<#׺<#׎<#�<#�o<#�<#ܯ<#��<#�<#�&<#��<#�o<#�8<#�I<#��<#�<#׎<#�D<#��<#�{<#ף<#��<#�N<#ܯ<#��<#�D<#�{<#�<#��<#׺<#�<#׺<#�<#��<#׎<#�<#�<#�<#׺<#��<#�U<#�N<#�<#׺<#��<#�<#؄<#��<#ۮ<#��<#�D<#��<#ף<#��<#�{<#�<#��<#�*<#�I<#�&<#�<#�<#ا<#��<#׎<#ا<#ף<#��<#׺<#ٛ<#�i<#ٛ<#׎<#�<#ף<#�<#�<#ܯ<#ף<#�D<#׺<#�<#ף<#��<#�<#��<#�$<#؄<#ף<#�
<#�
<#��<#��<#��<#�<#�o<#�{<#ף<#��<#��<#׺<#׺<#�<#�{<#׺<#�&<#�<#��<#׎<#��<#�D<#��<#�<#�i<#�&<#ף<#׎<#�<#�<#�<#�<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = CTM_ADJ_PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                              None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment;OW: r =1(+/-0.0001), vertically averaged dS =-0.005(+/-0.002),                                                                                                             SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT.; No significant drift detected in conductivity                                                                                                                                   202205040000002022050400000020220504000000  AO  ARGQQCPL                                                                    20210217020209  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210217020209  QCF$                G�O�G�O�G�O�0               WHOIARSQWHQCV0.5                                                                20211004000000  QC                  G�O�G�O�G�O�                WHOIARSQCTM V1.0                                                                20220503000000  IP                  G�O�G�O�G�O�                WHOIARCAOWC V2.0ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     20220504000000  IP                  G�O�G�O�G�O�                