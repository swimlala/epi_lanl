CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       $Woods Hole Oceanographic Institution   source        
Argo float     history       92021-02-17T00:27:27Z creation; 2022-05-04T12:55:30Z DMQC;      
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
_FillValue                 �  J   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  N    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  a�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ̀   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20210217002727  20220504085530  1901663 US ARGO PROJECT                                                 BRECK OWENS, STEVE JAYNE, P.E. ROBBINS                          PRES            TEMP            PSAL               �A   AO  5137                            2C  D   S2A                             7179                            SBE602 V1.3                     854 @��~�0
1   @���:P�D"�쪹@D�Z����1   GPS     Primary sampling: averaged [nominal 2 dbar binned data sampled at 0.5 Hz from a SBE41CP]                                                                                                                                                                           A   A   A   ?�z�@   @=p�@�  @��\@��R@�(�A   AG�A!G�A>�RA_\)A�Q�A��A�  A�Q�A�  A�Q�A�  A�  A��B  B  B(�B (�B(Q�B0Q�B8(�B@(�BH  BO�
BW�
B_�
Bh  Bp  Bx(�B�  B��
B�  B�{B�  B�  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�{B�{B��B��B�  B�  B�  B�  B�{B�  B��B��B��B��B��B��
C   C  C  C
=C
=C
  C  C
=C
=C
=C  C  C
=C��C  C  C 
=C!��C$  C&  C(  C*  C,  C.  C/�C2  C4
=C6
=C8  C9��C<
=C>
=C?��CB  CD  CF  CH
=CJ
=CL  CN  CP  CQ��CT  CV
=CX
=CZ  C\  C^  C`  Cb  Cc��Ce��Ch  Cj  Cl  Cn  Cp  Cr
=Ct  Cv
=Cx
=Cz
=C|
=C~
=C�  C�C�
=C�C���C���C�  C���C���C�  C�  C�  C�C�  C�  C�C���C�  C�  C�  C�  C�C�  C�  C�  C�  C�  C���C���C�  C���C���C�  C�C�  C�  C�  C�  C�C�C�C�  C���C���C���C���C���C�  C�  C���C���C���C���C�  C�  C���C���C���C�  C�  C���C���C���C�C�C�  C�  C�C�  C�C�C�  C�  C�C�C�C�  C���C���C�  C�C�C�C�C�  C���C���C�  C�  C���C���C�  C�C�  C�  C�C�C�C�  C�  C�  C���C���C���C�  C�  C�  C�  C�C�  C���C�  C�  C�  C���C���C�C�  C���C���C���C�  C�C�C�C�C�C�D �D }qD  D� D�qD}qD�qD}qD  D� D  D� D  D� D�D��D  D� D	  D	}qD
  D
� D
�qD� D�D� D�qD}qD�qD� D  D}qD�qD}qD  D��D�D� D�qD� D�D��D  D� D  D��D  D}qD�qD}qD�qD� D�D� D  D� D  D��D  D}qD�qD}qD  D� D   D }qD!  D!��D"�D"��D#  D#� D$�D$� D%  D%� D&  D&� D'�D'��D'�qD(� D)�D)��D*  D*}qD+  D+��D,  D,}qD-  D-� D.  D.� D.�qD/}qD0  D0}qD0�qD1� D1�qD2}qD2�qD3� D4�D4� D4�qD5}qD5�qD6� D7�D7� D8  D8� D9  D9� D:�D:� D:�qD;� D<  D<��D=�D=� D=�qD>}qD?  D?��D@  D@� DA  DA� DB�DB� DB�qDC}qDD  DD� DE  DE}qDF  DF��DG�DG� DH  DH��DI  DI}qDI�qDJ}qDJ�qDK}qDL  DL��DM  DM� DN  DN��DO�DO� DO�qDP� DQ  DQ� DR  DR� DS  DS� DT�DT��DU  DU��DV  DV� DV�qDW� DX�DX� DY  DY��DZ�DZ� D[  D[� D[�qD\}qD\�qD]}qD]�qD^}qD_  D_��D`�D`� Da  Da}qDa�qDb� Dc  Dc� Dd  Dd� De  De� De�qDf}qDg  Dg}qDh  Dh� Di  Di� Dj  Dj� Dj�qDk� Dl  Dl��Dm�Dm� Dn  Dn}qDn�qDo}qDp  Dp� Dq  Dq}qDq�qDr� Ds�Ds�Dt  Dt� Du�Du� Dv  Dv� Dw  Dw}qDx  Dx��Dy  Dy� Dz�Dz��D{�D{��D|  D|� D}�D}��D~�D~��D  D� D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�� D���D�>�D�}qD�� D�  D�@ D�� D���D�  D�AHD��HD��HD�  D�@ D��HD��HD�  D�@ D�� D���D�  D�@ D�� D��HD�  D�>�D��HD��HD�HD�@ D�~�D��HD�HD�>�D�� D��HD�  D�@ D�~�D���D�  D�@ D�~�D�� D�HD�@ D�� D�� D���D�>�D�� D��HD�  D�=qD�~�D���D���D�>�D�� D��HD�  D�@ D�� D��HD�HD�@ D��HD���D���D�@ D�~�D���D�  D�@ D�~�D�� D���D�>�D�� D��HD��D�AHD�~�D��qD�  D�AHD�� D��HD�  D�>�D�~�D���D���D�@ D��HD�� D�  D�@ D�� D���D�  D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�~�D�� D�  D�>�D�� D��HD�HD�AHD�~�D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�>�D�� D��HD�  D�@ D�� D���D���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�>�D�� D��HD�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�AHD��HD�� D���D�@ D��HD�� D���D�@ D��HD�D�  D�@ D��HD��HD�HD�AHD��HD�� D���D�@ D�� D��HD�  D�>�D�� D�� D�  D�AHD��HD�� D�HD�@ D�� D��HD�  D�@ D�� D��HD�HD�AHD�� D��HD�HD�AHD�� D���D�  D�AHD�� D���D���D�>�D�� D���D�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D��HD�� D���D�>�D�� D�� D���D�AHD�� D�� D�HD�AHD�� D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D���D�>�D D�� D�  D�AHDÀ Dþ�D���D�@ DĂ�D��HD�HD�@ D�~�D�� D�HD�@ Dƀ Dƾ�D�  D�@ D�~�D�� D�HD�AHDȀ DȾ�D�  D�@ DɁHD�� D�  D�AHDʀ D�� D�HD�AHD˂�D�� D���D�@ D̀ D�� D�  D�AHD́HD��HD�HD�>�D�~�Dξ�D�HD�AHDπ DϾ�D�  D�@ DЀ D��HD���D�@ DсHD�� D�  D�AHDҀ DҾ�D���D�>�DӀ DӾ�D���D�@ DԀ DԾ�D�HD�@ DՀ D�� D�HD�@ DցHD�� D��qD�=qD׀ D�� D�  D�AHD؁HDؾ�D���D�>�Dـ D��HD�HD�AHDځHD�� D���D�@ DہHD�� D���D�>�D܀ D�� D���D�@ D݀ Dݾ�D�  D�>�D�~�D޾�D���D�@ D�~�D�� D�HD�@ D�� DྸD���D�@ D� D�� D���D�>�D�~�D�� D�HD�B�D� D�� D���D�>�D�~�D侸D���D�>�D�~�D徸D�  D�@ D�~�D�� D�  D�@ D� D��HD�  D�@ D�~�D�� D�  D�AHD�HD�� D�  D�@ D�HD�� D�  D�>�D� D�� D�  D�AHD� D�� D�  D�>�D�~�D���D���D�>�D� D��HD�  D�AHD� DﾸD���D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�@ D� D�D���D�@ D� D�� D�HD�AHD�HD��HD�  D�AHD��HD�� D�  D�>�D�� D�� D�  D�@ D��HD��D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�@ D���D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�z�@   @=p�@�  @��\@��R@�(�A   AG�A!G�A>�RA_\)A�Q�A��A�  A�Q�A�  A�Q�A�  A�  A��B  B  B(�B (�B(Q�B0Q�B8(�B@(�BH  BO�
BW�
B_�
Bh  Bp  Bx(�B�  B��
B�  B�{B�  B�  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�{B�{B��B��B�  B�  B�  B�  B�{B�  B��B��B��B��B��B��
C   C  C  C
=C
=C
  C  C
=C
=C
=C  C  C
=C��C  C  C 
=C!��C$  C&  C(  C*  C,  C.  C/�C2  C4
=C6
=C8  C9��C<
=C>
=C?��CB  CD  CF  CH
=CJ
=CL  CN  CP  CQ��CT  CV
=CX
=CZ  C\  C^  C`  Cb  Cc��Ce��Ch  Cj  Cl  Cn  Cp  Cr
=Ct  Cv
=Cx
=Cz
=C|
=C~
=C�  C�C�
=C�C���C���C�  C���C���C�  C�  C�  C�C�  C�  C�C���C�  C�  C�  C�  C�C�  C�  C�  C�  C�  C���C���C�  C���C���C�  C�C�  C�  C�  C�  C�C�C�C�  C���C���C���C���C���C�  C�  C���C���C���C���C�  C�  C���C���C���C�  C�  C���C���C���C�C�C�  C�  C�C�  C�C�C�  C�  C�C�C�C�  C���C���C�  C�C�C�C�C�  C���C���C�  C�  C���C���C�  C�C�  C�  C�C�C�C�  C�  C�  C���C���C���C�  C�  C�  C�  C�C�  C���C�  C�  C�  C���C���C�C�  C���C���C���C�  C�C�C�C�C�C�D �D }qD  D� D�qD}qD�qD}qD  D� D  D� D  D� D�D��D  D� D	  D	}qD
  D
� D
�qD� D�D� D�qD}qD�qD� D  D}qD�qD}qD  D��D�D� D�qD� D�D��D  D� D  D��D  D}qD�qD}qD�qD� D�D� D  D� D  D��D  D}qD�qD}qD  D� D   D }qD!  D!��D"�D"��D#  D#� D$�D$� D%  D%� D&  D&� D'�D'��D'�qD(� D)�D)��D*  D*}qD+  D+��D,  D,}qD-  D-� D.  D.� D.�qD/}qD0  D0}qD0�qD1� D1�qD2}qD2�qD3� D4�D4� D4�qD5}qD5�qD6� D7�D7� D8  D8� D9  D9� D:�D:� D:�qD;� D<  D<��D=�D=� D=�qD>}qD?  D?��D@  D@� DA  DA� DB�DB� DB�qDC}qDD  DD� DE  DE}qDF  DF��DG�DG� DH  DH��DI  DI}qDI�qDJ}qDJ�qDK}qDL  DL��DM  DM� DN  DN��DO�DO� DO�qDP� DQ  DQ� DR  DR� DS  DS� DT�DT��DU  DU��DV  DV� DV�qDW� DX�DX� DY  DY��DZ�DZ� D[  D[� D[�qD\}qD\�qD]}qD]�qD^}qD_  D_��D`�D`� Da  Da}qDa�qDb� Dc  Dc� Dd  Dd� De  De� De�qDf}qDg  Dg}qDh  Dh� Di  Di� Dj  Dj� Dj�qDk� Dl  Dl��Dm�Dm� Dn  Dn}qDn�qDo}qDp  Dp� Dq  Dq}qDq�qDr� Ds�Ds�Dt  Dt� Du�Du� Dv  Dv� Dw  Dw}qDx  Dx��Dy  Dy� Dz�Dz��D{�D{��D|  D|� D}�D}��D~�D~��D  D� D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�� D���D�>�D�}qD�� D�  D�@ D�� D���D�  D�AHD��HD��HD�  D�@ D��HD��HD�  D�@ D�� D���D�  D�@ D�� D��HD�  D�>�D��HD��HD�HD�@ D�~�D��HD�HD�>�D�� D��HD�  D�@ D�~�D���D�  D�@ D�~�D�� D�HD�@ D�� D�� D���D�>�D�� D��HD�  D�=qD�~�D���D���D�>�D�� D��HD�  D�@ D�� D��HD�HD�@ D��HD���D���D�@ D�~�D���D�  D�@ D�~�D�� D���D�>�D�� D��HD��D�AHD�~�D��qD�  D�AHD�� D��HD�  D�>�D�~�D���D���D�@ D��HD�� D�  D�@ D�� D���D�  D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�~�D�� D�  D�>�D�� D��HD�HD�AHD�~�D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�>�D�� D��HD�  D�@ D�� D���D���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�>�D�� D��HD�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�AHD��HD�� D���D�@ D��HD�� D���D�@ D��HD�D�  D�@ D��HD��HD�HD�AHD��HD�� D���D�@ D�� D��HD�  D�>�D�� D�� D�  D�AHD��HD�� D�HD�@ D�� D��HD�  D�@ D�� D��HD�HD�AHD�� D��HD�HD�AHD�� D���D�  D�AHD�� D���D���D�>�D�� D���D�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D��HD�� D���D�>�D�� D�� D���D�AHD�� D�� D�HD�AHD�� D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D���D�>�D D�� D�  D�AHDÀ Dþ�D���D�@ DĂ�D��HD�HD�@ D�~�D�� D�HD�@ Dƀ Dƾ�D�  D�@ D�~�D�� D�HD�AHDȀ DȾ�D�  D�@ DɁHD�� D�  D�AHDʀ D�� D�HD�AHD˂�D�� D���D�@ D̀ D�� D�  D�AHD́HD��HD�HD�>�D�~�Dξ�D�HD�AHDπ DϾ�D�  D�@ DЀ D��HD���D�@ DсHD�� D�  D�AHDҀ DҾ�D���D�>�DӀ DӾ�D���D�@ DԀ DԾ�D�HD�@ DՀ D�� D�HD�@ DցHD�� D��qD�=qD׀ D�� D�  D�AHD؁HDؾ�D���D�>�Dـ D��HD�HD�AHDځHD�� D���D�@ DہHD�� D���D�>�D܀ D�� D���D�@ D݀ Dݾ�D�  D�>�D�~�D޾�D���D�@ D�~�D�� D�HD�@ D�� DྸD���D�@ D� D�� D���D�>�D�~�D�� D�HD�B�D� D�� D���D�>�D�~�D侸D���D�>�D�~�D徸D�  D�@ D�~�D�� D�  D�@ D� D��HD�  D�@ D�~�D�� D�  D�AHD�HD�� D�  D�@ D�HD�� D�  D�>�D� D�� D�  D�AHD� D�� D�  D�>�D�~�D���D���D�>�D� D��HD�  D�AHD� DﾸD���D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�@ D� D�D���D�@ D� D�� D�HD�AHD�HD��HD�  D�AHD��HD�� D�  D�>�D�� D�� D�  D�@ D��HD��D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�@ D���D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AUl�AUt�AU�AU|�AU|�AU�AU�AU�AU�7AU�7AU�7AUx�AUp�AUt�AUhsAUhsAUl�AUhsAUp�AUl�AUp�AUl�AUl�AUl�AUp�AUx�AUx�AUx�AUx�AUx�AUx�AUx�AU|�AUx�AUx�AU�AU�7AUx�AU�AU�AU�AU�7AUx�AUt�AUt�AUl�AUhsAUp�AUt�AUt�AUl�AUhsAUhsAU�AU�AU�AUhsAUhsAU|�AU�AUt�AUl�AUt�AU�AUK�AU7LAU&�AU+AUO�AU\)AU`BAUx�AU�AU�7AU`BAUp�AU��AU��AUdZAUS�AUAUƨAU?}AU/AUC�AT��AT�DAS�hAS&�AS"�ARn�AR�AR��ASS�AR9XAQ��AQ|�AQ33AO�AOVAN�ANE�ANJAM�-AL��ALQ�AK��AK��AK33AJ�yAJ�AJ��AI��AIhsAJ�RAKdZAK�;AI��AHQ�AHI�AGt�AF��ADA�AC33AB�AA�AAhsA@�DA?oA>�jA> �A=p�A=O�A=33A=
=A<�jA<�DA<  A;oA:�!A:JA9?}A8ĜA8�\A8r�A8Q�A85?A7�A7��A7��A7`BA6�/A4�RA2ZA2  A/�A/G�A.ĜA-��A,�A+�#A+XA*�`A*(�A)|�A(��A'�7A'/A&��A&�9A&r�A& �A%�A$�A$��A$��A$$�A#��A#�TA#A#��A#�PA#S�A#O�A"�A"��A"��A"�+A"�A!��A ZA��A�At�Ap�AK�A�A�HA��A��AVA�7A7LA�A��A�`A��AĜA�9A�uAr�AjAbNA�mA�A�A�/A�DAQ�A1'A��A��A�PAdZAS�AO�A"�A��A�`A�Al�A�A��A��A��A�A�RAbNA{A�PAG�A;dA"�A�A��AbAp�A?}AVA��A�!AffA|�A
�HA
�uA
M�A	�wA	|�A	S�A��A��Av�A  A�A��AoA�+AS�A��A��A�A�A��AC�A�9A1'A �\@�A�@�r�@���@�Ĝ@�r�@��y@��/@�9X@�ƨ@��@�~�@��@��#@�&�@���@���@�/@��^@�E�@�33@�7L@���@�ȴ@�-@�hs@�%@�(�@ߕ�@���@��T@�hs@�?}@�&�@��/@�z�@�9X@ۥ�@�K�@�^5@ٲ-@�9X@�ƨ@ԋD@Ѻ^@�`B@У�@Ͼw@���@�S�@ɩ�@���@���@ȼj@��;@�ȴ@���@�7L@å�@�5?@��#@�@���@��@��9@�"�@�-@�x�@���@��@�?}@�I�@��@��R@���@��-@�hs@��@��@���@���@�J@���@�7L@��@���@��u@��w@�J@��7@��7@�x�@�hs@�/@���@��/@�Ĝ@���@��@��u@�z�@�bN@�b@�t�@�"�@�ȴ@�E�@�@��h@�hs@�hs@�hs@�X@�G�@��j@��P@���@�{@�b@��y@��!@�~�@�ff@�^5@�M�@�=q@�-@�$�@��@���@��@�V@�%@�%@�Ĝ@��@�Z@�(�@���@���@��@�x�@�7L@��j@�1'@��m@��@�V@���@�/@���@���@���@���@�Ĝ@��9@��u@��@�z�@�1'@�ƨ@��@�5?@��@��@��@�  @�ƨ@���@�K�@��!@�^5@�M�@�=q@�-@���@��@��@�1@�S�@��@��y@���@�V@��@��h@��D@��m@�
=@��!@��@���@�@���@��/@���@���@��/@�Ĝ@���@�Ĝ@��@���@�z�@�1@���@��@�|�@�l�@�\)@�+@���@��\@�~�@�J@��@��@���@��@�p�@�p�@�G�@���@�z�@�A�@�Z@��;@���@���@��P@�l�@�dZ@�|�@�l�@�ȴ@���@�n�@�^5@��@�?}@��@���@���@�Ĝ@�r�@�j@�bN@�Z@�Z@�Z@�(�@�w@\)@
=@~5?@}/@|�@z~�@yhs@x��@x  @w\)@wK�@w+@w+@w
=@v�R@vff@v$�@uO�@s�m@s�@s33@r��@q��@q�@pĜ@pbN@p1'@o�;@o��@o|�@o;d@o�@o+@o+@oK�@ol�@o|�@ol�@o\)@o
=@n��@nff@n{@m�T@m�@l�D@k��@k�F@kt�@kS�@kC�@kC�@kC�@k33@k33@ko@ko@kS�@k��@k�m@l1@l1@l(�@l9X@lj@lj@lZ@lI�@l(�@l1@kS�@j�@j�H@j~�@j=q@j�@jJ@i��@i��@h��@hb@g�@g��@g�w@g|�@g\)@g�@gK�@f�@e`B@cƨ@c�F@cƨ@c�
@c�
@c�F@c��@b��@a�^@a%@_�@_;d@_
=@^V@\I�@Y%@T�j@O�w@O�@Nȴ@Nff@N{@Nff@N��@N��@N$�@L�D@LI�@Lj@L�@K�
@Lj@NE�@N$�@M�h@M?}@L�@L�D@K�m@K�F@Kt�@Kt�@K"�@K��@M/@O�P@Q�@Qx�@Q��@Q�^@Q�@P��@P1'@O�;@M��@M�@MV@L��@L�/@L��@L�@LZ@L(�@L�@K�F@K"�@J��@J��@J�!@J�!@J�!@J��@J��@J~�@J=q@J-@J�@JJ@JJ@I�@I�#@I��@I��@I��@I�7@I�7@Ihs@IX@IG�@I&�@H��@H��@HĜ@HQ�@H1'@H  @H  @H  @Hb@H  @G�;@G�;@G��@G|�@G;d@G+@G+@G+@G+@G+@F��@F�@F�@Fȴ@Fȴ@F�R@F�+@Fff@Fff@F{@E��@E@E@E�-@E�-@E�-@E��@E��@E�@E?}@D��@D��@D�D@Dj@DZ@DZ@DZ@DI�@D�@C�
@C��@Ct�@CdZ@CdZ@CdZ@CdZ@Co@Co@Co@C@B��@B^5@B=q@B�@B�@B�@BJ@BJ@A��@A�@A�^@A��@A��@A��@A��@AX@A%@@��@@�`@@�9@@r�@@A�@@ �@@b@@b@@b@@b@@  @?�@?��@?�@?�P@?|�@?|�@?l�@?+@>�@>ȴ@>�+@>ff@>ff@>ff@>V@>5?@>5?@>{@=�h@=�@=V@<��@<��@<��@<�@<�D@;�F@;o@:�\@:�\@:~�@:n�@:^5@:-@9�@9�#@9�^@9��@9��@97L@8��@8��@8bN@81'@8b@8b@7�@7�;@7�;@7��@7��@7��@7��@7�@7�P@7\)@7
=@6�y@6�R@6��@6��@6v�@6v�@6V@65?@6$�@6@5�@5@5�h@5�h@5��@5��@5�-@5�-@5�-@5��@5��@5p�@5O�@4��@4�@4�@4�@4�/@4�/@4�/@4�/@4��@4��@4��@4�j@4�@4��@4�D@4z�@4�D@4Z@49X@49X@49X@49X@49X@4(�@41@3�m@3�
@3�
@3ƨ@3�F@3S�@2��@2��@2~�@2n�@2^5@2M�@2J@1�^@1��@1�7@1�7@1x�@1�7@1x�@1x�@17L@1�@1�@0��@0��@0��@0��@0��@0Ĝ@0�9@0��@0�u@0Q�@0b@0  @/�@/�;@/�;@/��@/�w@/�@/�@/|�@/\)@/;d@/�@.�y@.�y@.�@.ȴ@.ȴ@.�R@.��@.�+@.V@.$�@.$�@.{@.@.@.@-�@-�@-�@-�T@-�T@-`B@-?}@-?}@-/@-�@-�@-V@,�@,�/@,�/@,��@,�@,�@,�@,��@,��@,��@,�D@,j@,Z@,I�@,9X@,1@+��@+�m@+��@+t�@+S�@+C�@+C�@+33@+o@+o@+@+@+@*�@*��@*��@*��@*�!@*�!@*�!@*�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AUl�AUt�AU�AU|�AU|�AU�AU�AU�AU�7AU�7AU�7AUx�AUp�AUt�AUhsAUhsAUl�AUhsAUp�AUl�AUp�AUl�AUl�AUl�AUp�AUx�AUx�AUx�AUx�AUx�AUx�AUx�AU|�AUx�AUx�AU�AU�7AUx�AU�AU�AU�AU�7AUx�AUt�AUt�AUl�AUhsAUp�AUt�AUt�AUl�AUhsAUhsAU�AU�AU�AUhsAUhsAU|�AU�AUt�AUl�AUt�AU�AUK�AU7LAU&�AU+AUO�AU\)AU`BAUx�AU�AU�7AU`BAUp�AU��AU��AUdZAUS�AUAUƨAU?}AU/AUC�AT��AT�DAS�hAS&�AS"�ARn�AR�AR��ASS�AR9XAQ��AQ|�AQ33AO�AOVAN�ANE�ANJAM�-AL��ALQ�AK��AK��AK33AJ�yAJ�AJ��AI��AIhsAJ�RAKdZAK�;AI��AHQ�AHI�AGt�AF��ADA�AC33AB�AA�AAhsA@�DA?oA>�jA> �A=p�A=O�A=33A=
=A<�jA<�DA<  A;oA:�!A:JA9?}A8ĜA8�\A8r�A8Q�A85?A7�A7��A7��A7`BA6�/A4�RA2ZA2  A/�A/G�A.ĜA-��A,�A+�#A+XA*�`A*(�A)|�A(��A'�7A'/A&��A&�9A&r�A& �A%�A$�A$��A$��A$$�A#��A#�TA#A#��A#�PA#S�A#O�A"�A"��A"��A"�+A"�A!��A ZA��A�At�Ap�AK�A�A�HA��A��AVA�7A7LA�A��A�`A��AĜA�9A�uAr�AjAbNA�mA�A�A�/A�DAQ�A1'A��A��A�PAdZAS�AO�A"�A��A�`A�Al�A�A��A��A��A�A�RAbNA{A�PAG�A;dA"�A�A��AbAp�A?}AVA��A�!AffA|�A
�HA
�uA
M�A	�wA	|�A	S�A��A��Av�A  A�A��AoA�+AS�A��A��A�A�A��AC�A�9A1'A �\@�A�@�r�@���@�Ĝ@�r�@��y@��/@�9X@�ƨ@��@�~�@��@��#@�&�@���@���@�/@��^@�E�@�33@�7L@���@�ȴ@�-@�hs@�%@�(�@ߕ�@���@��T@�hs@�?}@�&�@��/@�z�@�9X@ۥ�@�K�@�^5@ٲ-@�9X@�ƨ@ԋD@Ѻ^@�`B@У�@Ͼw@���@�S�@ɩ�@���@���@ȼj@��;@�ȴ@���@�7L@å�@�5?@��#@�@���@��@��9@�"�@�-@�x�@���@��@�?}@�I�@��@��R@���@��-@�hs@��@��@���@���@�J@���@�7L@��@���@��u@��w@�J@��7@��7@�x�@�hs@�/@���@��/@�Ĝ@���@��@��u@�z�@�bN@�b@�t�@�"�@�ȴ@�E�@�@��h@�hs@�hs@�hs@�X@�G�@��j@��P@���@�{@�b@��y@��!@�~�@�ff@�^5@�M�@�=q@�-@�$�@��@���@��@�V@�%@�%@�Ĝ@��@�Z@�(�@���@���@��@�x�@�7L@��j@�1'@��m@��@�V@���@�/@���@���@���@���@�Ĝ@��9@��u@��@�z�@�1'@�ƨ@��@�5?@��@��@��@�  @�ƨ@���@�K�@��!@�^5@�M�@�=q@�-@���@��@��@�1@�S�@��@��y@���@�V@��@��h@��D@��m@�
=@��!@��@���@�@���@��/@���@���@��/@�Ĝ@���@�Ĝ@��@���@�z�@�1@���@��@�|�@�l�@�\)@�+@���@��\@�~�@�J@��@��@���@��@�p�@�p�@�G�@���@�z�@�A�@�Z@��;@���@���@��P@�l�@�dZ@�|�@�l�@�ȴ@���@�n�@�^5@��@�?}@��@���@���@�Ĝ@�r�@�j@�bN@�Z@�Z@�Z@�(�@�w@\)@
=@~5?@}/@|�@z~�@yhs@x��@x  @w\)@wK�@w+@w+@w
=@v�R@vff@v$�@uO�@s�m@s�@s33@r��@q��@q�@pĜ@pbN@p1'@o�;@o��@o|�@o;d@o�@o+@o+@oK�@ol�@o|�@ol�@o\)@o
=@n��@nff@n{@m�T@m�@l�D@k��@k�F@kt�@kS�@kC�@kC�@kC�@k33@k33@ko@ko@kS�@k��@k�m@l1@l1@l(�@l9X@lj@lj@lZ@lI�@l(�@l1@kS�@j�@j�H@j~�@j=q@j�@jJ@i��@i��@h��@hb@g�@g��@g�w@g|�@g\)@g�@gK�@f�@e`B@cƨ@c�F@cƨ@c�
@c�
@c�F@c��@b��@a�^@a%@_�@_;d@_
=@^V@\I�@Y%@T�j@O�w@O�@Nȴ@Nff@N{@Nff@N��@N��@N$�@L�D@LI�@Lj@L�@K�
@Lj@NE�@N$�@M�h@M?}@L�@L�D@K�m@K�F@Kt�@Kt�@K"�@K��@M/@O�P@Q�@Qx�@Q��@Q�^@Q�@P��@P1'@O�;@M��@M�@MV@L��@L�/@L��@L�@LZ@L(�@L�@K�F@K"�@J��@J��@J�!@J�!@J�!@J��@J��@J~�@J=q@J-@J�@JJ@JJ@I�@I�#@I��@I��@I��@I�7@I�7@Ihs@IX@IG�@I&�@H��@H��@HĜ@HQ�@H1'@H  @H  @H  @Hb@H  @G�;@G�;@G��@G|�@G;d@G+@G+@G+@G+@G+@F��@F�@F�@Fȴ@Fȴ@F�R@F�+@Fff@Fff@F{@E��@E@E@E�-@E�-@E�-@E��@E��@E�@E?}@D��@D��@D�D@Dj@DZ@DZ@DZ@DI�@D�@C�
@C��@Ct�@CdZ@CdZ@CdZ@CdZ@Co@Co@Co@C@B��@B^5@B=q@B�@B�@B�@BJ@BJ@A��@A�@A�^@A��@A��@A��@A��@AX@A%@@��@@�`@@�9@@r�@@A�@@ �@@b@@b@@b@@b@@  @?�@?��@?�@?�P@?|�@?|�@?l�@?+@>�@>ȴ@>�+@>ff@>ff@>ff@>V@>5?@>5?@>{@=�h@=�@=V@<��@<��@<��@<�@<�D@;�F@;o@:�\@:�\@:~�@:n�@:^5@:-@9�@9�#@9�^@9��@9��@97L@8��@8��@8bN@81'@8b@8b@7�@7�;@7�;@7��@7��@7��@7��@7�@7�P@7\)@7
=@6�y@6�R@6��@6��@6v�@6v�@6V@65?@6$�@6@5�@5@5�h@5�h@5��@5��@5�-@5�-@5�-@5��@5��@5p�@5O�@4��@4�@4�@4�@4�/@4�/@4�/@4�/@4��@4��@4��@4�j@4�@4��@4�D@4z�@4�D@4Z@49X@49X@49X@49X@49X@4(�@41@3�m@3�
@3�
@3ƨ@3�F@3S�@2��@2��@2~�@2n�@2^5@2M�@2J@1�^@1��@1�7@1�7@1x�@1�7@1x�@1x�@17L@1�@1�@0��@0��@0��@0��@0��@0Ĝ@0�9@0��@0�u@0Q�@0b@0  @/�@/�;@/�;@/��@/�w@/�@/�@/|�@/\)@/;d@/�@.�y@.�y@.�@.ȴ@.ȴ@.�R@.��@.�+@.V@.$�@.$�@.{@.@.@.@-�@-�@-�@-�T@-�T@-`B@-?}@-?}@-/@-�@-�@-V@,�@,�/@,�/@,��@,�@,�@,�@,��@,��@,��@,�D@,j@,Z@,I�@,9X@,1@+��@+�m@+��@+t�@+S�@+C�@+C�@+33@+o@+o@+@+@+@*�@*��@*��@*��@*�!@*�!@*�!@*�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�JB�JB�PB�\B�\B�VB�PB�PB�VB�VB�VB�JB�DB�DB�=B�DB�=B�=B�DB�=B�=B�=B�=B�=B�=B�DB�DB�DB�=B�=B�=B�=B�=B�=B�=B�=B�DB�=B�=B�=B�=B�DB�=B�7B�7B�7B�7B�7B�7B�7B�7B�1B�1B�=B�=B�=B�1B�7B�7B�7B�7B�1B�7B�7B�%B�B�B�B�%B�+B�+B�1B�1B�7B�+B�+B�=B�DB�+B�%B�JB�DB�B�B�B� By�Bn�BhsBiyB_;B`BBe`BiyB[#BS�BP�BK�B7LB1'B/B)�B'�B#�B�B�BhBoBoBoBuB�B�B&�BN�B\)BcTBT�BM�BT�BO�BH�B>wB8RB49B.B(�B!�B�BuBPB1B%BBBB��B��B��B��B�B�sB�mB�mB�sB�sB�B�B�B�B�fB�;BǮB�B��B�oB�7B�Bx�Bp�Be`B_;B[#BR�BK�BC�B<jB9XB:^B9XB8RB6FB49B1'B0!B.B+B)�B(�B'�B&�B%�B#�B"�B�B�B�B�B�BoB	7B%BBBBBBBB  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�fB
�NB
�HB
�BB
�;B
�5B
�/B
�)B
�)B
�#B
�#B
�#B
�B
�B
�B
��B
��B
��B
�qB
�dB
�FB
�'B
�!B
�B
�B
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
�\B
�=B
�+B
�B
�B
�B
�7B
�1B
�%B
�B
�B
�B
� B
z�B
v�B
q�B
p�B
o�B
m�B
jB
hsB
e`B
aHB
]/B
Q�B
B�B
F�B
G�B
I�B
H�B
E�B
?}B
>wB
<jB
9XB
8RB
6FB
49B
1'B
(�B
&�B
#�B
�B
\B
1B
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�`B	�NB	�BB	�#B	�B	��B	��B	��B	ǮB	ÖB	�qB	�XB	�XB	�^B	�^B	�^B	�XB	�LB	�LB	�FB	�?B	�9B	�9B	�9B	�9B	�3B	�'B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�'B	�-B	�-B	�'B	�'B	�-B	�-B	�3B	�9B	�3B	�9B	�?B	�FB	�FB	�LB	�LB	�RB	�RB	�LB	�LB	�^B	�dB	�qB	�}B	��B	��B	B	ÖB	B	B	B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�;B	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
%B
+B
1B

=B
JB
JB
JB
DB
DB
DB
DB
DB
DB

=B
DB
DB
PB
VB
bB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
\B
VB
\B
\B
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
-B
0!B
6FB
>wB
B�B
D�B
E�B
G�B
I�B
I�B
J�B
I�B
I�B
L�B
N�B
N�B
O�B
O�B
P�B
R�B
S�B
S�B
VB
ZB
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
`BB
aHB
bNB
cTB
cTB
dZB
dZB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
jB
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
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
y�B
z�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�1B
�1B
�1B
�=B
�=B
�DB
�DB
�PB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�hB
�oB
�oB
�oB
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
�B
�B
�B
�!B
�-B
�-B
�-B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�9B
�9B
�FB
�LB
�RB
�XB
�^B
�^B
�dB
�dB
�dB
�jB
�qB
�qB
�wB
��B
ÖB
ĜB
ĜB
ĜB
ŢB
ƨB
ƨB
ǮB
ǮB
ȴB
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
��B
��B
��B
��B
��B
��B
�B
�B
�
B
�
B
�
B
�B
�B
�B
�B
�#B
�#B
�)B
�/B
�5B
�;B
�BB
�BB
�HB
�NB
�NB
�TB
�TB
�`B
�fB
�fB
�mB
�sB
�sB
�sB
�sB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BBBBBBBBBBBB%B%B%B%B+B%B+B+B1B1B1B	7B	7B	7B	7BDBJBJBJBPBPBPBPBVBVBVB\B\BbBbBbBbBb1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�sB�NB�6B�fB�PB�5B�PB�WB�zB�dB�=B�^B�EB�3B�IB�.B�IB�6B�HB�?B�>B�3B�+B�?B�CB�AB�?B�>B�>B�3B�JB�>B�)B�-B�lB�*B�=B�3B�:B�jB�KB�9B�MB�CB�!B�.B�7B�PB�BB�1B��B�8B�BB�wB�2B� B�"B�aB�LB�B�B��B�hB�MB�B��B�B�B��B�B�#B��B� B��B�B��B�TB�B�;B��B�cB��B��B�BB|yBo�Bh�Bk$B_B_]Bd0Bl#B\�BT�BQ�BO�B8�B1�B0�B*�B) B&�B�B�B�BvB:B�B�B"B�B#�BL�B[ Bh�BY@BNBW-BR�BOBA�B9~B6�B/�B+qB%�B�B)B2B�BtB�B�B�B �B�^B��B��B��B��B�B��B��B��B��B��B�B�B�B�
B�B��B��B�{B��B�&B{Bs�Bf�B`�B]=BT�BN.BF�B=�B9�B;*B:B9IB8B5�B1�B0�B/|B+�B*;B)VB(EB'7B&|B#�B$B BB
B�BB�B
�B�BKB/B�B�B�BhB`B �B&B
��B
�QB
�DB
�B
�B
�"B
�(B
�XB
�UB
�B
�B
�xB
�<B
�B
�B
�:B
��B
ߢB
��B
ݲB
��B
ܟB
�TB
�<B
۠B
ڔB
�YB
�iB
�:B
�*B
�BB
�B
��B
�-B
��B
� B
�B
��B
��B
�-B
�HB
�(B
��B
�B
��B
�NB
�9B
��B
�|B
��B
�-B
�!B
�"B
�B
��B
��B
��B
�.B
��B
�B
�nB
�]B
��B
�B
|�B
z9B
r�B
q+B
p�B
o8B
kbB
i�B
gB
cB
b9B
X�B
B|B
FaB
G�B
JBB
KB
H�B
@�B
?2B
=�B
:B
8�B
6�B
5bB
4JB
*�B
("B
(�B
�B
�B
HB
2B	��B	�wB	�XB	�~B	�B	��B	��B	��B	�[B	��B	��B	�B	�B	��B	�\B	�B	��B	�pB	�iB	� B	��B	�B	̉B	��B	�6B	ǪB	�B	��B	�aB	�yB	��B	��B	�
B	��B	�QB	��B	�hB	��B	�jB	��B	��B	��B	�tB	��B	�0B	�4B	� B	�RB	�ZB	�B	�B	�B	��B	�7B	�B	�B	��B	�@B	��B	�RB	�bB	��B	��B	�\B	��B	�B	�sB	��B	��B	��B	�B	� B	��B	��B	��B	��B	��B	��B	��B	�6B	��B	�.B	�7B	�wB	�wB	��B	��B	��B	��B	��B	��B	��B	�hB	��B	��B	��B	�GB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�VB	��B	��B	��B	��B	�B	�B	�B	�B	��B	�	B	�3B	��B	�AB	��B	��B	�NB	�~B	��B	��B	��B	�dB	��B	��B	��B	��B	��B	�B	��B	��B	�LB	�B	��B	�CB	�B	�%B	��B	��B	�6B	� B	�`B	��B	�_B	� B	��B	� B	�qB	�B	��B	��B	�B	�LB	�<B	�oB	�ZB	�RB	��B	��B	��B	�RB	��B	��B	�xB	�&B	�UB	�"B	�.B	�B	�B	�BB	�B	�*B	�CB	�5B	�pB	��B	��B	�jB	�|B	�SB	�PB	��B	�B	�TB	�TB	��B	�fB	�JB	�zB	��B	�bB	�RB	��B	��B	�B	��B	�CB	�B	��B	�oB	�{B	��B	��B	�jB	��B	ÄB	��B	��B	·B	�B	��B	��B	��B	��B	��B	�(B	��B	��B	��B	��B	��B	�$B	�KB	�.B	�$B	φB	ϬB	�YB	�zB	˪B	�ZB	�CB	�8B	��B	��B	��B	��B	�B	�B	�B	ѐB	��B	�EB	�5B	�cB	ѺB	�_B	�BB	�LB	�1B	�HB	�(B	�ZB	�MB	�:B	�B	�(B	�B	� B	�1B	�LB	�PB	߅B	�B	�{B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�qB	�B	�}B	��B	��B	��B	��B	��B	��B	�B	�B	�B
 (B
�B
sB
DB
�B

wB
jB
^B
`B
�B
B
�B
gB
bB
XB

zB
dB
�B
6B
�B
�B
�B
�B
uB
xB
�B
�B
�B
KB
HB
/B
uB
*B
�B
@B
DB
7B
�B
.B
�B
�B
�B
�B
@B
ZB
�B
B
�B
�B
�B
�B
�B
2B
KB
&B
)bB
);B
)?B
)OB
)yB
)%B
).B
)B
*:B
,�B
.�B
4uB
=<B
B:B
DvB
E�B
H2B
JB
JB
KB
KrB
J4B
L�B
N�B
N�B
O�B
PB
Q,B
SB
TB
TTB
V�B
ZjB
],B
]?B
]3B
]3B
]@B
]2B
]PB
^mB
`TB
aXB
b_B
cXB
cpB
diB
d�B
ffB
fvB
ftB
flB
g�B
g�B
h�B
h�B
i�B
i�B
i�B
j�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
uB
t�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
xB
yB
zB
{B
|B
{�B
|�B
|�B
} B
}B
}&B
~,B
�B
�B
�B
�B
�B
�EB
�B
�B
�B
�GB
�`B
�7B
�<B
�%B
�(B
�3B
�,B
�6B
�4B
�QB
�8B
�1B
�>B
�4B
�hB
�vB
�HB
�QB
�kB
��B
�yB
�sB
�jB
�`B
�bB
�bB
�oB
�rB
�~B
��B
��B
�}B
�tB
��B
��B
��B
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
�B
��B
��B
��B
��B
��B
�B
�pB
�WB
�HB
��B
� B
�B
�B
�B
�+B
�B
�B
�B
�B
�UB
�TB
�B
�uB
�OB
�EB
�/B
�KB
�@B
�4B
�BB
�4B
�5B
�<B
�VB
�XB
�lB
��B
�mB
�zB
�gB
�lB
�B
�fB
�B
��B
�|B
��B
��B
��B
��B
×B
ĐB
ĝB
đB
ţB
ƨB
ƻB
ǳB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�B
�B
�!B
�B
�B
�B
�B
�cB
ـB
�DB
�>B
�-B
�0B
�<B
�fB
�xB
�YB
�QB
�HB
�WB
�@B
�^B
�XB
�B
�|B
�iB
�B
�B
�sB
�uB
�tB
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
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�\BBBBBBB/B&BB+B7B&B)B4B)B*B8BCB@B>BABXB	EB	FB	2B	�BaB\BLB[BfBQB\BTBWBhBvBfBaBkBbBcBcBc1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#��<#�e<#��<#ף<#�r<#��<#�
<#�D<#�&<#�<#��<#�<#�0<#�<#�<<#��<#�{<#׺<#�<#�0<#�i<#�<#�<#�X<#�<#�<#�<#�<#�<#�<#�<#�X<#׎<#�<#�C<#��<#��<#�$<#�
<#�X<#�<#�r<#ף<#�<#؄<#�{<#؄<#�I<#�
<#��<#�i<#�
<#�<#�<#�<#�N<#�<#�E<#�c<#�l<#�c<#��<#��<$a<#�<#�<#׎<#�<#��<#�<#��<#��<#ף<#�)<#ܯ<#�g<#��<$2G<#�+<$��<#׺<%'<#�<#��<$I�<%F<)N<$�<#�<%��<#�o<$v�<$�!<)K?<%��<$�<$�<0�t<%��<$6�<%��<$=<$�7<)�<$��<&�n<#�N<$��<$T�<#�E<#��<)q<$t <+��<&�A<$��<8g~<1p[<#�N<'�<)7,<@��<+><$��<(n�<%�d<(�</��<$�B<&6<&�%<$<<#�<$�<$b�<$*<%��<(��<$�<&L�<'��<%^�<$}<#�<#�<#�g<%S�<#�N<#ا<$$<%��<<]"<Ak<&!�<;�?<'�<%�`<+	<'d<*��<%�<%X
<'A><&�.<(I�<+�<$ʾ<$'<$U�<$E<$��<&,f<%�d<$<<$�<%`�<$
<#�&<#�(<#�<#�<$k<#�r<%F<#��<#��<#�<$�<%��<- 6<&!�<$aD<#ޫ<#؄<#��<$k<$&<#�W<#��<$�X<'|<$��<#�Q<#�m<#�<#�J<#�J<#�J<#��<#�N<#�D<#��<%͍<B�<%&<$?[<$�b<$0.<#�g<$+<$a<$F9<$�<#�^<#��<$�<$<<#�U<#�5<1K�<9 d<&4p<$�<(��<*K8<$J�<$�<$�<%�R<$R'<#ܯ<#�<#ޫ<$t <'x�<&��<$�<$�<#�<$j|<$��<)SQ<&D�<$v�<$j|<%�`<$Z�<$�<$��<$6�<$�<%,#<#�4<$	<'�<&9�<,�?<$��<$�<$�X<%��<$t <$�<%�!<&$h<6�l<D�<<#�$<#�l<#��<$r<(j<*,�<$�B<$A�<%�<$<<$�<#��<$�`<+Z�<&`8<% �<5�,<7Z�<3�<+"�<'|<%�y<%��<$�<$.<%&<$� <%�<%@�<$J�<#�!<#�8<$p<$�<#��<$t <$%<%S�<$��<'k�<$��<4��<0�E<$I�<$�w<%�d<0Z�<)7,<(�,<$��<#�D<#�H<%@�<&�<%�d<$�j<(4<'r#<$�<#�^<#�<$F9<$T�<'�<%��<$��<$��<*�><(g?<%��<%:<%.+<%Q�<#��<$�<%X
<%8j<$��<$Z<$<<$+<$<<#��<#�&<$<<%>�<(��<$^�<#׎<#�r<#��<#��<#�<#�&<#��<#�&<#�<#�<#��<#�J<$F<$��<$r<$�<$c�<$\"<#�5<#�<#�i<#�&<#��<#ܯ<$�<&q<%p<$�k<+h�<&h�<#��<#�<#�<#��<#��<#�]<#��<#��<#ٛ<$B�<$|d<#�<#��<#��<#�N<#�H<#�<#��<$}�<%$<%`�<$C�<$�<$T�<$t <$a<$8�<&n4<$ح<$A�<$�<#��<#�I<#�X<#؄<#��<#�<#�*<#��<$/<$9�<$�1<%�J<'��<#�<#�C<$\"<#�<#�<$f<$��<$r<#��<#�r<#�+<$Z<$�.<$@|<$�J<$�q<#��<#�5<$�<#��<#��<$|d<%�b<$�J<%S�<$"2<$z�<$�<#��<#�5<$��<#�*<#�<#�<#�<#�<#؄<#��<#�<#�<$:�<#�W<#�e<#�<#��<#��<#�5<$m,<#�8<#ۮ<$8�<#��<#��<#�l<$�<#��<#�{<#�!<$�<$Sa<#��<#׺<$N�<#��<#��<#ا<#�<#�<#��<#��<$��<#�<#�4<#��<$�<%8j<#�M<#�J<#��<#�8<$�<#��<#�C<#׺<#�<#�I<#�"<$�<#�N<#�4<$2G<$^�<$�<&�<$|d<$%<$f<$.<#�<#�]<#�<<#�]<#�<#�<#�U<$0.<$��<#�(<#�<$v<$aD<#�a<#�<#�<#�8<#�&<#��<#�<#��<#��<#�0<#�<#�C<#׺<#�<#��<#�c<#�<#�W<#�8<#�<#�^<#��<$MO<$�<#�<#�<#��<#�$<#�&<#�{<#�<#�0<#��<#�0<#�8<#ܯ<#��<#�$<#�<#�X<#׎<#�o<#�<#�<#��<#��<#��<$�<#�<#��<#�<#�N<#�*<#�C<#؄<#�<$XX<$p<#��<#��<#�C<#�e<#�*<#�e<#�<#��<$�.<%�<#�<#�&<#�I<#�0<#�D<#�<$L<$:�<$!><$z�<$F<#�^<$+<%��<)3-<-m<.��<$�<#�M<#�<#��<#ߜ<#�8<#؄<#�	<%�<#�N<#׎<#�U<#�<#��<%k�<#�*<#��<#�<#�M<#�5<$a<#��<#��<#׎<#��<#��<$�-<&e�<%�<#�<#�r<#�
<$.<#��<#��<#�<&!�<$p<#�$<#��<#ڑ<#ף<#ڑ<#�l<#ۮ<#�c<#��<$v<#�!<#�<#��<#�<#�<#��<#�<#�]<#��<#�<#��<#��<#�<#�o<#׺<#�l<#�
<#��<#ף<#�&<#ٛ<#ا<#׎<#�]<#ۮ<#�I<#�^<#�m<#��<#�l<#�<#�<#�&<#�<#��<#�
<#�$<#�<#�E<#��<#�<<#�<#�&<#�<#ܯ<#ٛ<#�<#�<#�0<#�C<#�<#��<#�<<#�<#��<#�{<#�<#�C<#�<#�<#�$<#�<#�]<#��<#��<#��<#��<#ٛ<#��<#�
<#�<#�<#�l<#��<#��<#��<#��<#�<#�<#�&<#�<#�
<#�<#�<#�<#�<#��<#ٛ<#�&<#�I<#ף<#�0<#��<#׺<#��<#׎<#�&<#�$<#�<#�E<#�<#�i<#�C<#ۮ<#�<#�+<#ٛ<#ף<#�<#�&<#�&<#�$<#؄<#�o<#�*<#��<#ף<#�<#�$<#��<#�<#��<#��<#��<#�
<#�<#�c<#��<#�&<#�]<$ �<#�N<#�X<#�<#�<#�<#�$<#�<$7�<$�<#�)<#�
<#��<#��<#��<#��<#�<#ף<#�o<#׺<#؄<#�5<#�<#�I<#�<#�<#��<#�<#��<#׎<#�<#׺<#�<#�<#�<#ٛ<#��<#��<#��<#�]<#��<#׺<#ף<#�]<#�<#�D<#��<#�<#�<#�c<#��<#ۮ<#�<#�{<#�<#�i<#�<#�
<#�$<#�<#�+<#ٛ<#�&<#׺<#�<#�<#׺<#�<#�<#�<#׺<#�<#�<#�<#׎<#��<#׎<#׺<#�0<#��<#�<#�<#�
<#�<#�<#��<#��<#ٛ<#�<#�<#��<#�C<#�<#��<#ۮ<#�]<#�X<#׎<#�$<#�E<#�<#��<#׺<#�&<#׺<#ף<#��<#�<#��<#�o<#�<#��<#��<#�
<#�<#�<#��<#ף<#��<#׺<#�<#�<#��<#��<#׺<#�&<#ף<#�<#�i<#�<#�l<#��<#�<#�<#��<#�<#׎<#��<#�<#ף<#��<#ٛ<#ۮ<#��<#�<#׺<#�{<#�
<#�
<#�<#�
<#�<#��<#�&<#��<#��<#�<#�X<#׎<#�<#׎<#ٛ<#�C<#�
<#�<#��<#�<#�<#׺<#�<#�<#�$<#��<#�c<#׎<#��<#ۮ<#ף<#׺<#�<#�g<#ٛ<#�<#�<#��<#؄<#�<#�{<#�<#�<#�<#�*<#�X<#�<#�I<#�
<#�<#�<#�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = CTM_ADJ_PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                              None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment;OW: r =1(+/-0.0001), vertically averaged dS =-0.005(+/-0.002),                                                                                                             SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT.; No significant drift detected in conductivity                                                                                                                                   202205040000002022050400000020220504000000  AO  ARGQQCPL                                                                    20210217002727  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210217002727  QCF$                G�O�G�O�G�O�0               WHOIARSQWHQCV0.5                                                                20211004000000  QC                  G�O�G�O�G�O�                WHOIARSQCTM V1.0                                                                20220503000000  IP                  G�O�G�O�G�O�                WHOIARCAOWC V2.0ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     20220504000000  IP                  G�O�G�O�G�O�                