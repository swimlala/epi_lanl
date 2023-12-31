CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-30T22:31:05Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        x  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [X   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  c8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɀ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x 0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x f�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230730223105  20230730223105  5905275 5905275 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7316                            7316                            2B  2B  AA  SOLO_II                         SOLO_II                         8644                            8644                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�;�-!݃@�;�-!݃11  @�;�`�'�@�;�`�'�@,�3�ti@,�3�ti�c�x���c�x��11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?�  @�@@  @xQ�@�p�@�  @�  A ��A��A   A,��A@  A^�RA�  A�  A�  A�  A�  A�Q�A�  A�  B (�B�
B�
B�
B   B(Q�B0(�B7�
B?�
BG�
BO�
BW�
B_�
Bh  Bp(�Bx  B�  B�  B�  B��B�  B�{B�  B�  B�  B�  B�  B�  B�{B��B��
B��B�  B�  B�(�B�(�B��B�  B�{B�{B�(�B�{B�  B�  B�  B�  B�  B��B��C  C  C  C
=C
  C��C��C  C  C  C��C  C  C  C  C   C"  C$  C&  C'��C*  C,  C.  C0  C1��C4
=C6
=C7��C:  C<
=C>  C?�CA��CD  CF  CH  CJ{CL{CN{CO��CQ��CT  CV  CX
=CZ
=C\
=C^
=C`
=Cb
=Cd
=Cf
=Ch  Cj
=Ck��Cm��Cp  Cr  Ct  Cv  Cx
=Cz{C|
=C~  C�  C�  C�
=C�C�  C�  C�  C�  C�C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�  C�  C�  C���C�  C�C�  C���C�  C�
=C�C�C�  C���C���C���C���C�C�C�C�  C���C�  C�C�C�C���C���C�  C�  C�C�C�  C�  C�  C�  C�C�C���C�  C�
=C�C���C�C�  C�  C�
=C�
=C�  C�C�
=C�C�C���C���C���C���C���C�  C�C�  C���C�  C�C�
=C�C�  C���C���C�  C���C���C�  C�
=C�C�  C�
=C�C�  C�
=C�\C�C�  C�
=C�C�  C���C���C���C���C�  C�  C���C���C���C���C�C�  C�  C�  C�C�
=C���C���C���C���C�  C�C�
=D   D ��D�D� D�D��D  D� D�D�D�D� D�qDz�D  D�D�qD}qD	�D	}qD
  D
�D�D��D�D� D  D}qD�qD}qD��D� D  DxRD�RD}qD  D}qD  D�D�D� D�qD}qD��D� D�D� D�qD}qD�qD��DD�D�D� D  D�DD� D  D� D�D� D   D ��D!�D!� D"  D"��D#�D#��D#�qD$}qD$�qD%� D&  D&}qD'�D'� D'�qD(}qD(�qD)� D*  D*� D+  D+� D,  D,� D,�qD-}qD-�qD.}qD.�qD/� D0�D0}qD0�qD1� D2  D2�D3�D3��D4  D4}qD4�qD5}qD6  D6��D7  D7}qD7�qD8��D9�D9� D9�qD:}qD:�qD;� D<  D<��D=�D=��D>  D>}qD?�D?�D@�D@��DA�DA��DB�DB� DC  DC� DD  DD� DD�qDE}qDE�qDF� DF�qDG� DG�qDH� DI�DI��DJ  DJ}qDK  DK� DL  DL}qDM  DM� DN  DN}qDN��DOz�DO�qDP}qDQ  DQ��DR�DR� DS  DS� DT  DT� DU  DU� DV�DV��DW�DW��DX�DX��DY�DY��DZ  DZ� D[�D[�D\D\��D]�D]� D]��D^}qD^�qD_}qD`  D`� Da  Da� Db  Db� Dc  Dc� Dc�qDd� De�De��Df�Df}qDf�qDg��Dh  Dh}qDi�Di��Dj  Dj}qDj�qDk}qDl  Dl��Dm  Dm� Dn  Dn}qDn�qDo}qDp  Dp� Dp�qDq� Dr�Dr��Dr�qDs� Dt  Dt� DuDu�Dv�Dv� Dv�qDw� Dx  Dx}qDx�qDy}qDy��Dzz�Dz��D{� D|D|�D}  D}z�D}�qD~}qD~�qD� D�HD�AHD�� D���D���D�AHD��HD��HD�HD�AHD��HD�� D���D�@ D��HD�� D���D�@ D��HD��HD��D�AHD�� D��HD��D�B�D��HD��HD�HD�AHD�� D�� D�  D�>�D�~�D��HD�  D�=qD�� D�D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�@ D�~�D�� D�  D�@ D���D�� D���D�AHD�� D�� D���D�AHD���D��HD�  D�>�D�� D��HD�  D�>�D�� D��HD�  D�>�D�� D�� D���D�@ D�� D�� D�HD�>�D�}qD���D�  D�AHD��HD�� D���D�>�D�~�D��HD��D�@ D�~�D���D���D�>�D�� D��HD�  D�>�D�� D�� D���D�@ D�~�D���D�  D�@ D�� D���D�  D�AHD�~�D�� D�HD�@ D�� D��HD���D�@ D��HD�� D���D�>�D�� D�D�HD�@ D�� D�� D�HD�@ D�~�D���D���D�>�D�~�D�� D�  D�>�D�� D�� D�HD�AHD�~�D���D�  D�AHD�� D���D�  D�>�D�}qD�� D���D�>�D�� D���D��qD�>�D�~�D���D���D�>�D�~�D��HD���D�=qD�� D��HD�  D�>�D�~�D��qD���D�AHD��HD�D��D�C�D��HD�� D�HD�B�D��HD���D�  D�B�D���D��HD���D�=qD�}qD���D��qD�=qD�� D�� D�  D�@ D�� D��HD���D�@ D��HD��HD�HD�>�D�� D�� D�  D�>�D�}qD��)D���D�>�D�}qD�� D��D�AHD��HD�� D�  D�@ D�� D�� D�  D�AHD��HD��HD�HD�>�D�|)D��qD��qD�>�D�� D�� D�  D�AHD��HD�� D�HD�AHD��HD�� D���D�>�D�� D�� D�  D�AHD�� D���D���D�>�D�� D�D��D�AHD�� D�� D�HD�AHDHD��HD��D�AHDÀ D��HD�HD�@ DĀ D�� D�HD�@ D�~�D�� D�  D�AHDƀ D�� D�  D�@ Dǀ DǾ�D���D�@ DȀ D�� D�  D�@ DɁHD�D��D�>�Dʀ D�� D��qD�>�D�~�D˽qD���D�>�D̀ D�� D�  D�@ D�~�D;�D�  D�AHD΀ D�� D�  D�@ DρHD��HD��D�@ DЀ Dо�D�  D�AHDсHD�� D�  D�AHDҁHD��HD�HD�@ D�~�D�� D���D�>�DԀ D�� D�HD�@ DՀ D�� D���D�AHDր D־�D�  D�@ D�~�D׾�D�  D�@ D؁HD�� D���D�>�DفHD�� D�  D�@ Dڀ D�� D�  D�AHDۀ D۾�D�  D�AHD܀ Dܾ�D�  D�@ D݀ D�� D��qD�>�D�~�D޾�D�  D�@ D߁HD��HD�  D�@ D�� DྸD�HD�B�DႏD��HD�HD�AHD�HD�D��D�B�DわD�� D���D�>�D�~�D��HD��D�AHD�~�D徸D�  D�AHD� D�� D���D�>�D�HD��HD�HD�AHD� D�� D�  D�@ D� D��HD�HD�@ D�~�D�� D�HD�@ D�~�D뾸D���D�>�D� D�� D��qD�=qD� D�� D���D�>�D�HDD��qD�>�D� D��HD���D�>�D�� D�� D�  D�>�D� D��HD�HD�AHD�HD�D�  D�@ D� D�� D��qD�=qD�HD��HD�  D�@ D�� D�D��D�AHD�~�D�� D�  D�>�D�� D��HD�  D�>�D�� D���D�  D�AHD��HD�D�HD�P�?\)?aG�?�z�?�p�?�(�@�\@��@.{@@  @Q�@h��@�  @���@�33@�p�@��@��@���@��
@�{@ٙ�@�\@�@�AG�AA
=qA�RA�
A��A{A"�\A'
=A,��A2�\A6ffA;�AAG�AFffAK�AP  AUA[�A`��Ae�Aj�HAp��AuAz�HA\)A�=qA�p�A�\)A��A�z�A�
=A�G�A��A�{A���A�33A�p�A��A��\A��A�
=A���A�(�A�
=A�G�A��A�ffA�G�A��
A�{A���AÅA�{A�Q�A��HA�A�Q�A�33A�p�A׮A��HA�A�Q�A�\A�p�A�Q�A��HA��A�A�\A�p�A��A�=qA���B   BG�BffB�BG�B�RB�
B	�B
�\B  BG�BffB�
BG�B�RB�B�B�RB  B�B�\B  Bp�BffB   B!p�B"�RB#�
B%G�B&�RB(  B)�B*�\B,  B-�B.=qB/�
B1�B2ffB3�B5�B6�\B7�B9�B:�\B<(�B=p�B>�\B@  BA��BC
=BDQ�BE��BG33BH��BI�BK33BL��BN=qBO�BP��BR=qBS�BUG�BV�\BW�
BY�BZ�RB\(�B]p�B^�RB`(�Ba��Bc
=Bd(�Bep�Bg
=Bhz�BiBj�HBl(�BmBo\)Bpz�BqBs33Bt��Bv{Bw33Bx��Bz=qB{�B|��B~=qB�B��\B�33B��
B�ffB��B��
B�z�B��B�B�z�B�33B�B�ffB��B��
B�ffB�
=B��B�ffB��B��B�Q�B���B��B�Q�B���B���B�Q�B���B��B�=qB���B���B�=qB���B�p�B�(�B���B��B�  B���B�\)B�{B���B�33B��B��\B�G�B��B�ffB�
=B��B�ffB�
=B���B�=qB���B���B�{B��RB�p�B�{B���B�33B�B�ffB�
=B��B�{B��RB�G�B��B�z�B�
=B���B�Q�B��HB�p�B��B�z�B�33B��
B�ffB�
=B���B�Q�B���B��B�ffB���B���B�=qB���B�B�z�B��B�B�z�B�33B�  B���B�p�B�{B��HBîB�z�B�33B��BƏ\B�G�B�{B���BɮB�z�B�33B��B̸RBͅB�ffB�33B�  B���BхB�Q�B�
=B�  B���BծB�z�B�G�B�{B��HBٮB�z�B�G�B�{B���Bݙ�B�ffB��B�B�ffB���B�B�{B��B�33B�B�(�B�\B��B�B�  B�ffB�RB�33B癚B�(�B�\B�
=B�p�B��
B�=qB��B��B뙚B�  B�z�B��HB�G�B��B�  B�ffB��HB�G�B�B�(�B��B���B�\)B�B�{B�ffB��HB�G�B�B�(�B�z�B���B�\)B��
B�=qB���B���B�\)B�B�(�B��\B�
=B��B��B�Q�B��RB��B��B��
B�Q�B��RB��B���B�  B�ffB���B�33B��B��C (�C \)C �C �RC �C{CG�Cz�C�C�HC{CG�Cz�C�C�HC{CG�Cz�C��C�
C
=C33C\)C�\C�RC�HC{CG�Cz�C�C�
C
=C=qCp�C��C��C  C(�C\)C�C�RC�C�C=qCp�C��C��C	  C	33C	\)C	�\C	�RC	��C
(�C
\)C
�\C
C
��C�CQ�C�\CC��C33CffC��C��C  C33Cp�C��C�HC{CG�Cz�C�RC�C(�CffC��C��C
=C=qCz�C�RC�C(�C\)C��C��C
=CG�Cz�C�RC��C33CffC��C�HC�CQ�C�\CC  C=qCz�C�C�HC�C\)C�\C��C
=C=qCp�C�C�C�C\)C��C��C
=CG�C�C�RC��C33Cp�C�C�C33CffC��C�HC�C\)C��C�
C�C\)C��C�
C{CQ�C�\CC
=CG�Cz�C�RC��C 33C p�C �C �C!(�C!p�C!�C!�C"(�C"ffC"��C"�HC#�C#\)C#��C#�
C$�C$\)C$��C$�HC%�C%\)C%��C%�HC&{C&Q�C&�\C&��C'  C'G�C'z�C'�C'�C((�C(p�C(��C(�HC)�C)ffC)��C)�HC*�C*\)C*��C*�
C+{C+Q�C+�\C+��C,  C,G�C,�C,��C-
=C-G�C-�C-C.  C.33C.p�C.�C.��C/=qC/p�C/�RC/��C0(�C0ffC0��C0�HC1�C1ffC1�C1�HC2{C2G�C2�C2C3  C3G�C3�\C3��C4
=C4G�C4z�C4C5  C5G�C5�\C5�
C6
=C6G�C6�C6C7  C7G�C7�\C7��C8{C8Q�C8�\C8��C9  C9G�C9z�C9��C:
=C:Q�C:�\C:�
C;{C;Q�C;�\C;��C<{C<\)C<��C<�HC=(�C=p�C=��C=�HC>(�C>p�C>�RC?  C?G�C?�\C?��C@
=C@Q�C@�\C@�
CA�CAffCA�RCB  CBG�CB�\CB��CC
=CCQ�CC��CC�HCD(�CDp�CD�RCD��CE=qCEz�CECF
=CFQ�CF��CF�CG33CGp�CG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                 ?�  @�@@  @xQ�@�p�@�  @�  A ��A��A   A,��A@  A^�RA�  A�  A�  A�  A�  A�Q�A�  A�  B (�B�
B�
B�
B   B(Q�B0(�B7�
B?�
BG�
BO�
BW�
B_�
Bh  Bp(�Bx  B�  B�  B�  B��B�  B�{B�  B�  B�  B�  B�  B�  B�{B��B��
B��B�  B�  B�(�B�(�B��B�  B�{B�{B�(�B�{B�  B�  B�  B�  B�  B��B��C  C  C  C
=C
  C��C��C  C  C  C��C  C  C  C  C   C"  C$  C&  C'��C*  C,  C.  C0  C1��C4
=C6
=C7��C:  C<
=C>  C?�CA��CD  CF  CH  CJ{CL{CN{CO��CQ��CT  CV  CX
=CZ
=C\
=C^
=C`
=Cb
=Cd
=Cf
=Ch  Cj
=Ck��Cm��Cp  Cr  Ct  Cv  Cx
=Cz{C|
=C~  C�  C�  C�
=C�C�  C�  C�  C�  C�C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C�  C�  C�  C���C�  C�C�  C���C�  C�
=C�C�C�  C���C���C���C���C�C�C�C�  C���C�  C�C�C�C���C���C�  C�  C�C�C�  C�  C�  C�  C�C�C���C�  C�
=C�C���C�C�  C�  C�
=C�
=C�  C�C�
=C�C�C���C���C���C���C���C�  C�C�  C���C�  C�C�
=C�C�  C���C���C�  C���C���C�  C�
=C�C�  C�
=C�C�  C�
=C�\C�C�  C�
=C�C�  C���C���C���C���C�  C�  C���C���C���C���C�C�  C�  C�  C�C�
=C���C���C���C���C�  C�C�
=D   D ��D�D� D�D��D  D� D�D�D�D� D�qDz�D  D�D�qD}qD	�D	}qD
  D
�D�D��D�D� D  D}qD�qD}qD��D� D  DxRD�RD}qD  D}qD  D�D�D� D�qD}qD��D� D�D� D�qD}qD�qD��DD�D�D� D  D�DD� D  D� D�D� D   D ��D!�D!� D"  D"��D#�D#��D#�qD$}qD$�qD%� D&  D&}qD'�D'� D'�qD(}qD(�qD)� D*  D*� D+  D+� D,  D,� D,�qD-}qD-�qD.}qD.�qD/� D0�D0}qD0�qD1� D2  D2�D3�D3��D4  D4}qD4�qD5}qD6  D6��D7  D7}qD7�qD8��D9�D9� D9�qD:}qD:�qD;� D<  D<��D=�D=��D>  D>}qD?�D?�D@�D@��DA�DA��DB�DB� DC  DC� DD  DD� DD�qDE}qDE�qDF� DF�qDG� DG�qDH� DI�DI��DJ  DJ}qDK  DK� DL  DL}qDM  DM� DN  DN}qDN��DOz�DO�qDP}qDQ  DQ��DR�DR� DS  DS� DT  DT� DU  DU� DV�DV��DW�DW��DX�DX��DY�DY��DZ  DZ� D[�D[�D\D\��D]�D]� D]��D^}qD^�qD_}qD`  D`� Da  Da� Db  Db� Dc  Dc� Dc�qDd� De�De��Df�Df}qDf�qDg��Dh  Dh}qDi�Di��Dj  Dj}qDj�qDk}qDl  Dl��Dm  Dm� Dn  Dn}qDn�qDo}qDp  Dp� Dp�qDq� Dr�Dr��Dr�qDs� Dt  Dt� DuDu�Dv�Dv� Dv�qDw� Dx  Dx}qDx�qDy}qDy��Dzz�Dz��D{� D|D|�D}  D}z�D}�qD~}qD~�qD� D�HD�AHD�� D���D���D�AHD��HD��HD�HD�AHD��HD�� D���D�@ D��HD�� D���D�@ D��HD��HD��D�AHD�� D��HD��D�B�D��HD��HD�HD�AHD�� D�� D�  D�>�D�~�D��HD�  D�=qD�� D�D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�@ D�~�D�� D�  D�@ D���D�� D���D�AHD�� D�� D���D�AHD���D��HD�  D�>�D�� D��HD�  D�>�D�� D��HD�  D�>�D�� D�� D���D�@ D�� D�� D�HD�>�D�}qD���D�  D�AHD��HD�� D���D�>�D�~�D��HD��D�@ D�~�D���D���D�>�D�� D��HD�  D�>�D�� D�� D���D�@ D�~�D���D�  D�@ D�� D���D�  D�AHD�~�D�� D�HD�@ D�� D��HD���D�@ D��HD�� D���D�>�D�� D�D�HD�@ D�� D�� D�HD�@ D�~�D���D���D�>�D�~�D�� D�  D�>�D�� D�� D�HD�AHD�~�D���D�  D�AHD�� D���D�  D�>�D�}qD�� D���D�>�D�� D���D��qD�>�D�~�D���D���D�>�D�~�D��HD���D�=qD�� D��HD�  D�>�D�~�D��qD���D�AHD��HD�D��D�C�D��HD�� D�HD�B�D��HD���D�  D�B�D���D��HD���D�=qD�}qD���D��qD�=qD�� D�� D�  D�@ D�� D��HD���D�@ D��HD��HD�HD�>�D�� D�� D�  D�>�D�}qD��)D���D�>�D�}qD�� D��D�AHD��HD�� D�  D�@ D�� D�� D�  D�AHD��HD��HD�HD�>�D�|)D��qD��qD�>�D�� D�� D�  D�AHD��HD�� D�HD�AHD��HD�� D���D�>�D�� D�� D�  D�AHD�� D���D���D�>�D�� D�D��D�AHD�� D�� D�HD�AHDHD��HD��D�AHDÀ D��HD�HD�@ DĀ D�� D�HD�@ D�~�D�� D�  D�AHDƀ D�� D�  D�@ Dǀ DǾ�D���D�@ DȀ D�� D�  D�@ DɁHD�D��D�>�Dʀ D�� D��qD�>�D�~�D˽qD���D�>�D̀ D�� D�  D�@ D�~�D;�D�  D�AHD΀ D�� D�  D�@ DρHD��HD��D�@ DЀ Dо�D�  D�AHDсHD�� D�  D�AHDҁHD��HD�HD�@ D�~�D�� D���D�>�DԀ D�� D�HD�@ DՀ D�� D���D�AHDր D־�D�  D�@ D�~�D׾�D�  D�@ D؁HD�� D���D�>�DفHD�� D�  D�@ Dڀ D�� D�  D�AHDۀ D۾�D�  D�AHD܀ Dܾ�D�  D�@ D݀ D�� D��qD�>�D�~�D޾�D�  D�@ D߁HD��HD�  D�@ D�� DྸD�HD�B�DႏD��HD�HD�AHD�HD�D��D�B�DわD�� D���D�>�D�~�D��HD��D�AHD�~�D徸D�  D�AHD� D�� D���D�>�D�HD��HD�HD�AHD� D�� D�  D�@ D� D��HD�HD�@ D�~�D�� D�HD�@ D�~�D뾸D���D�>�D� D�� D��qD�=qD� D�� D���D�>�D�HDD��qD�>�D� D��HD���D�>�D�� D�� D�  D�>�D� D��HD�HD�AHD�HD�D�  D�@ D� D�� D��qD�=qD�HD��HD�  D�@ D�� D�D��D�AHD�~�D�� D�  D�>�D�� D��HD�  D�>�D�� D���D�  D�AHD��HD�D�HD�P�?\)?aG�?�z�?�p�?�(�@�\@��@.{@@  @Q�@h��@�  @���@�33@�p�@��@��@���@��
@�{@ٙ�@�\@�@�AG�AA
=qA�RA�
A��A{A"�\A'
=A,��A2�\A6ffA;�AAG�AFffAK�AP  AUA[�A`��Ae�Aj�HAp��AuAz�HA\)A�=qA�p�A�\)A��A�z�A�
=A�G�A��A�{A���A�33A�p�A��A��\A��A�
=A���A�(�A�
=A�G�A��A�ffA�G�A��
A�{A���AÅA�{A�Q�A��HA�A�Q�A�33A�p�A׮A��HA�A�Q�A�\A�p�A�Q�A��HA��A�A�\A�p�A��A�=qA���B   BG�BffB�BG�B�RB�
B	�B
�\B  BG�BffB�
BG�B�RB�B�B�RB  B�B�\B  Bp�BffB   B!p�B"�RB#�
B%G�B&�RB(  B)�B*�\B,  B-�B.=qB/�
B1�B2ffB3�B5�B6�\B7�B9�B:�\B<(�B=p�B>�\B@  BA��BC
=BDQ�BE��BG33BH��BI�BK33BL��BN=qBO�BP��BR=qBS�BUG�BV�\BW�
BY�BZ�RB\(�B]p�B^�RB`(�Ba��Bc
=Bd(�Bep�Bg
=Bhz�BiBj�HBl(�BmBo\)Bpz�BqBs33Bt��Bv{Bw33Bx��Bz=qB{�B|��B~=qB�B��\B�33B��
B�ffB��B��
B�z�B��B�B�z�B�33B�B�ffB��B��
B�ffB�
=B��B�ffB��B��B�Q�B���B��B�Q�B���B���B�Q�B���B��B�=qB���B���B�=qB���B�p�B�(�B���B��B�  B���B�\)B�{B���B�33B��B��\B�G�B��B�ffB�
=B��B�ffB�
=B���B�=qB���B���B�{B��RB�p�B�{B���B�33B�B�ffB�
=B��B�{B��RB�G�B��B�z�B�
=B���B�Q�B��HB�p�B��B�z�B�33B��
B�ffB�
=B���B�Q�B���B��B�ffB���B���B�=qB���B�B�z�B��B�B�z�B�33B�  B���B�p�B�{B��HBîB�z�B�33B��BƏ\B�G�B�{B���BɮB�z�B�33B��B̸RBͅB�ffB�33B�  B���BхB�Q�B�
=B�  B���BծB�z�B�G�B�{B��HBٮB�z�B�G�B�{B���Bݙ�B�ffB��B�B�ffB���B�B�{B��B�33B�B�(�B�\B��B�B�  B�ffB�RB�33B癚B�(�B�\B�
=B�p�B��
B�=qB��B��B뙚B�  B�z�B��HB�G�B��B�  B�ffB��HB�G�B�B�(�B��B���B�\)B�B�{B�ffB��HB�G�B�B�(�B�z�B���B�\)B��
B�=qB���B���B�\)B�B�(�B��\B�
=B��B��B�Q�B��RB��B��B��
B�Q�B��RB��B���B�  B�ffB���B�33B��B��C (�C \)C �C �RC �C{CG�Cz�C�C�HC{CG�Cz�C�C�HC{CG�Cz�C��C�
C
=C33C\)C�\C�RC�HC{CG�Cz�C�C�
C
=C=qCp�C��C��C  C(�C\)C�C�RC�C�C=qCp�C��C��C	  C	33C	\)C	�\C	�RC	��C
(�C
\)C
�\C
C
��C�CQ�C�\CC��C33CffC��C��C  C33Cp�C��C�HC{CG�Cz�C�RC�C(�CffC��C��C
=C=qCz�C�RC�C(�C\)C��C��C
=CG�Cz�C�RC��C33CffC��C�HC�CQ�C�\CC  C=qCz�C�C�HC�C\)C�\C��C
=C=qCp�C�C�C�C\)C��C��C
=CG�C�C�RC��C33Cp�C�C�C33CffC��C�HC�C\)C��C�
C�C\)C��C�
C{CQ�C�\CC
=CG�Cz�C�RC��C 33C p�C �C �C!(�C!p�C!�C!�C"(�C"ffC"��C"�HC#�C#\)C#��C#�
C$�C$\)C$��C$�HC%�C%\)C%��C%�HC&{C&Q�C&�\C&��C'  C'G�C'z�C'�C'�C((�C(p�C(��C(�HC)�C)ffC)��C)�HC*�C*\)C*��C*�
C+{C+Q�C+�\C+��C,  C,G�C,�C,��C-
=C-G�C-�C-C.  C.33C.p�C.�C.��C/=qC/p�C/�RC/��C0(�C0ffC0��C0�HC1�C1ffC1�C1�HC2{C2G�C2�C2C3  C3G�C3�\C3��C4
=C4G�C4z�C4C5  C5G�C5�\C5�
C6
=C6G�C6�C6C7  C7G�C7�\C7��C8{C8Q�C8�\C8��C9  C9G�C9z�C9��C:
=C:Q�C:�\C:�
C;{C;Q�C;�\C;��C<{C<\)C<��C<�HC=(�C=p�C=��C=�HC>(�C>p�C>�RC?  C?G�C?�\C?��C@
=C@Q�C@�\C@�
CA�CAffCA�RCB  CBG�CB�\CB��CC
=CCQ�CC��CC�HCD(�CDp�CD�RCD��CE=qCEz�CECF
=CFQ�CF��CF�CG33CGp�CG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA��mA��yA��TA��`A��mA��yA��yA��A��A��yA��A��yA��TA��`A��;A��`A�ƨA���A���A�A���AҾwAҼjAҺ^AҺ^AҺ^AҮAҮAҬAҬAҩ�Aҩ�Aҩ�Aҩ�Aҧ�Aҟ�Aқ�Aҝ�Aқ�Aҗ�Aҕ�A҇+A�v�A�bNA�(�A�A�XA��`AϼjÃA�l�A�ZA�hsA�G�A���A�K�A��A��7A���A�`BA�t�A���A���A���A��A�bA��A�A��!A�|�A��A�oA��uA�A�z�A��/A��uA��A�7LA�n�A���A���A��\A���A���A���A�bNA���A���A���A���A��A{|�Ax��As�PAqC�An��Aj��AiVAf  Ab�A^-A\�AZ��AY�AVn�ATjAR �AO�hAH��AF1'AC�AA�hA@�A?�A>ffA=�^A<��A<��A<�`A=33A>r�A=?}A:�A8�\A7?}A6�A4�A4ffA4^5A3\)A2 �A1��A1�;A1�;A1�wA1�^A1/A0��A0n�A/�A/�-A/�7A.��A-�
A,r�A+l�A*E�A)��A)+A(ȴA(�\A(1'A'�A't�A&�9A%A%�wA%%A$jA$�yA$$�A#%A!��A!C�A!�A!G�A|�A�9AbNA{A�mA��AA�^A�FA��AO�A&�A�RA�;Al�A�DAM�A��A�A�PA/AȴA��A�7A\)A/A��A��A?}An�A1A��AA��A/A�DA9XA��A��A?}AVAĜA~�A-A�A�
AA�A��A|�A�At�A��A=qA  A&�A^5AJA�A
��A
�!A
r�A
9XA
�A	�A	��A	�A��A��AdZA�A��A�A��Ax�A7LA%AĜAn�AbA�PAG�A%A�`A��A��A$�A��AK�A ��A ��A Q�@��@��y@�{@�`B@��D@�1'@��@���@���@��@�`B@�?}@�/@�%@��/@�1@�l�@�33@�o@���@��@�G�@���@��
@�\)@�ff@�7@�?}@��`@�r�@@�^5@�h@��@�I�@ꗍ@�$�@���@�b@�w@�S�@�+@�@�&�@�j@�A�@�F@��H@��@�^@��@�\)@�M�@݉7@�7L@�&�@���@ܬ@��m@�
=@ڏ\@�^5@�J@�p�@�G�@�&�@�r�@ם�@׍P@�K�@�
=@ְ!@��@Ձ@�j@�1@ӶF@�t�@�@�M�@с@���@�j@��@϶F@��@�5?@�G�@̼j@� �@�+@���@�5?@ɺ^@�?}@�r�@ǥ�@�ȴ@��@��@őh@ļj@�b@�|�@�"�@+@�=q@��@�@���@���@�bN@�(�@�  @�"�@�{@�@�x�@�%@��j@�r�@�Q�@�I�@�b@�l�@��\@�$�@�@�X@�%@��@��F@�dZ@�dZ@���@��@���@��#@��h@�7L@��/@�Z@�1@���@�"�@��R@���@�V@��#@�p�@�/@���@���@�r�@�1'@��;@���@�S�@��@���@�-@���@��^@�x�@�p�@�X@��@��/@��/@���@�Z@�1'@��m@��w@�C�@��R@�ff@�V@�M�@�@�x�@��@� �@��w@�l�@���@��!@��\@�n�@��T@�/@��@�I�@�1@��;@���@�t�@�l�@�;d@���@��@��H@��R@�ff@�=q@��@�O�@�?}@�&�@��`@�j@���@��w@���@�|�@�\)@�;d@�@��R@���@�ff@���@���@�X@��@��D@�bN@��w@�"�@���@���@���@�~�@�M�@�@��7@�/@���@��@�r�@�1'@��
@��P@�+@��H@�v�@�=q@�$�@��T@��-@�`B@��@��@��9@�Z@�b@���@��m@�ƨ@�t�@�33@���@��@���@�~�@�-@�@��@���@��@���@��D@��m@��
@���@���@�;d@���@���@�n�@�n�@�ff@�5?@��@���@���@��7@��@�x�@�?}@��@��@��@�bN@�1'@��;@�t�@�+@���@�^5@�E�@�5?@�{@��@���@��h@�X@�G�@��@��j@��@�I�@�9X@��@�1@��F@��P@�t�@�;d@��@��R@�5?@���@���@�x�@�%@��j@���@�I�@��@���@�l�@�S�@�;d@�@��@���@�M�@�-@���@��-@���@�X@��@��`@�Ĝ@��u@�A�@�b@�  @��@�@��@l�@~�y@~��@~�+@~@}�h@}?}@|�/@|�D@{�
@{S�@z�!@z��@y�@y�@x��@x �@w|�@v�+@v{@u�@u?}@t�@tj@tI�@t1@sƨ@s��@st�@sC�@r��@rn�@q�@q��@q&�@p�u@p1'@p  @o\)@n�y@n��@n5?@m��@l�@l9X@k��@kS�@j��@i�@h�@h1'@g�@f��@f�+@fV@f5?@f$�@e�@d��@c�F@cdZ@bn�@a��@a�7@aG�@a�@`�9@`1'@_��@_|�@_
=@^��@^v�@^{@]��@\��@\�@\z�@\I�@[ƨ@[C�@[o@Z��@Y��@Y�7@Y&�@X��@XĜ@X�u@W��@Wl�@V��@V�R@Vff@VE�@V@U��@UO�@UV@T�/@T�D@TZ@T(�@Sƨ@SdZ@R�H@R~�@RM�@RJ@Q��@Q��@Qx�@Q&�@P�`@PQ�@O�P@O
=@Nȴ@N��@Nv�@N$�@N@M�-@MO�@L�/@L��@L�j@Lj@K�
@K�F@K��@Kt�@KC�@K33@J�@J�!@JM�@JJ@I��@IG�@H�u@G�@G��@Gl�@G;d@G�@F�R@Fv�@F5?@E��@Ep�@EV@D�j@D�@D�@Dz�@D1@C�m@C�
@C�F@CS�@Co@B�@B��@B-@A�@A�7@A7L@@Ĝ@?�@?l�@?K�@>�y@>�+@>{@=��@=/@=V@<��@<�@<��@<Z@;�m@;�@;S�@;33@;"�@:�!@:^5@9��@9��@9X@8�9@81'@7�@7;d@6��@6�@6��@6v�@65?@5�T@5��@5O�@4�j@3�m@3t�@3"�@2�@2��@2��@2��@2�!@2~�@2^5@2=q@2�@1�@1�#@1�^@1��@1�7@1G�@0�9@01'@/�@/�P@/l�@/
=@.�R@.ff@.@-�-@-�@,��@,��@,Z@,�@+�m@+ƨ@+�F@+�@*�@*��@*��@*��@*=q@)�@)��@)hs@)G�@(�9@(bN@(A�@(1'@(b@'�@'�w@'�@'�P@'l�@'�@&v�@%�T@%�-@%�h@%`B@$��@$�/@$��@$�D@$j@$I�@#��@#��@#t�@#C�@#"�@"�!@"��@"�\@"~�@"M�@"-@!��@!�^@!�7@!�7@!hs@!7L@!%@ Ĝ@ r�@ bN@ A�@�;@�P@l�@\)@
=@��@ff@E�@$�@�@��@�-@/@��@�j@�@��@I�@1@�
@��@t�@C�@�@�!@�\@n�@^5@�@��@�#@��@x�@7L@��@�`@�u@Q�@  @��@\)@+@�@
=@
=@�@v�@{@@��@�h@�h@�@`B@O�@�@V@�@�j@��@�D@Z@(�@�
@ƨ@�F@�@dZ@C�@o@@�H@�!@��@n�@�@J@��@�7@hs@7L@�@%@%@��@�`@�`@�9@r�@bN@bN@ �@�;@�w@�@\)@+@+@+A��A��A��mA��HA��HA��yA��yA��A��mA��`A��mA��HA��;A��HA��mA��mA��`A��mA��`A��A��yA��yA��`A��A��A��yA��yA��yA��A��A��A��mA��A��A��mA��`A��yA���A��yA��`A��yA��A��mA��`A��`A��A��mA��;A��HA��TA��`A��TA��HA��HA��`A��mA��`A��TA��mA��HA��#A��/A��A��;A��HA��A��A��A��yA��TA��/A��TA���A���A�A�ĜA�ĜA���AҾwAҾwA�A�ĜA���AҾwA�A�A���AҾwA���A�ĜA�ĜA���AҾwA�ĜA�ĜA���AҾwA���A�A�AҾwAҼjAҾwA���AҾwAҺ^AҼjA���A���AҺ^AҺ^AҾwAҾwAҼjAҸRAҺ^AҼjAҸRAҸRAҼjAҾwAҺ^AҸRAҼjAҼjAҸRAҸRAҾwAҺ^AҸRAҺ^AҼjAҸRAҬAҮAҲ-AҮAҬAҰ!AҲ-AҮAҬAҮAҮAҬAҩ�Aҩ�AҮAҮAҩ�Aҩ�AҮAҰ!AҬAҧ�Aҧ�AҬAҬAҧ�Aҧ�Aҩ�AҬAҬAҧ�Aҧ�Aҩ�AҬAҩ�Aҧ�Aҧ�Aҩ�AҬAҩ�Aҧ�Aҩ�AҬAҬAҥ�Aҥ�Aҩ�Aҩ�Aҧ�Aң�Aҧ�Aҩ�Aҩ�Aҝ�Aҝ�Aҡ�Aң�Aқ�Aҝ�Aқ�Aҟ�Aқ�Aҗ�AґhAҙ�Aҝ�Aҡ�Aҝ�Aҝ�Aҟ�Aҟ�Aҝ�Aҙ�Aҙ�Aҙ�Aҕ�AғuAҗ�Aҙ�Aҙ�Aҗ�Aҗ�Aҙ�Aҙ�Aҗ�Aҕ�AғuAҕ�AҍPA҉7A҇+A҇+AҋDA҅A�|�A�|�A�z�A�x�A�r�A�n�A�l�A�n�A�p�A�n�A�S�A�I�A�VA�;dA�$�A�&�A��A��A� �A��A�bA�
=A�
=A�1A��HAћ�AэPA�p�A�bNA�A�A�/A���A���A��A��mA��TA��yA��;A�ƨAЧ�A�dZA��A�z�A�;dA��A��#A�ƨAΉ7A�=qA�
=A�z�A�jA�VA�~�A�+A��A�A��A���A���Aŏ\A�n�A�ȴA�C�AÉ7A�-A�C�A���A�^5A�33A�A��RA�I�A�~�A���A��A�/A���A��9A���A��A�l�A�ZA�C�A�{A��`A�VA�A�z�A�7LA���A�XA���A��mA��A�n�A��A��+A�Q�A��#A�+A���A�
=A��A��A��HA���A��FA��A�hsA�ĜA��yA�E�A���A���A�v�A�`BA�Q�A�5?A�&�A� �A��A�oA�
=A���A��A��/A��
A���A���A���A��-A��-A��A���A��+A�x�A�bNA�E�A�9XA�/A�"�A�A��/A�ĜA��A���A���A��7A�x�A�`BA�?}A�1'A�"�A�A���A��RA�VA�K�A�(�A���A�A��!A���A�p�A�ZA�M�A�A�A�5?A�&�A�bA��`A�A���A�r�A�^5A�M�A�9XA� �A�A��`A�A���A��hA��A�ffA�C�A� �A�A��
A���A�M�A�ƨA�-A��A�O�A�$�A�1A���A��hA�jA�M�A�33A�A��mA��#A���A�ƨA��A���A��+A�M�A�$�A�VA���A��A��^A���A��A�9XA��+A��!A��A��9A��FA��9A���A���A�r�A�VA�(�A�bA���A��A��HA���A��FA���A��uA�~�A�ffA�G�A�+A��#A���A���A�x�A�$�A���A�jA�G�A�&�A�{A�VA��A���A��RA���A��DA�O�A�A���A���A��PA�|�A�r�A�dZA�Q�A�;dA� �A�  A���A��A�z�A�A�A�VA��A�ĜA���A�~�A�^5A�?}A�"�A�oA���A��#A�A��-A���A��uA�~�A�jA�I�A�33A�oA���A��;A���A���A���A��!A��+A�O�A�5?A�oA��TA�ffA��jA�ZA�9XA�
=A��9A�v�A�?}A���A���A���A�bNA�$�A�
=A�A���A��A��/A���A��RA���A�r�A��A��9A�A�ȴA�^5A��A��
A��A� �A���A��mA�A�A��`A�z�A�+A��A�A���A�M�A��#A�t�A�1'A�+A�%A���A��mA��
A���A�ĜA�ĜA��9A���A��A�VA�%A�A���A��7A�+A��;A�r�A��`A�ZA�33A��7A��;A���A�C�A��A���A�`BA�bNA�K�A���A��A�M�A�?}A�-A�{A�  A��A�A�
A��A�AhsAO�A~��A}�A{�;A{��A{t�A{33Az��Az�Az^5Az1'AyƨAydZAxĜAxQ�Aw�Aw��Av�AvVAt�`At  As�AshsAsVAr�DAr9XAr9XAr  Aq�
Aq��AqC�Ap�HAp��Apv�Ao�TAo�hAoO�Ao
=An��An�Am�wAm`BAlĜAl1Ak�AkhsAjr�Ai�Ai�^Ai��Ai�AiS�Ai;dAi&�Ai�Ai�AiVAh��AgXAg%Af�jAfffAe�FAd��Ad�9Ac�wAc"�Ab��Ab^5Ab1'Aa�7A`�yA`M�A_�A_dZA^�A]�#A]\)A]A\�RA\�uA\n�A\ZA\I�A\{A[��A[��A[�hA[l�A[7LAZ�`AZ��AZbNAZQ�AZ �AY�#AY��AY+AXȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                 A��yA��mA��yA��TA��`A��mA��yA��yA��A��A��yA��A��yA��TA��`A��;A��`A�ƨA���A���A�A���AҾwAҼjAҺ^AҺ^AҺ^AҮAҮAҬAҬAҩ�Aҩ�Aҩ�Aҩ�Aҧ�Aҟ�Aқ�Aҝ�Aқ�Aҗ�Aҕ�A҇+A�v�A�bNA�(�A�A�XA��`AϼjÃA�l�A�ZA�hsA�G�A���A�K�A��A��7A���A�`BA�t�A���A���A���A��A�bA��A�A��!A�|�A��A�oA��uA�A�z�A��/A��uA��A�7LA�n�A���A���A��\A���A���A���A�bNA���A���A���A���A��A{|�Ax��As�PAqC�An��Aj��AiVAf  Ab�A^-A\�AZ��AY�AVn�ATjAR �AO�hAH��AF1'AC�AA�hA@�A?�A>ffA=�^A<��A<��A<�`A=33A>r�A=?}A:�A8�\A7?}A6�A4�A4ffA4^5A3\)A2 �A1��A1�;A1�;A1�wA1�^A1/A0��A0n�A/�A/�-A/�7A.��A-�
A,r�A+l�A*E�A)��A)+A(ȴA(�\A(1'A'�A't�A&�9A%A%�wA%%A$jA$�yA$$�A#%A!��A!C�A!�A!G�A|�A�9AbNA{A�mA��AA�^A�FA��AO�A&�A�RA�;Al�A�DAM�A��A�A�PA/AȴA��A�7A\)A/A��A��A?}An�A1A��AA��A/A�DA9XA��A��A?}AVAĜA~�A-A�A�
AA�A��A|�A�At�A��A=qA  A&�A^5AJA�A
��A
�!A
r�A
9XA
�A	�A	��A	�A��A��AdZA�A��A�A��Ax�A7LA%AĜAn�AbA�PAG�A%A�`A��A��A$�A��AK�A ��A ��A Q�@��@��y@�{@�`B@��D@�1'@��@���@���@��@�`B@�?}@�/@�%@��/@�1@�l�@�33@�o@���@��@�G�@���@��
@�\)@�ff@�7@�?}@��`@�r�@@�^5@�h@��@�I�@ꗍ@�$�@���@�b@�w@�S�@�+@�@�&�@�j@�A�@�F@��H@��@�^@��@�\)@�M�@݉7@�7L@�&�@���@ܬ@��m@�
=@ڏ\@�^5@�J@�p�@�G�@�&�@�r�@ם�@׍P@�K�@�
=@ְ!@��@Ձ@�j@�1@ӶF@�t�@�@�M�@с@���@�j@��@϶F@��@�5?@�G�@̼j@� �@�+@���@�5?@ɺ^@�?}@�r�@ǥ�@�ȴ@��@��@őh@ļj@�b@�|�@�"�@+@�=q@��@�@���@���@�bN@�(�@�  @�"�@�{@�@�x�@�%@��j@�r�@�Q�@�I�@�b@�l�@��\@�$�@�@�X@�%@��@��F@�dZ@�dZ@���@��@���@��#@��h@�7L@��/@�Z@�1@���@�"�@��R@���@�V@��#@�p�@�/@���@���@�r�@�1'@��;@���@�S�@��@���@�-@���@��^@�x�@�p�@�X@��@��/@��/@���@�Z@�1'@��m@��w@�C�@��R@�ff@�V@�M�@�@�x�@��@� �@��w@�l�@���@��!@��\@�n�@��T@�/@��@�I�@�1@��;@���@�t�@�l�@�;d@���@��@��H@��R@�ff@�=q@��@�O�@�?}@�&�@��`@�j@���@��w@���@�|�@�\)@�;d@�@��R@���@�ff@���@���@�X@��@��D@�bN@��w@�"�@���@���@���@�~�@�M�@�@��7@�/@���@��@�r�@�1'@��
@��P@�+@��H@�v�@�=q@�$�@��T@��-@�`B@��@��@��9@�Z@�b@���@��m@�ƨ@�t�@�33@���@��@���@�~�@�-@�@��@���@��@���@��D@��m@��
@���@���@�;d@���@���@�n�@�n�@�ff@�5?@��@���@���@��7@��@�x�@�?}@��@��@��@�bN@�1'@��;@�t�@�+@���@�^5@�E�@�5?@�{@��@���@��h@�X@�G�@��@��j@��@�I�@�9X@��@�1@��F@��P@�t�@�;d@��@��R@�5?@���@���@�x�@�%@��j@���@�I�@��@���@�l�@�S�@�;d@�@��@���@�M�@�-@���@��-@���@�X@��@��`@�Ĝ@��u@�A�@�b@�  @��@�@��@l�@~�y@~��@~�+@~@}�h@}?}@|�/@|�D@{�
@{S�@z�!@z��@y�@y�@x��@x �@w|�@v�+@v{@u�@u?}@t�@tj@tI�@t1@sƨ@s��@st�@sC�@r��@rn�@q�@q��@q&�@p�u@p1'@p  @o\)@n�y@n��@n5?@m��@l�@l9X@k��@kS�@j��@i�@h�@h1'@g�@f��@f�+@fV@f5?@f$�@e�@d��@c�F@cdZ@bn�@a��@a�7@aG�@a�@`�9@`1'@_��@_|�@_
=@^��@^v�@^{@]��@\��@\�@\z�@\I�@[ƨ@[C�@[o@Z��@Y��@Y�7@Y&�@X��@XĜ@X�u@W��@Wl�@V��@V�R@Vff@VE�@V@U��@UO�@UV@T�/@T�D@TZ@T(�@Sƨ@SdZ@R�H@R~�@RM�@RJ@Q��@Q��@Qx�@Q&�@P�`@PQ�@O�P@O
=@Nȴ@N��@Nv�@N$�@N@M�-@MO�@L�/@L��@L�j@Lj@K�
@K�F@K��@Kt�@KC�@K33@J�@J�!@JM�@JJ@I��@IG�@H�u@G�@G��@Gl�@G;d@G�@F�R@Fv�@F5?@E��@Ep�@EV@D�j@D�@D�@Dz�@D1@C�m@C�
@C�F@CS�@Co@B�@B��@B-@A�@A�7@A7L@@Ĝ@?�@?l�@?K�@>�y@>�+@>{@=��@=/@=V@<��@<�@<��@<Z@;�m@;�@;S�@;33@;"�@:�!@:^5@9��@9��@9X@8�9@81'@7�@7;d@6��@6�@6��@6v�@65?@5�T@5��@5O�@4�j@3�m@3t�@3"�@2�@2��@2��@2��@2�!@2~�@2^5@2=q@2�@1�@1�#@1�^@1��@1�7@1G�@0�9@01'@/�@/�P@/l�@/
=@.�R@.ff@.@-�-@-�@,��@,��@,Z@,�@+�m@+ƨ@+�F@+�@*�@*��@*��@*��@*=q@)�@)��@)hs@)G�@(�9@(bN@(A�@(1'@(b@'�@'�w@'�@'�P@'l�@'�@&v�@%�T@%�-@%�h@%`B@$��@$�/@$��@$�D@$j@$I�@#��@#��@#t�@#C�@#"�@"�!@"��@"�\@"~�@"M�@"-@!��@!�^@!�7@!�7@!hs@!7L@!%@ Ĝ@ r�@ bN@ A�@�;@�P@l�@\)@
=@��@ff@E�@$�@�@��@�-@/@��@�j@�@��@I�@1@�
@��@t�@C�@�@�!@�\@n�@^5@�@��@�#@��@x�@7L@��@�`@�u@Q�@  @��@\)@+@�@
=@
=@�@v�@{@@��@�h@�h@�@`B@O�@�@V@�@�j@��@�D@Z@(�@�
@ƨ@�F@�@dZ@C�@o@@�H@�!@��@n�@�@J@��@�7@hs@7L@�@%@%@��@�`@�`@�9@r�@bN@bN@ �@�;@�w@�@\)@+@+@+A��A��A��mA��HA��HA��yA��yA��A��mA��`A��mA��HA��;A��HA��mA��mA��`A��mA��`A��A��yA��yA��`A��A��A��yA��yA��yA��A��A��A��mA��A��A��mA��`A��yA���A��yA��`A��yA��A��mA��`A��`A��A��mA��;A��HA��TA��`A��TA��HA��HA��`A��mA��`A��TA��mA��HA��#A��/A��A��;A��HA��A��A��A��yA��TA��/A��TA���A���A�A�ĜA�ĜA���AҾwAҾwA�A�ĜA���AҾwA�A�A���AҾwA���A�ĜA�ĜA���AҾwA�ĜA�ĜA���AҾwA���A�A�AҾwAҼjAҾwA���AҾwAҺ^AҼjA���A���AҺ^AҺ^AҾwAҾwAҼjAҸRAҺ^AҼjAҸRAҸRAҼjAҾwAҺ^AҸRAҼjAҼjAҸRAҸRAҾwAҺ^AҸRAҺ^AҼjAҸRAҬAҮAҲ-AҮAҬAҰ!AҲ-AҮAҬAҮAҮAҬAҩ�Aҩ�AҮAҮAҩ�Aҩ�AҮAҰ!AҬAҧ�Aҧ�AҬAҬAҧ�Aҧ�Aҩ�AҬAҬAҧ�Aҧ�Aҩ�AҬAҩ�Aҧ�Aҧ�Aҩ�AҬAҩ�Aҧ�Aҩ�AҬAҬAҥ�Aҥ�Aҩ�Aҩ�Aҧ�Aң�Aҧ�Aҩ�Aҩ�Aҝ�Aҝ�Aҡ�Aң�Aқ�Aҝ�Aқ�Aҟ�Aқ�Aҗ�AґhAҙ�Aҝ�Aҡ�Aҝ�Aҝ�Aҟ�Aҟ�Aҝ�Aҙ�Aҙ�Aҙ�Aҕ�AғuAҗ�Aҙ�Aҙ�Aҗ�Aҗ�Aҙ�Aҙ�Aҗ�Aҕ�AғuAҕ�AҍPA҉7A҇+A҇+AҋDA҅A�|�A�|�A�z�A�x�A�r�A�n�A�l�A�n�A�p�A�n�A�S�A�I�A�VA�;dA�$�A�&�A��A��A� �A��A�bA�
=A�
=A�1A��HAћ�AэPA�p�A�bNA�A�A�/A���A���A��A��mA��TA��yA��;A�ƨAЧ�A�dZA��A�z�A�;dA��A��#A�ƨAΉ7A�=qA�
=A�z�A�jA�VA�~�A�+A��A�A��A���A���Aŏ\A�n�A�ȴA�C�AÉ7A�-A�C�A���A�^5A�33A�A��RA�I�A�~�A���A��A�/A���A��9A���A��A�l�A�ZA�C�A�{A��`A�VA�A�z�A�7LA���A�XA���A��mA��A�n�A��A��+A�Q�A��#A�+A���A�
=A��A��A��HA���A��FA��A�hsA�ĜA��yA�E�A���A���A�v�A�`BA�Q�A�5?A�&�A� �A��A�oA�
=A���A��A��/A��
A���A���A���A��-A��-A��A���A��+A�x�A�bNA�E�A�9XA�/A�"�A�A��/A�ĜA��A���A���A��7A�x�A�`BA�?}A�1'A�"�A�A���A��RA�VA�K�A�(�A���A�A��!A���A�p�A�ZA�M�A�A�A�5?A�&�A�bA��`A�A���A�r�A�^5A�M�A�9XA� �A�A��`A�A���A��hA��A�ffA�C�A� �A�A��
A���A�M�A�ƨA�-A��A�O�A�$�A�1A���A��hA�jA�M�A�33A�A��mA��#A���A�ƨA��A���A��+A�M�A�$�A�VA���A��A��^A���A��A�9XA��+A��!A��A��9A��FA��9A���A���A�r�A�VA�(�A�bA���A��A��HA���A��FA���A��uA�~�A�ffA�G�A�+A��#A���A���A�x�A�$�A���A�jA�G�A�&�A�{A�VA��A���A��RA���A��DA�O�A�A���A���A��PA�|�A�r�A�dZA�Q�A�;dA� �A�  A���A��A�z�A�A�A�VA��A�ĜA���A�~�A�^5A�?}A�"�A�oA���A��#A�A��-A���A��uA�~�A�jA�I�A�33A�oA���A��;A���A���A���A��!A��+A�O�A�5?A�oA��TA�ffA��jA�ZA�9XA�
=A��9A�v�A�?}A���A���A���A�bNA�$�A�
=A�A���A��A��/A���A��RA���A�r�A��A��9A�A�ȴA�^5A��A��
A��A� �A���A��mA�A�A��`A�z�A�+A��A�A���A�M�A��#A�t�A�1'A�+A�%A���A��mA��
A���A�ĜA�ĜA��9A���A��A�VA�%A�A���A��7A�+A��;A�r�A��`A�ZA�33A��7A��;A���A�C�A��A���A�`BA�bNA�K�A���A��A�M�A�?}A�-A�{A�  A��A�A�
A��A�AhsAO�A~��A}�A{�;A{��A{t�A{33Az��Az�Az^5Az1'AyƨAydZAxĜAxQ�Aw�Aw��Av�AvVAt�`At  As�AshsAsVAr�DAr9XAr9XAr  Aq�
Aq��AqC�Ap�HAp��Apv�Ao�TAo�hAoO�Ao
=An��An�Am�wAm`BAlĜAl1Ak�AkhsAjr�Ai�Ai�^Ai��Ai�AiS�Ai;dAi&�Ai�Ai�AiVAh��AgXAg%Af�jAfffAe�FAd��Ad�9Ac�wAc"�Ab��Ab^5Ab1'Aa�7A`�yA`M�A_�A_dZA^�A]�#A]\)A]A\�RA\�uA\n�A\ZA\I�A\{A[��A[��A[�hA[l�A[7LAZ�`AZ��AZbNAZQ�AZ �AY�#AY��AY+AXȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
�fB
��B
��B
��B
��B
�1B
��B
�fB
�1B
�1B
�fB
�1B
��B
�fB
�fB
�1B
�1B
�1B
�1B
�fB
�fB
�fB
�fB
��B
�B
��B
��B
�B
��B
�B
��B
�B
�B
�7B
��B
��B
�7B
��B
��B
��B
��B
�YB
��B
��B
}�B
s�B
m�B
v�B
��B
�B
�WB
��B/OBOvBp;B��B�ByrB��B��B�lB��B�AB�oB�+B�@B��Ba�B-�B�Bs�Bv�Bn�B[WBL�B>�B)*B�B�B
��B
��B
�*B
�hB
[WB
=�B
.IB
xB	�ZB	ʌB	�LB	�kB	��B	��B	��B	cB	wfB	oiB	r�B	s�B	~(B	�B	��B	�7B	��B	��B	�B	��B	xB	cTB	QB	R�B	Y�B	`vB	f�B	m�B	�YB	� B	�CB	��B	�EB	��B	��B	�#B	��B	ߤB	��B	��B
�B
&B
3�B
?HB
C�B
Q�B
X�B
ZQB
`�B
]/B
d&B
]�B
e�B
qB
}"B
tB
jB
d�B
_;B
^�B
^5B
b�B
k�B
i�B
iyB
jB
lWB
g8B
gmB
e�B
Z�B
gB
h
B
dZB
\�B
V�B
^B
e�B
\]B
W�B
T,B
VmB
W�B
Y�B
^5B
b�B
gmB
kQB
k�B
jB
h�B
f�B
d&B
^5B
_�B
`�B
_�B
f2B
f�B
f�B
aHB
^5B
b�B
`vB
a|B
^jB
^B
X�B
V9B
T�B
[�B
_�B
^5B
[#B
X�B
YB
YKB
W?B
WsB
W
B
W?B
Y�B
ZQB
ZB
YB
YKB
YKB
ZB
[�B
\]B
^�B
Z�B
X�B
Z�B
S�B
S[B
VmB
T�B
S�B
R B
P�B
P}B
OvB
O�B
MjB
K^B
I�B
F?B
FB
D3B
A�B
C-B
A�B
A�B
@�B
A�B
?}B
?}B
=B
;0B
:*B
8�B
8B
8RB
7�B
6zB
4�B
4nB
3�B
2�B
3hB
0�B
1'B
1'B
/�B
4nB
5B
3�B
2�B
3�B
3�B
3hB
2�B
2�B
2�B
4B
1�B
1'B
0�B
/�B
.�B
1�B
,�B
,=B
,B
+6B
(�B
'�B
'�B
'�B
)�B
'�B
%�B
$tB
#�B
#:B
 �B
 'B
�B
�B
qB
=B
kB
kB
B
�B
eB
YB
B
�B
�B
B
oB
4B
bB
.B
bB
�B
�B
B
uB
{B
�B
SB
B
B
�B
SB
�B
�B
�B
�B
�B
B
:B
�B
�B
B
oB
�B
oB
�B
4B
bB
�B
bB
�B
�B
�B
4B
:B
�B
�B
hB
 B
.B
�B
�B
.B
�B
�B
.B
�B
�B
�B
�B
�B
bB
 B
 B
:B
�B
:B
�B
�B
{B
FB
FB
{B
FB
FB
FB
�B
�B
�B
B
{B
�B
FB
B
B
B
FB
B
MB
�B
�B
�B
B
B
�B
SB
SB
$B
�B
�B
�B
+B
�B
�B
�B
�B
�B
1B
eB
�B
7B
�B
=B
=B
B
=B
qB
�B
qB
qB
B
CB
CB
IB
~B
�B
OB
OB
VB
�B
OB
OB
�B
�B
 \B
!�B
!�B
!-B
!�B
"hB
#nB
#�B
#�B
$�B
%B
%B
%FB
$�B
%B
%�B
&B
&�B
'�B
'�B
(XB
(�B
(�B
)�B
)�B
*eB
)�B
)�B
)�B
*�B
+B
+6B
+kB
+�B
+kB
+�B
+�B
,B
,B
+�B
,=B
,�B
,�B
,�B
-B
-�B
-B
.�B
.�B
.}B
.�B
.�B
.�B
.�B
/OB
/B
/B
.�B
/�B
/B
/�B
/�B
0!B
0�B
0�B
1�B
1'B
1[B
1[B
1[B
1�B
1�B
1�B
1�B
2�B
2�B
2aB
2-B
2-B
2�B
33B
3hB
33B
3�B
3�B
4�B
3�B
5B
4nB
4nB
4�B
4�B
6B
5tB
5tB
5�B
6zB
6FB
7B
6�B
6�B
6�B
7B
7LB
7�B
8B
8�B
8�B
8�B
8�B
9$B
9$B
9�B
9XB
8�B
9�B
:*B
:^B
;0B
;�B
;dB
;dB
;�B
<B
<B
<jB
<�B
<6B
<�B
=<B
=�B
=�B
=�B
=�B
=�B
>wB
>wB
>wB
>�B
?B
?HB
@�B
@B
@B
@B
A�B
AUB
A B
A�B
B'B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
CaB
C�B
D3B
D3B
E9B
E�B
F?B
FB
FtB
GB
GzB
GEB
GzB
GzB
GzB
G�B
G�B
GzB
G�B
H�B
H�B
IRB
IB
I�B
J#B
JXB
J�B
J#B
K)B
K^B
K^B
K^B
K�B
L�B
L�B
M6B
MB
M�B
M�B
M�B
NB
NB
NB
NB
NB
N<B
NpB
OBB
N�B
OvB
O�B
O�B
PB
P�B
P�B
P�B
P�B
Q�B
R B
RTB
Q�B
R�B
R�B
TaB
TaB
T,B
T�B
U2B
U2B
UgB
U2B
T�B
T�B
V�B
V�B
VmB
XB
W�B
W�B
XB
XB
XyB
X�B
YB
YB
Y�B
Y�B
YB
ZQB
ZQB
[#B
[#B
[#B
[WB
[�B
\)B
[�B
\�B
\�B
\�B
]/B
]/B
\�B
]/B
^5B
^B
^�B
^jB
^jB
^jB
^�B
_;B
_pB
_;B
_pB
_�B
_pB
_�B
`B
`BB
`�B
`vB
`�B
aB
`�B
aB
`�B
aHB
aHB
a�B
b�B
b�B
b�B
b�B
b�B
c B
b�B
cTB
c�B
c�B
c�B
cTB
c�B
d&B
c�B
c�B
c�B
c�B
dZB
d�B
d�B
e,B
e`B
e�B
e�B
e�B
e,B
e`B
e,B
e,B
e`B
e�B
e�B
f2B
e�B
f2B
f�B
f�B
f�B
f�B
f�B
gB
g8B
gB
gB
gmB
g�B
g�B
h
B
h>B
hsB
h�B
h�B
iB
jB
jB
jB
j�B
j�B
kQB
kQB
kQB
kQB
k�B
k�B
k�B
k�B
l"B
l"B
l"B
l"B
l"B
l�B
l�B
l�B
l�B
m)B
m�B
m�B
n/B
n�B
n�B
n�B
o B
o B
oiB
o�B
pB
pB
poB
qB
qvB
q�B
q�B
q�B
q�B
q�B
q�B
rB
rB
r|B
r|B
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
t�B
tTB
t�B
t�B
t�B
uZB
t�B
u%B
u�B
u�B
v+B
v�B
v�B
wfB
w�B
wfB
w�B
x8B
x8B
xB
xlB
x8B
xlB
x�B
x�B
x�B
y�B
y�B
y�B
zB
zB
zB
zDB
zB
zDB
zB
zxB
{JB
{�B
{�B
{�B
|B
|PB
|PB
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}VB
}�B
~(B
~(B
~(B
~(B
~]B
~�B
~�B
~�B
.B
�B
� B
� B
�4B
��B
��B
��B
��B
�oB
��B
�oB
�oB
��B
�B
�B
�B
�B
��B
��B
�B
�GB
��B
��B
��B
��B
�MB
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�%B
�%B
�%B
�YB
��B
��B
��B
��B
��B
��B
��B
�1B
��B
��B
��B
��B
��B
�B
�lB
��B
�	B
�	B
�=B
�=B
�rB
��B
��B
��B
��B
��B
�B
�B
��B
�DB
�xB
��B
�B
�B
�JB
�JB
�~B
��B
��B
��B
��B
��B
�PB
�B
�B
��B
��B
��B
��B
�VB
��B
��B
��B
��B
�VB
�(B
�(B
�(B
�(B
��B
��B
��B
��B
�bB
�bB
�bB
��B
�_B
��B
�+B
��B
��B
��B
��B
�YB
�fB
��B
�B
��B
��B
��B
��B
��B
�1B
�7B
��B
��B
��B
�=B
��B
��B
�B
��B
��B
��B
��B
��B
�fB
��B
�1B
��B
�fB
�7B
�+B
��B
��B
�lB
��B
��B
��B
��B
�B
�YB
�_B
�fB
�fB
�7B
�_B
��B
�lB
�7B
��B
��B
�B
��B
�_B
�B
�B
�fB
��B
��B
�+B
�1B
�+B
�_B
�_B
�	B
�1B
��B
�+B
�7B
��B
�+B
�+B
��B
�7B
�7B
�_B
��B
�fB
�lB
�1B
��B
�fB
��B
��B
�+B
��B
�B
�lB
�+B
�_B
��B
��B
��B
��B
��B
��B
�lB
��B
��B
�1B
�7B
�fB
��B
��B
�B
�B
��B
��B
�fB
��B
��B
��B
�B
�lB
��B
��B
��B
��B
��B
��B
�lB
�7B
�_B
�fB
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
�B
��B
��B
�1B
�fB
�lB
�lB
�1B
�1B
��B
��B
��B
��B
�B
��B
�lB
�fB
��B
�lB
��B
�lB
��B
�1B
�lB
�=B
�B
��B
��B
�	B
��B
�fB
��B
��B
�	B
�7B
�fB
�fB
�lB
�=B
��B
��B
�B
�lB
��B
��B
��B
��B
��B
�fB
��B
��B
�lB
��B
�B
�B
��B
�B
�B
�lB
�_B
��B
�=B
��B
��B
�B
��B
�7B
��B
��B
�	B
��B
�fB
��B
�7B
�lB
��B
��B
��B
�B
��B
�fB
�lB
��B
�7B
��B
�+B
��B
�7B
��B
��B
��B
�+B
��B
��B
��B
�B
�SB
��B
��B
��B
�DB
��B
��B
��B
�MB
��B
�B
�B
��B
�iB
~�B
�_B
��B
{B
~�B
|�B
~�B
zxB
wfB
s�B
v�B
u%B
r�B
oiB
p;B
s�B
o�B
sMB
�SB
rGB
a�B
d�B
b�B
Z�B
^�B
�lB
c B
��B
�AB
��B
�\B
��B
��B
��B
��B
�B
��B
��B
�zB
ɺB
�HB
��B
רB
�B
�B
�B�B
�B
��B
�B%B�B
	B�B"hBP�BaBT,BNpBN�BK^BQ�BU2BhsBsB{�B�B�B�bB��B��B�B�BzxB�&B�VBv+B��Bv�B~�Br|Bs�ByrB}"By�BzDB��B��B��B��B��B�(B�DB�B�xB�PB�	B�rB��B�	B�	B��B�7B��B��B��B��B�lB��B��B��B�fB�+B��B�=B��B�B�GB�uB��B��B�B~�B�B~�B~�B�;B�oB��B~�B.B}�B}�B�"BcB�B��B�_B�_B�MB�1B�	B�	B��B�B��B��B��B��B�1B�1B��B��B� B��B�(B��B��B�B�fB�fB��B��B.Bx�BsBo5BsMBrBq�B`�Bf�BGEBE9B@�B=<B8�B5tB0UB0�B0�B(�B'B%FB#�B$tB�BVB(XB�B�B"�B�B.B1B#�B%�BV9Bn/Br�Bp�Bu%BwfB.B}�B}�B��B�ABzDBv�By�Bu�Bu�Bu�BtTBr�Bs�Bt�Bp;BqB{Bh
Bs�Bk�Bp;Bq�Ba�BaHB_pB\�B[#B_;B^5BW�BX�BU2B`vBUgBS�BOBBK)BJ�BH�BIRBH�BGzBE�BDgBC�B@�B?}B>BB<jB1�B5tB1�B,=B*�B(�B'�B#:B$@B#nB!-B!BxBxB�B7B�BYB_BFBFB\B�B�B�B�B�B�B�B�B�B:B
�B
�B
�B
��B
�B
��B
��B
�gB
��B
��B
͟B
�B
��B
��B
��B
�OB
��B
��B
��B
��B
��B
�wB
��B
��B
�+B
��B
�%B
~(B
uZB
u�B
zxB
kQB
ZB
\]B
R�B
K)B
JXB
FB
J�B
JXB
K�B
33B
<B
5?B
1�B
5tB
49B
1�B
/�B
,�B
-B
/�B
)�B
,�B
*�B
+B
CB
$B
-CB
�B
	B
bB
B
%B
	�B
GB	��B	��B	ݘB	خB	�B	�B	ѷB	��B	�B	ѷB	��B	�3B	��B	�UB	�0B	��B	��B	�6B	�hB	��B	��B	�}B	��B	�hB	��B	�4B	��B	��B	�\B	��B	�CB	�'B	��B	��B	�B	��B	��B	�B	��B	�tB	� B	�B	��B	��B	�FB	��B	~]B	��B	�{B	��B	��B	�AB	}�B	�B	��B	�B	~�B	.B	��B	�B	{B	y�B	�iB	�B	p�B	s�B	cB	u�B	p�B	n�B	qvB	ncB	m�B	l�B	k�B	i�B	iDB	y�B	t�B	m]B	n/B	oiB	{JB	sB	r�B	qvB	q�B	sB	kB	o B	y	B	t�B	tB	zB	{JB	}VB	�MB	��B	}�B	~�B	}�B	.B	}"B	}�B	��B	��B	�;B	~�B	�4B	��B	��B	��B	��B	��B	��B	��B	�rB	��B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                 B
��B
�FB
��B
�B
�zB
��B
�LB
��B
��B
�zB
�B
��B
��B
�B
��B
�FB
�B
�B
��B
��B
��B
��B
�B
�B
�B
�B
�LB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�RB
��B
��B
��B
��B
�LB
�zB
�B
~3B
|\B
wqB
m4B
gCB
p�B
��B
��B
�	B
�IB)BI(Bi�B�UB��Bs$B�@B��B�B�zB{�B{!B��B��B�FB[cB'�B�Bm4Bp�Bh~BU	BFB8]B"�B�BqB
�YB
˞B
��B
�B
U	B
7�B
'�B
*B	�B	�>B	��B	�B	��B	�kB	~3B	yB	qB	iB	l�B	m�B	w�B	yIB	~hB	��B	�HB	�gB	��B	��B	q�B	]B	J�B	L;B	SfB	Z(B	`�B	g�B	�B	��B	��B	�UB	��B	�CB	�B	��B	ΰB	�VB	�B	�B
?B
�B
-�B
8�B
=�B
KiB
R`B
TB
Z�B
V�B
]�B
WJB
_�B
j�B
v�B
m�B
c�B
^uB
X�B
X�B
W�B
\4B
e�B
c_B
c+B
d1B
f	B
`�B
aB
_GB
TlB
`�B
a�B
^B
V�B
PSB
W�B
_{B
VB
Q�B
M�B
PB
QZB
SfB
W�B
\�B
aB
eB
elB
c�B
b�B
`MB
]�B
W�B
Y�B
Z\B
YVB
_�B
`MB
`�B
Z�B
W�B
\4B
Z(B
[.B
XB
W�B
R�B
O�B
NGB
U>B
Y�B
W�B
T�B
R`B
R�B
R�B
P�B
Q%B
P�B
P�B
S�B
TB
S�B
R�B
R�B
R�B
S�B
U�B
VB
X�B
TlB
R`B
TlB
MuB
MB
PB
NGB
MAB
K�B
J�B
J/B
I(B
I]B
GB
EB
C�B
?�B
?�B
=�B
;�B
<�B
;�B
;�B
:jB
;;B
9/B
9/B
6�B
4�B
3�B
2�B
1�B
2B
1gB
0,B
.TB
. B
-NB
,HB
-B
*�B
*�B
*�B
)�B
. B
.�B
-NB
,|B
-NB
-NB
-B
,�B
,|B
,�B
-�B
+vB
*�B
*<B
)�B
(�B
+vB
&�B
%�B
%�B
$�B
"�B
!�B
!mB
!9B
#yB
!mB
aB
&B
�B
�B
�B
�B
6B
^B
#B
�B
B
B
�B
zB
B
B
�B
[B
gB
�B
!B

�B

B
	�B

B

}B
OB
�B
'B
-B
3B
B
�B
�B
9B
B
�B
9B
9B
�B
aB
�B
�B
OB
OB
�B
!B
UB
!B
�B

�B

B

IB

B

}B
OB

}B

�B
�B
OB
�B
B

�B
	�B
	wB

IB
	�B
	wB
	wB
	�B
	wB
	wB
	CB
	wB
	�B

B

�B

�B
�B
�B
�B
�B
[B
-B
�B
�B
-B
�B
�B
�B
�B
�B
�B
�B
-B
aB
�B
�B
�B
�B
�B
�B
�B
gB
gB
3B
�B
�B
9B
B
B
�B
tB
�B
?B
�B
EB
EB
�B
EB
�B
�B
B
�B
�B
RB
�B
�B
�B
�B
#B
XB
#B
#B
�B
�B
�B
�B
0B
dB
B
B
B
jB
B
B
6B
�B
B
�B
�B
�B
}B
B
 B
�B
�B
�B
�B
�B
�B
�B
�B
aB
�B
 3B
!mB
!mB
"
B
"?B
"sB
#yB
#EB
$B
#EB
#yB
#yB
$KB
$�B
$�B
%B
%QB
%B
%QB
%QB
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'^B
&�B
(�B
(dB
(/B
(�B
(�B
(dB
(dB
)B
(�B
(�B
(�B
)5B
(�B
)5B
)jB
)�B
*pB
*�B
+BB
*�B
+B
+B
+B
+BB
+�B
+�B
+�B
,HB
,HB
,B
+�B
+�B
,|B
,�B
-B
,�B
-�B
-�B
.TB
-�B
.�B
. B
. B
.TB
.TB
/�B
/&B
/&B
/ZB
0,B
/�B
0�B
0�B
0`B
0`B
0�B
0�B
1gB
1�B
28B
28B
28B
28B
2�B
2�B
3sB
3
B
2�B
3>B
3�B
4B
4�B
5B
5B
5B
5B
5�B
5�B
6B
6QB
5�B
6�B
6�B
7WB
7WB
7WB
7�B
7�B
8)B
8)B
8)B
8]B
8�B
8�B
:5B
9�B
9�B
9�B
;;B
;B
:�B
;�B
;�B
<AB
<AB
<AB
<AB
<�B
<vB
<vB
=|B
=B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
@&B
@�B
A,B
@�B
A,B
A,B
A,B
A`B
A�B
A,B
A�B
B2B
BfB
CB
B�B
C8B
C�B
D
B
D>B
C�B
D�B
EB
EB
EB
E�B
FB
FB
F�B
F�B
GQB
G�B
GQB
G�B
G�B
G�B
G�B
G�B
G�B
H"B
H�B
H�B
I(B
I�B
I�B
I�B
JcB
J�B
JcB
JcB
K5B
K�B
LB
K�B
L;B
LoB
NB
NB
M�B
N|B
N�B
N�B
OB
N�B
N�B
N�B
PSB
P�B
PB
Q�B
Q�B
Q�B
Q�B
Q�B
R+B
R�B
R�B
R�B
SfB
S�B
S1B
TB
TB
T�B
T�B
T�B
U	B
UrB
U�B
UrB
VDB
V�B
VxB
V�B
V�B
V�B
V�B
W�B
W�B
XPB
XB
XB
XB
XPB
X�B
Y"B
X�B
Y"B
YVB
Y"B
YVB
Y�B
Y�B
Z\B
Z(B
Z\B
Z�B
Z\B
Z�B
Z�B
Z�B
Z�B
[�B
\4B
\�B
\iB
\iB
\�B
\�B
\�B
]B
]oB
]:B
]:B
]B
]oB
]�B
]�B
]:B
]oB
]oB
^B
^uB
^�B
^�B
_B
_GB
_GB
_GB
^�B
_B
^�B
^�B
_B
_{B
_GB
_�B
_�B
_�B
`MB
`MB
`MB
`MB
`�B
`�B
`�B
`�B
`�B
aB
aSB
aSB
a�B
a�B
b%B
b�B
bYB
b�B
c�B
c�B
c�B
deB
deB
eB
eB
eB
eB
e7B
e7B
e7B
e7B
e�B
e�B
e�B
e�B
e�B
f=B
f=B
f�B
frB
f�B
gxB
gxB
g�B
hJB
hJB
h~B
h�B
h�B
iB
i�B
i�B
i�B
j!B
j�B
k(B
k\B
k�B
k�B
k\B
k�B
k�B
k�B
k�B
l.B
l.B
lbB
lbB
l�B
lbB
l�B
l�B
m4B
m�B
n:B
nB
n:B
n�B
n�B
oB
n�B
n�B
o�B
o�B
o�B
pFB
p�B
qB
qLB
qB
qLB
q�B
q�B
q�B
rB
q�B
rB
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
s�B
s�B
s�B
t*B
t�B
ueB
u�B
ueB
u�B
vB
vB
v7B
v7B
v7B
v7B
v�B
v�B
wB
wB
wqB
w�B
w�B
w�B
w�B
xB
xCB
xwB
x�B
x�B
yIB
y�B
y�B
y�B
zOB
z�B
zOB
z�B
{!B
{UB
{!B
{!B
{UB
{�B
{�B
{�B
{�B
|\B
|\B
|�B
|�B
}bB
}bB
}�B
}bB
}�B
~3B
~3B
~hB
~�B
~�B
:B
:B
nB
:B
nB
�B
�B
�B
�B
�@B
�tB
��B
��B
�FB
�FB
�zB
��B
��B
�LB
�LB
��B
�LB
��B
�B
��B
��B
��B
��B
��B
�$B
�XB
�XB
��B
�XB
��B
��B
��B
��B
��B
�*B
��B
��B
��B
��B
��B
�0B
�eB
�eB
��B
��B
�eB
�B
��B
��B
�kB
�6B
��B
�<B
�B
�<B
�<B
�<B
�qB
�B
��B
��B
��B
��B
�CB
�wB
�wB
�wB
�B
�B
�B
�wB
�B
��B
��B
��B
��B
�zB
�zB
�B
�B
�RB
��B
��B
�LB
�LB
��B
�tB
��B
��B
��B
�@B
�LB
��B
��B
��B
��B
��B
��B
�LB
��B
�@B
�B
��B
��B
��B
�B
��B
��B
�tB
�RB
�B
��B
�@B
��B
��B
��B
�B
�B
�B
�B
��B
�B
�zB
�B
��B
��B
�tB
��B
��B
�B
��B
��B
�B
��B
�B
��B
��B
��B
�B
�B
��B
��B
�RB
��B
��B
��B
��B
��B
�LB
��B
��B
�B
�tB
�B
�B
��B
�FB
�B
��B
�LB
��B
�FB
��B
�B
��B
�B
�LB
��B
�LB
�FB
�zB
�LB
�B
�LB
�FB
��B
��B
�B
�FB
�FB
��B
��B
�FB
�FB
�B
�RB
��B
�FB
��B
�B
�zB
�FB
��B
�RB
�FB
�zB
�B
��B
�B
�B
�RB
��B
�FB
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
�LB
��B
�B
�B
�B
��B
��B
�RB
�RB
��B
�zB
��B
��B
�B
�B
��B
�B
��B
�B
��B
��B
�B
��B
��B
�zB
��B
��B
�RB
�B
�zB
�LB
��B
��B
�B
�B
�B
��B
�LB
��B
��B
�B
�RB
��B
��B
��B
��B
�B
�LB
�RB
�B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
�LB
��B
��B
��B
�LB
��B
��B
�LB
�B
��B
��B
�B
�LB
�zB
�zB
��B
��B
�B
�B
�zB
��B
�LB
��B
�LB
��B
�LB
�FB
��B
��B
�FB
�zB
�@B
~�B
B
�6B
�RB
}bB
��B
�tB
|�B
�B
}�B
{�B
|�B
}�B
|�B
zB
xCB
�B
zOB
u1B
x�B
v�B
x�B
t*B
qB
m4B
pFB
n�B
lbB
iB
i�B
m4B
iPB
l�B
B
k�B
[�B
^uB
\4B
TlB
XPB
�B
\�B
��B
{�B
:B
�B
�IB
�OB
��B
��B
��B
��B
��B
�,B
�lB
��B
˞B
�ZB
��B
�VB
�B<B
�hB
�7B
��B
��B
�bB�B
�nBBJ�BZ�BM�BH"BHWBEBK�BN�Bb%Bl�BueByIB}�B�B}bB�HB��Bz�Bt*B��B�Bo�B:Bp{BxCBl.BmhBs$Bv�BsYBs�B��B��B��B�3B�aB��B��B��B�*B�B��B�$B��B��B��B�RB��B��B��B��B�LB�B�zB:B�B�B��B�tB��B��B}�B|�B|'B�B{UB|�BxwByIBxCBxCBz�B{!B|�Bx�Bx�Bw�BwqB��ByBy~B}bB�B�B}�B��B��B��B��B��B�eB��B��B�zB��B��B�3B��B��B�CB��B�CB��B��B�B�Bz�B~hBx�Br�Bl�Bh�Bl�Bk�Bk�BZ\B`�B@�B>�B:5B6�B28B/&B*B*pB*�B"sB �B�BUB&B�BB"
B�B�BNB	CB	�B�B�BaBO�Bg�BlbBj�Bn�BqBx�Bw�BwqB{UB{�Bs�Bp�Bs�Bo�Bo�Bo�BnBlbBmhBnnBi�Bj�Bu1Ba�Bm�Be�Bi�Bk�B[cBZ�BY"BV�BT�BX�BW�BQ�BR`BN�BZ(BOBMABH�BD�BD�BB�BCBB2BA,B?TB>B=�B:5B9/B7�B6B+�B/&B+vB%�B$�B"?B!�B�B�B B�B�B*B*BXB�B�BBB�B�B	BqB�BkB�BkB
��B
��B
�:B	CB�B
�B
�eB
�B
�B
�B
ݣB
�~B
�B
ЈB
˞B
�QB
��B
��B
�;B
�jB
�B
�KB
�B
�TB
�ZB
��B
�)B
��B
��B
��B
�UB
�B
w�B
oB
o@B
t*B
eB
S�B
VB
L�B
D�B
D
B
?�B
D>B
D
B
E�B
,�B
5�B
.�B
+�B
/&B
-�B
+vB
)�B
&�B
&�B
)5B
#yB
&�B
$KB
$�B
�B
�B
&�B
eB
�B

B
�B
�B
�B	��B	�B	�uB	�JB	�`B	�lB	��B	�iB	�B	��B	�iB	��B	��B	�WB	�B	��B	�2B	��B	��B	�B	�|B	�jB	�/B	ȋB	�B	�[B	��B	��B	�aB	�B	�^B	��B	��B	��B	��B	��B	�UB	��B	��B	�aB	�&B	��B	��B	��B	|�B	��B	�B	xB	��B	}-B	~�B	��B	{�B	wqB	��B	��B	{�B	xwB	x�B	{�B	y~B	t�B	s�B	zB	z�B	jVB	m4B	yB	ouB	j�B	hJB	k(B	hB	g�B	f�B	e�B	c�B	b�B	s�B	n:B	gB	g�B	iB	t�B	l�B	l�B	k(B	k\B	l�B	d�B	h�B	r�B	n�B	m�B	s�B	t�B	wB	}�B	z�B	wqB	x�B	wqB	x�B	v�B	w�B	|\B	{�B	z�B	xwB	y�B	~hB	�B	��B	�B	}�B	�FB	�zB	�$B	�6B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0061566                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 24 11 2022 179 -0.0061566 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230730223105                            20230730223105AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023073022310520230730223105  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023073022310520230730223105QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023073022310520230730223105QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               