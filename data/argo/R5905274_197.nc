CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-31T14:00:45Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɠ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  р   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � `   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � g    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230531140045  20230531140045  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�,���z@�,���z11  @�,�  �@�,�  �@0?��f�@0?��f��d$лn�w�d$лn�w11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?k�?��H@=p�@z�H@�p�@��R@�  A   A  A ��A+�A?\)A`  A�  A�  A��A��A��AϮA߮A�  B (�B(�B  B�
B�B'�B0(�B8(�B?�BG�BO�
BX(�B`(�Bg�
Bp  Bx(�B�
B�B��
B�  B�{B�  B�  B�{B�(�B�=qB�{B�  B�  B��B��B��B��B�  B�  B��
B��B�{B�{B�{B�{B��B�  B�{B��B��B�  B��C   C  C��C  C  C
  C
=C
=C(�C��C  C  C
=C  C  C
=C 
=C"
=C$
=C%��C(  C*
=C,  C.  C0  C2  C3��C5�C7��C9�C<  C>
=C@  CB  CD  CF  CH{CJ{CL
=CN
=CP  CQ�CT  CV  CW�HCY��C\
=C]��C`  Cb  Cc�HCe��Ch
=Cj{Cl  Cm��Cp  Cr
=Ct  Cu��Cw��Cz  C|  C}��C��C�  C�C�C���C���C�  C�  C�  C�C�  C�  C���C���C�C�C�  C�C�C�C�C�  C���C��C���C�  C���C���C�  C���C���C���C�
=C�
=C�  C�  C�  C���C���C���C���C���C�  C�C���C�  C�  C���C�  C�C�  C�C�
=C�C�  C���C�C�  C���C��C���C�
=C�  C���C���C���C�  C�C�C�C�C�C�C�  C�  C���C���C���C�  C�C���C��C���C�  C�C�C�C���C�C�C���C�  C�  C���C�  C���C�C�  C���C�C�  C��C��C���C�C�C�
=C�C���C���C���C�  C���C���C���C���C�C�
=C�
=C�C�C�C���C���C���C���C���C���D   D z�D ��D}qD  D� D�D}qD��D}qD  D� D�qD}qD�D�=D�D��D	D	��D
  D
��D�D��DD�D�D�D�D��D  D}qD�qD}qD��Dz�D��D}qD�qD� D  D��D�qD}qD  Dz�D  D��D�qD� D  D}qD  D}qD��DxRD�qD� DD��DD��D  D��D �D �D!�D!� D"  D"��D#�D#��D#�qD$}qD%  D%}qD&�D&�D'  D'�D(
=D(��D)D)��D*  D*� D*�qD+}qD,�D,� D,��D-z�D.  D.}qD.��D/� D0D0��D0�qD1�D2�D2xRD3  D3��D3�qD4� D4��D5��D6  D6� D7  D7��D7�qD8� D9�D9}qD:�D:��D;�D;� D<  D<}qD<�RD=z�D>  D>}qD>�RD?z�D@  D@�DA�DA� DB�DB��DCDC� DC�qDD� DEDE��DFDF�DGDG�DH  DH��DI  DI}qDJ  DJ��DKDK� DK�qDL� DM�DM��DNDN��DO�DO��DP  DPz�DP�qDQ� DR�DR��DS�DS��DTDT��DU  DU��DV  DV� DV�qDW� DX  DX� DX�qDYxRDY�qDZ��D[�D[��D[�qD\}qD]  D]��D^  D^��D_D_��D`�D`��Da  Da}qDb  Db��Dc�Dc}qDc�qDd� De�De�DfDf�DgDg��Dh�Dh� Dh��Di� Dj  Djz�Dj�qDk� Dl�Dl��Dl�qDm}qDm�qDn��Do  Do}qDo�qDp� Dp�qDq� Dr�Dr� Ds�Ds�Dt  Dt��DuDu��Dv  Dv� DwDw� Dw�qDxz�Dx�qDy��Dz�Dz� D{  D{}qD{�RD|}qD}�D}��D}�RD~}qD  D� D�  D�>�D��HD�� D�HD�AHD�� D�D�HD�@ D�~�D�� D�  D�C�D��HD��qD��qD�>�D�� D��HD�HD�=qD�~�D�D���D�@ D��HD��qD�  D�@ D�� D�� D�HD�>�D��HD�� D���D�=qD�� D���D�HD�B�D��HD�� D���D�>�D�� D��HD�HD�@ D�� D�� D�  D�AHD�� D���D�  D�AHD��HD��HD���D�=qD��HD��HD��D�@ D�� D���D��D�AHD��HD��qD�HD�AHD���D���D�HD�>�D��HD��HD�  D�AHD�~�D��HD�  D�AHD��HD���D�  D�@ D�� D���D�HD�@ D�~�D��HD��D�@ D��HD��HD�HD�@ D�~�D���D�HD�AHD�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D�D�  D�AHD��HD�� D�HD�@ D�~�D���D�  D�AHD��HD�� D�HD�@ D�� D��HD�  D�>�D�� D���D���D�@ D�~�D���D���D�@ D��HD�� D���D�>�D�� D�D��qD�>�D�~�D�� D���D�>�D�� D��HD�HD�@ D��HD�D�HD�AHD��HD���D���D�AHD�� D�� D�  D�@ D�� D�� D��D�AHD�� D�� D���D�>�D�� D���D�  D�AHD�� D�� D�  D�AHD���D�� D���D�@ D��HD�� D�  D�@ D��HD���D���D�>�D�� D���D���D�>�D�� D���D���D�@ D�� D�� D�  D�AHD���D�D�  D�AHD��HD��qD�  D�>�D��HD�� D��qD�<)D�~�D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�HD�B�D���D��HD�  D�>�D�� D��qD���D�AHD�� D�D��D�AHD�� D�� D�  D�AHD��HD�� D�  D�AHD�� D��HD�HD�AHD�� D��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�AHDĀ Dľ�D�HD�@ Dŀ Dž�D�HD�@ Dƀ D�� D�  D�@ DǁHD�� D�HD�AHDȁHD��HD�HD�B�Dɀ Dɾ�D���D�AHD�~�D�� D���D�@ D�~�D˾�D���D�@ D́HD��HD�HD�@ D̀ D�� D��)D�=qD΁HDξ�D��qD�@ Dπ D��HD��qD�AHD�~�Dо�D�  D�@ Dр D�D�HD�B�DҀ D�� D���D�B�Dӂ�D�� D���D�>�DԁHD�D�HD�>�D�~�D��HD��qD�@ Dւ�D��HD�  D�>�D�}qD�� D���D�=qD؀ Dؾ�D��qD�=qDـ D�D�  D�>�DځHD�� D�  D�>�DہHD��HD�  D�>�D܀ Dܾ�D���D�@ D݁HD�� D�  D�>�Dހ D��HD�HD�AHD߁HD߾�D�HD�AHD��HDྸD���D�B�D�HD�D��D�B�D₏D�D�HD�AHD�HD�� D�HD�@ D� D��HD���D�AHD�HD�� D�  D�B�D�HD�� D���D�=qD�~�D�� D��D�@ D� D辸D���D�@ D��D�� D�HD�AHDꂏD�D�  D�AHD낏D��HD�HD�AHD� D쾸D�  D�>�D�~�D�� D��D�B�D�HD��HD�  D�B�DD�� D��qD�@ D���D��HD�HD�>�D�}qD�qD��qD�>�D�HD�D�HD�AHD�~�D�qD�  D�@ D�HD�� D��qD�AHD�� D���D���D�=qD��HD�� D�  D�>�D�}qD�D���D�AHD��HD�D���D�@ D�� D���D�  D�@ D�w
?�?B�\?�=q?��
?\?�G�@   @\)@!G�@0��@B�\@Q�@fff@s33@�G�@���@���@��H@��
@��@��@��H@��
@˅@��@ٙ�@�p�@�ff@�{@�z�@�(�A�\A�A	��A��A33A
=A=qA{A"�\A'
=A*=qA,��A0��A4z�A8��A<��A@��AE�AHQ�AL��AQG�AUAX��A]p�Aa�AeAi��Amp�Ar�\AuAx��A|��A���A��\A��A�
=A���A��A�{A�  A��\A�(�A�ffA���A��A�p�A�  A��A�(�A�ffA�Q�A��HA��A�\)A�Q�A��HA���A�
=A�G�A�33A�A��A��A�(�A�ffAȣ�A�33A�{A�  A�=qA�z�A�ffAأ�Aڏ\A�(�A�ffA�Q�A�\A�z�A�ffA���A�A�p�A�\)A�A�A���A��RA���A��\A�(�A�ffB (�B�B=qB�Bz�Bp�B�RB�B��B	��B
�\B33BQ�BG�BffB33BQ�BG�BffB�BG�BffB�B��BB�HB(�BG�B{B33B (�B!G�B"�\B$  B%�B&=qB'�B(��B)��B*ffB+\)B,��B-B.�RB0  B1�B2�\B3�
B4��B6{B733B8z�B9��B:�\B;�B<z�B=��B>�RB?�B@��BA�BC33BD  BEp�BF�\BG�BI�BJ{BK\)BLQ�BMG�BN{BO
=BP(�BQp�BR�\BS�BTz�BUBW
=BX(�BY��BZ�RB[�
B\��B^{B_\)B`Q�BaG�Bb=qBc\)Bdz�Be��Bf�HBg�
Bh��Bj{Bk33Bl(�Bm��Bn�RBo�
Bp��Br{Bs�Bt��Bu�Bw
=Bx(�ByG�BzffB{�B|��B}B
=B�{B��\B�
=B��B�{B��RB�33B��
B�ffB���B��B�{B���B�33B�B�Q�B�
=B��B�=qB���B�\)B��B�z�B�
=B���B�=qB���B�p�B��
B�Q�B���B�\)B�  B�z�B��B���B�ffB�
=B���B�(�B���B�\)B��B�(�B��RB�G�B��
B�ffB��HB�p�B�{B��RB�\)B��B�z�B�
=B���B�(�B��\B���B���B�  B��\B��B�B�ffB��HB�p�B��B�z�B��HB�G�B�B�=qB��RB�\)B��B��\B�
=B��B�{B���B��B��B�  B�z�B�33B��
B�ffB�
=B�p�B��
B�ffB���B�p�B�(�B���B��B�{B��\B��B��B�=qB���B��B�=qB���B�\)B�  B�z�B�
=B���B�(�B���B�G�B�(�B���B�\)B�  B�Q�B��HBÅB�(�B��HBŅB�  B�ffB�
=BǅB�Q�B���BɅB�(�Bʣ�B�33B˙�B�(�B̸RB�G�B�  BΣ�B�33BϮB�(�BиRB�33B��
Bҏ\B�33B�B�ffBԸRB�\)B��B�z�B��B��
B�z�B�
=BٮB�=qBڣ�B�33B�B�ffB��B��
B�ffB�
=B߅B�  B��\B�33B��B�\B�33B�B�(�B��B�p�B�(�B�RB�33B�B�ffB���B�B�Q�B���B�\)B�  B�z�B�\)B��B�\B���B�B�{B�RB�B�{B�z�B���B�B�Q�B��HB�p�B��
B�ffB���B���B�=qB���B�33B��B�=qB��HB���B�(�B��RB�33B�B�Q�B�
=B�C 33C �C �RC
=CQ�C�C{C\)C�C�C33C�C�HC=qC�\C�
C{CffC�RC�CffCC�C=qC�C  CG�C��C�
C	(�C	z�C	��C
=qC
�\C
�
C{CQ�C��C�Cp�C�C  CG�C�RC
=C\)C�\C�
C(�Cz�C�RC��C{C33Cz�C�C�
C��C�C
=C33CG�CG�CG�C\)Cp�C��C�RC�CC��C{C(�C(�C=qCG�Cz�C��C�RC��C�
C�C  C33CQ�Cp�C�C�\C�C�
C  C�C(�C=qCQ�C�C�CC�
C�C(�CQ�CQ�CffC��C��C�
C��C{CG�CffCp�C�C��C��C�C{CG�Cp�C�\C�\C�RC�HC
=C33C33CQ�C�\C��C�RC��C�C�C=qC\)C\)Cz�C�C�
C��C��C{CQ�C\)Cp�C�\CC�HC�C
=C=qCffC�C�\C�C�C
=C(�C=qCffC��C�C��C�C(�CG�CQ�Cz�C�RC�HC�C
=CQ�Cz�Cz�C��C�HC
=C(�C33C\)C��C�CC�HC (�C G�C Q�C z�C �RC �RC �HC ��C!=qC!Q�C!p�C!��C!��C!�
C!��C"�C"Q�C"ffC"z�C"�C"�HC"�
C#  C#=qC#\)C#ffC#�C#�RC#�HC#�C$�C$=qC$G�C$\)C$��C$C$C$�
C%�C%=qC%=qC%ffC%��C%C%C%�HC&(�C&G�C&\)C&z�C&��C&�
C&��C'
=C'33C'ffC'ffC'�\C'��C'�HC(  C((�C(\)C(ffC(�C(C(�HC(��C)33C)\)C)ffC)�C)C)�HC)��C*33C*\)C*ffC*�C*C*�
C*�C+33C+\)C+ffC+��C+��C+��C,  C,=qC,=qC,p�C,�C,�C,�HC-�C-�C-Q�C-�C-��C-�RC.  C.{C.(�C.p�C.��C.�C.�
C/{C/�C/=qC/�C/��C/�RC0  C0(�C033C0z�C0��C0�C0�C1�C1(�C1Q�C1��C1��C1�
C2{C2�C2\)C2�\C2��C2�HC3
=C3{C3\)C3�\C3�\C3�HC4
=C4{C4Q�C4�\C4��C4C5
=C5{C5G�C5�\C5��C5��C6{C6�C6Q�C6��C6C6�HC7�C7\)C7ffC7�RC7�HC8  C8G�C8p�C8��C8�HC9{C933C9�C9��C9��C:{C:33C:ffC:�RC:��C;  C;Q�C;ffC;��C;��C<
=C<\)C<�\C<��C<��C={C==qC=��C=�C=�HC>33C>G�C>z�C>��C>�C?(�C?p�C?�\C?�HC?��C@33C@z�C@�\C@�CA  CA33CA�CA��CA�CB{CBG�CB��CB�CC  CCG�CCQ�CC��CCCD
=CDG�CDp�CDCD�
CE{CEffCEz�CE��CE�CF(�CFffCF�CF�
CF��CGG�CGp�CG�\CG�HCH  CHG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111144111141111411111111111111111111111111114111111111111411114111141111111114111141111111111111111111111111111111114111111111111111411111111111111141111111111111141114111411111111111411111111111111111111111111111141141141141111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                       ?k�?��H@=p�@z�H@�p�@��R@�  A   A  A ��A+�A?\)A`  A�  A�  A��A��A��AϮA߮A�  B (�B(�B  B�
B�B'�B0(�B8(�B?�BG�BO�
BX(�B`(�Bg�
Bp  Bx(�B�
B�B��
B�  B�{B�  B�  B�{B�(�B�=qB�{B�  B�  B��B��B��B��B�  B�  B��
B��B�{B�{B�{B�{B��B�  B�{B��B��B�  B��C   C  C��C  C  C
  C
=C
=C(�C��C  C  C
=C  C  C
=C 
=C"
=C$
=C%��C(  C*
=C,  C.  C0  C2  C3��C5�C7��C9�C<  C>
=C@  CB  CD  CF  CH{CJ{CL
=CN
=CP  CQ�CT  CV  CW�HCY��C\
=C]��C`  Cb  Cc�HCe��Ch
=Cj{Cl  Cm��Cp  Cr
=Ct  Cu��Cw��Cz  C|  C}��C��C�  C�C�C���C���C�  C�  C�  C�C�  C�  C���C���C�C�C�  C�C�C�C�C�  C���C��C���C�  C���C���C�  C���C���C���C�
=C�
=C�  C�  C�  C���C���C���C���C���C�  C�C���C�  C�  C���C�  C�C�  C�C�
=C�C�  C���C�C�  C���C��C���C�
=C�  C���C���C���C�  C�C�C�C�C�C�C�  C�  C���C���C���C�  C�C���C��C���C�  C�C�C�C���C�C�C���C�  C�  C���C�  C���C�C�  C���C�C�  C��C��C���C�C�C�
=C�C���C���C���C�  C���C���C���C���C�C�
=C�
=C�C�C�C���C���C���C���C���C���D   D z�D ��D}qD  D� D�D}qD��D}qD  D� D�qD}qD�D�=D�D��D	D	��D
  D
��D�D��DD�D�D�D�D��D  D}qD�qD}qD��Dz�D��D}qD�qD� D  D��D�qD}qD  Dz�D  D��D�qD� D  D}qD  D}qD��DxRD�qD� DD��DD��D  D��D �D �D!�D!� D"  D"��D#�D#��D#�qD$}qD%  D%}qD&�D&�D'  D'�D(
=D(��D)D)��D*  D*� D*�qD+}qD,�D,� D,��D-z�D.  D.}qD.��D/� D0D0��D0�qD1�D2�D2xRD3  D3��D3�qD4� D4��D5��D6  D6� D7  D7��D7�qD8� D9�D9}qD:�D:��D;�D;� D<  D<}qD<�RD=z�D>  D>}qD>�RD?z�D@  D@�DA�DA� DB�DB��DCDC� DC�qDD� DEDE��DFDF�DGDG�DH  DH��DI  DI}qDJ  DJ��DKDK� DK�qDL� DM�DM��DNDN��DO�DO��DP  DPz�DP�qDQ� DR�DR��DS�DS��DTDT��DU  DU��DV  DV� DV�qDW� DX  DX� DX�qDYxRDY�qDZ��D[�D[��D[�qD\}qD]  D]��D^  D^��D_D_��D`�D`��Da  Da}qDb  Db��Dc�Dc}qDc�qDd� De�De�DfDf�DgDg��Dh�Dh� Dh��Di� Dj  Djz�Dj�qDk� Dl�Dl��Dl�qDm}qDm�qDn��Do  Do}qDo�qDp� Dp�qDq� Dr�Dr� Ds�Ds�Dt  Dt��DuDu��Dv  Dv� DwDw� Dw�qDxz�Dx�qDy��Dz�Dz� D{  D{}qD{�RD|}qD}�D}��D}�RD~}qD  D� D�  D�>�D��HD�� D�HD�AHD�� D�D�HD�@ D�~�D�� D�  D�C�D��HD��qD��qD�>�D�� D��HD�HD�=qD�~�D�D���D�@ D��HD��qD�  D�@ D�� D�� D�HD�>�D��HD�� D���D�=qD�� D���D�HD�B�D��HD�� D���D�>�D�� D��HD�HD�@ D�� D�� D�  D�AHD�� D���D�  D�AHD��HD��HD���D�=qD��HD��HD��D�@ D�� D���D��D�AHD��HD��qD�HD�AHD���D���D�HD�>�D��HD��HD�  D�AHD�~�D��HD�  D�AHD��HD���D�  D�@ D�� D���D�HD�@ D�~�D��HD��D�@ D��HD��HD�HD�@ D�~�D���D�HD�AHD�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D�D�  D�AHD��HD�� D�HD�@ D�~�D���D�  D�AHD��HD�� D�HD�@ D�� D��HD�  D�>�D�� D���D���D�@ D�~�D���D���D�@ D��HD�� D���D�>�D�� D�D��qD�>�D�~�D�� D���D�>�D�� D��HD�HD�@ D��HD�D�HD�AHD��HD���D���D�AHD�� D�� D�  D�@ D�� D�� D��D�AHD�� D�� D���D�>�D�� D���D�  D�AHD�� D�� D�  D�AHD���D�� D���D�@ D��HD�� D�  D�@ D��HD���D���D�>�D�� D���D���D�>�D�� D���D���D�@ D�� D�� D�  D�AHD���D�D�  D�AHD��HD��qD�  D�>�D��HD�� D��qD�<)D�~�D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�HD�B�D���D��HD�  D�>�D�� D��qD���D�AHD�� D�D��D�AHD�� D�� D�  D�AHD��HD�� D�  D�AHD�� D��HD�HD�AHD�� D��HD�  D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�AHDĀ Dľ�D�HD�@ Dŀ Dž�D�HD�@ Dƀ D�� D�  D�@ DǁHD�� D�HD�AHDȁHD��HD�HD�B�Dɀ Dɾ�D���D�AHD�~�D�� D���D�@ D�~�D˾�D���D�@ D́HD��HD�HD�@ D̀ D�� D��)D�=qD΁HDξ�D��qD�@ Dπ D��HD��qD�AHD�~�Dо�D�  D�@ Dр D�D�HD�B�DҀ D�� D���D�B�Dӂ�D�� D���D�>�DԁHD�D�HD�>�D�~�D��HD��qD�@ Dւ�D��HD�  D�>�D�}qD�� D���D�=qD؀ Dؾ�D��qD�=qDـ D�D�  D�>�DځHD�� D�  D�>�DہHD��HD�  D�>�D܀ Dܾ�D���D�@ D݁HD�� D�  D�>�Dހ D��HD�HD�AHD߁HD߾�D�HD�AHD��HDྸD���D�B�D�HD�D��D�B�D₏D�D�HD�AHD�HD�� D�HD�@ D� D��HD���D�AHD�HD�� D�  D�B�D�HD�� D���D�=qD�~�D�� D��D�@ D� D辸D���D�@ D��D�� D�HD�AHDꂏD�D�  D�AHD낏D��HD�HD�AHD� D쾸D�  D�>�D�~�D�� D��D�B�D�HD��HD�  D�B�DD�� D��qD�@ D���D��HD�HD�>�D�}qD�qD��qD�>�D�HD�D�HD�AHD�~�D�qD�  D�@ D�HD�� D��qD�AHD�� D���D���D�=qD��HD�� D�  D�>�D�}qD�D���D�AHD��HD�D���D�@ D�� D���D�  D�@ D�w
?�?B�\?�=q?��
?\?�G�@   @\)@!G�@0��@B�\@Q�@fff@s33@�G�@���@���@��H@��
@��@��@��H@��
@˅@��@ٙ�@�p�@�ff@�{@�z�@�(�A�\A�A	��A��A33A
=A=qA{A"�\A'
=A*=qA,��A0��A4z�A8��A<��A@��AE�AHQ�AL��AQG�AUAX��A]p�Aa�AeAi��Amp�Ar�\AuAx��A|��A���A��\A��A�
=A���A��A�{A�  A��\A�(�A�ffA���A��A�p�A�  A��A�(�A�ffA�Q�A��HA��A�\)A�Q�A��HA���A�
=A�G�A�33A�A��A��A�(�A�ffAȣ�A�33A�{A�  A�=qA�z�A�ffAأ�Aڏ\A�(�A�ffA�Q�A�\A�z�A�ffA���A�A�p�A�\)A�A�A���A��RA���A��\A�(�A�ffB (�B�B=qB�Bz�Bp�B�RB�B��B	��B
�\B33BQ�BG�BffB33BQ�BG�BffB�BG�BffB�B��BB�HB(�BG�B{B33B (�B!G�B"�\B$  B%�B&=qB'�B(��B)��B*ffB+\)B,��B-B.�RB0  B1�B2�\B3�
B4��B6{B733B8z�B9��B:�\B;�B<z�B=��B>�RB?�B@��BA�BC33BD  BEp�BF�\BG�BI�BJ{BK\)BLQ�BMG�BN{BO
=BP(�BQp�BR�\BS�BTz�BUBW
=BX(�BY��BZ�RB[�
B\��B^{B_\)B`Q�BaG�Bb=qBc\)Bdz�Be��Bf�HBg�
Bh��Bj{Bk33Bl(�Bm��Bn�RBo�
Bp��Br{Bs�Bt��Bu�Bw
=Bx(�ByG�BzffB{�B|��B}B
=B�{B��\B�
=B��B�{B��RB�33B��
B�ffB���B��B�{B���B�33B�B�Q�B�
=B��B�=qB���B�\)B��B�z�B�
=B���B�=qB���B�p�B��
B�Q�B���B�\)B�  B�z�B��B���B�ffB�
=B���B�(�B���B�\)B��B�(�B��RB�G�B��
B�ffB��HB�p�B�{B��RB�\)B��B�z�B�
=B���B�(�B��\B���B���B�  B��\B��B�B�ffB��HB�p�B��B�z�B��HB�G�B�B�=qB��RB�\)B��B��\B�
=B��B�{B���B��B��B�  B�z�B�33B��
B�ffB�
=B�p�B��
B�ffB���B�p�B�(�B���B��B�{B��\B��B��B�=qB���B��B�=qB���B�\)B�  B�z�B�
=B���B�(�B���B�G�B�(�B���B�\)B�  B�Q�B��HBÅB�(�B��HBŅB�  B�ffB�
=BǅB�Q�B���BɅB�(�Bʣ�B�33B˙�B�(�B̸RB�G�B�  BΣ�B�33BϮB�(�BиRB�33B��
Bҏ\B�33B�B�ffBԸRB�\)B��B�z�B��B��
B�z�B�
=BٮB�=qBڣ�B�33B�B�ffB��B��
B�ffB�
=B߅B�  B��\B�33B��B�\B�33B�B�(�B��B�p�B�(�B�RB�33B�B�ffB���B�B�Q�B���B�\)B�  B�z�B�\)B��B�\B���B�B�{B�RB�B�{B�z�B���B�B�Q�B��HB�p�B��
B�ffB���B���B�=qB���B�33B��B�=qB��HB���B�(�B��RB�33B�B�Q�B�
=B�C 33C �C �RC
=CQ�C�C{C\)C�C�C33C�C�HC=qC�\C�
C{CffC�RC�CffCC�C=qC�C  CG�C��C�
C	(�C	z�C	��C
=qC
�\C
�
C{CQ�C��C�Cp�C�C  CG�C�RC
=C\)C�\C�
C(�Cz�C�RC��C{C33Cz�C�C�
C��C�C
=C33CG�CG�CG�C\)Cp�C��C�RC�CC��C{C(�C(�C=qCG�Cz�C��C�RC��C�
C�C  C33CQ�Cp�C�C�\C�C�
C  C�C(�C=qCQ�C�C�CC�
C�C(�CQ�CQ�CffC��C��C�
C��C{CG�CffCp�C�C��C��C�C{CG�Cp�C�\C�\C�RC�HC
=C33C33CQ�C�\C��C�RC��C�C�C=qC\)C\)Cz�C�C�
C��C��C{CQ�C\)Cp�C�\CC�HC�C
=C=qCffC�C�\C�C�C
=C(�C=qCffC��C�C��C�C(�CG�CQ�Cz�C�RC�HC�C
=CQ�Cz�Cz�C��C�HC
=C(�C33C\)C��C�CC�HC (�C G�C Q�C z�C �RC �RC �HC ��C!=qC!Q�C!p�C!��C!��C!�
C!��C"�C"Q�C"ffC"z�C"�C"�HC"�
C#  C#=qC#\)C#ffC#�C#�RC#�HC#�C$�C$=qC$G�C$\)C$��C$C$C$�
C%�C%=qC%=qC%ffC%��C%C%C%�HC&(�C&G�C&\)C&z�C&��C&�
C&��C'
=C'33C'ffC'ffC'�\C'��C'�HC(  C((�C(\)C(ffC(�C(C(�HC(��C)33C)\)C)ffC)�C)C)�HC)��C*33C*\)C*ffC*�C*C*�
C*�C+33C+\)C+ffC+��C+��C+��C,  C,=qC,=qC,p�C,�C,�C,�HC-�C-�C-Q�C-�C-��C-�RC.  C.{C.(�C.p�C.��C.�C.�
C/{C/�C/=qC/�C/��C/�RC0  C0(�C033C0z�C0��C0�C0�C1�C1(�C1Q�C1��C1��C1�
C2{C2�C2\)C2�\C2��C2�HC3
=C3{C3\)C3�\C3�\C3�HC4
=C4{C4Q�C4�\C4��C4C5
=C5{C5G�C5�\C5��C5��C6{C6�C6Q�C6��C6C6�HC7�C7\)C7ffC7�RC7�HC8  C8G�C8p�C8��C8�HC9{C933C9�C9��C9��C:{C:33C:ffC:�RC:��C;  C;Q�C;ffC;��C;��C<
=C<\)C<�\C<��C<��C={C==qC=��C=�C=�HC>33C>G�C>z�C>��C>�C?(�C?p�C?�\C?�HC?��C@33C@z�C@�\C@�CA  CA33CA�CA��CA�CB{CBG�CB��CB�CC  CCG�CCQ�CC��CCCD
=CDG�CDp�CDCD�
CE{CEffCEz�CE��CE�CF(�CFffCF�CF�
CF��CGG�CGp�CG�\CG�HCH  CHG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111144111141111411111111111111111111111111114111111111111411114111141111111114111141111111111111111111111111111111114111111111111111411111111111111141111111111111141114111411111111111411111111111111111111111111111141141141141111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�
=A�bA�bA�bA�bA�bA�bA�VA�VA�bA�{A��A��A��A��A��A��A��A��A�{A�bA�oA�oA�{A�  A��HA�A���AиRAд9AЗ�A�n�A�oA�oA�{A�bA� �A�;dA�7LA�oA���A��A��`A�ȴAϏ\A�A�AΗ�A͓uA��A�1A�A���A�ZA�(�AƝ�A�v�AƁA�^5A�%A���AŰ!AŁA�\)A�C�A�=qA���A�VA��A��HA�/A��-A�G�A�1A���A��yA�/A��A���A���A�~�A�VA���A���A��wA�;dA�C�A�oA��mA�bA��jA���A��#A�^5A�{A���A�A��\A��wA��PA�1'A���A�-A��DA��uA���A��;A��TA�
=A��A�5?Au+Ap�Aox�AlbAf��Ad�9A`�A\��AZE�AUO�AT�AS|�AQ�hAO%ANE�AM�TAM�-ALbNAJjAH��AG�AES�AAp�A?�A=VA:A�A7�;A6�jA5��A4~�A2��A/+A.jA-�A,9XA+��A,�A.�/A-��A,-A+�mA(�RA%�A#�A!C�A z�A�A��AoA�TA`BA=qAt�AJA��AĜA��AVA1'A�AhsAVA
=A�^A�DA{A`BA��A��AZAJA��AO�A��A�!A
��A��AE�A�TA�A��AQ�A��A�A�DA1A�TA�^AQ�A\)A%@��@��T@�%@� �@�Z@��@��@�E�@��@��@�;d@���@���@�M�@�  @�(�@�A�@�b@�K�@�@�j@��m@�\)@���@��`@��y@旍@�ff@�-@�J@�7L@�@���@�r�@��@�p�@�j@ߥ�@�-@���@ݡ�@ݙ�@���@ݡ�@�1'@�S�@�$�@�O�@��@�7L@ف@��T@ڟ�@�S�@ە�@�"�@�%@��@�"�@���@�M�@�=q@�=q@ա�@�/@�%@��@��/@Ԭ@ԋD@�r�@�dZ@�@щ7@�hs@�?}@��@��`@��`@�z�@��m@ύP@�33@�M�@́@��@̓u@�b@ˮ@�C�@��y@�ȴ@ʏ\@�M�@�5?@��@ɺ^@ɡ�@�?}@ȣ�@��@�|�@�K�@��@�M�@�$�@���@�p�@�G�@ēu@�I�@�A�@�A�@� �@�1@��;@å�@���@\@�J@���@�`B@�/@�Ĝ@�z�@�Q�@�1@�ƨ@���@�|�@��@���@���@��R@��@���@�`B@�%@���@�I�@�1'@�b@��@���@�l�@�C�@�33@�o@�ȴ@�^5@��#@��`@�A�@��@��
@��w@���@�t�@�ȴ@�n�@�J@��@�&�@��@�1'@��w@�l�@��y@��+@�n�@�@�?}@�/@��@��u@�9X@�1@��w@���@�+@�ȴ@���@�M�@���@�@���@�X@�/@��`@��9@�Q�@�(�@�1@��w@�dZ@���@�M�@�J@��T@��T@��T@���@���@��-@�hs@���@�A�@��F@�|�@�o@��+@�n�@�=q@�{@��@��#@���@�x�@�/@��@�I�@�1@��@�l�@�t�@�"�@���@�~�@�$�@�@��-@��-@���@�x�@�O�@�Z@�  @��;@���@�
=@��\@�J@�`B@���@��@�Z@�A�@��;@���@��@���@��@�|�@�K�@�33@�o@���@��@��y@�~�@�E�@��@��-@���@�Ĝ@��@�z�@�Z@�9X@� �@��@�ƨ@�|�@���@���@���@�`B@�G�@�7L@�/@�V@��/@���@�bN@�  @��
@��F@�K�@�
=@�ȴ@�E�@��#@�X@�V@���@��`@��/@��9@��u@�z�@�r�@�bN@�9X@�1@��
@���@�l�@�C�@��H@�~�@�V@�E�@�{@���@��@��j@��u@�z�@�j@�A�@� �@�  @��F@���@�n�@�n�@�ff@�n�@�E�@�5?@�{@���@��#@�`B@���@��`@��9@�I�@�b@�  @��
@�|�@�\)@�C�@�n�@�M�@�=q@�-@�J@��@��@�%@��@��u@�9X@���@���@�ƨ@��F@��P@�t�@��y@��!@��+@�$�@���@�`B@��@�Ĝ@��@\)@
=@~�+@}�@|��@|�D@|Z@|�@{�m@{S�@z��@z��@y��@x�u@x1'@x  @w�;@w|�@wK�@v��@v�+@vE�@v$�@u�T@t��@t�D@s��@r�H@rM�@q�@q�^@qx�@p��@p��@pQ�@o�;@o�P@o;d@o
=@nȴ@nv�@n{@m�@m?}@l��@lZ@kt�@j�@j�!@j^5@i�@ix�@hr�@h1'@g�w@g|�@g+@f�+@fV@f5?@f{@e�@ep�@d�j@dZ@d1@c�m@cƨ@c�F@c��@c33@b��@b~�@b�@a�@a�^@a&�@`��@_�;@_;d@_+@_+@^�+@]�@]�T@]@]�@\��@\Z@\(�@[��@[�F@[dZ@[o@Z��@Z~�@Z-@Y�#@Y�^@Y�7@XĜ@XbN@XA�@W�;@W��@Wl�@W�@V�y@V{@U�@T��@T�D@S�m@S��@SS�@S33@R�!@RM�@Q�#@QG�@P��@PĜ@P�9@P��@P�@PQ�@O�;@N�@NV@NE�@N$�@M�@M�@L�/@L�@L��@L(�@K�m@K�
@K��@KS�@K"�@J�@J�H@J��@J��@J��@J^5@I��@I��@Ihs@H��@G|�@F�@F$�@E��@Ep�@E?}@EV@D�@D�/@D�@Dz�@D1@CdZ@B�H@B�\@B=q@A��@A�#@A�#@A�#@A�#@A�#@A�#@A�^@A��@A�7@Ahs@AX@AG�@@A�@?K�@?;d@?
=@>�@>�R@>v�@>ff@>V@>{@=@=��@=O�@<��@<��@<�D@<Z@<(�@;��@;�m@;ƨ@;�F@;��@;��@;�@:�@:J@9�7@9X@9%@8bN@7�;@7l�@7;d@6�y@6V@5��@5�-@5�h@5�h@5�h@5p�@5?}@5V@4��@4�D@4�D@4Z@3�m@3�F@3dZ@3"�@2��@1��@1x�@17L@1%@0��@0�u@01'@/�;@/\)@/
=@.�y@.ȴ@.��@.v�@.V@.$�@-�h@-O�@-?}@-�@,��@,��@,�@+�@*^5@)��@)7L@)�@(��@(�u@(�@(�@(bN@( �@'�;@'�@';d@&��@&�@&ȴ@&��@&v�@&@%�h@%p�@%`B@%?}@%�@%V@%V@%V@$��@$��@$�j@$�j@$��@$�j@$�j@$��@$��@$9X@#��@#�
@#��@#t�@"�!@!��@!hs@!7L@ Ĝ@ r�@�w@l�@\)@K�@+@+@
=@�@�+@��@`B@O�@?}@/@��@��@�D@Z@I�@�@ƨ@dZ@"�@�H@^5@��@x�@7L@%@��@�`@Ĝ@�9@��@��@r�@A�@1'@b@  @�;@�;@��@�P@\)@;d@+@�@�R@ff@��@�-@�@/@�@�D@j@j@9X@9X@(�@�@1@1@�m@��@t�@S�@S�@S�@C�@33@@��@~�@M�@�@��@�@�#@��@��@X@�@�@%@��@��@��@Ĝ@  @;d@�@ȴ@E�@�@�@��@`B@�/@�@�
@ƨ@�
@�F@dZ@"�@"�@o@@
�H@
��@
�\@
~�@
=q@	��@	7L@	&�@	�@	%@��@�@bN@A�@A�@ �@b@�@�;@��@�A��A��A��A�1A�1A�1A�
=A�
=A�oA�bA�oA�bA�oA�oA�{A�oA�bA�VA�bA�VA�VA�bA�VA�VA�JA�VA�bA�VA�bA�bA�oA�{A�oA�bA�bA�VA�VA�JA�JA�
=A�
=A�VA�bA�oA�oA��A��A��A��A��A��A��A�{A��A��A�{A�{A�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�{A�bA�VA�bA�{A�{A�{A�bA�bA�bA�VA�oA�oA�oA�bA�bA�bA�bA�bA�bA�oA�{A�{A�{A��A��A�oA�{A�{A��A�oA�VA�bA�{A�oA�{A�{A�VA���A���A�  A�  A�  A��A��A��A��mA��;A��/A�ĜA�ƨA�ƨA�ƨA�ƨA�ĜA�ĜA�A���AоwA�AмjAмjAмjAоwAмjAмjAиRAиRAк^Aк^Aк^Aк^Aк^AиRAк^AиRAд9Aа!AЬAЩ�AХ�AЛ�AГuAГuAГuAЏ\AЍPAЍPAЉ7AЅA�r�A�hsA�ZA�G�A�+A�oA�VA�JA�
=A�
=A�VA�bA�oA�{A�{A��A��A��A��A��A��A��A��A�{A�oA�bA�bA�VA�VA�VA�bA�VA�VA�bA�bA��A�(�A�1'A�33A�33A�5?A�=qA�A�A�E�A�E�A�E�A�C�A�C�A�A�A�;dA�7LA�/A�&�A� �A��A��A�oA�VA�JA�1A�A���A���A���A���A���A��A���A���A���A���A��A��A��A��yA��mA��`A��`A��`A��TA��HA��/A��A���A���A���A�ĜAϺ^Aϲ-Aϩ�Aϟ�AϑhAω7AυAυAσA�z�A�l�A�Q�A�K�A�E�A�9XA�-A�bA���AξwAΟ�AΙ�AΑhA΅A�r�A�ZA��A��TA;wA͏\A�hsA�;dA��A�ƨA̕�A�p�A�=qAˇ+A�A�A�%A��;Aʣ�A�ffA��A��Aɡ�A�z�A�r�A�n�A�jA�VA�1Aȕ�A�1'A��A��A��;A���AǸRAǣ�AǍPA�t�A�`BA�ZA�Q�A�K�A�I�A�E�A�E�A�E�A�E�A�?}A��A���A��#A���Aư!Aƥ�Aƙ�AƉ7A�x�A�x�A�v�A�t�A�r�A�t�A�|�A�|�A�|�A�v�A�|�AƃAƇ+AƇ+AƁA�~�A�z�A�p�A�bNA�Q�A�Q�A�I�A�;dA�(�A�oA�  A���A��A��A���A��A��TA���A�Aź^AżjAŸRAŲ-AŴ9AŴ9AŶFAŮAř�Aŏ\AŅA�|�A�~�A�|�A�t�A�n�A�jA�ffA�ffA�\)A�K�A�A�A�?}A�A�A�C�A�G�A�G�A�E�A�C�A�A�A�M�A�Q�A�C�A�1'A��A�1A���A��A��mA��mAĺ^A�t�A�S�A�K�A�+A��A�  A��A��`A��#A���AìA�(�A��A°!A\A�ffA�7LA�JA���A��A���A�v�A�r�A�I�A�?}A�7LA�bA�
=A���A��A���A���A��uA��hA��DA��A��A�n�A�M�A��A�JA��TA��jA�~�A�9XA�ƨA�dZA�&�A�JA���A��
A���A��A��PA�n�A�`BA�C�A�&�A��A�\)A��^A��uA��A�ZA��mA���A��9A��uA�ffA�1'A��A�  A��A��HA��FA�z�A�p�A�ffA�l�A�`BA�VA�E�A�$�A��A��A��FA�v�A�E�A�VA��;A��A�x�A�M�A��A���A��A���A�\)A�$�A��A��A�l�A�&�A�1A��
A��PA�E�A��A���A��TA�ȴA��9A���A��+A�p�A�E�A��A��A��RA�z�A�A�A��A���A��A�z�A��A�t�A�p�A�l�A�dZA�=qA��A���A���A��-A���A�z�A�dZA�?}A��A�A��A��A��yA��A��yA��TA��;A��#A��
A���A�ȴA�ĜA�A�ƨA�ĜA�A��jA��^A��^A��RA��-A��A���A��PA�Q�A�I�A�7LA�"�A�{A���A��yA���A���A�x�A�5?A��HA���A�E�A�ȴA���A��A�9XA��A�oA�VA�VA�
=A�
=A�%A�  A���A��mA�G�A�A��mA���A��uA�n�A�M�A�?}A�;dA�;dA�9XA�33A�33A�33A�+A��A�VA�%A�A���A��A���A��A��PA�|�A�jA�ZA�/A��;A���A��7A�~�A�dZA�`BA�VA�G�A�A�A�=qA�5?A�1'A�33A� �A��A�%A��A��HA���A��RA���A�v�A�E�A�"�A��TA��A�=qA��A�A��yA��-A�;dA��A��jA��A���A�I�A�VA���A�%A���A��A��yA��HA���A��wA��RA��!A���A���A���A�r�A�S�A�?}A�+A���A���A��^A���A�x�A�^5A�I�A�5?A�-A�"�A�VA�A��A��mA��/A��
A���A�A��^A��-A���A���A�x�A�&�A�ĜA�v�A��A��mA��
A���A��9A���A���A��DA�M�A��A���A��+A�t�A�ffA�`BA�`BA�VA�Q�A�O�A�I�A�E�A�G�A�C�A�?}A�?}A�=qA�7LA�33A�-A��A�JA��A��HA��;A���A���A���A���A�ƨA�ȴA�ƨA�A��-A�~�A�dZA�O�A�;dA�+A� �A�VA���A��A���A��jA���A��7A�l�A�bNA�I�A���A�^5A��
A�-A�jA��#A��PA�ZA�bA��A���A�`BA�-A��A��A���A�n�A�G�A�9XA�9XA�$�A��A��A�{A��TA��9A�dZA���A���A��7A��A��A�p�A�bNA�O�A�G�A�;dA�  A��9A�~�A�K�A�oA���A��#A���A��^A���A��A�jA�9XA�  A���A���A�XA��A��TA��A�33A��A�
=A��#A��PA�|�A�jA�^5A�\)A�M�A�E�A�&�A�%A��/A���A�t�A�hsA�XA�E�A�33A�&�A�
=A��A��mA���A�A��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                       A���A�
=A�bA�bA�bA�bA�bA�bA�VA�VA�bA�{A��A��A��A��A��A��A��A��A�{A�bA�oA�oA�{A�  A��HA�A���AиRAд9AЗ�A�n�A�oA�oA�{A�bA� �A�;dA�7LA�oA���A��A��`A�ȴAϏ\A�A�AΗ�A͓uA��A�1A�A���A�ZA�(�AƝ�A�v�AƁA�^5A�%A���AŰ!AŁA�\)A�C�A�=qA���A�VA��A��HA�/A��-A�G�A�1A���A��yA�/A��A���A���A�~�A�VA���A���A��wA�;dA�C�A�oA��mA�bA��jA���A��#A�^5A�{A���A�A��\A��wA��PA�1'A���A�-A��DA��uA���A��;A��TA�
=A��A�5?Au+Ap�Aox�AlbAf��Ad�9A`�A\��AZE�AUO�AT�AS|�AQ�hAO%ANE�AM�TAM�-ALbNAJjAH��AG�AES�AAp�A?�A=VA:A�A7�;A6�jA5��A4~�A2��A/+A.jA-�A,9XA+��A,�A.�/A-��A,-A+�mA(�RA%�A#�A!C�A z�A�A��AoA�TA`BA=qAt�AJA��AĜA��AVA1'A�AhsAVA
=A�^A�DA{A`BA��A��AZAJA��AO�A��A�!A
��A��AE�A�TA�A��AQ�A��A�A�DA1A�TA�^AQ�A\)A%@��@��T@�%@� �@�Z@��@��@�E�@��@��@�;d@���@���@�M�@�  @�(�@�A�@�b@�K�@�@�j@��m@�\)@���@��`@��y@旍@�ff@�-@�J@�7L@�@���@�r�@��@�p�@�j@ߥ�@�-@���@ݡ�@ݙ�@���@ݡ�@�1'@�S�@�$�@�O�@��@�7L@ف@��T@ڟ�@�S�@ە�@�"�@�%@��@�"�@���@�M�@�=q@�=q@ա�@�/@�%@��@��/@Ԭ@ԋD@�r�@�dZ@�@щ7@�hs@�?}@��@��`@��`@�z�@��m@ύP@�33@�M�@́@��@̓u@�b@ˮ@�C�@��y@�ȴ@ʏ\@�M�@�5?@��@ɺ^@ɡ�@�?}@ȣ�@��@�|�@�K�@��@�M�@�$�@���@�p�@�G�@ēu@�I�@�A�@�A�@� �@�1@��;@å�@���@\@�J@���@�`B@�/@�Ĝ@�z�@�Q�@�1@�ƨ@���@�|�@��@���@���@��R@��@���@�`B@�%@���@�I�@�1'@�b@��@���@�l�@�C�@�33@�o@�ȴ@�^5@��#@��`@�A�@��@��
@��w@���@�t�@�ȴ@�n�@�J@��@�&�@��@�1'@��w@�l�@��y@��+@�n�@�@�?}@�/@��@��u@�9X@�1@��w@���@�+@�ȴ@���@�M�@���@�@���@�X@�/@��`@��9@�Q�@�(�@�1@��w@�dZ@���@�M�@�J@��T@��T@��T@���@���@��-@�hs@���@�A�@��F@�|�@�o@��+@�n�@�=q@�{@��@��#@���@�x�@�/@��@�I�@�1@��@�l�@�t�@�"�@���@�~�@�$�@�@��-@��-@���@�x�@�O�@�Z@�  @��;@���@�
=@��\@�J@�`B@���@��@�Z@�A�@��;@���@��@���@��@�|�@�K�@�33@�o@���@��@��y@�~�@�E�@��@��-@���@�Ĝ@��@�z�@�Z@�9X@� �@��@�ƨ@�|�@���@���@���@�`B@�G�@�7L@�/@�V@��/@���@�bN@�  @��
@��F@�K�@�
=@�ȴ@�E�@��#@�X@�V@���@��`@��/@��9@��u@�z�@�r�@�bN@�9X@�1@��
@���@�l�@�C�@��H@�~�@�V@�E�@�{@���@��@��j@��u@�z�@�j@�A�@� �@�  @��F@���@�n�@�n�@�ff@�n�@�E�@�5?@�{@���@��#@�`B@���@��`@��9@�I�@�b@�  @��
@�|�@�\)@�C�@�n�@�M�@�=q@�-@�J@��@��@�%@��@��u@�9X@���@���@�ƨ@��F@��P@�t�@��y@��!@��+@�$�@���@�`B@��@�Ĝ@��@\)@
=@~�+@}�@|��@|�D@|Z@|�@{�m@{S�@z��@z��@y��@x�u@x1'@x  @w�;@w|�@wK�@v��@v�+@vE�@v$�@u�T@t��@t�D@s��@r�H@rM�@q�@q�^@qx�@p��@p��@pQ�@o�;@o�P@o;d@o
=@nȴ@nv�@n{@m�@m?}@l��@lZ@kt�@j�@j�!@j^5@i�@ix�@hr�@h1'@g�w@g|�@g+@f�+@fV@f5?@f{@e�@ep�@d�j@dZ@d1@c�m@cƨ@c�F@c��@c33@b��@b~�@b�@a�@a�^@a&�@`��@_�;@_;d@_+@_+@^�+@]�@]�T@]@]�@\��@\Z@\(�@[��@[�F@[dZ@[o@Z��@Z~�@Z-@Y�#@Y�^@Y�7@XĜ@XbN@XA�@W�;@W��@Wl�@W�@V�y@V{@U�@T��@T�D@S�m@S��@SS�@S33@R�!@RM�@Q�#@QG�@P��@PĜ@P�9@P��@P�@PQ�@O�;@N�@NV@NE�@N$�@M�@M�@L�/@L�@L��@L(�@K�m@K�
@K��@KS�@K"�@J�@J�H@J��@J��@J��@J^5@I��@I��@Ihs@H��@G|�@F�@F$�@E��@Ep�@E?}@EV@D�@D�/@D�@Dz�@D1@CdZ@B�H@B�\@B=q@A��@A�#@A�#@A�#@A�#@A�#@A�#@A�^@A��@A�7@Ahs@AX@AG�@@A�@?K�@?;d@?
=@>�@>�R@>v�@>ff@>V@>{@=@=��@=O�@<��@<��@<�D@<Z@<(�@;��@;�m@;ƨ@;�F@;��@;��@;�@:�@:J@9�7@9X@9%@8bN@7�;@7l�@7;d@6�y@6V@5��@5�-@5�h@5�h@5�h@5p�@5?}@5V@4��@4�D@4�D@4Z@3�m@3�F@3dZ@3"�@2��@1��@1x�@17L@1%@0��@0�u@01'@/�;@/\)@/
=@.�y@.ȴ@.��@.v�@.V@.$�@-�h@-O�@-?}@-�@,��@,��@,�@+�@*^5@)��@)7L@)�@(��@(�u@(�@(�@(bN@( �@'�;@'�@';d@&��@&�@&ȴ@&��@&v�@&@%�h@%p�@%`B@%?}@%�@%V@%V@%V@$��@$��@$�j@$�j@$��@$�j@$�j@$��@$��@$9X@#��@#�
@#��@#t�@"�!@!��@!hs@!7L@ Ĝ@ r�@�w@l�@\)@K�@+@+@
=@�@�+@��@`B@O�@?}@/@��@��@�D@Z@I�@�@ƨ@dZ@"�@�H@^5@��@x�@7L@%@��@�`@Ĝ@�9@��@��@r�@A�@1'@b@  @�;@�;@��@�P@\)@;d@+@�@�R@ff@��@�-@�@/@�@�D@j@j@9X@9X@(�@�@1@1@�m@��@t�@S�@S�@S�@C�@33@@��@~�@M�@�@��@�@�#@��@��@X@�@�@%@��@��@��@Ĝ@  @;d@�@ȴ@E�@�@�@��@`B@�/@�@�
@ƨ@�
@�F@dZ@"�@"�@o@@
�H@
��@
�\@
~�@
=q@	��@	7L@	&�@	�@	%@��@�@bN@A�@A�@ �@b@�@�;@��@�A��A��A��A�1A�1A�1A�
=A�
=A�oA�bA�oA�bA�oA�oA�{A�oA�bA�VA�bA�VA�VA�bA�VA�VA�JA�VA�bA�VA�bA�bA�oA�{A�oA�bA�bA�VA�VA�JA�JA�
=A�
=A�VA�bA�oA�oA��A��A��A��A��A��A��A�{A��A��A�{A�{A�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�{A�bA�VA�bA�{A�{A�{A�bA�bA�bA�VA�oA�oA�oA�bA�bA�bA�bA�bA�bA�oA�{A�{A�{A��A��A�oA�{A�{A��A�oA�VA�bA�{A�oA�{A�{A�VA���A���A�  A�  A�  A��A��A��A��mA��;A��/A�ĜA�ƨA�ƨA�ƨA�ƨA�ĜA�ĜA�A���AоwA�AмjAмjAмjAоwAмjAмjAиRAиRAк^Aк^Aк^Aк^Aк^AиRAк^AиRAд9Aа!AЬAЩ�AХ�AЛ�AГuAГuAГuAЏ\AЍPAЍPAЉ7AЅA�r�A�hsA�ZA�G�A�+A�oA�VA�JA�
=A�
=A�VA�bA�oA�{A�{A��A��A��A��A��A��A��A��A�{A�oA�bA�bA�VA�VA�VA�bA�VA�VA�bA�bA��A�(�A�1'A�33A�33A�5?A�=qA�A�A�E�A�E�A�E�A�C�A�C�A�A�A�;dA�7LA�/A�&�A� �A��A��A�oA�VA�JA�1A�A���A���A���A���A���A��A���A���A���A���A��A��A��A��yA��mA��`A��`A��`A��TA��HA��/A��A���A���A���A�ĜAϺ^Aϲ-Aϩ�Aϟ�AϑhAω7AυAυAσA�z�A�l�A�Q�A�K�A�E�A�9XA�-A�bA���AξwAΟ�AΙ�AΑhA΅A�r�A�ZA��A��TA;wA͏\A�hsA�;dA��A�ƨA̕�A�p�A�=qAˇ+A�A�A�%A��;Aʣ�A�ffA��A��Aɡ�A�z�A�r�A�n�A�jA�VA�1Aȕ�A�1'A��A��A��;A���AǸRAǣ�AǍPA�t�A�`BA�ZA�Q�A�K�A�I�A�E�A�E�A�E�A�E�A�?}A��A���A��#A���Aư!Aƥ�Aƙ�AƉ7A�x�A�x�A�v�A�t�A�r�A�t�A�|�A�|�A�|�A�v�A�|�AƃAƇ+AƇ+AƁA�~�A�z�A�p�A�bNA�Q�A�Q�A�I�A�;dA�(�A�oA�  A���A��A��A���A��A��TA���A�Aź^AżjAŸRAŲ-AŴ9AŴ9AŶFAŮAř�Aŏ\AŅA�|�A�~�A�|�A�t�A�n�A�jA�ffA�ffA�\)A�K�A�A�A�?}A�A�A�C�A�G�A�G�A�E�A�C�A�A�A�M�A�Q�A�C�A�1'A��A�1A���A��A��mA��mAĺ^A�t�A�S�A�K�A�+A��A�  A��A��`A��#A���AìA�(�A��A°!A\A�ffA�7LA�JA���A��A���A�v�A�r�A�I�A�?}A�7LA�bA�
=A���A��A���A���A��uA��hA��DA��A��A�n�A�M�A��A�JA��TA��jA�~�A�9XA�ƨA�dZA�&�A�JA���A��
A���A��A��PA�n�A�`BA�C�A�&�A��A�\)A��^A��uA��A�ZA��mA���A��9A��uA�ffA�1'A��A�  A��A��HA��FA�z�A�p�A�ffA�l�A�`BA�VA�E�A�$�A��A��A��FA�v�A�E�A�VA��;A��A�x�A�M�A��A���A��A���A�\)A�$�A��A��A�l�A�&�A�1A��
A��PA�E�A��A���A��TA�ȴA��9A���A��+A�p�A�E�A��A��A��RA�z�A�A�A��A���A��A�z�A��A�t�A�p�A�l�A�dZA�=qA��A���A���A��-A���A�z�A�dZA�?}A��A�A��A��A��yA��A��yA��TA��;A��#A��
A���A�ȴA�ĜA�A�ƨA�ĜA�A��jA��^A��^A��RA��-A��A���A��PA�Q�A�I�A�7LA�"�A�{A���A��yA���A���A�x�A�5?A��HA���A�E�A�ȴA���A��A�9XA��A�oA�VA�VA�
=A�
=A�%A�  A���A��mA�G�A�A��mA���A��uA�n�A�M�A�?}A�;dA�;dA�9XA�33A�33A�33A�+A��A�VA�%A�A���A��A���A��A��PA�|�A�jA�ZA�/A��;A���A��7A�~�A�dZA�`BA�VA�G�A�A�A�=qA�5?A�1'A�33A� �A��A�%A��A��HA���A��RA���A�v�A�E�A�"�A��TA��A�=qA��A�A��yA��-A�;dA��A��jA��A���A�I�A�VA���A�%A���A��A��yA��HA���A��wA��RA��!A���A���A���A�r�A�S�A�?}A�+A���A���A��^A���A�x�A�^5A�I�A�5?A�-A�"�A�VA�A��A��mA��/A��
A���A�A��^A��-A���A���A�x�A�&�A�ĜA�v�A��A��mA��
A���A��9A���A���A��DA�M�A��A���A��+A�t�A�ffA�`BA�`BA�VA�Q�A�O�A�I�A�E�A�G�A�C�A�?}A�?}A�=qA�7LA�33A�-A��A�JA��A��HA��;A���A���A���A���A�ƨA�ȴA�ƨA�A��-A�~�A�dZA�O�A�;dA�+A� �A�VA���A��A���A��jA���A��7A�l�A�bNA�I�A���A�^5A��
A�-A�jA��#A��PA�ZA�bA��A���A�`BA�-A��A��A���A�n�A�G�A�9XA�9XA�$�A��A��A�{A��TA��9A�dZA���A���A��7A��A��A�p�A�bNA�O�A�G�A�;dA�  A��9A�~�A�K�A�oA���A��#A���A��^A���A��A�jA�9XA�  A���A���A�XA��A��TA��A�33A��A�
=A��#A��PA�|�A�jA�^5A�\)A�M�A�E�A�&�A�%A��/A���A�t�A�hsA�XA�E�A�33A�&�A�
=A��A��mA���A�A��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
gB
f�B
ffB
f�B
f�B
f�B
gB
f�B
gB
gB
gB
f�B
gB
gB
gB
gB
f�B
gB
f�B
g�B
hsB
h�B
h�B
hsB
h�B
kQB
p;B
t�B
w�B
�_B
��B
��B
��B
�B
��B
�-B
�hB
��B
B
�mB
�3B
B
�3B
�EB
�)B
�6B
�B
�WB
�B_B(�BC�B3hB%�B"�B �B(�B<�BNBR�B[�B|�B��B�xB��B�OB�dB�UB��B_B$�B'�B)*B=B@�BD3BbNBo Bf�B\�BS�BN�BJ�BD�B+�B$B�B�B�B�zB��B�{B�;Bm�Bn�BlWBaBI�B!B�B
�>B
�fB
��B
�XB
��B
��B
�~B
r|B
G�B
3hB
'�B	�fB	ȴB	�B	��B	��B	��B	xlB	]�B	U2B	A�B	9$B	4�B	1�B	'B	#B	�B	~B	�B	B	�B	1B		lB��B�+B�B�B�B��B�B��B��B�NB�;B�,B�)B�5B�(B	B�B	K^B	C�B	HKB	>�B	2-B	*�B	�B	B	�B	�B	&�B	%B	%�B	&�B	&�B	/�B	0�B	0�B	1'B	1�B	1�B	2�B	3�B	3�B	0!B	/�B	.}B	/�B	49B	6FB	0�B	3hB	6zB	<jB	;dB	:�B	7�B	5?B	'�B	$tB	!�B	!�B	%�B	8�B	G�B	K�B	O�B	N�B	K�B	IB	F?B	<6B	9�B	.B	+kB	+�B	(�B	(�B	-wB	/OB	.�B	7B	5tB	8RB	?B	GB	I�B	C�B	G�B	I�B	L0B	N�B	N<B	I�B	HKB	GB	G�B	J�B	GB	IB	K�B	M�B	NpB	P�B	W?B	`BB	aB	_pB	aB	[�B	`vB	dZB	e`B	e�B	f2B	g�B	m]B	m�B	l�B	qB	q�B	sMB	u%B	x�B	~]B	�MB	�~B	��B	��B	��B	��B	��B	�nB	��B	�XB	��B	�OB	�[B	��B	�zB	�XB	��B	��B	�^B	��B	�*B	�dB	�jB	�}B	��B	�'B	��B	��B	B	B	ĜB	ȀB	�XB	��B	уB	ѷB	��B	�gB	��B	�
B	�B	�B	�B	�QB	�B	چB	�]B	�/B	�B	ߤB	�B	�|B	�B	�B	�TB	�TB	�B	�B	��B	�B	�B	�8B	�B	�
B	�B	�B	�DB	�B	�B	�B	�QB	��B	�)B	�B	�B	� B	�5B	�5B	�B	�B	�B	�;B	�vB	�vB	�B	�B	�MB	�B	�B	�B	�B	�B	�vB	�B	�|B	�B	�B	��B	�B	�|B	��B	�MB	�%B	�`B	�2B	�fB	�DB	�rB	��B	�DB	��B	��B	�VB	��B	�"B	��B	��B	�(B
 iB
 �B
  B
;B
B
�B
AB
GB
uB
�B
�B
{B
�B
SB
B
�B
B
�B
�B
%B
�B
�B
+B
�B
1B
	�B

	B

�B

	B

	B

	B

	B

	B
	�B

=B

�B
�B
�B
�B
�B
�B
�B
(B
.B
�B
bB
.B
�B
bB
 B
bB
�B
�B
 B
�B
�B
B
�B
B
�B
B
�B
B
B
�B
�B
B
B
uB
�B
�B
FB
�B
B
�B
�B
$B
YB
+B
�B
�B
eB
eB
�B
1B
�B
�B
=B
IB
�B
�B
�B
B
B
IB
~B
�B
B
�B
�B
�B
~B
B
�B
�B
�B
B
B
B
�B
�B
B
B
�B
�B
B
�B
VB
�B
!B
�B
�B
 'B
 �B
 �B
 �B
 �B
 �B
 �B
!-B
 �B
!-B
!-B
!�B
!�B
"4B
"4B
"4B
"�B
#B
#:B
"�B
#:B
#�B
%FB
%zB
%zB
%�B
%zB
%�B
%�B
%�B
&�B
)�B
(�B
(�B
)*B
(XB
)*B
)*B
(�B
)*B
)_B
+B
+6B
+6B
+6B
,=B
,qB
,=B
,qB
-wB
,�B
-B
/B
.B
.�B
.}B
.B
0!B
/�B
/�B
/�B
0�B
0UB
0�B
0�B
0�B
1�B
1�B
1'B
1�B
2�B
2aB
33B
33B
2�B
2�B
3hB
4�B
4�B
4�B
5?B
6FB
5tB
5tB
5tB
5�B
5tB
6zB
6�B
7LB
8�B
9XB
8�B
9�B
8�B
9�B
9XB
:�B
9�B
:^B
9�B
:^B
;�B
<B
=<B
=<B
>BB
=�B
=�B
>wB
>�B
>wB
>wB
?HB
@B
@OB
@OB
@�B
@�B
A�B
AUB
A�B
B'B
A�B
C-B
C�B
C�B
D�B
D�B
C�B
EmB
EB
EmB
FtB
FtB
FtB
F�B
GB
GB
F�B
GEB
H�B
H�B
H�B
H�B
H�B
H�B
HB
HKB
HKB
HKB
H�B
H�B
HKB
IRB
I�B
I�B
I�B
JXB
J�B
LdB
K�B
K�B
K�B
K�B
M6B
M�B
M6B
MB
L�B
M�B
M�B
MjB
NB
N�B
N�B
NpB
N�B
N�B
N�B
OB
PB
OvB
O�B
O�B
P�B
R�B
RTB
Q�B
RTB
R�B
S&B
S[B
S�B
TaB
TaB
T,B
U�B
UgB
U�B
U�B
U�B
U�B
U�B
VB
V�B
VB
U�B
UgB
W
B
V�B
VmB
W?B
WsB
W�B
W
B
V�B
WsB
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
YKB
ZB
ZB
Z�B
[�B
[�B
\�B
\)B
[�B
\]B
\�B
\�B
\�B
]�B
\�B
^B
^�B
_B
_B
`B
`vB
`BB
_�B
_�B
`BB
`BB
_�B
_�B
_�B
`B
`vB
`BB
`B
a�B
a|B
a�B
bNB
b�B
b�B
bNB
bB
bNB
b�B
c B
cTB
c�B
c�B
c�B
c�B
d�B
d&B
c�B
c�B
dZB
d�B
d�B
d�B
d&B
d�B
e�B
ffB
ffB
ffB
f�B
g�B
g�B
g8B
h>B
hsB
h>B
h�B
hsB
hsB
hsB
hsB
iB
h�B
iDB
i�B
h�B
i�B
i�B
iDB
i�B
iDB
i�B
jKB
jB
j�B
j�B
k�B
lWB
m]B
l�B
m�B
m)B
m�B
l�B
l�B
m]B
l�B
m)B
l�B
lWB
l�B
m�B
m�B
m]B
n/B
n�B
m�B
m]B
n/B
n�B
o5B
o5B
o�B
o5B
o�B
oiB
o�B
o�B
poB
o�B
pB
o�B
pB
oiB
poB
p�B
qAB
qvB
q�B
rB
rGB
r|B
r�B
r�B
r�B
r�B
sB
sB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
tB
tTB
t�B
u%B
u�B
v+B
v`B
u�B
v+B
u�B
v+B
v�B
w2B
w�B
xB
wfB
w�B
x8B
x�B
x�B
y	B
y	B
y>B
yrB
y�B
yrB
zB
zB
zB
zB
zB
zDB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{JB
{B
{B
|B
{�B
|PB
|�B
|�B
}"B
}"B
}"B
}�B
~(B
~]B
~�B
~�B
cB
� B
� B
�4B
�B
��B
�4B
�4B
�4B
��B
�iB
��B
�B
�;B
�;B
�;B
�;B
�;B
�;B
��B
��B
�B
�B
�uB
�uB
�uB
�uB
��B
��B
�GB
�GB
�B
�GB
�GB
�GB
�B
�GB
�MB
�B
�SB
�B
�%B
�%B
��B
�%B
��B
�_B
�1B
�fB
�1B
��B
�1B
�B
�B
��B
��B
�B
�B
��B
�lB
�7B
�	B
�=B
�B
��B
�B
�DB
�B
�B
��B
�~B
��B
��B
��B
��B
��B
�B
��B
hsB
hsB
jB
e`B
h
B
gB
f�B
f2B
e`B
ffB
e�B
f�B
e�B
e�B
e�B
e�B
f2B
ffB
f�B
gB
gB
gmB
g�B
gmB
h
B
g�B
f�B
f�B
f�B
ffB
f�B
e,B
e�B
f�B
f2B
f�B
gB
g�B
h>B
h
B
h
B
gmB
g8B
ffB
ffB
f2B
f2B
ffB
ffB
e�B
ffB
f�B
gB
f�B
gB
g8B
g�B
g�B
gmB
h
B
gmB
g8B
gB
f�B
gB
f�B
f�B
gB
gB
f�B
e�B
f2B
f2B
f2B
f2B
f�B
ffB
gB
gmB
gmB
g�B
h
B
g�B
g�B
g�B
g8B
gB
f�B
gB
f�B
f�B
e�B
f2B
f2B
f2B
f2B
f2B
f�B
g8B
gmB
g�B
g�B
h�B
h�B
h>B
g�B
f�B
g8B
g8B
gB
gB
g�B
h�B
hsB
jB
iyB
h�B
h�B
hsB
h>B
iyB
hsB
h>B
h�B
g�B
hsB
h�B
iDB
i�B
iDB
i�B
i�B
i�B
h�B
iB
h>B
g�B
g�B
g8B
hsB
g8B
h�B
g8B
i�B
iyB
iDB
h>B
h�B
iB
iB
j�B
k�B
j�B
jB
jKB
j�B
o B
m�B
m�B
p�B
poB
poB
v�B
sB
uZB
s�B
s�B
tTB
t�B
u�B
v�B
w2B
wfB
zDB
x8B
y	B
xlB
x�B
}�B
��B
��B
��B
�DB
�xB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�B
�LB
��B
��B
��B
��B
��B
�B
�!B
��B
��B
��B
�-B
�B
�IB
�B
��B
�CB
�IB
��B
�}B
�B
�}B
��B
�OB
��B
�!B
�UB
��B
��B
��B
��B
��B
��B
�hB
�3B
�3B
��B
��B
�B
�B
�B
�tB
�?B
��B
��B
� B
��B
��B
��B
��B
�'B
��B
��B
�3B
ĜB
�gB
ŢB
�gB
��B
�B
ŢB
�9B
ĜB
��B
��B
�[B
��B
�-B
�aB
�-B
��B
�aB
B
��B
��B
��B
��B
��B
ĜB
�mB
��B
�tB
�EB
�EB
ƨB
�B
�B
��B
ȴB
ɆB
�#B
�#B
ʌB
˒B
˒B
�dB
̘B
͟B
��B
��B
�B
�6B
̘B
�6B
�pB
��B
�0B
�dB
��B
�B
�TB
�TB
��B
�B
ٴB
��B
��B
ݘB
�BB
�8B
��B
�
B
�B
��B
�B
�oB
�B
�B
�B
��B�B�B�BeB#�BB%FB!B0!B:*B>B>BB>�BA�BB'BVmB?HB;dB:*B4B2�B1[B.�B-CB,qB%�B$B%�B$�B#�B#nB!�B!�B �B"4B(�B"�B"�B \B�B�B�B#B#�B&�B&�B&�B&B&�B'RB/�B1�B5?B8�B=B>BB@OBC�BE�BH�BL�BOBBQ�BO�BPBRTBP}BU2BR�BQ�BQ�BS&BT�BX�BX�BV�BT�B^5BiBm�Bv+BzxBz�B~]B�MB�YB�B��B�7B��B��B�7B�rB�rB�=B��B�DB��B�\B�B��B�YB��B��B��B�qB��B��B��B��B�LB��B�B�B��B��B�B�)B�gB��B�jB��B�wBB� B�B��B�XB��B��B�.B�B(B�B\B�B�BFB#B%zB�B'�B!-B �B-�B&�B(XB(XB+�B+B$�B$�B%zB#:B"�B&�B-CB,B+�B2aB1�B3�B=BD�BE9BB�B?�B=�BA�B@OB@BC�B<jB=B:�B=BA�BX�BL�BUgBY�BlWBn�BcTBe,Bg�BncBq�BqBp�Bo�BncBtTBr�Bk�BjBe�BgmBf�BcTBcTBd�B]�Bc�BjBjKBhsBg8Bk�BjBiyBh�BdZBe�Bh�BiBh>Bd�Be�B`BBcTB_�BdZBc�Bc B]�BZ�BY�B\]B[WBX�BVmBX�BY�BXyBV�BW?BTaBW�BX�BYBTaBR�BPBT�BQBP�BPHBU2BQ�BT,BT�BQNBP�BPBPBTaBQBM6BNBM�BM�BI�BL�BMjBM�BM�BLdBK^BL�BM6BL�BK^BJXBJ�BL0BK)BJ#BIBI�BI�BH�BGBO�BH�BGEBC�BE9BD�BEmBC�BA�BA�BF�BE9B?B=�BB[BP}B[WB1�B+�B(�B(�B'�B(�B'�B)_B&�B)_B+kBT,B,B,�B+�B/B)�B%�B%B#�B"hB!-B#B!�B \B �B"�B!�BB�B�B�B�B �B�B�B�B�B�B�BYB�B{B:B�BB@B B\B�B.B�B�B\BPB�B�BPB	B+B+BYB��B{B�B�xB�B�cB�/B��B�2B��B�]B�B�yB�[B�B�9B�^B��B�aB��B�B�OB��B��B��B��B��B�6B��B��B�0B��B��B��B��B�B��B��B�CB��B��B��B�1B��B�$B��B�{B��B��B��B�:B��B��B��B��B��B�:B��B��B��BzDB{�B{Bv�ByrBw�B.BzBs�BpoBn/Bm�Bm]Bk�Bm)Bm�BlWBm)Bm�Bl�Bl�Bm�Bm�Bl�Bm�Bm�Bm�Bp�BqvBr|BqABoiBqABm�BlWBm�Bn/Bk�Bk�Bm)BpBlWBlWBjKBiBiDBe�Bc�Be`BaHBc�Ba�Bc B^jB[#BXEBXyB[WB]dB\]BZ�BT�BN�B7�B6�B6FB.�B,=B)_B$@B)�B�B�BB$B�B�BB�B"B�BFB�BBB�BuB
�B
�B
�(B
�B
�lB
��B
�B
��B
��B
�TB
�B
�iB
��B
�B
��B
�B
�|B
��B
�pB
ߤB
�B
�QB
ܒB
ںB
��B
�
B
�2B
�?B
�}B
�B
ʌB
�6B
��B
�B
��B
��B
�B
�B
�B
��B
��B
��B
��B
�B
�zB
��B
��B
�hB
��B
��B
�qB
��B
�	B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                       B
_B
^�B
^rB
^�B
^�B
^�B
_B
^�B
_B
_B
_B
^�B
_B
_B
_B
_B
^�B
_B
^�B
_�B
`B
`�B
`�B
`B
`�B
c]B
hGB
l�B
o�B
kB
��B
��B
��B
� B
��B
�9B
�tB
��B
��B
�yB
�?B
��B
�?B
�QB
�5B
�BB
�B
�cB
�"B
�kB �B;�B+tB�B�BB �B4�BFBJ�BS�Bt�B��B��B��B�[B�pB�aB�BkB�B�B!6B5B8�B<?BZZBgB^�BT�BK�BF�BB�B<�B#�BB�B�B�B��B��B��ByGBe�Bf�BdcBYBA�B-B�B
�JB
�rB
�
B
�dB
��B
��B
��B
j�B
?�B
+tB
�B	�rB	��B	�B	��B	��B	�B	pxB	U�B	M>B	9�B	10B	,�B	*B	*B	B	�B	�B	�B	B	�B	 =B	xB��B�7B�B�B�B��BܛB��B��B�ZB�GB�8B�5B�AB�4B	;B	CjB	;�B	@WB	6�B	*9B	"�B	�B	B	�B	�B	�B	B	�B	�B	�B	'�B	(�B	(�B	)3B	*B	)�B	+B	+�B	+�B	(-B	'�B	&�B	'�B	,EB	.RB	(�B	+tB	.�B	4vB	3pB	2�B	/�B	-KB	�B	�B	B	B	�B	0�B	?�B	C�B	G�B	F�B	DB	A)B	>KB	4BB	1�B	& B	#wB	#�B	 �B	!B	%�B	'[B	&�B	/#B	-�B	0^B	7 B	?B	A�B	;�B	?�B	A�B	D<B	F�B	FHB	A�B	@WB	?B	?�B	B�B	?B	A)B	C�B	E�B	F|B	H�B	OKB	XNB	YB	W|B	YB	S�B	X�B	\fB	]lB	]�B	^>B	_�B	eiB	fB	d�B	iB	i�B	kYB	m1B	p�B	viB	|YB	��B	��B	��B	��B	��B	��B	�zB	��B	�dB	��B	�[B	�gB	��B	��B	�dB	��B	��B	�jB	��B	�6B	�pB	�vB	��B	��B	�3B	��B	��B	��B	��B	��B	��B	�dB	��B	ɏB	��B	��B	�sB	��B	�B	�B	�#B	ыB	�]B	�)B	ҒB	�iB	�;B	�B	װB	�B	وB	�%B	�%B	�`B	�`B	��B	�B	��B	�B	�B	�DB	߭B	�B	�B	�B	�PB	�B	�(B	�(B	�]B	� B	�5B	�B	�B	�B	�AB	�AB	�B	�B	�B	�GB	�B	�B	�B	�B	�YB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	��B	�B	�B	��B	�YB	�1B	�lB	�>B	�rB	�PB	�~B	�B	�PB	�B	��B	�bB	��B	�.B	��B	��B	�4B	�uB	��B	�B	�GB	�B	��B	�MB	�SB	��B	��B	��B	��B	��B	�_B	�+B	��B	�+B	��B	��B	�1B	��B	��B	�7B
 	B
 =B
�B
B
�B
B
B
B
B
B
�B
IB
�B
�B
�B
�B
�B
�B
�B
4B
:B
�B
nB
:B
�B
nB
	B
nB
B
�B
	B
	�B
	�B

B
	�B

B
	�B

B
	�B

B

B
	�B
�B
B
B
�B
�B
�B
RB
�B
*B
�B
�B
0B
eB
7B
�B
�B
qB
qB
B
=B
�B
�B
IB
UB
�B
�B
�B
B
!B
UB
�B
�B
'B
�B
�B
�B
�B
'B
�B
�B
�B
'B
'B
'B
�B
�B
'B
'B
�B
�B
'B
�B
bB
�B
-B
�B
�B
3B
�B
�B
�B
�B
B
B
9B
B
9B
9B
�B
�B
@B
@B
@B
�B
B
FB
�B
FB
�B
RB
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
!B
!6B
 dB
!6B
!6B
!B
!6B
!kB
#B
#BB
#BB
#BB
$IB
$}B
$IB
$}B
%�B
$�B
%B
''B
& B
&�B
&�B
& B
(-B
'�B
'�B
'�B
(�B
(aB
(�B
(�B
(�B
)�B
)�B
)3B
*B
*�B
*mB
+?B
+?B
*�B
*�B
+tB
,�B
,�B
,�B
-KB
.RB
-�B
-�B
-�B
-�B
-�B
.�B
.�B
/XB
0�B
1dB
0�B
1�B
0�B
1�B
1dB
2�B
1�B
2jB
2B
2jB
3�B
4B
5HB
5HB
6NB
5�B
5�B
6�B
6�B
6�B
6�B
7TB
8&B
8[B
8[B
8�B
8�B
9�B
9aB
9�B
:3B
9�B
;9B
<
B
<
B
<�B
<�B
<
B
=yB
=B
=yB
>�B
>�B
>�B
>�B
?B
?B
>�B
?QB
@�B
@�B
@�B
@�B
@�B
@�B
@#B
@WB
@WB
@WB
@�B
@�B
@WB
A^B
A�B
A�B
A�B
BdB
CB
DpB
DB
C�B
C�B
DB
EBB
E�B
EBB
EB
D�B
E�B
E�B
EvB
FB
F�B
F�B
F|B
F�B
F�B
F�B
GB
H B
G�B
G�B
G�B
H�B
J�B
J`B
I�B
J`B
J�B
K2B
KgB
K�B
LmB
LmB
L8B
M�B
MsB
M�B
M�B
M�B
M�B
M�B
NB
N�B
NB
M�B
MsB
OB
N�B
NyB
OKB
OB
O�B
OB
N�B
OB
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
QWB
R)B
R)B
R�B
S�B
S�B
T�B
T5B
S�B
TiB
UB
T�B
T�B
U�B
UB
VB
V�B
WB
WB
XB
X�B
XNB
W�B
W�B
XNB
XNB
W�B
W�B
W�B
XB
X�B
XNB
XB
Y�B
Y�B
Y�B
ZZB
Z�B
Z�B
ZZB
Z%B
ZZB
Z�B
[,B
[`B
[�B
[�B
[�B
[�B
\�B
\2B
[�B
[�B
\fB
\�B
\�B
]B
\2B
\�B
]�B
^rB
^rB
^rB
^�B
_�B
_�B
_DB
`JB
`B
`JB
`�B
`B
`B
`B
`B
aB
`�B
aPB
a�B
`�B
a�B
a�B
aPB
a�B
aPB
a�B
bWB
b"B
b�B
b�B
c�B
dcB
eiB
d�B
e�B
e5B
e�B
e B
e B
eiB
d�B
e5B
d�B
dcB
e B
e�B
e�B
eiB
f;B
f�B
e�B
eiB
f;B
f�B
gAB
gAB
g�B
gAB
g�B
guB
g�B
g�B
h{B
g�B
hB
g�B
hB
guB
h{B
h�B
iMB
i�B
i�B
jB
jSB
j�B
j�B
j�B
j�B
j�B
k%B
k%B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l+B
l`B
l+B
l`B
l�B
m1B
nB
n7B
nlB
nB
n7B
m�B
n7B
n�B
o>B
o�B
pB
orB
o�B
pDB
p�B
p�B
qB
qB
qJB
q~B
q�B
q~B
rB
rB
rB
rB
rB
rPB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s"B
s�B
sVB
s�B
s�B
t(B
s�B
t\B
t�B
t�B
u.B
u.B
u.B
u�B
v4B
viB
v�B
wB
woB
xB
xB
x@B
w�B
x�B
x@B
x@B
x@B
x�B
xuB
x�B
yB
yGB
yGB
yGB
yGB
yGB
yGB
y�B
y�B
zB
zB
z�B
z�B
z�B
z�B
z�B
z�B
{SB
{SB
{B
{SB
{SB
{SB
{B
{SB
|YB
}+B
}_B
}+B
~1B
~1B
}�B
~1B
~�B
kB
�=B
�rB
�=B
�	B
�=B
�B
�B
��B
��B
�B
�B
��B
�xB
�CB
�B
�IB
�B
��B
�B
�PB
�B
�!B
��B
��B
��B
��B
��B
��B
��B
�'B
��B
`B
`B
b"B
]lB
`B
_B
^�B
^>B
]lB
^rB
^
B
^�B
]�B
^
B
]�B
^
B
^>B
^rB
^�B
_B
_B
_yB
_�B
_yB
`B
_�B
^�B
^�B
^�B
^rB
^�B
]8B
]�B
^�B
^>B
^�B
_B
_�B
`JB
`B
`B
_yB
_DB
^rB
^rB
^>B
^>B
^rB
^rB
]�B
^rB
^�B
_B
^�B
_B
_DB
_�B
_�B
_yB
`B
_yB
_DB
_B
^�B
_B
^�B
^�B
_B
_B
^�B
^
B
^>B
^>B
^>B
^>B
^�B
^rB
_B
_yB
_yB
_�B
`B
_�B
_�B
_�B
_DB
_B
^�B
_B
^�B
^�B
]�B
^>B
^>B
^>B
^>B
^>B
^�B
_DB
_yB
_�B
_�B
`�B
`�B
`JB
_�B
^�B
_DB
_DB
_B
_B
_�B
`�B
`B
b"B
a�B
`�B
`�B
`B
`JB
a�B
`B
`JB
`�B
_�B
`B
`�B
aPB
a�B
aPB
a�B
a�B
a�B
`�B
aB
`JB
_�B
_�B
_DB
`B
_DB
`�B
_DB
a�B
a�B
aPB
`JB
`�B
aB
aB
b�B
c�B
b�B
b�B
bWB
b�B
gB
e�B
e�B
h�B
h{B
h{B
n�B
k%B
mfB
k�B
k�B
l`B
l�B
m�B
n�B
o>B
orB
rPB
pDB
qB
pxB
p�B
u�B
{�B
~�B
�B
�PB
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�$B
�XB
��B
��B
��B
��B
��B
�B
�-B
��B
��B
��B
�9B
� B
�UB
� B
��B
�OB
�UB
��B
��B
� B
��B
��B
�[B
��B
�-B
�aB
��B
��B
�B
�B
��B
�B
�tB
�?B
�?B
�B
��B
�B
�B
�B
��B
�KB
��B
��B
�,B
��B
��B
��B
�B
�3B
�
B
�
B
�?B
��B
�sB
��B
�sB
��B
�B
��B
�EB
��B
�
B
��B
�gB
��B
�9B
�mB
�9B
��B
�mB
��B
��B
��B
��B
��B
��B
��B
�yB
��B
��B
�QB
�QB
��B
�B
�#B
��B
��B
��B
�/B
�/B
B
ÞB
ÞB
�pB
ĤB
ūB
��B
��B
�B
�BB
ĤB
�BB
�|B
��B
�<B
�pB
��B
�B
�`B
�`B
��B
ыB
��B
��B
��B
դB
�NB
�DB
�
B
�B
�B
��B
�(B
�{B
�B
�B
�B
��B�B�B�BqB�BBRB-B(-B26B6B6NB6�B9�B:3BNyB7TB3pB26B,B*�B)gB&�B%OB$}B�BB�B�B�BzBB�BB@B!B�B�BhB�B�B�BB�B�B�B�B$B�B^B'�B)�B-KB0�B5B6NB8[B;�B=�B@�BD�BGNBI�BG�BH BJ`BH�BM>BJ�BI�BI�BK2BL�BP�BP�BN�BL�BVABaBe�Bn7Br�Br�BviB|YB~eB�B��B�CBB�B�CB�~B�~B�IB��B�PB��B�hB�B��B�eB��B��B��B�}B�B��B��B��B�XB��B�)B�B��B��B�)B�5B�sB��B�vB��B��B��B�,B�B��B�dB��B��B�:B�B4B�BhB�B
�BRBB�B�B�B9BB%�B�B dB dB#�B#B�B�B�BFB�B�B%OB$B#�B*mB*B+�B5B<�B=EB:�B7�B5�B9�B8[B8&B<
B4vB5B2�B5B9�BP�BD�BMsBQ�BdcBf�B[`B]8B_�BfoBi�BiBh�Bg�BfoBl`Bj�Bc�Bb�B^
B_yB^�B[`B[`B\�BU�B[�Bb"BbWB`B_DBc�Bb�Ba�B`�B\fB]�B`�BaB`JB\�B]�BXNB[`BW�B\fB[�B[,BU�BR�BQ�BTiBScBP�BNyBP�BQ�BP�BN�BOKBLmBO�BP�BQ#BLmBJ�BH BM
BI&BH�BHTBM>BI�BL8BL�BIZBH�BH BH BLmBI&BEBBFBE�BE�BA�BD�BEvBE�BE�BDpBCjBD�BEBBD�BCjBBdBCBD<BC5BB/BA)BA�BA�B@�B?BG�B@�B?QB<
B=EB<�B=yB<
B9�B9�B>�B=EB7 B5�B:gBH�BScB)�B#�B!B!B�B �B�B!kB�B!kB#wBL8B$B$�B#�B''B!�B�BB�BtB9BBBhBB�B�B'B�B�B�B�B�B�B�B�B�B�B�BeB�B�B
FB�B
BLB	BhB�B:B�B	�BhB\B�B�B\BB�7B�7B�eB��B��B��B�B�B�oB�;B��B�>B��B�iB�B�B�gB�B�EB�jB��B�mB��B�'B�[B��B��B��B��B��B�BB��B��B�<B��B��B��B��B�B��B��B�OB��B��B�B�=B�B�0B��B��B��B��B��B�FB��B��B�B�B�B�FB��B��By�BrPBs�Bs�Bn�Bq~Bo�Bw:BrBk�Bh{Bf;Be�BeiBc�Be5Be�BdcBe5BfBd�Be BfBe�Bd�Be�Be�BfBh�Bi�Bj�BiMBguBiMBfBdcBfBf;Bc�Bc�Be5BhBdcBdcBbWBaBaPB]�B[�B]lBYTB[�BY�B[,BVvBS/BPQBP�BScBUpBTiBR�BL�BF�B/�B.�B.RB&�B$IB!kBLB!�B�B�B!B0B	�B
�BB�B.B�BRB
�B!B'B
��B
��B
�(B
�B
�4B
�"B
�xB
��B
�+B
��B
��B
�`B
�B
�uB
��B
ݡB
�B
��B
وB
��B
�|B
װB
�B
�]B
ԞB
��B
��B
�B
�>B
�KB
��B
�&B
B
�BB
��B
�#B
��B
��B
�B
�B
�B
��B
��B
��B
��B
�$B
��B
��B
��B
�tB
��B
��B
�}B
��B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230531140045                            20230531140045AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023053114004520230531140045  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023053114004520230531140045QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023053114004520230531140045QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               