CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-10-25T07:05:22Z creation; 2023-04-26T19:24:34Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҩ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � hH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20221025070522  20230426192434  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315_008643_176                 7315_008643_176                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @��έ���@��έ���11  @�����+@�����+@08O�4�@08O�4��df�I�W�df�I�W11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @@  @}p�@��R@�  @�  @��RA�RA   A.�RA@��A`  A���A�Q�A�  A�  A���A�Q�A�
=A�B   B(�B  BQ�B z�B((�B0  B7�B?�
BH(�BP(�BW�
B`  Bh(�Bp(�Bx(�B�  B�  B�{B�  B��B�  B�{B�{B�  B�{B�  B��B�  B��B�  B�{B�(�B�(�B��B��B��B��
B��B�  B�  B�  B��B��B��B�B��B��B��C��C��C��C��C

=C  C  C  C  C��C�C�HC�C  C  C   C"{C$  C&  C({C*  C,  C.  C0
=C2
=C3��C6  C8
=C:  C<  C>  C?��CA�CC�CF{CH{CJ  CL
=CN
=CP  CR
=CT
=CU��CW��CY��C[��C^
=C`
=Ca��Cc��Ce��Cg��Ci��Cl  Cn  Cp
=Cr{Ct  Cu�Cw��Cz  C|  C}�C�  C�  C�  C�  C���C���C�C�C�  C�C�C�  C�  C���C�  C���C�  C�C���C�  C�C�\C�\C�\C�C�C�  C�  C�C�C�  C�  C���C���C�  C�C�  C�  C�  C�  C�C�  C���C���C���C�  C���C�  C�
=C�C�C�  C�C�  C�  C���C���C�  C���C���C�  C�C�  C�  C���C�  C�C���C���C�  C�C�C�C�
=C���C���C�  C�C�
=C�  C�  C�  C�
=C�
=C�  C�C���C�  C�C�  C���C���C���C�  C�  C���C�  C�C���C���C�  C���C���C�
=C�
=C�C�  C���C���C���C���C���C�  C���C��C���C���C�  C�
=C�
=C�\C�
=C�
=C�  C��C���C�  C�  D D ��D  D� D�qDxRD��D}qD  D�DD� D��Dz�D  D�DD}qD��D	}qD
�D
� D  D� D�D}qD�RD��DD}qD��D� DD��D�D� D  D� D�D��D  Dz�D��D��D�D�DD��D�qD� D  D��D�qDz�D�qD� D�qDz�D�D� D�qD� D�D� D�RD }qD!  D!}qD!�qD"� D#  D#}qD$  D$� D$�qD%��D&  D&}qD&�qD'� D(  D(��D)D)��D*  D*� D*�qD+z�D+�qD,z�D,�qD-��D.  D.��D/  D/}qD/�qD0z�D0��D1xRD1��D2� D2�qD3� D4D4��D5  D5�D6  D6}qD7  D7� D8�D8� D8�RD9z�D:�D:�D;�D;z�D;�RD<}qD=�D=� D=�qD>� D>�qD?z�D?�qD@� DA  DA� DB  DB� DB�qDC}qDC��DD}qDE  DE�DF�DF� DG  DG��DG�qDH� DI�DI��DJ  DJ��DKDK� DL  DL� DL�qDMz�DM��DN}qDN�qDOz�DO��DPz�DQ�DQ��DQ��DR}qDS  DS� DT  DT}qDT��DU}qDV  DV� DV�qDW}qDW�qDXz�DX�qDY� DZ  DZ� D[  D[��D\D\��D\�qD]� D^D^��D^�qD_}qD`  D`��Da  Da}qDb  Db��Dc�Dc��Dc�qDd� De�De��Df  Df� Df�qDg� Dh  Dh� Di  Diz�Di�RDj}qDj�qDk}qDl�Dl� Dl�qDm}qDm�qDn� Do  Do� Dp�Dp��Dp�qDqz�Dr�Dr�Ds�Ds��Ds�qDt}qDt�qDu}qDv�Dv� Dv�qDw� DxDx��Dy  Dyz�Dy�qDz� D{  D{}qD{�qD|z�D|�qD}��D~�D~� D  D��D��D�B�D���D�D��D�@ D�}qD�� D�HD�AHD��HD�� D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D���D�=qD�� D�� D���D�@ D��HD��HD�HD�AHD�� D�� D���D�>�D���D���D�HD�AHD���D��HD��D�B�D�� D���D�HD�AHD��HD�D�HD�AHD�~�D��qD�HD�@ D�� D��HD�HD�>�D�� D��HD�  D�@ D�� D���D�  D�=qD�|)D��qD�  D�>�D�}qD�� D��D�B�D��HD��HD��qD�=qD�� D���D��D�@ D�}qD���D�HD�AHD��HD��HD���D�@ D���D���D�HD�>�D�|)D��qD�  D�=qD�~�D��HD�  D�>�D�� D�D�HD�@ D�� D���D��)D�@ D���D�D���D�AHD�~�D�� D��D�AHD�}qD��HD��D�@ D�~�D��HD�  D�>�D�� D�� D���D�@ D��HD��HD�  D�B�D�~�D��HD�  D�>�D�� D���D�HD�@ D�� D��qD�HD�@ D��HD��qD�HD�>�D�~�D�� D�HD�C�D���D�� D��D�@ D�}qD��HD���D�@ D�}qD�� D�HD�@ D��HD��HD���D�>�D�z�D��HD��D�AHD�� D���D���D�>�D��HD�� D��qD�AHD�� D��HD�  D�AHD���D��qD�  D�=qD��HD��HD�  D�=qD�� D�D��qD�@ D���D���D�  D�B�D�|)D���D�  D�AHD���D��HD�  D�>�D�~�D�� D���D�@ D�~�D�� D�  D�@ D��HD�� D��D�@ D�}qD���D���D�@ D��HD���D���D�>�D���D�� D���D�B�D��HD��HD�  D�@ D��HD�D�HD�AHD���D��HD��qD�@ D��HD��HD�HD�AHD�� D���D�HD�AHD�D�D��D�@ D�}qD�� D��D�@ D�~�D�� D�HD�AHD�~�DŽqD�  D�B�Dƃ�D�D��qD�>�Dǂ�D�� D��qD�>�DȀ DȾ�D�  D�>�D�~�D�� D�  D�@ D�}qDʽqD���D�@ D�}qD��HD�  D�@ D́HD̾�D��D�>�D̀ D;�D�HD�@ D΀ D�� D�  D�@ D�~�D�� D���D�>�D�~�DнqD�  D�@ D�~�DѽqD�  D�AHD҂�DҾ�D�HD�>�D�~�DӾ�D��qD�AHD�}qD��HD�HD�@ DՀ D�� D�HD�>�D�}qD־�D���D�>�DׁHD׾�D���D�>�D؀ D�D�  D�B�DفHD�� D�  D�=qDځHD��HD�  D�@ D�~�D�� D��qD�=qD܀ Dܾ�D���D�>�D݀ D��HD�HD�@ D�~�D�� D�  D�B�D�~�D�� D���D�@ D�~�D��HD���D�B�D� DᾸD���D�@ D�~�D⾸D�HD�@ D�~�D�� D���D�>�D�HD�D���D�>�D� D��HD�  D�>�D� D�� D�  D�@ D� D羸D���D�AHD�HD�� D���D�@ D� D龸D���D�>�D�~�D�� D�  D�=qD�~�D뾸D���D�>�D� D�� D�  D�@ D�}qD���D�  D�>�D� DD���D�AHDD��HD�  D�@ D�}qD�D�HD�AHD�HD�D�  D�>�D� D�� D�HD�AHD�HD�D���D�>�D�~�D�D�HD�AHD��HD�� D�  D�=qD�~�D�D�  D�>�D�� D���D�HD�>�D�~�D��qD���D�>�D��HD��HD��qD�AHD�z�?#�
?.{?aG�?��R?\?�@
=q@&ff@5@L��@aG�@z�H@��@�z�@�  @�\)@�(�@Ǯ@�z�@�G�@�\)@��HA33A
=qAG�AQ�A\)A&ffA.{A5�A<(�AC33AJ�HAQG�AXQ�A_\)AeAl��As33Az=qA���A��
A�
=A��\A�A���A�(�A��RA��A���A�  A��HA�{A���A�(�A�
=A��A��A�Q�A��A�
=A�=qA��A�  A�33A�ffAљ�A��Aأ�AۅA޸RA��A��A�Q�A��
A�\)A�\A�{A���A�(�A��B��B33B��B�RBz�B
{B�
BG�B
=B��B�RBQ�B�B�B��B=qB�
B�B�\B�
B ��B"ffB#\)B$��B%��B&�RB'�B(��B)��B*�RB+\)B,(�B-�B.ffB/\)B0z�B1p�B2ffB3\)B4Q�B5G�B6=qB733B8(�B9�B:{B;
=B<  B<��B=�B?
=B@  B@��BB{BB�HBD  BD��BF{BF�HBH  BH��BI�BJ�HBK�
BL��BMp�BN�\BO�BPz�BQp�BRffBS\)BTQ�BU�BV{BV�HBW�BXQ�BYp�BZ=qB[\)B\��B]��B^�\B_\)B`  B`��Bb{Bc33Bc�
Bd��Be��Bf�HBg�
Bh��Bi��Bj�RBk�Bl��Bmp�Bn=qBo33Bp(�Bp��Bq�Br�HBs�Bt��Bup�BvffBw33Bx(�Bx��By�Bz�HB{�B|z�B}G�B~{B
=B�  B�ffB��RB��B�p�B��
B�=qB��RB�
=B��B��B�z�B��HB�p�B��
B�=qB���B�
=B��B��B�Q�B���B��B�p�B��B�Q�B��RB��B�p�B��
B�=qB���B���B�\)B��
B�=qB��\B���B��B��B�  B�=qB���B��B���B�{B��\B���B�\)B�B�(�B��\B�
=B�p�B��
B�=qB���B�
=B�p�B��
B�=qB���B��B��B�  B�z�B��HB�G�B���B��B�ffB��HB�\)B��
B�=qB��RB��B��
B�ffB���B�\)B��
B�Q�B���B�33B���B�  B���B�
=B�p�B��B�ffB��HB��B�{B��\B��B��B�{B�z�B���B��B�(�B��RB�\)B��B��\B��B�B�=qB���B�33B�B�Q�B���B��B�(�B���B�p�B�{B��RB�\)B��B��\B�33B�B�ffB��B��B�  B��\B�33B�B��\B�33B�B�z�B�
=B�p�B�  B£�B�33B��
Bģ�B�\)B��Bƣ�B�33B��
B�Q�B���Bə�B�(�B���B�B�ffB�
=B͙�B�(�BθRB�\)B��BЏ\B�\)B�{BҸRB�G�B��
B�ffB���BծB�z�B��B׮B�(�BظRB�\)B�=qB���B�p�B��
B܏\B��B�  Bޣ�B�G�B�B�Q�B�
=B��
B�z�B�33B㙚B�=qB���B�B�Q�B���B�p�B�  B��B�B�{B��B�33B��
B�\B�G�B�  B�z�B�
=BB��\B�33B�B�=qB��HB�B�ffB���B��B�(�B�
=B��B�{B��RB���B�Q�B���B�p�B�  B��\B��B�(�B��\B�33B�C Q�C ��C ��C33C�C�
CG�C��C��C�C�\C�HC{C\)C��C{CQ�C��C�CQ�C��C�
C�C�\C�HC{CffCC	(�C	z�C	�RC
{C
�C
�
C{C\)C�
C(�CffC�RC�Cz�C�C
=Cp�C��C
=C\)C��C(�CffC�C33C�CC{C�\C�C(�Cp�C��CG�C�\C�
CG�C�C�HC=qC�RC  CG�C�RC�Cz�C�RC{C�C��C�C��C�C=qC�C{CQ�C�C(�C�CC=qC��C�HC=qC�C   C Q�C ��C!33C!z�C!�C"\)C"�\C"��C#p�C#�C${C$�\C$�
C%33C%�C&
=C&\)C&�
C'33C'�C(  C(ffC(�C){C)�\C)�HC*\)C*C+
=C+�C+��C,=qC,�RC-(�C-p�C-�HC.\)C.��C/�C/�\C/�HC0Q�C0��C1{C1�C2  C2Q�C2C333C3�C3�HC4p�C4�RC5�C5��C6  C6Q�C6�HC7=qC7�\C8{C8�C8��C9G�C9C:
=C:�C:��C;=qC;�C<(�C<ffC<�HC=\)C=��C>�C>��C>�HC?ffC?��C@�C@��CA
=CA\)CA�HCB33CB�\CC{CCffCC��CDQ�CD��CE  CEp�CE��CF
=CFQ�CF�\CF��CG�CG\)CG�RCG�
CH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                   ?�  @   @@  @}p�@��R@�  @�  @��RA�RA   A.�RA@��A`  A���A�Q�A�  A�  A���A�Q�A�
=A�B   B(�B  BQ�B z�B((�B0  B7�B?�
BH(�BP(�BW�
B`  Bh(�Bp(�Bx(�B�  B�  B�{B�  B��B�  B�{B�{B�  B�{B�  B��B�  B��B�  B�{B�(�B�(�B��B��B��B��
B��B�  B�  B�  B��B��B��B�B��B��B��C��C��C��C��C

=C  C  C  C  C��C�C�HC�C  C  C   C"{C$  C&  C({C*  C,  C.  C0
=C2
=C3��C6  C8
=C:  C<  C>  C?��CA�CC�CF{CH{CJ  CL
=CN
=CP  CR
=CT
=CU��CW��CY��C[��C^
=C`
=Ca��Cc��Ce��Cg��Ci��Cl  Cn  Cp
=Cr{Ct  Cu�Cw��Cz  C|  C}�C�  C�  C�  C�  C���C���C�C�C�  C�C�C�  C�  C���C�  C���C�  C�C���C�  C�C�\C�\C�\C�C�C�  C�  C�C�C�  C�  C���C���C�  C�C�  C�  C�  C�  C�C�  C���C���C���C�  C���C�  C�
=C�C�C�  C�C�  C�  C���C���C�  C���C���C�  C�C�  C�  C���C�  C�C���C���C�  C�C�C�C�
=C���C���C�  C�C�
=C�  C�  C�  C�
=C�
=C�  C�C���C�  C�C�  C���C���C���C�  C�  C���C�  C�C���C���C�  C���C���C�
=C�
=C�C�  C���C���C���C���C���C�  C���C��C���C���C�  C�
=C�
=C�\C�
=C�
=C�  C��C���C�  C�  D D ��D  D� D�qDxRD��D}qD  D�DD� D��Dz�D  D�DD}qD��D	}qD
�D
� D  D� D�D}qD�RD��DD}qD��D� DD��D�D� D  D� D�D��D  Dz�D��D��D�D�DD��D�qD� D  D��D�qDz�D�qD� D�qDz�D�D� D�qD� D�D� D�RD }qD!  D!}qD!�qD"� D#  D#}qD$  D$� D$�qD%��D&  D&}qD&�qD'� D(  D(��D)D)��D*  D*� D*�qD+z�D+�qD,z�D,�qD-��D.  D.��D/  D/}qD/�qD0z�D0��D1xRD1��D2� D2�qD3� D4D4��D5  D5�D6  D6}qD7  D7� D8�D8� D8�RD9z�D:�D:�D;�D;z�D;�RD<}qD=�D=� D=�qD>� D>�qD?z�D?�qD@� DA  DA� DB  DB� DB�qDC}qDC��DD}qDE  DE�DF�DF� DG  DG��DG�qDH� DI�DI��DJ  DJ��DKDK� DL  DL� DL�qDMz�DM��DN}qDN�qDOz�DO��DPz�DQ�DQ��DQ��DR}qDS  DS� DT  DT}qDT��DU}qDV  DV� DV�qDW}qDW�qDXz�DX�qDY� DZ  DZ� D[  D[��D\D\��D\�qD]� D^D^��D^�qD_}qD`  D`��Da  Da}qDb  Db��Dc�Dc��Dc�qDd� De�De��Df  Df� Df�qDg� Dh  Dh� Di  Diz�Di�RDj}qDj�qDk}qDl�Dl� Dl�qDm}qDm�qDn� Do  Do� Dp�Dp��Dp�qDqz�Dr�Dr�Ds�Ds��Ds�qDt}qDt�qDu}qDv�Dv� Dv�qDw� DxDx��Dy  Dyz�Dy�qDz� D{  D{}qD{�qD|z�D|�qD}��D~�D~� D  D��D��D�B�D���D�D��D�@ D�}qD�� D�HD�AHD��HD�� D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D���D�=qD�� D�� D���D�@ D��HD��HD�HD�AHD�� D�� D���D�>�D���D���D�HD�AHD���D��HD��D�B�D�� D���D�HD�AHD��HD�D�HD�AHD�~�D��qD�HD�@ D�� D��HD�HD�>�D�� D��HD�  D�@ D�� D���D�  D�=qD�|)D��qD�  D�>�D�}qD�� D��D�B�D��HD��HD��qD�=qD�� D���D��D�@ D�}qD���D�HD�AHD��HD��HD���D�@ D���D���D�HD�>�D�|)D��qD�  D�=qD�~�D��HD�  D�>�D�� D�D�HD�@ D�� D���D��)D�@ D���D�D���D�AHD�~�D�� D��D�AHD�}qD��HD��D�@ D�~�D��HD�  D�>�D�� D�� D���D�@ D��HD��HD�  D�B�D�~�D��HD�  D�>�D�� D���D�HD�@ D�� D��qD�HD�@ D��HD��qD�HD�>�D�~�D�� D�HD�C�D���D�� D��D�@ D�}qD��HD���D�@ D�}qD�� D�HD�@ D��HD��HD���D�>�D�z�D��HD��D�AHD�� D���D���D�>�D��HD�� D��qD�AHD�� D��HD�  D�AHD���D��qD�  D�=qD��HD��HD�  D�=qD�� D�D��qD�@ D���D���D�  D�B�D�|)D���D�  D�AHD���D��HD�  D�>�D�~�D�� D���D�@ D�~�D�� D�  D�@ D��HD�� D��D�@ D�}qD���D���D�@ D��HD���D���D�>�D���D�� D���D�B�D��HD��HD�  D�@ D��HD�D�HD�AHD���D��HD��qD�@ D��HD��HD�HD�AHD�� D���D�HD�AHD�D�D��D�@ D�}qD�� D��D�@ D�~�D�� D�HD�AHD�~�DŽqD�  D�B�Dƃ�D�D��qD�>�Dǂ�D�� D��qD�>�DȀ DȾ�D�  D�>�D�~�D�� D�  D�@ D�}qDʽqD���D�@ D�}qD��HD�  D�@ D́HD̾�D��D�>�D̀ D;�D�HD�@ D΀ D�� D�  D�@ D�~�D�� D���D�>�D�~�DнqD�  D�@ D�~�DѽqD�  D�AHD҂�DҾ�D�HD�>�D�~�DӾ�D��qD�AHD�}qD��HD�HD�@ DՀ D�� D�HD�>�D�}qD־�D���D�>�DׁHD׾�D���D�>�D؀ D�D�  D�B�DفHD�� D�  D�=qDځHD��HD�  D�@ D�~�D�� D��qD�=qD܀ Dܾ�D���D�>�D݀ D��HD�HD�@ D�~�D�� D�  D�B�D�~�D�� D���D�@ D�~�D��HD���D�B�D� DᾸD���D�@ D�~�D⾸D�HD�@ D�~�D�� D���D�>�D�HD�D���D�>�D� D��HD�  D�>�D� D�� D�  D�@ D� D羸D���D�AHD�HD�� D���D�@ D� D龸D���D�>�D�~�D�� D�  D�=qD�~�D뾸D���D�>�D� D�� D�  D�@ D�}qD���D�  D�>�D� DD���D�AHDD��HD�  D�@ D�}qD�D�HD�AHD�HD�D�  D�>�D� D�� D�HD�AHD�HD�D���D�>�D�~�D�D�HD�AHD��HD�� D�  D�=qD�~�D�D�  D�>�D�� D���D�HD�>�D�~�D��qD���D�>�D��HD��HD��qD�AHG�O�?#�
?.{?aG�?��R?\?�@
=q@&ff@5@L��@aG�@z�H@��@�z�@�  @�\)@�(�@Ǯ@�z�@�G�@�\)@��HA33A
=qAG�AQ�A\)A&ffA.{A5�A<(�AC33AJ�HAQG�AXQ�A_\)AeAl��As33Az=qA���A��
A�
=A��\A�A���A�(�A��RA��A���A�  A��HA�{A���A�(�A�
=A��A��A�Q�A��A�
=A�=qA��A�  A�33A�ffAљ�A��Aأ�AۅA޸RA��A��A�Q�A��
A�\)A�\A�{A���A�(�A��B��B33B��B�RBz�B
{B�
BG�B
=B��B�RBQ�B�B�B��B=qB�
B�B�\B�
B ��B"ffB#\)B$��B%��B&�RB'�B(��B)��B*�RB+\)B,(�B-�B.ffB/\)B0z�B1p�B2ffB3\)B4Q�B5G�B6=qB733B8(�B9�B:{B;
=B<  B<��B=�B?
=B@  B@��BB{BB�HBD  BD��BF{BF�HBH  BH��BI�BJ�HBK�
BL��BMp�BN�\BO�BPz�BQp�BRffBS\)BTQ�BU�BV{BV�HBW�BXQ�BYp�BZ=qB[\)B\��B]��B^�\B_\)B`  B`��Bb{Bc33Bc�
Bd��Be��Bf�HBg�
Bh��Bi��Bj�RBk�Bl��Bmp�Bn=qBo33Bp(�Bp��Bq�Br�HBs�Bt��Bup�BvffBw33Bx(�Bx��By�Bz�HB{�B|z�B}G�B~{B
=B�  B�ffB��RB��B�p�B��
B�=qB��RB�
=B��B��B�z�B��HB�p�B��
B�=qB���B�
=B��B��B�Q�B���B��B�p�B��B�Q�B��RB��B�p�B��
B�=qB���B���B�\)B��
B�=qB��\B���B��B��B�  B�=qB���B��B���B�{B��\B���B�\)B�B�(�B��\B�
=B�p�B��
B�=qB���B�
=B�p�B��
B�=qB���B��B��B�  B�z�B��HB�G�B���B��B�ffB��HB�\)B��
B�=qB��RB��B��
B�ffB���B�\)B��
B�Q�B���B�33B���B�  B���B�
=B�p�B��B�ffB��HB��B�{B��\B��B��B�{B�z�B���B��B�(�B��RB�\)B��B��\B��B�B�=qB���B�33B�B�Q�B���B��B�(�B���B�p�B�{B��RB�\)B��B��\B�33B�B�ffB��B��B�  B��\B�33B�B��\B�33B�B�z�B�
=B�p�B�  B£�B�33B��
Bģ�B�\)B��Bƣ�B�33B��
B�Q�B���Bə�B�(�B���B�B�ffB�
=B͙�B�(�BθRB�\)B��BЏ\B�\)B�{BҸRB�G�B��
B�ffB���BծB�z�B��B׮B�(�BظRB�\)B�=qB���B�p�B��
B܏\B��B�  Bޣ�B�G�B�B�Q�B�
=B��
B�z�B�33B㙚B�=qB���B�B�Q�B���B�p�B�  B��B�B�{B��B�33B��
B�\B�G�B�  B�z�B�
=BB��\B�33B�B�=qB��HB�B�ffB���B��B�(�B�
=B��B�{B��RB���B�Q�B���B�p�B�  B��\B��B�(�B��\B�33B�C Q�C ��C ��C33C�C�
CG�C��C��C�C�\C�HC{C\)C��C{CQ�C��C�CQ�C��C�
C�C�\C�HC{CffCC	(�C	z�C	�RC
{C
�C
�
C{C\)C�
C(�CffC�RC�Cz�C�C
=Cp�C��C
=C\)C��C(�CffC�C33C�CC{C�\C�C(�Cp�C��CG�C�\C�
CG�C�C�HC=qC�RC  CG�C�RC�Cz�C�RC{C�C��C�C��C�C=qC�C{CQ�C�C(�C�CC=qC��C�HC=qC�C   C Q�C ��C!33C!z�C!�C"\)C"�\C"��C#p�C#�C${C$�\C$�
C%33C%�C&
=C&\)C&�
C'33C'�C(  C(ffC(�C){C)�\C)�HC*\)C*C+
=C+�C+��C,=qC,�RC-(�C-p�C-�HC.\)C.��C/�C/�\C/�HC0Q�C0��C1{C1�C2  C2Q�C2C333C3�C3�HC4p�C4�RC5�C5��C6  C6Q�C6�HC7=qC7�\C8{C8�C8��C9G�C9C:
=C:�C:��C;=qC;�C<(�C<ffC<�HC=\)C=��C>�C>��C>�HC?ffC?��C@�C@��CA
=CA\)CA�HCB33CB�\CC{CCffCC��CDQ�CD��CE  CEp�CE��CF
=CFQ�CF�\CF��CG�CG\)CG�RCG�
CH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�r�A�p�A�z�A܃A܃A܁A܅A܍PA܍PA܋DA܋DA܋DA܍PA܍PA܍PAܑhAܑhAܑhAܓuA܍PAܓuAܕ�Aܕ�AܓuAܑhA�z�A�z�A�r�A�p�A�jA�t�A�p�A�jA�jA�dZA�O�A�K�A�G�A�$�A���A��TA��HA�jA�ffA�ƨA���A�"�A�n�A��A�A���A�5?A�E�A��A�{A�dZAɬAȧ�A���A�C�A���AüjAº^A�ĜA���A�A�|�A��A��#A���A��PA��9A�XA���A���A��A�XA��FA�r�A�$�A�;dA�XA�7LA��A��
A��yA��A���A���A�~�A��-A�v�A��#A��+A��A�+A�r�A��wA�A�A�ZA���A�A�x�A�"�A��^A�-A���A���A�K�A�t�A};dA|��A|JA{��Az�Ay�Aw��Av�HAst�ApA�Am�TAi33Ae�Ac�FA^�uA[hsAX�AU�AS
=AR�`AR��AO��AJ��AH�uAG"�AD�uACXABz�AA�A@JA=��A;�;A;XA9�A7�PA5A4�+A3��A1��A.��A.�A-�A,�yA,5?A+A)x�A(��A(�jA(�A(�jA( �A&5?A$bA"�9A ��A�DA��AZAI�A1A��A�AA��AQ�A�A��A�A��Ap�AA~�AJA��A�9AZAJA��A�A�A�A��A��A5?A��A%A�yA�A5?A�hAhsA;dA�/A{A�FA��A7LA
5?A	�mA	XAn�A��A�A��AffA1'A��A��A�9A��A �A��A�A|�AO�AVA��Ar�AZAQ�AQ�A=qA��Ax�A7LA ��A @�`B@�C�@���@���@���@�?}@�V@��9@�r�@�9X@���@��P@�33@���@�-@���@�|�@�+@��@��#@�-@�7@�O�@�bN@��@�;d@��@�ff@���@���@�dZ@�K�@�+@���@�ff@�E�@�5?@���@��@�@�%@畁@�{@�V@�\)@⟾@�v�@�^5@�E�@���@��`@�Z@�;d@�ȴ@�M�@ݑh@��/@��@۝�@�S�@��y@ڇ+@�J@�hs@ش9@�bN@�1@�ƨ@�S�@��@��@�/@�(�@ҸR@ёh@�hs@��@���@ύP@�~�@ͩ�@���@̼j@�j@���@��@���@ʏ\@ɲ-@��@�  @Ǖ�@�+@ƸR@�V@���@�hs@��@ě�@�Z@�b@�dZ@\@�@��7@�p�@�`B@�O�@�?}@�7L@�V@��u@�1@���@�o@�@�`B@��/@�9X@��@���@���@��@��y@���@�-@�?}@��9@���@�A�@��@��w@��@��@��R@�J@�@�`B@��@���@��@�ƨ@�ƨ@���@�|�@�S�@���@��@�p�@�/@��@�r�@�Z@�I�@�r�@�A�@�S�@���@�
=@���@�33@�33@�
=@��@�$�@��7@��@��u@��m@�ƨ@�t�@�"�@���@�ff@�J@���@���@�`B@���@�r�@�ƨ@�K�@��y@��\@�J@�p�@��@���@��/@���@�z�@�(�@�  @�  @�b@��@�o@���@��!@�v�@���@�7L@��`@�1'@�+@�@��@���@�ȴ@�ȴ@���@��!@�v�@���@�7L@���@���@��@�j@�1'@�b@���@�S�@�o@��!@�v�@��@�@���@��7@��@��/@�I�@��@�dZ@�\)@�S�@��H@���@�n�@�$�@���@�O�@���@���@�I�@���@�t�@�S�@�33@�
=@�ȴ@�^5@�$�@��@�@���@�hs@��@��@��@�I�@�b@���@���@��P@�\)@�ȴ@��\@�V@��T@��-@��@�&�@�%@��/@��@�z�@� �@��w@�K�@�+@��y@��+@�V@��@���@�O�@���@���@��u@�r�@�A�@�b@��@�\)@�dZ@�o@��!@�V@��@�{@��@��^@�@���@��^@���@��h@�x�@�hs@�?}@���@��j@�1'@��
@��@��P@�|�@���@�~�@�-@�$�@��^@��^@���@��@�p�@�X@�7L@��@���@���@�j@�1'@��@�@~�@~��@~E�@~@}��@}`B@|�j@|I�@{��@{dZ@{C�@{33@{33@{o@z�H@z-@y�7@x�9@xA�@w�@w�P@vȴ@vV@v5?@u`B@t�@t1@s�@sS�@s@r�\@rM�@q�#@q�7@qx�@qx�@qx�@qG�@q7L@q&�@q%@p��@pbN@p �@o�@o�;@o��@o�@o�P@o�@nv�@m�T@m�@m?}@m�@mV@mV@l�/@l��@lj@l�@k�F@kC�@j�@j�!@j~�@i��@h�`@h��@h�9@h �@g�w@g�@gK�@f��@e�@ep�@e/@d�/@d�@dz�@c�@co@b�@b�@b��@b�\@bM�@a��@aX@`��@`r�@`A�@_�@_;d@^��@^ff@]�@]�@]�@\�D@\(�@[�
@[�F@[o@Z�H@Z��@Z�!@Z��@ZM�@Y�7@Y%@Xr�@Xb@W�@Vȴ@V��@VV@U�h@U/@T�@TZ@Sƨ@S�@So@R��@RM�@Q�@Q��@Q�7@QX@P��@P�u@P1'@P �@O�@O�P@O�@N�+@N5?@N@N@M@Mp�@M/@Lj@L9X@K�@J�H@J=q@I7L@H��@H��@HĜ@HbN@G�;@G��@G�P@Fȴ@F{@D��@D��@D��@DI�@D1@C�m@C�F@C"�@Bn�@B=q@B=q@B=q@B�@BJ@A�#@A��@Ax�@Ax�@A%@@��@@r�@?�@?�@?l�@?K�@?+@?
=@>ȴ@>V@=��@=�h@=p�@=`B@=?}@=?}@=V@<�/@<�j@<z�@<�@;ƨ@;"�@:M�@9�^@9��@8��@8�u@8bN@7�@7�@7|�@7+@6��@6{@5�T@5�h@5V@4�@4�D@49X@3�m@3��@3C�@2�H@2��@2J@1�@1�^@1��@1&�@0��@0�u@0�@0r�@0b@/��@/�@/l�@.ȴ@.��@.V@-�@-�h@-/@,��@,�@+ƨ@+��@+dZ@+S�@+33@*��@*~�@*n�@*=q@)�@)x�@)X@)&�@)%@(��@(Q�@'��@'l�@'\)@';d@'�@&ȴ@&��@&v�@&{@%�-@%p�@%/@$��@$Z@$(�@#�
@#33@"�H@"�!@"n�@"-@!�#@!�#@!�^@!7L@ �@ A�@��@�w@��@|�@|�@\)@K�@+@�@��@�@�R@��@V@$�@�@�T@��@�-@��@�@�@p�@`B@O�@/@�@�/@�j@��@�D@�D@j@9X@�m@ƨ@�@C�@o@o@@�@��@�\@n�@=q@J@�@��@�^@�^@hs@7L@�@��@�`@�9@�@bN@1'@b@  @�@�w@|�@\)@+@
=@�@��@ff@$�@@@�@/@��@�D@j@Z@9X@(�@�@1@��@�F@��@��@dZ@33@�@�!@n�@^5@=q@-@�@J@��@��@��@�@�#@��@X@%@Ĝ@��@�@Q�@1'@1'@b@�;@l�@K�@;d@�@��@��@�+@E�@5?@@�@�T@�T@��@@p�@O�@/@��@�/@�j@��@j@I�@Z@9X@�@1@ƨ@ƨA�bNA�`BA�`BA�p�A�n�A�v�A�v�A�p�A�p�A�v�A�x�A�z�A܃A܅A܅A܃A܁A܃A܃A܁A܁A�~�A܃A܋DA܋DA܋DA܉7A܉7A܉7A܉7A܉7A܇+A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܋DA܋DA܋DA܋DA܋DA܍PA܍PAܑhAܑhA܏\A܏\AܓuAܓuAܑhAܑhAܓuAܓuAܓuAܓuAܓuAܓuAܓuAܓuAܕ�AܓuAܑhA܏\A܍PA܉7A܋DA܉7A܉7A܍PAܓuAܕ�Aܕ�Aܗ�Aܗ�Aܗ�Aܗ�Aܗ�Aܙ�Aܙ�Aܙ�Aܗ�Aܕ�AܓuAܕ�AܓuAܓuAܕ�AܓuAܓuAܓuAܓuAܓuAܑhA܃A܃A�|�A�|�A�|�A�p�A�t�A�x�A�z�A�~�A�|�A�~�A܁A�|�A�x�A�r�A�v�A�p�A�n�A�v�A�t�A�v�A�x�A�t�A�n�A�p�A�r�A�n�A�p�A�p�A�n�A�ffA�hsA�jA�`BA�`BA�jA�l�A�ffA�jA�dZA�r�A�r�A�r�A�z�A�v�A�n�A�p�A�z�A�t�A�v�A�t�A�p�A�r�A�r�A�r�A�p�A�r�A�p�A�l�A�jA�n�A�jA�jA�jA�jA�jA�l�A�n�A�n�A�bNA�jA�jA�hsA�jA�jA�hsA�hsA�hsA�jA�ffA�\)A�ZA�XA�XA�XA�S�A�O�A�M�A�M�A�M�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�K�A�M�A�M�A�M�A�K�A�K�A�I�A�G�A�E�A�A�A�;dA�7LA�33A�/A�(�A� �A��A��A��A��A��A�oA�JA�  A��A��HA���A۸RA۰!A۩�Aۏ\A�p�A�\)A�7LA�oA���A���A��yAھwA�l�A�-A��A�oA���A��A��mA��;A���A�ĜAټjAٶFAٴ9A٧�A١�AٓuA�~�A�hsA�VA�C�A�/A�{A�ȴAؕ�A�p�A�dZA�XA�XA�M�A�A�A�/A��A�A���A���A��A�A�dZA�&�A�VA���A��A���A�
=A�1A�
=A���A��A֮A֋DA�bNA���A��mA���Aՙ�A�|�A�"�A��#A�ffA�I�A�(�A���A�r�A�{A��#A҇+A�1AэPA�?}A��HAЙ�A�\)A�1A���A�Aϙ�A�9XA�1A��#AθRAήAΩ�AάAΟ�A�n�A�O�A�1'A�VA���A��/A�A͟�A̓A�z�A�XÁA�+A�
=A��A��mA���A˺^A˃A�bNA�=qA��A���A���AʼjAʕ�A�p�A�jA�`BA�S�A�5?A�A��mA���Aɰ!AɍPA�ZA�+A�oA���A��mAȣ�A�"�AǅA�M�A���A��mA��`A��#A���Aƺ^AƬAƕ�AƑhA�A�Aŝ�A�=qA��A�  A���AĲ-Aď\A�p�A�-A��Aú^Aã�AÁA�n�A�VA��A���A\A�ZA�M�A�$�A�oA��yA��A���A�jA�C�A�/A��A��A�
=A���A�ZA���A��`A���A���A��A�bA��A�A�^5A�7LA�&�A�"�A�VA�1A�1A���A��
A��jA��RA��A�E�A�ƨA�jA�"�A��A���A�t�A�7LA��A�1'A���A�ĜA��!A���A���A��A�/A���A���A��!A���A�ƨA���A��A��A���A��A��A��TA���A��PA�JA��jA��\A�K�A��A� �A�E�A��A��jA�Q�A��RA���A���A�ffA�O�A�A�=qA���A��^A��hA�jA�A�A�~�A�(�A��A��uA�"�A�  A��yA���A���A�VA���A��RA��A�t�A�bNA�A�z�A�%A���A���A�n�A��hA���A�/A��+A�G�A�oA���A��!A���A�z�A�E�A���A�ƨA��7A�?}A�$�A���A���A�hsA�1'A�oA���A��-A�dZA�=qA�1A��mA��A���A���A��A�n�A���A�K�A��A���A���A���A���A��DA�VA��A���A�dZA� �A���A��A�t�A�r�A�hsA�VA���A���A�t�A�XA�=qA�ȴA�ffA�$�A��A���A�A�ĜA���A��\A�^5A��A���A�O�A���A��A���A��A��`A��wA�r�A�?}A� �A�VA��A��!A��A�JA��A���A�^5A���A���A�\)A��A�|�A�^5A�;dA��A���A���A��mA��HA�~�A�7LA��A��-A��PA�`BA��A�ȴA�hsA�v�A��yA��wA��A�~�A�^5A�A�A�
=A��9A��A�VA�A���A�5?A�JA�  A��yA��#A��-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                   A�dZA�r�A�p�A�z�A܃A܃A܁A܅A܍PA܍PA܋DA܋DA܋DA܍PA܍PA܍PAܑhAܑhAܑhAܓuA܍PAܓuAܕ�Aܕ�AܓuAܑhA�z�A�z�A�r�A�p�A�jA�t�A�p�A�jA�jA�dZA�O�A�K�A�G�A�$�A���A��TA��HA�jA�ffA�ƨA���A�"�A�n�A��A�A���A�5?A�E�A��A�{A�dZAɬAȧ�A���A�C�A���AüjAº^A�ĜA���A�A�|�A��A��#A���A��PA��9A�XA���A���A��A�XA��FA�r�A�$�A�;dA�XA�7LA��A��
A��yA��A���A���A�~�A��-A�v�A��#A��+A��A�+A�r�A��wA�A�A�ZA���A�A�x�A�"�A��^A�-A���A���A�K�A�t�A};dA|��A|JA{��Az�Ay�Aw��Av�HAst�ApA�Am�TAi33Ae�Ac�FA^�uA[hsAX�AU�AS
=AR�`AR��AO��AJ��AH�uAG"�AD�uACXABz�AA�A@JA=��A;�;A;XA9�A7�PA5A4�+A3��A1��A.��A.�A-�A,�yA,5?A+A)x�A(��A(�jA(�A(�jA( �A&5?A$bA"�9A ��A�DA��AZAI�A1A��A�AA��AQ�A�A��A�A��Ap�AA~�AJA��A�9AZAJA��A�A�A�A��A��A5?A��A%A�yA�A5?A�hAhsA;dA�/A{A�FA��A7LA
5?A	�mA	XAn�A��A�A��AffA1'A��A��A�9A��A �A��A�A|�AO�AVA��Ar�AZAQ�AQ�A=qA��Ax�A7LA ��A @�`B@�C�@���@���@���@�?}@�V@��9@�r�@�9X@���@��P@�33@���@�-@���@�|�@�+@��@��#@�-@�7@�O�@�bN@��@�;d@��@�ff@���@���@�dZ@�K�@�+@���@�ff@�E�@�5?@���@��@�@�%@畁@�{@�V@�\)@⟾@�v�@�^5@�E�@���@��`@�Z@�;d@�ȴ@�M�@ݑh@��/@��@۝�@�S�@��y@ڇ+@�J@�hs@ش9@�bN@�1@�ƨ@�S�@��@��@�/@�(�@ҸR@ёh@�hs@��@���@ύP@�~�@ͩ�@���@̼j@�j@���@��@���@ʏ\@ɲ-@��@�  @Ǖ�@�+@ƸR@�V@���@�hs@��@ě�@�Z@�b@�dZ@\@�@��7@�p�@�`B@�O�@�?}@�7L@�V@��u@�1@���@�o@�@�`B@��/@�9X@��@���@���@��@��y@���@�-@�?}@��9@���@�A�@��@��w@��@��@��R@�J@�@�`B@��@���@��@�ƨ@�ƨ@���@�|�@�S�@���@��@�p�@�/@��@�r�@�Z@�I�@�r�@�A�@�S�@���@�
=@���@�33@�33@�
=@��@�$�@��7@��@��u@��m@�ƨ@�t�@�"�@���@�ff@�J@���@���@�`B@���@�r�@�ƨ@�K�@��y@��\@�J@�p�@��@���@��/@���@�z�@�(�@�  @�  @�b@��@�o@���@��!@�v�@���@�7L@��`@�1'@�+@�@��@���@�ȴ@�ȴ@���@��!@�v�@���@�7L@���@���@��@�j@�1'@�b@���@�S�@�o@��!@�v�@��@�@���@��7@��@��/@�I�@��@�dZ@�\)@�S�@��H@���@�n�@�$�@���@�O�@���@���@�I�@���@�t�@�S�@�33@�
=@�ȴ@�^5@�$�@��@�@���@�hs@��@��@��@�I�@�b@���@���@��P@�\)@�ȴ@��\@�V@��T@��-@��@�&�@�%@��/@��@�z�@� �@��w@�K�@�+@��y@��+@�V@��@���@�O�@���@���@��u@�r�@�A�@�b@��@�\)@�dZ@�o@��!@�V@��@�{@��@��^@�@���@��^@���@��h@�x�@�hs@�?}@���@��j@�1'@��
@��@��P@�|�@���@�~�@�-@�$�@��^@��^@���@��@�p�@�X@�7L@��@���@���@�j@�1'@��@�@~�@~��@~E�@~@}��@}`B@|�j@|I�@{��@{dZ@{C�@{33@{33@{o@z�H@z-@y�7@x�9@xA�@w�@w�P@vȴ@vV@v5?@u`B@t�@t1@s�@sS�@s@r�\@rM�@q�#@q�7@qx�@qx�@qx�@qG�@q7L@q&�@q%@p��@pbN@p �@o�@o�;@o��@o�@o�P@o�@nv�@m�T@m�@m?}@m�@mV@mV@l�/@l��@lj@l�@k�F@kC�@j�@j�!@j~�@i��@h�`@h��@h�9@h �@g�w@g�@gK�@f��@e�@ep�@e/@d�/@d�@dz�@c�@co@b�@b�@b��@b�\@bM�@a��@aX@`��@`r�@`A�@_�@_;d@^��@^ff@]�@]�@]�@\�D@\(�@[�
@[�F@[o@Z�H@Z��@Z�!@Z��@ZM�@Y�7@Y%@Xr�@Xb@W�@Vȴ@V��@VV@U�h@U/@T�@TZ@Sƨ@S�@So@R��@RM�@Q�@Q��@Q�7@QX@P��@P�u@P1'@P �@O�@O�P@O�@N�+@N5?@N@N@M@Mp�@M/@Lj@L9X@K�@J�H@J=q@I7L@H��@H��@HĜ@HbN@G�;@G��@G�P@Fȴ@F{@D��@D��@D��@DI�@D1@C�m@C�F@C"�@Bn�@B=q@B=q@B=q@B�@BJ@A�#@A��@Ax�@Ax�@A%@@��@@r�@?�@?�@?l�@?K�@?+@?
=@>ȴ@>V@=��@=�h@=p�@=`B@=?}@=?}@=V@<�/@<�j@<z�@<�@;ƨ@;"�@:M�@9�^@9��@8��@8�u@8bN@7�@7�@7|�@7+@6��@6{@5�T@5�h@5V@4�@4�D@49X@3�m@3��@3C�@2�H@2��@2J@1�@1�^@1��@1&�@0��@0�u@0�@0r�@0b@/��@/�@/l�@.ȴ@.��@.V@-�@-�h@-/@,��@,�@+ƨ@+��@+dZ@+S�@+33@*��@*~�@*n�@*=q@)�@)x�@)X@)&�@)%@(��@(Q�@'��@'l�@'\)@';d@'�@&ȴ@&��@&v�@&{@%�-@%p�@%/@$��@$Z@$(�@#�
@#33@"�H@"�!@"n�@"-@!�#@!�#@!�^@!7L@ �@ A�@��@�w@��@|�@|�@\)@K�@+@�@��@�@�R@��@V@$�@�@�T@��@�-@��@�@�@p�@`B@O�@/@�@�/@�j@��@�D@�D@j@9X@�m@ƨ@�@C�@o@o@@�@��@�\@n�@=q@J@�@��@�^@�^@hs@7L@�@��@�`@�9@�@bN@1'@b@  @�@�w@|�@\)@+@
=@�@��@ff@$�@@@�@/@��@�D@j@Z@9X@(�@�@1@��@�F@��@��@dZ@33@�@�!@n�@^5@=q@-@�@J@��@��@��@�@�#@��@X@%@Ĝ@��@�@Q�@1'@1'@b@�;@l�@K�@;d@�@��@��@�+@E�@5?@@�@�T@�T@��@@p�@O�@/@��@�/@�j@��@j@I�@Z@9X@�@1@ƨG�O�A�bNA�`BA�`BA�p�A�n�A�v�A�v�A�p�A�p�A�v�A�x�A�z�A܃A܅A܅A܃A܁A܃A܃A܁A܁A�~�A܃A܋DA܋DA܋DA܉7A܉7A܉7A܉7A܉7A܇+A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܉7A܋DA܋DA܋DA܋DA܋DA܍PA܍PAܑhAܑhA܏\A܏\AܓuAܓuAܑhAܑhAܓuAܓuAܓuAܓuAܓuAܓuAܓuAܓuAܕ�AܓuAܑhA܏\A܍PA܉7A܋DA܉7A܉7A܍PAܓuAܕ�Aܕ�Aܗ�Aܗ�Aܗ�Aܗ�Aܗ�Aܙ�Aܙ�Aܙ�Aܗ�Aܕ�AܓuAܕ�AܓuAܓuAܕ�AܓuAܓuAܓuAܓuAܓuAܑhA܃A܃A�|�A�|�A�|�A�p�A�t�A�x�A�z�A�~�A�|�A�~�A܁A�|�A�x�A�r�A�v�A�p�A�n�A�v�A�t�A�v�A�x�A�t�A�n�A�p�A�r�A�n�A�p�A�p�A�n�A�ffA�hsA�jA�`BA�`BA�jA�l�A�ffA�jA�dZA�r�A�r�A�r�A�z�A�v�A�n�A�p�A�z�A�t�A�v�A�t�A�p�A�r�A�r�A�r�A�p�A�r�A�p�A�l�A�jA�n�A�jA�jA�jA�jA�jA�l�A�n�A�n�A�bNA�jA�jA�hsA�jA�jA�hsA�hsA�hsA�jA�ffA�\)A�ZA�XA�XA�XA�S�A�O�A�M�A�M�A�M�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�I�A�K�A�M�A�M�A�M�A�K�A�K�A�I�A�G�A�E�A�A�A�;dA�7LA�33A�/A�(�A� �A��A��A��A��A��A�oA�JA�  A��A��HA���A۸RA۰!A۩�Aۏ\A�p�A�\)A�7LA�oA���A���A��yAھwA�l�A�-A��A�oA���A��A��mA��;A���A�ĜAټjAٶFAٴ9A٧�A١�AٓuA�~�A�hsA�VA�C�A�/A�{A�ȴAؕ�A�p�A�dZA�XA�XA�M�A�A�A�/A��A�A���A���A��A�A�dZA�&�A�VA���A��A���A�
=A�1A�
=A���A��A֮A֋DA�bNA���A��mA���Aՙ�A�|�A�"�A��#A�ffA�I�A�(�A���A�r�A�{A��#A҇+A�1AэPA�?}A��HAЙ�A�\)A�1A���A�Aϙ�A�9XA�1A��#AθRAήAΩ�AάAΟ�A�n�A�O�A�1'A�VA���A��/A�A͟�A̓A�z�A�XÁA�+A�
=A��A��mA���A˺^A˃A�bNA�=qA��A���A���AʼjAʕ�A�p�A�jA�`BA�S�A�5?A�A��mA���Aɰ!AɍPA�ZA�+A�oA���A��mAȣ�A�"�AǅA�M�A���A��mA��`A��#A���Aƺ^AƬAƕ�AƑhA�A�Aŝ�A�=qA��A�  A���AĲ-Aď\A�p�A�-A��Aú^Aã�AÁA�n�A�VA��A���A\A�ZA�M�A�$�A�oA��yA��A���A�jA�C�A�/A��A��A�
=A���A�ZA���A��`A���A���A��A�bA��A�A�^5A�7LA�&�A�"�A�VA�1A�1A���A��
A��jA��RA��A�E�A�ƨA�jA�"�A��A���A�t�A�7LA��A�1'A���A�ĜA��!A���A���A��A�/A���A���A��!A���A�ƨA���A��A��A���A��A��A��TA���A��PA�JA��jA��\A�K�A��A� �A�E�A��A��jA�Q�A��RA���A���A�ffA�O�A�A�=qA���A��^A��hA�jA�A�A�~�A�(�A��A��uA�"�A�  A��yA���A���A�VA���A��RA��A�t�A�bNA�A�z�A�%A���A���A�n�A��hA���A�/A��+A�G�A�oA���A��!A���A�z�A�E�A���A�ƨA��7A�?}A�$�A���A���A�hsA�1'A�oA���A��-A�dZA�=qA�1A��mA��A���A���A��A�n�A���A�K�A��A���A���A���A���A��DA�VA��A���A�dZA� �A���A��A�t�A�r�A�hsA�VA���A���A�t�A�XA�=qA�ȴA�ffA�$�A��A���A�A�ĜA���A��\A�^5A��A���A�O�A���A��A���A��A��`A��wA�r�A�?}A� �A�VA��A��!A��A�JA��A���A�^5A���A���A�\)A��A�|�A�^5A�;dA��A���A���A��mA��HA�~�A�7LA��A��-A��PA�`BA��A�ȴA�hsA�v�A��yA��wA��A�~�A�^5A�A�A�
=A��9A��A�VA�A���A�5?A�JA�  A��yA��#A��-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                   ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBPB�BJB�B�BBB~B�B~B�B�B�B�B�B�B~B�B�B~BJB�B�BB�B~BJBJBJB~BBJBBBB�BDB
�B�BYB
	B
�B~BFB'�BD�B�;B��BB��B�&BخB��B��B�B��BSB1BBeB$�B&LB>�B?�B5�B+B8RBQBh>Bi�BW?B^Bx�B~�B� B�@B�qB�*B�eB�@B��Bu�B\]BIRB8�B:�B3hB�B�B�>B� B�KB�}B��B�~B~(BgB>�B$tB{B
�B
�B
�[B
��B
��B
�VB
w�B
i�B
]�B
GEB
$@B
!bB
B
+B
�B

rB	��B	�>B	�B	�&B	��B	�CB	��B	��B	u�B	Z�B	R B	B'B	5?B	0UB	.�B	&LB	�B	B	!�B	 �B	�B	�B	xB	#�B	0UB	&�B	$B	%B	=B	�B	1�B	P�B	^�B	_�B	a�B	�B	��B	�zB	��B	�@B	��B	��B	��B	�IB	�-B	�B	��B	�XB	��B	��B	�DB	� B	��B	��B	�MB	�MB	�$B	��B	��B	��B	�B	��B	�+B	��B	�_B	�7B	��B	��B	�B	��B	��B	�0B	�eB	�6B	��B	�B	��B	��B	��B	��B	�-B	��B	�nB	��B	�hB	�3B	�B	�aB	��B	��B	�3B	�CB	��B	��B	��B	��B	�6B	��B	��B	��B	�kB	�B	�*B	�XB	��B	�_B	��B	�wB	�=B	�wB	�CB	��B	��B	�=B	��B	�=B	�B	��B	��B	�qB	�}B	��B	�OB	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�OB	��B	�OB	��B	��B	�9B	��B	�hB	�3B	��B	��B	�nB	�B	�B	��B	��B	�0B	��B	��B	��B	��B	��B	�BB	��B	��B	�wB	��B	��B	��B	�gB	�B	�9B	��B	�aB	�aB	�-B	�-B	�gB	ŢB	�-B	��B	��B	�9B	�B	��B	ȀB	��B	�RB	�B	�#B	�dB	ϫB	��B	�B	ϫB	ϫB	�}B	�BB	�HB	�B	�HB	ӏB	ԕB	��B	�[B	� B	�2B	��B	՛B	��B	�&B	��B	�,B	��B	�[B	�,B	�gB	��B	רB	֡B	�sB	�?B	�EB	�EB	خB	��B	ٴB	�B	�B	��B	یB	ܒB	��B	�]B	�]B	ܒB	�)B	��B	�]B	�dB	�B	�pB	�HB	��B	�pB	�jB	�B	�B	��B	�B	��B	�,B	��B	��B	�B	��B	�B	�mB	�8B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�WB	�iB	�iB	�iB	� B	�B	�B	�GB	��B	�B	��B	�|B	��B	��B	��B	�2B	��B	��B	��B	�PB	�VB	��B	��B	�cB	��B	��B
 iB
 �B
 �B
�B
�B
B
�B
�B
SB
�B
�B
+B
_B
_B
+B
�B
�B
	�B
	lB
	�B
	�B

	B
	�B
	�B

	B
	�B

�B
B
B
�B
\B
\B
�B
.B
.B
�B
.B
�B
�B
�B
B
:B
:B
oB
oB
B
�B
�B
�B
4B
�B
B
�B
�B
oB
:B
�B
oB
�B
@B
�B
FB
�B
�B
�B
�B
�B
SB
B
SB
�B
�B
$B
�B
�B
1B
eB
1B
B
�B
�B
	B
	B
qB
qB
CB
xB
xB
�B
�B
~B
�B
�B
!B
�B
!B
�B
�B
�B
 �B
!bB
!bB
!�B
"�B
"�B
#B
#:B
#B
#nB
#nB
#�B
$@B
$�B
%zB
%FB
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)_B
)_B
)�B
)�B
*eB
+�B
-�B
.�B
.}B
/OB
/B
/�B
/�B
0UB
1�B
2�B
4nB
4�B
4�B
4�B
4�B
5?B
5�B
5�B
6�B
6�B
6�B
6zB
6B
5�B
4B
3�B
5tB
5B
5tB
5�B
6FB
6�B
6�B
6�B
6zB
7�B
7�B
7�B
7�B
7�B
8�B
8B
8RB
8RB
8�B
8�B
8�B
8�B
9XB
:^B
;0B
;dB
;�B
;�B
<B
<jB
=B
=B
=B
<6B
;�B
<6B
=B
=B
<�B
=<B
=<B
=�B
=qB
=qB
>B
>BB
>BB
>wB
>�B
>�B
>�B
>wB
>�B
>�B
>�B
?B
?�B
?}B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
A B
A�B
B[B
B�B
B�B
B[B
B'B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D3B
EB
E�B
E9B
E9B
F?B
E�B
EmB
FtB
GB
G�B
G�B
HB
H�B
G�B
HKB
I�B
IRB
IRB
IRB
IRB
IB
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K)B
LdB
L0B
L�B
L�B
M�B
MjB
M�B
MjB
NpB
N<B
NB
N<B
N<B
NpB
OBB
OvB
O�B
O�B
PHB
QNB
P�B
QNB
Q�B
Q�B
Q�B
R�B
R�B
S&B
R�B
S&B
T,B
S�B
T,B
S�B
TaB
TaB
T�B
T�B
TaB
T�B
U2B
VB
VB
V�B
V�B
VB
W
B
V9B
W
B
W
B
V�B
W�B
W?B
XEB
X�B
X�B
X�B
X�B
X�B
YB
X�B
YB
Y�B
Z�B
[#B
Z�B
[#B
Z�B
[WB
[#B
[#B
\]B
\]B
\�B
\)B
\]B
\�B
\]B
\�B
]dB
]/B
]/B
]dB
]dB
^B
^jB
]�B
^jB
^�B
^5B
^�B
^�B
_;B
_;B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
_�B
`BB
`BB
`�B
a|B
a�B
a�B
b�B
b�B
b�B
c B
c B
c�B
c�B
c�B
d&B
c�B
c�B
c�B
d�B
d�B
e�B
e�B
e,B
e�B
f2B
ffB
f�B
f2B
f�B
gmB
g�B
g�B
g�B
h>B
h
B
iyB
iyB
i�B
jKB
jB
j�B
jB
kB
kQB
k�B
kQB
l"B
k�B
k�B
l�B
l"B
l�B
m)B
m]B
m)B
m]B
m]B
ncB
m�B
m�B
n/B
m�B
o B
n�B
o�B
o�B
o�B
p;B
qB
p�B
p�B
qAB
q�B
q�B
q�B
rGB
rB
rGB
r�B
sB
s�B
s�B
s�B
tB
tB
tTB
s�B
u%B
uZB
uZB
v`B
u�B
v`B
v`B
v+B
v�B
v+B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
wfB
x8B
w�B
xB
xlB
xB
y	B
xlB
x�B
y>B
x�B
y	B
y>B
y>B
x�B
y�B
y>B
y>B
y�B
y�B
zxB
y�B
z�B
zxB
{B
z�B
{B
z�B
{B
{�B
{B
{�B
{�B
{B
|B
|PB
{�B
|�B
|�B
|B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}VB
}VB
}�B
}�B
~(B
~�B
~�B
~�B
~�B
cB
.B
~�B
cB
�iB
��B
��B
��B
��B
��B
�iB
�iB
�iB
��B
�oB
�;B
��B
��B
�oB
�;B
�B
��B
�uB
��B
��B
��B
�B
��B
��B
��B
��B
��B
�GB
�B
��B
��B
�MB
��B
�B
�SB
�B
��B
�SB
�%B
��B
�SB
�SB
��B
�%B
��B
��B
�YB
��B
��B
�_B
��B
��B
�_B
�_B
��B
�_B
��B
��B
��B
��B
�1B
��B
��B
�fB
��B
�fB
�lB
��BVB�B�B�B�BJB�B~B�BB~B�B	lBxBB�B�B�BBPB�B�B�BPBPB�B"B�B�B�B�B�B�B�B�B�B�BPB�B�B�B�B�B�B�B�B�BPBPB�B�BBxB�BPB�B�B�B�B�B�BxB�BDB�BB�B~B�B~B�BPB~B�B"BB�BPB�B�B
�BJB�B�B�BBxB�B�B�B�B�BBBBB�B�BB�BPBPB�B:BDB"B�BBB�B�B
�B	�B�B�BDB�B"B�BJB�BPB�BJB�BB~B�BB~BPBJB�B~BPBPB�B BB
rB(B(BB(BBPBJB	�B�B�BDB
rB�BB~B�BBxBDBxBxBDB�B�B~BB�BDBDBBDBxB�B\BBxBxBJBBJBBBJB�BVB�BB�B�B�B�B�B~B~BJBJB~BJBBJBJBBJB�B�BxB
�B
=B
	B
	B	�B
rB
rB
	B�B
�BBDBB
rB
=B1B�B�BfBfB	lB�B+B�B�B	B�B{B�B_B�B+B_B�BYBB�B�B�B�B	lB�B
�BB
	BB~BBJBDB
�BB
�B~BVBBPB�B"B�B�B+B�B�BFB�B�B�B�B	BOBxB$B)�B6B:^B+kB(�B*0B1'B1�BHKBOBBS�Bf2Bk�Bq�Bs�ByrB�JB��B��B��B��B�4B��B��B��B�B�aB�}B�-B�3B��B�tBϫB̘B�B�}BΥB�B�^B�0BѷBخB�&B�,B��B҉B��B�B�,B��B�sB�B�BخB�KBרBیB�B��BݘB�B��B�KB�QB�8B�B��B�+B��B��B��B �B��B��B��B�(B�B��B �B1B1B�B�B�B{B�B�B��B�]B�(B.B�B�B	BkB�B�BB�B�B(B4B�B �B.IB&B$�B#�B&�B(�B"4B#�B)�B%B$B#B&B&B-wB33B5�BXEBC�BB'BA BD3B;dB>BBC�BC�B;�B9$B4�B1'B0�B7�B=�B'RB&�B$@B$B%�BC�B;0B+kB.IB0�B=�BA BGzBF?BL0BT�B[WBXyB]�Bd&Bp;Bk�Bg�BiDBj�BiyBl"Bd&By	Be,B]�B[�BUgBTaBR�BS&BZ�BP�BYBR�BV�BlWBj�BtTB�+Bd&Bt�B��Bu�Bt�Bv�B�"B�iB}VB�By>Bu�By	B� B��B��B�B�{B��B��B�B��B��B�!B��B��B�1B��B�kB�@B��B��B��B��B��B��B��B��B��B�B�B�LB��B�'B��B�6B�~B�1B��B�VB��B��B�B|�Bo5Bl�Bg�Ba�B]/B]�Ba�BZQBS�BT�BJ#BF�BOBB>�B@OB;�B6FB9$B33B>BD�B>�B9�B5�B4�B8�B/�B1�BB'B5�B$tBOB�B$B�B�B=B$B�B~B�B�B�B��B��B�TB�B��B�+B��B��B�NB��B��BбB�mB��B��B��B�hB�!B�FB��B��B�XB��B�qB�JB��B��B��B�rB�B~�Bv`Bt�Bu�B��B[#BN<BM�BL�BF�B<6B/�B;dB)�B�B"�BOB$B�B!�B(�B1B
��B
��B
��B
�B
�B
�yB
�;B
�B
�;B
�NB
ÖB
�aB
ǮB
��B
�9B
�HB
�qB
�?B
��B
�XB
��B
�wB
�FB
�~B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                   BB�B�B{BMBMBMBGB�BB�BB�BB3B�BB�B�B3B�B�BMBgB�B�B�B�B�B�B�B�B�B{B�B�B-B�B�B�B�B?B�B
�B�B$�BDMB�mB�yB�fB�bBϑB�YB�B�B��B��BB)B�BWB$&B%�B=B=�B7�B1vB5?BQ�BmBk6BVSB`BB{�B.B��B��B��B��B�CB��B��BvzB]�BI�B7�B:*B4B�B�B��B��B�/B�B�9B�^B�BlqBCGB+�BB
��B
��B
ĜB
��B
�B
�B
t�B
f�B
kQB
I�B
~B
	B
B
 B
�B
�B	��B	�JB	�)B	�@B	��B	�5B	�sB	��B	w�B	Z�B	S�B	B�B	-�B	)�B	0�B	,qB	B	 B	!-B	�B	QB	1B	�B	#�B	-]B	 �B	!�B	#nB	#B	bB	,=B	O�B	^�B	Z�B	Z7B	z�B	�B	�-B	�zB	�B	��B	�_B	�B	�>B	�oB	��B	�B	��B	�B	��B	�3B	��B	�B	��B	��B	��B	�B	�BB	�pB	�(B	�NB	��B	� B	�:B	�oB	�B	�B	�WB	�VB	��B	�NB	��B	��B	��B	��B	��B	�RB	��B	�B	�B	�B	�qB	��B	�WB	�B	��B	� B	��B	�DB	�B	��B	��B	�`B	�fB	�KB	�0B	��B	��B	��B	�nB	��B	��B	��B	��B	�NB	�|B	��B	��B	�,B	��B	��B	��B	�tB	��B	�&B	��B	�FB	��B	�,B	��B	�qB	��B	��B	��B	��B	�8B	�B	�B	��B	��B	�B	��B	�mB	��B	�XB	��B	��B	��B	�)B	��B	�kB	�QB	�B	�qB	�CB	�]B	� B	��B	��B	�B	�%B	��B	��B	�ZB	�`B	�+B	��B	�B	��B	��B	�B	�dB	��B	��B	�B	�<B	�B	�JB	�JB	�B	��B	��B	��B	��B	�"B	��B	��B	��B	� B	�oB	�B	��B	�GB	��B	�B	�fB	ȚB	�1B	ȚB	ɠB	�7B	ʦB	��B	��B	��B	��B	˒B	�JB	�B	��B	��B	��B	�dB	��B	�JB	̈́B	�JB	�~B	�B	��B	�BB	�}B	�vB	�HB	�B	�NB	�B	�NB	ѷB	� B	�:B	өB	ԯB	ԯB	�MB	ԯB	�,B	�,B	�aB	��B	�FB	�gB	ևB	��B	��B	ۦB	�KB	ؓB	��B	��B	�9B	��B	�B	ںB	�~B	�;B	��B	��B	�B	�5B	��B	�pB	�B	�bB	��B	��B	�B	�B	�B	�B	�ZB	� B	�B	�RB	�B	�B	�sB	�eB	�B	�B	�QB	��B	��B	�0B	�QB	�/B	��B	�iB	�B	��B	��B	��B	�?B	��B	��B	�B	��B	��B	�$B	��B	�	B	��B	�B	�dB	�B	��B	�B	�(B	��B	�}B
 B	��B	�}B
  B
 B
AB
�B
�B
�B
B
�B
�B
�B
oB
uB
�B
�B
�B
+B
zB
�B
�B
fB
�B
	�B
�B
�B
	lB
	�B
	�B
	�B

#B

�B
B
B
	�B
	�B
	B
	�B

	B
	�B

=B

�B

XB

�B

rB

�B
xB
�B
0B
PB
�B
VB
�B
�B
B
�B
�B
�B
�B
\B
BB
.B
�B
hB
�B
�B
�B
�B
�B
B
�B
�B
aB
{B
{B
�B
�B
�B
�B
9B
?B

B
?B
�B
�B
�B
KB
B
B
�B
�B
�B
WB
#B
�B
qB
qB
�B
�B
dB
dB
~B
B
�B
�B
�B
�B
 �B
 �B
 �B
!HB
!bB
!�B
"hB
"�B
#:B
%�B
'RB
&�B
'RB
&�B
'mB
'�B
'�B
)DB
*�B
,=B
,�B
,�B
,�B
,�B
-wB
-�B
.�B
/B
.�B
.�B
.}B
/5B
.B
,WB
+�B
-�B
,�B
-CB
-�B
.B
.}B
.�B
.�B
.�B
/�B
0B
0!B
0B
0!B
0�B
0B
0UB
0;B
0�B
0�B
1AB
0�B
1�B
2-B
2�B
3B
3hB
3�B
4B
4�B
5tB
5�B
5%B
49B
3�B
4�B
5%B
4�B
5tB
5�B
5�B
5�B
5ZB
5tB
6+B
6+B
6`B
6`B
6zB
6FB
6FB
6FB
6�B
6`B
6�B
7B
7�B
7fB
7�B
7fB
7fB
7�B
7�B
7�B
8�B
9rB
9�B
:DB
:DB
:DB
9�B
9�B
:�B
:�B
:�B
;B
;�B
;�B
;�B
<B
<�B
=qB
=VB
=B
=�B
>BB
=�B
=�B
>�B
?}B
@B
?�B
@B
@iB
?�B
A B
A�B
AB
@�B
A B
A;B
A B
A�B
B'B
B�B
B�B
B�B
B�B
CB
C�B
C�B
D�B
DgB
EB
D�B
E�B
EmB
E�B
E�B
F?B
E�B
E�B
FB
FtB
GB
G�B
G�B
G�B
H1B
H�B
I7B
H�B
I�B
J	B
I�B
JXB
J�B
J�B
K^B
J�B
KxB
LJB
K�B
K�B
K�B
L~B
L~B
MB
L�B
LJB
MB
MjB
NVB
NB
N�B
NVB
NB
OB
N<B
O�B
OB
OBB
P.B
O�B
Q4B
P�B
P}B
P}B
P�B
QB
QNB
Q B
Q�B
R�B
S[B
SB
RoB
S&B
R�B
S&B
S&B
S�B
T�B
T,B
T,B
S�B
TB
TFB
T,B
T�B
U2B
T�B
UMB
UMB
U�B
V9B
VSB
U�B
VB
VmB
VB
V�B
V�B
W?B
W
B
WYB
W?B
W�B
W?B
WYB
W�B
W�B
W�B
W�B
X+B
X_B
X�B
YB
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
[qB
[�B
[�B
[�B
[�B
[�B
[�B
\xB
\�B
]dB
]dB
]B
]�B
]�B
^�B
^OB
]�B
^OB
_pB
_�B
_pB
_;B
_�B
_�B
aHB
a-B
a|B
bhB
b4B
b�B
bhB
cB
c:B
c�B
c�B
c�B
c�B
cnB
dZB
c�B
d�B
d�B
d�B
d�B
eFB
e`B
fB
e�B
e�B
e�B
fB
gB
f�B
g8B
g�B
g�B
h$B
h�B
hsB
h�B
iDB
iyB
i�B
i�B
jKB
i�B
j0B
kB
kB
kQB
kQB
k�B
lB
k�B
lB
lB
mwB
mCB
m]B
m�B
mwB
nB
m�B
m�B
ncB
m�B
n/B
n}B
nIB
n�B
n}B
n�B
oiB
o5B
o�B
oiB
o�B
pB
o�B
p�B
pB
p;B
p�B
pUB
p�B
qB
p�B
p�B
qvB
p�B
p�B
qvB
q�B
r-B
q�B
r|B
rGB
r�B
rGB
r�B
r|B
sMB
shB
sMB
s�B
shB
s3B
s�B
s�B
s�B
tTB
tnB
s�B
t9B
t�B
t�B
t9B
t�B
u?B
uZB
utB
u%B
u?B
utB
u�B
u�B
v`B
v�B
v�B
v�B
wB
wB
v�B
wfB
x�B
xRB
xRB
xlB
xRB
x8B
xB
xB
xB
xlB
y	B
x�B
yrB
y�B
yXB
y$B
y�B
zDB
z*B
zxB
zDB
zDB
z�B
zxB
zxB
zDB
z^B
zxB
{0B
|B
{�B
{�B
|B
|�B
|�B
|�B
|�B
|�B
}VB
}�B
}qB
}B
}B
}qB
}�B
~]B
~wB
~(B
~(B
~�B
~�B
~]B
~wB
HB
B
~�B
.B
�B
�B
HB
�B
�B
�B
�B
�B
��B
�4B
��G�O�B�B%BB
��B�B�BGB�BB�B�BB �B�B�B�BMBB�B�BB�BB�B�B�B�B�B�B�BBSB�BBBB�B�B�BSBBSBBBSB�B�B�B�BMBMB�B�BMB�BMBMBBBGBB�BGB�BB{BGB�BMB�B�B�B�B�B�B�BSB�BMBBB�BGBGBB{B�BGBGBBBB�B�B�B�BB�B�B�B�B�B�B	�B�B�BYB�B�BBMBAB;BBGB�BB�BB�BMB�B
�.B�BMB{B�BGB{B�B�B�B�B�B�B�BMBfB�B�B�B�B�B�B�B�B�B;B�BGB�B�B�B{B�BB{B�B�B�B�B�BMBMB�B{BGB�B�B{B�B�BB�B{B�B�B�B{B�B{B{B�B�B�BMB�BMBBMBMBB�B�B�B�B�B�B{B�B�B{B�BGBGB�BAB�BoBoB;B�B�BoBBABuB�BuB�B�B
��B 4B  B
��B
��B �B
�.B
��B  B
��B iB
��B
��B
��B
��B
��B
��B
��BGB
��B
��B
��B%B	7B%B �B  BABuBoBuB�B{B�B�BAB�BB�B�B�B�B�B�BBB�B
	B1B�BBVB�B.BoB�B�BqB!-B-wB1�B"�B�B!�B(�B)*B?�BF�BJ�B]~BcBi*Bj�Bp�B��B�'B��B�?B|B��B��B��B�B�]B��B��B�yB�B�B��B��B��B�_B��B��B�SBªB�{B�B��B�rB�xB�JB��B�7B�fB�xB�.BοB�hB�hB��BЗB��B��B��B�"B��B�6B�'B�B�BބB��B�FB�wB��B��B��B��B�B��B��B�tB�hB��B��B�}B�}B��B�B��B��B�6B�$B��B��B�tBzB	B0BTB�B�B
#B
XBEB�BtB�B-BEB%�BdB)B#BB�BB�B �B]BWBQBdBdB$�B*B-)BO�B;B9rB8lB;B2�B5�B:�B;B3B0oB,"B(sB(
B.�B4�B�B�B�BWB/B;B2|B"�B%�B(
B5%B8lB>�B=�BC{BK�BR�BO�BT�B[qBg�BcB_!B`�Ba�B`�BcnB[qBpUB\xBUBS@BL�BK�BJ=BJrBQ�BG�BPbBI�BM�Bc�Ba�Bk�B�vB[qBlBy�BmBk�BnB�mBw�Bt�BxRBp�BmCBpUB�KB��B�)B�gB��B�&B�?B�gB�B��B�mB�B��B�}B��B��B��B�B��B� B�GB�B�/B��B�,B��B�NB�jB��B�2B�sB�/B��B��B�}B��B��B�B�B�dBtBf�Bc�B^�BX�BT{BT�BY1BQ�BKDBLBAoB=�BF�B5�B7�B3B-�B0oB*B5ZB<B6+B1B-)B+�B/�B'B)DB9rB,�B�B�B
BpBB	B�BpBB�B B B�$B��B�6B�B�_B�-B�wB�-B�=BٚB�B�B��B��B�:B�B��B��B��B��B�0B�CB��B�CB��B��B.B� B�%B��BzxBv+Bm�BlWBm]B{JBR�BE�BE9BC�B>B3�B'RB2�B!-B$BB�BqB�BeB�B
��B
�B
��B
�WB
�B
��B
��B
֡B
�B
�B
ȴB
��B
��B
�B
�3B
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
�MB
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<_0�<�m�<x�<#�
<#�
<#�
<5��<#�
<#�
<#�
<#�
<M��<#�
<?�<#�
<#�
<#�
<#�
<?X=<�?�<#�
<+,o<jm<8��<#�
<C��<P
�<&��<Y�*<e<c8]<<p�<Le�<�K�<��^<0�*<6�<&	<#�
<#�
<,ɷ<-�l<$�C<#�
<8�K<Y�f<Fݮ<T�	<#�
<ii�<qb�<a,�<�/<r,�<r#�<U�'<@�p<J��<BH�<*�0<#�
<#�
<�;9<Eс<#�
<#�
<#�
<#�
<#�
<#�
<#�
<=q<6�<#�
<u�&<9�d<#�
<���<6-=<#�
<4�<$&�<#�
<#�
<;f'<w�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0083(+/-0.0049)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NO correction for Conductivity Thermal Mass (CTM) is applied;    OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0083(+/-0.0049)                                                                                                                     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2022102507052220221025070522IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARCAARCAADJSADJS                                                                                                                                        2022110407020620221104070206  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022110407020620221104070206QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022110407020620221104070206QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�4000            4000            SI  SI  ARFMARFM                                                                                                                                                2023042610040820230426100408IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242520230426192425IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242520230426192425IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242520230426192425IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                