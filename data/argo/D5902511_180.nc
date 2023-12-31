CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-08-26T21:26:38Z creation; 2022-02-04T23:30:03Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     �  d�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � aP   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � i8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210826212638  20220204223516  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_180                 6810_008521_180                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @َ���?�@َ���?�11  @َ�����@َ�����@1�$5inY@1�$5inY�d�򐫴N�d�򐫴N11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@:�H@z�H@�  @��R@�  @��RA\)A   A,(�A?\)A_\)A\)A��A�  A�  A�  A�  A߮A�A��B  BQ�BQ�B (�B(  B0  B8  B@  BG�
BO�
BX  B`  Bh  BpQ�Bx  B�
B��B��B�  B�{B�{B�{B�(�B�  B��B�{B��B�  B�  B�{B�  B��B�(�B�{B�{B�{B�  B��B�  B�{B�  B�{B�{B�(�B�{B��B�  B��C
=C
=C
=C
=C
  C  C{C{C  C��C��C  C  C��C  C 
=C"{C$
=C&  C'��C*
=C,�C.
=C0  C2  C4{C6
=C7��C9��C<
=C>  C@
=CB  CC�CF  CH
=CJ
=CL  CN  CP
=CR  CT
=CU��CX  CZ
=C\  C]��C_��Ca��Cc��Cf  Cg��Ci�Cl  Cn{Co��Cr{Ct
=Cv  Cw��Cz  C|  C~  C�  C�  C�  C�  C�  C�  C���C�C�C�  C�  C�C�  C�C�C�  C�  C�  C�  C�
=C�  C�  C�  C�  C�C�  C�  C�  C�  C�C�  C�C�C���C���C�  C���C�  C�  C�C�  C�  C�  C�  C���C�C�C���C���C�C�  C�  C�  C���C�  C�  C�  C�  C�C�  C���C�C�C�  C���C�  C�C�  C�C�
=C�  C���C�C�  C�  C�
=C�C�
=C�
=C�  C�C�  C���C���C�  C�  C���C�C�  C���C���C�C�C�  C���C�  C�C�C�  C�  C�C�  C���C���C���C�  C�C�  C�C�C�  C���C�C�  C�C�
=C�  C�  C�  C�
=C���C���C�  C�  C�
=C�
=C�C�  D   D }qD �qD}qD�D��D�qD}qD�D��D�qD}qD�qD� D  D� D  D� D��D	}qD	�qD
z�D
�qD� D  D� D  D��D�qDz�D��D��DD}qD�qDz�D�qD��D�D� D  D� D�qD}qD�qDz�D  D� D�qD}qD  D}qD��D}qD�D� DD�D  D}qD  D��D�D� D   D }qD!  D!}qD"  D"��D#  D#� D$  D$}qD%  D%� D%�qD&� D'  D'� D(  D(� D(�qD)� D*�D*� D*�qD+}qD,  D,� D-�D-� D.  D.��D/�D/� D0�D0}qD0��D1}qD2�D2� D2��D3}qD4  D4}qD5  D5��D6  D6}qD7  D7� D8�D8� D8�qD9� D:�D:�D;�D;}qD;��D<}qD=  D=��D>�D>��D?  D?� D@�D@�DA�DA� DBDB� DC  DC��DD  DD� DE  DE��DF�DF� DG�DG�DH�DH� DI  DI�DJDJ� DJ�qDK� DL  DL��DM�DM� DN  DN��DN�qDO}qDP�DP��DQ  DQz�DQ�qDR}qDS  DS��DT  DT� DU�DU��DV�DV�DWDW��DX  DX}qDYDY��DZ  DZ� DZ�qD[}qD\  D\� D]  D]��D]�qD^z�D^�qD_}qD`  D`��Da  Da��DbDb��Db�qDc��Dd�Dd}qDe  De��Df�Df� Dg  Dg��DhDh��Di�Di}qDi�qDj}qDj�qDk��Dl�Dl��Dm�Dm}qDn  Dn� Do  Do� Dp�Dp��DqDq��Dr�Dr��Ds�Ds��Ds��Dtz�Du  Du� Du�qDv��Dw  Dwz�Dw�qDx� Dy�Dy}qDy��Dz�D{D{��D|�D|� D|�qD}��D~�D~}qD  D� D��D�B�D��HD��HD��D�B�D��HD��HD���D�=qD�~�D��HD�HD�@ D�� D���D�  D�AHD�� D��qD���D�@ D�� D�� D���D�>�D�~�D��qD�  D�>�D�}qD�� D�HD�AHD��HD�� D��qD�@ D��HD��HD�HD�AHD�~�D���D���D�>�D�� D���D��qD�>�D�� D�� D�HD�@ D�� D��HD�HD�>�D�}qD�� D�HD�AHD���D�� D���D�AHD���D�D�HD�AHD���D�� D�  D�@ D�~�D���D��qD�=qD�~�D�� D���D�=qD�~�D��qD��qD�>�D�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�>�D�}qD�� D�  D�AHD�� D��)D���D�@ D�~�D�� D�HD�AHD���D��HD�  D�>�D�� D�D�  D�=qD�}qD��HD��D�B�D��HD���D���D�@ D��HD���D�  D�@ D��HD�� D���D�AHD�� D��HD��D�AHD�~�D�� D��D�C�D��HD�� D���D�=qD�~�D���D��qD�=qD�~�D�� D�HD�B�D��HD�� D�HD�@ D�~�D��qD�  D�@ D�~�D�� D���D�@ D�� D�� D��D�AHD�~�D���D�  D�>�D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D���D�  D�@ D��HD�D�  D�>�D��HD��HD�  D�>�D�� D��HD�HD�AHD�� D��HD�HD�AHD�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�HD�B�D��HD��HD�  D�>�D�~�D���D�HD�AHD�� D��HD��D�AHD�� D��HD�HD�@ D�~�D�� D��D�B�D��HD��HD�HD�@ D�� D�� D���D�>�D��HD��HD�HD�AHD�� D�� D���D�>�D�~�D�� D���D�@ D���D��HD�  D�AHD�� D��HD�  D�@ D�~�D�� D��D�AHD�}qD½qD�  D�@ DÀ Dþ�D���D�B�DĂ�D��HD�HD�@ D�}qDŽqD�  D�AHDƀ Dƾ�D�  D�B�Dǀ DǾ�D�  D�@ DȀ D��HD���D�>�DɁHD�� D�HD�@ D�}qDʾ�D���D�>�DˁHD��HD�  D�>�D̀ D�� D�HD�AHD͂�D��HD�  D�@ D΀ Dξ�D���D�AHDπ DϾ�D���D�>�D�~�DнqD�  D�AHDсHD��HD�HD�AHDҀ D�� D���D�AHDӁHD�� D�  D�@ DԀ D�� D���D�@ DՀ D�� D�HD�@ Dր D�� D�  D�@ D׀ D׽qD��qD�=qD�}qD�� D�  D�=qD�~�D��HD�  D�>�Dڀ D��HD���D�>�Dۀ D�� D�  D�@ D܀ D��HD�HD�>�D�~�D��HD�  D�>�DށHD�� D�HD�@ D�~�D�� D���D�AHD�� DྸD�  D�@ D�~�D��HD�HD�@ D� D⾸D���D�@ D�~�D�� D�HD�B�D䂏D��HD�HD�@ D� D�� D�  D�@ D� D澸D���D�AHD�HD��HD�  D�AHD�HD�qD���D�@ D�~�D�qD���D�@ D�HD꾸D��qD�@ D�HD��HD�HD�AHD�HD��HD���D�=qD� D�� D���D�AHDDD�  D�@ D� D��HD�HD�AHD���D��HD�HD�AHD�HD��HD�  D�@ D�HD�� D���D�>�D� D��HD�HD�@ D�~�D���D�  D�@ D��HD���D�  D�AHD��HD��HD�  D�AHD��HD�� D�  D�AHD�� D�� D�  D�@ D�� D���D��qD�>�D�� D�D��D�5�?#�
?L��?�  ?��
?Ǯ?�
=?�@\)@(�@(��@@  @W
=@fff@s33@��@�{@�z�@�  @��@��@���@�ff@��@��H@��
@�\)@���A   Az�A�A�RA�\A��A�RA!G�A%A+�A.�RA333A8��A=p�AA�AFffAL(�AP��ATz�AZ=qA^�RAb�\Ag
=Al��Ap��Atz�Az�HA�  A���A�(�A�
=A�G�A��A�A���A��HA��A�  A��HA�z�A�
=A��A��A�p�A�  A��\A�z�A��RA�G�A�33A��A�  A��A��A�ffA�G�A�33A���A�\)A��A��
A�p�A�Q�A�=qA��
A�{Aأ�A��A��
A޸RA�Q�A��A�z�A�RA�  A�\A���A�
=A���A�\A���A�\)A���A�33A��B   BG�B{B
=Bz�B��B=qB�B��B	�B
�HBz�BB�HB�B�B�\B\)BQ�BB
=B�
BG�B�\B33Bz�B�B
=B�
B ��B"=qB#\)B$(�B%G�B&�\B'�B(Q�B)��B+
=B+�
B,��B-�B/
=B/�
B0��B2{B3\)B4(�B5G�B6�RB7\)B8Q�B9B:�RB;�B<��B=�B>�RB?�
BAG�BB=qBB�HBD(�BEp�BFffBG33BHz�BIBJ�\BK\)BL��BM�BN�\BO�BQ�BR{BR�RBTQ�BUG�BV{BW\)BXz�BY�BZffB[�B\(�B]p�B^�\B_�B`z�BaG�Bb�RBc�Bdz�BeBf�HBg�Bh��Bj{Bk33Bl  Bl��BnffBo�Bpz�Bqp�Br�HBt  Bt��Bu�Bw33BxQ�By�BzffB{�B|z�B}p�B~�HB�  B�z�B��HB��B�(�B��\B�
=B��B�=qB��RB�33B��B�ffB��HB�\)B�{B��\B���B��B�(�B��\B�
=B��
B�Q�B��RB�33B��B�z�B���B�p�B�(�B���B��B���B�Q�B��HB��B��B�ffB���B�B�=qB���B�33B��
B�ffB���B�p�B�  B���B�33B���B�{B��RB�G�B�B�=qB���B�p�B�  B�ffB���B�p�B�{B�z�B���B�p�B�(�B��RB��B���B�=qB��HB�G�B��
B�z�B��B���B�{B��\B�33B��
B�ffB���B�p�B�(�B���B�
=B��B�Q�B��RB�G�B�  B��\B���B���B�=qB���B�33B��
B��\B��B��B�(�B��HB�p�B��
B�z�B�33B���B�(�B��HB��B�  B�z�B�
=B�B�Q�B��RB�G�B�  B\B�
=BÙ�B�Q�B���B�\)B�  BƸRB�\)B�B�=qB���Bə�B�=qBʸRB�33BˮB�Q�B�
=BͅB��B�ffB��B�B�Q�BиRB�G�B�  Bҏ\B�
=BӅB�{BԸRB�G�B�B�=qBָRB�\)B��B؏\B��BمB�{BڸRB�p�B��B�ffB��HB݅B�(�B޸RB��Bߙ�B�Q�B���B�p�B��
B�ffB��B�B�=qB��B�33B��
B�ffB�
=B�p�B��
B�z�B��B�B��B�z�B���B�B�(�B��B��B홚B�=qB���B��BB�(�B�RB�
=B�B�  B�z�B��B�B��B�Q�B�
=B��B��B�=qB���B�\)B�B�(�B���B�33B��B�ffB���B�G�B��
B�ffB���B�p�B��B�Q�B���B��C   C =qC z�C �RC  C\)C��C�
C�Cp�C��C��C=qC�C�
C�Cp�C�RC�C(�Cz�C��C{CQ�C�\C��C{Cp�C�RC��C(�Cz�CC	�C	Q�C	�C	�
C
�C
p�C
�RC
�C(�Cp�CC{C\)C��C�
C{CQ�C��C  CG�C�C�RC  CG�C��C�C(�CffC��C�C=qC�\C��C
=CG�C�\C�HC33Cz�C�C�C(�Cp�C�RC{CQ�C�CC
=CffC�C��C(�Cp�CC�Cp�C�RC�C=qC��C�C(�CffCC�Cz�C�RC��CG�C�C  CQ�C�\C�
C33C�\C�
C{C\)CC �C ffC �C ��C!G�C!��C!��C"G�C"�\C"��C#�C#z�C#�HC$�C$ffC$C%(�C%z�C%�HC&(�C&p�C&��C'33C'�\C'�C(=qC(�C(�HC)G�C)��C*  C*=qC*�\C*��C+\)C+C,{C,\)C,��C-  C-\)C-C.{C.ffC.�C/  C/\)C/�RC0�C0p�C0�RC1  C1G�C1�C2
=C2ffC2�C3  C3Q�C3��C4  C4ffC4�RC5  C5G�C5��C5�C6Q�C6��C6��C733C7�\C7�
C833C8�\C8�HC933C9p�C9�RC:  C:Q�C:��C:�C;=qC;�C;C;�C<(�C<p�C<�RC<��C=(�C=Q�C=p�C=��C=�HC>{C>=qC>\)C>z�C>��C>��C?  C?=qC?ffC?�\C?��C?�HC@{C@G�C@z�C@��C@C@�HCA
=CA=qCAz�CA��CA�
CA��CB{CB=qCBffCB��CB��CC
=CC(�CCG�CCp�CC��CC�
CD
=CD=qCD\)CDz�CD�CD�CE�CE=qCE\)CE�\CE��CF  CF(�CFQ�CFz�CF��CFCF��CG33CG\)CGz�CG��CGCG�CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                     ?�  ?��H@:�H@z�H@�  @��R@�  @��RA\)A   A,(�A?\)A_\)A\)A��A�  A�  A�  A�  A߮A�A��B  BQ�BQ�B (�B(  B0  B8  B@  BG�
BO�
BX  B`  Bh  BpQ�Bx  B�
B��B��B�  B�{B�{B�{B�(�B�  B��B�{B��B�  B�  B�{B�  B��B�(�B�{B�{B�{B�  B��B�  B�{B�  B�{B�{B�(�B�{B��B�  B��C
=C
=C
=C
=C
  C  C{C{C  C��C��C  C  C��C  C 
=C"{C$
=C&  C'��C*
=C,�C.
=C0  C2  C4{C6
=C7��C9��C<
=C>  C@
=CB  CC�CF  CH
=CJ
=CL  CN  CP
=CR  CT
=CU��CX  CZ
=C\  C]��C_��Ca��Cc��Cf  Cg��Ci�Cl  Cn{Co��Cr{Ct
=Cv  Cw��Cz  C|  C~  C�  C�  C�  C�  C�  C�  C���C�C�C�  C�  C�C�  C�C�C�  C�  C�  C�  C�
=C�  C�  C�  C�  C�C�  C�  C�  C�  C�C�  C�C�C���C���C�  C���C�  C�  C�C�  C�  C�  C�  C���C�C�C���C���C�C�  C�  C�  C���C�  C�  C�  C�  C�C�  C���C�C�C�  C���C�  C�C�  C�C�
=C�  C���C�C�  C�  C�
=C�C�
=C�
=C�  C�C�  C���C���C�  C�  C���C�C�  C���C���C�C�C�  C���C�  C�C�C�  C�  C�C�  C���C���C���C�  C�C�  C�C�C�  C���C�C�  C�C�
=C�  C�  C�  C�
=C���C���C�  C�  C�
=C�
=C�C�  D   D }qD �qD}qD�D��D�qD}qD�D��D�qD}qD�qD� D  D� D  D� D��D	}qD	�qD
z�D
�qD� D  D� D  D��D�qDz�D��D��DD}qD�qDz�D�qD��D�D� D  D� D�qD}qD�qDz�D  D� D�qD}qD  D}qD��D}qD�D� DD�D  D}qD  D��D�D� D   D }qD!  D!}qD"  D"��D#  D#� D$  D$}qD%  D%� D%�qD&� D'  D'� D(  D(� D(�qD)� D*�D*� D*�qD+}qD,  D,� D-�D-� D.  D.��D/�D/� D0�D0}qD0��D1}qD2�D2� D2��D3}qD4  D4}qD5  D5��D6  D6}qD7  D7� D8�D8� D8�qD9� D:�D:�D;�D;}qD;��D<}qD=  D=��D>�D>��D?  D?� D@�D@�DA�DA� DBDB� DC  DC��DD  DD� DE  DE��DF�DF� DG�DG�DH�DH� DI  DI�DJDJ� DJ�qDK� DL  DL��DM�DM� DN  DN��DN�qDO}qDP�DP��DQ  DQz�DQ�qDR}qDS  DS��DT  DT� DU�DU��DV�DV�DWDW��DX  DX}qDYDY��DZ  DZ� DZ�qD[}qD\  D\� D]  D]��D]�qD^z�D^�qD_}qD`  D`��Da  Da��DbDb��Db�qDc��Dd�Dd}qDe  De��Df�Df� Dg  Dg��DhDh��Di�Di}qDi�qDj}qDj�qDk��Dl�Dl��Dm�Dm}qDn  Dn� Do  Do� Dp�Dp��DqDq��Dr�Dr��Ds�Ds��Ds��Dtz�Du  Du� Du�qDv��Dw  Dwz�Dw�qDx� Dy�Dy}qDy��Dz�D{D{��D|�D|� D|�qD}��D~�D~}qD  D� D��D�B�D��HD��HD��D�B�D��HD��HD���D�=qD�~�D��HD�HD�@ D�� D���D�  D�AHD�� D��qD���D�@ D�� D�� D���D�>�D�~�D��qD�  D�>�D�}qD�� D�HD�AHD��HD�� D��qD�@ D��HD��HD�HD�AHD�~�D���D���D�>�D�� D���D��qD�>�D�� D�� D�HD�@ D�� D��HD�HD�>�D�}qD�� D�HD�AHD���D�� D���D�AHD���D�D�HD�AHD���D�� D�  D�@ D�~�D���D��qD�=qD�~�D�� D���D�=qD�~�D��qD��qD�>�D�� D�� D�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�>�D�}qD�� D�  D�AHD�� D��)D���D�@ D�~�D�� D�HD�AHD���D��HD�  D�>�D�� D�D�  D�=qD�}qD��HD��D�B�D��HD���D���D�@ D��HD���D�  D�@ D��HD�� D���D�AHD�� D��HD��D�AHD�~�D�� D��D�C�D��HD�� D���D�=qD�~�D���D��qD�=qD�~�D�� D�HD�B�D��HD�� D�HD�@ D�~�D��qD�  D�@ D�~�D�� D���D�@ D�� D�� D��D�AHD�~�D���D�  D�>�D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D���D�  D�@ D��HD�D�  D�>�D��HD��HD�  D�>�D�� D��HD�HD�AHD�� D��HD�HD�AHD�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�HD�B�D��HD��HD�  D�>�D�~�D���D�HD�AHD�� D��HD��D�AHD�� D��HD�HD�@ D�~�D�� D��D�B�D��HD��HD�HD�@ D�� D�� D���D�>�D��HD��HD�HD�AHD�� D�� D���D�>�D�~�D�� D���D�@ D���D��HD�  D�AHD�� D��HD�  D�@ D�~�D�� D��D�AHD�}qD½qD�  D�@ DÀ Dþ�D���D�B�DĂ�D��HD�HD�@ D�}qDŽqD�  D�AHDƀ Dƾ�D�  D�B�Dǀ DǾ�D�  D�@ DȀ D��HD���D�>�DɁHD�� D�HD�@ D�}qDʾ�D���D�>�DˁHD��HD�  D�>�D̀ D�� D�HD�AHD͂�D��HD�  D�@ D΀ Dξ�D���D�AHDπ DϾ�D���D�>�D�~�DнqD�  D�AHDсHD��HD�HD�AHDҀ D�� D���D�AHDӁHD�� D�  D�@ DԀ D�� D���D�@ DՀ D�� D�HD�@ Dր D�� D�  D�@ D׀ D׽qD��qD�=qD�}qD�� D�  D�=qD�~�D��HD�  D�>�Dڀ D��HD���D�>�Dۀ D�� D�  D�@ D܀ D��HD�HD�>�D�~�D��HD�  D�>�DށHD�� D�HD�@ D�~�D�� D���D�AHD�� DྸD�  D�@ D�~�D��HD�HD�@ D� D⾸D���D�@ D�~�D�� D�HD�B�D䂏D��HD�HD�@ D� D�� D�  D�@ D� D澸D���D�AHD�HD��HD�  D�AHD�HD�qD���D�@ D�~�D�qD���D�@ D�HD꾸D��qD�@ D�HD��HD�HD�AHD�HD��HD���D�=qD� D�� D���D�AHDDD�  D�@ D� D��HD�HD�AHD���D��HD�HD�AHD�HD��HD�  D�@ D�HD�� D���D�>�D� D��HD�HD�@ D�~�D���D�  D�@ D��HD���D�  D�AHD��HD��HD�  D�AHD��HD�� D�  D�AHD�� D�� D�  D�@ D�� D���D��qD�>�D�� D�D��G�O�?#�
?L��?�  ?��
?Ǯ?�
=?�@\)@(�@(��@@  @W
=@fff@s33@��@�{@�z�@�  @��@��@���@�ff@��@��H@��
@�\)@���A   Az�A�A�RA�\A��A�RA!G�A%A+�A.�RA333A8��A=p�AA�AFffAL(�AP��ATz�AZ=qA^�RAb�\Ag
=Al��Ap��Atz�Az�HA�  A���A�(�A�
=A�G�A��A�A���A��HA��A�  A��HA�z�A�
=A��A��A�p�A�  A��\A�z�A��RA�G�A�33A��A�  A��A��A�ffA�G�A�33A���A�\)A��A��
A�p�A�Q�A�=qA��
A�{Aأ�A��A��
A޸RA�Q�A��A�z�A�RA�  A�\A���A�
=A���A�\A���A�\)A���A�33A��B   BG�B{B
=Bz�B��B=qB�B��B	�B
�HBz�BB�HB�B�B�\B\)BQ�BB
=B�
BG�B�\B33Bz�B�B
=B�
B ��B"=qB#\)B$(�B%G�B&�\B'�B(Q�B)��B+
=B+�
B,��B-�B/
=B/�
B0��B2{B3\)B4(�B5G�B6�RB7\)B8Q�B9B:�RB;�B<��B=�B>�RB?�
BAG�BB=qBB�HBD(�BEp�BFffBG33BHz�BIBJ�\BK\)BL��BM�BN�\BO�BQ�BR{BR�RBTQ�BUG�BV{BW\)BXz�BY�BZffB[�B\(�B]p�B^�\B_�B`z�BaG�Bb�RBc�Bdz�BeBf�HBg�Bh��Bj{Bk33Bl  Bl��BnffBo�Bpz�Bqp�Br�HBt  Bt��Bu�Bw33BxQ�By�BzffB{�B|z�B}p�B~�HB�  B�z�B��HB��B�(�B��\B�
=B��B�=qB��RB�33B��B�ffB��HB�\)B�{B��\B���B��B�(�B��\B�
=B��
B�Q�B��RB�33B��B�z�B���B�p�B�(�B���B��B���B�Q�B��HB��B��B�ffB���B�B�=qB���B�33B��
B�ffB���B�p�B�  B���B�33B���B�{B��RB�G�B�B�=qB���B�p�B�  B�ffB���B�p�B�{B�z�B���B�p�B�(�B��RB��B���B�=qB��HB�G�B��
B�z�B��B���B�{B��\B�33B��
B�ffB���B�p�B�(�B���B�
=B��B�Q�B��RB�G�B�  B��\B���B���B�=qB���B�33B��
B��\B��B��B�(�B��HB�p�B��
B�z�B�33B���B�(�B��HB��B�  B�z�B�
=B�B�Q�B��RB�G�B�  B\B�
=BÙ�B�Q�B���B�\)B�  BƸRB�\)B�B�=qB���Bə�B�=qBʸRB�33BˮB�Q�B�
=BͅB��B�ffB��B�B�Q�BиRB�G�B�  Bҏ\B�
=BӅB�{BԸRB�G�B�B�=qBָRB�\)B��B؏\B��BمB�{BڸRB�p�B��B�ffB��HB݅B�(�B޸RB��Bߙ�B�Q�B���B�p�B��
B�ffB��B�B�=qB��B�33B��
B�ffB�
=B�p�B��
B�z�B��B�B��B�z�B���B�B�(�B��B��B홚B�=qB���B��BB�(�B�RB�
=B�B�  B�z�B��B�B��B�Q�B�
=B��B��B�=qB���B�\)B�B�(�B���B�33B��B�ffB���B�G�B��
B�ffB���B�p�B��B�Q�B���B��C   C =qC z�C �RC  C\)C��C�
C�Cp�C��C��C=qC�C�
C�Cp�C�RC�C(�Cz�C��C{CQ�C�\C��C{Cp�C�RC��C(�Cz�CC	�C	Q�C	�C	�
C
�C
p�C
�RC
�C(�Cp�CC{C\)C��C�
C{CQ�C��C  CG�C�C�RC  CG�C��C�C(�CffC��C�C=qC�\C��C
=CG�C�\C�HC33Cz�C�C�C(�Cp�C�RC{CQ�C�CC
=CffC�C��C(�Cp�CC�Cp�C�RC�C=qC��C�C(�CffCC�Cz�C�RC��CG�C�C  CQ�C�\C�
C33C�\C�
C{C\)CC �C ffC �C ��C!G�C!��C!��C"G�C"�\C"��C#�C#z�C#�HC$�C$ffC$C%(�C%z�C%�HC&(�C&p�C&��C'33C'�\C'�C(=qC(�C(�HC)G�C)��C*  C*=qC*�\C*��C+\)C+C,{C,\)C,��C-  C-\)C-C.{C.ffC.�C/  C/\)C/�RC0�C0p�C0�RC1  C1G�C1�C2
=C2ffC2�C3  C3Q�C3��C4  C4ffC4�RC5  C5G�C5��C5�C6Q�C6��C6��C733C7�\C7�
C833C8�\C8�HC933C9p�C9�RC:  C:Q�C:��C:�C;=qC;�C;C;�C<(�C<p�C<�RC<��C=(�C=Q�C=p�C=��C=�HC>{C>=qC>\)C>z�C>��C>��C?  C?=qC?ffC?�\C?��C?�HC@{C@G�C@z�C@��C@C@�HCA
=CA=qCAz�CA��CA�
CA��CB{CB=qCBffCB��CB��CC
=CC(�CCG�CCp�CC��CC�
CD
=CD=qCD\)CDz�CD�CD�CE�CE=qCE\)CE�\CE��CF  CF(�CFQ�CFz�CF��CFCF��CG33CG\)CGz�CG��CGCG�CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                     @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��@��@��@֧G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��`A��mA��mA��mA��yA��yA��A��A��A��A��A��mA��yA��mA��TA��`A��`A��A��A��A��A��/A���A���A�ƨAܺ^A܋DA�ZA�%A���Aۏ\A�=qA��yAں^Aڟ�AړuA�jA��A��`Aٟ�A��mA�n�A�dZA�1'A�K�A�dZAգ�A�JA�M�A���A�p�AҲ-AѰ!A�S�A�5?A��TA�&�A�
=A�"�A���A�1'AɃA��A��A�;dA�`BA�?}A�bNA�A�O�A�t�A��#A�;dA�\)A��
A��;A��TA�5?A�;dA���A���A��!A��A��FA�t�A�A�A�z�A��A�p�A���A�?}A�ĜA�^5A���A���A��9A�A�\)A���A��A�
=A�^5A���A�1'A�ZA�1'A�5?A��A�bNA���A�$�A�~�A��uA��RA�p�A���A��7A�p�A�A}�A{Ay
=At��Aq�-Ao"�AmXAiC�Ag�7Ae%Ab�yAa��A_XA]
=AZ��AX��AWS�AV�AR��AQ%AP��AOC�AL{AJ��AI�AH�AG�AF�\AF �AD=qABȴABE�A@��A>r�A;�;A8n�A5��A4jA2jA1�7A1%A/t�A.��A.A�A-A-7LA*1'A)��A'`BA$��A"�9A!�7A ^5A�/A�A��A=qA�hA�+Ar�AC�AS�A��A��AĜA?}A�yAbA�FAdZA��Ax�A
bA	|�A�HA~�A�A�AA�AbNA1A�^AXA�HA$�A�-A�yAffAAV@�dZ@��9@���@���@�n�@�$�@�@���@�+@���@��@�+@�@�j@�  @�S�@�{@�$�@�X@�&�@��/@�@��@�G�@��@���@�S�@�^5@�$�@�-@޸R@���@�x�@ᙚ@��@�Ĝ@��u@�bN@�I�@�I�@���@�t�@�;d@�\)@�C�@ޏ\@��@��@���@���@�\)@�33@ڧ�@�~�@��y@�@�G�@�7L@���@ؼj@ش9@�1'@���@���@���@և+@ղ-@�V@��@��`@ԛ�@���@Ӆ@��@җ�@��@�`B@�%@�9X@ύP@��H@�5?@�@�`B@�V@���@��`@̣�@�z�@�1@�\)@��@�~�@���@Ɂ@���@�Ĝ@�9X@��
@ǥ�@ǅ@�;d@Ƈ+@�=q@��#@š�@Ł@���@� �@Å@�S�@�"�@��y@�ȴ@§�@�$�@��^@�7L@��@��j@�9X@���@��;@���@��@���@�n�@�M�@�5?@��@�{@���@���@�@��7@�V@�bN@��m@��
@�ƨ@�|�@���@�^5@�=q@�-@�{@���@��T@���@�hs@�%@��j@�I�@�S�@�l�@�l�@��@��^@��@�K�@��@��@���@�l�@�+@��@���@���@�n�@�@�&�@��@�/@�&�@�Z@���@��w@��@�bN@�  @�S�@��@���@��!@�v�@�hs@�/@�/@�?}@��j@�  @��;@��@�S�@���@���@��T@�?}@��@���@�A�@���@��F@�t�@�o@��y@��R@��R@�v�@�J@���@��@�p�@��/@�Ĝ@��u@�Z@��m@�l�@���@���@�~�@�=q@�{@���@�`B@��j@�r�@�j@�Q�@�1@�  @��;@��w@�K�@�@�ȴ@�n�@��T@�X@���@��u@�9X@��;@��@��@�|�@�t�@�S�@�;d@�@���@�V@�5?@���@��-@���@��h@�&�@�%@��@��`@���@��D@�Q�@��@���@�\)@�@��@��!@���@�E�@��@���@��@�?}@��@��/@�9X@���@���@�V@�=q@�J@�`B@��/@��D@�r�@�b@��F@���@�l�@��@���@��\@�v�@�V@��@��-@�p�@��@���@��D@�Z@�(�@��@�1@��m@���@���@�t�@�C�@��@���@��@���@���@���@�/@��9@�r�@�  @��
@���@�;d@�@��y@��+@�@��7@�p�@�`B@�O�@�/@��j@�Z@�b@��;@��F@��@�"�@�
=@���@��y@�ȴ@��\@�^5@�$�@�@���@��-@���@��h@��@��@���@��9@�z�@�b@��@l�@K�@;d@~��@~�@~��@~v�@~V@}�T@}/@|9X@{�m@{ƨ@{S�@z��@zJ@y�@wl�@vV@u�-@u�@u`B@u�@t(�@s�
@s��@sdZ@r��@r=q@q��@q�#@q��@qhs@p��@p��@pbN@p1'@o��@o�@ol�@o�@nv�@m�h@mp�@mO�@l�/@lI�@k�m@k��@k��@k�F@k�F@k�@ko@j^5@i�@i��@ix�@i7L@i%@h�@g�;@f�R@e@e/@e�@e�@e�@d�/@d��@dz�@dj@dj@dZ@d(�@c��@c"�@b~�@bM�@b=q@a7L@`Ĝ@`A�@`1'@_�;@_�P@^��@^ȴ@^��@^ff@^{@]�-@]O�@\��@\j@\9X@\1@[�m@[��@[C�@Z�@Z��@Z��@Z-@Yhs@X��@X�9@Xr�@W\)@Vv�@VE�@U��@U`B@T�@T��@T1@SdZ@S33@S33@S"�@RM�@RJ@Q�#@Qx�@P��@PQ�@O�P@N�R@N�+@Nv�@N{@MV@L�j@L�D@LZ@K�m@K��@KS�@K33@J�H@J~�@JM�@I��@I�7@H�`@Hr�@H1'@G�w@Gl�@F��@F�+@F5?@E��@E?}@D��@DI�@D9X@D9X@D(�@C�m@CdZ@Co@B��@Bn�@BM�@BJ@B-@A�7@A7L@A%@@�9@@1'@?�@>�R@>@=@=�-@=O�@<��@<��@<�D@<I�@;��@;�@:�!@:n�@9��@9�7@8�`@8Q�@8  @7��@7�@7�P@7K�@7�@6�R@6�+@6�+@6v�@6E�@5��@5?}@4�D@4j@4j@4j@4I�@4�@3��@3��@3�m@3��@3C�@3o@2��@2~�@2^5@2�@1��@1��@1x�@1&�@1�@0�`@0�u@/�@/�P@/
=@.ȴ@.��@.�+@.ff@.V@.E�@.5?@.{@-�@-��@-��@-V@,�j@,z�@,(�@+��@+C�@+o@+@*�H@*�!@*��@*=q@)��@)7L@(�`@(�`@(��@(1'@'�@'�w@'+@&�y@&ȴ@&��@&v�@&v�@&5?@%�h@%`B@%?}@%V@$�@$�/@$�@$�@$j@$�@$�@$1@#��@#��@#�
@#�F@#��@#dZ@"�@"��@"��@"^5@"=q@!��@!�^@!x�@!X@!G�@!7L@!&�@ �`@ �u@ Q�@ 1'@�;@�w@�w@�@��@|�@
=@��@v�@E�@{@�@�T@�-@��@�@`B@/@��@��@�D@Z@I�@I�@I�@��@�
@��@��@S�@@��@��@��@�!@^5@-@��@�^@��@��@hs@&�@�`@Ĝ@��@�u@�@r�@Q�@1'@b@�@��@�@l�@K�@��@ȴ@��@��@v�@ff@E�@$�@@�T@@��@�@O�@?}@�@�@Z@I�@(�@�m@��@�@t�@C�@"�@"�@o@��@��@^5@-@-@J@��@��@��@hs@G�@&�@�@�`@Ĝ@�u@bN@A�@ �@�;@�@�P@l�@;d@+@
=@��@�@�+@E�@E�@$�@@�@��@��@�@O�@V@��@�@z�@9X@(�@(�@1@�m@ƨ@��@S�@C�A��A��A��
A��#A��#A���A��A��/A��
A���A��#A��`A��`A��TA��mA��yA��`A��`A��yA��mA��`A��mA��A��yA��`A��`A��A��mA��mA��A��yA��`A��A��A��mA��yA��A��yA��yA��A��A��yA��A��A��mA��yA��A��A��yA��A��A��A��yA��A��A��yA��yA��A��A��A��yA��yA��mA��TA��`A��A��yA��mA��A��A��mA��yA��A��mA��yA��yA��mA��;A��TA��HA��HA��TA��`A��mA��TA��HA��`A��TA��TA��yA��yA��TA��mA��mA��`A��HA��`A��`A��HA��`A��mA��yA��yA��A��A��A��mA��mA��A��A��mA��mA��A��A��A��A��A��A��A��A���A��A��A���A���A���A��A��A��A��A��`A��mA��mA���A��;A��
A���A���A���A���A���A���A��
A��
A���A���A���A���A�ȴA���A���A�ȴA�ƨA���A���A�ȴA�ĜA�ƨA�ĜAܼjAܼjA�ĜAܾwAܼjAܸRAܲ-AܮAܮAܮAܝ�A�t�A܋DA�x�A�dZA�bNA�hsA�ffA�`BA�dZA�`BA�E�A�/A�1'A�$�A��A�oA��yA��;A��#A��;A��/A��A��A��A�ƨA�ĜA�ƨA���AۼjA۴9Aۡ�A�x�A�p�A�hsA�dZA�\)A�^5A�`BA�K�A�9XA� �A��A�A���A��A��A��TA��/A��;A��;A���A�ĜA�ƨA���Aں^Aڧ�AڅAڋDAڍPAڕ�Aڣ�Aک�Aڰ!Aڲ-AڬAڥ�Aڡ�Aڟ�Aڕ�AڃA�z�A�x�A�v�A�n�A�l�A�p�A�l�A�dZA�ZA�G�A�33A�"�A�bA�JA�%A�A�A�A���A���A���A��/A���Aٺ^Aٺ^AٸRAٶFAٰ!Aٟ�AكA�z�A�`BA�9XA��A���A���AجA؏\A؁A�z�A�v�A�l�A�jA�l�A�l�A�hsA�dZA�dZA�hsA�hsA�dZA�^5A�`BA�^5A�VA�=qA�-A�-A�+A� �A�VA��Aו�A�`BA�I�A�5?A�bA��mA�ƨA֡�A։7A�v�A�K�A�-A�{A��A��A���AոRA՛�AՅA�`BA�S�A�G�A�5?A��A�bA�A���AԸRAԩ�A�\)A�?}A�33A�1'A�(�A� �A��A��A�
=A���A��`A���AӶFAӟ�AӇ+A�x�A�ffA�Q�A�C�A�?}A�/A�1A��;Aҕ�A�z�A�K�A�&�A�JA��mA���Aщ7A�l�A�ffA�\)A�XA�S�A�Q�A�S�A�S�A�O�A�K�A�I�A�I�A�G�A�;dA�1'A��A�%A���A��A��A��mA��;A���Aа!AЙ�AЇ+A�XA��A��;A���A�Aϥ�A�r�A�A�A�{A��AθRA΋DA�I�A��A�l�A�"�A��A��;Ḁ�A��A˛�A�VA�JA���Aʣ�A�x�A�z�A�n�A�XA�G�A�A�A�9XA�%A��A��A��yA���AɬAɗ�A�v�A�7LA���A���A�hsA�;dA�-A��A�A��TA���Aǧ�AǁA�XA�"�A��/AƟ�A�x�A�K�A�/A�VA�A��yAũ�A�(�AĲ-A�1'A�l�A�1A�%A�ƨA+A�=qA���A���A�?}A��TA�v�A�`BA�"�A�A��A��HA��RA�l�A�bNA�ZA�VA�VA�Q�A�K�A�?}A�5?A�%A��A��mA��
A��FA���A��\A�p�A�S�A�=qA�"�A��A�ȴA��FA���A�t�A�XA�?}A�1'A�"�A��A�JA�A���A���A���A��\A��+A��A�t�A�`BA�&�A���A��RA��A�x�A�v�A�p�A�VA�G�A�5?A�+A��A���A��yA�ĜA��RA��A��hA�x�A�I�A�bA���A���A�\)A�1'A��A�JA��A��/A���A��jA��A��PA�G�A�A��A�A��A�A�ĜA�dZA�33A��A��wA�G�A���A�bNA�+A���A��wA��hA�E�A��mA���A���A�  A�-A��A��yA��TA���A��^A���A���A���A��uA�jA�VA�33A��A�  A��A��#A���A�ȴA���A��9A���A��uA��7A�~�A�l�A�"�A��mA�\)A�t�A��wA� �A��HA��wA��!A���A���A���A��uA�C�A��
A�=qA��!A�z�A�M�A�JA��mA��^A��A��A�A�A�/A�JA��TA��!A���A�p�A�S�A�+A��yA�S�A�bA��#A���A�ffA�+A��A���A�x�A�hsA�bNA�M�A�  A���A���A�jA�A�A�A��wA���A��+A�hsA�O�A�JA��/A���A�n�A�;dA�(�A�1A��
A�ĜA��A��PA�v�A�n�A�M�A�I�A�VA�oA��A��#A���A��9A���A��A��A��A�|�A�r�A�M�A�I�A�1'A�&�A��A��^A���A�XA�33A���A���A�`BA�5?A�$�A�1A��A���A���A�`BA�G�A�=qA�/A��A���A��A��`A��HA��#A���A���A���A�p�A�S�A�+A�JA��TA���A���A�=qA���A�O�A�I�A�;dA�$�A��
A���A�VA�"�A���A�9XA�JA���A��yA���A��jA���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                     A��A��A��A��`A��mA��mA��mA��yA��yA��A��A��A��A��A��mA��yA��mA��TA��`A��`A��A��A��A��A��/A���A���A�ƨAܺ^A܋DA�ZA�%A���Aۏ\A�=qA��yAں^Aڟ�AړuA�jA��A��`Aٟ�A��mA�n�A�dZA�1'A�K�A�dZAգ�A�JA�M�A���A�p�AҲ-AѰ!A�S�A�5?A��TA�&�A�
=A�"�A���A�1'AɃA��A��A�;dA�`BA�?}A�bNA�A�O�A�t�A��#A�;dA�\)A��
A��;A��TA�5?A�;dA���A���A��!A��A��FA�t�A�A�A�z�A��A�p�A���A�?}A�ĜA�^5A���A���A��9A�A�\)A���A��A�
=A�^5A���A�1'A�ZA�1'A�5?A��A�bNA���A�$�A�~�A��uA��RA�p�A���A��7A�p�A�A}�A{Ay
=At��Aq�-Ao"�AmXAiC�Ag�7Ae%Ab�yAa��A_XA]
=AZ��AX��AWS�AV�AR��AQ%AP��AOC�AL{AJ��AI�AH�AG�AF�\AF �AD=qABȴABE�A@��A>r�A;�;A8n�A5��A4jA2jA1�7A1%A/t�A.��A.A�A-A-7LA*1'A)��A'`BA$��A"�9A!�7A ^5A�/A�A��A=qA�hA�+Ar�AC�AS�A��A��AĜA?}A�yAbA�FAdZA��Ax�A
bA	|�A�HA~�A�A�AA�AbNA1A�^AXA�HA$�A�-A�yAffAAV@�dZ@��9@���@���@�n�@�$�@�@���@�+@���@��@�+@�@�j@�  @�S�@�{@�$�@�X@�&�@��/@�@��@�G�@��@���@�S�@�^5@�$�@�-@޸R@���@�x�@ᙚ@��@�Ĝ@��u@�bN@�I�@�I�@���@�t�@�;d@�\)@�C�@ޏ\@��@��@���@���@�\)@�33@ڧ�@�~�@��y@�@�G�@�7L@���@ؼj@ش9@�1'@���@���@���@և+@ղ-@�V@��@��`@ԛ�@���@Ӆ@��@җ�@��@�`B@�%@�9X@ύP@��H@�5?@�@�`B@�V@���@��`@̣�@�z�@�1@�\)@��@�~�@���@Ɂ@���@�Ĝ@�9X@��
@ǥ�@ǅ@�;d@Ƈ+@�=q@��#@š�@Ł@���@� �@Å@�S�@�"�@��y@�ȴ@§�@�$�@��^@�7L@��@��j@�9X@���@��;@���@��@���@�n�@�M�@�5?@��@�{@���@���@�@��7@�V@�bN@��m@��
@�ƨ@�|�@���@�^5@�=q@�-@�{@���@��T@���@�hs@�%@��j@�I�@�S�@�l�@�l�@��@��^@��@�K�@��@��@���@�l�@�+@��@���@���@�n�@�@�&�@��@�/@�&�@�Z@���@��w@��@�bN@�  @�S�@��@���@��!@�v�@�hs@�/@�/@�?}@��j@�  @��;@��@�S�@���@���@��T@�?}@��@���@�A�@���@��F@�t�@�o@��y@��R@��R@�v�@�J@���@��@�p�@��/@�Ĝ@��u@�Z@��m@�l�@���@���@�~�@�=q@�{@���@�`B@��j@�r�@�j@�Q�@�1@�  @��;@��w@�K�@�@�ȴ@�n�@��T@�X@���@��u@�9X@��;@��@��@�|�@�t�@�S�@�;d@�@���@�V@�5?@���@��-@���@��h@�&�@�%@��@��`@���@��D@�Q�@��@���@�\)@�@��@��!@���@�E�@��@���@��@�?}@��@��/@�9X@���@���@�V@�=q@�J@�`B@��/@��D@�r�@�b@��F@���@�l�@��@���@��\@�v�@�V@��@��-@�p�@��@���@��D@�Z@�(�@��@�1@��m@���@���@�t�@�C�@��@���@��@���@���@���@�/@��9@�r�@�  @��
@���@�;d@�@��y@��+@�@��7@�p�@�`B@�O�@�/@��j@�Z@�b@��;@��F@��@�"�@�
=@���@��y@�ȴ@��\@�^5@�$�@�@���@��-@���@��h@��@��@���@��9@�z�@�b@��@l�@K�@;d@~��@~�@~��@~v�@~V@}�T@}/@|9X@{�m@{ƨ@{S�@z��@zJ@y�@wl�@vV@u�-@u�@u`B@u�@t(�@s�
@s��@sdZ@r��@r=q@q��@q�#@q��@qhs@p��@p��@pbN@p1'@o��@o�@ol�@o�@nv�@m�h@mp�@mO�@l�/@lI�@k�m@k��@k��@k�F@k�F@k�@ko@j^5@i�@i��@ix�@i7L@i%@h�@g�;@f�R@e@e/@e�@e�@e�@d�/@d��@dz�@dj@dj@dZ@d(�@c��@c"�@b~�@bM�@b=q@a7L@`Ĝ@`A�@`1'@_�;@_�P@^��@^ȴ@^��@^ff@^{@]�-@]O�@\��@\j@\9X@\1@[�m@[��@[C�@Z�@Z��@Z��@Z-@Yhs@X��@X�9@Xr�@W\)@Vv�@VE�@U��@U`B@T�@T��@T1@SdZ@S33@S33@S"�@RM�@RJ@Q�#@Qx�@P��@PQ�@O�P@N�R@N�+@Nv�@N{@MV@L�j@L�D@LZ@K�m@K��@KS�@K33@J�H@J~�@JM�@I��@I�7@H�`@Hr�@H1'@G�w@Gl�@F��@F�+@F5?@E��@E?}@D��@DI�@D9X@D9X@D(�@C�m@CdZ@Co@B��@Bn�@BM�@BJ@B-@A�7@A7L@A%@@�9@@1'@?�@>�R@>@=@=�-@=O�@<��@<��@<�D@<I�@;��@;�@:�!@:n�@9��@9�7@8�`@8Q�@8  @7��@7�@7�P@7K�@7�@6�R@6�+@6�+@6v�@6E�@5��@5?}@4�D@4j@4j@4j@4I�@4�@3��@3��@3�m@3��@3C�@3o@2��@2~�@2^5@2�@1��@1��@1x�@1&�@1�@0�`@0�u@/�@/�P@/
=@.ȴ@.��@.�+@.ff@.V@.E�@.5?@.{@-�@-��@-��@-V@,�j@,z�@,(�@+��@+C�@+o@+@*�H@*�!@*��@*=q@)��@)7L@(�`@(�`@(��@(1'@'�@'�w@'+@&�y@&ȴ@&��@&v�@&v�@&5?@%�h@%`B@%?}@%V@$�@$�/@$�@$�@$j@$�@$�@$1@#��@#��@#�
@#�F@#��@#dZ@"�@"��@"��@"^5@"=q@!��@!�^@!x�@!X@!G�@!7L@!&�@ �`@ �u@ Q�@ 1'@�;@�w@�w@�@��@|�@
=@��@v�@E�@{@�@�T@�-@��@�@`B@/@��@��@�D@Z@I�@I�@I�@��@�
@��@��@S�@@��@��@��@�!@^5@-@��@�^@��@��@hs@&�@�`@Ĝ@��@�u@�@r�@Q�@1'@b@�@��@�@l�@K�@��@ȴ@��@��@v�@ff@E�@$�@@�T@@��@�@O�@?}@�@�@Z@I�@(�@�m@��@�@t�@C�@"�@"�@o@��@��@^5@-@-@J@��@��@��@hs@G�@&�@�@�`@Ĝ@�u@bN@A�@ �@�;@�@�P@l�@;d@+@
=@��@�@�+@E�@E�@$�@@�@��@��@�@O�@V@��@�@z�@9X@(�@(�@1@�m@ƨ@��@S�G�O�A��A��A��
A��#A��#A���A��A��/A��
A���A��#A��`A��`A��TA��mA��yA��`A��`A��yA��mA��`A��mA��A��yA��`A��`A��A��mA��mA��A��yA��`A��A��A��mA��yA��A��yA��yA��A��A��yA��A��A��mA��yA��A��A��yA��A��A��A��yA��A��A��yA��yA��A��A��A��yA��yA��mA��TA��`A��A��yA��mA��A��A��mA��yA��A��mA��yA��yA��mA��;A��TA��HA��HA��TA��`A��mA��TA��HA��`A��TA��TA��yA��yA��TA��mA��mA��`A��HA��`A��`A��HA��`A��mA��yA��yA��A��A��A��mA��mA��A��A��mA��mA��A��A��A��A��A��A��A��A���A��A��A���A���A���A��A��A��A��A��`A��mA��mA���A��;A��
A���A���A���A���A���A���A��
A��
A���A���A���A���A�ȴA���A���A�ȴA�ƨA���A���A�ȴA�ĜA�ƨA�ĜAܼjAܼjA�ĜAܾwAܼjAܸRAܲ-AܮAܮAܮAܝ�A�t�A܋DA�x�A�dZA�bNA�hsA�ffA�`BA�dZA�`BA�E�A�/A�1'A�$�A��A�oA��yA��;A��#A��;A��/A��A��A��A�ƨA�ĜA�ƨA���AۼjA۴9Aۡ�A�x�A�p�A�hsA�dZA�\)A�^5A�`BA�K�A�9XA� �A��A�A���A��A��A��TA��/A��;A��;A���A�ĜA�ƨA���Aں^Aڧ�AڅAڋDAڍPAڕ�Aڣ�Aک�Aڰ!Aڲ-AڬAڥ�Aڡ�Aڟ�Aڕ�AڃA�z�A�x�A�v�A�n�A�l�A�p�A�l�A�dZA�ZA�G�A�33A�"�A�bA�JA�%A�A�A�A���A���A���A��/A���Aٺ^Aٺ^AٸRAٶFAٰ!Aٟ�AكA�z�A�`BA�9XA��A���A���AجA؏\A؁A�z�A�v�A�l�A�jA�l�A�l�A�hsA�dZA�dZA�hsA�hsA�dZA�^5A�`BA�^5A�VA�=qA�-A�-A�+A� �A�VA��Aו�A�`BA�I�A�5?A�bA��mA�ƨA֡�A։7A�v�A�K�A�-A�{A��A��A���AոRA՛�AՅA�`BA�S�A�G�A�5?A��A�bA�A���AԸRAԩ�A�\)A�?}A�33A�1'A�(�A� �A��A��A�
=A���A��`A���AӶFAӟ�AӇ+A�x�A�ffA�Q�A�C�A�?}A�/A�1A��;Aҕ�A�z�A�K�A�&�A�JA��mA���Aщ7A�l�A�ffA�\)A�XA�S�A�Q�A�S�A�S�A�O�A�K�A�I�A�I�A�G�A�;dA�1'A��A�%A���A��A��A��mA��;A���Aа!AЙ�AЇ+A�XA��A��;A���A�Aϥ�A�r�A�A�A�{A��AθRA΋DA�I�A��A�l�A�"�A��A��;Ḁ�A��A˛�A�VA�JA���Aʣ�A�x�A�z�A�n�A�XA�G�A�A�A�9XA�%A��A��A��yA���AɬAɗ�A�v�A�7LA���A���A�hsA�;dA�-A��A�A��TA���Aǧ�AǁA�XA�"�A��/AƟ�A�x�A�K�A�/A�VA�A��yAũ�A�(�AĲ-A�1'A�l�A�1A�%A�ƨA+A�=qA���A���A�?}A��TA�v�A�`BA�"�A�A��A��HA��RA�l�A�bNA�ZA�VA�VA�Q�A�K�A�?}A�5?A�%A��A��mA��
A��FA���A��\A�p�A�S�A�=qA�"�A��A�ȴA��FA���A�t�A�XA�?}A�1'A�"�A��A�JA�A���A���A���A��\A��+A��A�t�A�`BA�&�A���A��RA��A�x�A�v�A�p�A�VA�G�A�5?A�+A��A���A��yA�ĜA��RA��A��hA�x�A�I�A�bA���A���A�\)A�1'A��A�JA��A��/A���A��jA��A��PA�G�A�A��A�A��A�A�ĜA�dZA�33A��A��wA�G�A���A�bNA�+A���A��wA��hA�E�A��mA���A���A�  A�-A��A��yA��TA���A��^A���A���A���A��uA�jA�VA�33A��A�  A��A��#A���A�ȴA���A��9A���A��uA��7A�~�A�l�A�"�A��mA�\)A�t�A��wA� �A��HA��wA��!A���A���A���A��uA�C�A��
A�=qA��!A�z�A�M�A�JA��mA��^A��A��A�A�A�/A�JA��TA��!A���A�p�A�S�A�+A��yA�S�A�bA��#A���A�ffA�+A��A���A�x�A�hsA�bNA�M�A�  A���A���A�jA�A�A�A��wA���A��+A�hsA�O�A�JA��/A���A�n�A�;dA�(�A�1A��
A�ĜA��A��PA�v�A�n�A�M�A�I�A�VA�oA��A��#A���A��9A���A��A��A��A�|�A�r�A�M�A�I�A�1'A�&�A��A��^A���A�XA�33A���A���A�`BA�5?A�$�A�1A��A���A���A�`BA�G�A�=qA�/A��A���A��A��`A��HA��#A���A���A���A�p�A�S�A�+A�JA��TA���A���A�=qA���A�O�A�I�A�;dA�$�A��
A���A�VA�"�A���A�9XA�JA���A��yA���A��jA���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                     ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
�?B
�B
�FB
�B
�zB
�FB
��B
�zB
�FB
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�LB
��B
�<B
��B
��B
�tB
�^B
�B
� B
�vB
�)B
�B
�B
�pB
��B
�<B
�wB
�6B
�XB
�-B
�XB
�-B
��B
��B
�:B
��B
�FB
�@B
��B
��B
�B
�B
��B
��B
��B
��B
�RB
��B
�5BJB:�BV9B`vBe,BoiB|�B��B�FB�B�B�B�}B��B��B�aB�B�|B�DB�WB�B�vB��B�nB��B�=B�oBw�B�iB��B�xB�7B��B�CB��B�B�7BncB]�B]/B[WBR�BE�B5�B(�B�B�BB
��B
�B
ȀB
�B
��B
�bB
�xB
��B
{B
o�B
W�B
>wB
-�B
 \B
�B	��B	�B	�fB	�]B	ҽB	�gB	��B	��B	�:B	�B	�B	��B	�oB	}VB	sB	ncB	lWB	c�B	`BB	\]B	\)B	U�B	OvB	I�B	G�B	<6B	4nB	&�B	 �B	eB	�B	
rB		7B	1B	AB	�B�.B�]B�xB�MB�GB�B�yB�
B�B��B�B�B�cB�B��B�)B�"B�B�yB�B�cB�vB	�B	�B	 �B��B�"B��B	B�cB�"B�PB�.B	.B	�B	�B	$B	�B	�B	B	B	�B	�B	YB	�B	�B		B	�B	FB	PB		lB	�B	�B	"4B	VB	B	-CB	5?B	<B	A�B	B�B	C-B	E�B	J�B	VB	\)B	a�B	d&B	e,B	ffB	c�B	kB	n�B	m�B	o5B	o�B	s�B	{�B	��B	��B	�bB	�bB	��B	��B	��B	�B	�OB	��B	�UB	�9B	�6B	�HB	B	�KB	�jB	ӏB	�B	ӏB	�yB	�yB	��B	�;B	��B	�B	�B	�2B	�B	�B	�B	�/B	�/B	��B	�AB	�TB	�B	��B	�`B	�`B	��B	��B	��B	��B	�>B	��B	�xB	��B	�]B	��B
AB
GB
�B
MB
B
SB
B
MB
�B
�B
�B
	B
�B

	B

�B

�B
�B
�B
~B
~B
~B
~B
�B
�B
�B
�B
VB
�B
�B
�B
�B
4B
�B
�B
B
uB
uB
�B
{B
�B
B
�B
�B
�B
YB
YB
�B
�B
�B
�B
+B
_B
+B
�B
�B
B
kB
7B
kB
=B
�B
�B
xB
CB
xB
xB
CB
B
qB
�B
=B
kB
$B
+B
�B
_B
B
hB
PB
B
+B
�B
�B
xB
�B
�B
B
IB
IB
B
xB
�B
~B
CB
7B
�B
B
VB
�B
!�B
!bB
!bB
!-B
 �B
!�B
 'B
 'B
�B
%�B
%FB
&B
&LB
&LB
$�B
$�B
%�B
$B
#�B
$B
$tB
$tB
%B
'�B
*�B
+�B
,�B
-wB
/�B
.IB
.IB
/OB
/OB
.�B
0�B
2�B
2aB
2�B
3hB
1�B
3�B
49B
5B
4�B
4�B
4�B
5�B
5�B
5tB
5�B
6�B
6�B
6�B
6FB
7�B
7�B
7�B
8�B
9�B
:�B
<B
=<B
>wB
?B
?}B
?�B
?�B
?�B
@�B
A B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
EmB
EB
E�B
F�B
GEB
HB
HB
HB
G�B
H�B
I�B
I�B
I�B
I�B
I�B
JXB
K�B
L0B
J#B
J#B
JXB
J�B
L�B
L�B
LdB
LdB
MjB
MjB
MjB
M�B
OB
NpB
N�B
N�B
OB
O�B
O�B
OvB
PB
O�B
O�B
O�B
OvB
OBB
OBB
O�B
OvB
PB
P�B
P�B
Q�B
Q�B
RTB
R�B
R�B
R�B
S�B
S&B
S&B
S&B
S&B
T�B
W?B
W?B
V�B
XB
X�B
YB
YB
X�B
X�B
YB
Y�B
Z�B
Z�B
Z�B
Z�B
[�B
\�B
\�B
\�B
\�B
\�B
]/B
]dB
]�B
]�B
^5B
^5B
^jB
^�B
^�B
`BB
_�B
`B
aB
c B
c�B
c�B
c�B
dZB
e�B
f2B
f�B
ffB
f�B
ffB
ffB
e�B
e�B
e�B
e�B
e`B
d�B
c�B
aB
_�B
_;B
^�B
`B
`BB
`�B
a|B
aB
aB
aHB
aB
a�B
bNB
bNB
c B
c�B
c�B
dZB
d�B
d�B
d�B
e�B
e�B
g8B
g�B
gmB
g�B
hsB
h�B
i�B
j�B
kQB
k�B
k�B
l�B
n/B
n�B
n�B
n�B
ncB
n�B
ncB
n�B
n/B
o5B
p�B
o�B
p;B
poB
p;B
qAB
q�B
q�B
q�B
q�B
qAB
q�B
r�B
sB
s�B
sMB
sMB
t�B
t�B
tTB
tTB
t�B
t�B
t�B
t�B
u%B
u%B
u�B
v+B
v`B
v�B
v�B
w2B
w2B
w2B
w�B
w�B
xB
w�B
w�B
xB
xB
w�B
xlB
x�B
zDB
zDB
z�B
{B
{JB
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
~]B
~]B
~�B
.B
~�B
cB
�B
cB
cB
cB
�4B
�4B
�4B
�4B
��B
��B
��B
��B
�B
�oB
�;B
�oB
�B
�uB
�uB
��B
�GB
�GB
��B
�B
�B
��B
��B
�B
�B
��B
��B
�B
�SB
��B
�YB
��B
��B
�+B
��B
�B
�rB
�B
��B
��B
��B
�rB
�DB
�DB
�B
��B
��B
�B
�JB
��B
��B
��B
�"B
��B
�(B
��B
��B
�bB
� B
�4B
�hB
�4B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�oB
�@B
�@B
��B
��B
�B
�{B
��B
��B
��B
��B
�B
�SB
�SB
��B
��B
�YB
�YB
�YB
��B
��B
��B
��B
�+B
�_B
�_B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�1B
��B
�B
�7B
�kB
��B
��B
��B
�=B
�=B
��B
��B
�qB
�B
�CB
�xB
�xB
�B
��B
��B
�IB
�B
�B
��B
�OB
�B
�B
�OB
�OB
��B
�!B
�VB
�'B
�'B
��B
�'B
�\B
�\B
�\B
��B
��B
��B
�bB
�-B
��B
��B
��B
�4B
�hB
��B
��B
��B
��B
��B
�B
�nB
��B
��B
�B
�B
�B
�@B
�@B
�@B
�B
�FB
�zB
�zB
��B
��B
��B
�B
�B
�B
�LB
��B
��B
��B
�B
�RB
�RB
�RB
�RB
��B
��B
��B
��B
�$B
��B
��B
��B
��B
��B
�*B
�*B
�_B
�_B
�_B
�_B
��B
��B
�0B
��B
��B
��B
��B
��B
�B
�B
�6B
�6B
�kB
��B
��B
��B
�=B
�=B
�qB
�qB
��B
��B
��B
�B
�CB
�CB
�wB
�wB
�wB
��B
��B
�B
�}B
��B
��B
��B
�B
�OB
��B
��B
��B
��B
��B
��B
�!B
�UB
��B
��B
��B
��B
�'B
�[B
�[B
��B
��B
��B
��B
��B
��B
�-B
�-B
�-B
�aB
��B
��B
��B
�3B
�hB
�hB
��B
��B
�B
�nB
�nB
��B
��B
��B
��B
�B
�B
�?B
�?B
�tB
��B
��B
�B
�zB
�zB
�zB
��B
��B
��B
��B
�LB
�LB
�B
�tB
�FB
��B
�nB
��B
��B
�tB
�B
��B
�B
��B
��B
��B
�tB
�B
��B
�B
��B
��B
��B
��B
�tB
��B
�B
��B
��B
��B
�LB
�B
�zB
��B
��B
�tB
��B
��B
�B
�B
��B
�?B
�FB
�LB
�B
�zB
��B
��B
��B
�B
��B
��B
��B
��B
��B
�B
��B
��B
�B
��B
�FB
�zB
�B
�B
�zB
��B
��B
�tB
�zB
�B
�?B
�FB
��B
�zB
��B
�B
�zB
�B
�B
��B
�zB
��B
��B
��B
��B
�FB
��B
��B
�FB
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
��B
��B
�zB
��B
��B
��B
�FB
�FB
�FB
��B
��B
�zB
�tB
�LB
��B
�B
�?B
�LB
��B
�FB
��B
��B
��B
�tB
�B
��B
�B
�tB
�FB
�RB
�FB
��B
��B
��B
��B
��B
��B
�hB
�B
�*B
�LB
�zB
�tB
��B
�B
�FB
�tB
�B
�zB
��B
��B
�RB
�B
��B
��B
�LB
��B
��B
�B
��B
��B
�zB
��B
��B
�?B
�B
�B
�B
�zB
�B
��B
�?B
��B
��B
��B
��B
�B
�jB
�0B
��B
��B
��B
�6B
��B
��B
�qB
�B
�}B
�aB
��B
��B
ÖB
�[B
B
��B
��B
��B
�?B
�-B
�[B
��B
�gB
��B
�zB
��B
ȴB
��B
�KB
��B
�B
�EB
�B
��B
�jB
��B
ΥB
ϫB
�<B
�6B
�<B
�B
͟B
̘B
��B
�BB
�jB
��B
��B
ٴB
՛B
רB
��B
�B
ܒB
�HB
�yB
��B
�B
��B
�cB
��B
��B
�B
�)B
�QB
�B
�"B
�B
�B
�B
�]B
��B
�B
��B
�B
�&B
�B
�B
�|B
�BB
�B
��B
�B
ߤB
��B
�;B
��B
�B
�]B
��B
�)B
��B
�)B
՛B
�?B
�,B
�aB
�B
�6B
�B
ƨB
B
�B
�wB
��B
�HB
��B
�jB
��B
��B
�B
�dB
�dB
��B
��B
�0B
�*B
��B
�BB
�*B
��B
��B
��B
��B
��B
��B
��B
��B
�IB
��B
��B
��B
�wB
�XB
�FB
�$B
��B
�B
�bB
�B
�4B
�B
�!B
�bB
�tB
��B
��B
�~B
��B
�B
��B
�tB
��B
��B
�B
��B
��B
��B
�!B
��B
�bB
��B
��B
�B
�tB
�tB
��B
��B
��B
�B
�B
��B
�B
�B
�zB
�RB
�zB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�hB
��B
��B
��B
��B
�-B
��B
�'B
��B
��B
�\B
�!B
��B
�'B
�nB
��B
��B
�B
�B
�B
��B
�OB
�IB
��B
��B
�B
��B
��B
�SB
�1B
�!B
��B
�\B
��B
�\B
��B
�hB
�B
�=B
��B
��B
��B
�tB
��B
�aB
�dB
��B
�B
��B
�*B
��B
�CB
��B
��B
�B
�B
�B
B
�}B
��B
�-B
��B
��B
��B
��B
��B
уB
�KB
��B
چB
ٴB
�B
��B
�dB
ݘB
�TB
�&B
��B
�B
�oB
�B
�`B
�+B
��B
��B
��B
�8BMB4BB"�B:^B)�B(XB?}B:�B>�B=BJ�BJ�BU�BN�B\�BU�BVBV�BWsB\�BcTB_�B`�B`vB_�B`vB`�Ba�Bb�Bi�Be`Bd&Bf2Bj�BiBi�Bn/Bm)BpoBsBx�Bv`Bt�By>BcB~�B�AB��B��B��B�SB�B��B��B�JB�"B��B��B��B��B��B�{B��B��B��B��B�qB�!B��B��B��B�hB��B�nB�B��B��B�RB�RB��B��B��B��B��B��B�IB��B��B��B�IB��B��B��B��B�zB��B�B�B�B�aB�tB�[B��B�pB�B��B�B�&B�NB�}B�BB�?B�yB�#B�B��B��B�NB�BߤB�B�B� B�B�B�B�B�B�KB�B�B�B��B�WB�B�QB��B�B�cB�"B�WB�QB��B!�B�B�"B	7B�B�BΥB�B�^BɺB��BɆB�dB��B�B�?B��B��B��B��B�OB�_B�~B�kB�\B�{B�hB��B��B�1B�%B��B�7B�B}�Bv+B|PBy	BwfBy�B~�BxBsBs�BtTB|�Bm]BrB��B��B�AB��B�B��B�uB�+B��B��B��B�=B�1B�+B��B��B��B��B�_B��B�1B��B�eB�B�!B��B��B�B�1B��B��B�B��B�=B�qB�hB��B��B��B�UB�xB��B�YB��B��B�tB�-B�OB�CB�7B��B�OB�:B�	B�+B��B��B�MB�SB�:B�VB�VB��B��B�B��B�(B��B��B��B��B��B�oB}"B��BqBn�Bo�BrGBuZBm�BiDBk�Bx�Bj�Bb�B`BB_�B_�B^�B_�B\�B\)B[WG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021082621263820210826212638IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021090600010920210906000109QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021090600010920210906000109QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365220220126093652IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295420220204232954IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295420220204232954IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295420220204232954IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                