CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-07-11T23:00:32Z creation; 2023-04-26T19:14:26Z DMQC;      
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
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  d`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʨ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ҈   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20180711230032  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_018                 7316_008644_018                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�q8��Q�@�q8��Q�11  @�q9�@�q9�@(��1P��@(��1P���d��J�d��J11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@B�\@�G�@��R@��R@�  A   A\)A\)A*�HA@  A`  A���A�Q�A��A��A��AϮA�  A�Q�A��B�
B(�B(�B (�B((�B0  B8  B?�
BG�
BP  BX(�B`(�Bh(�Bo�
Bw�B�  B�Q�B��B�{B�  B��B��B�  B�  B�  B��B��B�{B�  B��
B��B�  B�  B�  B�  B�{B�  B�  B�  B��
B��B�  B�{B�  B��B��B��C   C
=C  C  C  C	��C��C
=C  C  C{C
=C��C��C  C
=C 
=C"
=C${C&
=C'��C)��C,  C.{C0
=C2
=C3��C5�C7�C9��C;��C=��C@
=CB  CC��CF  CH  CJ  CL{CN{CO��CQ�HCS�CV  CX
=CZ
=C[��C^  C`  Ca��Cd
=Cf
=Ch  Cj{Cl  Cm��Cp  Cq��Cs�Cu��Cx  Cy��C|  C~
=C�  C�C�C�  C�  C�  C�C�C�C�  C�  C�  C�  C�  C�C�  C���C�  C�C�  C�  C�  C�C�C���C�  C�  C�  C�C�
=C�  C���C�  C�C�C�  C�C�  C�  C�  C���C���C���C�  C�
=C�C�  C���C�  C�C�  C���C�C�  C���C�  C�C���C���C�  C�C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�C�
=C�
=C�
=C�
=C�
=C�  C���C�  C�  C�  C�C�C�C�  C�C�  C���C�  C�  C�  C�  C���C���C���C�  C���C���C���C���C�  C�  C�C�C�
=C�C�  C�  C�C�C�
=C�  C���C�C�C�C�  C�  C�  C�  C�C�  C���C���C�  C���C�  D �D ��D�D��D  D}qD�D�D�D� D  D�D  D��DD��D  D}qD��D	� D
�D
� D�D��D�qD� D�D� D  D}qD�D�D�D��D�D� D�D}qD�qD��D�D� D  D}qD  D� D  D��D�D� D��D}qD  D}qD��D}qD�qD��D  D}qD  D�D�D}qD �D � D �qD!}qD!�qD"� D"�qD#z�D#�qD$��D%  D%� D%�qD&}qD&�qD'� D(  D(� D)  D)� D*�D*}qD*��D+}qD,  D,��D,�qD-z�D.  D.��D/�D/z�D/��D0� D1  D1}qD1�qD2}qD3  D3}qD3�qD4��D5  D5� D6  D6��D7  D7� D8�D8� D9�D9��D:D:�D;  D;��D<D<� D<�qD=z�D>  D>��D?  D?��D@D@�DADA�DB�DB}qDB��DCz�DC��DDz�DD�qDE}qDF  DF��DG�DG��DH�DH�DI�DI� DJ  DJ}qDJ�qDK}qDL  DL}qDM  DM��DN  DN}qDO  DO� DP�DP� DP�qDQ� DR�DR��DS�DS� DT  DT��DT�qDU� DV  DV� DW  DW}qDW��DX}qDY  DY� DZ  DZ� D[  D[� D[�qD\}qD]  D]� D^�D^��D_�D_� D_�qD`� Da  Da� Db�Db� Dc  Dc��Dd  Dd� Dd�qDe}qDf  Df}qDg  Dg� Dh  Dh� Di  Di}qDj  Dj��Dk  Dk}qDl  Dl� Dm  Dm��Dn  Dn� Dn�qDo� DpDp��Dq�Dq� Dr  Dr� Ds  Ds� Dt  Dt��Du�Du��Dv�Dv}qDv�qDwz�Dx  Dx� Dx�qDy}qDy�qDz��D{�D{� D{�qD|}qD}�D}��D~  D~� D  D��D��D�AHD�� D���D���D�>�D�� D�� D�HD�AHD��HD�� D�  D�@ D�� D�� D��qD�>�D�� D�� D���D�=qD�~�D��qD���D�@ D�� D���D���D�AHD�� D�� D�  D�@ D�� D���D��qD�>�D�~�D�� D�HD�@ D�~�D���D��qD�=qD�~�D�� D���D�>�D�~�D���D���D�@ D�� D��HD�HD�>�D�}qD�� D�HD�@ D�~�D�� D���D�=qD�~�D�� D��D�AHD��HD��HD�HD�B�D���D��HD�  D�@ D���D��HD�  D�AHD�� D�� D�HD�>�D��HD��HD��D�AHD�� D�� D�  D�AHD��HD�� D�  D�@ D�}qD��qD��qD�>�D���D�� D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��HD�HD�=qD�� D��HD�HD�@ D�� D���D�  D�AHD�� D���D��qD�@ D�� D���D�HD�@ D�~�D�� D��D�@ D�~�D�� D�HD�>�D�~�D��qD���D�@ D��HD�� D���D�@ D��HD�� D��qD�@ D�� D�� D�  D�AHD�� D�� D�HD�@ D�� D��HD�HD�B�D�~�D���D�HD�AHD�� D�� D���D�=qD�� D�D�HD�>�D�� D��HD�  D�>�D�~�D���D�  D�@ D�� D���D���D�>�D�� D���D���D�AHD�� D��HD�HD�@ D��HD�� D���D�>�D�~�D���D���D�>�D�� D��HD�HD�AHD��HD�D�  D�@ D��HD���D�  D�>�D�}qD���D�HD�@ D�~�D�� D�HD�AHD�� D���D���D�AHD��HD�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�>�D�� D�� D�HD�@ D�}qD�� D�  D�@ D�� D���D�  D�@ D�~�D��HD�HD�@ D��HD�� D�  D�@ D�� D���D�  D�AHDHD��HD���D�@ DÀ D�� D�  D�AHDĀ D�� D�HD�@ DŁHD�� D�  D�AHDƁHD��HD�  D�@ Dǀ D��HD�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�B�DʁHD�� D�  D�@ Dˀ D��HD�  D�=qD�}qD̾�D�  D�@ D�~�D�� D�  D�@ D΀ D�� D�HD�@ Dπ D�D�  D�@ DЁHD�� D�  D�AHDр D��HD�HD�AHDҁHD��HD�HD�>�DӀ D�� D���D�AHDԁHD��HD�HD�AHDՁHDվ�D���D�=qD�}qD�� D�HD�>�D�}qD׽qD���D�@ D؁HD�� D�  D�@ Dـ D��HD�HD�AHDڂ�D��HD�HD�AHDہHD��HD�HD�@ D܀ D�� D�  D�@ D݀ D��HD�HD�B�DށHD޾�D���D�AHD߀ D߾�D�  D�AHD�� DྸD�  D�AHD� D�� D���D�>�D�~�D�� D�HD�AHD�HD㾸D�HD�>�D� D�� D���D�>�D� D��HD���D�AHD� D�� D�HD�AHD� D��HD���D�>�D�~�D�� D�HD�@ D�}qD龸D���D�@ D�~�D꾸D��qD�>�D�~�D뾸D�  D�AHD�HD��HD�  D�AHD�~�D���D�HD�>�D�~�D��HD�  D�>�D�HD��HD�  D�>�D�� D�D�  D�AHD�~�D�D���D�AHD� D�� D�HD�AHD� D��HD�  D�>�D� D�� D�HD�>�D�~�D���D�  D�@ D�� D�� D�  D�>�D��HD��HD�HD�>�D�~�D���D�  D�@ D�� D��HD��D�0�?�?\)?W
=?��?���?�p�?�(�?�@�@(�@(��@=p�@L��@c�
@z�H@��@�{@�Q�@��@���@���@��
@�\)@��H@�ff@��@��HAz�A
�HA  A�A�HA   A&ffA+�A0  A5A;�AA�AFffAL(�AQ�AW�A\��Aa�AfffAl(�Aq�AvffAz�HA�  A�=qA�z�A�ffA�  A��A��
A�p�A��RA�Q�A�=qA�(�A�A�
=A���A��\A�z�A�{A�\)A�G�A��A��A��RA�  A�=qA�(�A�{A�\)A�G�A�33A�p�A�\)A���A�33A�p�A�  A��AÅA�A�Q�A�=qA�z�A�ffAУ�A�33A��A�
=A�G�A��
A�{A�  A��A�z�A�RA���A��HA���A�\)A�A�33A��A�\)A���A��
A��A�\)B ��B{B
=B  B��B�B33BQ�B	�B
{B�Bz�Bp�BffB�B��BB�RB�B��B{B
=B  B�BffB33B(�B�BffB�B z�B!p�B"=qB#�B$��B%��B&�\B'�B(��B)B*�\B+�B,��B-B.�HB0  B0��B1B2�RB3�
B4��B5B6�RB7�
B9�B:{B;
=B<  B=G�B>�\B?�B@z�BABB�HBD(�BEG�BF=qBG�BH��BI�BK
=BL(�BM�BN=qBO�BP��BQBR�HBS�
BU�BVffBW\)BXQ�BYp�BZ�\B[�
B\��B]B^�HB_�
Ba�Bb{Bc
=Bd  Be�Bf=qBg\)BhQ�Bi�Bj{Bk
=Bl  Bm�Bn=qBo
=Bo�
Bp��BqBr�HBs�Btz�BuG�Bv=qBw\)Bxz�Byp�BzffB{33B|(�B}G�B~ffB�B�(�B���B�
=B��B��B�ffB��RB�33B�p�B��B��
B�{B�Q�B��\B���B���B�
=B��B�\)B��B��
B�{B�=qB�ffB��RB���B�33B�p�B��B��B�{B�ffB��RB�
=B�G�B��B��B�  B�=qB��\B��HB�33B�p�B�B�  B�=qB��\B���B��B�\)B��B�  B�Q�B���B���B�G�B���B��
B�(�B�ffB���B��HB�33B��B��
B�(�B�z�B��RB��B�\)B��B�  B�=qB�z�B���B��B�p�B�B�{B�ffB���B��HB��B�p�B�B�{B�ffB��RB�
=B�\)B�B�{B�ffB��RB���B�G�B���B��B�(�B�z�B��RB�
=B�G�B���B��
B�=qB�z�B���B�
=B�\)B��B�  B�=qB��\B��HB��B�p�B��B�  B�=qB�z�B��RB��B�\)B��B��B�=qB��\B��HB�33B���B��
B�=qB��\B���B�G�B��B�  B�Q�B��RB���B�\)B��B�  B�ffB��RB��B�p�B��
B�(�B��\B��HB�G�B��B�  B�ffB���B�33B���B��B�ffB���B�33B���B�  B�ffB���B�33B���B�  B�z�B��HB�G�B�B�(�B��\B�
=B�p�B��B�Q�B���B�G�B�B�(�B���B�
=B��B�  B�ffB��HB�G�B��
B�Q�B���B�G�B��
B�=qB���B�G�B��B�(�B£�B��BÅB�  Bď\B��Bř�B�{BƏ\B�
=BǅB�  B�ffB��HB�\)B��
B�ffB��HB�\)B��
B�ffB���B�p�B��B�ffB��HB�\)B��
B�ffB��HB�G�B�B�=qBҸRB�33BӮB�(�Bԣ�B��Bՙ�B�{B�z�B���BׅB��B�z�B���B�p�B�  B�ffB���B�p�B�  B܏\B�
=B݅B�  Bޏ\B�
=B߅B�{B��\B��B�B�=qB���B�\)B�  B�z�B�
=B噚B�{B��B�33B�B�=qB���B�\)B�  B�\B��B�B�Q�B��HB�p�B��B�\B�
=BB�{B��B�33B�B�ffB���B�B�{B���B�33B��B�(�B���B�p�B�  B��\B��B��B�(�B��RB�33B�B�Q�B���B��B�{B��\B��B��C {C ffC �C  CG�C�C�
C{C\)C�C��CG�C��C�HC33Cp�C�RC  CQ�C�C��C33C�C��C{Cp�C�RC��C=qC�\C�HC	=qC	�C	��C
{C
\)C
�RC
=CQ�C��C�HC(�Cz�C��C{CffC��C�C=qC�C�HC33Cz�C��C
=C\)C�C  CQ�C��C�HC33C�\C�HC(�Cp�CC{Cp�C�RC  CQ�C�C  CQ�C�\C�C=qC�\C�C=qC�C��C�Cp�C��C(�Cz�CC{CffCC�Cp�CC{CffCC(�Cz�C��C�Cp�C��C (�C z�C ��C!�C!p�C!��C"(�C"z�C"�
C#(�C#z�C#��C$(�C$�C$�HC%33C%�C%�
C&(�C&�C&�C'=qC'�\C'�HC(33C(�\C(�C)G�C)��C)��C*\)C*�C+
=C+\)C+�C,{C,p�C,�
C-�C-z�C-�HC.G�C.��C.��C/G�C/�C0
=C0p�C0��C1�C1p�C1��C233C2��C2�C3=qC3�\C3��C4Q�C4�C5  C5\)C5�RC6�C6z�C6�
C7(�C7�\C7��C8\)C8�C9
=C9ffC9��C:33C:�\C:�HC;G�C;�C<{C<p�C<��C=(�C=�\C=��C>\)C>�C?{C?z�C?�HC@=qC@�\C@��CA\)CACB�CBp�CB��CC33CC��CD  CD\)CD�CE
=CEp�CE�
CF=qCF��CF��CGQ�CG�RCH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            ?�  @�\@B�\@�G�@��R@��R@�  A   A\)A\)A*�HA@  A`  A���A�Q�A��A��A��AϮA�  A�Q�A��B�
B(�B(�B (�B((�B0  B8  B?�
BG�
BP  BX(�B`(�Bh(�Bo�
Bw�B�  B�Q�B��B�{B�  B��B��B�  B�  B�  B��B��B�{B�  B��
B��B�  B�  B�  B�  B�{B�  B�  B�  B��
B��B�  B�{B�  B��B��B��C   C
=C  C  C  C	��C��C
=C  C  C{C
=C��C��C  C
=C 
=C"
=C${C&
=C'��C)��C,  C.{C0
=C2
=C3��C5�C7�C9��C;��C=��C@
=CB  CC��CF  CH  CJ  CL{CN{CO��CQ�HCS�CV  CX
=CZ
=C[��C^  C`  Ca��Cd
=Cf
=Ch  Cj{Cl  Cm��Cp  Cq��Cs�Cu��Cx  Cy��C|  C~
=C�  C�C�C�  C�  C�  C�C�C�C�  C�  C�  C�  C�  C�C�  C���C�  C�C�  C�  C�  C�C�C���C�  C�  C�  C�C�
=C�  C���C�  C�C�C�  C�C�  C�  C�  C���C���C���C�  C�
=C�C�  C���C�  C�C�  C���C�C�  C���C�  C�C���C���C�  C�C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�C�
=C�
=C�
=C�
=C�
=C�  C���C�  C�  C�  C�C�C�C�  C�C�  C���C�  C�  C�  C�  C���C���C���C�  C���C���C���C���C�  C�  C�C�C�
=C�C�  C�  C�C�C�
=C�  C���C�C�C�C�  C�  C�  C�  C�C�  C���C���C�  C���C�  D �D ��D�D��D  D}qD�D�D�D� D  D�D  D��DD��D  D}qD��D	� D
�D
� D�D��D�qD� D�D� D  D}qD�D�D�D��D�D� D�D}qD�qD��D�D� D  D}qD  D� D  D��D�D� D��D}qD  D}qD��D}qD�qD��D  D}qD  D�D�D}qD �D � D �qD!}qD!�qD"� D"�qD#z�D#�qD$��D%  D%� D%�qD&}qD&�qD'� D(  D(� D)  D)� D*�D*}qD*��D+}qD,  D,��D,�qD-z�D.  D.��D/�D/z�D/��D0� D1  D1}qD1�qD2}qD3  D3}qD3�qD4��D5  D5� D6  D6��D7  D7� D8�D8� D9�D9��D:D:�D;  D;��D<D<� D<�qD=z�D>  D>��D?  D?��D@D@�DADA�DB�DB}qDB��DCz�DC��DDz�DD�qDE}qDF  DF��DG�DG��DH�DH�DI�DI� DJ  DJ}qDJ�qDK}qDL  DL}qDM  DM��DN  DN}qDO  DO� DP�DP� DP�qDQ� DR�DR��DS�DS� DT  DT��DT�qDU� DV  DV� DW  DW}qDW��DX}qDY  DY� DZ  DZ� D[  D[� D[�qD\}qD]  D]� D^�D^��D_�D_� D_�qD`� Da  Da� Db�Db� Dc  Dc��Dd  Dd� Dd�qDe}qDf  Df}qDg  Dg� Dh  Dh� Di  Di}qDj  Dj��Dk  Dk}qDl  Dl� Dm  Dm��Dn  Dn� Dn�qDo� DpDp��Dq�Dq� Dr  Dr� Ds  Ds� Dt  Dt��Du�Du��Dv�Dv}qDv�qDwz�Dx  Dx� Dx�qDy}qDy�qDz��D{�D{� D{�qD|}qD}�D}��D~  D~� D  D��D��D�AHD�� D���D���D�>�D�� D�� D�HD�AHD��HD�� D�  D�@ D�� D�� D��qD�>�D�� D�� D���D�=qD�~�D��qD���D�@ D�� D���D���D�AHD�� D�� D�  D�@ D�� D���D��qD�>�D�~�D�� D�HD�@ D�~�D���D��qD�=qD�~�D�� D���D�>�D�~�D���D���D�@ D�� D��HD�HD�>�D�}qD�� D�HD�@ D�~�D�� D���D�=qD�~�D�� D��D�AHD��HD��HD�HD�B�D���D��HD�  D�@ D���D��HD�  D�AHD�� D�� D�HD�>�D��HD��HD��D�AHD�� D�� D�  D�AHD��HD�� D�  D�@ D�}qD��qD��qD�>�D���D�� D���D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D��HD�HD�=qD�� D��HD�HD�@ D�� D���D�  D�AHD�� D���D��qD�@ D�� D���D�HD�@ D�~�D�� D��D�@ D�~�D�� D�HD�>�D�~�D��qD���D�@ D��HD�� D���D�@ D��HD�� D��qD�@ D�� D�� D�  D�AHD�� D�� D�HD�@ D�� D��HD�HD�B�D�~�D���D�HD�AHD�� D�� D���D�=qD�� D�D�HD�>�D�� D��HD�  D�>�D�~�D���D�  D�@ D�� D���D���D�>�D�� D���D���D�AHD�� D��HD�HD�@ D��HD�� D���D�>�D�~�D���D���D�>�D�� D��HD�HD�AHD��HD�D�  D�@ D��HD���D�  D�>�D�}qD���D�HD�@ D�~�D�� D�HD�AHD�� D���D���D�AHD��HD�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�>�D�� D�� D�HD�@ D�}qD�� D�  D�@ D�� D���D�  D�@ D�~�D��HD�HD�@ D��HD�� D�  D�@ D�� D���D�  D�AHDHD��HD���D�@ DÀ D�� D�  D�AHDĀ D�� D�HD�@ DŁHD�� D�  D�AHDƁHD��HD�  D�@ Dǀ D��HD�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�B�DʁHD�� D�  D�@ Dˀ D��HD�  D�=qD�}qD̾�D�  D�@ D�~�D�� D�  D�@ D΀ D�� D�HD�@ Dπ D�D�  D�@ DЁHD�� D�  D�AHDр D��HD�HD�AHDҁHD��HD�HD�>�DӀ D�� D���D�AHDԁHD��HD�HD�AHDՁHDվ�D���D�=qD�}qD�� D�HD�>�D�}qD׽qD���D�@ D؁HD�� D�  D�@ Dـ D��HD�HD�AHDڂ�D��HD�HD�AHDہHD��HD�HD�@ D܀ D�� D�  D�@ D݀ D��HD�HD�B�DށHD޾�D���D�AHD߀ D߾�D�  D�AHD�� DྸD�  D�AHD� D�� D���D�>�D�~�D�� D�HD�AHD�HD㾸D�HD�>�D� D�� D���D�>�D� D��HD���D�AHD� D�� D�HD�AHD� D��HD���D�>�D�~�D�� D�HD�@ D�}qD龸D���D�@ D�~�D꾸D��qD�>�D�~�D뾸D�  D�AHD�HD��HD�  D�AHD�~�D���D�HD�>�D�~�D��HD�  D�>�D�HD��HD�  D�>�D�� D�D�  D�AHD�~�D�D���D�AHD� D�� D�HD�AHD� D��HD�  D�>�D� D�� D�HD�>�D�~�D���D�  D�@ D�� D�� D�  D�>�D��HD��HD�HD�>�D�~�D���D�  D�@ D�� D��HD��G�O�?�?\)?W
=?��?���?�p�?�(�?�@�@(�@(��@=p�@L��@c�
@z�H@��@�{@�Q�@��@���@���@��
@�\)@��H@�ff@��@��HAz�A
�HA  A�A�HA   A&ffA+�A0  A5A;�AA�AFffAL(�AQ�AW�A\��Aa�AfffAl(�Aq�AvffAz�HA�  A�=qA�z�A�ffA�  A��A��
A�p�A��RA�Q�A�=qA�(�A�A�
=A���A��\A�z�A�{A�\)A�G�A��A��A��RA�  A�=qA�(�A�{A�\)A�G�A�33A�p�A�\)A���A�33A�p�A�  A��AÅA�A�Q�A�=qA�z�A�ffAУ�A�33A��A�
=A�G�A��
A�{A�  A��A�z�A�RA���A��HA���A�\)A�A�33A��A�\)A���A��
A��A�\)B ��B{B
=B  B��B�B33BQ�B	�B
{B�Bz�Bp�BffB�B��BB�RB�B��B{B
=B  B�BffB33B(�B�BffB�B z�B!p�B"=qB#�B$��B%��B&�\B'�B(��B)B*�\B+�B,��B-B.�HB0  B0��B1B2�RB3�
B4��B5B6�RB7�
B9�B:{B;
=B<  B=G�B>�\B?�B@z�BABB�HBD(�BEG�BF=qBG�BH��BI�BK
=BL(�BM�BN=qBO�BP��BQBR�HBS�
BU�BVffBW\)BXQ�BYp�BZ�\B[�
B\��B]B^�HB_�
Ba�Bb{Bc
=Bd  Be�Bf=qBg\)BhQ�Bi�Bj{Bk
=Bl  Bm�Bn=qBo
=Bo�
Bp��BqBr�HBs�Btz�BuG�Bv=qBw\)Bxz�Byp�BzffB{33B|(�B}G�B~ffB�B�(�B���B�
=B��B��B�ffB��RB�33B�p�B��B��
B�{B�Q�B��\B���B���B�
=B��B�\)B��B��
B�{B�=qB�ffB��RB���B�33B�p�B��B��B�{B�ffB��RB�
=B�G�B��B��B�  B�=qB��\B��HB�33B�p�B�B�  B�=qB��\B���B��B�\)B��B�  B�Q�B���B���B�G�B���B��
B�(�B�ffB���B��HB�33B��B��
B�(�B�z�B��RB��B�\)B��B�  B�=qB�z�B���B��B�p�B�B�{B�ffB���B��HB��B�p�B�B�{B�ffB��RB�
=B�\)B�B�{B�ffB��RB���B�G�B���B��B�(�B�z�B��RB�
=B�G�B���B��
B�=qB�z�B���B�
=B�\)B��B�  B�=qB��\B��HB��B�p�B��B�  B�=qB�z�B��RB��B�\)B��B��B�=qB��\B��HB�33B���B��
B�=qB��\B���B�G�B��B�  B�Q�B��RB���B�\)B��B�  B�ffB��RB��B�p�B��
B�(�B��\B��HB�G�B��B�  B�ffB���B�33B���B��B�ffB���B�33B���B�  B�ffB���B�33B���B�  B�z�B��HB�G�B�B�(�B��\B�
=B�p�B��B�Q�B���B�G�B�B�(�B���B�
=B��B�  B�ffB��HB�G�B��
B�Q�B���B�G�B��
B�=qB���B�G�B��B�(�B£�B��BÅB�  Bď\B��Bř�B�{BƏ\B�
=BǅB�  B�ffB��HB�\)B��
B�ffB��HB�\)B��
B�ffB���B�p�B��B�ffB��HB�\)B��
B�ffB��HB�G�B�B�=qBҸRB�33BӮB�(�Bԣ�B��Bՙ�B�{B�z�B���BׅB��B�z�B���B�p�B�  B�ffB���B�p�B�  B܏\B�
=B݅B�  Bޏ\B�
=B߅B�{B��\B��B�B�=qB���B�\)B�  B�z�B�
=B噚B�{B��B�33B�B�=qB���B�\)B�  B�\B��B�B�Q�B��HB�p�B��B�\B�
=BB�{B��B�33B�B�ffB���B�B�{B���B�33B��B�(�B���B�p�B�  B��\B��B��B�(�B��RB�33B�B�Q�B���B��B�{B��\B��B��C {C ffC �C  CG�C�C�
C{C\)C�C��CG�C��C�HC33Cp�C�RC  CQ�C�C��C33C�C��C{Cp�C�RC��C=qC�\C�HC	=qC	�C	��C
{C
\)C
�RC
=CQ�C��C�HC(�Cz�C��C{CffC��C�C=qC�C�HC33Cz�C��C
=C\)C�C  CQ�C��C�HC33C�\C�HC(�Cp�CC{Cp�C�RC  CQ�C�C  CQ�C�\C�C=qC�\C�C=qC�C��C�Cp�C��C(�Cz�CC{CffCC�Cp�CC{CffCC(�Cz�C��C�Cp�C��C (�C z�C ��C!�C!p�C!��C"(�C"z�C"�
C#(�C#z�C#��C$(�C$�C$�HC%33C%�C%�
C&(�C&�C&�C'=qC'�\C'�HC(33C(�\C(�C)G�C)��C)��C*\)C*�C+
=C+\)C+�C,{C,p�C,�
C-�C-z�C-�HC.G�C.��C.��C/G�C/�C0
=C0p�C0��C1�C1p�C1��C233C2��C2�C3=qC3�\C3��C4Q�C4�C5  C5\)C5�RC6�C6z�C6�
C7(�C7�\C7��C8\)C8�C9
=C9ffC9��C:33C:�\C:�HC;G�C;�C<{C<p�C<��C=(�C=�\C=��C>\)C>�C?{C?z�C?�HC@=qC@�\C@��CA\)CACB�CBp�CB��CC33CC��CD  CD\)CD�CE
=CEp�CE�
CF=qCF��CF��CGQ�CG�RCH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�lG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�A���A���A���A���A���A��A���A���A���A�  A���A���A���A�  A���A��A��A��A��A��yA��mA���A�VA�&�A�I�A�r�Aܛ�Aܺ^Aܺ^Aܰ!A܍PA�oA�dZA��HAѺ^A�`BA�A�?}A�A�A�dZA¾wA�n�A��A��A���A��A���A�A�1'A�1'A�A�9XA�E�A�dZA���A��A��;A�XA��A��A�E�A��#A�dZA��RA��/A�ƨA�M�A���A�{A�1A��AzE�AqAmVAh�9AgoAdA^z�A[7LAW|�AQ;dAM7LAJI�AG��AA��A>ffA=x�A:(�A7VA5\)A4�A4�!A3A3x�A2�DA0�\A/\)A/�wA/�
A.�/A-��A,�A+l�A*n�A)"�A&�`A%\)A%O�A%�PA%�A&(�A%�TA$�RA$^5A#��A#�;A#�-A#��A#�mA#��A$1'A$^5A$�A$�uA$ȴA$�`A$��A$��A${A#hsA#�A"��A"VA!�TA!;dA �A��Ap�A��AVA��AK�A�A|�A\)A��A��A�TA|�A?}A/A�A��AM�A(�A�-A%A��A^5A �A1A�mA�
A�^A�A�A^5A�TA�hA?}AoA�jA$�A�AA�hAx�A/A�`A�RAr�A1'A�A�wA�7AK�A�A�AȴAn�A �A1A  A��A�hA`BA33A��A��A�RAVAJA��A�Al�AS�AVA
�DA	�TA	�7A	�A�jA�jAr�AA�
A�A;dA��A�9Az�A �A�mA��Al�A/A%AȴA�\AVA��AS�A?}A�A~�A��A�Ap�A/A �/A ��A �A �\A ff@�\)@��@��!@�v�@�5?@�hs@��@��9@�(�@���@�@��h@�O�@�j@�|�@��@�n�@��#@�7L@�bN@�|�@�o@�5?@���@��@�t�@�S�@�R@��#@�j@�I�@��@�o@�$�@��T@�7@��@�j@�bN@��m@�33@旍@���@�X@�u@��
@�S�@�33@�@��y@�!@�V@�G�@���@�z�@� �@�b@�  @�ƨ@��@�{@ݺ^@�/@��`@ܓu@�bN@��y@�M�@�@١�@�x�@��/@�j@��@ם�@���@�x�@� �@�b@�b@��@љ�@�%@��
@�n�@�{@�/@�r�@�Q�@��
@�;d@�@ə�@ȋD@�1@�dZ@��@Ə\@���@���@��
@�l�@��@�@��^@�@�G�@�r�@��@���@�ff@�$�@�X@�(�@��@�S�@�o@���@�{@��T@��-@���@��j@��D@�Q�@� �@��w@�o@�n�@��7@�G�@�?}@�&�@���@��@�1'@�S�@�
=@���@���@���@�=q@���@�G�@��@�9X@��@�K�@�@���@�~�@�5?@���@�hs@�p�@�V@��@���@��@��@�"�@�n�@���@�?}@�Ĝ@���@�9X@���@�l�@�K�@���@�ff@�5?@��^@�X@�V@�V@�%@���@�Ĝ@�bN@��@��@�t�@�\)@�;d@���@�^5@��@�x�@�X@�V@���@�z�@�I�@���@�+@���@�ff@�E�@�{@��-@��@��j@� �@��@�+@�
=@�ȴ@��+@�E�@��-@�7L@��@���@�(�@��F@�+@�M�@���@�p�@�?}@�/@���@���@�Z@�1'@�ƨ@�dZ@�+@�ȴ@�v�@�E�@���@���@�x�@�G�@��@�Q�@�9X@� �@��@��m@���@�33@��\@�-@��@��-@���@��@�/@�%@�Ĝ@�bN@�1'@�b@��
@��P@�dZ@��@���@�M�@�{@��T@��h@��@�x�@�O�@��@�V@��`@��@���@���@��@�1'@�1@��m@��F@���@��@�S�@�K�@�C�@��@���@�E�@�5?@��@���@�`B@�%@��@�r�@�I�@�1'@��@��;@���@�S�@�;d@�o@��@��y@��@�ȴ@���@�M�@�J@���@�`B@�&�@��/@��@���@���@�I�@�@l�@~�@~��@~5?@}��@}`B@}�@|�@|�@{�@{"�@z��@z=q@y��@yhs@x�`@x�9@x�@xQ�@x1'@x1'@x  @w�@vv�@v5?@u��@u��@u�-@u@u�-@up�@u`B@u/@t�@tj@t(�@sC�@so@r�@r�H@so@r�@r��@r^5@r�@q��@q�@p�`@p�9@pQ�@o�;@o+@o
=@n��@n�+@m�@m@mO�@m�@l�@l9X@k��@j�H@i��@i��@i��@i&�@hbN@g�w@gl�@gK�@g
=@f�R@f�R@f��@f�+@fv�@f$�@e�@e/@d��@d9X@c�
@c��@ct�@b��@bn�@bM�@a�#@ahs@a%@`A�@_��@_|�@^��@^��@^$�@]��@]O�@\j@[�m@[��@[@Y��@YG�@Xr�@W�;@W+@V�+@V{@U@U�@U?}@T�D@TZ@T�@S��@R��@RJ@Q�#@QX@P��@PbN@O�@OK�@N��@N��@N�@N�R@Nv�@NE�@N5?@M��@Mp�@MV@K��@K�m@K�
@K�
@L1@K��@K��@Ko@J�@J��@J~�@J=q@J=q@J=q@JM�@J-@Ihs@I%@H�`@H�`@H��@G�@G�@Gl�@G�P@G|�@G�@F��@FE�@F$�@E�@E`B@Dz�@DI�@C�
@B�@B~�@B�@A�7@AX@@�`@@��@@bN@@ �@?�@?|�@?K�@>��@>�R@>v�@>5?@=�T@=��@=V@<��@<(�@;�F@;��@;t�@;o@:��@:�!@:��@:n�@:-@9�#@9��@9x�@9&�@8�9@8A�@7�@7;d@6ȴ@6��@6v�@6E�@6{@5�T@5��@4�@4z�@4I�@4�@3��@3�
@3t�@3"�@3dZ@3C�@3@2�@2�H@2��@2n�@2J@1�#@1��@1��@1�7@1G�@1&�@1%@0Ĝ@/��@.�y@.�R@.��@.V@.{@-��@-��@-��@-`B@-�@,��@+t�@+"�@*�@*�!@*�\@*^5@*�@)�@)�#@)�^@)��@)x�@)&�@)%@(��@(��@( �@'�@'�P@'�P@'|�@'\)@'+@&�y@&��@&�+@&ff@&5?@%?}@$��@$�D@#ƨ@#33@"�H@"^5@!�@!�^@!��@!��@!x�@!7L@!7L@!&�@!7L@!�@ ��@ �`@ ��@ A�@   @�@�@�P@+@�@ȴ@ȴ@��@�+@E�@{@�h@p�@O�@?}@/@V@��@�@�/@��@j@9X@1@�
@C�@@��@��@��@�\@~�@~�@M�@-@J@�@�@�#@�#@�^@7L@��@�`@��@��@1'@�;@��@
=@�R@E�@$�@{@�@@@��@p�@?}@��@�/@�@�D@j@Z@I�@(�@1@�m@ƨ@��@dZ@�H@n�@-@&�@�`@Ĝ@Q�@  @�w@��@|�@|�@|�@|�@|�@|�@\)@;d@
=@ȴ@V@$�@$�@{@�T@�-@�h@p�@?}@��@�/@�j@�@z�@(�@�m@�m@�
@ƨ@��@�@o@
�!@
�!@
�\@
^5@
^5@
n�@
^5@
-@	�#@	��@	x�@	X@	7L@	&�@��@�u@A�@1'@ �@ �@  @  @  @  @�A�1A�
=A�%A�  A���A�A���A���A�A�
=A�A�  A�%A�A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A��A��A��A���A���A���A���A���A���A���A���A�  A�A�A���A���A���A�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�  A�  A���A���A���A�  A���A���A�  A�  A�  A���A���A���A�  A�A�  A���A�  A�  A���A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��yA��A��A��A��A��yA��A��A��A��yA��A��A��A��yA��mA��A��A��A��yA��mA��`A��yA��A��mA��`A��yA��A��yA��mA��yA��A��yA��`A��`A��yA��A��yA���A���A���A���A�  A�1A�JA�VA�JA�
=A�JA�VA��A��A�$�A�"�A�$�A�$�A�&�A�(�A�33A�;dA�;dA�?}A�A�A�I�A�S�A�\)A�`BA�bNA�bNA�jA�r�A�x�A܁A܃A܉7Aܕ�Aܕ�Aܕ�Aܙ�Aܟ�Aܰ!AܶFAܴ9AܸRAܾwAܾwAܼjAܸRAܺ^AܼjAܾwAܾwAܼjAܶFAܶFAܶFAܺ^AܶFAܰ!AܮAܮAܬAܧ�Aܡ�Aܙ�AܓuAܓuAܓuA܇+A�z�A�VA�/A��A�oA�VA�1A�A�A��A��HA۸RAۛ�A�t�A�O�A�oA��mAڗ�A�bNA�(�A���A�r�A��TA�G�A��
A�&�A�ĜA�(�A��TAҺ^A�M�A��Aϕ�A��A΍PA���A�p�A�A�A�VA�O�A���A�v�A�"�A�1A��yA���A�bA��
Aɡ�AɓuA�t�A�E�A��A�
=A��
A���Aȝ�A��Aǧ�A�t�A�ZA�?}A��A�A���Aƥ�A�|�A�\)A�S�A�XA�ZA�ZA�XA�XA�O�A�G�A�E�A�9XA�1'A�VA���A��HA�ĜAŰ!Aŗ�A�~�A�dZA�XA�E�A��A�%A��A��#A���A�ƨAĸRAĮAģ�Aě�AčPAăA�jA�\)A�S�A�A�A�&�A�
=A��mAú^A�bNA� �A��A��;A���A�A°!A¥�ADA�l�A�=qA�bA���A���A��A���A��A�ffA�M�A�9XA��A�A��A��/A���A�ȴA��-A��PA�x�A�p�A�dZA�I�A� �A�A��HA�A��A� �A��;A���A�~�A�1'A�VA��A��A�;dA��HA��A�A���A�C�A���A��A�=qA��A��/A��
A���A�M�A�33A��A��A�{A�oA��A��A�VA��yA��TA���A��`A���A�^5A�C�A�bA��
A��^A��+A���A�bNA�5?A�A��
A�A�jA�I�A�9XA��A���A���A��FA��A�z�A�ffA�\)A�7LA�1'A�/A�1'A�/A�+A�(�A�33A�5?A�/A�+A�(�A�(�A�"�A�$�A�/A�?}A�I�A�I�A�I�A�A�A�9XA�&�A�1A��uA�ffA��A��A��+A�=qA�{A��#A���A�5?A���A�VA�p�A��A���A��A��`A��A���A��^A��A���A��+A�hsA�A�A��
A���A�z�A�A�A�  A���A�ZA�-A�%A���A���A�VA��A��uA�+A���A�{A�`BA�A��uA�^5A��A�A��DA�E�A��A��A��9A�\)A���A��A���A�A�A��#A���A��A�|�A��yA���A�+A���A���A�A�A��hA���A�%A���A��A�|�A�t�A�S�A���A�t�A���A��PA�l�A�7LA�JA��A��A���A��^A��RA��!A���A��hA��A�hsA�\)A�7LA��`A�~�A�;dA�A�x�A�K�A�A�A�33A�(�A�"�A�{A�%A���A��DA�VA�G�A�9XA��A��TA���A�ffA�+A��A�K�A�A���A�t�A�;dA�oA��A��jA�VA��A�/A��A��A��wA�n�A��jA�;dA�ƨA�XA���A�E�A�ƨA�M�A���A���A��+A��A��A�p�A�C�A�JAXA~A�A};dA|��A{Az��Az�Ay%Aw��AuAr��Ar$�Aq�Ap��ApȴAp�+Ao�TAn��Am�^Am�Al��Ak��Ajv�AidZAh��Ah�RAh�Ah�+Ahv�AhAg�FAg��Ag�Ag/AfM�Ae�#Ae��Ad��AdbNAc�Ac�Ac/Ab��A`ĜA_|�A]��A]p�A\��A\�9A\JA[��A[C�AZ�HAZ�\AZQ�AZ9XAY�#AY�AW|�AU`BASt�AR�AQ��AQx�AQ+AQVAP�AOx�AN��AMC�AL��ALM�AK��AJ��AJjAJ^5AJI�AJ�AI��AI�wAIx�AIVAH^5AF��AD�AC�#ACS�AB��AB1A@�uA?��A?A>��A>n�A>I�A> �A>A=�A=��A=�wA=�7A<��A<ZA;�A;&�A:bNA9?}A8jA8�A7�A6�A6�A6��A6��A6A5�A5K�A5\)A5�A4��A4�A4�RA4�9A4�A4��A4�`A4��A4�jA4�A4�DA4r�A4I�A3�-A3��A3�7A3�7A3�7A3|�A3x�A3x�A3|�A3\)A3K�A2�/A2Q�A2(�A2bA1�#A1XA0�A0�+A/��A.�HA/"�A/7LA/x�A/|�A/�PA/��A/�^A/�^A/A/��A/�mA01A01A/��A/��A/�hA/hsA/?}A.�yA.^5A-�A-��A-�A-p�A-�A-�-A-�A-��A,�9A,ȴA,�/A,bA+�;A+�A+K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            A�A�A�A���A���A���A���A���A��A���A���A���A�  A���A���A���A�  A���A��A��A��A��A��yA��mA���A�VA�&�A�I�A�r�Aܛ�Aܺ^Aܺ^Aܰ!A܍PA�oA�dZA��HAѺ^A�`BA�A�?}A�A�A�dZA¾wA�n�A��A��A���A��A���A�A�1'A�1'A�A�9XA�E�A�dZA���A��A��;A�XA��A��A�E�A��#A�dZA��RA��/A�ƨA�M�A���A�{A�1A��AzE�AqAmVAh�9AgoAdA^z�A[7LAW|�AQ;dAM7LAJI�AG��AA��A>ffA=x�A:(�A7VA5\)A4�A4�!A3A3x�A2�DA0�\A/\)A/�wA/�
A.�/A-��A,�A+l�A*n�A)"�A&�`A%\)A%O�A%�PA%�A&(�A%�TA$�RA$^5A#��A#�;A#�-A#��A#�mA#��A$1'A$^5A$�A$�uA$ȴA$�`A$��A$��A${A#hsA#�A"��A"VA!�TA!;dA �A��Ap�A��AVA��AK�A�A|�A\)A��A��A�TA|�A?}A/A�A��AM�A(�A�-A%A��A^5A �A1A�mA�
A�^A�A�A^5A�TA�hA?}AoA�jA$�A�AA�hAx�A/A�`A�RAr�A1'A�A�wA�7AK�A�A�AȴAn�A �A1A  A��A�hA`BA33A��A��A�RAVAJA��A�Al�AS�AVA
�DA	�TA	�7A	�A�jA�jAr�AA�
A�A;dA��A�9Az�A �A�mA��Al�A/A%AȴA�\AVA��AS�A?}A�A~�A��A�Ap�A/A �/A ��A �A �\A ff@�\)@��@��!@�v�@�5?@�hs@��@��9@�(�@���@�@��h@�O�@�j@�|�@��@�n�@��#@�7L@�bN@�|�@�o@�5?@���@��@�t�@�S�@�R@��#@�j@�I�@��@�o@�$�@��T@�7@��@�j@�bN@��m@�33@旍@���@�X@�u@��
@�S�@�33@�@��y@�!@�V@�G�@���@�z�@� �@�b@�  @�ƨ@��@�{@ݺ^@�/@��`@ܓu@�bN@��y@�M�@�@١�@�x�@��/@�j@��@ם�@���@�x�@� �@�b@�b@��@љ�@�%@��
@�n�@�{@�/@�r�@�Q�@��
@�;d@�@ə�@ȋD@�1@�dZ@��@Ə\@���@���@��
@�l�@��@�@��^@�@�G�@�r�@��@���@�ff@�$�@�X@�(�@��@�S�@�o@���@�{@��T@��-@���@��j@��D@�Q�@� �@��w@�o@�n�@��7@�G�@�?}@�&�@���@��@�1'@�S�@�
=@���@���@���@�=q@���@�G�@��@�9X@��@�K�@�@���@�~�@�5?@���@�hs@�p�@�V@��@���@��@��@�"�@�n�@���@�?}@�Ĝ@���@�9X@���@�l�@�K�@���@�ff@�5?@��^@�X@�V@�V@�%@���@�Ĝ@�bN@��@��@�t�@�\)@�;d@���@�^5@��@�x�@�X@�V@���@�z�@�I�@���@�+@���@�ff@�E�@�{@��-@��@��j@� �@��@�+@�
=@�ȴ@��+@�E�@��-@�7L@��@���@�(�@��F@�+@�M�@���@�p�@�?}@�/@���@���@�Z@�1'@�ƨ@�dZ@�+@�ȴ@�v�@�E�@���@���@�x�@�G�@��@�Q�@�9X@� �@��@��m@���@�33@��\@�-@��@��-@���@��@�/@�%@�Ĝ@�bN@�1'@�b@��
@��P@�dZ@��@���@�M�@�{@��T@��h@��@�x�@�O�@��@�V@��`@��@���@���@��@�1'@�1@��m@��F@���@��@�S�@�K�@�C�@��@���@�E�@�5?@��@���@�`B@�%@��@�r�@�I�@�1'@��@��;@���@�S�@�;d@�o@��@��y@��@�ȴ@���@�M�@�J@���@�`B@�&�@��/@��@���@���@�I�@�@l�@~�@~��@~5?@}��@}`B@}�@|�@|�@{�@{"�@z��@z=q@y��@yhs@x�`@x�9@x�@xQ�@x1'@x1'@x  @w�@vv�@v5?@u��@u��@u�-@u@u�-@up�@u`B@u/@t�@tj@t(�@sC�@so@r�@r�H@so@r�@r��@r^5@r�@q��@q�@p�`@p�9@pQ�@o�;@o+@o
=@n��@n�+@m�@m@mO�@m�@l�@l9X@k��@j�H@i��@i��@i��@i&�@hbN@g�w@gl�@gK�@g
=@f�R@f�R@f��@f�+@fv�@f$�@e�@e/@d��@d9X@c�
@c��@ct�@b��@bn�@bM�@a�#@ahs@a%@`A�@_��@_|�@^��@^��@^$�@]��@]O�@\j@[�m@[��@[@Y��@YG�@Xr�@W�;@W+@V�+@V{@U@U�@U?}@T�D@TZ@T�@S��@R��@RJ@Q�#@QX@P��@PbN@O�@OK�@N��@N��@N�@N�R@Nv�@NE�@N5?@M��@Mp�@MV@K��@K�m@K�
@K�
@L1@K��@K��@Ko@J�@J��@J~�@J=q@J=q@J=q@JM�@J-@Ihs@I%@H�`@H�`@H��@G�@G�@Gl�@G�P@G|�@G�@F��@FE�@F$�@E�@E`B@Dz�@DI�@C�
@B�@B~�@B�@A�7@AX@@�`@@��@@bN@@ �@?�@?|�@?K�@>��@>�R@>v�@>5?@=�T@=��@=V@<��@<(�@;�F@;��@;t�@;o@:��@:�!@:��@:n�@:-@9�#@9��@9x�@9&�@8�9@8A�@7�@7;d@6ȴ@6��@6v�@6E�@6{@5�T@5��@4�@4z�@4I�@4�@3��@3�
@3t�@3"�@3dZ@3C�@3@2�@2�H@2��@2n�@2J@1�#@1��@1��@1�7@1G�@1&�@1%@0Ĝ@/��@.�y@.�R@.��@.V@.{@-��@-��@-��@-`B@-�@,��@+t�@+"�@*�@*�!@*�\@*^5@*�@)�@)�#@)�^@)��@)x�@)&�@)%@(��@(��@( �@'�@'�P@'�P@'|�@'\)@'+@&�y@&��@&�+@&ff@&5?@%?}@$��@$�D@#ƨ@#33@"�H@"^5@!�@!�^@!��@!��@!x�@!7L@!7L@!&�@!7L@!�@ ��@ �`@ ��@ A�@   @�@�@�P@+@�@ȴ@ȴ@��@�+@E�@{@�h@p�@O�@?}@/@V@��@�@�/@��@j@9X@1@�
@C�@@��@��@��@�\@~�@~�@M�@-@J@�@�@�#@�#@�^@7L@��@�`@��@��@1'@�;@��@
=@�R@E�@$�@{@�@@@��@p�@?}@��@�/@�@�D@j@Z@I�@(�@1@�m@ƨ@��@dZ@�H@n�@-@&�@�`@Ĝ@Q�@  @�w@��@|�@|�@|�@|�@|�@|�@\)@;d@
=@ȴ@V@$�@$�@{@�T@�-@�h@p�@?}@��@�/@�j@�@z�@(�@�m@�m@�
@ƨ@��@�@o@
�!@
�!@
�\@
^5@
^5@
n�@
^5@
-@	�#@	��@	x�@	X@	7L@	&�@��@�u@A�@1'@ �@ �@  @  @  @  G�O�A�1A�
=A�%A�  A���A�A���A���A�A�
=A�A�  A�%A�A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A��A��A��A���A���A���A���A���A���A���A���A�  A�A�A���A���A���A�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�  A�  A���A���A���A�  A���A���A�  A�  A�  A���A���A���A�  A�A�  A���A�  A�  A���A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��yA��A��A��A��A��yA��A��A��A��yA��A��A��A��yA��mA��A��A��A��yA��mA��`A��yA��A��mA��`A��yA��A��yA��mA��yA��A��yA��`A��`A��yA��A��yA���A���A���A���A�  A�1A�JA�VA�JA�
=A�JA�VA��A��A�$�A�"�A�$�A�$�A�&�A�(�A�33A�;dA�;dA�?}A�A�A�I�A�S�A�\)A�`BA�bNA�bNA�jA�r�A�x�A܁A܃A܉7Aܕ�Aܕ�Aܕ�Aܙ�Aܟ�Aܰ!AܶFAܴ9AܸRAܾwAܾwAܼjAܸRAܺ^AܼjAܾwAܾwAܼjAܶFAܶFAܶFAܺ^AܶFAܰ!AܮAܮAܬAܧ�Aܡ�Aܙ�AܓuAܓuAܓuA܇+A�z�A�VA�/A��A�oA�VA�1A�A�A��A��HA۸RAۛ�A�t�A�O�A�oA��mAڗ�A�bNA�(�A���A�r�A��TA�G�A��
A�&�A�ĜA�(�A��TAҺ^A�M�A��Aϕ�A��A΍PA���A�p�A�A�A�VA�O�A���A�v�A�"�A�1A��yA���A�bA��
Aɡ�AɓuA�t�A�E�A��A�
=A��
A���Aȝ�A��Aǧ�A�t�A�ZA�?}A��A�A���Aƥ�A�|�A�\)A�S�A�XA�ZA�ZA�XA�XA�O�A�G�A�E�A�9XA�1'A�VA���A��HA�ĜAŰ!Aŗ�A�~�A�dZA�XA�E�A��A�%A��A��#A���A�ƨAĸRAĮAģ�Aě�AčPAăA�jA�\)A�S�A�A�A�&�A�
=A��mAú^A�bNA� �A��A��;A���A�A°!A¥�ADA�l�A�=qA�bA���A���A��A���A��A�ffA�M�A�9XA��A�A��A��/A���A�ȴA��-A��PA�x�A�p�A�dZA�I�A� �A�A��HA�A��A� �A��;A���A�~�A�1'A�VA��A��A�;dA��HA��A�A���A�C�A���A��A�=qA��A��/A��
A���A�M�A�33A��A��A�{A�oA��A��A�VA��yA��TA���A��`A���A�^5A�C�A�bA��
A��^A��+A���A�bNA�5?A�A��
A�A�jA�I�A�9XA��A���A���A��FA��A�z�A�ffA�\)A�7LA�1'A�/A�1'A�/A�+A�(�A�33A�5?A�/A�+A�(�A�(�A�"�A�$�A�/A�?}A�I�A�I�A�I�A�A�A�9XA�&�A�1A��uA�ffA��A��A��+A�=qA�{A��#A���A�5?A���A�VA�p�A��A���A��A��`A��A���A��^A��A���A��+A�hsA�A�A��
A���A�z�A�A�A�  A���A�ZA�-A�%A���A���A�VA��A��uA�+A���A�{A�`BA�A��uA�^5A��A�A��DA�E�A��A��A��9A�\)A���A��A���A�A�A��#A���A��A�|�A��yA���A�+A���A���A�A�A��hA���A�%A���A��A�|�A�t�A�S�A���A�t�A���A��PA�l�A�7LA�JA��A��A���A��^A��RA��!A���A��hA��A�hsA�\)A�7LA��`A�~�A�;dA�A�x�A�K�A�A�A�33A�(�A�"�A�{A�%A���A��DA�VA�G�A�9XA��A��TA���A�ffA�+A��A�K�A�A���A�t�A�;dA�oA��A��jA�VA��A�/A��A��A��wA�n�A��jA�;dA�ƨA�XA���A�E�A�ƨA�M�A���A���A��+A��A��A�p�A�C�A�JAXA~A�A};dA|��A{Az��Az�Ay%Aw��AuAr��Ar$�Aq�Ap��ApȴAp�+Ao�TAn��Am�^Am�Al��Ak��Ajv�AidZAh��Ah�RAh�Ah�+Ahv�AhAg�FAg��Ag�Ag/AfM�Ae�#Ae��Ad��AdbNAc�Ac�Ac/Ab��A`ĜA_|�A]��A]p�A\��A\�9A\JA[��A[C�AZ�HAZ�\AZQ�AZ9XAY�#AY�AW|�AU`BASt�AR�AQ��AQx�AQ+AQVAP�AOx�AN��AMC�AL��ALM�AK��AJ��AJjAJ^5AJI�AJ�AI��AI�wAIx�AIVAH^5AF��AD�AC�#ACS�AB��AB1A@�uA?��A?A>��A>n�A>I�A> �A>A=�A=��A=�wA=�7A<��A<ZA;�A;&�A:bNA9?}A8jA8�A7�A6�A6�A6��A6��A6A5�A5K�A5\)A5�A4��A4�A4�RA4�9A4�A4��A4�`A4��A4�jA4�A4�DA4r�A4I�A3�-A3��A3�7A3�7A3�7A3|�A3x�A3x�A3|�A3\)A3K�A2�/A2Q�A2(�A2bA1�#A1XA0�A0�+A/��A.�HA/"�A/7LA/x�A/|�A/�PA/��A/�^A/�^A/A/��A/�mA01A01A/��A/��A/�hA/hsA/?}A.�yA.^5A-�A-��A-�A-p�A-�A-�-A-�A-��A,�9A,ȴA,�/A,bA+�;A+�A+K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�B�HB�vB��B�BB�BBϫBϫB�BB�B�BBϫB�vB�vBϫBϫB�B�BуBѷB�aB��B�B�sB��B	(B	*eB	Q�B	�{B	��B	�gB	�2B	уB	�aB	�hB	�tB	�B	��B	�jB
&�B
iB
��B
��B
�B
�B
�3B
��B
�GB
��B
��B
ޞB�BEB�B
˒B
��B
��B
l"B
M�B
JXB
-�B
'B
(B
GB
�B
fB
�B

=B
1B	��B	�B	�[B	�kB	�B	ffB	K�B	.B	$@B	�B	�B�]B� B�5B� B��B�B�TBܒB�;B��B�)B�(B	SB	�B	'B	33B	W?B	tB	h�B	r�B	zB	y	B	h�B	k�B	`BB	UgB	U2B	K�B	K�B	MjB	WsB	^�B	m�B	z�B	�B	��B	�%B	��B	��B	��B	��B	��B	��B	�)B	�DB	�B	�2B
B
+B

�B
�B
4B
�B
oB
uB
�B
�B
B
+B
�B
�B
�B
�B
�B
OB
VB
B
B
"�B
�B
"�B
&�B
'RB
($B
)�B
)�B
+B
1[B
/�B
0!B
0�B
2-B
2aB
2�B
2aB
3�B
4�B
7B
8�B
9�B
9$B
9�B
8�B
9�B
<6B
>�B
CaB
E�B
E�B
HB
IRB
J#B
L�B
N<B
N�B
OB
OvB
OB
OBB
N<B
N�B
OBB
MjB
K�B
K�B
M�B
L�B
K)B
L�B
MB
L0B
LdB
K)B
K�B
K�B
K)B
J�B
I�B
JXB
I�B
H�B
GEB
G�B
GB
G�B
I�B
IB
HKB
H�B
G�B
G�B
F�B
FtB
EmB
E�B
E�B
DgB
D3B
C-B
C-B
B[B
B[B
A�B
?�B
>BB
>wB
=<B
<�B
:�B
:^B
;dB
;�B
;0B
:�B
:�B
:^B
:^B
7�B
7�B
7LB
7B
5�B
3�B
2�B
4nB
1�B
0�B
.�B
-�B
.B
+kB
*eB
*eB
)*B
(�B
'�B
&�B
&�B
%�B
$tB
$B
"hB
!�B
!�B
"4B
!�B
 �B
 \B
!�B
�B
VB
�B
�B
B
B
�B
�B
�B
B
�B
�B
qB
7B
B
�B
eB
1B
�B
�B
+B
_B
�B
$B
�B
$B
$B
�B
�B
SB
{B
FB
�B
B
�B
@B
�B
uB
B
�B
.B
�B
"B
�B
JB
�B
�B
(B

�B
	�B
B

�B

	B
B
B

�B
�B
xB
�B
B
~B
B
DB
�B
xB
�B
"B

�B

=B
	lB
�B
	�B

rB
�B
xB
B

�B
JB
�B
B
JB
B
B

�B

�B
�B

rB

rB
DB

rB

rB

	B

	B

=B

=B
B
B

�B

=B

rB

rB

rB
xB
~B
B
�B
�B
DB
B
B
xB
xB
�B
�B
~B
�B
�B
�B
"B
�B
�B
hB
uB
uB
�B
�B
�B
�B
{B
B
�B
�B
{B
�B
MB
�B
�B
�B
�B
B
B
�B
�B
�B
�B
$B
$B
$B
�B
�B
�B
�B
�B
�B
1B
7B
=B
CB
�B
�B
�B
IB
�B
qB
IB
~B
�B
�B
!B
�B
B
�B
�B
�B
~B
IB
~B
~B
B
�B
B
B
�B
�B
�B
 \B
 \B
 �B
!-B
!bB
"hB
#B
"�B
#B
$B
$@B
$tB
$�B
$�B
%zB
%�B
&�B
&LB
&LB
'�B
'�B
'�B
'�B
(XB
)�B
(�B
(�B
'�B
'�B
(XB
)_B
)�B
)_B
*eB
)�B
*�B
+B
*�B
*�B
+6B
+B
+B
+�B
+�B
+�B
,B
,=B
,�B
,�B
,qB
,�B
-B
-B
-�B
.IB
.B
-�B
-�B
.�B
.}B
.�B
.�B
.�B
.�B
/OB
/B
/B
/OB
0UB
1'B
0�B
0�B
0�B
1[B
1[B
2-B
1�B
2-B
2aB
2aB
2�B
2�B
33B
2�B
3�B
3hB
33B
3hB
33B
3�B
4B
49B
4�B
4�B
5B
5�B
5tB
5?B
5tB
6B
6B
6zB
6FB
6�B
7LB
7�B
7�B
7LB
7�B
7�B
7�B
7�B
8B
8B
8B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
:�B
;0B
<B
<6B
<�B
=�B
?HB
?�B
?�B
?}B
@�B
@B
@�B
@�B
@�B
A B
AUB
A�B
C�B
C�B
C�B
C�B
C�B
D3B
D3B
C�B
C�B
C�B
C�B
EmB
EmB
E9B
FB
E�B
FB
FtB
FB
F�B
F�B
F�B
GEB
FtB
F?B
E�B
FB
EmB
F?B
F�B
GEB
GzB
GzB
GEB
GzB
H�B
I�B
J#B
J#B
J�B
JXB
J�B
J�B
JXB
J�B
K�B
K�B
K^B
K�B
K�B
K�B
K^B
K�B
K�B
L�B
MB
M6B
M6B
M�B
M�B
M�B
NB
M�B
OBB
OB
P}B
PHB
QNB
QB
Q�B
Q�B
Q�B
R�B
RTB
R B
S&B
S&B
S[B
S[B
S[B
S&B
S[B
S�B
R�B
S�B
TaB
T,B
U2B
UgB
U2B
U2B
T�B
UgB
V9B
V�B
W�B
W�B
W�B
XB
X�B
[#B
[#B
[�B
[�B
[�B
\�B
\)B
\�B
\]B
[�B
\�B
^B
]/B
]�B
\�B
]dB
\�B
[�B
\]B
\�B
^B
^B
^�B
]�B
^5B
]dB
^B
\�B
\]B
\]B
[�B
Z�B
[#B
\]B
\�B
\�B
\�B
\]B
\]B
]/B
\�B
]/B
]dB
]�B
^B
]�B
^B
^5B
^�B
_;B
_B
_�B
`BB
`vB
aHB
a�B
b�B
b�B
cTB
cTB
cTB
b�B
b�B
c B
c B
d&B
c�B
dZB
dZB
d�B
e,B
d�B
d�B
e,B
dZB
e,B
d�B
d�B
e,B
d�B
e�B
f�B
g�B
h�B
iDB
iyB
iDB
iyB
jB
kB
j�B
kQB
jB
j�B
jKB
kB
jKB
i�B
jB
j�B
iDB
i�B
h�B
iB
i�B
i�B
jKB
jKB
jB
kQB
l�B
l�B
l�B
l"B
lWB
l"B
lWB
l�B
l�B
m)B
l�B
l�B
m�B
l�B
l�B
m]B
m)B
m)B
n/B
n�B
ncB
m�B
n�B
o5B
n�B
n�B
oiB
o B
n�B
pB
o�B
o�B
o�B
oiB
p;B
p�B
qvB
qAB
qB
qB
qAB
q�B
rB
rB
q�B
r|B
r|B
r|B
rB
s�B
sMB
sMB
sMB
s�B
s�B
t�B
t�B
tB
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v+B
v`B
v`B
v+B
v+B
v+B
v�B
v�B
v�B
v�B
w2B
x8B
x8B
x8B
xB
xB
x�B
xlB
x8B
x�B
xlB
x�B
x�B
x�B
y	B
x8B
x�B
y�B
y>B
y>B
y�B
y>B
y�B
zDB
yrB
y�B
y>B
y�B
yrB
y�B
zDB
{B
{JB
{�B
}VB
}"B
}�B
}�B
}�B
}�B
}�B
~(B
~]B
~(B
~(B
~�B
~�B
~�B
~(B
~�B
~�B
}�B
~�B
~�B
~]B
�B
�B
�B
�4B
��B
��B
��B
�B
��B
�;B
�oB
�B
�;B
�;B
�oB
��B
�B
��B
�;B
��B
��B
��B
�B
�B
�GB
�B
��B
�B
�B
��B
��B
�B
��B
�B
�SB
�YB
��B
��B
��B
�1B
�1B
��B
�7B
��B
��B
��B
�DB
�B
��B
�xB
�~B
�B
�B
�~B
�PB
��B
�PB
��B
��B
�B
��B�}BбB�pB�}B�}B�B�}B�NBΥB��B�}B��BϫBϫB�<B�vB�HB�BΥBΥB�vBбB��B�<B�B��BбB�BB�pB��B�HBϫB�pB�<BϫBбB�B�pBΥBϫBϫB�pB��B�}B�}B�B�<B��B�BϫB�pB�<B�B�HB�}B�pB�pB��BбB�BΥB�B�<B�B�B�BB�pB�HB�B��B�pB�}BбB�BB�B��BϫB�}BбB�vB�pB�B�}B��B�BB��B� B�NB�vB�BϫB�HB�HB�BB�}B��B�TB�HBбBуB҉BѷB�B�B��BӏB�NBѷBѷBҽB��B��B�NBӏB�aBӏB��B�9B�mB�
B��B�?B�mBרB�B�EB�sB��BخB��B�?BخBݘB�dB�/B�)B��B�B�"B�KB�B��B��B�B��B��B�VB��B��B	B	B	B	�B	�B	VB	4B	VB	�B	VB	�B	!-B	(XB	)�B	9$B	=�B	=qB	A�B	B�B	I�B	Q�B	VmB	\�B	iB	jB	u�B	x�B	|�B	��B	�B	��B	��B	�HB	��B	�B	��B	��B	��B	�aB	�9B	�B	��B	��B	�9B	�B	�2B	��B	՛B	�mB	�9B	�2B	��B	�&B	�[B	�,B	ҽB	�}B	ϫB	бB	�HB	��B	˒B	ÖB	��B	B	�'B	��B	��B	�B	��B	��B	�?B	��B	��B	�wB	��B	�_B	��B	�B	��B	�\B	��B	�IB	��B	�B	�bB	�.B	f�B	ffB	w�B	��B	�CB	��B	�oB	�B	��B	�CB	��B	��B	�KB	��B	�B	��B	��B	�hB	ҽB	�6B	��B	�B	��B	�9B	�gB	��B	�B	��B	�RB	ܒB	�NB	��B	��B	�NB	�,B	��B	�oB	��B
�B
PB
\B
.B
B
�B
�B
OB
"�B
&�B
(�B
2aB
5�B
?�B
CaB
L�B
P�B
S�B
XEB
[�B
e�B
e`B
jB
q�B
tTB
w�B
|�B
{B
|PB
cB
��B
�oB
��B
�GB
�%B
��B
�B
��B
�@B
�eB
�IB
�B
�B
��B
�vB
��B
ӏB
ԕB
�
B
��B
�sB
��B
��B
��B
�B
�mB
�iB 4B
��B
�ZB
�B
��B
�B
�B
��B
�oB
�/B
��B
�]B
�B
��B
�KB
�B
�B
�]B
��B
�KB
��B
�B
�B
�8B
��B
�)B
��B
�dB
�BB
ʌB
�AB
�B
��B
��B
�HB
�qB
��B
��B
��B
��B
�B
��B
��B
�	B
��B
��B
�B
�GB
�MB
��B
�4B
�SB
��B
��B
�rB
��B
��B
��B
�	B
�B
|PB
��B
� B
��B
�	B
�B
��B
��B
�B
�oB
�B
�rB
��B
��B
�uB
�1B
��B
��B
�IB
��B
��B
҉B
�2B
�B
�B
خB
��B
�2B
�	BGB�B_BDBB�BBBB&LB(�B4nB@�BJ�BS[BV9BOvBMBI�BY�B7�BIBFB�B iB;B
��B
� B
��B
ɺB
��B
��B
��B
�B
�RB
�RB
��B
��B
��B
�hB
��B
��B
��B
�B
��B
�.B
��B
��B
~(B
x�B
v�B
sMB
rB
r|B
n/B
kQB
j�B
gmB
e`B
YB
S�B
TaB
D�B
FB
PHB
PHB
HKB
J�B
M�B
L�B
L�B
J�B
`vB
EB
6�B
7�B
@OB
,qB
 'B
=<B
�B
"�B
�B
"�B
{JB
2�B
�B
&B
B
 B
�B
�B
�B
SB
oB
GB
�B
.B
�B
	lB
�B
�B	��B	�cB
B
 �B	�cB
B	�.B
 �B
SB
hB
SB
�B
�B
1B
�B
�B
�B
AB
�B
GB
uB
�B
~B
_B
�B
�B
+B
B
�B
.B
�B
B
+B
9�B
#:B
�B

	B
�B
B
�B
xB
7B	�;B	�5B	��B	��B	��B
oB	�B	�B	ޞB	�#B	�TB	�2B	��B	�B	�!B	��B	��B	�B	�kB	�CB	��B	��B	��B	��B	�+B	��B	�.B	�fB	��B	��B	��B	}VB	a�B	_;B	[�B	N�B	T�B	R B	Y�B	K�B	9�B	PB	L0B	K�B	@�B	2�B	(�B	)�B	-B	(�B	/�B	%FB	�B	!�B	&B	!�B	�B	YB	!B	�B	�B	�B	�B	B	"�B	$B	B��B�B�ZB�B��B�;B�B�2B�BޞB�DB�>B��B�GB��B��B�BیB�B��B��B�mB��B�B֡B�B��B�B֡B�,B��B�aBӏB��B� B��B�B�8B�5BܒB�]B�B�8B��B�B� BݘB�B�WB�dBچB��B�QBܒB�pB��B��B�sB�fB��B�2B�KB��B�5B�B�B�B�"B	�B�rB�B��B	�B	;B	B	B�]B		7B	�B	�B	�B	B	�B	�B	�B	�B	%B	($B	,�B	0!B	.B	3�B	3hB	4B	1�B	8�B	:�B	N<B	bNB	`�B	iDB	s�B	v+B	u�B	k�B	sB	gB	i�B	c�B	o�B	o�B	tB	sMB	s�B	s�B	s�B	u�B	uZB	xB	{�B	~�B	}"B	yrB	� B	u�B	�AB	v�B	tTB	n�B	jB	g�B	c B	j�B	jB	m�B	}�B	Y�B	aHB	b�B	c�B	g8B	ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            B��B�.BЗBϫB��B�\B�vB��BϫB�BB�B�BB��BϑB�vB��B��B�HB�HBуB��B�{B��B�B�
B�jB	�B	)�B	Q4B	�-B	�B	՛B	�B	�FB	ȚB	ÖB	ɠB	��B	�VB	�B
,WB
ncB
�?B
��B
�BtB
��B
��B
�[B
��B
��B
޸BBT�B0oB
�:B
��B
��B
�B
^OB
aHB
J�B
7LB
�B
EB
6B
<B
�B
VB
*B
jB	�]B	˒B	�B	�CB	vB	Y�B	4nB	.�B	$�B	4B�B	�B�B�B�B�9B��B�B�B��B��B��B	?B	�B	(sB	6�B	]�B	wfB	h>B	r�B	}�B	}VB	k�B	p�B	c�B	Z7B	\�B	QB	K�B	L�B	V�B	]~B	n�B	~�B	�GB	�GB	��B	�VB	��B	�B	�mB	��B	�+B	��B	��B	�B	��B
9B
�B
�B
�B
oB
B
�B
2B
$B
SB
�B
�B
7B
�B
=B
B
IB
"4B
�B
�B
�B
$�B
 �B
#�B
'8B
(>B
)DB
*�B
*�B
,�B
3�B
0�B
1'B
1�B
2�B
2�B
2�B
2�B
4�B
6FB
9�B
:DB
:�B
:DB
:xB
:B
;�B
=B
?}B
DB
FB
F�B
IB
J	B
K)B
M�B
O(B
O\B
O�B
PHB
O�B
O�B
N�B
O�B
PHB
M�B
L0B
LJB
N�B
M�B
K�B
M�B
M�B
L�B
M�B
LJB
MB
L0B
KxB
K)B
KB
LJB
LB
J	B
H�B
H�B
G+B
IB
KDB
I�B
IlB
I�B
H�B
H�B
G�B
G�B
FYB
F�B
F?B
ESB
D�B
DB
C�B
CGB
C�B
C�B
@OB
?cB
@B
?B
>wB
;dB
;dB
<�B
<6B
;B
;B
;�B
<�B
;JB
8RB
8RB
7�B
8�B
6�B
4B
3�B
6�B
3�B
1[B
/OB
/OB
/�B
,=B
+�B
+�B
*eB
*eB
)yB
'�B
(>B
($B
&B
%,B
"�B
# B
#nB
$@B
"hB
!HB
"NB
#�B
 BB
 B
�B
�B
�B
B
;B
�B
B
IB
IB
)B
]B
�B
kB
B
�B
B
�B
kB
�B
�B
�B
YB
sB
�B
�B
sB
�B
�B
B
�B
9B
SB
�B
�B
B
{B
�B
TB
4B
hB
�B
B
�B
0B
B
NB
0B
B
�B
�B
�B
PB
xB
�B
B
�B
�B
B
�B
PB
0B
~B
�B
<B
�B
�B

�B

rB
jB

	B
^B
6B
6B
0B
�B
�B
�B
<B
6B
�B
�B
xB
�B
JB

�B
�B
�B

�B

�B

rB

�B
�B
xB
�B
�B

�B

rB

�B
B
xB
B
B
~B
0B
�B
�B
6B
�B
�B
B
�B
6B
B
6B
jB
"B
�B
�B
�B
 B
FB
�B
2B
FB
�B
MB
�B
2B
�B
B
MB
�B
�B
�B
�B
�B
�B
B
�B
?B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
+B
�B
B
#B
�B
�B
jB
�B
5B
�B
�B
xB
�B
�B
B
VB
 BB
�B
VB
�B
�B
5B
B
�B
B
�B
B
�B
�B
B
�B
 B
!-B
!bB
!B
!-B
!bB
!�B
# B
#�B
#:B
#�B
$�B
$�B
%,B
%zB
%FB
&B
&�B
&�B
&�B
'mB
($B
'�B
'�B
'�B
(sB
*B
)�B
)�B
(sB
(sB
(�B
)�B
)�B
)�B
*�B
*B
+QB
+kB
+B
+QB
+�B
+kB
+�B
,�B
,=B
,=B
,qB
,�B
,�B
,�B
,�B
-CB
-CB
-]B
.B
.cB
.B
.B
.}B
/ B
.�B
/OB
.�B
.�B
/OB
/iB
/5B
/�B
0UB
0�B
1AB
0�B
1[B
1�B
2B
2B
2�B
2GB
2aB
2�B
2�B
3MB
3�B
3hB
3MB
3�B
3�B
3MB
3�B
3�B
49B
4�B
4�B
5?B
5%B
5�B
6B
5�B
5ZB
6B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
8B
7�B
8B
8�B
8B
8lB
8�B
8lB
8�B
9	B
8�B
8�B
9$B
9	B
8�B
8�B
9rB
;0B
;B
<jB
<jB
<�B
=�B
?cB
@B
?�B
?�B
AB
@iB
@�B
A�B
@�B
A;B
AoB
A�B
C�B
DB
DMB
DB
D�B
D�B
DgB
C�B
DgB
DMB
D�B
E�B
E�B
E�B
F�B
FB
FtB
F�B
F�B
G+B
GEB
G_B
H1B
F�B
FYB
FYB
F�B
FB
F�B
F�B
G�B
G�B
GzB
G_B
G�B
IB
J	B
J�B
J�B
J�B
J�B
K)B
J�B
J�B
K^B
L0B
K�B
K�B
LJB
K�B
L~B
K�B
LB
LB
MB
MjB
M�B
M�B
NpB
N"B
NB
N�B
N�B
O�B
O�B
QB
Q B
Q�B
Q�B
Q�B
Q�B
R:B
S&B
R�B
RoB
S�B
TB
S�B
S�B
S�B
S�B
S�B
TB
S[B
TFB
TaB
TaB
UMB
U�B
UgB
UMB
UgB
U�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
[qB
[�B
[�B
[�B
\B
\�B
\)B
\�B
\]B
\)B
]�B
^jB
]dB
]�B
]IB
^B
]IB
\)B
\CB
\�B
^jB
^�B
^�B
^B
^jB
^B
^�B
]/B
\�B
]/B
\]B
[WB
[�B
\�B
\�B
]IB
]B
\�B
\�B
]dB
]/B
]~B
]�B
]�B
^OB
^B
^OB
^�B
_!B
_�B
_pB
`B
`vB
`�B
a�B
a�B
b�B
c B
c�B
c�B
c�B
b�B
c:B
c�B
c�B
dtB
dtB
d�B
d�B
d�B
e`B
e,B
e,B
ezB
eB
e�B
d�B
e,B
eFB
e,B
f2B
f�B
gmB
h�B
iyB
i�B
i_B
i�B
jeB
k�B
kB
kkB
j�B
j�B
j�B
k6B
jB
j0B
kB
k�B
iyB
i�B
i*B
i_B
i�B
jB
jKB
j�B
jeB
k�B
m�B
mB
l�B
lqB
lqB
lWB
l�B
l�B
mB
mCB
l�B
m)B
m�B
m)B
m)B
m�B
m�B
m�B
nIB
n�B
n}B
n/B
o B
o�B
oB
n�B
o�B
oOB
o�B
poB
p!B
pUB
poB
o�B
p�B
qAB
q�B
q[B
qB
qAB
qvB
q�B
r-B
rB
q�B
r�B
r�B
r�B
r�B
s�B
shB
sMB
s�B
tTB
t9B
t�B
t�B
t9B
t�B
t�B
t�B
utB
vB
u�B
vB
vFB
vzB
vzB
vFB
vFB
vzB
v�B
v�B
v�B
w2B
w�B
x�B
xlB
x8B
xB
x8B
x�B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y	B
xlB
yXB
zB
yXB
yXB
y�B
y�B
z*B
z�B
zB
y�B
y�B
y�B
y�B
y�B
zxB
{B
{JB
|PB
}�B
}qB
~B
}�B
}�B
}�B
~B
~BB
~wB
~BB
~BB
~�B
~�B
~�B
~�B
.B
B
~�B
B
~�B
~�B
�B
�B
�B
�OB
��B
��B
��B
�B
��B
�UB
��B
�;B
��B
��B
��B
��B
� B
��B
�oB
��B
��B
��B
�[B
�-B
�aB
�3B
��B
�gB
�MB
��B
��B
�3B
�B
�9B
��B
��B
��B
�+B
��B
�1B
�1B
�B
�lB
�#B
��B
��B
�^B
�)B
��B
��B
��B
�dB
�6B
��B
�PB
��B
�PB
��B
��B
�6G�O�B�}BбB�pB�}B�}B�B�}B�NBΥB��B�}B��BϫBϫB�<B�vB�HB�BΥBΥB�vBбB��B�<B�B��BбB�BB�pB��B�HBϫB�pB�<BϫBбB�B�pBΥBϫBϫB�pB��B�}B�}B�B�<B��B�BϫB�pB�<B�B�HB�}B�pB�pB��BбB�BΥB�B�<B�B�B�BB�pB�HB�B��B�pB�}BбB�BB�B��BϫB�}BбB�vB�pB�B�}B��B�BB��B� B�NB�vB�BϫB�HB�HB�BB�}B��B�TB�HBбBуB҉BѷB�B�B��BӏB�NBѷBѷBҽB��B��B�NBӏB�aBӏB��B�9B�mB�
B��B�?B�mBרB�B�EB�sB��BخB��B�?BخBݘB�dB�/B�)B��B�B�"B�KB�B��B��B�B��B��B�VB��B��B	B	B	B	�B	�B	VB	4B	VB	�B	VB	�B	!-B	(XB	)�B	9$B	=�B	=qB	A�B	B�B	I�B	Q�B	VmB	\�B	iB	jB	u�B	x�B	|�B	��B	�B	��B	��B	�HB	��B	�B	��B	��B	��B	�aB	�9B	�B	��B	��B	�9B	�B	�2B	��B	՛B	�mB	�9B	�2B	��B	�&B	�[B	�,B	ҽB	�}B	ϫB	бB	�HB	��B	˒B	ÖB	��B	B	�'B	��B	��B	�B	��B	��B	�?B	��B	��B	�wB	��B	�_B	��B	�B	��B	�\B	��B	�IB	��B	�B	�bB	�.B	f�B	ffB	w�B	��B	�CB	��B	�oB	�B	��B	�CB	��B	��B	�KB	��B	�B	��B	��B	�hB	ҽB	�6B	��B	�B	��B	�9B	�gB	��B	�B	��B	�RB	ܒB	�NB	��B	��B	�NB	�,B	��B	�oB	��B
�B
PB
\B
.B
B
�B
�B
OB
"�B
&�B
(�B
2aB
5�B
?�B
CaB
L�B
P�B
S�B
XEB
[�B
e�B
e`B
jB
q�B
tTB
w�B
|�B
{B
|PB
cB
��B
�oB
��B
�GB
�%B
��B
�B
��B
�@B
�eB
�IB
�B
�B
��B
�vB
��B
ӏB
ԕB
�
B
��B
�sB
��B
��B
��B
�B
�mB
�iB 4B
��B
�ZB
�B
��B
�B
�B
��B
�oB
�/B
��B
�]B
�B
��B
�KB
�B
�B
�]B
��B
�KB
��B
�B
�B
�8B
��B
�)B
��B
�dB
�BB
ʌB
�AB
�B
��B
��B
�HB
�qB
��B
��B
��B
��B
�B
��B
��B
�	B
��B
��B
�B
�GB
�MB
��B
�4B
�SB
��B
��B
�rB
��B
��B
��B
�	B
�B
|PB
��B
� B
��B
�	B
�B
��B
��B
�B
�oB
�B
�rB
��B
��B
�uB
�1B
��B
��B
�IB
��B
��B
҉B
�2B
�B
�B
خB
��B
�2B
�	BGB�B_BDBB�BBBB&LB(�B4nB@�BJ�BS[BV9BOvBMBI�BY�B7�BIBFB�B iB;B
��B
� B
��B
ɺB
��B
��B
��B
�B
�RB
�RB
��B
��B
��B
�hB
��B
��B
��B
�B
��B
�.B
��B
��B
~(B
x�B
v�B
sMB
rB
r|B
n/B
kQB
j�B
gmB
e`B
YB
S�B
TaB
D�B
FB
PHB
PHB
HKB
J�B
M�B
L�B
L�B
J�B
`vB
EB
6�B
7�B
@OB
,qB
 'B
=<B
�B
"�B
�B
"�B
{JB
2�B
�B
&B
B
 B
�B
�B
�B
SB
oB
GB
�B
.B
�B
	lB
�B
�B	��B	�cB
B
 �B	�cB
B	�.B
 �B
SB
hB
SB
�B
�B
1B
�B
�B
�B
AB
�B
GB
uB
�B
~B
_B
�B
�B
+B
B
�B
.B
�B
B
+B
9�B
#:B
�B

	B
�B
B
�B
xB
7B	�;B	�5B	��B	��B	��B
oB	�B	�B	ޞB	�#B	�TB	�2B	��B	�B	�!B	��B	��B	�B	�kB	�CB	��B	��B	��B	��B	�+B	��B	�.B	�fB	��B	��B	��B	}VB	a�B	_;B	[�B	N�B	T�B	R B	Y�B	K�B	9�B	PB	L0B	K�B	@�B	2�B	(�B	)�B	-B	(�B	/�B	%FB	�B	!�B	&B	!�B	�B	YB	!B	�B	�B	�B	�B	B	"�B	$B	B��B�B�ZB�B��B�;B�B�2B�BޞB�DB�>B��B�GB��B��B�BیB�B��B��B�mB��B�B֡B�B��B�B֡B�,B��B�aBӏB��B� B��B�B�8B�5BܒB�]B�B�8B��B�B� BݘB�B�WB�dBچB��B�QBܒB�pB��B��B�sB�fB��B�2B�KB��B�5B�B�B�B�"B	�B�rB�B��B	�B	;B	B	B�]B		7B	�B	�B	�B	B	�B	�B	�B	�B	%B	($B	,�B	0!B	.B	3�B	3hB	4B	1�B	8�B	:�B	N<B	bNB	`�B	iDB	s�B	v+B	u�B	k�B	sB	gB	i�B	c�B	o�B	o�B	tB	sMB	s�B	s�B	s�B	u�B	uZB	xB	{�B	~�B	}"B	yrB	� B	u�B	�AB	v�B	tTB	n�B	jB	g�B	c B	j�B	jB	m�B	}�B	Y�B	aHB	b�B	c�B	g8B	ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�%�=�=��<��<$"x<#�
<#�
<6 �<Qhd<7�<��;<�<1�<�[�<NU�<#�
<#�
<#�
<���<���<#�
<Bq<���<�]<��<�ϳ<�s<�%�<#�
<#�
<6 �<#�
<#�
<�\�<���<���<�/�<%��<�/�<ՙ <�4~<k��<#�
<7�<�iQ<I�x<h"<�.�<eO</V�<-�!<���<F3<#�
<C<0�J<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018071123003220180711230032IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072207031520180722070315QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072207031520180722070315QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550920190521075509IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                