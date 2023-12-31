CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-01-16T16:17:19Z creation; 2023-04-26T19:14:27Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20190116161719  20230426191427  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               %   %AA  AOAO7316_008644_037                 7316_008644_037                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @ؠi-#N�@ؠi-#N�11  @ؠiw1��@ؠiw1��@*��8�J�@*��8�J��c�~��6�c�~��611  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@B�\@�G�@�G�@�  @�  A   A��A ��A+�A?\)A`  A�Q�A�  A��A�Q�A�  A�  A߮A�  B   B�B  BQ�B   B'�
B0(�B8  B?�
BH  BP(�BW�
B_�Bh  Bp  Bw�
B�
B�  B�{B��B��
B�  B�{B�  B�  B�(�B�B�p�B��B�  B�(�B�{B�{B�(�B�{B�  B�  B��B��B�  B�  B��B��B��B�  B�  B�  B�{C   C�C�C�C  C
{C{C��C�C��C
=C��C��C
=C
=C
=C   C"  C${C&
=C'��C)��C,  C.{C0
=C1��C3�C6
=C8  C9�C;��C>
=C?��CB  CD  CF  CH
=CI��CK��CM�CO��CQ��CT
=CV
=CX  CY��C\  C^{C`  Cb  Cd  Cf
=Ch
=Cj
=Cl
=Cn  Co��Cq��Ct  Cu��Cw��Cz  C|  C~  C�C�  C�  C�  C�  C�C�C�  C���C�  C�  C���C�  C���C���C�C�  C���C�  C�C�C���C���C�  C�  C�  C�C�  C�  C���C�  C�  C���C�  C�C�  C���C���C�  C�C�C�C�C�  C���C�  C�C�C�  C�  C�  C�C���C���C�  C�  C�C�  C�  C�  C�  C�C�C�C�  C�  C�  C���C�  C�C�C�  C�  C�  C���C���C�  C���C�  C�C�C���C���C�  C���C���C�  C�  C�  C�  C���C���C���C�  C�  C�  C�  C�  C�  C���C���C�  C�C�  C���C���C���C�C�C�  C�  C���C�  C�
=C�
=C�C�  C�C�  C�  C�C�  C���C�  C�C�C�  C�D   D � D �qD}qD�D��D�qDz�D�qD}qD  D� D�qD� D�D��D  D� D�qD	}qD
  D
� D  D� D  D� D  D� D�D}qD��D� D  D� D�D� D�qD��D  D� D�qD}qD  D� D  D��D�qDz�D�qD� D  D��D�D� D��D}qD�D� D�qDz�D  D��D�D��D   D }qD ��D!� D"�D"� D"�qD#� D$D$��D%�D%��D&  D&� D&�qD'z�D'�qD(��D)  D)� D)�qD*}qD*�qD+� D,D,��D-  D-��D.D.��D.�qD/}qD0�D0� D0�qD1}qD1��D2}qD3  D3��D4  D4� D5�D5��D6  D6� D6�qD7� D8�D8� D8�qD9}qD9�qD:}qD;  D;��D<  D<}qD<�qD=}qD=�qD>}qD?�D?��D?�qD@}qDA  DA� DB�DB�DC�DC� DD�DD� DD�qDE}qDF  DF��DGDG��DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL��DL�qDM}qDN  DN}qDN��DO� DP�DP}qDP�qDQz�DQ��DRz�DS  DS��DT  DT� DT�qDUz�DV  DV� DW  DW��DX�DX��DY�DY� DY��DZz�DZ�qD[� D[�qD\� D]�D]� D^�D^�D_�D_}qD`  D`� Da�Da��Db  Db}qDc�Dc� Dc�qDd� DeDe��Df  Df}qDf��Dg}qDh  Dh��Dh�qDi}qDj  Dj}qDj��Dk� Dl�Dl�DmDm��Dn  Dn� Do  Do��Dp�Dp� Dq  Dq��Dr  Dr}qDs  Ds��Dt�Dt� Du  Du}qDv  Dv� Dv�qDw� Dx  Dx��DyDy� Dy�qDz��D{  D{� D|  D|� D}�D}��D~  D~� D~�qD� D�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�>�D�� D�� D�HD�>�D�� D��HD�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�HD�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D�� D���D���D�@ D��HD�D�HD�AHD��HD���D��qD�>�D�� D�� D�HD�@ D�~�D�� D�  D�AHD�� D���D���D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�~�D��HD�  D�@ D�� D�� D�  D�>�D�� D�D�  D�@ D�}qD�� D�HD�AHD��HD��HD�  D�@ D�� D���D���D�@ D�� D���D���D�@ D�~�D���D�  D�@ D�� D���D���D�>�D�~�D���D�  D�AHD�~�D���D�  D�@ D�~�D�� D�HD�AHD�� D���D���D�AHD���D���D��D�B�D��HD��HD�HD�AHD��HD�D�  D�=qD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D��qD���D�AHD�� D�� D�HD�@ D�� D��HD�  D�>�D�� D��HD��D�AHD�~�D��HD��D�B�D��HD�� D�  D�>�D�~�D�� D���D�=qD�� D�� D�HD�AHD���D��HD�  D�AHD�� D���D�  D�AHD��HD�D�HD�@ D��HD�� D���D�@ D�~�D��qD���D�AHD���D�� D���D�@ D�~�D���D���D�>�D�� D��HD�  D�>�D�� D��HD�HD�B�D��HD�� D���D�@ D���D�� D���D�@ D�� D�� D�HD�B�D�� D���D�  D�B�D�� D���D�  D�@ D�~�D���D���D�@ D��HD�� D��qD�>�D�� D�� D�  D�AHD�� D��qD��qD�=qD�~�D�� D�  D�AHD�~�D�� D�  D�@ D D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�=qD�}qDŽqD���D�>�Dƀ D��HD�HD�AHDǂ�D��HD��D�B�DȁHD�� D�HD�B�DɁHD�� D�HD�@ D�~�D�� D�HD�@ D�~�D˾�D�  D�>�D�}qD�� D��qD�>�D�~�D;�D��qD�>�D΀ D��HD�HD�@ DρHD�� D���D�>�DЁHD��HD�  D�@ Dр D�� D�  D�>�D�~�D�� D�  D�@ DӁHD�� D�  D�@ DԀ DԾ�D�  D�AHDՀ Dվ�D���D�AHDր D�� D�  D�>�D�~�D�� D���D�@ D؁HD��HD�  D�>�D�~�D�� D���D�>�Dڀ Dھ�D���D�>�DہHD۾�D�  D�AHD܁HD�� D���D�>�D�~�D�� D�  D�@ DށHD�� D���D�>�D߀ D߾�D�  D�AHD�� D�� D�HD�>�D�~�D�� D�HD�>�D�}qD�qD���D�@ D�HD��HD�HD�AHD� D�� D�HD�@ D� D�� D�  D�@ D�~�D�� D�  D�AHD�HD�� D�  D�@ D�HD�D�  D�@ D�~�D�� D�  D�>�D�~�D꾸D���D�@ D�HD뾸D���D�AHD� D쾸D�  D�@ D�~�D���D��qD�@ D�HD��HD�  D�@ D�~�D�D�  D�@ D��HD�D���D�@ D� D�� D���D�AHD� D�D�  D�@ D� D�D��qD�>�D�~�D�� D���D�>�D��HD�� D�HD�@ D�~�D�D�  D�AHD��HD�� D���D�@ D�� D��HD�  D�@ D�� D��HD���D�7
?#�
?W
=?�=q?���?Ǯ?���?��@�@��@&ff@333@E�@W
=@fff@u@��@���@�z�@�(�@��
@�{@�
=@�G�@Ǯ@��@�(�@��
@�@�z�@��RA33A
=A(�A��A�A��A��A!�A%A)��A,��A0��A5�A8��A;�A?\)ADz�AG�AJ�HAN�RAS33AW
=AZ=qA^�RAc33Ag�Aj=qAn{As33Aw�Az=qA}p�A�G�A�33A���A�ffA���A��HA�z�A�{A�Q�A��\A�z�A�A�  A��A�(�A�A�  A��\A�z�A�{A���A�33A��A�\)A���A�33A�A��A���A��
A�ffA���A��HA��A�  A�=qA�z�A�
=A��A�z�AָRA�G�A�(�A�
=A���A�33A�{A���A��HA��A�  A�\A�p�A�\)A��A���A�\)B ��B{B�B��B�B33B��B
{B33Bz�B{B�Bz�B�B\)B��B=qB\)B��BffB�B��B�\B (�B!G�B"�\B$(�B%��B&�HB((�B)p�B+
=B,Q�B-��B/
=B0z�B1�B3\)B4z�B5B7\)B8��B:{B;\)B<��B>�\B?�
BA�BB�\BD(�BE��BF�HBHQ�BIBK\)BL��BM�BO\)BP��BR=qBS�
BU�BV=qBW�
BYG�BZ�RB\  B]G�B^�\B`  Bap�Bb�RBd  BeG�Bf�RBh(�BiG�BjffBk�
Bm�Bn�\Bo�
Bp��Br=qBs�Bu�Bv{Bw�Bx��Bz=qB{�B|z�B}�B33B�=qB���B�\)B�{B��RB�G�B��
B�ffB�
=B��B�=qB���B�\)B�(�B��RB�33B��
B�ffB��B�B�=qB���B�\)B�{B���B�33B��B�=qB��HB��B�  B�z�B���B���B�(�B��RB��B���B�(�B��RB�33B��B�{B���B�33B��B�(�B���B�G�B��
B�(�B��RB�G�B��
B�z�B���B�p�B��B�z�B�
=B���B�(�B��\B��B��B�(�B���B��B��B�=qB��RB�33B��B�  B��\B���B�\)B��B�{B��\B��HB�G�B�p�B��B��B�(�B�=qB�=qB�=qB�=qB�=qB�{B��B��
B��
B�B��B��B�\)B�G�B�G�B�33B���B��HB���B��RB���B��\B�ffB�=qB�(�B�{B�{B��B�B��B���B��B�p�B�33B�33B��B�
=B�
=B��HB���B��\B��\B�z�B�ffB�ffB�(�B�{B�  B��B��B�B��B��B��B��B�\)B�G�B�33B�
=B�
=B�
=B���B���B��RB���B���B���B��\B�z�B�Q�B�Q�B�Q�B�Q�B�Q�B�(�B�{B�  B�  B�{B�  B��B��
B�B�B�B�B���B��B�p�B�p�B�p�B�\)B�G�B��B�
=B���B���B���B��HB���B���B�z�B�ffB�ffB�=qB�=qB�{B��
B�B��B���B���B�p�B�G�B�33B��B�
=B�
=B���B��HB���B��RB���B��RB��RB��RB���B���B��\B��RB���B��HB���B�
=B��B�G�B��B��B��B�=qB�Q�B��\B���B��B��B��
B�(�B�z�B���B��B�p�B��
B�(�B��\B���B�\)B�B�{B�ffB���B��B��B��
B�(�B��\B�
=B�p�B��B�ffB��RB�33B���B�{B�z�B��HB�G�B��B�{B��\B�
=B��B��B�ffB��HB�\)B��
B�Q�B���B�33B��B�{B��\B�
=B��B�  B�z�B�
=B��B�  B��\B��B���B�(�B��RB�33B�B�=qB���B�G�B��
B�Q�B���B�G�B��
B�Q�B��HB�\)B��B�Q�B���B�G�B�B�=qBȸRB�33BɮB�(�Bʏ\B�
=B˅B�  B�z�B��HB�\)B��
B�ffB��HB�\)B��
B�Q�B���B�33B�B�(�BҸRB�33Bә�B�(�Bԣ�B��Bՙ�B�{B֏\B��Bי�B�(�BظRB�33B�B�Q�B���B�\)B��B�ffB���B�\)B��B�z�B���B�\)B��B�z�B�
=B�B�  B�\B�
=B㙚B�{B��B�33B�B�=qB�RB�\)B��
B�ffB���B�B�  B�z�B��B�B�(�B�RB�33B�B�Q�B��HB�p�B�  B��\B��B�B�(�B�RB�G�B��
B�ffB�
=B���B�(�B��RB�\)B��
B�ffB���B��B�{B���B�G�B��
B�z�B�
=B��B�=qB��HB��C   C Q�C ��C �HC�Cp�C�RC
=C\)C�C  CQ�C��C�C=qC�C�
C{Cp�CC{Cp�CC
=CffC�C  CG�C��C�C	=qC	�\C	�HC
=qC
�\C
�C33C�C�
C(�Cp�CC{C\)C�RC
=CffC�RC
=C\)C��C  CQ�C�C  C\)C�C  CQ�C��C�CG�C��C  CQ�C��C��CG�C��C��CQ�C�RC  C\)C�C  C\)C�RC{Cp�C��C(�Cz�C��C�Cz�C��C=qC�\C�C=qC�\C�CG�C�C  CQ�C��C��C Q�C �RC!{C!ffC!�RC"{C"ffC"��C#(�C#z�C#��C$�C$z�C$�HC%33C%�C%�
C&(�C&z�C&�HC'=qC'��C'�HC(33C(�C(�
C)33C)�\C)�C*33C*�C*�HC+=qC+��C+��C,G�C,��C,�C-Q�C-�C.
=C.\)C.��C/  C/\)C/�RC0{C0ffC0C1
=C1p�C1��C2(�C2p�C2��C3(�C3�C3�HC433C4�C4�
C5=qC5��C5��C6G�C6��C6��C7Q�C7�C8
=C8\)C8�C9
=C9p�C9��C:�C:p�C:��C;33C;�\C;�C<33C<�\C<��C=\)C=�RC>
=C>\)C>�C?
=C?p�C?��C@33C@z�C@��CA33CA�\CA�CBG�CB��CB�CC=qCC��CD
=CD\)CD�RCE
=CEffCECF�CFz�CF��CG�CGz�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        ?�  ?��H@B�\@�G�@�G�@�  @�  A   A��A ��A+�A?\)A`  A�Q�A�  A��A�Q�A�  A�  A߮A�  B   B�B  BQ�B   B'�
B0(�B8  B?�
BH  BP(�BW�
B_�Bh  Bp  Bw�
B�
B�  B�{B��B��
B�  B�{B�  B�  B�(�B�B�p�B��B�  B�(�B�{B�{B�(�B�{B�  B�  B��B��B�  B�  B��B��B��B�  B�  B�  B�{C   C�C�C�C  C
{C{C��C�C��C
=C��C��C
=C
=C
=C   C"  C${C&
=C'��C)��C,  C.{C0
=C1��C3�C6
=C8  C9�C;��C>
=C?��CB  CD  CF  CH
=CI��CK��CM�CO��CQ��CT
=CV
=CX  CY��C\  C^{C`  Cb  Cd  Cf
=Ch
=Cj
=Cl
=Cn  Co��Cq��Ct  Cu��Cw��Cz  C|  C~  C�C�  C�  C�  C�  C�C�C�  C���C�  C�  C���C�  C���C���C�C�  C���C�  C�C�C���C���C�  C�  C�  C�C�  C�  C���C�  C�  C���C�  C�C�  C���C���C�  C�C�C�C�C�  C���C�  C�C�C�  C�  C�  C�C���C���C�  C�  C�C�  C�  C�  C�  C�C�C�C�  C�  C�  C���C�  C�C�C�  C�  C�  C���C���C�  C���C�  C�C�C���C���C�  C���C���C�  C�  C�  C�  C���C���C���C�  C�  C�  C�  C�  C�  C���C���C�  C�C�  C���C���C���C�C�C�  C�  C���C�  C�
=C�
=C�C�  C�C�  C�  C�C�  C���C�  C�C�C�  C�D   D � D �qD}qD�D��D�qDz�D�qD}qD  D� D�qD� D�D��D  D� D�qD	}qD
  D
� D  D� D  D� D  D� D�D}qD��D� D  D� D�D� D�qD��D  D� D�qD}qD  D� D  D��D�qDz�D�qD� D  D��D�D� D��D}qD�D� D�qDz�D  D��D�D��D   D }qD ��D!� D"�D"� D"�qD#� D$D$��D%�D%��D&  D&� D&�qD'z�D'�qD(��D)  D)� D)�qD*}qD*�qD+� D,D,��D-  D-��D.D.��D.�qD/}qD0�D0� D0�qD1}qD1��D2}qD3  D3��D4  D4� D5�D5��D6  D6� D6�qD7� D8�D8� D8�qD9}qD9�qD:}qD;  D;��D<  D<}qD<�qD=}qD=�qD>}qD?�D?��D?�qD@}qDA  DA� DB�DB�DC�DC� DD�DD� DD�qDE}qDF  DF��DGDG��DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL��DL�qDM}qDN  DN}qDN��DO� DP�DP}qDP�qDQz�DQ��DRz�DS  DS��DT  DT� DT�qDUz�DV  DV� DW  DW��DX�DX��DY�DY� DY��DZz�DZ�qD[� D[�qD\� D]�D]� D^�D^�D_�D_}qD`  D`� Da�Da��Db  Db}qDc�Dc� Dc�qDd� DeDe��Df  Df}qDf��Dg}qDh  Dh��Dh�qDi}qDj  Dj}qDj��Dk� Dl�Dl�DmDm��Dn  Dn� Do  Do��Dp�Dp� Dq  Dq��Dr  Dr}qDs  Ds��Dt�Dt� Du  Du}qDv  Dv� Dv�qDw� Dx  Dx��DyDy� Dy�qDz��D{  D{� D|  D|� D}�D}��D~  D~� D~�qD� D�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�>�D�� D�� D�HD�>�D�� D��HD�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�HD�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D�� D���D���D�@ D��HD�D�HD�AHD��HD���D��qD�>�D�� D�� D�HD�@ D�~�D�� D�  D�AHD�� D���D���D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�~�D��HD�  D�@ D�� D�� D�  D�>�D�� D�D�  D�@ D�}qD�� D�HD�AHD��HD��HD�  D�@ D�� D���D���D�@ D�� D���D���D�@ D�~�D���D�  D�@ D�� D���D���D�>�D�~�D���D�  D�AHD�~�D���D�  D�@ D�~�D�� D�HD�AHD�� D���D���D�AHD���D���D��D�B�D��HD��HD�HD�AHD��HD�D�  D�=qD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D��qD���D�AHD�� D�� D�HD�@ D�� D��HD�  D�>�D�� D��HD��D�AHD�~�D��HD��D�B�D��HD�� D�  D�>�D�~�D�� D���D�=qD�� D�� D�HD�AHD���D��HD�  D�AHD�� D���D�  D�AHD��HD�D�HD�@ D��HD�� D���D�@ D�~�D��qD���D�AHD���D�� D���D�@ D�~�D���D���D�>�D�� D��HD�  D�>�D�� D��HD�HD�B�D��HD�� D���D�@ D���D�� D���D�@ D�� D�� D�HD�B�D�� D���D�  D�B�D�� D���D�  D�@ D�~�D���D���D�@ D��HD�� D��qD�>�D�� D�� D�  D�AHD�� D��qD��qD�=qD�~�D�� D�  D�AHD�~�D�� D�  D�@ D D¾�D���D�>�D�~�Dþ�D���D�>�D�~�Dľ�D���D�=qD�}qDŽqD���D�>�Dƀ D��HD�HD�AHDǂ�D��HD��D�B�DȁHD�� D�HD�B�DɁHD�� D�HD�@ D�~�D�� D�HD�@ D�~�D˾�D�  D�>�D�}qD�� D��qD�>�D�~�D;�D��qD�>�D΀ D��HD�HD�@ DρHD�� D���D�>�DЁHD��HD�  D�@ Dр D�� D�  D�>�D�~�D�� D�  D�@ DӁHD�� D�  D�@ DԀ DԾ�D�  D�AHDՀ Dվ�D���D�AHDր D�� D�  D�>�D�~�D�� D���D�@ D؁HD��HD�  D�>�D�~�D�� D���D�>�Dڀ Dھ�D���D�>�DہHD۾�D�  D�AHD܁HD�� D���D�>�D�~�D�� D�  D�@ DށHD�� D���D�>�D߀ D߾�D�  D�AHD�� D�� D�HD�>�D�~�D�� D�HD�>�D�}qD�qD���D�@ D�HD��HD�HD�AHD� D�� D�HD�@ D� D�� D�  D�@ D�~�D�� D�  D�AHD�HD�� D�  D�@ D�HD�D�  D�@ D�~�D�� D�  D�>�D�~�D꾸D���D�@ D�HD뾸D���D�AHD� D쾸D�  D�@ D�~�D���D��qD�@ D�HD��HD�  D�@ D�~�D�D�  D�@ D��HD�D���D�@ D� D�� D���D�AHD� D�D�  D�@ D� D�D��qD�>�D�~�D�� D���D�>�D��HD�� D�HD�@ D�~�D�D�  D�AHD��HD�� D���D�@ D�� D��HD�  D�@ D�� D��HD���G�O�?#�
?W
=?�=q?���?Ǯ?���?��@�@��@&ff@333@E�@W
=@fff@u@��@���@�z�@�(�@��
@�{@�
=@�G�@Ǯ@��@�(�@��
@�@�z�@��RA33A
=A(�A��A�A��A��A!�A%A)��A,��A0��A5�A8��A;�A?\)ADz�AG�AJ�HAN�RAS33AW
=AZ=qA^�RAc33Ag�Aj=qAn{As33Aw�Az=qA}p�A�G�A�33A���A�ffA���A��HA�z�A�{A�Q�A��\A�z�A�A�  A��A�(�A�A�  A��\A�z�A�{A���A�33A��A�\)A���A�33A�A��A���A��
A�ffA���A��HA��A�  A�=qA�z�A�
=A��A�z�AָRA�G�A�(�A�
=A���A�33A�{A���A��HA��A�  A�\A�p�A�\)A��A���A�\)B ��B{B�B��B�B33B��B
{B33Bz�B{B�Bz�B�B\)B��B=qB\)B��BffB�B��B�\B (�B!G�B"�\B$(�B%��B&�HB((�B)p�B+
=B,Q�B-��B/
=B0z�B1�B3\)B4z�B5B7\)B8��B:{B;\)B<��B>�\B?�
BA�BB�\BD(�BE��BF�HBHQ�BIBK\)BL��BM�BO\)BP��BR=qBS�
BU�BV=qBW�
BYG�BZ�RB\  B]G�B^�\B`  Bap�Bb�RBd  BeG�Bf�RBh(�BiG�BjffBk�
Bm�Bn�\Bo�
Bp��Br=qBs�Bu�Bv{Bw�Bx��Bz=qB{�B|z�B}�B33B�=qB���B�\)B�{B��RB�G�B��
B�ffB�
=B��B�=qB���B�\)B�(�B��RB�33B��
B�ffB��B�B�=qB���B�\)B�{B���B�33B��B�=qB��HB��B�  B�z�B���B���B�(�B��RB��B���B�(�B��RB�33B��B�{B���B�33B��B�(�B���B�G�B��
B�(�B��RB�G�B��
B�z�B���B�p�B��B�z�B�
=B���B�(�B��\B��B��B�(�B���B��B��B�=qB��RB�33B��B�  B��\B���B�\)B��B�{B��\B��HB�G�B�p�B��B��B�(�B�=qB�=qB�=qB�=qB�=qB�{B��B��
B��
B�B��B��B�\)B�G�B�G�B�33B���B��HB���B��RB���B��\B�ffB�=qB�(�B�{B�{B��B�B��B���B��B�p�B�33B�33B��B�
=B�
=B��HB���B��\B��\B�z�B�ffB�ffB�(�B�{B�  B��B��B�B��B��B��B��B�\)B�G�B�33B�
=B�
=B�
=B���B���B��RB���B���B���B��\B�z�B�Q�B�Q�B�Q�B�Q�B�Q�B�(�B�{B�  B�  B�{B�  B��B��
B�B�B�B�B���B��B�p�B�p�B�p�B�\)B�G�B��B�
=B���B���B���B��HB���B���B�z�B�ffB�ffB�=qB�=qB�{B��
B�B��B���B���B�p�B�G�B�33B��B�
=B�
=B���B��HB���B��RB���B��RB��RB��RB���B���B��\B��RB���B��HB���B�
=B��B�G�B��B��B��B�=qB�Q�B��\B���B��B��B��
B�(�B�z�B���B��B�p�B��
B�(�B��\B���B�\)B�B�{B�ffB���B��B��B��
B�(�B��\B�
=B�p�B��B�ffB��RB�33B���B�{B�z�B��HB�G�B��B�{B��\B�
=B��B��B�ffB��HB�\)B��
B�Q�B���B�33B��B�{B��\B�
=B��B�  B�z�B�
=B��B�  B��\B��B���B�(�B��RB�33B�B�=qB���B�G�B��
B�Q�B���B�G�B��
B�Q�B��HB�\)B��B�Q�B���B�G�B�B�=qBȸRB�33BɮB�(�Bʏ\B�
=B˅B�  B�z�B��HB�\)B��
B�ffB��HB�\)B��
B�Q�B���B�33B�B�(�BҸRB�33Bә�B�(�Bԣ�B��Bՙ�B�{B֏\B��Bי�B�(�BظRB�33B�B�Q�B���B�\)B��B�ffB���B�\)B��B�z�B���B�\)B��B�z�B�
=B�B�  B�\B�
=B㙚B�{B��B�33B�B�=qB�RB�\)B��
B�ffB���B�B�  B�z�B��B�B�(�B�RB�33B�B�Q�B��HB�p�B�  B��\B��B�B�(�B�RB�G�B��
B�ffB�
=B���B�(�B��RB�\)B��
B�ffB���B��B�{B���B�G�B��
B�z�B�
=B��B�=qB��HB��C   C Q�C ��C �HC�Cp�C�RC
=C\)C�C  CQ�C��C�C=qC�C�
C{Cp�CC{Cp�CC
=CffC�C  CG�C��C�C	=qC	�\C	�HC
=qC
�\C
�C33C�C�
C(�Cp�CC{C\)C�RC
=CffC�RC
=C\)C��C  CQ�C�C  C\)C�C  CQ�C��C�CG�C��C  CQ�C��C��CG�C��C��CQ�C�RC  C\)C�C  C\)C�RC{Cp�C��C(�Cz�C��C�Cz�C��C=qC�\C�C=qC�\C�CG�C�C  CQ�C��C��C Q�C �RC!{C!ffC!�RC"{C"ffC"��C#(�C#z�C#��C$�C$z�C$�HC%33C%�C%�
C&(�C&z�C&�HC'=qC'��C'�HC(33C(�C(�
C)33C)�\C)�C*33C*�C*�HC+=qC+��C+��C,G�C,��C,�C-Q�C-�C.
=C.\)C.��C/  C/\)C/�RC0{C0ffC0C1
=C1p�C1��C2(�C2p�C2��C3(�C3�C3�HC433C4�C4�
C5=qC5��C5��C6G�C6��C6��C7Q�C7�C8
=C8\)C8�C9
=C9p�C9��C:�C:p�C:��C;33C;�\C;�C<33C<�\C<��C=\)C=�RC>
=C>\)C>�C?
=C?p�C?��C@33C@z�C@��CA33CA�\CA�CBG�CB��CB�CC=qCC��CD
=CD\)CD�RCE
=CEffCECF�CFz�CF��CG�CGz�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aї�Aѥ�Aѥ�AѰ!AѶFAѮAѰ!AѮAѩ�Aѩ�Aѧ�Aѧ�AѮAѺ^AѴ9AѴ9AѺ^AѺ^AѾwA���A�ȴA���A���A���A���A���A���A��
A��
A���A���A���A���A���A���A��
A���A�ȴA�ȴAѴ9AуAѡ�A���A��AёhA�oA��A�^5A��#A��+A�A���A�A�A���A�`BA��A��\A�jA���A���A�XA�bA�l�A�|�A�~�A��!A�XA���A�n�A{l�AyhsAvVAs�An9XAl�Akx�Ah��Ac�A`A\�AY7LAWdZAU�hAR5?AO`BANz�AJ�AF�ADbAAt�A@JA<�A<{A;?}A:jA9O�A7�A5��A4��A3�A1�A0z�A.��A-�-A-%A,�yA,^5A*��A(��A&��A&��A&�A&9XA%�mA%�FA&��A'XA'x�A'K�A'�A&�RA&-A%�
A%oA$��A$M�A#ƨA#|�A#VA"ZA!��A!XA �RA  �Al�A�A~�A1'A`BA%A�/A��A��A��AO�A"�A��A��AVAƨA`BA"�A�HAVA�A�
A�AG�A��A�\A9XAbA�mA�^A�7AhsA��Av�AA�AbA�AAdZA�/A��AZAJA��A��A\)AA�AĜAjA��AA�hAdZAVA��A��A�/AȴA��A�RAv�AJAA�7Ap�A7LAVA�AbNA�A��Al�A+AA
��A
�A
�uA
�\A
�\A
�\A
ffA	�-AȴA��AbNA5?A�A�A��A��A�!A�A��A�DAI�A|�A��A��AffAA�A5?A$�A �A�A{AƨAp�A��A�yA�jA �A�mA?}A ��A �HA �A Q�@��@�;d@���@��T@���@�ƨ@�\)@�^5@�?}@�j@�C�@���@��!@�M�@��T@���@��@�t�@���@��`@�r�@�F@�dZ@���@�-@�`B@���@�r�@�Q�@�9X@�1@�|�@��y@�^5@��#@�X@��/@�r�@��
@�dZ@�ȴ@�=q@��@��@��@�j@�D@�@�j@� �@�l�@��@�V@�A�@��;@�l�@��y@ާ�@�E�@ݙ�@�O�@ܣ�@�(�@��m@�S�@ڇ+@��@٩�@�O�@�Z@��m@�|�@�;d@ָR@�-@�x�@�1'@��
@Ӯ@�|�@�\)@�K�@�"�@��H@ҧ�@�V@�J@���@ѩ�@��@�r�@��@�t�@��@��@�(�@��;@˾w@�S�@�@ʸR@�=q@ɡ�@�O�@�%@ȓu@��m@ǍP@���@��@Ų-@ŉ7@�x�@��@�z�@�bN@� �@�ƨ@�o@�@�ff@�$�@��T@���@�7L@��@��@�bN@��@�l�@�o@��y@���@�$�@��-@��@��u@�j@���@��;@�ƨ@��F@��P@�+@��@���@��+@�^5@���@�O�@���@� �@��@�t�@��R@�v�@�@�&�@���@�1'@��@��;@��
@���@��H@�ff@�=q@��T@�p�@�?}@�V@��@���@�S�@���@�ff@��@���@�X@���@�r�@�9X@��m@���@�o@�ȴ@�ff@��@���@��7@�&�@��9@�bN@�1'@��@��;@���@�l�@�;d@�
=@�ȴ@�v�@��@���@��-@�p�@���@��9@�j@�9X@�ƨ@�l�@�;d@�o@��@���@�M�@��T@���@�?}@���@��u@���@��
@���@��F@��@�33@��R@�E�@��@��@���@�hs@�V@��u@�Z@�(�@�  @��F@�K�@���@�v�@���@��@��@��u@�Q�@���@��P@�t�@�;d@�o@��H@���@�V@�@��h@�O�@��@���@���@��u@�A�@��;@�l�@�o@���@�{@���@��@��-@��7@�O�@�?}@�7L@�&�@�V@��@�r�@�I�@��m@�l�@�"�@��@��!@�5?@���@�@���@�O�@��@�Ĝ@�r�@�(�@���@��w@�S�@�"�@�~�@�$�@���@�x�@�G�@��@�z�@�ƨ@�l�@�S�@�C�@��@���@�M�@�E�@�5?@��@��#@���@��@�hs@�`B@�7L@���@��9@�r�@�(�@���@���@�l�@�S�@��@���@��\@�v�@�V@�$�@���@��@�`B@�O�@�G�@�/@��9@�j@��@��@l�@;d@
=@~ȴ@~��@~@}��@}?}@}V@|�/@|�D@|Z@{C�@z�!@z~�@z-@y�@xA�@w�;@wl�@w�@w
=@v��@v�R@u�T@u�-@u`B@t��@t9X@s��@sS�@r�H@r�\@rJ@q��@q��@qx�@q7L@pĜ@pQ�@o�w@n��@nff@n@m�@m/@l��@l�/@l��@l��@lj@kƨ@k�@kS�@j�@j�!@j=q@i�^@ihs@i%@h1'@g��@f�y@fV@e��@e�@eO�@d�@dI�@c��@b��@a��@a�^@aG�@`��@`r�@`1'@`  @_��@_�P@_\)@_�@^ȴ@^�+@^E�@]p�@\�@[�@[dZ@[S�@Z��@ZJ@Y�^@Y��@YX@Y%@X�`@XĜ@X��@XQ�@X1'@X1'@X �@W��@V��@U@UV@T�j@Tz�@TZ@T(�@T1@S�m@S�
@Sƨ@SS�@SC�@S33@R�!@R�\@R~�@Rn�@RJ@Qx�@PĜ@P�@PQ�@P  @O�P@Nff@M�-@M��@M�h@M`B@M?}@L��@L��@Lz�@L1@KdZ@J�H@J~�@JJ@I��@I�7@I�7@Ihs@H�`@H �@G�;@G�@G�P@Gl�@G+@F�@F��@F��@Fv�@Fff@F5?@E�-@Ep�@E�@D�@D��@C��@C"�@B��@B�\@B^5@B-@B�@A�#@A��@A7L@@�`@@r�@?;d@>��@>{@=�h@=O�@=V@<��@<�D@<j@<j@<9X@;�@;C�@:�@:��@:�!@:�\@:�\@:n�@9�#@9G�@97L@9&�@8��@8�@8Q�@81'@8b@7�;@7�w@7�@7|�@7|�@7\)@6ȴ@6v�@6V@6$�@5�-@5/@4��@4�D@4Z@3��@3��@333@2�\@2^5@1�@1��@1��@1x�@1�@0�`@01'@/�;@/�P@/l�@/\)@/;d@.�y@.ff@.$�@-�-@,�@,�D@,z�@,I�@,�@+��@+�F@+C�@+@*��@*�\@*M�@)�@)��@)�^@)X@)%@(�9@( �@'�@'�w@'\)@';d@';d@&��@&�@&��@&ff@%�-@%/@$��@$�@$1@#��@#ƨ@#33@#o@"�H@"��@"n�@!��@!��@!��@!�@ �`@ �u@ A�@ b@�@�w@l�@�@�R@�+@ff@5?@�@�-@p�@/@�@�/@�@9X@�
@�
@ƨ@��@dZ@"�@o@�@��@��@��@~�@n�@^5@�@��@��@��@G�@�@��@��@Q�@b@�;@|�@l�@�@�@E�@@@�@p�@O�@/@�j@�D@z�@�@�m@�
@�F@�F@t�@S�@o@�H@n�@��@�^@��@hs@X@X@&�@�`@�`@Ĝ@�u@�@r�@1'@1'@ �@ �@1'@1'@1'@��@l�@l�@l�@\)@\)@\)@;d@�@��@ȴ@�R@��@�+@ff@$�@@�@�T@@�h@`B@?}@V@�@�j@z�@j@1@ƨ@��@t�@dZ@dZ@dZAѕ�Aѕ�Aѕ�Aѝ�Aѝ�Aѥ�Aѧ�Aѩ�Aѣ�Aѡ�Aѧ�Aѣ�Aѥ�Aѣ�Aѧ�AѶFAѼjAѸRAѶFAѲ-AѴ9AѰ!AѬAѩ�AѮAѴ9AѬAѮAѴ9AѲ-Aѧ�Aѧ�Aѩ�AѮAѩ�Aѧ�AѬAѬAѧ�Aѥ�Aѧ�AѬAѩ�Aѧ�Aѥ�Aѧ�AѬAѧ�Aѡ�Aѧ�Aѧ�Aѥ�AѬAѰ!AѴ9AѰ!AѮAѲ-AѺ^AѺ^AѸRAѺ^AѼjAѼjAѶFAѸRAѶFAѴ9AѬAѮAѶFAѸRAѶFAѴ9AѶFAѴ9AѬAѮAѶFAѶFAѺ^AѸRAѼjAѾwAѼjAѺ^AѺ^AѼjAѼjAѺ^AѺ^AѾwAѼjAѺ^AѸRAѺ^AѼjAѼjAѸRAѾwA���A�A�ƨA�ȴA���A�ȴA�ȴA���A���A���A���A�ƨA�ƨA�ĜA�ȴA�ȴA���A���A�ȴA�ƨA���A���A�ȴA�ȴA���A���A���A���A���A���A���A�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��#A��
A��A��#A��#A���A���A��#A���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��
A��A��#A��HA���A���A���A�ĜA�ĜA�ĜA�ȴA���A�ȴA�ƨA���A���A���A�ĜA�ĜA�ƨA�ȴA�ƨA�ĜA�ĜAѺ^AѶFAѰ!Aџ�Aћ�Aџ�AѓuA�v�A�r�A�v�A�~�AхAщ7AёhAї�Aѩ�A�ĜA�ĜA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A��
A��A���A��A��HAѲ-AэPA�~�A�ffA�S�A�9XA�&�A�"�A��A��A�VA�A���A��A��A��A��A��`A��`A��TA��mA��`A��HA��/A��/A��;A��;A��#A��A��A��A��/A��A���A���A���A�G�Aϣ�A�dZA�bA��TA���Aΰ!AΧ�AΣ�AΩ�AΥ�AΝ�AΙ�AΙ�AΓuAΑhAΑhA΍PA΅AΉ7A΋DA΋DAΉ7A΅A΁A�|�A�~�A�~�A�z�A�v�A�r�A�r�A�r�A�l�A�hsA�ffA�jA�jA�l�A�hsA�bNA�bNA�dZA�hsA�hsA�jA�ffA�bNA�dZA�hsA�jA�hsA�hsA�jA�p�A�r�A�r�A�n�A�l�A�jA�l�A�l�A�n�A�jA�hsA�ffA�ffA�ffA�`BA�ZA�VA�O�A�M�A�G�A�C�A�?}A�9XA�33A�+A�"�A��A��A�JA�A�  A�A���A��A��A��`A��HA��;A��#A���A���A�ȴA���A���A;wAͼjA���A���A;wA���A�A���A��A��#A��A��TA��A���A���A���A���A�  A�%A�
=A�bA�bA�VA�
=A�A���A��A��mA��;A���AͼjAͶFAͮAͣ�A͗�A�p�A̼jA��A�l�AʓuA�XAɸRAɍPAȝ�A�v�AżjA�dZA�A�bNA�bNA��A���A���A�n�A���A��!A�dZA�`BA���A�r�A�t�A�jA�r�A��A���A���A���A��A��^A�dZA�O�A��7A��;A�jA��uA��#A��9A��A��A�G�A��uA�A���A�jA�1'A���A���A���A�M�A�JA��-A�A�A��A���A��A��-A�^5A��-A���A�?}A���A�dZA�=qA�JA��wA�bNA�/A���A�S�A���A�M�A�bA���A��9A���A���A��A�n�A�VA�G�A�7LA�+A�(�A�+A�+A�(�A�(�A� �A��A���A��A���A�t�A��A�^5A��A��uA�;dA�1A���A��7A��A�7LA���A��^A�l�A�I�A��A���A�ƨA�I�A���A��A�XA�A�A�1A���A�l�A��A�A��A�5?A��A��`A���A�/A�oA���A���A��FA��A��uA�n�A�XA�7LA��A���A�`BA��A��;A��9A���A�p�A�33A��A���A��+A��TA���A�l�A�K�A��A���A��#A��^A���A��A�t�A�bNA�C�A�1'A��A�G�A���A�5?A�/A�-A�+A�+A�&�A�JA��FA��A��uA�-A�ȴA�^5A��A�ƨA��+A�VA�bA���A�S�A���A��PA�
=AO�A~��A}�mA|�`A{�hAz��Az��AzI�AzbAy�mAy�;Ay�^Ay\)Ax��Ax��AxZAwƨAw\)Av��AuG�At�AtbNAtM�At=qAs�AsG�Ar5?Ap�AoƨAn��An��AnI�Am�;Am7LAmoAm�Am�Am�Al�Al��Al�\AljAl1'Ak�#Ak`BAj�Aj��Ajv�Aj  AihsAh��Ag�-Af�!Ae�hAd��AdVAc��AcXAb��AaG�A`bNA`�A_�A_�wA_��A_/A^ �A]XA\�A\I�A\�A[�^AZr�AY?}AX�/AX��AXz�AX  AW��AW��AWO�AW�AV�/AV�uAV=qAUAUhsAU"�AT�AT�uASƨAR��AQx�AP��AP$�AO��AO��AOdZAO/AOVAN�/AN��AN�!AN�DANn�AN�AMK�AL�AKC�AJbNAIXAH�HAH�DAG�AG?}AFȴAF9XAE�AEl�AEVADbNACACoAB��ABVAA�PAAdZAA/A@��A@�A@��A@jA@$�A?��A?��A>�/A=�wA=+A<�yA<��A<jA<I�A<$�A< �A<$�A<bA;��A;�A;�;A;�A;"�A:��A:�jA:�!A:�\A:z�A:ffA:E�A:$�A9�A9��A9l�A9"�A8�jA8��A8=qA7��A7
=A6~�A6VA6�A5�#A5�^A5��A5|�A5l�A5XA5&�A5�A4�HA4�A4��A4r�A49XA4bA3A3�PA3C�A3
=A2�DA1�TA1\)A0�HA1A1�A0��A0E�A0JA/�^A/K�A.��A.�uA.ffA.=qA.1A-�#A-��A-�-A-�-A-��A-S�A-/A,�A,�`A,�A,��A,��A,��A,��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        Aї�Aѥ�Aѥ�AѰ!AѶFAѮAѰ!AѮAѩ�Aѩ�Aѧ�Aѧ�AѮAѺ^AѴ9AѴ9AѺ^AѺ^AѾwA���A�ȴA���A���A���A���A���A���A��
A��
A���A���A���A���A���A���A��
A���A�ȴA�ȴAѴ9AуAѡ�A���A��AёhA�oA��A�^5A��#A��+A�A���A�A�A���A�`BA��A��\A�jA���A���A�XA�bA�l�A�|�A�~�A��!A�XA���A�n�A{l�AyhsAvVAs�An9XAl�Akx�Ah��Ac�A`A\�AY7LAWdZAU�hAR5?AO`BANz�AJ�AF�ADbAAt�A@JA<�A<{A;?}A:jA9O�A7�A5��A4��A3�A1�A0z�A.��A-�-A-%A,�yA,^5A*��A(��A&��A&��A&�A&9XA%�mA%�FA&��A'XA'x�A'K�A'�A&�RA&-A%�
A%oA$��A$M�A#ƨA#|�A#VA"ZA!��A!XA �RA  �Al�A�A~�A1'A`BA%A�/A��A��A��AO�A"�A��A��AVAƨA`BA"�A�HAVA�A�
A�AG�A��A�\A9XAbA�mA�^A�7AhsA��Av�AA�AbA�AAdZA�/A��AZAJA��A��A\)AA�AĜAjA��AA�hAdZAVA��A��A�/AȴA��A�RAv�AJAA�7Ap�A7LAVA�AbNA�A��Al�A+AA
��A
�A
�uA
�\A
�\A
�\A
ffA	�-AȴA��AbNA5?A�A�A��A��A�!A�A��A�DAI�A|�A��A��AffAA�A5?A$�A �A�A{AƨAp�A��A�yA�jA �A�mA?}A ��A �HA �A Q�@��@�;d@���@��T@���@�ƨ@�\)@�^5@�?}@�j@�C�@���@��!@�M�@��T@���@��@�t�@���@��`@�r�@�F@�dZ@���@�-@�`B@���@�r�@�Q�@�9X@�1@�|�@��y@�^5@��#@�X@��/@�r�@��
@�dZ@�ȴ@�=q@��@��@��@�j@�D@�@�j@� �@�l�@��@�V@�A�@��;@�l�@��y@ާ�@�E�@ݙ�@�O�@ܣ�@�(�@��m@�S�@ڇ+@��@٩�@�O�@�Z@��m@�|�@�;d@ָR@�-@�x�@�1'@��
@Ӯ@�|�@�\)@�K�@�"�@��H@ҧ�@�V@�J@���@ѩ�@��@�r�@��@�t�@��@��@�(�@��;@˾w@�S�@�@ʸR@�=q@ɡ�@�O�@�%@ȓu@��m@ǍP@���@��@Ų-@ŉ7@�x�@��@�z�@�bN@� �@�ƨ@�o@�@�ff@�$�@��T@���@�7L@��@��@�bN@��@�l�@�o@��y@���@�$�@��-@��@��u@�j@���@��;@�ƨ@��F@��P@�+@��@���@��+@�^5@���@�O�@���@� �@��@�t�@��R@�v�@�@�&�@���@�1'@��@��;@��
@���@��H@�ff@�=q@��T@�p�@�?}@�V@��@���@�S�@���@�ff@��@���@�X@���@�r�@�9X@��m@���@�o@�ȴ@�ff@��@���@��7@�&�@��9@�bN@�1'@��@��;@���@�l�@�;d@�
=@�ȴ@�v�@��@���@��-@�p�@���@��9@�j@�9X@�ƨ@�l�@�;d@�o@��@���@�M�@��T@���@�?}@���@��u@���@��
@���@��F@��@�33@��R@�E�@��@��@���@�hs@�V@��u@�Z@�(�@�  @��F@�K�@���@�v�@���@��@��@��u@�Q�@���@��P@�t�@�;d@�o@��H@���@�V@�@��h@�O�@��@���@���@��u@�A�@��;@�l�@�o@���@�{@���@��@��-@��7@�O�@�?}@�7L@�&�@�V@��@�r�@�I�@��m@�l�@�"�@��@��!@�5?@���@�@���@�O�@��@�Ĝ@�r�@�(�@���@��w@�S�@�"�@�~�@�$�@���@�x�@�G�@��@�z�@�ƨ@�l�@�S�@�C�@��@���@�M�@�E�@�5?@��@��#@���@��@�hs@�`B@�7L@���@��9@�r�@�(�@���@���@�l�@�S�@��@���@��\@�v�@�V@�$�@���@��@�`B@�O�@�G�@�/@��9@�j@��@��@l�@;d@
=@~ȴ@~��@~@}��@}?}@}V@|�/@|�D@|Z@{C�@z�!@z~�@z-@y�@xA�@w�;@wl�@w�@w
=@v��@v�R@u�T@u�-@u`B@t��@t9X@s��@sS�@r�H@r�\@rJ@q��@q��@qx�@q7L@pĜ@pQ�@o�w@n��@nff@n@m�@m/@l��@l�/@l��@l��@lj@kƨ@k�@kS�@j�@j�!@j=q@i�^@ihs@i%@h1'@g��@f�y@fV@e��@e�@eO�@d�@dI�@c��@b��@a��@a�^@aG�@`��@`r�@`1'@`  @_��@_�P@_\)@_�@^ȴ@^�+@^E�@]p�@\�@[�@[dZ@[S�@Z��@ZJ@Y�^@Y��@YX@Y%@X�`@XĜ@X��@XQ�@X1'@X1'@X �@W��@V��@U@UV@T�j@Tz�@TZ@T(�@T1@S�m@S�
@Sƨ@SS�@SC�@S33@R�!@R�\@R~�@Rn�@RJ@Qx�@PĜ@P�@PQ�@P  @O�P@Nff@M�-@M��@M�h@M`B@M?}@L��@L��@Lz�@L1@KdZ@J�H@J~�@JJ@I��@I�7@I�7@Ihs@H�`@H �@G�;@G�@G�P@Gl�@G+@F�@F��@F��@Fv�@Fff@F5?@E�-@Ep�@E�@D�@D��@C��@C"�@B��@B�\@B^5@B-@B�@A�#@A��@A7L@@�`@@r�@?;d@>��@>{@=�h@=O�@=V@<��@<�D@<j@<j@<9X@;�@;C�@:�@:��@:�!@:�\@:�\@:n�@9�#@9G�@97L@9&�@8��@8�@8Q�@81'@8b@7�;@7�w@7�@7|�@7|�@7\)@6ȴ@6v�@6V@6$�@5�-@5/@4��@4�D@4Z@3��@3��@333@2�\@2^5@1�@1��@1��@1x�@1�@0�`@01'@/�;@/�P@/l�@/\)@/;d@.�y@.ff@.$�@-�-@,�@,�D@,z�@,I�@,�@+��@+�F@+C�@+@*��@*�\@*M�@)�@)��@)�^@)X@)%@(�9@( �@'�@'�w@'\)@';d@';d@&��@&�@&��@&ff@%�-@%/@$��@$�@$1@#��@#ƨ@#33@#o@"�H@"��@"n�@!��@!��@!��@!�@ �`@ �u@ A�@ b@�@�w@l�@�@�R@�+@ff@5?@�@�-@p�@/@�@�/@�@9X@�
@�
@ƨ@��@dZ@"�@o@�@��@��@��@~�@n�@^5@�@��@��@��@G�@�@��@��@Q�@b@�;@|�@l�@�@�@E�@@@�@p�@O�@/@�j@�D@z�@�@�m@�
@�F@�F@t�@S�@o@�H@n�@��@�^@��@hs@X@X@&�@�`@�`@Ĝ@�u@�@r�@1'@1'@ �@ �@1'@1'@1'@��@l�@l�@l�@\)@\)@\)@;d@�@��@ȴ@�R@��@�+@ff@$�@@�@�T@@�h@`B@?}@V@�@�j@z�@j@1@ƨ@��@t�@dZ@dZG�O�Aѕ�Aѕ�Aѕ�Aѝ�Aѝ�Aѥ�Aѧ�Aѩ�Aѣ�Aѡ�Aѧ�Aѣ�Aѥ�Aѣ�Aѧ�AѶFAѼjAѸRAѶFAѲ-AѴ9AѰ!AѬAѩ�AѮAѴ9AѬAѮAѴ9AѲ-Aѧ�Aѧ�Aѩ�AѮAѩ�Aѧ�AѬAѬAѧ�Aѥ�Aѧ�AѬAѩ�Aѧ�Aѥ�Aѧ�AѬAѧ�Aѡ�Aѧ�Aѧ�Aѥ�AѬAѰ!AѴ9AѰ!AѮAѲ-AѺ^AѺ^AѸRAѺ^AѼjAѼjAѶFAѸRAѶFAѴ9AѬAѮAѶFAѸRAѶFAѴ9AѶFAѴ9AѬAѮAѶFAѶFAѺ^AѸRAѼjAѾwAѼjAѺ^AѺ^AѼjAѼjAѺ^AѺ^AѾwAѼjAѺ^AѸRAѺ^AѼjAѼjAѸRAѾwA���A�A�ƨA�ȴA���A�ȴA�ȴA���A���A���A���A�ƨA�ƨA�ĜA�ȴA�ȴA���A���A�ȴA�ƨA���A���A�ȴA�ȴA���A���A���A���A���A���A���A�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��#A��
A��A��#A��#A���A���A��#A���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��
A��A��#A��HA���A���A���A�ĜA�ĜA�ĜA�ȴA���A�ȴA�ƨA���A���A���A�ĜA�ĜA�ƨA�ȴA�ƨA�ĜA�ĜAѺ^AѶFAѰ!Aџ�Aћ�Aџ�AѓuA�v�A�r�A�v�A�~�AхAщ7AёhAї�Aѩ�A�ĜA�ĜA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A��
A��A���A��A��HAѲ-AэPA�~�A�ffA�S�A�9XA�&�A�"�A��A��A�VA�A���A��A��A��A��A��`A��`A��TA��mA��`A��HA��/A��/A��;A��;A��#A��A��A��A��/A��A���A���A���A�G�Aϣ�A�dZA�bA��TA���Aΰ!AΧ�AΣ�AΩ�AΥ�AΝ�AΙ�AΙ�AΓuAΑhAΑhA΍PA΅AΉ7A΋DA΋DAΉ7A΅A΁A�|�A�~�A�~�A�z�A�v�A�r�A�r�A�r�A�l�A�hsA�ffA�jA�jA�l�A�hsA�bNA�bNA�dZA�hsA�hsA�jA�ffA�bNA�dZA�hsA�jA�hsA�hsA�jA�p�A�r�A�r�A�n�A�l�A�jA�l�A�l�A�n�A�jA�hsA�ffA�ffA�ffA�`BA�ZA�VA�O�A�M�A�G�A�C�A�?}A�9XA�33A�+A�"�A��A��A�JA�A�  A�A���A��A��A��`A��HA��;A��#A���A���A�ȴA���A���A;wAͼjA���A���A;wA���A�A���A��A��#A��A��TA��A���A���A���A���A�  A�%A�
=A�bA�bA�VA�
=A�A���A��A��mA��;A���AͼjAͶFAͮAͣ�A͗�A�p�A̼jA��A�l�AʓuA�XAɸRAɍPAȝ�A�v�AżjA�dZA�A�bNA�bNA��A���A���A�n�A���A��!A�dZA�`BA���A�r�A�t�A�jA�r�A��A���A���A���A��A��^A�dZA�O�A��7A��;A�jA��uA��#A��9A��A��A�G�A��uA�A���A�jA�1'A���A���A���A�M�A�JA��-A�A�A��A���A��A��-A�^5A��-A���A�?}A���A�dZA�=qA�JA��wA�bNA�/A���A�S�A���A�M�A�bA���A��9A���A���A��A�n�A�VA�G�A�7LA�+A�(�A�+A�+A�(�A�(�A� �A��A���A��A���A�t�A��A�^5A��A��uA�;dA�1A���A��7A��A�7LA���A��^A�l�A�I�A��A���A�ƨA�I�A���A��A�XA�A�A�1A���A�l�A��A�A��A�5?A��A��`A���A�/A�oA���A���A��FA��A��uA�n�A�XA�7LA��A���A�`BA��A��;A��9A���A�p�A�33A��A���A��+A��TA���A�l�A�K�A��A���A��#A��^A���A��A�t�A�bNA�C�A�1'A��A�G�A���A�5?A�/A�-A�+A�+A�&�A�JA��FA��A��uA�-A�ȴA�^5A��A�ƨA��+A�VA�bA���A�S�A���A��PA�
=AO�A~��A}�mA|�`A{�hAz��Az��AzI�AzbAy�mAy�;Ay�^Ay\)Ax��Ax��AxZAwƨAw\)Av��AuG�At�AtbNAtM�At=qAs�AsG�Ar5?Ap�AoƨAn��An��AnI�Am�;Am7LAmoAm�Am�Am�Al�Al��Al�\AljAl1'Ak�#Ak`BAj�Aj��Ajv�Aj  AihsAh��Ag�-Af�!Ae�hAd��AdVAc��AcXAb��AaG�A`bNA`�A_�A_�wA_��A_/A^ �A]XA\�A\I�A\�A[�^AZr�AY?}AX�/AX��AXz�AX  AW��AW��AWO�AW�AV�/AV�uAV=qAUAUhsAU"�AT�AT�uASƨAR��AQx�AP��AP$�AO��AO��AOdZAO/AOVAN�/AN��AN�!AN�DANn�AN�AMK�AL�AKC�AJbNAIXAH�HAH�DAG�AG?}AFȴAF9XAE�AEl�AEVADbNACACoAB��ABVAA�PAAdZAA/A@��A@�A@��A@jA@$�A?��A?��A>�/A=�wA=+A<�yA<��A<jA<I�A<$�A< �A<$�A<bA;��A;�A;�;A;�A;"�A:��A:�jA:�!A:�\A:z�A:ffA:E�A:$�A9�A9��A9l�A9"�A8�jA8��A8=qA7��A7
=A6~�A6VA6�A5�#A5�^A5��A5|�A5l�A5XA5&�A5�A4�HA4�A4��A4r�A49XA4bA3A3�PA3C�A3
=A2�DA1�TA1\)A0�HA1A1�A0��A0E�A0JA/�^A/K�A.��A.�uA.ffA.=qA.1A-�#A-��A-�-A-�-A-��A-S�A-/A,�A,�`A,�A,��A,��A,��A,��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B�B�zB��B��B�FB��B�zB��B�B��B��B�zB��B�zB��B��B�B��B�B�RB��B��B��B��B�$B��B�6B�^B��B��B�zB��B�LB�BB�EB�XB҉B�;B�B��B
	B�B5�BMjB�B2aB_pB
��B
UgB
J�B
B�B
L�B
GzB
C-B
?B
#�B
eB
/OB
,B
xB
"B	��B	��B	��B	��B	��B	��B	�1B	��B	|�B	i�B	[WB	S�B	VB	Q�B	>�B	-�B	#nB		B	JB	B	�B��B�B� B�;BٴB��BԕB��B��B��B�yB�dB�QB�;B��B�PB	AB	
�B	=B	%�B	0UB	5tB	>�B	N�B	XyB	hsB	s�B	��B	��B	�fB	�B	�B	�B	�`B	�B	�"B
  B
�B

=B
�B
hB
�B
kB
�B
$�B
'RB
(�B
,qB
-�B
/�B
0UB
1'B
2-B
2-B
5B
5tB
5B
5?B
5�B
5?B
8RB
<�B
:�B
;0B
9�B
8B
7�B
7�B
9XB
:�B
:�B
=<B
>B
=B
>�B
?�B
@OB
A�B
C-B
B�B
B�B
B[B
A B
?B
>wB
>BB
=�B
>B
>BB
>�B
>�B
?HB
@�B
?}B
?B
@�B
C-B
B�B
C�B
FtB
F�B
GB
F�B
G�B
GB
F�B
FtB
FtB
F?B
E�B
E�B
E�B
EmB
DgB
C�B
C�B
CaB
C-B
B�B
B�B
B[B
A�B
@�B
A B
@�B
A B
@�B
@�B
@�B
@B
?�B
@B
@�B
?�B
>�B
>BB
>BB
=�B
>B
>B
<jB
:�B
:�B
:*B
9�B
9�B
8�B
7B
6�B
6B
5�B
5�B
5�B
5tB
5B
4�B
5B
49B
2�B
1�B
2�B
/�B
/�B
/�B
.}B
.B
-wB
-B
+kB
+B
)�B
(�B
(�B
'�B
&LB
&�B
&�B
&LB
$�B
$tB
$tB
$@B
$@B
$�B
#:B
#nB
$B
!�B
 �B
�B
!B
B
OB
�B
~B
�B
~B
~B
IB
~B
IB
xB
xB
B
�B
	B
qB
7B
kB
�B
B
�B
�B
�B
_B
�B
�B
YB
YB
�B
�B
MB
�B
B
{B
�B
FB
uB
B
B
@B
:B
uB
�B
�B
4B
�B
hB
�B
bB
�B
�B
�B
.B
�B
�B
�B
�B
�B
(B
�B
�B
(B
�B
�B
�B
�B
�B
VB
�B
�B
PB
�B
PB
B
JB
~B
�B
B
JB
JB
DB
xB
�B
�B
B
~B
PB
PB
�B
�B
"B
B
�B
�B
PB
�B
�B
�B
B
�B
�B
B
B
�B
�B
�B
�B
B
B
B
�B
�B
�B
"B
VB
�B
VB
"B
�B
�B
�B
�B
�B
"B
"B
�B
�B
(B
\B
�B
\B
�B
�B
�B
hB
 B
oB
B
B
hB
B
@B
oB
�B
B
uB
�B
�B
@B
FB
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
SB
B
�B
YB
�B
�B
�B
�B
+B
�B
�B
+B
�B
_B
�B
�B
�B
1B
�B
�B
eB
B
kB
�B
kB
7B
=B
	B
	B
=B
qB
qB
B
xB
CB
�B
xB
B
�B
B
�B
�B
~B
xB
B
B
~B
IB
IB
B
B
�B
OB
�B
OB
�B
�B
 �B
 'B
 'B
 \B
 'B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#B
#nB
$�B
%�B
%�B
%zB
&B
&�B
&�B
&LB
&LB
&�B
'RB
'�B
(�B
(�B
)*B
(�B
(�B
(�B
)*B
)�B
)�B
)�B
*0B
*�B
+B
+B
+�B
,B
,=B
,qB
,�B
-CB
-CB
-�B
-�B
.IB
.B
.�B
.�B
.�B
0UB
/�B
0UB
0UB
0�B
0�B
1[B
1�B
1�B
1�B
1�B
2-B
3�B
3hB
3hB
3hB
4B
3�B
4B
49B
4B
4B
49B
4�B
5B
5B
4�B
6B
6FB
6�B
6zB
6�B
7LB
7LB
7LB
7�B
7B
8�B
8RB
8�B
8�B
8RB
8RB
9$B
8�B
9�B
9XB
9XB
9�B
9XB
9XB
9XB
9�B
9XB
:�B
:^B
:^B
:*B
9�B
;�B
;0B
;0B
:�B
<6B
<B
<6B
<�B
<�B
<�B
<jB
<�B
=<B
=B
=qB
>wB
=�B
>wB
?B
>�B
?B
?HB
?HB
?HB
?HB
?HB
?B
?}B
?�B
@OB
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B'B
B'B
B'B
B'B
B�B
B�B
CaB
CaB
C-B
CaB
D3B
D3B
EmB
E9B
E9B
EmB
E9B
EmB
E�B
FB
GEB
GEB
GEB
G�B
G�B
HKB
HKB
H�B
H�B
H�B
H�B
H�B
IB
H�B
IB
I�B
K�B
K^B
K)B
J�B
L0B
K�B
K�B
L0B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
LdB
L0B
L�B
M�B
N<B
NpB
NpB
NpB
N�B
OB
N�B
OB
N�B
OB
OvB
OvB
OvB
P}B
PB
O�B
O�B
PB
P�B
QNB
QNB
QB
Q�B
QNB
S�B
S&B
R�B
R�B
S&B
R�B
S�B
S[B
S�B
S�B
TaB
T�B
TaB
UgB
T�B
U2B
T�B
U2B
VB
VmB
VmB
V�B
VmB
V�B
W
B
W?B
W�B
WsB
W�B
W?B
W�B
XB
XEB
X�B
XyB
X�B
YKB
Y�B
Y�B
ZQB
ZB
ZQB
ZB
Z�B
Z�B
Z�B
Z�B
[WB
[�B
[�B
]/B
]�B
]�B
^B
^B
^jB
^B
^5B
^5B
_pB
_B
_B
_B
_;B
_;B
_B
_;B
`B
`B
`B
`B
`BB
`�B
`vB
`vB
`�B
aB
aB
aB
aHB
aB
aHB
a�B
bB
bB
a�B
b�B
cTB
cTB
cTB
c�B
c�B
c�B
c�B
dZB
dZB
d�B
d�B
d�B
d�B
e�B
e,B
f2B
ffB
ffB
f�B
f�B
ffB
gmB
g8B
g8B
h>B
iDB
h�B
iDB
i�B
i�B
i�B
jKB
j�B
kB
k�B
k�B
l"B
l�B
l�B
l�B
m)B
m]B
n/B
o B
o B
o5B
o�B
o�B
o�B
o�B
o�B
pB
o�B
p�B
p�B
qAB
qAB
qAB
qvB
p�B
rB
rB
qAB
qvB
rB
rGB
r|B
rGB
sB
r�B
s�B
s�B
sMB
s�B
t�B
t�B
t�B
t�B
uZB
u�B
u�B
u�B
u�B
u�B
v�B
v+B
v`B
v�B
wfB
wfB
wfB
w�B
xB
xlB
x�B
y	B
y>B
x�B
x�B
y>B
yrB
y�B
y>B
yrB
zB
zxB
zDB
zB
zB
zB
z�B
{B
{B
{JB
{JB
{�B
|PB
{�B
}�B
}VB
}�B
}�B
}VB
}�B
}VB
~�B
~�B
~�B
.B
~�B
�B
�B
cB
� B
�4B
�B
� B
�iB
��B
�B
�;B
�;B
�oB
�oB
�oB
��B
��B
�AB
�AB
�AB
�uB
�{B
��B
�B
�{B
�GB
��B
�B
�MB
�MB
��B
�B
��B
�MB
�B
��B
�B
��B
�B
��B
�B
��B
��B
��B
�YB
�%B
��B
��B
��B
�_B
�+B
�+B
��B
�_B
�1B
�1B
��B
�B
�B
��B
��B
��B
�fB��B��B��B�?B�B�LB�B�?B�B��B��B��B�B�B�B�B�?B��B��B��B��B��B�LB�B��B�tB�B�B��B��B�B��B�tB�B�B��B�zB��B�B�RB��B�FB�B��B��B�B�tB��B��B��B��B��B�zB�FB�tB��B��B�zB�B�FB�LB��B�?B�B��B��B�B�zB�RB�B�B��B��B�B�zB�B��B��B�B��B��B��B�zB�tB��B�LB��B��B�B��B��B��B�FB��B��B�LB��B��B��B��B�zB�zB�XB��B�B��B�RB��B�RB��B��B��B��B�zB��B��B��B�B��B�XB��B�B��B�XB�RB�B��B��B�RB��B��B��B��B��B�B��B��B�B��B�0B��B��B��B��B�dB��B��B�dB�6B��B�B�XB�jB��B��B��B��B�qB�^B��B��B�}B�<B��B�^B�B��B�XB��B��B�$B�*B�B��B��B�6B��B�$B��B��B��B�B��B��B��B�B��B�FB��B�FB�B�B��B��B�LB�LB��B��B��B��B�BB�[B��B�B��B��B�tB�B�^B��B�^B�XB�RB�BʌB�BȴB�<B�pB�NB��B�BیB�]B�]B��B��B��B�&B�&B�B�&B��B��B�B�B�B�TB��B�JB��B�B�B�B	�B	7B�B	7B
�B~BBB
rB�BxB�BB�B�B!�B%FB2�B6�B8�B=qBA BE9BH�BI�BL�BK�BNpBQBS[BUgBT�BT�BT�BW�BWsBW�BV9BV�BW�BX�BX�BW�BW?BX�BYBXyBWsBT�BV�BXEBW?BZ�Bv�B��B�:B��B�jB� BɺB�jB�0B��B˒B��B�jB�B�pB͟B�jBϫB҉B�B�B��B�HB�NB��B��B҉B�TBӏB�,B��B՛B�gB�?B�yBخB�sB�9B�B�?B�KB��B�EB�
B�
B�
B��B�BخB��B�
B�yB�yB�EB��B�9B�mB�?B�B�BרB�?B֡B��B�mBרB�
B�9BרB�QB�WB�WB�#B�]BܒBݘBޞB�B�B�TB�B�&B�
B�B�B�>B�WB�5B�;B��B��B�B�|B�TB�%B�2B�B��B��B�xB��B�DB�DB��B��B�`B�B��B�MB�5B�B�B�DB�B�DB�2B�&B��B�B�vB�B�B�B�ZB�2B�B�2B�B��B��B�B�B�B�8B�B;�B_�B�	B��B��B��B�yB	K�B	O�B	��B
!�B
OBB
J�B
_;B
u�B
c�B
rGB
OB
�:B
�|B
��B
uZB
�GB
�lB
�B
��B
�+B
�DB
��B
�1B
��B
��B
��B
��B
�@B
�B
�VB
��B
{�B
m�B
�uB
��B
n�B
c�B
e,B
]/B
W�B
VB
R�B
MjB
N�B
T�B
K�B
R�B
O�B
IB
GEB
DgB
D�B
J�B
Q�B
NB
[�B
NB
<�B
8�B
:�B
=�B
=�B
9�B
OvB
O�B
RTB
MB
LdB
K)B
GB
F�B
H�B
FB
HKB
E�B
HKB
F�B
D�B
DgB
B�B
C�B
B�B
AUB
CaB
A�B
Q�B
FtB
J#B
JXB
P�B
:�B
=B
>B
1'B
0�B
1�B
,qB
@�B
+6B
7B
~B
MB
 B
�B

�B
xB
�B
/�B
R�B
�B
�B
4B
�B
(�B
&LB
-�B
7B
T�B
YB
L�B
B�B
%�B
�B
 \B
OB
 �B
IB
�B
�B
B
~B
�B
$�B
�B
"hB
�B
B

�B
JB
 B
�B
;B
B
�B	��B	�+B
 �B	�xB	��B	��B	�iB	�B	�B	�B	�fB	�B	�&B	�B	��B	�B	��B	��B	֡B	՛B	��B	ҽB	�sB	��B	�B	��B	ѷB	��B	�B	�B	�qB	�B	�LB	�*B	��B	��B	��B	�nB	�CB	�B	�=B	�B	��B	�bB	��B	�bB	�B	�B	�YB	��B	��B	�DB	�	B	}�B	��B	z�B	zB	�(B	}�B	o B	iB	d�B	dZB	h
B	jKB	n�B	w2B	d&B	aHB	YB	T�B	[#B	^�B	P}B	QB	Q�B	TaB	W�B	Y�B	U�B	VB	T�B	X�B	\�B	R�B	R B	LdB	O�B	S�B	T�B	VB	M�B	J#B	>B	?�B	9$B	8�B	<6B	K�B	3�B	*eB	'B	%�B	#B	33B	:*B	�B	$�B	�B	�B	 �B	0UB	-wB	�B	�B	�B	+B	�B	~B		�B	
=B	
=B		�B		B		�B	�B��B	�B��B	\B	
rB	DB��B�>B�`B�B��B��B� B�B��B��B�B��B�"B��B�+B�]B�B��B�B�pB�B�vB�)B�/B��B��B��B�jB�5BܒB�B�B�|B�pB҉B��B҉B�pB�B�6B��B�B��B�|B�2B�|B�,B��B՛BרB�gB�,B�2B�2B��B��BخBߤB�sB�B�
B�BרB�B��B�B�5B��BܒB�B��B�#B�
B�WB�iB�"B��B��B�oB�B�5B�GB��B��B�vB�B��B�8B�fB�B��B�B�PB��B�]B��B	{B��B	
	B��B	 �B		�B	{B	�B	B	\B	�B	�B	�B	B	"�B	!bB	&�B	$@B	!�B	%�B	&B	,�B	.IB	2-B	1'B	0�B	4B	4B	4�B	4�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        B�FB��B��B�`B��B��B�`B��B�zB��B�B��B��B��B��B�`B��B��B��B��B�B�RB��B�lB��B��B�	B��B�PB�^B��B�xB�zB��B�LB�wB�_B�rB�B�'B��B�B	�BHB9�Bd&B!HBMjB��B
�/B
d�B
_B
R:B
T,B
J�B
SB
R�B
:�B
5%B
M�B
4�B
'RB
B
�B	�+B	�]B	�>B	�B	��B	��B	��B	��B	x8B	`B	YeB	`�B	a|B	KxB	88B	/�B	!HB	�B	HB	�B�`B��B��B�_B�B�7BބB޸B��B�B��B�B�;B��B��B	�B	�B	�B	�B	($B	0�B	7�B	DMB	VSB	^�B	iB	r�B	��B	��B	��B	�?B	�uB	�B	��B	�PB	�wB
�B
	RB
�B
 B
�B
�B
�B
 �B
'RB
(�B
*�B
.�B
0B
2-B
2-B
2�B
3hB
4�B
6FB
6+B
6+B
7fB
6�B
6`B
8�B
="B
;�B
<�B
;�B
9�B
8�B
8�B
;0B
;B
<B
>]B
>�B
>(B
@iB
A B
@�B
B[B
C�B
C�B
CaB
DgB
B[B
?�B
?.B
>�B
>�B
?cB
@B
?�B
?�B
@�B
B'B
?�B
?�B
B'B
C�B
C-B
ESB
G�B
G�B
G�B
G_B
H�B
G_B
F�B
F�B
F�B
FYB
FB
F�B
GB
FtB
E9B
DgB
D�B
C�B
C�B
D�B
DgB
CaB
B[B
A�B
A�B
AoB
A�B
A;B
AB
@�B
@4B
@�B
B�B
DB
@�B
?}B
>�B
>�B
>�B
?cB
@�B
<�B
;B
:�B
:�B
:�B
<PB
:�B
8lB
7�B
6�B
6B
6B
5�B
5�B
5?B
5�B
6FB
5�B
3B
2�B
4�B
0�B
1�B
0�B
/ B
.�B
.�B
.cB
,�B
,=B
+B
*B
*�B
(�B
(>B
(�B
(�B
(>B
%zB
%B
%,B
%,B
&LB
%�B
$�B
&fB
%�B
"�B
"NB
�B
�B
�B
�B
�B
5B
B
�B
�B
OB
�B
OB
~B
~B
�B
xB
CB
]B
WB
qB
�B
�B
eB
eB
�B
yB
�B
+B
�B
B
�B
+B
B
�B
B
�B
�B
�B
B
FB
�B
�B
[B
�B
�B
�B
B
:B
:B
�B
 B
 B
B
hB
TB
�B
.B
.B
�B
�B
vB
B
B
�B
B
(B
vB
�B
�B
\B
�B
BB
}B
HB
�B
dB
B
B
~B
B
jB
�B
�B
dB
B
jB
6B
"B
"B
�B
�B
�B
(B
jB
jB
�B
�B
�B
PB
jB
�B
6B
�B
"B
jB
6B
PB
"B
�B
jB
�B
"B
�B
�B
�B
�B
(B
�B
�B
VB
VB
�B
\B
BB
�B
�B
BB
�B
�B
bB
�B
�B
�B
�B
B
�B
oB
�B
�B
:B
 B
B
[B
,B
�B
�B
�B
�B
@B
�B
�B
�B
�B
gB
B
MB
�B
�B
mB
9B
SB
SB
�B
�B
�B
YB
�B
�B
sB
_B
_B
�B
�B
+B
�B
�B
�B
�B
KB
�B
�B
7B
�B
B
#B
�B
kB
�B
	B
�B
qB
WB
�B
�B
B
�B
�B
B
dB
IB
5B
5B
/B
B
IB
B
dB
B
�B
�B
�B
B
�B
B
�B
�B
�B
�B
�B
 �B
!bB
!�B
!-B
 �B
 �B
!HB
!|B
"�B
!�B
!�B
"NB
"4B
"NB
"NB
#B
#:B
#�B
#�B
%,B
%�B
%�B
&B
&�B
'�B
'�B
'B
'RB
&�B
'�B
(XB
)DB
(�B
)DB
)B
)*B
)*B
)�B
*eB
*eB
*eB
+B
+kB
+kB
+�B
,�B
,�B
,�B
,�B
-CB
-�B
-�B
.IB
.IB
.�B
.�B
/�B
/iB
/�B
1B
0�B
0�B
0�B
1AB
1vB
2�B
2�B
1�B
2-B
2aB
3B
4B
3�B
3�B
3�B
49B
49B
4TB
4nB
4B
4TB
4�B
5ZB
5�B
5�B
5�B
6zB
6�B
6�B
6�B
7�B
7�B
7�B
7�B
7�B
7�B
9	B
8�B
8�B
8�B
8�B
9>B
9�B
9XB
:B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
:xB
:�B
<B
;dB
;�B
<B
=B
<jB
<�B
<�B
<�B
<�B
<�B
=qB
=qB
=qB
>(B
>�B
>(B
?B
?}B
?.B
?�B
?�B
?}B
?}B
?�B
?�B
?�B
@B
@�B
@�B
AUB
AoB
A�B
A�B
A�B
A�B
A�B
A�B
B�B
BuB
B[B
B�B
B�B
CGB
C�B
C�B
C�B
D3B
D�B
EB
E�B
E�B
E�B
E�B
E�B
FB
F?B
G+B
HB
G�B
G�B
HKB
HKB
H�B
H�B
H�B
H�B
H�B
IB
I7B
IlB
I7B
I�B
K)B
LB
KxB
KDB
KxB
L�B
LJB
LB
L~B
MB
L�B
L�B
L�B
L�B
L�B
L�B
L~B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O(B
N�B
O(B
N�B
OvB
O�B
O�B
O�B
P�B
P.B
O�B
PHB
P�B
QNB
Q�B
Q�B
Q�B
RB
RoB
TaB
S@B
SB
S&B
S[B
S&B
S�B
S�B
TB
T,B
T�B
UgB
T�B
U�B
UB
U2B
U2B
U�B
V�B
V�B
V�B
V�B
V�B
W$B
WYB
WsB
W�B
W�B
W�B
WsB
X+B
X_B
X�B
X�B
X�B
YB
ZB
Z7B
Z7B
Z�B
ZQB
ZkB
ZkB
Z�B
Z�B
[=B
[=B
\xB
\�B
\xB
]�B
]�B
^B
^OB
^OB
^�B
^B
^jB
^�B
_�B
_VB
_!B
_!B
_VB
_;B
_;B
_�B
`�B
`'B
`'B
`BB
`�B
`�B
`�B
`�B
aB
a-B
a-B
aHB
aHB
aHB
a�B
b4B
bNB
bNB
bhB
c:B
c�B
c�B
c�B
c�B
dB
d@B
d�B
d�B
d�B
eB
d�B
eB
d�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
i*B
i_B
h�B
iyB
i�B
jB
j0B
j�B
k6B
kQB
k�B
k�B
l�B
l�B
l�B
m)B
mwB
m�B
n�B
o5B
o5B
o�B
o�B
o�B
pB
o�B
o�B
pUB
pUB
q[B
qvB
q�B
q[B
q[B
q�B
qvB
r-B
rGB
q[B
q�B
r|B
r|B
r�B
r�B
sMB
s3B
s�B
s�B
s�B
s�B
t�B
t�B
uZB
u%B
utB
u�B
vB
vFB
vFB
vFB
v�B
vzB
v�B
wfB
w�B
wfB
w�B
w�B
xRB
x�B
x�B
y$B
yXB
x�B
x�B
yXB
y�B
y�B
y�B
y�B
z*B
z�B
z�B
zDB
zDB
z^B
z�B
{�B
{JB
{�B
{dB
|B
|�B
|�B
}�B
}�B
}�B
}�B
}qB
}�B
}�B
~�B
~�B
~�B
cB
B
�B
�B
�B
�4B
��B
� B
��B
��B
��B
� B
�oB
�UB
�oB
��B
��B
��B
��B
�uB
�[B
�[B
��B
�{B
��B
�B
�{B
�GB
��B
�{B
��B
�MB
��B
�3B
��B
�MB
�3B
��B
�MB
�B
�9B
��B
�9B
��B
��B
�B
�tB
�?B
��B
��B
��B
�zB
�_B
�_B
��B
��B
�KB
��B
��B
�7B
�B
��B
��B
��G�O�B��B��B��B�?B�B�LB�B�?B�B��B��B��B�B�B�B�B�?B��B��B��B��B��B�LB�B��B�tB�B�B��B��B�B��B�tB�B�B��B�zB��B�B�RB��B�FB�B��B��B�B�tB��B��B��B��B��B�zB�FB�tB��B��B�zB�B�FB�LB��B�?B�B��B��B�B�zB�RB�B�B��B��B�B�zB�B��B��B�B��B��B��B�zB�tB��B�LB��B��B�B��B��B��B�FB��B��B�LB��B��B��B��B�zB�zB�XB��B�B��B�RB��B�RB��B��B��B��B�zB��B��B��B�B��B�XB��B�B��B�XB�RB�B��B��B�RB��B��B��B��B��B�B��B��B�B��B�0B��B��B��B��B�dB��B��B�dB�6B��B�B�XB�jB��B��B��B��B�qB�^B��B��B�}B�<B��B�^B�B��B�XB��B��B�$B�*B�B��B��B�6B��B�$B��B��B��B�B��B��B��B�B��B�FB��B�FB�B�B��B��B�LB�LB��B��B��B��B�BB�[B��B�B��B��B�tB�B�^B��B�^B�XB�RB�BʌB�BȴB�<B�pB�NB��B�BیB�]B�]B��B��B��B�&B�&B�B�&B��B��B�B�B�B�TB��B�JB��B�B�B�B	�B	7B�B	7B
�B~BBB
rB�BxB�BB�B�B!�B%FB2�B6�B8�B=qBA BE9BH�BI�BL�BK�BNpBQBS[BUgBT�BT�BT�BW�BWsBW�BV9BV�BW�BX�BX�BW�BW?BX�BYBXyBWsBT�BV�BXEBW?BZ�Bv�B��B�:B��B�jB� BɺB�jB�0B��B˒B��B�jB�B�pB͟B�jBϫB҉B�B�B��B�HB�NB��B��B҉B�TBӏB�,B��B՛B�gB�?B�yBخB�sB�9B�B�?B�KB��B�EB�
B�
B�
B��B�BخB��B�
B�yB�yB�EB��B�9B�mB�?B�B�BרB�?B֡B��B�mBרB�
B�9BרB�QB�WB�WB�#B�]BܒBݘBޞB�B�B�TB�B�&B�
B�B�B�>B�WB�5B�;B��B��B�B�|B�TB�%B�2B�B��B��B�xB��B�DB�DB��B��B�`B�B��B�MB�5B�B�B�DB�B�DB�2B�&B��B�B�vB�B�B�B�ZB�2B�B�2B�B��B��B�B�B�B�8B�B;�B_�B�	B��B��B��B�yB	K�B	O�B	��B
!�B
OBB
J�B
_;B
u�B
c�B
rGB
OB
�:B
�|B
��B
uZB
�GB
�lB
�B
��B
�+B
�DB
��B
�1B
��B
��B
��B
��B
�@B
�B
�VB
��B
{�B
m�B
�uB
��B
n�B
c�B
e,B
]/B
W�B
VB
R�B
MjB
N�B
T�B
K�B
R�B
O�B
IB
GEB
DgB
D�B
J�B
Q�B
NB
[�B
NB
<�B
8�B
:�B
=�B
=�B
9�B
OvB
O�B
RTB
MB
LdB
K)B
GB
F�B
H�B
FB
HKB
E�B
HKB
F�B
D�B
DgB
B�B
C�B
B�B
AUB
CaB
A�B
Q�B
FtB
J#B
JXB
P�B
:�B
=B
>B
1'B
0�B
1�B
,qB
@�B
+6B
7B
~B
MB
 B
�B

�B
xB
�B
/�B
R�B
�B
�B
4B
�B
(�B
&LB
-�B
7B
T�B
YB
L�B
B�B
%�B
�B
 \B
OB
 �B
IB
�B
�B
B
~B
�B
$�B
�B
"hB
�B
B

�B
JB
 B
�B
;B
B
�B	��B	�+B
 �B	�xB	��B	��B	�iB	�B	�B	�B	�fB	�B	�&B	�B	��B	�B	��B	��B	֡B	՛B	��B	ҽB	�sB	��B	�B	��B	ѷB	��B	�B	�B	�qB	�B	�LB	�*B	��B	��B	��B	�nB	�CB	�B	�=B	�B	��B	�bB	��B	�bB	�B	�B	�YB	��B	��B	�DB	�	B	}�B	��B	z�B	zB	�(B	}�B	o B	iB	d�B	dZB	h
B	jKB	n�B	w2B	d&B	aHB	YB	T�B	[#B	^�B	P}B	QB	Q�B	TaB	W�B	Y�B	U�B	VB	T�B	X�B	\�B	R�B	R B	LdB	O�B	S�B	T�B	VB	M�B	J#B	>B	?�B	9$B	8�B	<6B	K�B	3�B	*eB	'B	%�B	#B	33B	:*B	�B	$�B	�B	�B	 �B	0UB	-wB	�B	�B	�B	+B	�B	~B		�B	
=B	
=B		�B		B		�B	�B��B	�B��B	\B	
rB	DB��B�>B�`B�B��B��B� B�B��B��B�B��B�"B��B�+B�]B�B��B�B�pB�B�vB�)B�/B��B��B��B�jB�5BܒB�B�B�|B�pB҉B��B҉B�pB�B�6B��B�B��B�|B�2B�|B�,B��B՛BרB�gB�,B�2B�2B��B��BخBߤB�sB�B�
B�BרB�B��B�B�5B��BܒB�B��B�#B�
B�WB�iB�"B��B��B�oB�B�5B�GB��B��B�vB�B��B�8B�fB�B��B�B�PB��B�]B��B	{B��B	
	B��B	 �B		�B	{B	�B	B	\B	�B	�B	�B	B	"�B	!bB	&�B	$@B	!�B	%�B	&B	,�B	.IB	2-B	1'B	0�B	4B	4B	4�B	4�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�S�<4}�<�y�=�aE<ĝ�<yYe<��$<���<#�
<#�
<�KD<���<�p4<�C�<�p^<#�
<6��<FN�<#�
<<c3<#�
<�)O<yH<��<#�
<.1�<7��<od9<#�
<#�
<2�~<r�<T5�<1WX<I�<#�
<#�
<:�l<#�
<#�
<T5�<G{E<+�<#�
<#�
<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019011616171920190116161719IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019012615003120190126150031QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019012615003120190126150031QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551320190521075513IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                