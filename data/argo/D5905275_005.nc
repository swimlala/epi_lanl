CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-03-04T23:26:17Z creation; 2023-04-26T19:14:25Z DMQC;      
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
_FillValue        G�O�     �  dt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180304232617  20230426191425  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_005                 7316_008644_005                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�P�k�}�@�P�k�}�11  @�P��?��@�P��?��@*7��n�@*7��n��d-�o�5��d-�o�5�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?��H@B�\@�G�@��R@��R@�  A ��A��A ��A,(�AAG�A`  A�  A�  A��A�Q�A�Q�A�  A߮A�  B (�B  B  B(�B (�B((�B0  B7�B@  BH  BP(�BX(�B`(�Bh  Bp(�Bx  B�
B��
B�  B�{B�  B�  B�  B�  B�  B��B�  B�{B�{B�=qB��B�B�  B�  B��B��B�  B��B��B��B��B�  B�  B�{B�  B��B�  B�{C {C{C  C  C  C
  C��C  C  C
=C  C��C
=C
=C��C  C   C!��C#��C&  C(  C*
=C+��C.  C/��C2  C3��C6  C8{C:
=C<  C=��C?��CA��CD
=CF  CG��CJ  CL  CN  CP{CR
=CT  CV  CW��CZ  C[��C^  C`
=Ca��Cc��Cf
=Ch
=Cj  Cl
=Cn
=Cp
=Cr  Cs��Cu��Cx  Cy��C{��C~  C�  C�  C�C�  C���C���C�  C�  C�  C�  C�C�  C�C�  C�  C�C�  C���C�  C�C�  C�C�C�  C���C���C���C�  C�
=C�  C���C�  C�C�  C�  C���C���C���C���C�C�C�  C�  C���C���C�  C���C���C���C�  C�C�C�  C���C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�C�
=C�C���C���C�  C�  C�C�C�C�  C���C�  C�C�C�  C�C�  C���C���C�  C�  C���C���C�  C���C���C���C���C�  C�  C���C�  C�C���C���C���C�C���C�  C�C���C���C���C�  C���C���C�C�C�C�  C�  C�  C�C�C�C�  C���C�C�  C�  C�  C�  D   D � D�D� D�qD}qD�qD}qD�qD� D  D� D  D� D�qD��D�D� D	  D	� D
�D
��D
�qD� D�D� D�qD��DD��D  D}qD�qDz�D��Dz�D�qD��D  D� D  D}qD  D��D�qD� D�D� D�qD}qD  D}qD  D�D�D� D�qD}qD  D��D  D��DD�D �D ��D!  D!}qD!�qD"z�D"��D#z�D#�qD$��D%  D%}qD&  D&�D'�D'� D(  D(}qD(�qD)z�D*  D*��D+�D+� D,  D,��D-�D-z�D.  D.��D.�qD/}qD/�qD0z�D0�qD1}qD2  D2� D3  D3� D4�D4��D5  D5}qD6  D6��D7  D7� D8  D8��D9  D9� D:�D:� D;  D;��D<  D<}qD=  D=��D>�D>}qD?  D?�D@�D@� D@�qDAz�DB  DB��DCDC��DD�DD��DEDE�DF�DF��DG�DG��DG�qDH}qDH�qDI}qDI�qDJ}qDK�DK� DL�DL�DM�DM� DM��DN}qDN�qDO� DP�DP��DQ  DQ� DQ�qDRz�DS  DS��DT�DT��DUDU� DU��DV}qDW  DW��DX�DX}qDX��DY}qDZ  DZ�D[  D[}qD[�qD\� D]�D]�D^  D^}qD_  D_� D`�D`��D`�qDa� Db�Db}qDb��Dc}qDd�Dd� Dd�qDe}qDf  Df��DgDg� Dh�Dh��Dh�qDiz�Dj  Dj�Dk�Dk� Dk�qDl}qDl�qDm� Dn  Dn� Do�Do}qDo�qDp}qDp�qDq� Dr  Dr��Ds  Ds��Dt  Dt}qDu  Du}qDu�qDv� Dw  Dw}qDw�qDx� Dy  Dy� Dy�qDz}qDz�qD{� D{�qD|}qD}  D}}qD~  D~��D  D� D�  D�@ D��HD�� D���D�>�D��HD�D�HD�@ D�~�D�� D�HD�@ D�� D�D�HD�>�D�� D�� D�HD�AHD�� D�� D�  D�AHD��HD�� D�  D�@ D�� D��HD�HD�@ D�� D��HD��D�@ D�� D��HD�HD�>�D�}qD��qD���D�>�D�~�D�� D���D�=qD�~�D�� D�HD�AHD�� D�� D�  D�AHD��HD���D�  D�@ D�~�D�� D�  D�AHD��HD��qD�  D�AHD���D�� D���D�@ D��HD�� D�  D�AHD��HD�� D���D�@ D�~�D���D���D�=qD�� D�� D�  D�AHD���D��HD�  D�>�D�� D��HD�  D�>�D�~�D���D���D�>�D�� D�� D�HD�AHD���D��HD�  D�AHD�� D���D�HD�@ D�� D�D�HD�=qD�~�D�� D���D�=qD�~�D���D��qD�@ D�~�D���D�HD�B�D���D���D�HD�@ D��HD�� D���D�@ D�~�D�� D��D�AHD��HD�� D�  D�=qD�}qD��HD�HD�>�D�� D��HD�  D�AHD���D�� D���D�>�D�� D�D�HD�>�D�~�D�� D��D�AHD��HD��HD�HD�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD�~�D�� D���D�<)D�}qD���D���D�>�D�~�D���D���D�@ D��HD��HD��D�B�D���D�� D��qD�=qD�~�D��qD���D�>�D�~�D���D���D�>�D�� D��HD��D�@ D�}qD�� D�HD�>�D�~�D���D���D�@ D�� D�� D�  D�@ D�� D���D�HD�AHD��HD�D�HD�@ D�� D��HD�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D�� D���D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D���D�=qDÀ D��HD�  D�AHDāHDľ�D���D�AHDŁHDž�D���D�@ Dƀ D�� D�  D�>�Dǀ D��HD�  D�@ D�~�D��HD�HD�@ DɁHD�� D���D�@ Dʀ D�� D���D�=qDˀ D�� D�  D�@ D̀ D��HD�  D�@ D�~�D�� D�HD�@ D΀ Dξ�D��qD�=qDπ D��HD�HD�@ DЀ D�� D�  D�@ D�~�DѾ�D�  D�@ DҁHD�� D�  D�>�D�}qD�� D�  D�@ DԁHDԾ�D�  D�>�D�}qDվ�D�  D�>�DցHD�D�  D�@ DׁHD��HD�HD�AHD؁HD�� D�  D�@ D�}qDٽqD���D�>�D�~�DڽqD��qD�@ DہHD��HD�HD�AHD܀ D�� D�  D�B�D݁HDݾ�D��qD�@ Dނ�D�D�  D�@ D߀ D��HD��D�@ D�� D��HD���D�>�D�}qD�qD���D�AHD� D⾸D��qD�@ D�HD�� D�  D�>�D�}qD侸D�HD�@ D�~�D�� D�HD�@ D�~�D澸D�  D�AHD�HD羸D�  D�AHD� D��HD�HD�@ D�~�D�� D�  D�=qD�~�D�� D�  D�@ D�HD��HD�  D�AHD�HD�� D�  D�@ D� D��HD�  D�=qD�~�DD�  D�>�D�~�D�D�HD�>�D�~�D��HD��D�@ D� D�� D�  D�@ D�HD��HD���D�@ D� D�qD���D�@ D� D�� D���D�=qD�}qD��qD���D�=qD�~�D���D���D�AHD���D��HD�  D�AHD��HD�� D���D�>�D�� D�� D�  D�AHD�p�D��3?\)?L��?�z�?�{?Ǯ?�ff@
=q@�@&ff@=p�@L��@Y��@xQ�@��@�{@�Q�@��@���@��H@�ff@�33@�p�@���@�33AG�A�A�AG�A�A{A#33A(��A/\)A6ffA<(�AA�AH��AP  AU�AZ�HAa�AhQ�Ao\)Atz�Az�HA���A�z�A�\)A�=qA�A���A��
A��RA��A��A��A��HA�{A���A��
A��RA��A���A�
=A�G�A��
A��RA���A\A�z�A�
=A���A��HA���A�
=A���Aҏ\A�(�A�{A�  A�G�A�33A��A�
=A�Q�A�=qA�(�A�ffA�A陚A�A�p�A�
=A���A��HA���A�ffA�Q�A��\A�(�A�A��B ��BBffB\)BQ�BG�B{B�HB  B��B	B
�\B�B��Bp�BffB�Bz�BG�B=qB�Bz�BG�B=qB�Bz�BG�BffB�B��BG�BffB\)B z�B!p�B"=qB#\)B$z�B%p�B&=qB'\)B(��B)p�B*=qB+\)B,��B-��B.ffB/\)B0z�B1��B2�\B3\)B4z�B5��B6�\B7\)B8��B9��B:�\B;\)B<��B=��B>ffB?\)B@z�BA��BBffBC33BDQ�BEG�BF=qBG
=BH  BI�BJ{BJ�HBK�BL��BM�BN�RBO�BP��BQBRffBS\)BTz�BUG�BV{BW
=BX(�BYG�BZ{BZ�HB\  B]�B]�B^�RB_�
B`��Ba�Bb�RBc�
Bd��BeBf�RBh  Bh��BiBj�RBk�
Bl��BmBn�RBo�Bp��Bq�Br�RBt  Bu�Bv{Bv�HBx  By�Bz=qB{
=B|(�B}�B~ffB\)B�{B���B�G�B�B�(�B���B�G�B��
B�=qB��HB��B�{B��\B��B��B�Q�B���B�p�B��B���B�33B��
B�Q�B��HB��B�(�B��RB�G�B��
B�ffB�
=B��B�=qB���B�\)B�  B��RB�G�B�B�ffB�
=B��B�=qB���B�G�B��B��\B�33B��B�=qB���B��B�(�B���B�33B�B�z�B�
=B��B�(�B���B�p�B�  B�z�B�
=B�B�ffB���B��B�{B��RB�\)B��B�z�B���B���B�=qB��HB�p�B��B��\B�33B�B�=qB��RB�\)B��B�z�B��HB�\)B��
B�ffB���B�33B�p�B��B�{B�Q�B�z�B��\B��\B��RB���B��RB��RB��RB��RB��HB��HB��HB���B���B��HB���B���B��HB��HB���B���B���B���B���B���B���B��HB��HB��HB���B���B���B��HB��HB��HB���B�
=B���B��HB��HB���B�
=B�
=B���B���B���B�
=B�
=B��B�
=B�
=B���B�
=B��B�33B��B��B��B�33B�G�B�G�B�33B�33B�G�B�\)B�p�B�\)B�\)B�\)B�p�B��B���B���B��B���B��B�B��
B��
B��
B��
B�  B�{B�{B�  B�{B�(�B�=qB�Q�B�ffB�z�B�ffB�ffB�z�B���B��RB���B���B���B���B��B��B��B�33B�G�B��B���B��B�B��
B��B�{B�=qB�Q�B�ffB�z�B�z�B��RB��HB�
=B��B�33B�\)B��B��B��B�{B�=qB�Q�B�z�B���B��HB��B�G�B�p�B���B�B�  B�(�B�ffB��RB���B��B�\)B��B�B��B�=qB��\B���B�
=B�33B�\)B��B��B�=qB�ffB£�B���B���B�33B�p�BîB�  B�(�B�Q�B�z�BĸRB���B�33B�p�BŮB�B�  B�=qB�z�B���B�
=B�G�B�p�BǮB��B�=qBȏ\B���B�33B�p�Bə�B��
B�(�B�z�B���B��B�p�BˮB��B�(�B�ffB̸RB��B�p�B�B�{B�Q�BθRB���B�G�BϮB�{B�ffB���B��B�p�B�B�{B�ffB���B��BӅB��B�ffBԸRB�
=B�\)B�B�{B�z�B��HB�G�B׮B�(�B؏\B���B�\)BٮB�{B�z�B���B�33Bۙ�B��B�=qBܣ�B���B�\)B�B�(�Bޏ\B���B�\)B��
B�=qB��\B�
=B�p�B��
B�=qB�\B�
=B�p�B��
B�=qB��B�
=B�p�B��
B�Q�B�RB�33B癚B�{B�\B���B�p�B��B�Q�B���B�G�B�B�(�B�\B���B�p�B��B�ffB��HB�G�B�B�=qB���B�33B�B�{B�\B�
=B�B�{B�\B�
=B��B�{B��\B�
=B��B�  B�z�B��HB�p�B�  B�z�B���B�p�B��B�z�B���B�p�B�  B�z�B���B�p�C   C =qC z�C �RC  C=qCz�C�RC  C33Cp�C�RC��C33Cz�C�RC  C=qCz�CC  CG�C�C��C
=CQ�C�\C�
C{C\)C��C�HC�CffC��C�HC	(�C	ffC	�C	�C
33C
p�C
�RC
��C=qCz�CC  CG�C�CC
=CQ�C�\C��C
=CQ�C�\C��C{CQ�C�\C�
C�C\)C�C��C33Cz�C�RC  C33Cz�C�RC��C=qC�C��C
=CG�C�C��C
=C=qCz�C�RC  C=qC�\C�
C{CQ�C��C��C{CQ�C��C�C(�CffC��C��C=qC�C�RC  CQ�C�\C��C{CQ�C�C�C(�Cp�CC
=C=qC�C�
C�C\)C��C��C =qC z�C C!{C!Q�C!��C!�HC"33C"ffC"�C#
=C#G�C#�\C#��C$�C$p�C$�C$��C%G�C%�\C%��C&�C&p�C&�RC'  C'\)C'��C'�C(G�C(�C(�HC)33C)p�C)��C*�C*ffC*�RC+
=C+\)C+��C+��C,Q�C,��C,�HC-=qC-�\C-��C.�C.z�C.C/
=C/\)C/�C/��C0=qC0��C0�C1=qC1z�C1�HC233C2z�C2C3�C3p�C3C4
=C4\)C4�C5
=C5Q�C5��C6
=C6\)C6��C7  C7\)C7�C8  C8\)C8�C9  C9\)C9�RC:
=C:ffC:��C;�C;p�C;�
C<33C<�C<�C==qC=��C=��C>G�C>�RC?
=C?ffC?��C@�C@�C@�HCA33CA�\CB  CBQ�CB�RCC�CCp�CC�
CD=qCD�\CD��CE\)CE�CF�CFz�CF��CG=qCG�\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                   ?u?��H@B�\@�G�@��R@��R@�  A ��A��A ��A,(�AAG�A`  A�  A�  A��A�Q�A�Q�A�  A߮A�  B (�B  B  B(�B (�B((�B0  B7�B@  BH  BP(�BX(�B`(�Bh  Bp(�Bx  B�
B��
B�  B�{B�  B�  B�  B�  B�  B��B�  B�{B�{B�=qB��B�B�  B�  B��B��B�  B��B��B��B��B�  B�  B�{B�  B��B�  B�{C {C{C  C  C  C
  C��C  C  C
=C  C��C
=C
=C��C  C   C!��C#��C&  C(  C*
=C+��C.  C/��C2  C3��C6  C8{C:
=C<  C=��C?��CA��CD
=CF  CG��CJ  CL  CN  CP{CR
=CT  CV  CW��CZ  C[��C^  C`
=Ca��Cc��Cf
=Ch
=Cj  Cl
=Cn
=Cp
=Cr  Cs��Cu��Cx  Cy��C{��C~  C�  C�  C�C�  C���C���C�  C�  C�  C�  C�C�  C�C�  C�  C�C�  C���C�  C�C�  C�C�C�  C���C���C���C�  C�
=C�  C���C�  C�C�  C�  C���C���C���C���C�C�C�  C�  C���C���C�  C���C���C���C�  C�C�C�  C���C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�C�
=C�C���C���C�  C�  C�C�C�C�  C���C�  C�C�C�  C�C�  C���C���C�  C�  C���C���C�  C���C���C���C���C�  C�  C���C�  C�C���C���C���C�C���C�  C�C���C���C���C�  C���C���C�C�C�C�  C�  C�  C�C�C�C�  C���C�C�  C�  C�  C�  D   D � D�D� D�qD}qD�qD}qD�qD� D  D� D  D� D�qD��D�D� D	  D	� D
�D
��D
�qD� D�D� D�qD��DD��D  D}qD�qDz�D��Dz�D�qD��D  D� D  D}qD  D��D�qD� D�D� D�qD}qD  D}qD  D�D�D� D�qD}qD  D��D  D��DD�D �D ��D!  D!}qD!�qD"z�D"��D#z�D#�qD$��D%  D%}qD&  D&�D'�D'� D(  D(}qD(�qD)z�D*  D*��D+�D+� D,  D,��D-�D-z�D.  D.��D.�qD/}qD/�qD0z�D0�qD1}qD2  D2� D3  D3� D4�D4��D5  D5}qD6  D6��D7  D7� D8  D8��D9  D9� D:�D:� D;  D;��D<  D<}qD=  D=��D>�D>}qD?  D?�D@�D@� D@�qDAz�DB  DB��DCDC��DD�DD��DEDE�DF�DF��DG�DG��DG�qDH}qDH�qDI}qDI�qDJ}qDK�DK� DL�DL�DM�DM� DM��DN}qDN�qDO� DP�DP��DQ  DQ� DQ�qDRz�DS  DS��DT�DT��DUDU� DU��DV}qDW  DW��DX�DX}qDX��DY}qDZ  DZ�D[  D[}qD[�qD\� D]�D]�D^  D^}qD_  D_� D`�D`��D`�qDa� Db�Db}qDb��Dc}qDd�Dd� Dd�qDe}qDf  Df��DgDg� Dh�Dh��Dh�qDiz�Dj  Dj�Dk�Dk� Dk�qDl}qDl�qDm� Dn  Dn� Do�Do}qDo�qDp}qDp�qDq� Dr  Dr��Ds  Ds��Dt  Dt}qDu  Du}qDu�qDv� Dw  Dw}qDw�qDx� Dy  Dy� Dy�qDz}qDz�qD{� D{�qD|}qD}  D}}qD~  D~��D  D� D�  D�@ D��HD�� D���D�>�D��HD�D�HD�@ D�~�D�� D�HD�@ D�� D�D�HD�>�D�� D�� D�HD�AHD�� D�� D�  D�AHD��HD�� D�  D�@ D�� D��HD�HD�@ D�� D��HD��D�@ D�� D��HD�HD�>�D�}qD��qD���D�>�D�~�D�� D���D�=qD�~�D�� D�HD�AHD�� D�� D�  D�AHD��HD���D�  D�@ D�~�D�� D�  D�AHD��HD��qD�  D�AHD���D�� D���D�@ D��HD�� D�  D�AHD��HD�� D���D�@ D�~�D���D���D�=qD�� D�� D�  D�AHD���D��HD�  D�>�D�� D��HD�  D�>�D�~�D���D���D�>�D�� D�� D�HD�AHD���D��HD�  D�AHD�� D���D�HD�@ D�� D�D�HD�=qD�~�D�� D���D�=qD�~�D���D��qD�@ D�~�D���D�HD�B�D���D���D�HD�@ D��HD�� D���D�@ D�~�D�� D��D�AHD��HD�� D�  D�=qD�}qD��HD�HD�>�D�� D��HD�  D�AHD���D�� D���D�>�D�� D�D�HD�>�D�~�D�� D��D�AHD��HD��HD�HD�AHD�� D�� D�  D�@ D��HD��HD�HD�AHD�~�D�� D���D�<)D�}qD���D���D�>�D�~�D���D���D�@ D��HD��HD��D�B�D���D�� D��qD�=qD�~�D��qD���D�>�D�~�D���D���D�>�D�� D��HD��D�@ D�}qD�� D�HD�>�D�~�D���D���D�@ D�� D�� D�  D�@ D�� D���D�HD�AHD��HD�D�HD�@ D�� D��HD�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D�� D���D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D���D�=qDÀ D��HD�  D�AHDāHDľ�D���D�AHDŁHDž�D���D�@ Dƀ D�� D�  D�>�Dǀ D��HD�  D�@ D�~�D��HD�HD�@ DɁHD�� D���D�@ Dʀ D�� D���D�=qDˀ D�� D�  D�@ D̀ D��HD�  D�@ D�~�D�� D�HD�@ D΀ Dξ�D��qD�=qDπ D��HD�HD�@ DЀ D�� D�  D�@ D�~�DѾ�D�  D�@ DҁHD�� D�  D�>�D�}qD�� D�  D�@ DԁHDԾ�D�  D�>�D�}qDվ�D�  D�>�DցHD�D�  D�@ DׁHD��HD�HD�AHD؁HD�� D�  D�@ D�}qDٽqD���D�>�D�~�DڽqD��qD�@ DہHD��HD�HD�AHD܀ D�� D�  D�B�D݁HDݾ�D��qD�@ Dނ�D�D�  D�@ D߀ D��HD��D�@ D�� D��HD���D�>�D�}qD�qD���D�AHD� D⾸D��qD�@ D�HD�� D�  D�>�D�}qD侸D�HD�@ D�~�D�� D�HD�@ D�~�D澸D�  D�AHD�HD羸D�  D�AHD� D��HD�HD�@ D�~�D�� D�  D�=qD�~�D�� D�  D�@ D�HD��HD�  D�AHD�HD�� D�  D�@ D� D��HD�  D�=qD�~�DD�  D�>�D�~�D�D�HD�>�D�~�D��HD��D�@ D� D�� D�  D�@ D�HD��HD���D�@ D� D�qD���D�@ D� D�� D���D�=qD�}qD��qD���D�=qD�~�D���D���D�AHD���D��HD�  D�AHD��HD�� D���D�>�D�� D�� D�  D�AHD�p�G�O�?\)?L��?�z�?�{?Ǯ?�ff@
=q@�@&ff@=p�@L��@Y��@xQ�@��@�{@�Q�@��@���@��H@�ff@�33@�p�@���@�33AG�A�A�AG�A�A{A#33A(��A/\)A6ffA<(�AA�AH��AP  AU�AZ�HAa�AhQ�Ao\)Atz�Az�HA���A�z�A�\)A�=qA�A���A��
A��RA��A��A��A��HA�{A���A��
A��RA��A���A�
=A�G�A��
A��RA���A\A�z�A�
=A���A��HA���A�
=A���Aҏ\A�(�A�{A�  A�G�A�33A��A�
=A�Q�A�=qA�(�A�ffA�A陚A�A�p�A�
=A���A��HA���A�ffA�Q�A��\A�(�A�A��B ��BBffB\)BQ�BG�B{B�HB  B��B	B
�\B�B��Bp�BffB�Bz�BG�B=qB�Bz�BG�B=qB�Bz�BG�BffB�B��BG�BffB\)B z�B!p�B"=qB#\)B$z�B%p�B&=qB'\)B(��B)p�B*=qB+\)B,��B-��B.ffB/\)B0z�B1��B2�\B3\)B4z�B5��B6�\B7\)B8��B9��B:�\B;\)B<��B=��B>ffB?\)B@z�BA��BBffBC33BDQ�BEG�BF=qBG
=BH  BI�BJ{BJ�HBK�BL��BM�BN�RBO�BP��BQBRffBS\)BTz�BUG�BV{BW
=BX(�BYG�BZ{BZ�HB\  B]�B]�B^�RB_�
B`��Ba�Bb�RBc�
Bd��BeBf�RBh  Bh��BiBj�RBk�
Bl��BmBn�RBo�Bp��Bq�Br�RBt  Bu�Bv{Bv�HBx  By�Bz=qB{
=B|(�B}�B~ffB\)B�{B���B�G�B�B�(�B���B�G�B��
B�=qB��HB��B�{B��\B��B��B�Q�B���B�p�B��B���B�33B��
B�Q�B��HB��B�(�B��RB�G�B��
B�ffB�
=B��B�=qB���B�\)B�  B��RB�G�B�B�ffB�
=B��B�=qB���B�G�B��B��\B�33B��B�=qB���B��B�(�B���B�33B�B�z�B�
=B��B�(�B���B�p�B�  B�z�B�
=B�B�ffB���B��B�{B��RB�\)B��B�z�B���B���B�=qB��HB�p�B��B��\B�33B�B�=qB��RB�\)B��B�z�B��HB�\)B��
B�ffB���B�33B�p�B��B�{B�Q�B�z�B��\B��\B��RB���B��RB��RB��RB��RB��HB��HB��HB���B���B��HB���B���B��HB��HB���B���B���B���B���B���B���B��HB��HB��HB���B���B���B��HB��HB��HB���B�
=B���B��HB��HB���B�
=B�
=B���B���B���B�
=B�
=B��B�
=B�
=B���B�
=B��B�33B��B��B��B�33B�G�B�G�B�33B�33B�G�B�\)B�p�B�\)B�\)B�\)B�p�B��B���B���B��B���B��B�B��
B��
B��
B��
B�  B�{B�{B�  B�{B�(�B�=qB�Q�B�ffB�z�B�ffB�ffB�z�B���B��RB���B���B���B���B��B��B��B�33B�G�B��B���B��B�B��
B��B�{B�=qB�Q�B�ffB�z�B�z�B��RB��HB�
=B��B�33B�\)B��B��B��B�{B�=qB�Q�B�z�B���B��HB��B�G�B�p�B���B�B�  B�(�B�ffB��RB���B��B�\)B��B�B��B�=qB��\B���B�
=B�33B�\)B��B��B�=qB�ffB£�B���B���B�33B�p�BîB�  B�(�B�Q�B�z�BĸRB���B�33B�p�BŮB�B�  B�=qB�z�B���B�
=B�G�B�p�BǮB��B�=qBȏ\B���B�33B�p�Bə�B��
B�(�B�z�B���B��B�p�BˮB��B�(�B�ffB̸RB��B�p�B�B�{B�Q�BθRB���B�G�BϮB�{B�ffB���B��B�p�B�B�{B�ffB���B��BӅB��B�ffBԸRB�
=B�\)B�B�{B�z�B��HB�G�B׮B�(�B؏\B���B�\)BٮB�{B�z�B���B�33Bۙ�B��B�=qBܣ�B���B�\)B�B�(�Bޏ\B���B�\)B��
B�=qB��\B�
=B�p�B��
B�=qB�\B�
=B�p�B��
B�=qB��B�
=B�p�B��
B�Q�B�RB�33B癚B�{B�\B���B�p�B��B�Q�B���B�G�B�B�(�B�\B���B�p�B��B�ffB��HB�G�B�B�=qB���B�33B�B�{B�\B�
=B�B�{B�\B�
=B��B�{B��\B�
=B��B�  B�z�B��HB�p�B�  B�z�B���B�p�B��B�z�B���B�p�B�  B�z�B���B�p�C   C =qC z�C �RC  C=qCz�C�RC  C33Cp�C�RC��C33Cz�C�RC  C=qCz�CC  CG�C�C��C
=CQ�C�\C�
C{C\)C��C�HC�CffC��C�HC	(�C	ffC	�C	�C
33C
p�C
�RC
��C=qCz�CC  CG�C�CC
=CQ�C�\C��C
=CQ�C�\C��C{CQ�C�\C�
C�C\)C�C��C33Cz�C�RC  C33Cz�C�RC��C=qC�C��C
=CG�C�C��C
=C=qCz�C�RC  C=qC�\C�
C{CQ�C��C��C{CQ�C��C�C(�CffC��C��C=qC�C�RC  CQ�C�\C��C{CQ�C�C�C(�Cp�CC
=C=qC�C�
C�C\)C��C��C =qC z�C C!{C!Q�C!��C!�HC"33C"ffC"�C#
=C#G�C#�\C#��C$�C$p�C$�C$��C%G�C%�\C%��C&�C&p�C&�RC'  C'\)C'��C'�C(G�C(�C(�HC)33C)p�C)��C*�C*ffC*�RC+
=C+\)C+��C+��C,Q�C,��C,�HC-=qC-�\C-��C.�C.z�C.C/
=C/\)C/�C/��C0=qC0��C0�C1=qC1z�C1�HC233C2z�C2C3�C3p�C3C4
=C4\)C4�C5
=C5Q�C5��C6
=C6\)C6��C7  C7\)C7�C8  C8\)C8�C9  C9\)C9�RC:
=C:ffC:��C;�C;p�C;�
C<33C<�C<�C==qC=��C=��C>G�C>�RC?
=C?ffC?��C@�C@�C@�HCA33CA�\CB  CBQ�CB�RCC�CCp�CC�
CD=qCD�\CD��CE\)CE�CF�CFz�CF��CG=qCG�\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�&G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aթ�Aգ�Aգ�Aե�Aգ�Aա�Aա�Aա�Aգ�Aա�Aգ�Aա�Aա�Aգ�Aգ�Aգ�Aե�Aէ�Aե�Aէ�Aէ�Aէ�Aթ�Aէ�Aէ�Aե�Aե�Aէ�Aէ�Aթ�Aթ�AլAլAծAթ�AՍPA�1'A�~�Aӝ�A�l�A�1'A�`BA�-A�oA���A��A���AΓuA�-A���A�ĜA�^5A���A�bAǉ7A�7LA���A��/A���A�^5A��-A�z�A�ĜA��hA��-A�ȴA��A�K�A��FA��yA� �A��uA���A��9A�bNA��\A��A�z�A�C�A��A�ZA��9A���A��PA��RA�|�Ay�Ap��Aj��Ab~�A[�;AZ�yAX��AW%AR=qAP�AN��AH��AB�yA@~�A>$�A:1A7�#A6�DA5�FA5G�A4��A3|�A333A2  A1/A1&�A0�uA0 �A01A/�
A/A/��A/&�A.JA-dZA,�A,^5A*��A(�A&�`A&��A&bA%/A$ĜA$n�A${A#�A#�A$�A#��A"��A"�A"�jA"^5A!ƨA!XA �A 9XA�^A�AffA{A�mA|�A�mA�jA  A��A\)AA��AjAZA�uAQ�Al�AE�A�PAC�A&�A�uA$�A�A�uAM�A$�A1A�mA�#A��A��AhsA"�A�A  AG�A�Ar�A�A  AA��AdZAO�A%A�/A��A�!AjA1'A�
A|�A�A�uAI�A�A�TA��A
�/A	�#A	�PA	O�A	VA�+AA�A�TA��A7LA�A��A;dAVA��A��AJAx�A"�A��A�/AȴA�A�AM�A$�AJA�FAC�A�A �/A ��A �9@��P@�M�@�@���@�dZ@��H@�=q@�x�@�7L@���@�(�@�b@���@��@��+@��^@�V@�@���@��@�V@���@��T@��@��@�@���@�u@��D@� �@�\)@�5?@�h@�V@�r�@�  @땁@�
=@�n�@�/@��`@�@���@�v�@�-@�&�@�9@��@�S�@��@�ff@�@���@�7L@��u@�(�@߾w@�l�@���@�@�p�@�?}@��/@ܬ@ܛ�@܋D@�Z@�A�@��;@�v�@�O�@�1'@�K�@���@֧�@�n�@�V@���@��@�z�@Ӿw@�t�@�C�@ҧ�@�@�G�@��`@�(�@ϝ�@�
=@ΰ!@�$�@�G�@���@���@̣�@�9X@���@���@Ȭ@�Z@�A�@Ǯ@�33@�~�@ũ�@��`@��
@�33@�@�{@��#@�x�@�7L@�&�@��`@���@��D@�bN@�9X@���@���@�|�@�"�@�@�ȴ@�~�@�M�@�-@���@�O�@�&�@��@�Ĝ@��j@�z�@�Z@�1'@�  @��
@�C�@���@�J@���@���@�X@�&�@���@�Q�@�  @��w@�t�@�33@��\@��#@���@�O�@��@��@�1@�
=@��\@�v�@�n�@�^5@�E�@�-@��@��u@��m@���@�|�@�+@��R@�5?@�$�@��#@�G�@��@��u@�(�@���@��@�C�@�n�@�J@���@�&�@���@���@��@��@�Z@�b@��w@��@�+@���@�n�@�5?@���@��7@�V@�Ĝ@�r�@�bN@�I�@�  @��@�|�@�K�@�
=@��y@���@�V@�hs@���@�bN@�9X@�b@��@��@��m@��@��@��!@�V@��T@�hs@�G�@���@��@�bN@�I�@�1@���@�t�@�l�@�S�@�;d@�o@�ȴ@��R@�~�@�-@���@���@�hs@��@�Q�@��;@���@��w@���@�;d@��!@�J@��#@�X@��@�1'@�  @��F@�33@��\@�V@���@�@�G�@��/@��u@�z�@�I�@��;@�\)@�;d@�+@��y@��+@�=q@��@��@��^@�`B@�7L@�V@���@�1'@���@�+@���@���@�=q@��^@�p�@�/@��D@��@��@�K�@���@���@�M�@��^@��@��@���@��j@��D@�Z@�1'@���@���@�|�@�\)@�C�@�
=@���@��+@�E�@��@���@�x�@���@���@��@�9X@�@|�@
=@~V@}V@|j@{t�@{o@z��@z�@y��@y�^@y7L@x�9@xQ�@w��@w��@wl�@vV@u�-@u�@u`B@u�@tj@t�@s�
@r��@q�@q��@q&�@p��@p��@pA�@o��@ol�@o�@n�+@m�T@m/@lj@l9X@k�m@kS�@j�!@jn�@j=q@i�^@i�@h��@hĜ@h�@hb@g�@fE�@e��@eV@d�/@dz�@cƨ@c�@cS�@b�H@bn�@bM�@b=q@bJ@a�^@a��@aG�@`��@_��@_;d@^�+@]�T@]��@]O�@]V@\�@\I�@[�F@["�@Z��@Z��@Z��@Z~�@Z�@Y�^@YG�@X�`@XbN@W�@W�w@WK�@V��@V5?@U��@U��@U`B@T��@TZ@T9X@S��@St�@SdZ@SdZ@S"�@R�!@R~�@R�@Q��@Q��@Q7L@P��@P�u@PbN@P1'@Pb@O��@O�P@O;d@N�@N��@Nv�@N@Mp�@M�@L�/@Lj@L1@K�
@K�F@K33@J�@J��@J~�@J^5@J=q@I��@Ix�@IG�@H��@H�u@H �@G�;@Gl�@G�@F��@FE�@F5?@E�@E�@D��@Dz�@DZ@D9X@D1@C�m@C�F@C33@B=q@B�@BJ@A�#@A&�@@Ĝ@@�@@b@?��@?+@?
=@>�+@>V@>5?@=�@=@=�h@=`B@=O�@=�@<�/@<��@<Z@;�m@;t�@;o@:�!@:n�@:=q@9��@9hs@9�@8Ĝ@8bN@8  @7��@7+@6�R@6v�@6ff@6V@65?@5��@5�@4Z@3��@3"�@2~�@2M�@2M�@2�@1��@1�^@1��@1x�@0�`@0Q�@/�@/\)@/+@.��@.�@.�+@.V@-��@-�@-p�@-O�@,�j@,9X@+��@+�@+t�@+S�@+"�@*�H@*��@*=q@*�@*�@*J@)�@)��@)&�@)�@)%@(�`@(1'@(  @'�@'�;@'�w@'l�@&�y@&��@&v�@&@%�@%��@%��@%p�@%O�@%?}@%/@%V@$�j@$��@$��@$z�@$z�@$9X@#�
@#�@#C�@#C�@#@"��@"M�@"�@!��@!�#@!�^@!hs@!�@ ��@ r�@ Q�@   @�w@|�@;d@��@��@E�@�@��@O�@��@��@9X@(�@�@�@�@1@�
@��@t�@33@@��@�\@n�@^5@=q@�@��@�7@7L@��@r�@A�@1'@ �@  @�;@�;@�w@�@�P@�P@�P@|�@l�@K�@+@�@�@�@�@��@�@ff@V@V@5?@�T@@��@�@O�@�@V@�@��@�j@�@z�@Z@�@�
@�F@��@��@��@dZ@o@��@^5@M�@n�@��@�H@�@�@�!@�\@=q@��@�^@hs@hs@G�@G�@7L@7L@7L@%@�9@�@bN@bN@1'@�@�P@\)@�@
=@�y@�y@��@$�@�@@��@��@��@p�@/@V@�/@��@z�@Z@(�@1@�F@��@�@S�@"�@
��@
�!@
�!@
��@
n�@
M�@
-@
�@
J@	��@	��@	�@	�#@	�#@	��@	�^@	��@	x�@	G�@	%@	%@�`@�9@��@�uAէ�AծAթ�Aե�Aե�Aե�Aա�Aգ�Aե�Aե�A՟�Aգ�Aթ�Aէ�Aգ�Aա�Aե�Aա�A՟�Aա�Aգ�Aա�A՟�Aգ�Aգ�Aա�Aա�Aգ�Aե�Aգ�A՝�Aե�Aե�Aա�A՝�A՟�Aգ�Aգ�A՟�A՟�Aա�Aգ�Aա�A՟�Aգ�Aե�Aգ�Aա�Aգ�Aե�Aգ�Aա�Aե�Aէ�Aգ�Aա�Aե�Aէ�Aգ�Aգ�Aե�Aթ�Aէ�Aե�Aե�Aէ�Aթ�Aե�Aե�Aէ�Aթ�Aգ�Aգ�Aէ�Aէ�Aե�Aգ�Aէ�Aթ�Aե�Aգ�Aէ�Aթ�Aե�Aե�Aէ�Aթ�Aէ�Aե�Aթ�Aթ�Aե�Aե�Aթ�Aթ�Aե�Aե�AլAէ�Aե�Aէ�Aթ�AլAե�Aե�Aթ�AլAթ�Aէ�Aթ�AծAթ�Aէ�Aթ�AծAթ�Aէ�AլAլAե�Aգ�Aէ�Aթ�Aէ�Aե�Aթ�AլAե�Aե�Aթ�Aթ�Aե�Aգ�Aե�Aէ�Aէ�Aե�Aգ�Aէ�Aէ�Aգ�Aե�Aթ�Aէ�Aգ�Aե�Aթ�Aէ�Aե�Aգ�Aէ�Aթ�Aթ�Aե�Aե�Aթ�Aէ�Aե�Aէ�AլAթ�Aե�Aէ�AլAէ�Aե�Aթ�AլAթ�Aէ�Aէ�AլAլAէ�Aէ�AլAծAթ�Aէ�Aթ�AլAթ�Aէ�AլAծAթ�Aթ�AծAծAթ�Aթ�AլAհ!AծAլAծAհ!Aհ!AլAծAղ-Aհ!AլAլAհ!AծAլAղ-Aհ!AծAէ�A՛�AՓuAՓuAՏ\AՋDAՏ\AՏ\AՃAՏ\A�|�A�`BA�E�A�=qA�?}A�oA�VA�
=A���A��/A�ƨAԍPAԇ+A�bNA���A��mA��`A��
AӴ9AӺ^AӼjA�t�A� �A҇+A�1A���A�hsA�(�A�VA��AоwAБhA�O�A�&�A�VA�%A���A���Aϙ�A�t�A�Q�A�9XA�9XA�5?A�33A�1'A�1'A�33A�-A�"�A��A��A��A�{A�VA�JA�JA�JA�
=A�A���A��A��A��`A��/A��A��A��/A��#A��
A���A���A���A���A���Aδ9AήAΩ�AΣ�AΝ�AΗ�AΗ�A΍PA΁A�|�A�z�A�x�A�l�A�9XA��#A���A���A���A���A���A���A���A���A���A���A�A���A�ĜA�ƨA�ĜA�A�A�ƨA�ȴA�ƨA�ĜA�ĜA�ƨA�ȴA�ȴA�ȴA�A�ĜA�ƨA�ƨA�ĜA�A�A�ĜA�ƨA�ĜA�ĜA�A���A;wAͼjA͸RAͰ!A͝�A͕�A͇+AͅÁA̓ÁA�|�A�x�A�v�A�v�A�v�A�v�A�r�A�n�A�n�A�n�A�p�A�n�A�jA�ffA�dZA�bNA�bNA�^5A�ZA�VA�Q�A�S�A�S�A�Q�A�M�A�K�A�I�A�G�A�I�A�I�A�E�A�E�A�G�A�I�A�C�A�;dA�33A�33A�33A�5?A�5?A�1'A�-A�-A�/A�1'A�1'A�-A�+A�+A�-A�-A�-A�+A�(�A�(�A�+A�+A�+A�"�A��A�{A�{A��A��A�{A�VA�VA�bA�oA�VA�%A�
=A�
=A�VA�
=A�A�  A���A��A��A��A��A��A��yA��yA��A��A��A��A��mA��HA��HA��A��A���A���A�ĜA�A���A�A���A̺^A̴9A̮A̡�A̙�A̋DA�r�A�^5A�E�A�/A�%A���Aˣ�A˟�AˋDA�K�A�~�A�A���Aɲ-Aɕ�A�bNA��A�ĜAȅA�?}A�
=A���Aǧ�A�|�A�l�A�bNA�Q�A�=qA�-A�(�A��A�bA��A��A��A��A��yA��#AƼjAƧ�A�p�A�M�A��A���Aě�AēuA���A��;AÕ�A�5?A�A�x�A�bA�1A���A��A��yA��yA��HA��#A��#A���A�ƨA��RA��A���A�|�A�E�A���A��#A�ZA�&�A��#A���A�M�A��A���A���A���A�jA�-A��jA�z�A�I�A� �A���A���A�/A���A���A�r�A�M�A�-A�A���A��A��`A��;A���A���A�ȴA�ĜA��-A���A���A��DA��PA��\A��DA��DA��7A��A�~�A�~�A�~�A��A�p�A�ZA�A�z�A��A��mA�A���A�~�A�dZA�-A��jA���A��A��DA�r�A�XA�?}A��A�oA�JA�%A���A��A�ȴA���A��PA�r�A�dZA�S�A�A�A�5?A�(�A�
=A���A���A�\)A���A��9A��PA�?}A��jA��A�O�A�+A��;A���A��hA��A�l�A�\)A�;dA�"�A��A��/A���A���A���A��jA���A���A���A��A�hsA�Q�A�-A��/A��RA���A�|�A�r�A�r�A�jA�VA�&�A�
=A��/A���A���A���A�jA�VA���A�
=A��9A��A�`BA�M�A�=qA� �A��#A��wA��-A���A���A�v�A�/A��A�A�n�A��A�Q�A�bA���A�z�A�E�A�(�A��A�%A���A�ZA��HA�VA�jA���A�VA�A��wA�M�A���A��`A��HA���A���A��!A�(�A��A�{A�VA�jA��A���A��#A��-A���A��7A�|�A�`BA���A���A�t�A�n�A�dZA�S�A�+A���A���A��jA��+A�ffA�A�A��
A�K�A��
A�z�A���A�/A��7A��HA�(�A�&�A���A�r�A�ĜA�z�A���A���A���A���A�I�A���A�ffA�A���A��9A�|�A�M�A��A�|�A�I�A���A��
A���A�G�A���A��hA�  A��A�/A��;A�O�AXA}x�A|��A{��Az��Az{Ay��Ax��Au;dAs��Ar��Aq�hAq33ApQ�AooAn~�Am�
Al�yAk��Aj�Aj(�Ai�AihsAh�Ag33Ae33AbE�A_��A^�HA^bA]hsA\��A\1A[�PA[&�A[�A[�A[�A[�A[�A[%AZ��AZ�AY�AYp�AX��AX�uAX5?AX1AW��AW�
AW��AWS�AV��AV �AUAS�-AR��AR �AQ�FAQ��AQC�AP��AP��AP�!AP�API�AP1AO�AO�^AOS�AN��AN �AM|�ALv�AK�AJ��AI��AHffAE�
ADM�AC�PAChsAC;dAB��AA�AAoA@�RA@v�A@M�A@A�A@=qA@(�A?A>Q�A<�A;�A;"�A:��A:5?A9ƨA9t�A9�A8��A8A7�wA7�A7G�A7
=A6�/A6�9A6v�A6E�A5��A5�;A5ƨA5�A5��A5�7A5x�A5S�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                   Aթ�Aգ�Aգ�Aե�Aգ�Aա�Aա�Aա�Aգ�Aա�Aգ�Aա�Aա�Aգ�Aգ�Aգ�Aե�Aէ�Aե�Aէ�Aէ�Aէ�Aթ�Aէ�Aէ�Aե�Aե�Aէ�Aէ�Aթ�Aթ�AլAլAծAթ�AՍPA�1'A�~�Aӝ�A�l�A�1'A�`BA�-A�oA���A��A���AΓuA�-A���A�ĜA�^5A���A�bAǉ7A�7LA���A��/A���A�^5A��-A�z�A�ĜA��hA��-A�ȴA��A�K�A��FA��yA� �A��uA���A��9A�bNA��\A��A�z�A�C�A��A�ZA��9A���A��PA��RA�|�Ay�Ap��Aj��Ab~�A[�;AZ�yAX��AW%AR=qAP�AN��AH��AB�yA@~�A>$�A:1A7�#A6�DA5�FA5G�A4��A3|�A333A2  A1/A1&�A0�uA0 �A01A/�
A/A/��A/&�A.JA-dZA,�A,^5A*��A(�A&�`A&��A&bA%/A$ĜA$n�A${A#�A#�A$�A#��A"��A"�A"�jA"^5A!ƨA!XA �A 9XA�^A�AffA{A�mA|�A�mA�jA  A��A\)AA��AjAZA�uAQ�Al�AE�A�PAC�A&�A�uA$�A�A�uAM�A$�A1A�mA�#A��A��AhsA"�A�A  AG�A�Ar�A�A  AA��AdZAO�A%A�/A��A�!AjA1'A�
A|�A�A�uAI�A�A�TA��A
�/A	�#A	�PA	O�A	VA�+AA�A�TA��A7LA�A��A;dAVA��A��AJAx�A"�A��A�/AȴA�A�AM�A$�AJA�FAC�A�A �/A ��A �9@��P@�M�@�@���@�dZ@��H@�=q@�x�@�7L@���@�(�@�b@���@��@��+@��^@�V@�@���@��@�V@���@��T@��@��@�@���@�u@��D@� �@�\)@�5?@�h@�V@�r�@�  @땁@�
=@�n�@�/@��`@�@���@�v�@�-@�&�@�9@��@�S�@��@�ff@�@���@�7L@��u@�(�@߾w@�l�@���@�@�p�@�?}@��/@ܬ@ܛ�@܋D@�Z@�A�@��;@�v�@�O�@�1'@�K�@���@֧�@�n�@�V@���@��@�z�@Ӿw@�t�@�C�@ҧ�@�@�G�@��`@�(�@ϝ�@�
=@ΰ!@�$�@�G�@���@���@̣�@�9X@���@���@Ȭ@�Z@�A�@Ǯ@�33@�~�@ũ�@��`@��
@�33@�@�{@��#@�x�@�7L@�&�@��`@���@��D@�bN@�9X@���@���@�|�@�"�@�@�ȴ@�~�@�M�@�-@���@�O�@�&�@��@�Ĝ@��j@�z�@�Z@�1'@�  @��
@�C�@���@�J@���@���@�X@�&�@���@�Q�@�  @��w@�t�@�33@��\@��#@���@�O�@��@��@�1@�
=@��\@�v�@�n�@�^5@�E�@�-@��@��u@��m@���@�|�@�+@��R@�5?@�$�@��#@�G�@��@��u@�(�@���@��@�C�@�n�@�J@���@�&�@���@���@��@��@�Z@�b@��w@��@�+@���@�n�@�5?@���@��7@�V@�Ĝ@�r�@�bN@�I�@�  @��@�|�@�K�@�
=@��y@���@�V@�hs@���@�bN@�9X@�b@��@��@��m@��@��@��!@�V@��T@�hs@�G�@���@��@�bN@�I�@�1@���@�t�@�l�@�S�@�;d@�o@�ȴ@��R@�~�@�-@���@���@�hs@��@�Q�@��;@���@��w@���@�;d@��!@�J@��#@�X@��@�1'@�  @��F@�33@��\@�V@���@�@�G�@��/@��u@�z�@�I�@��;@�\)@�;d@�+@��y@��+@�=q@��@��@��^@�`B@�7L@�V@���@�1'@���@�+@���@���@�=q@��^@�p�@�/@��D@��@��@�K�@���@���@�M�@��^@��@��@���@��j@��D@�Z@�1'@���@���@�|�@�\)@�C�@�
=@���@��+@�E�@��@���@�x�@���@���@��@�9X@�@|�@
=@~V@}V@|j@{t�@{o@z��@z�@y��@y�^@y7L@x�9@xQ�@w��@w��@wl�@vV@u�-@u�@u`B@u�@tj@t�@s�
@r��@q�@q��@q&�@p��@p��@pA�@o��@ol�@o�@n�+@m�T@m/@lj@l9X@k�m@kS�@j�!@jn�@j=q@i�^@i�@h��@hĜ@h�@hb@g�@fE�@e��@eV@d�/@dz�@cƨ@c�@cS�@b�H@bn�@bM�@b=q@bJ@a�^@a��@aG�@`��@_��@_;d@^�+@]�T@]��@]O�@]V@\�@\I�@[�F@["�@Z��@Z��@Z��@Z~�@Z�@Y�^@YG�@X�`@XbN@W�@W�w@WK�@V��@V5?@U��@U��@U`B@T��@TZ@T9X@S��@St�@SdZ@SdZ@S"�@R�!@R~�@R�@Q��@Q��@Q7L@P��@P�u@PbN@P1'@Pb@O��@O�P@O;d@N�@N��@Nv�@N@Mp�@M�@L�/@Lj@L1@K�
@K�F@K33@J�@J��@J~�@J^5@J=q@I��@Ix�@IG�@H��@H�u@H �@G�;@Gl�@G�@F��@FE�@F5?@E�@E�@D��@Dz�@DZ@D9X@D1@C�m@C�F@C33@B=q@B�@BJ@A�#@A&�@@Ĝ@@�@@b@?��@?+@?
=@>�+@>V@>5?@=�@=@=�h@=`B@=O�@=�@<�/@<��@<Z@;�m@;t�@;o@:�!@:n�@:=q@9��@9hs@9�@8Ĝ@8bN@8  @7��@7+@6�R@6v�@6ff@6V@65?@5��@5�@4Z@3��@3"�@2~�@2M�@2M�@2�@1��@1�^@1��@1x�@0�`@0Q�@/�@/\)@/+@.��@.�@.�+@.V@-��@-�@-p�@-O�@,�j@,9X@+��@+�@+t�@+S�@+"�@*�H@*��@*=q@*�@*�@*J@)�@)��@)&�@)�@)%@(�`@(1'@(  @'�@'�;@'�w@'l�@&�y@&��@&v�@&@%�@%��@%��@%p�@%O�@%?}@%/@%V@$�j@$��@$��@$z�@$z�@$9X@#�
@#�@#C�@#C�@#@"��@"M�@"�@!��@!�#@!�^@!hs@!�@ ��@ r�@ Q�@   @�w@|�@;d@��@��@E�@�@��@O�@��@��@9X@(�@�@�@�@1@�
@��@t�@33@@��@�\@n�@^5@=q@�@��@�7@7L@��@r�@A�@1'@ �@  @�;@�;@�w@�@�P@�P@�P@|�@l�@K�@+@�@�@�@�@��@�@ff@V@V@5?@�T@@��@�@O�@�@V@�@��@�j@�@z�@Z@�@�
@�F@��@��@��@dZ@o@��@^5@M�@n�@��@�H@�@�@�!@�\@=q@��@�^@hs@hs@G�@G�@7L@7L@7L@%@�9@�@bN@bN@1'@�@�P@\)@�@
=@�y@�y@��@$�@�@@��@��@��@p�@/@V@�/@��@z�@Z@(�@1@�F@��@�@S�@"�@
��@
�!@
�!@
��@
n�@
M�@
-@
�@
J@	��@	��@	�@	�#@	�#@	��@	�^@	��@	x�@	G�@	%@	%@�`@�9@��G�O�Aէ�AծAթ�Aե�Aե�Aե�Aա�Aգ�Aե�Aե�A՟�Aգ�Aթ�Aէ�Aգ�Aա�Aե�Aա�A՟�Aա�Aգ�Aա�A՟�Aգ�Aգ�Aա�Aա�Aգ�Aե�Aգ�A՝�Aե�Aե�Aա�A՝�A՟�Aգ�Aգ�A՟�A՟�Aա�Aգ�Aա�A՟�Aգ�Aե�Aգ�Aա�Aգ�Aե�Aգ�Aա�Aե�Aէ�Aգ�Aա�Aե�Aէ�Aգ�Aգ�Aե�Aթ�Aէ�Aե�Aե�Aէ�Aթ�Aե�Aե�Aէ�Aթ�Aգ�Aգ�Aէ�Aէ�Aե�Aգ�Aէ�Aթ�Aե�Aգ�Aէ�Aթ�Aե�Aե�Aէ�Aթ�Aէ�Aե�Aթ�Aթ�Aե�Aե�Aթ�Aթ�Aե�Aե�AլAէ�Aե�Aէ�Aթ�AլAե�Aե�Aթ�AլAթ�Aէ�Aթ�AծAթ�Aէ�Aթ�AծAթ�Aէ�AլAլAե�Aգ�Aէ�Aթ�Aէ�Aե�Aթ�AլAե�Aե�Aթ�Aթ�Aե�Aգ�Aե�Aէ�Aէ�Aե�Aգ�Aէ�Aէ�Aգ�Aե�Aթ�Aէ�Aգ�Aե�Aթ�Aէ�Aե�Aգ�Aէ�Aթ�Aթ�Aե�Aե�Aթ�Aէ�Aե�Aէ�AլAթ�Aե�Aէ�AլAէ�Aե�Aթ�AլAթ�Aէ�Aէ�AլAլAէ�Aէ�AլAծAթ�Aէ�Aթ�AլAթ�Aէ�AլAծAթ�Aթ�AծAծAթ�Aթ�AլAհ!AծAլAծAհ!Aհ!AլAծAղ-Aհ!AլAլAհ!AծAլAղ-Aհ!AծAէ�A՛�AՓuAՓuAՏ\AՋDAՏ\AՏ\AՃAՏ\A�|�A�`BA�E�A�=qA�?}A�oA�VA�
=A���A��/A�ƨAԍPAԇ+A�bNA���A��mA��`A��
AӴ9AӺ^AӼjA�t�A� �A҇+A�1A���A�hsA�(�A�VA��AоwAБhA�O�A�&�A�VA�%A���A���Aϙ�A�t�A�Q�A�9XA�9XA�5?A�33A�1'A�1'A�33A�-A�"�A��A��A��A�{A�VA�JA�JA�JA�
=A�A���A��A��A��`A��/A��A��A��/A��#A��
A���A���A���A���A���Aδ9AήAΩ�AΣ�AΝ�AΗ�AΗ�A΍PA΁A�|�A�z�A�x�A�l�A�9XA��#A���A���A���A���A���A���A���A���A���A���A�A���A�ĜA�ƨA�ĜA�A�A�ƨA�ȴA�ƨA�ĜA�ĜA�ƨA�ȴA�ȴA�ȴA�A�ĜA�ƨA�ƨA�ĜA�A�A�ĜA�ƨA�ĜA�ĜA�A���A;wAͼjA͸RAͰ!A͝�A͕�A͇+AͅÁA̓ÁA�|�A�x�A�v�A�v�A�v�A�v�A�r�A�n�A�n�A�n�A�p�A�n�A�jA�ffA�dZA�bNA�bNA�^5A�ZA�VA�Q�A�S�A�S�A�Q�A�M�A�K�A�I�A�G�A�I�A�I�A�E�A�E�A�G�A�I�A�C�A�;dA�33A�33A�33A�5?A�5?A�1'A�-A�-A�/A�1'A�1'A�-A�+A�+A�-A�-A�-A�+A�(�A�(�A�+A�+A�+A�"�A��A�{A�{A��A��A�{A�VA�VA�bA�oA�VA�%A�
=A�
=A�VA�
=A�A�  A���A��A��A��A��A��A��yA��yA��A��A��A��A��mA��HA��HA��A��A���A���A�ĜA�A���A�A���A̺^A̴9A̮A̡�A̙�A̋DA�r�A�^5A�E�A�/A�%A���Aˣ�A˟�AˋDA�K�A�~�A�A���Aɲ-Aɕ�A�bNA��A�ĜAȅA�?}A�
=A���Aǧ�A�|�A�l�A�bNA�Q�A�=qA�-A�(�A��A�bA��A��A��A��A��yA��#AƼjAƧ�A�p�A�M�A��A���Aě�AēuA���A��;AÕ�A�5?A�A�x�A�bA�1A���A��A��yA��yA��HA��#A��#A���A�ƨA��RA��A���A�|�A�E�A���A��#A�ZA�&�A��#A���A�M�A��A���A���A���A�jA�-A��jA�z�A�I�A� �A���A���A�/A���A���A�r�A�M�A�-A�A���A��A��`A��;A���A���A�ȴA�ĜA��-A���A���A��DA��PA��\A��DA��DA��7A��A�~�A�~�A�~�A��A�p�A�ZA�A�z�A��A��mA�A���A�~�A�dZA�-A��jA���A��A��DA�r�A�XA�?}A��A�oA�JA�%A���A��A�ȴA���A��PA�r�A�dZA�S�A�A�A�5?A�(�A�
=A���A���A�\)A���A��9A��PA�?}A��jA��A�O�A�+A��;A���A��hA��A�l�A�\)A�;dA�"�A��A��/A���A���A���A��jA���A���A���A��A�hsA�Q�A�-A��/A��RA���A�|�A�r�A�r�A�jA�VA�&�A�
=A��/A���A���A���A�jA�VA���A�
=A��9A��A�`BA�M�A�=qA� �A��#A��wA��-A���A���A�v�A�/A��A�A�n�A��A�Q�A�bA���A�z�A�E�A�(�A��A�%A���A�ZA��HA�VA�jA���A�VA�A��wA�M�A���A��`A��HA���A���A��!A�(�A��A�{A�VA�jA��A���A��#A��-A���A��7A�|�A�`BA���A���A�t�A�n�A�dZA�S�A�+A���A���A��jA��+A�ffA�A�A��
A�K�A��
A�z�A���A�/A��7A��HA�(�A�&�A���A�r�A�ĜA�z�A���A���A���A���A�I�A���A�ffA�A���A��9A�|�A�M�A��A�|�A�I�A���A��
A���A�G�A���A��hA�  A��A�/A��;A�O�AXA}x�A|��A{��Az��Az{Ay��Ax��Au;dAs��Ar��Aq�hAq33ApQ�AooAn~�Am�
Al�yAk��Aj�Aj(�Ai�AihsAh�Ag33Ae33AbE�A_��A^�HA^bA]hsA\��A\1A[�PA[&�A[�A[�A[�A[�A[�A[%AZ��AZ�AY�AYp�AX��AX�uAX5?AX1AW��AW�
AW��AWS�AV��AV �AUAS�-AR��AR �AQ�FAQ��AQC�AP��AP��AP�!AP�API�AP1AO�AO�^AOS�AN��AN �AM|�ALv�AK�AJ��AI��AHffAE�
ADM�AC�PAChsAC;dAB��AA�AAoA@�RA@v�A@M�A@A�A@=qA@(�A?A>Q�A<�A;�A;"�A:��A:5?A9ƨA9t�A9�A8��A8A7�wA7�A7G�A7
=A6�/A6�9A6v�A6E�A5��A5�;A5ƨA5�A5��A5�7A5x�A5S�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                   ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�UB��B�UB�UB�UB��B��B� B��B� B��B� B��B� B� B�UB��B��B� B� B�UB�UB��B�'B�'B�'B�'B��B�'B�'B�'B�[B��B��B�[BŢB� B��B��B	&�B	33B	:*B	=qB	@�B	B�B	L�B	bB	o5B	}VB	�B	�B	��B	�jB
B
|B
��B
�BbB%�B#nB �B+kB|�B��B�nB�zB��B��B�B�:B�_B��B|B{Bd&BB�BBAB
�qB
��B
u%B
/B
B	�&B	�0B	��B	~(B	[�B	H�B	HKB	:�B	5B	4B	2�B	;dB	9XB	>�B	O�B	K�B	_B	qB	�B	��B	�(B	�:B	�B	�=B	��B	��B	�BB	�
B	��B	�VB
�B
�B
B
 �B
%zB
($B
.IB
2�B
/�B
2�B
:�B
9�B
<�B
>�B
D�B
=�B
=B
;�B
:^B
9XB
=B
K�B
W
B
[WB
\]B
\�B
]/B
\�B
\�B
^jB
]/B
]�B
\�B
XyB
XB
V�B
VB
L�B
A B
8�B
7�B
7LB
9�B
<6B
<B
>wB
C�B
E�B
@B
A�B
A B
@OB
?}B
@B
B'B
U�B
\)B
Z�B
YB
X�B
W?B
W?B
WsB
ZQB
Z�B
ZQB
Y�B
Y�B
X�B
V9B
VmB
V�B
W�B
XB
XEB
W�B
W�B
V�B
W
B
W�B
X�B
W�B
W�B
W�B
W?B
W�B
V�B
U�B
T�B
TaB
S�B
T�B
Q�B
P}B
OvB
N�B
M�B
L0B
K^B
I�B
IB
FB
D�B
CaB
B'B
A�B
@OB
A B
>wB
=�B
=B
<�B
<6B
;�B
;�B
:�B
:�B
9�B
:*B
8RB
7�B
6zB
5?B
49B
5B
1'B
0UB
/�B
.B
-CB
,�B
+kB
,qB
+6B
*eB
*0B
*�B
*�B
)�B
)�B
)*B
'�B
($B
(�B
'�B
'B
&�B
&�B
&�B
&�B
'�B
&B
%zB
%�B
%FB
%zB
$B
#�B
#:B
"�B
"4B
!-B
!�B
 �B
�B
!B
!B
�B
�B
�B
CB
xB
B
=B
�B
B
eB
eB
�B
�B
B
B
�B
�B
�B
hB
�B
�B
@B
B
B
{B
�B
B
B
B
bB
�B
�B
\B
�B
\B
PB
�B
~B
�B
�B
JB
xB
	�B

=B

=B

=B

	B
	�B
	B
�B
%B
�B
�B
�B
B
�B
GB
AB
GB
uB
�B
�B
�B
oB
�B
oB
�B
�B
{B
�B
�B
�B
fB
�B
	�B

=B

�B
B
DB
~B
JB
�B
~B
�B
�B
xB
B
~B
�B
~B
�B
(B
�B
.B
�B
�B
 B
4B
�B
�B
(B
�B
�B
bB
bB
.B
.B
�B
�B
�B
B
�B
oB
oB
�B
�B
:B
uB
oB
�B
@B
@B
@B
uB
MB
B
B
FB
B
�B
FB
�B
�B
�B
�B
B
SB
�B
+B
�B
�B
�B
B
	B
�B
qB
qB
=B
�B
�B
�B
B
=B
B
�B
=B
�B
qB
B
�B
B
�B
xB
xB
�B
xB
xB
�B
IB
B
�B
�B
�B
xB
�B
�B
CB
xB
xB
B
B
�B
�B
B
IB
�B
IB
~B
�B
�B
~B
OB
�B
�B
�B
�B
VB
 \B
"4B
"hB
#:B
#B
#�B
#�B
$tB
$�B
%FB
$�B
$�B
$�B
$�B
%zB
%�B
%�B
%FB
$�B
$�B
$tB
$�B
$tB
$B
#�B
$@B
$tB
$�B
%FB
%B
%FB
%�B
&B
&�B
'RB
'�B
($B
(�B
(�B
(�B
)�B
)�B
*0B
*eB
*0B
*eB
*�B
*�B
,=B
,qB
,qB
-�B
-�B
.�B
.�B
.�B
/�B
1'B
1�B
1�B
1�B
2-B
1�B
2�B
1�B
0�B
0�B
0�B
0UB
0�B
0!B
0UB
0�B
0UB
0UB
0�B
1'B
1�B
1[B
1�B
1�B
1�B
33B
3hB
33B
4B
4B
4B
4nB
4�B
5B
5�B
5�B
6B
5�B
6zB
6�B
6�B
6�B
7B
7B
7LB
7�B
7LB
7LB
8�B
8�B
8�B
8RB
8�B
9$B
9$B
9$B
:�B
9�B
:*B
:^B
:*B
:�B
;0B
;0B
;�B
;�B
<6B
=B
=�B
=qB
=B
=B
=�B
>wB
>�B
>�B
?�B
@�B
@�B
@OB
@�B
A B
B�B
C�B
C�B
DgB
D3B
EmB
FB
FB
F?B
GB
GEB
GB
GB
GB
GzB
GEB
GEB
G�B
HKB
H�B
I�B
I�B
I�B
I�B
I�B
JXB
J�B
J�B
K^B
K^B
K)B
K)B
K)B
K^B
K�B
K�B
K�B
L�B
L�B
L�B
MB
MjB
M�B
M�B
M�B
NB
NpB
N�B
NpB
N�B
OB
N�B
N�B
OBB
OvB
OBB
O�B
O�B
O�B
P}B
P}B
P�B
P�B
P�B
QB
QB
QNB
Q�B
Q�B
Q�B
Q�B
R�B
S&B
R�B
S[B
S�B
S�B
S�B
S�B
TaB
T�B
T�B
T�B
T�B
T�B
U2B
U�B
UgB
U2B
T�B
U�B
U�B
V9B
V�B
W�B
WsB
W
B
W?B
XyB
XB
XEB
XEB
XyB
XyB
XyB
X�B
YKB
ZQB
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
[�B
[WB
[�B
[WB
\]B
\)B
\]B
\)B
\�B
\�B
\�B
\�B
\�B
]/B
]/B
]dB
]�B
^B
^jB
^�B
^�B
^�B
^�B
_;B
_pB
_B
_;B
_pB
_�B
`B
`BB
`vB
`vB
`vB
`vB
`�B
aHB
a�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dZB
d�B
d�B
e,B
e,B
e`B
e`B
e�B
e�B
f2B
f2B
f2B
e�B
f�B
g8B
g�B
gmB
g�B
g�B
g�B
g�B
h
B
h>B
hsB
hsB
h�B
h�B
iB
iDB
iyB
iDB
iyB
jB
jKB
jB
jB
jB
jKB
j�B
j�B
j�B
kQB
kB
kQB
k�B
k�B
k�B
k�B
k�B
k�B
lWB
l"B
l"B
l"B
l"B
l�B
l�B
m]B
m)B
m)B
m�B
m�B
ncB
n/B
ncB
ncB
ncB
n�B
o B
o�B
o�B
o�B
p;B
p;B
poB
poB
p�B
qB
qvB
qvB
q�B
q�B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sB
sB
sB
s�B
sB
s�B
s�B
s�B
s�B
tB
tB
tB
t�B
u%B
u�B
u�B
v+B
v`B
v`B
v`B
v�B
v�B
w2B
w2B
w2B
wfB
x8B
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y>B
y�B
y�B
y�B
y�B
zDB
zxB
z�B
z�B
{JB
{B
{JB
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|PB
|PB
{�B
|B
|�B
|�B
|�B
|�B
}"B
}VB
}�B
~�B
�iB
��B
�B
��B
�oB
��B
��B
��B
�AB
�AB
�B
�B
��B
��B
��B
�B
�{B
�GB
��B
�GB
�{B
�{B
��B
��B
��B
��B
�B
��B
�MB
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
�B
�SB
��B
��B
��B
��B
��B
��B
�%B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�_B
��B
��B
��B
�fB
��B
�7B
�B
��B
�lB
��B
�7B�'B�B�UB��B��B��B�'B��B��B��B��B� B��B� B�'B��B��B��B��B��B��B� B�[B�OB�B��B�'B�OB�B�UB��B�}B�OB�UB�'B�UB�OB�OB�UB��B��B��B�UB��B��B��B�UB�'B� B��B� B��B��B��B�UB��B��B��B�'BB��B��B�UBBB�UB��B�[BB��B� B��B�'B��B��B��B�[B�UB��B��B�'B��B��B�'B�'B� B��B� B��B��B��B�[B�[B��B� BB��B�OB� B��BB� B��B��B��B��B��B��B��B�'B��B�'BB��B� BB�-B��B� B�-B�-B��B� B�'B��B��B� BB�-B� B� B��B��B��B� B��BB��B��B��B��BB� B�'B�aB��B�UB�UBB��B��B� B�UB��B��B��B��B�-B�[B� B��B�-BB��B�'B��B��B�UB��B��BB� B��B�-B��B��B��BB��B�'B�UB�[B�aB��B� B��B�-B��B� BB��B��B��B��B��B�'B� B��B��B�'B��B��BB�'B��B��BB�OB�OB�UB��B��B��B��B�?B�B�gBĜBǮB��B�)B��B�&BѷB�B�
B�gB�9B��B�5B�;B�`B�B�5B�JB�B��B��B�B��B�fB��B	�B	!�B	�B	~B	-�B	(�B	+�B	/�B	,=B	1[B	2�B	3�B	33B	4�B	49B	>BB	8�B	=<B	9�B	7LB	6zB	9$B	:^B	;dB	;0B	;�B	?�B	A B	A�B	A�B	?}B	@�B	A�B	A�B	A�B	@OB	?}B	A B	C�B	B[B	C�B	D�B	F�B	I�B	K)B	J�B	LdB	PHB	S�B	V�B	XEB	^B	c�B	h
B	i�B	i�B	j�B	l�B	m�B	n/B	pB	s�B	u%B	v+B	u�B	xlB	�B	�SB	��B	��B	�SB	��B	�DB	�B	�"B	��B	��B	�.B	��B	��B	��B	�oB	��B	�B	�MB	��B	�{B	��B	�SB	��B	��B	��B	��B	��B	�_B	�YB	��B	��B	��B	��B	��B	��B	�B	��B	�$B	�YB	��B	�+B	�+B	��B	�	B	��B	��B	�nB	��B	��B	��B	��B	�FB	��B	��B	��B	��B	�B	��B	��B	��B	�$B	��B	�XB	�_B	��B	��B	�kB	��B	��B	�wB	�!B	�-B	��B	��B	�UB	��B	�-B	��B	��B	�-B	�aB	�hB	��B	��B	��B	��B	��B	��B	�^B	��B	��B	�$B	��B	��B	��B	�*B	�XB	�$B	�*B	�0B	��B	��B	��B	�XB	�*B	��B	�^B	��B	��B	�RB	��B	�B	�B	�}B	�wB	�B	�B	�UB	��B	��B	��B	�UB	ĜB	�'B	��B	��B	�OB	� B	��B	B	��B	�3B	�mB	�B	�B	�zB	�KB	ǮB	�EB	�B	ʌB	˒B	�dB	�dB	͟B	�6B	��B	�BB	�B	�}B	�B	ϫB	�BB	�B	уB	�aB	�B	�yB	�B	�pB	�|B	�`B	�B	��B	�B
B	��B	��B

rB
:�B
I�B
C�B
HB
JXB
MjB
T�B
`BB
c�B
o�B
o�B
z�B
{JB
}VB
� B
�iB
�iB
��B
�_B
�SB
��B
��B
�B
�xB
��B
�+B
�_B
�fB
��B
�PB
�B
��B
�wB
��B
��B
�0B
��B
��B
ƨB
�9B
��B(B
��B
��B
�B
�oB
�oB
�;B
�GB
�B
�/B
�GB
�cB
�B
�iB
�AB
�|B
�B
��B9�B�BeB \B"4B$@B"4B!�B"�B%�B$tB&�B*�B$@B�B�B&B=<BV�B"hB"�BxBqB=BB1BqB�B�B�BB�B!B"hB"4B$tB#�B#�B$tB%�B$tB%FB&LB&�B&�B%�B(�B6�B>�BY�B�B��B��B�B}"BzBy	ByrB�_B��B�{Bt�B�SB��B��B�+B�=B�qB��B��B��B�B��B��B��B��B��B�B�RB�eB�hB�qB��B�B��B��B�FB�^B��B�$B��B��B�$B��B�\B�hB�-B��B��B�'B��B�$B��B��B��B��B��B�xB�B�!B�OB�B�:B��B��B��B��B�~B�~B�B�CB�$B��B�:B�(B��B��B�"B��B�B�{B��B�B� B{�B|B.B��BxlBx8Bv+Bv`Bv�B}�Br�BwfB|PB�_Bx8BpBp;Bj�Bd�B_�B[�B[�BQ�B_�BN<BW�BC�B4�B-�B)�B'�B*eB 'B�BoB�B�B�B�B
��B
�B
�`B�B9�B
�QB
ǮB
�9B
��B
��B
�$B
��B
�-B
��B
��B
��B
��B
�@B
��B
��B
�7B
�B
��B
}�B
� B
��B
{B
kQB
b�B
bB
\�B
FtB
AUB
<B
,B
�B
B
{B	�]B
�B
!�B
B	��B	�B	��B	�KB	�|B	�B	�BB	͟B	��B	��B	��B	��B	ޞB	��B	�aB	��B	��B	�[B	�^B	��B	�FB	��B	�	B	��B	�xB	~]B	�B	�B	l�B	bB	��B	�B	jKB	lWB	_pB	V�B	^jB	`�B	Q�B	P�B	R�B	RTB	NpB	>B	7�B	@OB	=�B	N�B	[WB	yrB	B�B	:�B	6zB	<�B	A�B	<6B	;0B	:�B	4nB	4�B	1�B	2aB	1�B	2�B	8B	>BB	3�B	4B	9�B	1�B	5tB	-CB	,�B	0�B	.IB	/�B	0!B	B�B	6�B	\�B	=<B	6FB	=<B	2�B	;�B	=<B	9XB	7LB	;dB	<B	9XB	7�B	7LB	<�B	@�B	A�B	=<B	M�B	>�B	I�B	M�B	X�B	`BB	Z�B	A�B	<�B	>�B	YKB	YKB	iB	`�B	`�B	\�B	\)B	Y�B	ZQB	d�B	��B	}"B	tTB	�uB	zDB	~(B	��B	��B	�uB	�xB	�PB	��B	��B	�xB	��B	��B	��B	��B	�VB	�B	��B	�:B	��B	�oB	��B	�FB	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                   B��B��B�oB��B�oB�B�B� B�B� B��B�;B�B�;B�;B�oB��B��B�;B�;B�oB�oB��B�AB�AB�AB�'B�B�AB�AB�AB�uB�B��B�-B�B�mB�}B	EB	-]B	7�B	;dB	>(B	AoB	C�B	M�B	c B	qvB	HB	��B	�B	�vB	��B
(�B
�XB
�GB
��B# B=�B'�B"�B6zB��B�aB��BªB��B��B�B�XB�4B�mB��B�RBu�BR�B*B�B
��B
��B
�B
M�B
�B	�B	�B	�-B	�QB	o�B	dB	\�B	?HB	<B	;0B	A�B	A�B	@�B	Q�B	cB	UgB	g�B	~�B	��B	��B	� B	��B	�$B	��B	�B	��B	��B	רB	��B	��B
�B
TB
]B
!bB
'RB
+�B
0�B
4nB
1�B
9$B
A�B
?cB
=�B
@�B
GzB
?cB
>BB
=B
:�B
9rB
<�B
MPB
X�B
[�B
\�B
]�B
_;B
^OB
_�B
_�B
_!B
`vB
^�B
Y�B
X�B
X�B
[�B
P�B
C�B
9�B
9	B
8�B
:�B
<�B
<6B
=�B
EB
H�B
DMB
D�B
B'B
@�B
A�B
AoB
@�B
VB
]B
[WB
Y�B
YKB
WsB
WsB
XB
[=B
[�B
\)B
\]B
\]B
ZkB
W�B
W�B
WYB
XyB
X�B
Y1B
XEB
X�B
WsB
W?B
XyB
Y�B
X�B
X�B
Y1B
X�B
Y�B
W�B
V�B
U�B
UgB
V�B
W�B
R�B
QhB
P}B
P}B
N�B
M�B
LdB
K�B
L�B
HB
E�B
DB
CB
B�B
BuB
C-B
?�B
>]B
=qB
<�B
<�B
<jB
<�B
;�B
:�B
:�B
;�B
9	B
8RB
6�B
5�B
7�B
72B
2aB
2-B
2aB
/B
.}B
./B
+�B
-wB
,B
*�B
+B
+�B
+kB
+kB
+6B
*0B
)B
)�B
)�B
(XB
'RB
'RB
'B
&�B
'�B
(sB
&LB
&LB
'mB
'RB
&�B
%B
$�B
$B
#nB
#:B
"�B
$B
!-B
 �B
 �B
!|B
 \B
�B
�B
�B
�B
IB
�B
WB
kB
�B
�B
eB
�B
�B
B
mB
�B
B
 B
:B
�B
uB
gB
gB
gB
?B
?B
2B
�B
 B
�B
.B
�B
�B
 B
VB
B
B
B
�B
�B
dB

�B
�B
^B
^B

�B

�B

�B
fB
�B
YB
tB
�B
B
B
�B
�B
gB
{B
GB
�B
-B
aB
B
�B
�B
gB
3B
9B
�B
B
�B
	RB

#B

�B
)B
xB
�B
B
�B
dB
B
JB
�B
0B
B
�B
B
�B
�B
�B
B
}B
4B
4B
 B
:B
�B
BB
\B
}B
 B
hB
 B
�B
�B
B
hB
 B
aB
[B
B
&B
uB
�B
B
aB
�B
�B
[B
uB
�B
�B

B
MB
�B
�B
�B
�B
2B
�B
FB
�B
mB
�B
$B
+B
yB
�B
�B
�B
�B
�B
)B
qB
�B
�B
xB
xB
xB
�B
�B
�B
CB
�B
]B
B
�B
IB
�B
B
�B
B
~B
�B
�B
/B
�B
dB
�B
jB
 \B
�B
�B
�B
xB
xB
�B
�B
B
xB
]B
B
5B
/B
�B
B
5B
�B
B
!B
�B
�B
�B
!B
�B
 �B
"hB
"�B
#�B
#�B
$&B
$ZB
%B
&2B
&B
$�B
$�B
%FB
%�B
&�B
&�B
&fB
&LB
&2B
%�B
$�B
%FB
%zB
%FB
$ZB
$�B
$�B
%�B
&B
%�B
%zB
&B
&�B
'�B
'�B
'�B
(�B
)yB
)DB
)DB
)�B
*B
*�B
*�B
*�B
+6B
+�B
+�B
-)B
-)B
,�B
.cB
.�B
/OB
/�B
0!B
0oB
1�B
2|B
2aB
2�B
2�B
3B
3�B
1�B
1'B
1AB
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1B
1�B
1�B
1�B
2GB
2-B
2�B
4B
3�B
3�B
4�B
4�B
4�B
4�B
5ZB
6FB
6zB
6�B
6zB
6FB
7B
7B
6�B
72B
7�B
7�B
7�B
7�B
7�B
8RB
9$B
8�B
8�B
8�B
9XB
9rB
9�B
:DB
;0B
:^B
:�B
:�B
:xB
;0B
;�B
;�B
<B
<6B
<�B
=�B
>]B
=�B
=qB
=�B
>BB
>�B
>�B
?.B
@�B
@�B
@�B
@�B
A;B
BB
C�B
DB
D�B
D�B
D�B
FB
FYB
F?B
F�B
GzB
G_B
G+B
GEB
G_B
G�B
G�B
G�B
H�B
H�B
I�B
J#B
J=B
J=B
J=B
JXB
J�B
K^B
K^B
K�B
KxB
KDB
K^B
K�B
K�B
K�B
LdB
L~B
MB
L�B
MB
M�B
M�B
N<B
NB
M�B
N�B
N�B
N�B
N�B
O\B
O(B
N�B
O(B
O�B
O�B
O�B
P.B
PB
PHB
P�B
P�B
QB
QB
QB
QhB
QhB
Q�B
R B
R B
R B
RoB
S&B
SuB
S@B
S�B
S�B
T,B
S�B
T{B
T�B
T�B
T�B
T�B
T�B
UMB
U�B
U�B
U�B
U�B
UgB
U�B
VB
V�B
WYB
W�B
W�B
WYB
XB
X�B
XEB
X_B
X_B
X�B
X�B
X�B
Y1B
Z7B
ZkB
Y�B
ZB
ZkB
[WB
[	B
[	B
[�B
[�B
[�B
[�B
\�B
\]B
\�B
\]B
\�B
\�B
\�B
\�B
]IB
]~B
]~B
]�B
^5B
^jB
^�B
^�B
_B
^�B
_VB
_�B
_�B
_pB
_�B
_�B
`B
`vB
`�B
`�B
`�B
`�B
`�B
abB
bB
b�B
c:B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dZB
d�B
d�B
eFB
e`B
e`B
e�B
e�B
e�B
fLB
f�B
fLB
ffB
f�B
gRB
g�B
g�B
g�B
g�B
g�B
g�B
h$B
hsB
hXB
hsB
h�B
h�B
h�B
iyB
i_B
i�B
iyB
j0B
jKB
jeB
j0B
jKB
jeB
j�B
kB
kB
kQB
kkB
k6B
k�B
k�B
lB
lB
k�B
l"B
l=B
lqB
l"B
l=B
l"B
lqB
l�B
mB
m�B
mCB
mwB
nB
m�B
n�B
nIB
n}B
n�B
n�B
oB
oiB
o�B
o�B
o�B
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
r-B
r-B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
sMB
shB
s�B
sMB
s�B
tB
s�B
tB
t9B
tTB
t�B
uB
u�B
u�B
v+B
vFB
vzB
vzB
vzB
v�B
v�B
wLB
wLB
w2B
wfB
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z*B
z^B
z�B
z�B
z�B
{B
{0B
{dB
{�B
|B
|�B
|�B
}B
|�B
|�B
|jB
|jB
|B
|B
}"B
}<B
}<B
}VB
}<B
}<B
}<B
~�B
�OB
��B
�UB
��B
��B
��B
��B
��B
�AB
�[B
�B
�-B
��B
��B
�B
�aB
��B
�aB
��B
�{B
��B
��B
�B
�B
��B
��B
�B
�3B
��B
��B
��B
�B
��B
��B
��B
�9B
�9B
�B
�SB
�9B
�mB
��B
��B
��B
��B
��B
�%B
�YB
��B
��B
��B
��B
�+B
��B
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
�1B
��B
�B
�7B
�B
��B
��B
��G�O�B�'B�B�UB��B��B��B�'B��B��B��B��B� B��B� B�'B��B��B��B��B��B��B� B�[B�OB�B��B�'B�OB�B�UB��B�}B�OB�UB�'B�UB�OB�OB�UB��B��B��B�UB��B��B��B�UB�'B� B��B� B��B��B��B�UB��B��B��B�'BB��B��B�UBBB�UB��B�[BB��B� B��B�'B��B��B��B�[B�UB��B��B�'B��B��B�'B�'B� B��B� B��B��B��B�[B�[B��B� BB��B�OB� B��BB� B��B��B��B��B��B��B��B�'B��B�'BB��B� BB�-B��B� B�-B�-B��B� B�'B��B��B� BB�-B� B� B��B��B��B� B��BB��B��B��B��BB� B�'B�aB��B�UB�UBB��B��B� B�UB��B��B��B��B�-B�[B� B��B�-BB��B�'B��B��B�UB��B��BB� B��B�-B��B��B��BB��B�'B�UB�[B�aB��B� B��B�-B��B� BB��B��B��B��B��B�'B� B��B��B�'B��B��BB�'B��B��BB�OB�OB�UB��B��B��B��B�?B�B�gBĜBǮB��B�)B��B�&BѷB�B�
B�gB�9B��B�5B�;B�`B�B�5B�JB�B��B��B�B��B�fB��B	�B	!�B	�B	~B	-�B	(�B	+�B	/�B	,=B	1[B	2�B	3�B	33B	4�B	49B	>BB	8�B	=<B	9�B	7LB	6zB	9$B	:^B	;dB	;0B	;�B	?�B	A B	A�B	A�B	?}B	@�B	A�B	A�B	A�B	@OB	?}B	A B	C�B	B[B	C�B	D�B	F�B	I�B	K)B	J�B	LdB	PHB	S�B	V�B	XEB	^B	c�B	h
B	i�B	i�B	j�B	l�B	m�B	n/B	pB	s�B	u%B	v+B	u�B	xlB	�B	�SB	��B	��B	�SB	��B	�DB	�B	�"B	��B	��B	�.B	��B	��B	��B	�oB	��B	�B	�MB	��B	�{B	��B	�SB	��B	��B	��B	��B	��B	�_B	�YB	��B	��B	��B	��B	��B	��B	�B	��B	�$B	�YB	��B	�+B	�+B	��B	�	B	��B	��B	�nB	��B	��B	��B	��B	�FB	��B	��B	��B	��B	�B	��B	��B	��B	�$B	��B	�XB	�_B	��B	��B	�kB	��B	��B	�wB	�!B	�-B	��B	��B	�UB	��B	�-B	��B	��B	�-B	�aB	�hB	��B	��B	��B	��B	��B	��B	�^B	��B	��B	�$B	��B	��B	��B	�*B	�XB	�$B	�*B	�0B	��B	��B	��B	�XB	�*B	��B	�^B	��B	��B	�RB	��B	�B	�B	�}B	�wB	�B	�B	�UB	��B	��B	��B	�UB	ĜB	�'B	��B	��B	�OB	� B	��B	B	��B	�3B	�mB	�B	�B	�zB	�KB	ǮB	�EB	�B	ʌB	˒B	�dB	�dB	͟B	�6B	��B	�BB	�B	�}B	�B	ϫB	�BB	�B	уB	�aB	�B	�yB	�B	�pB	�|B	�`B	�B	��B	�B
B	��B	��B

rB
:�B
I�B
C�B
HB
JXB
MjB
T�B
`BB
c�B
o�B
o�B
z�B
{JB
}VB
� B
�iB
�iB
��B
�_B
�SB
��B
��B
�B
�xB
��B
�+B
�_B
�fB
��B
�PB
�B
��B
�wB
��B
��B
�0B
��B
��B
ƨB
�9B
��B(B
��B
��B
�B
�oB
�oB
�;B
�GB
�B
�/B
�GB
�cB
�B
�iB
�AB
�|B
�B
��B9�B�BeB \B"4B$@B"4B!�B"�B%�B$tB&�B*�B$@B�B�B&B=<BV�B"hB"�BxBqB=BB1BqB�B�B�BB�B!B"hB"4B$tB#�B#�B$tB%�B$tB%FB&LB&�B&�B%�B(�B6�B>�BY�B�B��B��B�B}"BzBy	ByrB�_B��B�{Bt�B�SB��B��B�+B�=B�qB��B��B��B�B��B��B��B��B��B�B�RB�eB�hB�qB��B�B��B��B�FB�^B��B�$B��B��B�$B��B�\B�hB�-B��B��B�'B��B�$B��B��B��B��B��B�xB�B�!B�OB�B�:B��B��B��B��B�~B�~B�B�CB�$B��B�:B�(B��B��B�"B��B�B�{B��B�B� B{�B|B.B��BxlBx8Bv+Bv`Bv�B}�Br�BwfB|PB�_Bx8BpBp;Bj�Bd�B_�B[�B[�BQ�B_�BN<BW�BC�B4�B-�B)�B'�B*eB 'B�BoB�B�B�B�B
��B
�B
�`B�B9�B
�QB
ǮB
�9B
��B
��B
�$B
��B
�-B
��B
��B
��B
��B
�@B
��B
��B
�7B
�B
��B
}�B
� B
��B
{B
kQB
b�B
bB
\�B
FtB
AUB
<B
,B
�B
B
{B	�]B
�B
!�B
B	��B	�B	��B	�KB	�|B	�B	�BB	͟B	��B	��B	��B	��B	ޞB	��B	�aB	��B	��B	�[B	�^B	��B	�FB	��B	�	B	��B	�xB	~]B	�B	�B	l�B	bB	��B	�B	jKB	lWB	_pB	V�B	^jB	`�B	Q�B	P�B	R�B	RTB	NpB	>B	7�B	@OB	=�B	N�B	[WB	yrB	B�B	:�B	6zB	<�B	A�B	<6B	;0B	:�B	4nB	4�B	1�B	2aB	1�B	2�B	8B	>BB	3�B	4B	9�B	1�B	5tB	-CB	,�B	0�B	.IB	/�B	0!B	B�B	6�B	\�B	=<B	6FB	=<B	2�B	;�B	=<B	9XB	7LB	;dB	<B	9XB	7�B	7LB	<�B	@�B	A�B	=<B	M�B	>�B	I�B	M�B	X�B	`BB	Z�B	A�B	<�B	>�B	YKB	YKB	iB	`�B	`�B	\�B	\)B	Y�B	ZQB	d�B	��B	}"B	tTB	�uB	zDB	~(B	��B	��B	�uB	�xB	�PB	��B	��B	�xB	��B	��B	��B	��B	�VB	�B	��B	�:B	��B	�oB	��B	�FB	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<WZ<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<H�<�b�<}�J<Řk<f%�<�"�<���<#�
<#�
<PŴ<���<#�
<#�
<`b<'g�<#�
<#�
<#�
<@9e<>�1<0Q<}�J<��p<��J<�M.<�ν<9�<k4�<氵<�CW<�7�<�hH<޺><���<�H�<�b�<�Q�<�*<#�
<#�
<#�
<�kX<#�
<#�
<�hH<��<9�<3>�<t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018030423261720180304232617IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018031500021020180315000210QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018031500021020180315000210QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550620190521075506IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                