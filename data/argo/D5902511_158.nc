CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-01-18T20:32:07Z creation; 2021-04-29T20:27:09Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20210118203207  20210429202818  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_158                 6810_008521_158                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�W���+�@�W���+�11  @�W���r@�W���r@2���$tT@2���$tT�e���a��e���a�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              
   
?��@   @B�\@}p�@��R@�G�@�  @��RA��A ��A+�A>�RAaG�A�  A�\)A�\)A��A�  A�Q�A�Q�A�Q�B Q�BQ�BQ�B  B�B'�B0  B8(�B?�
BG�
BO�BW�
B`Q�Bh(�Bo�
Bx  B�
B��B��B�  B�{B�  B��B�{B�(�B�  B�  B�{B�  B�  B�(�B�{B��B��B��
B��B�  B��B�  B�{B�  B�{B�{B�  B��B��B�  B�  B��C  C  C��C��C
  C  C��C
=C{C
=C  C
=C
=C��C��C   C!��C#��C%��C'��C)��C+�C-��C0  C1��C4
=C6
=C8  C:  C<
=C>
=C@  CB  CD  CE��CG��CJ  CL  CN{CP  CQ�CS��CU�CW��CY��C\
=C^  C`  Cb
=Cd  Cf  Ch
=Cj  Cl  Cn
=Cp
=Cr{Ct{Cv
=Cx
=Cz
=C|�C~
=C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C�
=C�C�  C�C�  C���C���C���C�  C�  C�  C�
=C�  C�C�C�C�  C�  C�  C�  C���C���C�C���C�  C���C�  C�  C�  C�
=C�C�  C���C���C�C�  C���C���C�  C�  C�C�  C���C�  C�  C���C�  C�C�  C���C���C���C�  C�  C�  C�  C�  C���C�C�  C�C�C�  C�
=C���C�  C�C�
=C�
=C�C���C���C�  C�  C�  C�  C�C�  C���C���C���C�  C�  C���C���C���C�  C�  C���C���C�  C�  C���C���C�C�C�  C�C�  C���C���C�  C�  C�
=C�C�C�  C�C���C�  C�C�  C���C���C�C�  C�  D �D ��D �qD��D  D}qD�D�D��Dz�D�qD}qD  D� D��DxRD��D}qD	  D	}qD
  D
��D  D� DD�D�D� D�qDz�D  D}qD�qD� D  Dz�D��D� D  D}qD�qDxRD��D}qD  D� D�qD}qD  D�D�D� D�qDz�D  D�DD��D�D� D  D}qD�D� D �D ��D ��D!z�D!�qD"}qD"�qD#}qD#�qD$}qD$��D%z�D%��D&� D&�qD'� D(  D(��D(�qD)}qD*�D*� D*�qD+� D,D,��D-�D-��D-�qD.� D/  D/� D0�D0� D1  D1��D2�D2��D3�D3� D4  D4��D5�D5��D6�D6�D7�D7��D8  D8��D9  D9� D:D:� D:��D;� D;�qD<}qD=  D=}qD>  D>}qD>�qD?}qD?�qD@��DADA}qDB  DB�DC�DC� DD  DD� DE�DE� DF�DF� DG  DG� DG�qDH� DI  DI� DJ�DJ� DJ�qDK� DLDL��DL�qDM� DM�qDNz�DN��DO� DP�DP��DQ  DQ� DR�DR� DR�qDS� DT�DT��DU  DU}qDU��DV}qDW  DW��DXDX��DY  DY}qDY��DZz�DZ�qD[� D\D\�D]�D]� D]�qD^��D_�D_}qD`  D`}qDa  Da� Da��Dbz�Dc  Dc� Dc�qDd}qDd�qDe}qDf  Df� Dg  Dg��Dh�Dh��Di  Di}qDj�Dj��Dk  Dk� Dk�qDl��Dm�Dm��Dn�Dn� Do  Do� Do�qDp� Dq  Dq� Dr  Dr� Ds  Ds��Dt  Dt��Du�Du��Du�qDv� Dw�Dw��Dw�qDx� DyDy� Dy�qDz� D{D{��D|  D|}qD|�qD}��D~D~�D�D� D�  D�@ D�� D���D���D�@ D�~�D���D�  D�AHD�� D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D���D�>�D�� D�� D���D�@ D���D��HD�HD�@ D�� D�� D�HD�AHD�� D���D�  D�AHD��HD��HD�HD�AHD��HD�� D�  D�AHD��HD�D�HD�>�D�~�D�D��D�@ D�� D���D��qD�>�D�� D�D��D�@ D��HD��HD�  D�@ D��HD�� D���D�AHD��HD��HD�HD�@ D��HD��HD�HD�B�D��HD��HD�HD�@ D�� D��HD�HD�@ D�~�D��HD�HD�>�D�~�D�� D��D�AHD�}qD���D�HD�@ D�~�D���D�  D�B�D���D��HD�  D�AHD��HD���D���D�>�D�}qD��qD���D�@ D�� D���D�  D�AHD�~�D���D�HD�@ D�� D��HD�  D�@ D�� D���D�  D�>�D�� D��HD�HD�@ D�~�D�� D�  D�@ D�~�D��qD���D�AHD��HD���D�HD�@ D�}qD���D�  D�@ D�~�D���D���D�@ D���D�� D�  D�AHD�� D�� D���D�>�D�� D�� D�HD�>�D�� D�� D�  D�@ D�� D��HD�HD�@ D�~�D��HD�HD�@ D�~�D���D���D�AHD���D�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D���D�  D�>�D�~�D���D�  D�AHD�� D�� D�HD�=qD�� D�D�HD�>�D�~�D���D��qD�>�D�� D���D��qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�� D���D�HD�C�D��HD��qD���D�@ D�~�D��HD�  D�>�D�� D�� D�  D�AHD�� D���D�  D�@ D�� D��HD��D�AHD�� D��HD��D�AHD�� D�� D�HD�AHD�� D���D���D�>�D�� D���D��qD�>�D�~�D¾�D���D�@ DÁHDþ�D�  D�AHD�~�D�� D�  D�>�D�~�D�� D�HD�>�D�~�Dƾ�D���D�AHDǁHD��HD�HD�@ DȁHD�� D�  D�>�D�~�D��HD�  D�>�DʁHD��HD�  D�AHDˀ D��HD���D�=qD�}qD�� D�  D�@ D̀ D�� D�  D�@ D�~�DνqD�  D�AHDπ D�� D�  D�AHDЁHD�� D�  D�=qD�}qD�� D�  D�@ DҁHD��HD��D�AHDӁHD�� D���D�<)D�~�DԾ�D��qD�AHDՂ�Dվ�D���D�@ D�~�D��HD�HD�AHDׁHD׾�D�  D�>�D؀ D�D�HD�AHDفHD��HD�HD�@ D�~�D��HD�  D�<)D�~�D��HD�HD�AHD�~�DܽqD���D�@ D݁HD��HD�  D�>�DށHD�D�HD�B�D߂�D�� D�  D�AHD�� D�� D���D�>�D� D�� D�HD�@ D�~�D��HD��D�B�D� D㾸D�HD�@ D�~�D�� D�HD�@ D� D�� D��qD�=qD� D��HD�HD�@ D�~�D�qD���D�@ D�HD��HD��qD�>�D�~�D龸D�HD�AHD� D�qD���D�@ D�HD��HD�HD�>�D� D�D�  D�>�D� D�� D�  D�AHD� D�� D�  D�>�D�~�DﾸD���D�@ D�� D��HD�  D�=qD�}qD�D���D�AHD�HD�� D�  D�@ D�~�D�� D�  D�>�D�HD��HD�  D�@ D�� D�� D��qD�>�D�� D�� D�  D�@ D�}qD�� D�HD�@ D�~�D�� D�HD�@ D�� D���D���D�1�>�?L��?�z�?�Q�?�
=?��H@z�@(��@=p�@E�@\(�@n{@�G�@�=q@�@�(�@��@�{@�
=@�(�@��
@���@���@��H@޸R@��@��@�
=@�p�A�
A�A��A  A�\Az�A=qA�RA!G�A#�
A*=qA,(�A0  A4z�A6ffA9��A?\)AB�\AE�AJ�HAO\)AS33AY��A^{Ac�
Aj=qAl��As33AxQ�A|(�A\)A��\A�{A�Q�A�=qA�p�A���A�=qA�{A���A�=qA�p�A�Q�A��\A��A�Q�A��HA��A���A��A�A���A��HA�A���A�33A�A�Q�A�33A�p�A�  A�33A�p�A�\)A�=qA�p�A�\)A�=qA��A�\)A陚A��A�
=A��A�p�A�  A�=qA��B Q�Bp�B�\B  B��B�RBz�B	�B
=BQ�BB\)BQ�Bp�B
=B�
BG�B�\B�B��BffB�B��B=qB�B ��B"=qB$(�B%�B&�\B(Q�B)G�B*�HB,z�B-��B.�HB0z�B1�B3
=B4(�B5�B733B8(�B9p�B:�HB;�
B<��B>�\B?�B@��BABC�BD��BEBG
=BHz�BI�BK
=BL(�BM�BO
=BP(�BQ�BS\)BT��BV{BW�BYp�B[
=B\z�B]�B_�BaG�BbffBc�
Bep�Bg
=Bh(�Bi�Bj�HBl(�Bm�BnffBo�
Bp��Br{Bs�
Bup�Bv�\Bx  By��Bz�RB|Q�B~{B�B�z�B�33B�{B���B�p�B�Q�B��B��B�ffB�G�B��B�z�B�p�B�(�B��RB�p�B�=qB�
=B���B�=qB�
=B��
B�z�B�
=B��
B���B�\)B��B���B��B�{B���B��B�=qB��RB�\)B�(�B��HB�p�B�  B��HB�\)B��B��RB�p�B�  B�z�B�33B�  B�z�B�
=B�B�ffB���B��B�(�B���B��B�B�ffB���B�33B��
B�ffB���B��B��B�{B�Q�B���B�G�B���B��
B�z�B��HB��B��B�  B��\B��HB��B���B�{B�Q�B��RB��B��B��B�=qB��RB�33B�p�B�B�=qB��RB��HB�G�B�B�{B�Q�B���B�33B��B�B�=qB��RB�
=B�G�B�B�=qB��\B��HB�33B�B�{B�Q�B��RB��B��B�B�=qB��RB���B�33B��B�(�B�Q�B¸RB�G�B�p�B�B�Q�Bď\B���B�G�B��
B�{B�Q�B���B�\)BǙ�B�{Bȏ\B��HB�33B�B�=qBʏ\B��HB�p�B��B�=qḄ�B�33BͅB��
B�ffB���B��B�p�B��B�Q�BУ�B���BхB��B�(�Bҏ\B�
=BӅB��
B�{BԸRB��B�G�B�B�Q�B֏\B���B�G�B�B�{B�=qBظRB�33BمB�B�(�Bڣ�B�
=B�G�B�B�=qB܏\B���B�\)B��
B�{Bޏ\B�
=B�\)Bߙ�B�(�B��B���B�33B�B�(�B�\B���B�33B�B�{B�Q�B���B�G�B噚B��
B�=qB���B�33B�p�B�B�Q�B�RB���B�33B�B�(�B�z�B�RB�33B뙚B��
B�{B��B��B�\)B홚B�{B�\B��HB�33B�B��
B�Q�B���B�
=B�\)B��
B�Q�B�RB���B�p�B��
B�ffB���B���B�\)B��B�Q�B��\B��HB�p�B��
B�{B�ffB���B�\)B���B��
B�=qB���B�
=B�G�B���B�{B��\B���B�
=B��B�  B�=qB��\B��HB�p�B��
C (�C G�C ffC ��C �C{C33CffC��C��C�C
=CG�C�C��CC  C=qC\)Cz�C�RC�C
=C(�C\)C��C�RC�C(�C\)Cz�C��C�C(�CG�Cp�C��C�C{C33CffC�C�
C
=C33C\)C��C�
C��C	�C	\)C	��C	��C	�C

=C
Q�C
�C
�C
��C
=CG�Cz�C��C��C��C33CffCz�C�C�C�CG�C\)C��C�HC  C{C\)C�\C�C��C
=CG�C\)C��C��C  C�C=qCz�C�RC�HC  C33Cp�C�\CC
=C(�CQ�C�\C��C�HC{C\)C�C�C��C(�CQ�Cz�C�RC  C(�CG�Cz�CC��C�CffC�\C�RC  C33C\)C�\C��C  C(�C\)C��C�
C  C=qC�C�C�
C{C\)C�C�RC  C=qCffC��C�C(�CQ�C�\C�HC�CG�Cz�C�
C
=C33Cz�C��C  C(�C\)C�C�C {C Q�C ��C �
C!
=C!G�C!��C!��C!��C"Q�C"�\C"�RC#  C#Q�C#z�C#��C#��C$=qC$ffC$��C$��C%=qC%z�C%��C&  C&=qC&p�C&��C'
=C'=qC'�\C'�
C({C(G�C(��C(�C){C)Q�C)�C)�C*�C*Q�C*�C*�C+�C+ffC+�RC+�C,(�C,p�C,C-  C-33C-p�C-C.{C.Q�C.�C.�
C/(�C/p�C/��C/�HC0(�C0�\C0��C1  C1G�C1��C1�HC2�C2Q�C2��C3  C3G�C3�C3C4{C4p�C4�C4�C5=qC5�\C5�HC6�C6ffC6��C7  C7Q�C7��C7�
C8{C8ffC8C9
=C9=qC9�C9�HC:(�C:\)C:��C;  C;G�C;z�C;C<�C<\)C<��C<��C={C=p�C=C>  C>=qC>p�C>�RC?{C?\)C?�C?�
C@(�C@ffC@��C@�CA=qCAz�CA�CA��CBG�CB��CB�
CC
=CCQ�CC��CC�CD{CDffCD�RCD�CE(�CEp�CE��CF{CF=qCF�CF�HCG(�CG\)CG��CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               ?��@   @B�\@}p�@��R@�G�@�  @��RA��A ��A+�A>�RAaG�A�  A�\)A�\)A��A�  A�Q�A�Q�A�Q�B Q�BQ�BQ�B  B�B'�B0  B8(�B?�
BG�
BO�BW�
B`Q�Bh(�Bo�
Bx  B�
B��B��B�  B�{B�  B��B�{B�(�B�  B�  B�{B�  B�  B�(�B�{B��B��B��
B��B�  B��B�  B�{B�  B�{B�{B�  B��B��B�  B�  B��C  C  C��C��C
  C  C��C
=C{C
=C  C
=C
=C��C��C   C!��C#��C%��C'��C)��C+�C-��C0  C1��C4
=C6
=C8  C:  C<
=C>
=C@  CB  CD  CE��CG��CJ  CL  CN{CP  CQ�CS��CU�CW��CY��C\
=C^  C`  Cb
=Cd  Cf  Ch
=Cj  Cl  Cn
=Cp
=Cr{Ct{Cv
=Cx
=Cz
=C|�C~
=C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C�
=C�C�  C�C�  C���C���C���C�  C�  C�  C�
=C�  C�C�C�C�  C�  C�  C�  C���C���C�C���C�  C���C�  C�  C�  C�
=C�C�  C���C���C�C�  C���C���C�  C�  C�C�  C���C�  C�  C���C�  C�C�  C���C���C���C�  C�  C�  C�  C�  C���C�C�  C�C�C�  C�
=C���C�  C�C�
=C�
=C�C���C���C�  C�  C�  C�  C�C�  C���C���C���C�  C�  C���C���C���C�  C�  C���C���C�  C�  C���C���C�C�C�  C�C�  C���C���C�  C�  C�
=C�C�C�  C�C���C�  C�C�  C���C���C�C�  C�  D �D ��D �qD��D  D}qD�D�D��Dz�D�qD}qD  D� D��DxRD��D}qD	  D	}qD
  D
��D  D� DD�D�D� D�qDz�D  D}qD�qD� D  Dz�D��D� D  D}qD�qDxRD��D}qD  D� D�qD}qD  D�D�D� D�qDz�D  D�DD��D�D� D  D}qD�D� D �D ��D ��D!z�D!�qD"}qD"�qD#}qD#�qD$}qD$��D%z�D%��D&� D&�qD'� D(  D(��D(�qD)}qD*�D*� D*�qD+� D,D,��D-�D-��D-�qD.� D/  D/� D0�D0� D1  D1��D2�D2��D3�D3� D4  D4��D5�D5��D6�D6�D7�D7��D8  D8��D9  D9� D:D:� D:��D;� D;�qD<}qD=  D=}qD>  D>}qD>�qD?}qD?�qD@��DADA}qDB  DB�DC�DC� DD  DD� DE�DE� DF�DF� DG  DG� DG�qDH� DI  DI� DJ�DJ� DJ�qDK� DLDL��DL�qDM� DM�qDNz�DN��DO� DP�DP��DQ  DQ� DR�DR� DR�qDS� DT�DT��DU  DU}qDU��DV}qDW  DW��DXDX��DY  DY}qDY��DZz�DZ�qD[� D\D\�D]�D]� D]�qD^��D_�D_}qD`  D`}qDa  Da� Da��Dbz�Dc  Dc� Dc�qDd}qDd�qDe}qDf  Df� Dg  Dg��Dh�Dh��Di  Di}qDj�Dj��Dk  Dk� Dk�qDl��Dm�Dm��Dn�Dn� Do  Do� Do�qDp� Dq  Dq� Dr  Dr� Ds  Ds��Dt  Dt��Du�Du��Du�qDv� Dw�Dw��Dw�qDx� DyDy� Dy�qDz� D{D{��D|  D|}qD|�qD}��D~D~�D�D� D�  D�@ D�� D���D���D�@ D�~�D���D�  D�AHD�� D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D���D�>�D�� D�� D���D�@ D���D��HD�HD�@ D�� D�� D�HD�AHD�� D���D�  D�AHD��HD��HD�HD�AHD��HD�� D�  D�AHD��HD�D�HD�>�D�~�D�D��D�@ D�� D���D��qD�>�D�� D�D��D�@ D��HD��HD�  D�@ D��HD�� D���D�AHD��HD��HD�HD�@ D��HD��HD�HD�B�D��HD��HD�HD�@ D�� D��HD�HD�@ D�~�D��HD�HD�>�D�~�D�� D��D�AHD�}qD���D�HD�@ D�~�D���D�  D�B�D���D��HD�  D�AHD��HD���D���D�>�D�}qD��qD���D�@ D�� D���D�  D�AHD�~�D���D�HD�@ D�� D��HD�  D�@ D�� D���D�  D�>�D�� D��HD�HD�@ D�~�D�� D�  D�@ D�~�D��qD���D�AHD��HD���D�HD�@ D�}qD���D�  D�@ D�~�D���D���D�@ D���D�� D�  D�AHD�� D�� D���D�>�D�� D�� D�HD�>�D�� D�� D�  D�@ D�� D��HD�HD�@ D�~�D��HD�HD�@ D�~�D���D���D�AHD���D�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D���D�  D�>�D�~�D���D�  D�AHD�� D�� D�HD�=qD�� D�D�HD�>�D�~�D���D��qD�>�D�� D���D��qD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�� D���D�HD�C�D��HD��qD���D�@ D�~�D��HD�  D�>�D�� D�� D�  D�AHD�� D���D�  D�@ D�� D��HD��D�AHD�� D��HD��D�AHD�� D�� D�HD�AHD�� D���D���D�>�D�� D���D��qD�>�D�~�D¾�D���D�@ DÁHDþ�D�  D�AHD�~�D�� D�  D�>�D�~�D�� D�HD�>�D�~�Dƾ�D���D�AHDǁHD��HD�HD�@ DȁHD�� D�  D�>�D�~�D��HD�  D�>�DʁHD��HD�  D�AHDˀ D��HD���D�=qD�}qD�� D�  D�@ D̀ D�� D�  D�@ D�~�DνqD�  D�AHDπ D�� D�  D�AHDЁHD�� D�  D�=qD�}qD�� D�  D�@ DҁHD��HD��D�AHDӁHD�� D���D�<)D�~�DԾ�D��qD�AHDՂ�Dվ�D���D�@ D�~�D��HD�HD�AHDׁHD׾�D�  D�>�D؀ D�D�HD�AHDفHD��HD�HD�@ D�~�D��HD�  D�<)D�~�D��HD�HD�AHD�~�DܽqD���D�@ D݁HD��HD�  D�>�DށHD�D�HD�B�D߂�D�� D�  D�AHD�� D�� D���D�>�D� D�� D�HD�@ D�~�D��HD��D�B�D� D㾸D�HD�@ D�~�D�� D�HD�@ D� D�� D��qD�=qD� D��HD�HD�@ D�~�D�qD���D�@ D�HD��HD��qD�>�D�~�D龸D�HD�AHD� D�qD���D�@ D�HD��HD�HD�>�D� D�D�  D�>�D� D�� D�  D�AHD� D�� D�  D�>�D�~�DﾸD���D�@ D�� D��HD�  D�=qD�}qD�D���D�AHD�HD�� D�  D�@ D�~�D�� D�  D�>�D�HD��HD�  D�@ D�� D�� D��qD�>�D�� D�� D�  D�@ D�}qD�� D�HD�@ D�~�D�� D�HD�@ D�� D���D���G�O�>�?L��?�z�?�Q�?�
=?��H@z�@(��@=p�@E�@\(�@n{@�G�@�=q@�@�(�@��@�{@�
=@�(�@��
@���@���@��H@޸R@��@��@�
=@�p�A�
A�A��A  A�\Az�A=qA�RA!G�A#�
A*=qA,(�A0  A4z�A6ffA9��A?\)AB�\AE�AJ�HAO\)AS33AY��A^{Ac�
Aj=qAl��As33AxQ�A|(�A\)A��\A�{A�Q�A�=qA�p�A���A�=qA�{A���A�=qA�p�A�Q�A��\A��A�Q�A��HA��A���A��A�A���A��HA�A���A�33A�A�Q�A�33A�p�A�  A�33A�p�A�\)A�=qA�p�A�\)A�=qA��A�\)A陚A��A�
=A��A�p�A�  A�=qA��B Q�Bp�B�\B  B��B�RBz�B	�B
=BQ�BB\)BQ�Bp�B
=B�
BG�B�\B�B��BffB�B��B=qB�B ��B"=qB$(�B%�B&�\B(Q�B)G�B*�HB,z�B-��B.�HB0z�B1�B3
=B4(�B5�B733B8(�B9p�B:�HB;�
B<��B>�\B?�B@��BABC�BD��BEBG
=BHz�BI�BK
=BL(�BM�BO
=BP(�BQ�BS\)BT��BV{BW�BYp�B[
=B\z�B]�B_�BaG�BbffBc�
Bep�Bg
=Bh(�Bi�Bj�HBl(�Bm�BnffBo�
Bp��Br{Bs�
Bup�Bv�\Bx  By��Bz�RB|Q�B~{B�B�z�B�33B�{B���B�p�B�Q�B��B��B�ffB�G�B��B�z�B�p�B�(�B��RB�p�B�=qB�
=B���B�=qB�
=B��
B�z�B�
=B��
B���B�\)B��B���B��B�{B���B��B�=qB��RB�\)B�(�B��HB�p�B�  B��HB�\)B��B��RB�p�B�  B�z�B�33B�  B�z�B�
=B�B�ffB���B��B�(�B���B��B�B�ffB���B�33B��
B�ffB���B��B��B�{B�Q�B���B�G�B���B��
B�z�B��HB��B��B�  B��\B��HB��B���B�{B�Q�B��RB��B��B��B�=qB��RB�33B�p�B�B�=qB��RB��HB�G�B�B�{B�Q�B���B�33B��B�B�=qB��RB�
=B�G�B�B�=qB��\B��HB�33B�B�{B�Q�B��RB��B��B�B�=qB��RB���B�33B��B�(�B�Q�B¸RB�G�B�p�B�B�Q�Bď\B���B�G�B��
B�{B�Q�B���B�\)BǙ�B�{Bȏ\B��HB�33B�B�=qBʏ\B��HB�p�B��B�=qḄ�B�33BͅB��
B�ffB���B��B�p�B��B�Q�BУ�B���BхB��B�(�Bҏ\B�
=BӅB��
B�{BԸRB��B�G�B�B�Q�B֏\B���B�G�B�B�{B�=qBظRB�33BمB�B�(�Bڣ�B�
=B�G�B�B�=qB܏\B���B�\)B��
B�{Bޏ\B�
=B�\)Bߙ�B�(�B��B���B�33B�B�(�B�\B���B�33B�B�{B�Q�B���B�G�B噚B��
B�=qB���B�33B�p�B�B�Q�B�RB���B�33B�B�(�B�z�B�RB�33B뙚B��
B�{B��B��B�\)B홚B�{B�\B��HB�33B�B��
B�Q�B���B�
=B�\)B��
B�Q�B�RB���B�p�B��
B�ffB���B���B�\)B��B�Q�B��\B��HB�p�B��
B�{B�ffB���B�\)B���B��
B�=qB���B�
=B�G�B���B�{B��\B���B�
=B��B�  B�=qB��\B��HB�p�B��
C (�C G�C ffC ��C �C{C33CffC��C��C�C
=CG�C�C��CC  C=qC\)Cz�C�RC�C
=C(�C\)C��C�RC�C(�C\)Cz�C��C�C(�CG�Cp�C��C�C{C33CffC�C�
C
=C33C\)C��C�
C��C	�C	\)C	��C	��C	�C

=C
Q�C
�C
�C
��C
=CG�Cz�C��C��C��C33CffCz�C�C�C�CG�C\)C��C�HC  C{C\)C�\C�C��C
=CG�C\)C��C��C  C�C=qCz�C�RC�HC  C33Cp�C�\CC
=C(�CQ�C�\C��C�HC{C\)C�C�C��C(�CQ�Cz�C�RC  C(�CG�Cz�CC��C�CffC�\C�RC  C33C\)C�\C��C  C(�C\)C��C�
C  C=qC�C�C�
C{C\)C�C�RC  C=qCffC��C�C(�CQ�C�\C�HC�CG�Cz�C�
C
=C33Cz�C��C  C(�C\)C�C�C {C Q�C ��C �
C!
=C!G�C!��C!��C!��C"Q�C"�\C"�RC#  C#Q�C#z�C#��C#��C$=qC$ffC$��C$��C%=qC%z�C%��C&  C&=qC&p�C&��C'
=C'=qC'�\C'�
C({C(G�C(��C(�C){C)Q�C)�C)�C*�C*Q�C*�C*�C+�C+ffC+�RC+�C,(�C,p�C,C-  C-33C-p�C-C.{C.Q�C.�C.�
C/(�C/p�C/��C/�HC0(�C0�\C0��C1  C1G�C1��C1�HC2�C2Q�C2��C3  C3G�C3�C3C4{C4p�C4�C4�C5=qC5�\C5�HC6�C6ffC6��C7  C7Q�C7��C7�
C8{C8ffC8C9
=C9=qC9�C9�HC:(�C:\)C:��C;  C;G�C;z�C;C<�C<\)C<��C<��C={C=p�C=C>  C>=qC>p�C>�RC?{C?\)C?�C?�
C@(�C@ffC@��C@�CA=qCAz�CA�CA��CBG�CB��CB�
CC
=CCQ�CC��CC�CD{CDffCD�RCD�CE(�CEp�CE��CF{CF=qCF�CF�HCG(�CG\)CG��CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��yA��mA��mA��mA��`A��mA��A��A��A��A���A�  A���A���A��A��A���A�{A��A��A��A�oA�VA��A��A��A��A��A�{A�oA�oA��A�$�A� �A�$�A�A�A�A�A�G�A�S�A�\)A�ffA�bNA�bNA�jA�x�A�dZA�?}A�dZA�n�Aԉ7A԰!AԴ9A���A���A�=qA�r�A�z�A�^5A��A���A�bNA�  A�5?A�9XA��
A�(�A��A�K�A���A���A��mA�9XA�ffA���A���A�G�A��A��FA�I�A�S�A�O�A���A��RA���A��A�S�A��A�/A�C�A���A�ZA�Q�A���A�"�A���A�%A���A�ȴA��!A��PA��A�G�A��yA�VA��9A�?}A���A��yA�A��
A�|�A���A��yA���A�r�A���A��^A��A�M�A���A�n�A�r�A���A��A��A��A��+A��-A�O�A��DA�$�A��#A�^5A�VA��hA}��A{�mA{G�AyXAwAv��Av�AtbArAp=qAmx�AlM�Ak�Ak��Ah�uAf��Ad�jAc�#AbM�A^�9A\��AZ�+AZ-AY�AX��AV5?AS��AQ?}AP�DAO%ALr�AK�hAIp�AH=qAE��AC�A?\)A<�jA;��A:��A9O�A8��A8�!A7|�A6�/A5VA3��A2��A2ZA2  A1�#A1��A1`BA/`BA,��A+"�A)�;A(�`A'��A&I�A#�#A#�A"�9A!�TA JA�+A��AVAƨA�jAE�AI�A\)A/A�AK�AȴAJAK�A�FA9XA��A{A��A��A
ffA	
=A1A`BA?}A��A�A��AbNA�#A�AS�A�!AS�A ��A (�@���@�ff@���@��P@��@��T@���@��F@���@��`@��@�o@��@��@�v�@�-@���@�x�@��`@���@�@�=q@���@�?}@�9@��@�"�@�~�@�E�@�^@���@��@�\)@�@�z�@� �@��@�^5@�=q@�{@�hs@���@�O�@� �@ڗ�@�O�@�z�@׶F@�l�@�M�@���@ԃ@�+@�5?@ёh@�t�@ύP@�@͉7@�x�@�x�@�x�@�&�@�Z@� �@� �@���@˝�@�v�@�p�@���@�9X@�  @��;@��;@���@��;@��
@Ǿw@��m@� �@�b@ǍP@��@Ɨ�@�~�@Ƈ+@�v�@�5?@��#@�hs@�V@�(�@���@Ý�@��@�5?@���@��@��u@�Z@�(�@�ƨ@�33@�
=@�@�
=@�o@�
=@��@�;d@�  @�(�@�b@�ƨ@���@�5?@���@��@��;@���@��R@��!@���@�~�@�ff@��^@��@���@�1'@��@���@�\)@�o@��@��R@�^5@�7L@�Ĝ@���@�z�@��@��@���@���@���@��@���@���@���@��@�?}@��@�r�@�1@��
@��w@�t�@��H@���@��+@�n�@�@���@��h@�O�@�r�@�ƨ@�o@�ff@��@�J@���@�@��7@�G�@��@��`@��@�Z@�  @��@��P@�K�@��@���@�n�@�=q@��T@���@�x�@��@��/@��9@�j@�9X@��@��F@���@�|�@�S�@�"�@���@�J@���@�X@��@��/@�1'@���@�ƨ@�S�@���@�V@�J@��#@�`B@���@� �@���@�t�@�;d@�@�ȴ@���@��\@�ȴ@���@�V@�-@�{@�{@��@���@�@��^@��-@���@�?}@�Ĝ@���@��@�Z@�Q�@�I�@�A�@�(�@���@��P@�"�@�@���@��\@�E�@�{@��7@��@�r�@�9X@�1@��F@�l�@�dZ@�dZ@�K�@�"�@�o@��y@���@���@�n�@�ff@�V@�{@�@�hs@�7L@���@��`@���@� �@��m@��;@��
@��@�t�@�@���@���@�n�@�J@��#@��7@�?}@��@��@�A�@�b@�b@��@���@�t�@�dZ@�o@�^5@���@�p�@�V@���@�A�@� �@��;@���@�C�@�33@�@���@���@�=q@���@�@���@�X@��@��9@��D@�r�@�A�@�  @� �@�(�@�1@��;@��
@��@�l�@�
=@��y@���@��@��R@�v�@�$�@�@��^@�7L@��/@�A�@�(�@�b@�;@�@�@|�@~�y@~�+@~5?@~@}�@}/@|�@|�@|(�@{�F@{dZ@{C�@{C�@z�\@y�@y��@yhs@xr�@w�@w�;@w|�@v��@vE�@v{@u�-@u?}@tZ@s�m@s��@sC�@r��@q��@q�7@q�@pr�@o�;@o�P@n��@nv�@m@l�@l(�@k��@kdZ@k"�@j�!@j^5@j-@i�#@i�^@ix�@i7L@i%@h�@g�;@g\)@f�y@fV@e@e?}@e�@d�@d9X@c��@cS�@c33@c@b�H@b��@b��@bJ@aX@`�@_�@_��@_�w@_��@_l�@^��@^E�@^5?@^$�@^$�@]�T@]?}@\�/@\�j@\�@\j@\�@["�@Z�!@ZJ@Y�@X�9@XQ�@W��@W;d@V��@Vȴ@Vȴ@V��@VE�@U�T@U��@U@U��@U?}@T��@T�j@Tj@T(�@T�@Sƨ@S�F@S�@S"�@R�@R��@RM�@Q�@Q�^@Qx�@QG�@Q&�@P��@PA�@O
=@N�R@Nff@N5?@M@M`B@L��@L�@L(�@Kƨ@K��@K��@K��@K�F@K�
@K�@KdZ@KS�@KC�@J��@JM�@I�^@I��@I�7@IG�@HbN@G�w@G�@G��@G�P@G|�@Gl�@F�R@F5?@E�@E?}@D�@D�D@DZ@DI�@D1@CC�@C@B��@B~�@BJ@BJ@BJ@BJ@A��@A��@A&�@@��@@A�@@b@?�@?
=@>�+@>V@>{@>{@>@=�@=�@=�T@=@=�-@=�-@=p�@=V@<��@<�D@<z�@<Z@;��@:�@:n�@9�^@97L@8��@81'@7�w@7�@7�P@6��@6ff@5�@5�@6@5�T@5V@4�j@4��@4j@49X@4�@3�m@3�F@3��@3��@3��@3��@3C�@3o@2�H@2��@2�\@2^5@2J@1�^@1hs@1�@0��@0r�@0Q�@01'@0 �@0b@0  @/�;@/��@/|�@.v�@-�@-�@-?}@,�@,�@,�D@,z�@,j@,(�@+S�@*�!@*n�@*=q@)�7@)&�@(��@(�9@(��@(�@(1'@'K�@&ȴ@%�T@%`B@%O�@%/@%�@%V@$��@$�@$�@$�/@$�@$�D@$�@#�
@#�F@#��@#��@#�@#33@"�!@"M�@"=q@"=q@"�@!�@!�#@!��@!x�@!hs@!X@!G�@!7L@!�@ �9@ �@ Q�@   @�w@�w@��@K�@�@V@��@p�@/@V@�D@�@�F@��@��@t�@t�@C�@�H@��@M�@��@�#@��@�^@��@x�@G�@&�@��@�9@1'@  @�w@�P@\)@K�@;d@+@��@ȴ@��@�+@$�@@�T@O�@V@�j@I�@1@��@�m@�
@ƨ@��@��@�@C�@"�@o@o@@��@~�@^5@M�@�@��@�#@��@X@�@��@Ĝ@�9@��@��@��@��@�@�@r�@bN@Q�@Q�@  @|�@\)@�@�R@��@��@��@��@��@��@��@V@�@�@�T@�-@`B@p�@�@�jA��mA��A��A��mA��A��yA��TA��yA��`A��`A��yA��yA��`A��yA��A��A��HA��`A��TA��TA��mA��`A��`A��yA��mA��`A��A��mA��yA��A��yA��A��A��A��A��A��A��yA��yA��A��A��A��A��A��A���A�A��A���A�1A�A�%A�  A���A�A���A���A���A���A��A���A���A���A���A�  A���A���A���A���A��yA��yA��A��mA��A��A��mA��mA��A���A�  A�A���A��A��A��A�JA�oA�{A�oA�JA��A��A�oA��A��A�{A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A�{A�
=A�VA�oA�bA�VA�bA�1A�1A�bA�VA�oA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A��A�{A��A��A�oA�{A��A��A��A�{A�bA�{A�bA�VA�oA�VA�oA��A��A��A��A�{A��A� �A�&�A�&�A�&�A�&�A�$�A� �A� �A�$�A��A��A�"�A�"�A��A� �A�"�A�$�A�(�A�-A�A�A�;dA�E�A�E�A�A�A�C�A�E�A�?}A�?}A�A�A�C�A�A�A�A�A�K�A�I�A�G�A�O�A�VA�S�A�Q�A�VA�XA�VA�VA�\)A�dZA�bNA�bNA�hsA�ffA�hsA�bNA�dZA�dZA�`BA�`BA�ffA�dZA�`BA�bNA�dZA�dZA�`BA�`BA�dZA�ffA�ffA�dZA�l�A�p�A�v�A�t�Aԇ+A�z�A�t�A�v�A�ffA�ffA�dZA�p�A�v�A�hsA�M�A�C�A�1'A�5?A�M�A�33A�(�A�S�A�dZA�dZA�dZA�`BA�ffA�ffA�bNA�`BA�p�A�ffA�bNA�hsA�l�A�l�A�jA�r�A�r�A�p�A�l�A�r�A�v�A�p�A�r�A�v�A�~�A�z�Aԉ7Aԝ�Aԛ�Aԟ�Aԥ�Aԩ�AԬAԧ�A԰!AԴ9AԲ-A԰!AԴ9AԸRAԴ9A԰!AԲ-AԶFAԴ9AԲ-AԲ-AԲ-AԲ-A԰!AԲ-AԴ9AԼjAԶFAԼjA�A�A�A�ĜA���A���A���A��mA��A��A��A��A���A���A���A���A���A���A�A�A�oA��A�"�A�1'A�7LA�Q�A�ffA�v�A�z�AՑhAՇ+A�~�AՇ+AՇ+A�|�A�v�A�bNA�=qA�+A�VA���AԴ9AԋDA�bNA�G�A�=qA�7LA�5?A�{A�1A�A��;A�XA�C�A�33A�+A��A�AҸRAҡ�A҃A�x�A�ZA�=qA�JA��A��TA��/A�n�A�dZA���A�;dA��;AΓuA�XA��A�O�A�  A��A��yA˰!A�~�A�l�A�\)A�XA�\)A�^5A�S�A�K�A�I�A�=qA�;dA�$�A� �A�{A�1A���A��HA���A�Aʩ�Aʉ7A�v�A�\)A�G�A�9XA�/A�+A�{A��A���A�ĜAɡ�AɓuAɗ�Aɇ+A�I�A��A�
=A�ȴAȸRAȰ!AȃA�\)A�M�A�E�A�oA��#Aǣ�A�v�A�ZA�A�A�+A�(�A��A�oA�bA�{A��A��A�G�A�S�A�XA�XA�XA�l�AǋDAǰ!AǺ^A���A�{A�M�Aț�A�ȴA���A��A�S�A�\)A�dZA�p�A�v�A�t�A�n�A�Q�A��Aȩ�AȍPA�VAǕ�A�-A�z�A�E�A�r�A�/A��/AļjAď\A�z�A�r�A�;dAá�A�oA���A��A�=qA×�A�~�A�hsA�;dA��`A�~�A�I�A�
=A�`BA��A�JA��\A���A���A���A��FA���A�K�A��A��/A�ȴA��9A��!A���A���A���A�z�A�l�A�\)A�S�A�Q�A�E�A�&�A�"�A� �A��A�{A�
=A�A�A���A��TA��
A���A���A��A���A���A���A���A���A��7A�~�A�z�A�~�A�v�A�p�A�p�A�bNA�S�A�M�A�C�A�9XA�7LA�1A���A��yA��TA���A���A���A���A��uA�v�A�^5A�C�A�VA��RA���A�VA�(�A�1A��A���A�^5A�9XA�VA��TA�VA��/A�(�A�"�A�"�A��A�VA�  A��yA���A��jA���A���A�hsA�M�A�9XA�$�A��A�A��/A���A��FA��PA�v�A�G�A�9XA�$�A�JA��mA��-A��uA�`BA�=qA�{A��-A�n�A�I�A�1'A�A���A�VA�A��A�n�A�C�A� �A�
=A���A��/A�ȴA���A���A�|�A�l�A�jA�dZA�S�A�K�A�A�A�33A�bA��A��yA�ȴA��jA��A�|�A�hsA�M�A�5?A�"�A�JA�  A���A��mA���A���A���A��+A�\)A��A�ĜA�l�A�G�A�"�A���A���A��!A��PA�n�A�S�A�=qA�/A�&�A�$�A� �A��A��A��A�bA�A��TA��RA��hA�M�A�%A��wA�hsA�  A���A�n�A���A���A��uA�VA�A�A�JA���A��A��mA��#A�ƨA��!A���A�r�A�`BA�M�A�G�A�5?A�/A��A�%A�%A���A��;A��mA��FA���A�~�A�Q�A�1'A�"�A��A�{A�bA���A��#A���A���A���A���A�ȴA���A���A���A�ȴA�ȴA���A���A�ƨA�A�ĜA�A���A���A��A���A���A���A���A���A���A��uA��PA��+A�|�A�v�A�XA� �A��A��TA���A���A�z�A�n�A�`BA�Q�A�E�A�=qA�33A�$�A�oA�
=A�JA�  A��A��TA��wA��+A�dZA�^5A�`BA�ZA�M�A�?}A�9XA�+A�VA���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               A��A��yA��mA��mA��mA��`A��mA��A��A��A��A���A�  A���A���A��A��A���A�{A��A��A��A�oA�VA��A��A��A��A��A�{A�oA�oA��A�$�A� �A�$�A�A�A�A�A�G�A�S�A�\)A�ffA�bNA�bNA�jA�x�A�dZA�?}A�dZA�n�Aԉ7A԰!AԴ9A���A���A�=qA�r�A�z�A�^5A��A���A�bNA�  A�5?A�9XA��
A�(�A��A�K�A���A���A��mA�9XA�ffA���A���A�G�A��A��FA�I�A�S�A�O�A���A��RA���A��A�S�A��A�/A�C�A���A�ZA�Q�A���A�"�A���A�%A���A�ȴA��!A��PA��A�G�A��yA�VA��9A�?}A���A��yA�A��
A�|�A���A��yA���A�r�A���A��^A��A�M�A���A�n�A�r�A���A��A��A��A��+A��-A�O�A��DA�$�A��#A�^5A�VA��hA}��A{�mA{G�AyXAwAv��Av�AtbArAp=qAmx�AlM�Ak�Ak��Ah�uAf��Ad�jAc�#AbM�A^�9A\��AZ�+AZ-AY�AX��AV5?AS��AQ?}AP�DAO%ALr�AK�hAIp�AH=qAE��AC�A?\)A<�jA;��A:��A9O�A8��A8�!A7|�A6�/A5VA3��A2��A2ZA2  A1�#A1��A1`BA/`BA,��A+"�A)�;A(�`A'��A&I�A#�#A#�A"�9A!�TA JA�+A��AVAƨA�jAE�AI�A\)A/A�AK�AȴAJAK�A�FA9XA��A{A��A��A
ffA	
=A1A`BA?}A��A�A��AbNA�#A�AS�A�!AS�A ��A (�@���@�ff@���@��P@��@��T@���@��F@���@��`@��@�o@��@��@�v�@�-@���@�x�@��`@���@�@�=q@���@�?}@�9@��@�"�@�~�@�E�@�^@���@��@�\)@�@�z�@� �@��@�^5@�=q@�{@�hs@���@�O�@� �@ڗ�@�O�@�z�@׶F@�l�@�M�@���@ԃ@�+@�5?@ёh@�t�@ύP@�@͉7@�x�@�x�@�x�@�&�@�Z@� �@� �@���@˝�@�v�@�p�@���@�9X@�  @��;@��;@���@��;@��
@Ǿw@��m@� �@�b@ǍP@��@Ɨ�@�~�@Ƈ+@�v�@�5?@��#@�hs@�V@�(�@���@Ý�@��@�5?@���@��@��u@�Z@�(�@�ƨ@�33@�
=@�@�
=@�o@�
=@��@�;d@�  @�(�@�b@�ƨ@���@�5?@���@��@��;@���@��R@��!@���@�~�@�ff@��^@��@���@�1'@��@���@�\)@�o@��@��R@�^5@�7L@�Ĝ@���@�z�@��@��@���@���@���@��@���@���@���@��@�?}@��@�r�@�1@��
@��w@�t�@��H@���@��+@�n�@�@���@��h@�O�@�r�@�ƨ@�o@�ff@��@�J@���@�@��7@�G�@��@��`@��@�Z@�  @��@��P@�K�@��@���@�n�@�=q@��T@���@�x�@��@��/@��9@�j@�9X@��@��F@���@�|�@�S�@�"�@���@�J@���@�X@��@��/@�1'@���@�ƨ@�S�@���@�V@�J@��#@�`B@���@� �@���@�t�@�;d@�@�ȴ@���@��\@�ȴ@���@�V@�-@�{@�{@��@���@�@��^@��-@���@�?}@�Ĝ@���@��@�Z@�Q�@�I�@�A�@�(�@���@��P@�"�@�@���@��\@�E�@�{@��7@��@�r�@�9X@�1@��F@�l�@�dZ@�dZ@�K�@�"�@�o@��y@���@���@�n�@�ff@�V@�{@�@�hs@�7L@���@��`@���@� �@��m@��;@��
@��@�t�@�@���@���@�n�@�J@��#@��7@�?}@��@��@�A�@�b@�b@��@���@�t�@�dZ@�o@�^5@���@�p�@�V@���@�A�@� �@��;@���@�C�@�33@�@���@���@�=q@���@�@���@�X@��@��9@��D@�r�@�A�@�  @� �@�(�@�1@��;@��
@��@�l�@�
=@��y@���@��@��R@�v�@�$�@�@��^@�7L@��/@�A�@�(�@�b@�;@�@�@|�@~�y@~�+@~5?@~@}�@}/@|�@|�@|(�@{�F@{dZ@{C�@{C�@z�\@y�@y��@yhs@xr�@w�@w�;@w|�@v��@vE�@v{@u�-@u?}@tZ@s�m@s��@sC�@r��@q��@q�7@q�@pr�@o�;@o�P@n��@nv�@m@l�@l(�@k��@kdZ@k"�@j�!@j^5@j-@i�#@i�^@ix�@i7L@i%@h�@g�;@g\)@f�y@fV@e@e?}@e�@d�@d9X@c��@cS�@c33@c@b�H@b��@b��@bJ@aX@`�@_�@_��@_�w@_��@_l�@^��@^E�@^5?@^$�@^$�@]�T@]?}@\�/@\�j@\�@\j@\�@["�@Z�!@ZJ@Y�@X�9@XQ�@W��@W;d@V��@Vȴ@Vȴ@V��@VE�@U�T@U��@U@U��@U?}@T��@T�j@Tj@T(�@T�@Sƨ@S�F@S�@S"�@R�@R��@RM�@Q�@Q�^@Qx�@QG�@Q&�@P��@PA�@O
=@N�R@Nff@N5?@M@M`B@L��@L�@L(�@Kƨ@K��@K��@K��@K�F@K�
@K�@KdZ@KS�@KC�@J��@JM�@I�^@I��@I�7@IG�@HbN@G�w@G�@G��@G�P@G|�@Gl�@F�R@F5?@E�@E?}@D�@D�D@DZ@DI�@D1@CC�@C@B��@B~�@BJ@BJ@BJ@BJ@A��@A��@A&�@@��@@A�@@b@?�@?
=@>�+@>V@>{@>{@>@=�@=�@=�T@=@=�-@=�-@=p�@=V@<��@<�D@<z�@<Z@;��@:�@:n�@9�^@97L@8��@81'@7�w@7�@7�P@6��@6ff@5�@5�@6@5�T@5V@4�j@4��@4j@49X@4�@3�m@3�F@3��@3��@3��@3��@3C�@3o@2�H@2��@2�\@2^5@2J@1�^@1hs@1�@0��@0r�@0Q�@01'@0 �@0b@0  @/�;@/��@/|�@.v�@-�@-�@-?}@,�@,�@,�D@,z�@,j@,(�@+S�@*�!@*n�@*=q@)�7@)&�@(��@(�9@(��@(�@(1'@'K�@&ȴ@%�T@%`B@%O�@%/@%�@%V@$��@$�@$�@$�/@$�@$�D@$�@#�
@#�F@#��@#��@#�@#33@"�!@"M�@"=q@"=q@"�@!�@!�#@!��@!x�@!hs@!X@!G�@!7L@!�@ �9@ �@ Q�@   @�w@�w@��@K�@�@V@��@p�@/@V@�D@�@�F@��@��@t�@t�@C�@�H@��@M�@��@�#@��@�^@��@x�@G�@&�@��@�9@1'@  @�w@�P@\)@K�@;d@+@��@ȴ@��@�+@$�@@�T@O�@V@�j@I�@1@��@�m@�
@ƨ@��@��@�@C�@"�@o@o@@��@~�@^5@M�@�@��@�#@��@X@�@��@Ĝ@�9@��@��@��@��@�@�@r�@bN@Q�@Q�@  @|�@\)@�@�R@��@��@��@��@��@��@��@V@�@�@�T@�-@`B@p�@�G�O�A��mA��A��A��mA��A��yA��TA��yA��`A��`A��yA��yA��`A��yA��A��A��HA��`A��TA��TA��mA��`A��`A��yA��mA��`A��A��mA��yA��A��yA��A��A��A��A��A��A��yA��yA��A��A��A��A��A��A���A�A��A���A�1A�A�%A�  A���A�A���A���A���A���A��A���A���A���A���A�  A���A���A���A���A��yA��yA��A��mA��A��A��mA��mA��A���A�  A�A���A��A��A��A�JA�oA�{A�oA�JA��A��A�oA��A��A�{A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A��A��A�{A�
=A�VA�oA�bA�VA�bA�1A�1A�bA�VA�oA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A��A�{A��A��A�oA�{A��A��A��A�{A�bA�{A�bA�VA�oA�VA�oA��A��A��A��A�{A��A� �A�&�A�&�A�&�A�&�A�$�A� �A� �A�$�A��A��A�"�A�"�A��A� �A�"�A�$�A�(�A�-A�A�A�;dA�E�A�E�A�A�A�C�A�E�A�?}A�?}A�A�A�C�A�A�A�A�A�K�A�I�A�G�A�O�A�VA�S�A�Q�A�VA�XA�VA�VA�\)A�dZA�bNA�bNA�hsA�ffA�hsA�bNA�dZA�dZA�`BA�`BA�ffA�dZA�`BA�bNA�dZA�dZA�`BA�`BA�dZA�ffA�ffA�dZA�l�A�p�A�v�A�t�Aԇ+A�z�A�t�A�v�A�ffA�ffA�dZA�p�A�v�A�hsA�M�A�C�A�1'A�5?A�M�A�33A�(�A�S�A�dZA�dZA�dZA�`BA�ffA�ffA�bNA�`BA�p�A�ffA�bNA�hsA�l�A�l�A�jA�r�A�r�A�p�A�l�A�r�A�v�A�p�A�r�A�v�A�~�A�z�Aԉ7Aԝ�Aԛ�Aԟ�Aԥ�Aԩ�AԬAԧ�A԰!AԴ9AԲ-A԰!AԴ9AԸRAԴ9A԰!AԲ-AԶFAԴ9AԲ-AԲ-AԲ-AԲ-A԰!AԲ-AԴ9AԼjAԶFAԼjA�A�A�A�ĜA���A���A���A��mA��A��A��A��A���A���A���A���A���A���A�A�A�oA��A�"�A�1'A�7LA�Q�A�ffA�v�A�z�AՑhAՇ+A�~�AՇ+AՇ+A�|�A�v�A�bNA�=qA�+A�VA���AԴ9AԋDA�bNA�G�A�=qA�7LA�5?A�{A�1A�A��;A�XA�C�A�33A�+A��A�AҸRAҡ�A҃A�x�A�ZA�=qA�JA��A��TA��/A�n�A�dZA���A�;dA��;AΓuA�XA��A�O�A�  A��A��yA˰!A�~�A�l�A�\)A�XA�\)A�^5A�S�A�K�A�I�A�=qA�;dA�$�A� �A�{A�1A���A��HA���A�Aʩ�Aʉ7A�v�A�\)A�G�A�9XA�/A�+A�{A��A���A�ĜAɡ�AɓuAɗ�Aɇ+A�I�A��A�
=A�ȴAȸRAȰ!AȃA�\)A�M�A�E�A�oA��#Aǣ�A�v�A�ZA�A�A�+A�(�A��A�oA�bA�{A��A��A�G�A�S�A�XA�XA�XA�l�AǋDAǰ!AǺ^A���A�{A�M�Aț�A�ȴA���A��A�S�A�\)A�dZA�p�A�v�A�t�A�n�A�Q�A��Aȩ�AȍPA�VAǕ�A�-A�z�A�E�A�r�A�/A��/AļjAď\A�z�A�r�A�;dAá�A�oA���A��A�=qA×�A�~�A�hsA�;dA��`A�~�A�I�A�
=A�`BA��A�JA��\A���A���A���A��FA���A�K�A��A��/A�ȴA��9A��!A���A���A���A�z�A�l�A�\)A�S�A�Q�A�E�A�&�A�"�A� �A��A�{A�
=A�A�A���A��TA��
A���A���A��A���A���A���A���A���A��7A�~�A�z�A�~�A�v�A�p�A�p�A�bNA�S�A�M�A�C�A�9XA�7LA�1A���A��yA��TA���A���A���A���A��uA�v�A�^5A�C�A�VA��RA���A�VA�(�A�1A��A���A�^5A�9XA�VA��TA�VA��/A�(�A�"�A�"�A��A�VA�  A��yA���A��jA���A���A�hsA�M�A�9XA�$�A��A�A��/A���A��FA��PA�v�A�G�A�9XA�$�A�JA��mA��-A��uA�`BA�=qA�{A��-A�n�A�I�A�1'A�A���A�VA�A��A�n�A�C�A� �A�
=A���A��/A�ȴA���A���A�|�A�l�A�jA�dZA�S�A�K�A�A�A�33A�bA��A��yA�ȴA��jA��A�|�A�hsA�M�A�5?A�"�A�JA�  A���A��mA���A���A���A��+A�\)A��A�ĜA�l�A�G�A�"�A���A���A��!A��PA�n�A�S�A�=qA�/A�&�A�$�A� �A��A��A��A�bA�A��TA��RA��hA�M�A�%A��wA�hsA�  A���A�n�A���A���A��uA�VA�A�A�JA���A��A��mA��#A�ƨA��!A���A�r�A�`BA�M�A�G�A�5?A�/A��A�%A�%A���A��;A��mA��FA���A�~�A�Q�A�1'A�"�A��A�{A�bA���A��#A���A���A���A���A�ȴA���A���A���A�ȴA�ȴA���A���A�ƨA�A�ĜA�A���A���A��A���A���A���A���A���A���A��uA��PA��+A�|�A�v�A�XA� �A��A��TA���A���A�z�A�n�A�`BA�Q�A�E�A�=qA�33A�$�A�oA�
=A�JA�  A��A��TA��wA��+A�dZA�^5A�`BA�ZA�M�A�?}A�9XA�+A�VA���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BVBVBVB�BVB!B!B�BVB!B!B�B�B�BVB�B�B�B!bB!-B!-B!bB �B!bB �B!-B �B �B �B!bB �B �B �B#�B!�B"hB(�B)*B)�B.}B0�B4�B33B33B49B:�B3�B)�B1�B5�B>BBNBOBBW?Bp�B�}B�B�B�BȴBv+BO�BL�BEB<jB>�BoiB�CB*�B.}B�.BVmB$�B�B�BxB�BB�B�B�BI�Bz�B�(B��B�bB��B�_B��B�4B|PB�iBs�Bp;Bk�Bk�Bg8Bc�BcTBa�B_�B^BU�BQ�BK)BE�B8�B,�B&LB1BPB�MB��BخB�B��B��B��B��B��B�B�4Bk�Bi�B`BFB2-B$�B�B�B
��B
�B
�yB
� B
�EB
��B
�:B
��B
�bB
�YB
{�B
s�B
qAB
b�B
X�B
L�B
?�B
3�B
/�B
*�B
$�B
B
	lB
 �B	��B	��B	�]B	��B	˒B	��B	��B	��B	�OB	�B	�=B	��B	�~B	��B	~�B	x�B	k�B	f2B	T,B	JXB	@OB	@OB	=qB	6�B	4nB	5B	.B	/B	$@B	"�B	!-B	�B	xB	7B	�B	+B	�B		�B	�B	MB�cB�VB��B��B�B�B��B�B�B��B�B�B��B	{B	�B		7B	.B	oB	�B	�B	YB	�B	.B	�B	YB	�B	fB	oB	B	�B	�B	�B	
=B		�B	1B	�B	�B	�B	�B	�B	
	B	PB	�B	4B	 B	�B	�B	OB	IB	B	�B	�B	�B	#�B	&LB	'B	'�B	(�B	*�B	-B	1'B	7�B	;�B	A�B	DgB	F�B	H�B	TaB	Z�B	c B	cTB	d�B	e�B	gB	h>B	iB	m]B	p�B	r�B	s�B	s�B	s�B	s�B	tTB	oiB	o�B	o�B	m�B	iyB	ffB	c�B	g8B	i�B	g�B	j�B	sMB	t�B	t�B	u�B	sMB	zDB	w�B	|�B	��B	�{B	��B	��B	�%B	�	B	��B	�$B	��B	�\B	�B	�:B	��B	��B	�FB	��B	�_B	��B	�*B	��B	�B	��B	�<B	�B	�BB	�wB	�B	��B	��B	��B	�3B	�mB	�EB	�?B	ƨB	ŢB	�#B	��B	̘B	��B	��B	֡B	خB	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	��B	�JB	��B	�.B
B
�B
�B
B
%B
_B
	�B

�B
�B
�B
�B
�B
.B
hB
B
uB
B
�B
�B
�B
�B
�B
1B
eB
B
7B
=B
7B
	B
�B
�B
B
�B
 'B
�B
�B
#nB
$�B
&�B
&B
'�B
'�B
($B
'�B
'�B
)�B
)�B
)*B
)_B
,B
-�B
/B
0UB
/�B
0!B
0UB
2-B
2-B
2�B
2�B
33B
3�B
4nB
4�B
5�B
6B
7LB
8�B
8RB
8�B
9�B
;0B
:�B
;0B
<jB
<jB
<�B
=B
=B
=�B
=�B
=�B
=�B
>B
=�B
>�B
?HB
?HB
?�B
@B
@OB
A�B
AUB
AUB
C-B
B�B
C-B
B'B
A�B
AUB
A�B
A�B
B[B
B�B
C-B
C�B
C�B
D�B
E�B
G�B
J�B
K)B
K�B
L0B
L0B
L0B
L0B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
OBB
OB
OB
N�B
N�B
N�B
OB
PB
PHB
PHB
PHB
PB
PB
O�B
O�B
PB
O�B
O�B
PB
P�B
QNB
QNB
QNB
Q�B
Q�B
Q�B
R�B
R�B
S[B
S�B
S�B
S�B
T,B
T�B
U2B
U2B
U�B
U�B
VmB
W�B
XB
XB
W�B
XyB
YKB
ZQB
ZQB
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
[WB
[WB
[�B
\�B
\�B
]/B
]/B
\�B
]/B
^�B
_;B
_B
`BB
`BB
aB
`�B
aHB
aB
`�B
`�B
aB
a�B
a�B
b�B
c�B
c B
c�B
b�B
bNB
b�B
bNB
b�B
d&B
c�B
e�B
f�B
gB
f�B
gmB
g�B
h
B
g�B
h�B
iB
i�B
i�B
jKB
kB
j�B
kQB
jB
jB
iB
iB
iB
iB
iB
i�B
j�B
jB
i�B
iyB
iyB
jKB
jB
jB
jKB
j�B
j�B
kQB
kQB
kQB
k�B
lWB
lWB
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
n/B
o5B
o5B
o5B
oiB
o�B
poB
p�B
qB
q�B
q�B
rB
r�B
r�B
s�B
tB
u�B
t�B
u%B
uZB
u�B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
w2B
w�B
w�B
x8B
x�B
yrB
yrB
y�B
y�B
zDB
z�B
{B
{B
{JB
{B
{B
{�B
|B
|�B
}�B
~]B
~(B
~(B
~(B
~(B
~�B
.B
.B
.B
~�B
cB
�4B
�iB
�iB
�iB
�iB
�iB
�oB
�B
��B
��B
�uB
��B
�{B
�{B
��B
��B
�{B
��B
�MB
��B
��B
��B
��B
�B
�SB
��B
��B
��B
��B
�%B
��B
�%B
��B
��B
��B
�_B
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
�=B
�rB
��B
�B
�xB
��B
��B
�B
�~B
��B
��B
�(B
�\B
��B
�.B
�4B
��B
�oB
�:B
�B
�:B
��B
��B
��B
��B
�uB
�uB
�@B
�{B
�{B
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
�YB
��B
�YB
�YB
�YB
�YB
��B
�+B
��B
�_B
�_B
��B
�eB
��B
��B
��B
��B
�B
�B
�B
�B
�7B
�7B
�B
�kB
��B
�=B
�	B
��B
�	B
��B
�B
�	B
�CB
�~B
�B
��B
�B
�B
�B
�!B
�VB
�!B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�-B
��B
�bB
�bB
�bB
��B
��B
��B
��B
��B
�4B
�4B
�hB
�B
��B
�nB
�B
�:B
�B
�B
�:B
�B
�B
�B
�B
�B
�tB
�B
�@B
��B
�tB
��B
��B
�tB
�tB
�@B
��B
�FB
��B
�zB
��B
��B
��B
��B
�RB
��B
��B
�XB
��B
�*B
��B
�*B
�*B
�_B
�*B
�_B
�*B
�_B
�_B
��B
��B
�eB
�eB
��B
��B
��B
��B
�kB
�B
�qB
�qB
�qB
�qB
�qB
�qB
��B
��B
�qB
��B
��B
�qB
��B
�CB
�CB
��B
�B
�IB
�IB
�IB
��B
�}B
�B
��B
��B
�UB
�!B
�[B
�[B
�-B
�-B
��B
��B
��B
��B
�3B
�hB
��B
��B
��B
�B
�B
�nB
�nB
�nB
�nB
�nB
�B
�tB
�?B
��B
��B
��B
��B
��B
�B
�B
�FB
�FB
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
�$B
�$B
�XB
��B
��B
��B
��B
��B
�*B
�^B
�^B
�^B
��B
��B
��B
�0B
�dB
��B
��B
�B
�B
�6B
�6B
�B
�6B
�jB
�6B
�6B
�jB
�jB
�jB
��B
��B
�qB
��B
��B
��B
��B
��B
�B
�B
�B
��B
��B
�B
�OB
�OB
��B
��B
��B
��B
��B!BIB�B#:B~B�B �B�BVB 'BOB�B �B!B�B�B!�BOBVB�B�B!B�B�B�B�B~B�B!B~B�B 'B�BVB�BVBOB�B�B~B�B�B�B �B�B�BVB!�B!-B �B!bBB"hB�BIB�B \B�B�B�B�BIBOB�B�B!B�B \B�B �B �B�B \B�BVB �B �B�B~B�B!B"�B!B�B�B�B!bB �B!�B�B!bB!bB"�B \B �B"�B �B �B!bB!�B�B!�B%B�B!B$B!bB \B�B"4B�B 'B!�B!bB�B �B$@B �B!bB"�B!-B 'B �B!�B�B!�B!�B�B �B \B \B \B"hB!bB�B!�B �BVB!�B �BVB!bB"�B �B �B�B �B�B!bB!�B �B�B!-B!�B�B �B"hB �B!-B�B �B!�B \BVB �B 'B�B�B!�B �B�B!�B \B%FBB�B%B$@B#nB"�B$B#nB!�B"�B"4B"4B \B �B!�B"hB!-B"4B%�B"�B)�B+kB(�B)�B+B*eB)*B(�B&�B*0B'�B(XB(�B)_B+�B.}B,�B-CB/B0!B/OB-�B1'B0�B1[B.�B4nB2-B5�B5B5?B6zB4nB2�B2�B3�B2�B2�B5B4�B2�B2�B1�B33B2�B2aB33B3�B5B33B;�B<jB=<B=�B;�B7�B8�B/�B1[B.IB-CB9$B49B�B?HB�B�B"hB.B&�B33B0�B33B0�B1[B2�B3�B/�B,�B0�B2�B1�B2aB7LB7LB5B6�B5?B6�B6�B7LB9�B8�B8�B9�B:�B?HBD�BEmBB[BJ�BIRBJ�BM�BO�BM6BL�BOvBOBNpBOvBP�BOvBNpBPBP�BQ�BQ�BM6BP�BP}BN�BM6BS&BR�BQ�BU�BVmBW�BT�B^5B\)B]�Bk�BncBjBj�Bm)Bo5BoiBqvBs�BtTB|�BcB�B��B�@B�qB��B�hB�-B�[B�B��B�BhB�B�B"�B!-B#�B�B�B"4B�B�BOB�BbB�BVBDBJB�BGBMB��B��B�5B��B�B�B��B�/B�TBбB�BBʌB�^B�UB�<B��B�B�nB��B�nB|By�Bw2BpoBkQBe�Bb�BXEBX�BP�BL�BPHBO�BN�BM�BOBBQBNpBN�BK�BQBM6BLdBM�BLdBQBL0BI�BLdBI�BG�BI�BE�BFtBDgBAUBA�BDgBAUB<�B@OB=B;�B>BC�B@�B8�B<6B8RB7�B8B7�B5?B6FB8�BA�B=�BD�BC�BI�BL0BK�BOBX�BoiBuZB{B}�B}�B�B��B��B�4B�_B��B�tB�6B�-BĜB��B�pB�fBAB�B�B0�B2�B6B7�B7�B8B<6BF�B>B;0BC�BOB3�B&B-B�B�B B�B+B �B�BBuB��B�B��BߤB�B=qB@BO�BMjBbBaHBiDBYKBXBGB[#B,qB�B/OB(�B,qB'�B'�B�B �B7B�B�BFBMB�BkBkB�BB�BB�B�B=B!BBxB�BOB#BB�B�B�B�B=B�BB=B�BB�B�B=BBIB�B�BeBBxBkB�B�B�BxB �BOB�B�BqBB�B�BB�B�B�B�B�B�cB�B��B�>B��B�B�B \B_BABuBB_BB�B\BxB�B\B	B�B �BVB"hB$B,qB*eB.B>BB'BN�BU�BZBZB\�BgmBk�Bt�Br�By�B�DB�B.B��B� B��B�{B��B��B�~B�B�bB�bB��B�oB��B�(B�:B�:B�\B�PB��B��B�.B��B��B�B��B�JB��B�bB��B��B�B��B��B�B��B�B��B�_B�fB��B�B��B��B��B�bB�%B�B�B��B�oB�BcB��B�iB�B�B�B~]B{�B}VB{�Bz�By�B|�B|PB}VB~�B� B�GBy�B}"B� B�B|�B�_BxlBx�Bo�Bj�BuZBoiBncBm�Bm)BpoBr�Bv�Bp�Bm)Bn�Bm�Bj�Bl�Bs�BjKBiyBh�Bp;Bh
Bk�BkQBl"BkQBj�BhsBe�Be`Be�Bh>Bm�Bd�Bc BcTBd&Be`Bd&Bb�Bc Bc�BdZBb�Bb�BdZBb�Ba�Ba�Bb�BhsB`vB_pBaHBb�Ba�B_�B^jB^5B^B_�B^�B\�BcTB^5B`vBW�B^jB[�BZ�BUgBVBVBVBT�BS[BTaBT,BS[BOBBQ�BS&BQBS&BOvBP}BK�BHBJXBJ#BIRBIRBGEBG�BIRBC�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202104292026552021042920265520210429202655202104292026552021042920265520210429202655SI  SI  ARFMARFM                                                                                                                                                2021011820320720210118203207IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021012823005320210128230053QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021012823005320210128230053QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021042910194120210429101941IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                