CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-11-22T00:22:13Z creation; 2023-04-26T19:24:32Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20211122002213  20230426192432  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315_008643_142                 7315_008643_142                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @٤x��1@٤x��111  @٤y�@٤y�@1��>�@1��>��dƆ�-��dƆ�-�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@   @@  @�G�@��\@�G�@�  @�p�A  A!G�A*�HA?\)A`  A�  A��A��A��A�Q�A�Q�A�\)A�\)B (�BQ�BQ�BQ�B   B'�
B0(�B8  B@  BG�
BP  BXQ�B`  Bh(�Bp(�Bw�B�
B�  B�  B��B��B�{B�(�B�  B�{B�(�B�  B�  B�  B�{B�(�B��B��B��
B��B��B�  B��B��B�  B�{B�{B��B�  B�  B�  B�{B�(�C 
=C��C��C  C
=C

=C  C{C
=C  C��C��C  C  C
=C
=C��C!��C$
=C%��C'�C*  C,
=C.
=C0
=C2{C4{C6{C8
=C:
=C;��C=�C?��CB  CD  CF
=CH
=CJ  CK��CM��CP
=CR  CS�CU��CW��CZ  C\
=C]��C`  Cb  Cd  Ce��Ch  Cj  Cl  Cn{Cp  Cq��Cs��Cu��Cw��Cy��C|
=C~
=C�C�  C���C���C���C���C�  C�\C�  C���C�C���C���C�  C�  C�C�C���C���C�C�  C���C���C���C���C���C�  C�C�C�
=C�  C���C���C�  C���C���C���C�  C�C�  C���C���C�C���C���C���C���C�C�  C�C�C�  C���C���C���C���C���C�  C�  C�  C�C���C���C�  C�  C���C���C�  C���C�  C�C�  C�C�C�  C�C�\C�
=C�C�\C�C���C��C��C���C�  C�  C�C�  C���C�C�  C�C�C�  C���C���C�C���C���C�C�  C���C�  C�C�
=C�C���C���C��C���C���C�  C�  C�  C�  C�  C���C�  C�{C�
=C�C�C�  C�
=C�C�C�D D �DD�D  D}qD  D}qD��D}qD  D� D�D��D�D}qD  D�D	�D	� D
  D
� D�D��D�D� D  D��D�D��D  D� D  D}qD�D�DD��D�qD}qD  D� D  D� D�qD� D  D}qD�qD� DD��D�D��D�D� D  D� D  D��D  D� D�qDz�D��D � D!D!�D"D"�D#D#��D$�D$��D$�qD%��D&�D&�D'�D'�D(D(��D)  D)}qD*�D*�D+  D+� D,�D,�D-�D-�D.�D.�D/�D/��D0D0z�D0�RD1� D2�D2� D3  D3��D4D4� D5�D5��D6D6��D7  D7z�D7�qD8��D9  D9�D:D:}qD:�qD;}qD;�qD<� D=  D=z�D>  D>}qD?  D?��D@  D@}qDA  DA}qDB  DB��DC�DC��DD  DD� DE�DE� DE�qDF� DG�DG��DH�DH� DI  DI� DI�qDJ� DK  DK� DK�qDL� DL�qDM� DN�DN��DO�DO� DO�qDP� DQ�DQ}qDQ�qDR� DS  DS� DT  DT� DUDU��DU��DV� DW�DW��DX�DX�DY�DY��DZ�DZ� D[  D[}qD[�qD\� D]�D]��D^D^��D_�D_�D`D`�Da�Da}qDa�qDb}qDc�Dc��Dd  Dd� Dd�qDe}qDf�Df�Df�qDgz�Dg�qDh� DiDi��Di�qDj}qDj��DkxRDl  Dl�Dm�DmxRDm�qDn��Dn�RDoz�Dp�Dp� Dp�RDqz�Dq�qDrz�Dr��Ds� Ds�qDt}qDuDu� Du�qDv�Dw�Dw� Dx  Dx��Dy�Dy��Dy�qDz� D{�D{}qD{�qD|� D}�D}}qD}��D~}qD~��DxRD�qD�>�D��HD�D�HD�@ D��HD�D�HD�>�D�|)D�D�HD�>�D�� D��HD�  D�AHD�� D�� D���D�@ D�� D���D�HD�@ D���D��HD�HD�B�D���D���D���D�@ D�~�D��HD�  D�AHD�~�D�� D��qD�@ D���D��qD���D�@ D�~�D�� D���D�AHD��HD�� D��qD�>�D�}qD���D���D�>�D�~�D���D���D�AHD�� D�� D���D�@ D�~�D���D�  D�@ D��HD�� D�HD�AHD�� D���D���D�AHD��HD��HD��qD�@ D�� D���D���D�>�D�� D��qD�HD�>�D��HD��HD�HD�=qD��HD���D���D�@ D�}qD��HD�HD�@ D��HD���D�HD�@ D�~�D��HD�HD�@ D�~�D���D�  D�>�D�� D�� D���D�>�D�� D�� D�  D�@ D���D��HD�  D�>�D�~�D�� D���D�@ D�� D��HD�  D�=qD�~�D��HD�  D�AHD�� D���D��qD�>�D�}qD�� D�  D�>�D�}qD���D���D�@ D�~�D���D�  D�AHD�� D�� D��D�@ D�� D��HD�  D�>�D�� D�D�HD�@ D��HD���D�  D�B�D��HD���D���D�>�D�� D�� D�HD�AHD�~�D���D�HD�B�D�� D��HD�  D�@ D�}qD���D��D�@ D�� D��HD�HD�@ D�~�D��HD�HD�=qD�|)D���D��qD�>�D�~�D���D�  D�>�D��HD�� D���D�>�D�� D�� D�HD�@ D�}qD�� D�  D�>�D��HD��HD�HD�AHD�� D�� D�  D�@ D�� D��)D���D�@ D�� D���D�HD�AHD�� D�� D�HD�AHD�� D���D���D�@ D��HD�� D�  D�@ D�� D���D�  D�@ D�~�D�� D�HD�@ D��HD�� D���D�@ D��HD��HD�HD�>�D�� D�� D�HD�B�D D�� D�  D�@ D�~�D�� D�HD�@ DĀ Dľ�D�  D�AHDŀ Dž�D���D�@ Dƀ DƽqD���D�AHDǁHDǾ�D�  D�@ DȀ D��HD�HD�>�Dɀ D��HD�  D�@ Dʀ D�� D�  D�AHDˀ D�� D�  D�=qD�~�D̾�D���D�@ D́HD�� D���D�@ D΁HD�� D�  D�AHDς�D�� D�  D�AHDЁHD��HD�HD�AHDр DѾ�D�HD�AHD�~�DҾ�D�  D�@ DӁHD�� D���D�AHDԁHD�� D�  D�=qD�~�D�� D�  D�>�D�~�D��HD�HD�>�D׀ D��HD�  D�AHD؀ D��HD�HD�AHD�}qD��HD�HD�=qD�~�DڽqD�  D�@ D�}qD۾�D���D�@ D܁HD��HD�  D�AHD݁HD��HD�  D�>�D�~�D�� D�  D�@ D߀ D�� D�HD�AHD��HDྸD���D�@ D�HD��HD�  D�>�D�~�D�� D�  D�AHD�HD�� D���D�>�D� D侸D�HD�AHD�HD�� D��D�AHD悏D�� D�HD�@ D�HD��HD��qD�@ D� D��HD�HD�>�D�HD�� D�  D�=qD�}qD�� D�HD�B�D� D��HD�  D�@ D� D�� D�HD�AHD�~�D�� D�HD�B�D�~�D�D���D�AHD�HD��HD���D�>�D�� D��HD�  D�=qD�HD��HD�HD�>�D� D�qD�HD�>�D�~�D�D�HD�>�D� D�D�HD�=qD�~�D��HD�HD�=qD�}qD���D�HD�AHD�}qD�� D�HD�<)D�� D���D���D�>�D�~�D���D�  D�=q>u>�G�?u?�33?�G�@
=q@&ff@B�\@\(�@u@�=q@�@��
@�\)@�(�@�=q@�
=@��
@�z�A   AQ�A\)AA��A#33A*=qA1G�A8Q�A>{AE�AL(�AS33AZ=qA`��AfffAl��As�
Az=qA�Q�A��
A��RA�=qA��A�  A��HA�{A���A��
A��RA���A�33A�A�Q�A�=qA�(�A��RA���A�33A���A�
=A���A��HA���A�\)A���AÅA��A�\)Aə�A��
A�AϮA��A��
A�{A�  A��A�(�A�{A�  A�=qA�(�A�ffA�Q�A�\A�z�A�ffA���A�33A��A�ffA���A��HA�p�A��B ��B�B
=B�
B��B�B�HB�
B��B	��B
�HB�B��B��B�\B\)B��B��B�HB�B��BB�HB�B��BB�RB�
B��B�B�HB�
B!�B"{B#
=B$(�B%G�B&=qB'\)B(Q�B)p�B*�\B+33B,z�B-��B.ffB/�B0z�B1B333B3�
B4��B5B6�HB7�
B9G�B:{B;33B<(�B=G�B>ffB?�B@z�BA��BB�\BC�BD��BE�BF�RBH  BH��BJ{BK
=BLQ�BMG�BNffBO\)BP��BQ��BR�HBS�
BU�BV=qBW\)BXQ�BYp�BZ�\B[�B\��B^{B_33B`Q�BaG�Bb=qBc�Bd��BeG�BfffBg�Bhz�BiBj�RBk�
Bl��Bn{Bo33Bp(�Bqp�Br=qBs�Btz�Bu��Bv�RBw�
Bx��Bz{Bz�HB|Q�B}��B~ffB�B�Q�B��RB�\)B��
B�ffB���B��B�  B��\B��B���B�(�B���B�33B�B�=qB���B�G�B��B�ffB��HB�p�B��B�z�B�
=B��B�{B���B�33B���B�(�B���B�G�B��B�Q�B���B�G�B��
B�ffB��HB�p�B��B�z�B���B���B�=qB���B�G�B��B�ffB���B�p�B�  B��\B��B��B�(�B���B�33B��
B�Q�B��HB�\)B��B�ffB���B��B��B�Q�B���B�\)B��B�ffB��HB�\)B��B�ffB���B��B�(�B��RB�33B�B�=qB���B�G�B��
B�Q�B��HB�\)B���B�Q�B��HB�33B�B�=qB���B�G�B��
B�ffB�
=B�p�B�  B��\B�33B���B�ffB��HB�p�B�  B�z�B�
=B��B�{B��\B���B�p�B�(�B���B��B��B�{B��\B��B���B�(�B¸RB�33B�B�z�B�
=BŅB�{Bƣ�B�33BǮB�=qB���B�33BɮB�=qB���B�G�B��
B�ffB��HB�p�B�  BθRB�33B�B�Q�B���B�p�B�  B�z�B�
=BӮB�(�Bԣ�B�
=Bՙ�B�(�B֣�B�33B�B�=qB��HBٙ�B�=qBڸRB�\)B��
B�z�B��BݮB�(�B޸RB�\)B��
B�z�B�
=B�B��B�\B��B�B�=qB���B�G�B�{B�RB�G�B��
B�ffB�
=B陚B�=qB�RB�33B뙚B�=qB�RB�\)B��B�z�B�
=BB�(�B�RB�G�B�  B�z�B�33B��
B�Q�B���B��B��B�ffB�
=B��B�(�B���B�33B�B��\B��B�B�=qB���B�p�B��B�z�B���B�\)C   C G�C �\C �
C{Cp�CC
=CG�C��C��C
=CG�C�\C��C{CQ�C�\C�
C�Cz�C�RC��C=qCz�C�C�HC�C\)C��C��C=qCp�C�RC��C	33C	ffC	��C	�HC
�C
ffC
�RC
��C=qCz�CC�C(�CffC�C�C=qC��C�
C{CG�C�\C�HC=qC�C�C�C(�C�\C�HC(�Cp�C�C�HC�Cp�C��C{C\)C�\C�
C�Cz�C��C{CQ�C�C�
C{Cz�CC
=C33Cz�C�HC(�Cz�C��C�C33C�\C�
C{CQ�C�\C��CG�Cz�CC
=Cz�C��C  C=qC�RC  CG�C�\C��C=qC�\C�RC 
=C ffC C!
=C!G�C!��C"  C"\)C"�C"�
C#G�C#��C#��C$(�C$�\C$�C%(�C%p�C%C&(�C&z�C&C'{C'z�C'�
C(
=C(\)C(C)(�C)�C)C*�C*�\C*��C+33C+�C,  C,ffC,��C,��C-ffC-�RC.  C.p�C.C/  C/\)C/��C033C0p�C0��C1G�C1�\C1�C2ffC2��C3(�C3z�C3�HC4Q�C4��C4��C5\)C5C6{C6ffC6�
C7=qC7z�C7�
C8G�C8�\C8�C9ffC9�RC:  C:Q�C:C;{C;\)C;�
C<33C<p�C<��C=G�C=��C=�C>=qC>�C?
=C?G�C?C@
=C@\)C@��CA33CAp�CACB=qCB�CB�
CCQ�CC�CC��CDp�CD�CE  CEz�CE��CF{CFz�CF�
CG{CGffCGCG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                    ?�=q@   @@  @�G�@��\@�G�@�  @�p�A  A!G�A*�HA?\)A`  A�  A��A��A��A�Q�A�Q�A�\)A�\)B (�BQ�BQ�BQ�B   B'�
B0(�B8  B@  BG�
BP  BXQ�B`  Bh(�Bp(�Bw�B�
B�  B�  B��B��B�{B�(�B�  B�{B�(�B�  B�  B�  B�{B�(�B��B��B��
B��B��B�  B��B��B�  B�{B�{B��B�  B�  B�  B�{B�(�C 
=C��C��C  C
=C

=C  C{C
=C  C��C��C  C  C
=C
=C��C!��C$
=C%��C'�C*  C,
=C.
=C0
=C2{C4{C6{C8
=C:
=C;��C=�C?��CB  CD  CF
=CH
=CJ  CK��CM��CP
=CR  CS�CU��CW��CZ  C\
=C]��C`  Cb  Cd  Ce��Ch  Cj  Cl  Cn{Cp  Cq��Cs��Cu��Cw��Cy��C|
=C~
=C�C�  C���C���C���C���C�  C�\C�  C���C�C���C���C�  C�  C�C�C���C���C�C�  C���C���C���C���C���C�  C�C�C�
=C�  C���C���C�  C���C���C���C�  C�C�  C���C���C�C���C���C���C���C�C�  C�C�C�  C���C���C���C���C���C�  C�  C�  C�C���C���C�  C�  C���C���C�  C���C�  C�C�  C�C�C�  C�C�\C�
=C�C�\C�C���C��C��C���C�  C�  C�C�  C���C�C�  C�C�C�  C���C���C�C���C���C�C�  C���C�  C�C�
=C�C���C���C��C���C���C�  C�  C�  C�  C�  C���C�  C�{C�
=C�C�C�  C�
=C�C�C�D D �DD�D  D}qD  D}qD��D}qD  D� D�D��D�D}qD  D�D	�D	� D
  D
� D�D��D�D� D  D��D�D��D  D� D  D}qD�D�DD��D�qD}qD  D� D  D� D�qD� D  D}qD�qD� DD��D�D��D�D� D  D� D  D��D  D� D�qDz�D��D � D!D!�D"D"�D#D#��D$�D$��D$�qD%��D&�D&�D'�D'�D(D(��D)  D)}qD*�D*�D+  D+� D,�D,�D-�D-�D.�D.�D/�D/��D0D0z�D0�RD1� D2�D2� D3  D3��D4D4� D5�D5��D6D6��D7  D7z�D7�qD8��D9  D9�D:D:}qD:�qD;}qD;�qD<� D=  D=z�D>  D>}qD?  D?��D@  D@}qDA  DA}qDB  DB��DC�DC��DD  DD� DE�DE� DE�qDF� DG�DG��DH�DH� DI  DI� DI�qDJ� DK  DK� DK�qDL� DL�qDM� DN�DN��DO�DO� DO�qDP� DQ�DQ}qDQ�qDR� DS  DS� DT  DT� DUDU��DU��DV� DW�DW��DX�DX�DY�DY��DZ�DZ� D[  D[}qD[�qD\� D]�D]��D^D^��D_�D_�D`D`�Da�Da}qDa�qDb}qDc�Dc��Dd  Dd� Dd�qDe}qDf�Df�Df�qDgz�Dg�qDh� DiDi��Di�qDj}qDj��DkxRDl  Dl�Dm�DmxRDm�qDn��Dn�RDoz�Dp�Dp� Dp�RDqz�Dq�qDrz�Dr��Ds� Ds�qDt}qDuDu� Du�qDv�Dw�Dw� Dx  Dx��Dy�Dy��Dy�qDz� D{�D{}qD{�qD|� D}�D}}qD}��D~}qD~��DxRD�qD�>�D��HD�D�HD�@ D��HD�D�HD�>�D�|)D�D�HD�>�D�� D��HD�  D�AHD�� D�� D���D�@ D�� D���D�HD�@ D���D��HD�HD�B�D���D���D���D�@ D�~�D��HD�  D�AHD�~�D�� D��qD�@ D���D��qD���D�@ D�~�D�� D���D�AHD��HD�� D��qD�>�D�}qD���D���D�>�D�~�D���D���D�AHD�� D�� D���D�@ D�~�D���D�  D�@ D��HD�� D�HD�AHD�� D���D���D�AHD��HD��HD��qD�@ D�� D���D���D�>�D�� D��qD�HD�>�D��HD��HD�HD�=qD��HD���D���D�@ D�}qD��HD�HD�@ D��HD���D�HD�@ D�~�D��HD�HD�@ D�~�D���D�  D�>�D�� D�� D���D�>�D�� D�� D�  D�@ D���D��HD�  D�>�D�~�D�� D���D�@ D�� D��HD�  D�=qD�~�D��HD�  D�AHD�� D���D��qD�>�D�}qD�� D�  D�>�D�}qD���D���D�@ D�~�D���D�  D�AHD�� D�� D��D�@ D�� D��HD�  D�>�D�� D�D�HD�@ D��HD���D�  D�B�D��HD���D���D�>�D�� D�� D�HD�AHD�~�D���D�HD�B�D�� D��HD�  D�@ D�}qD���D��D�@ D�� D��HD�HD�@ D�~�D��HD�HD�=qD�|)D���D��qD�>�D�~�D���D�  D�>�D��HD�� D���D�>�D�� D�� D�HD�@ D�}qD�� D�  D�>�D��HD��HD�HD�AHD�� D�� D�  D�@ D�� D��)D���D�@ D�� D���D�HD�AHD�� D�� D�HD�AHD�� D���D���D�@ D��HD�� D�  D�@ D�� D���D�  D�@ D�~�D�� D�HD�@ D��HD�� D���D�@ D��HD��HD�HD�>�D�� D�� D�HD�B�D D�� D�  D�@ D�~�D�� D�HD�@ DĀ Dľ�D�  D�AHDŀ Dž�D���D�@ Dƀ DƽqD���D�AHDǁHDǾ�D�  D�@ DȀ D��HD�HD�>�Dɀ D��HD�  D�@ Dʀ D�� D�  D�AHDˀ D�� D�  D�=qD�~�D̾�D���D�@ D́HD�� D���D�@ D΁HD�� D�  D�AHDς�D�� D�  D�AHDЁHD��HD�HD�AHDр DѾ�D�HD�AHD�~�DҾ�D�  D�@ DӁHD�� D���D�AHDԁHD�� D�  D�=qD�~�D�� D�  D�>�D�~�D��HD�HD�>�D׀ D��HD�  D�AHD؀ D��HD�HD�AHD�}qD��HD�HD�=qD�~�DڽqD�  D�@ D�}qD۾�D���D�@ D܁HD��HD�  D�AHD݁HD��HD�  D�>�D�~�D�� D�  D�@ D߀ D�� D�HD�AHD��HDྸD���D�@ D�HD��HD�  D�>�D�~�D�� D�  D�AHD�HD�� D���D�>�D� D侸D�HD�AHD�HD�� D��D�AHD悏D�� D�HD�@ D�HD��HD��qD�@ D� D��HD�HD�>�D�HD�� D�  D�=qD�}qD�� D�HD�B�D� D��HD�  D�@ D� D�� D�HD�AHD�~�D�� D�HD�B�D�~�D�D���D�AHD�HD��HD���D�>�D�� D��HD�  D�=qD�HD��HD�HD�>�D� D�qD�HD�>�D�~�D�D�HD�>�D� D�D�HD�=qD�~�D��HD�HD�=qD�}qD���D�HD�AHD�}qD�� D�HD�<)D�� D���D���D�>�D�~�D���D�  G�O�>u>�G�?u?�33?�G�@
=q@&ff@B�\@\(�@u@�=q@�@��
@�\)@�(�@�=q@�
=@��
@�z�A   AQ�A\)AA��A#33A*=qA1G�A8Q�A>{AE�AL(�AS33AZ=qA`��AfffAl��As�
Az=qA�Q�A��
A��RA�=qA��A�  A��HA�{A���A��
A��RA���A�33A�A�Q�A�=qA�(�A��RA���A�33A���A�
=A���A��HA���A�\)A���AÅA��A�\)Aə�A��
A�AϮA��A��
A�{A�  A��A�(�A�{A�  A�=qA�(�A�ffA�Q�A�\A�z�A�ffA���A�33A��A�ffA���A��HA�p�A��B ��B�B
=B�
B��B�B�HB�
B��B	��B
�HB�B��B��B�\B\)B��B��B�HB�B��BB�HB�B��BB�RB�
B��B�B�HB�
B!�B"{B#
=B$(�B%G�B&=qB'\)B(Q�B)p�B*�\B+33B,z�B-��B.ffB/�B0z�B1B333B3�
B4��B5B6�HB7�
B9G�B:{B;33B<(�B=G�B>ffB?�B@z�BA��BB�\BC�BD��BE�BF�RBH  BH��BJ{BK
=BLQ�BMG�BNffBO\)BP��BQ��BR�HBS�
BU�BV=qBW\)BXQ�BYp�BZ�\B[�B\��B^{B_33B`Q�BaG�Bb=qBc�Bd��BeG�BfffBg�Bhz�BiBj�RBk�
Bl��Bn{Bo33Bp(�Bqp�Br=qBs�Btz�Bu��Bv�RBw�
Bx��Bz{Bz�HB|Q�B}��B~ffB�B�Q�B��RB�\)B��
B�ffB���B��B�  B��\B��B���B�(�B���B�33B�B�=qB���B�G�B��B�ffB��HB�p�B��B�z�B�
=B��B�{B���B�33B���B�(�B���B�G�B��B�Q�B���B�G�B��
B�ffB��HB�p�B��B�z�B���B���B�=qB���B�G�B��B�ffB���B�p�B�  B��\B��B��B�(�B���B�33B��
B�Q�B��HB�\)B��B�ffB���B��B��B�Q�B���B�\)B��B�ffB��HB�\)B��B�ffB���B��B�(�B��RB�33B�B�=qB���B�G�B��
B�Q�B��HB�\)B���B�Q�B��HB�33B�B�=qB���B�G�B��
B�ffB�
=B�p�B�  B��\B�33B���B�ffB��HB�p�B�  B�z�B�
=B��B�{B��\B���B�p�B�(�B���B��B��B�{B��\B��B���B�(�B¸RB�33B�B�z�B�
=BŅB�{Bƣ�B�33BǮB�=qB���B�33BɮB�=qB���B�G�B��
B�ffB��HB�p�B�  BθRB�33B�B�Q�B���B�p�B�  B�z�B�
=BӮB�(�Bԣ�B�
=Bՙ�B�(�B֣�B�33B�B�=qB��HBٙ�B�=qBڸRB�\)B��
B�z�B��BݮB�(�B޸RB�\)B��
B�z�B�
=B�B��B�\B��B�B�=qB���B�G�B�{B�RB�G�B��
B�ffB�
=B陚B�=qB�RB�33B뙚B�=qB�RB�\)B��B�z�B�
=BB�(�B�RB�G�B�  B�z�B�33B��
B�Q�B���B��B��B�ffB�
=B��B�(�B���B�33B�B��\B��B�B�=qB���B�p�B��B�z�B���B�\)C   C G�C �\C �
C{Cp�CC
=CG�C��C��C
=CG�C�\C��C{CQ�C�\C�
C�Cz�C�RC��C=qCz�C�C�HC�C\)C��C��C=qCp�C�RC��C	33C	ffC	��C	�HC
�C
ffC
�RC
��C=qCz�CC�C(�CffC�C�C=qC��C�
C{CG�C�\C�HC=qC�C�C�C(�C�\C�HC(�Cp�C�C�HC�Cp�C��C{C\)C�\C�
C�Cz�C��C{CQ�C�C�
C{Cz�CC
=C33Cz�C�HC(�Cz�C��C�C33C�\C�
C{CQ�C�\C��CG�Cz�CC
=Cz�C��C  C=qC�RC  CG�C�\C��C=qC�\C�RC 
=C ffC C!
=C!G�C!��C"  C"\)C"�C"�
C#G�C#��C#��C$(�C$�\C$�C%(�C%p�C%C&(�C&z�C&C'{C'z�C'�
C(
=C(\)C(C)(�C)�C)C*�C*�\C*��C+33C+�C,  C,ffC,��C,��C-ffC-�RC.  C.p�C.C/  C/\)C/��C033C0p�C0��C1G�C1�\C1�C2ffC2��C3(�C3z�C3�HC4Q�C4��C4��C5\)C5C6{C6ffC6�
C7=qC7z�C7�
C8G�C8�\C8�C9ffC9�RC:  C:Q�C:C;{C;\)C;�
C<33C<p�C<��C=G�C=��C=�C>=qC>�C?
=C?G�C?C@
=C@\)C@��CA33CAp�CACB=qCB�CB�
CCQ�CC�CC��CDp�CD�CE  CEz�CE��CF{CFz�CF�
CG{CGffCGCG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                    @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�&�A�oA�oA�oA�oA��A�bA�oA�JA�VA�VA�VA�JA�
=A�
=A�
=A�
=A�1A�
=A�VA�JA�
=A�JA�JA�JA�JA�bA�bA�oA�oA�oA�oA�oA�oA�oA�oA�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�"�A�"�A�$�A�"�A� �A��A�{A�bA�bA�A��A��TA���A֬A�/A�/Aӕ�A���A�  A���A�p�A�  A�t�A���A��AÁA�x�A�p�A�5?A��RA�ZA��A�^5A���A��!A��mA��A��A�/A�Q�A��+A�G�A�ƨA�  A�7LA�A�v�A�bA��-A���A�
=A�1A��HA�|�A���A�VA��yA��wA�ĜA���A�jA��A���A���A��;A��-A�|�A���A�JA�bNA�^5A��A�I�A��A}�AzbAux�AlI�A`��A^bNA\��AY33AW�AUAS��AS�AP9XAMK�AK��AIl�AG\)AF{AE�ADĜADQ�AC33AA��A?+A=�hA;��A9�TA7�TA4��A3A3t�A1�A0��A/t�A.z�A-�TA-t�A,$�A+l�A+7LA*�/A*5?A(^5A'A'S�A&M�A%��A$��A#t�A#%A"bA�wA�AVA�;A+A��AĜA�\AQ�A�7A�HAr�AbNA�AffA��A��A�jA1'A  A�PAv�A|�AffAXA��A�+Ar�AM�A�A|�AA��AdZA�AJA��A��A�A`BAO�A/AA�A��AO�A
��A
��A
{A	�FA	p�A	;dA	VA�yAȴA�9AQ�AA��AZAJAƨAJA��A�A;dAffAhsA Q�@��;@��@��H@���@��@���@�l�@�+@��@�
=@���@���@�?}@�Z@�M�@�p�@�/@��@�Z@���@�@�M�@�x�@��@�Ĝ@�(�@�l�@ꟾ@�x�@�j@�F@�S�@�33@���@�ȴ@��T@��@�r�@�I�@�w@�@�h@�%@�j@�z�@�K�@�E�@���@��m@ف@�  @ם�@�33@��@�@��y@�ff@�J@��T@���@�E�@��H@���@�  @�=q@Л�@��@ͺ^@͉7@�/@�z�@�Q�@�(�@�1@˥�@ʰ!@���@���@�A�@ǶF@ǝ�@�l�@�S�@�ȴ@�X@�j@��@�+@�~�@���@��/@�bN@�Q�@�Q�@�b@�"�@�J@�X@��@���@�1@�K�@�v�@�M�@��@�O�@�/@�/@�/@��@��/@��u@�A�@�b@��w@��P@�dZ@��@���@�=q@�hs@�r�@�  @���@�C�@���@��!@�^5@�=q@��@���@��9@�r�@�1'@�  @�t�@�@��!@��\@�$�@��@�X@�V@���@�j@�9X@��@��@���@���@��\@�v�@�V@��7@��@�j@�A�@���@���@�|�@�dZ@�S�@�;d@��R@�ff@�{@�@�J@��@��#@�hs@�G�@���@��@��@�r�@�A�@�  @���@���@�|�@�l�@�K�@�o@���@�ff@��#@�?}@���@�z�@�A�@�1'@� �@��m@��P@�dZ@�C�@�o@��@��!@�M�@���@��h@�O�@�?}@���@�b@��@�;d@�
=@���@��\@�=q@��#@���@���@��@�O�@�%@��u@�  @��F@�dZ@��H@�5?@��@���@�X@��/@�z�@�I�@�1@�t�@�S�@�"�@��@���@�5?@���@���@���@���@�G�@��u@�A�@�1'@�(�@�b@�  @���@��;@��w@�|�@�33@�o@�@��R@�E�@�@��@�X@�%@�%@��`@��@�I�@�b@��@�K�@��@��@�ff@���@�G�@���@��/@���@���@�Q�@�1@��P@�t�@�;d@�o@��@�~�@�E�@�5?@�-@��@��-@��@�?}@�%@��j@��D@�r�@�(�@��m@��w@���@�\)@�o@���@���@���@��+@�v�@���@��^@���@��7@�`B@�%@��D@�I�@� �@�b@�  @��@�P@~��@~v�@~$�@}�-@}`B@}V@{ƨ@{33@z��@z�\@z^5@y�@yx�@x��@x1'@w�@w|�@w�@v�R@v5?@u�-@t�/@t��@t�@st�@sC�@so@r��@r=q@q�7@o�w@o
=@o
=@n��@n�@nȴ@n�y@n��@nV@m��@mp�@m?}@l�D@l9X@l�@k��@k@j-@j�@j-@i��@i��@i�#@i&�@h�`@h��@h��@h�u@gl�@f�@fE�@e�-@e�@e/@dz�@c�F@cS�@co@b~�@a��@a&�@a&�@a%@`��@`��@`��@`��@`r�@`1'@_�;@_�@_�@_�P@_l�@_+@_�@_
=@^�@^��@^$�@]V@[��@[ƨ@[�@[33@Z�@Z�!@Z�@Y7L@X�9@XbN@XQ�@X1'@W�@W��@W�@W|�@V�R@V�+@VE�@U�-@T��@T�D@TI�@S�m@S��@S�@SS�@S@R�!@RM�@Q�#@P�u@O��@O\)@OK�@N��@MO�@Lj@L1@Kƨ@K��@KdZ@KC�@K"�@J�H@J��@J�!@J��@J~�@J^5@I��@I��@IG�@I&�@H�`@H��@HbN@H  @G�@G
=@F$�@E��@D��@DI�@D1@C�F@CC�@B�\@B^5@B=q@A�@A��@Ax�@AG�@@�`@@Q�@@1'@?�@?|�@?;d@>��@=�T@=��@=�h@=�h@=�@=p�@=O�@=�@<�/@<��@<9X@;�m@;�F@:�@:=q@9X@9&�@9%@8�9@8b@7��@65?@5��@5`B@5?}@5/@5�@4�j@49X@3��@3��@2�@2��@2M�@1�@1��@1��@1��@1��@1�7@1x�@1x�@0�9@0�u@0�@0�@0r�@0r�@0Q�@0  @/��@/��@/|�@/l�@/+@.�@.��@.ff@-@-`B@,�/@,Z@+�m@+dZ@+"�@+o@+@*��@*��@*~�@*^5@*M�@*=q@*-@*J@)�7@)�7@)x�@)�@(�`@(Ĝ@(r�@(b@'��@'�@';d@'
=@&�y@&��@&v�@&v�@&ff@&V@&V@&V@&5?@%V@$9X@$1@#��@#t�@#C�@#"�@#o@#@"�@"�@#@"�@"�H@"�H@"��@"^5@"�@!�@!��@!�7@!hs@!&�@ ��@ �9@ �@ 1'@ b@�@�w@|�@K�@
=@�@�R@��@�+@v�@ff@ff@ff@ff@v�@�+@�+@V@5?@$�@{@��@`B@V@��@�@�@z�@9X@(�@1@1@�m@�m@�m@�m@�
@�F@��@�@dZ@C�@o@�H@�\@M�@-@J@�#@x�@G�@7L@�@�`@�9@r�@r�@A�@ �@  @  @�;@�w@��@l�@K�@;d@+@�@
=@�@ff@�@�@�T@��@��@p�@O�@/@�@V@�/@�/@�j@z�@(�@�
@�F@�F@��@�F@��@dZ@C�@33@�H@��@�!@��@~�@^5@M�@=q@-@�@hs@��@��@�`@��@Ĝ@�@r�@r�@bN@��@��@�R@�+@ff@V@E�@@�@�T@@�h@`B@`B@`B@`B@?}@/@/@�@VA�(�A�+A�"�A�oA��A�-A�{A�bA�VA�bA�bA�bA�oA�{A�{A��A��A��A�oA�oA�oA�{A�VA�
=A�JA�JA�JA�VA�
=A�JA�
=A�JA�
=A�JA�JA�VA�JA�JA�VA�JA�JA�JA�JA�JA�JA�JA�JA�
=A�
=A�1A�
=A�JA�JA�JA�JA�JA�JA�
=A�JA�JA�JA�VA�VA�JA�VA�VA�VA�VA�JA�
=A�1A�1A�1A�
=A�1A�
=A�
=A�
=A�
=A�JA�bA�VA�bA�oA�oA�VA�VA�VA�JA�
=A�VA�bA�JA�
=A�
=A�1A�
=A�
=A�1A�
=A�1A�1A�1A�
=A�1A�1A�1A�
=A�
=A�
=A�
=A�JA�
=A�1A�
=A�
=A�1A�1A�1A�
=A�1A�
=A�
=A�1A�1A�
=A�
=A�
=A�
=A�
=A�
=A�JA�JA�JA�JA�JA�JA�VA�bA�bA�bA�bA�oA�oA�VA�bA�oA�oA�bA�oA�bA�oA�{A�{A�oA�{A�{A�{A�{A�{A�{A��A�{A�{A��A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�oA�{A�oA�bA�bA�bA�bA�oA�oA�oA�oA�oA�oA�{A�oA�oA�{A�oA�bA�oA�oA�bA�bA�bA�oA�oA�oA�{A�{A��A��A�{A�{A�{A�{A�{A��A��A�{A�{A�{A�{A�{A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A�"�A� �A� �A�"�A� �A� �A� �A� �A��A� �A� �A� �A� �A� �A��A��A��A��A��A��A�"�A�"�A�"�A�"�A�$�A�$�A�$�A�"�A�"�A�$�A�$�A�"�A�$�A�$�A�"�A�"�A�"�A�"�A� �A�"�A� �A�"�A�"�A�$�A�$�A�$�A�&�A�&�A�&�A�&�A�$�A�$�A�"�A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�oA�{A�oA�bA�VA�VA�JA�VA�oA�oA�bA�oA�oA�bA�bA�
=A�JA�A�A�A�  A���A���A��A��A��A��A��A��A��A��A��`A��;A��/A��#A��/A��
A��
A��
A���A�ȴA�ȴA�ĜA�ĜA־wAָRAִ9A֥�A֣�A֥�A֡�A։7AցA�n�A�/A��A�{A��A�oA���A���AոRAՃA�7LA��A��AԸRAԑhA�dZA�=qA��A��#A�VA�&�AҾwA�S�A��A�I�A�-AϏ\A�Aκ^A��mA͏\AͅA�~�A�-A̸RA̋DA�\)A��yA�x�A��A��TA���Aʡ�A�p�A�{AɮAɡ�Aɗ�A�x�A�1'A��;Aȉ7A�G�A�1'A�A��;AǸRAǧ�Aǝ�AǑhAǅA�~�A�z�A�r�A�Q�A�7LA�"�A�%A��A��A��A��A��/Aơ�AƉ7A�^5A�(�Aź^A��HA�p�A�JAô9AÙ�A�v�A�JA���A¬A�A�A�z�A�XA�&�A���A��`A���A�Q�A�-A��A�VA���A��A��DA��A���A�5?A�
=A��;A���A���A��PA�v�A�K�A�bA���A��wA���A��A�l�A�1'A��DA�n�A���A��A��;A�jA�K�A�7LA�"�A��A��-A��A���A���A���A��uA�XA�ƨA�v�A�Q�A�+A��jA�jA�%A���A�33A���A��^A���A���A���A���A���A��\A��A�~�A�t�A�jA�bNA�G�A�1'A���A���A��9A���A�~�A�$�A��^A�dZA�oA��FA�K�A�A���A���A�jA�E�A��A���A��mA��/A���A�ƨA���A���A���A��7A�33A�%A�z�A�
=A��/A���A�I�A���A�x�A�n�A�7LA�+A��A��`A��A�=qA�
=A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                    A��A�&�A�oA�oA�oA�oA��A�bA�oA�JA�VA�VA�VA�JA�
=A�
=A�
=A�
=A�1A�
=A�VA�JA�
=A�JA�JA�JA�JA�bA�bA�oA�oA�oA�oA�oA�oA�oA�oA�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�"�A�"�A�$�A�"�A� �A��A�{A�bA�bA�A��A��TA���A֬A�/A�/Aӕ�A���A�  A���A�p�A�  A�t�A���A��AÁA�x�A�p�A�5?A��RA�ZA��A�^5A���A��!A��mA��A��A�/A�Q�A��+A�G�A�ƨA�  A�7LA�A�v�A�bA��-A���A�
=A�1A��HA�|�A���A�VA��yA��wA�ĜA���A�jA��A���A���A��;A��-A�|�A���A�JA�bNA�^5A��A�I�A��A}�AzbAux�AlI�A`��A^bNA\��AY33AW�AUAS��AS�AP9XAMK�AK��AIl�AG\)AF{AE�ADĜADQ�AC33AA��A?+A=�hA;��A9�TA7�TA4��A3A3t�A1�A0��A/t�A.z�A-�TA-t�A,$�A+l�A+7LA*�/A*5?A(^5A'A'S�A&M�A%��A$��A#t�A#%A"bA�wA�AVA�;A+A��AĜA�\AQ�A�7A�HAr�AbNA�AffA��A��A�jA1'A  A�PAv�A|�AffAXA��A�+Ar�AM�A�A|�AA��AdZA�AJA��A��A�A`BAO�A/AA�A��AO�A
��A
��A
{A	�FA	p�A	;dA	VA�yAȴA�9AQ�AA��AZAJAƨAJA��A�A;dAffAhsA Q�@��;@��@��H@���@��@���@�l�@�+@��@�
=@���@���@�?}@�Z@�M�@�p�@�/@��@�Z@���@�@�M�@�x�@��@�Ĝ@�(�@�l�@ꟾ@�x�@�j@�F@�S�@�33@���@�ȴ@��T@��@�r�@�I�@�w@�@�h@�%@�j@�z�@�K�@�E�@���@��m@ف@�  @ם�@�33@��@�@��y@�ff@�J@��T@���@�E�@��H@���@�  @�=q@Л�@��@ͺ^@͉7@�/@�z�@�Q�@�(�@�1@˥�@ʰ!@���@���@�A�@ǶF@ǝ�@�l�@�S�@�ȴ@�X@�j@��@�+@�~�@���@��/@�bN@�Q�@�Q�@�b@�"�@�J@�X@��@���@�1@�K�@�v�@�M�@��@�O�@�/@�/@�/@��@��/@��u@�A�@�b@��w@��P@�dZ@��@���@�=q@�hs@�r�@�  @���@�C�@���@��!@�^5@�=q@��@���@��9@�r�@�1'@�  @�t�@�@��!@��\@�$�@��@�X@�V@���@�j@�9X@��@��@���@���@��\@�v�@�V@��7@��@�j@�A�@���@���@�|�@�dZ@�S�@�;d@��R@�ff@�{@�@�J@��@��#@�hs@�G�@���@��@��@�r�@�A�@�  @���@���@�|�@�l�@�K�@�o@���@�ff@��#@�?}@���@�z�@�A�@�1'@� �@��m@��P@�dZ@�C�@�o@��@��!@�M�@���@��h@�O�@�?}@���@�b@��@�;d@�
=@���@��\@�=q@��#@���@���@��@�O�@�%@��u@�  @��F@�dZ@��H@�5?@��@���@�X@��/@�z�@�I�@�1@�t�@�S�@�"�@��@���@�5?@���@���@���@���@�G�@��u@�A�@�1'@�(�@�b@�  @���@��;@��w@�|�@�33@�o@�@��R@�E�@�@��@�X@�%@�%@��`@��@�I�@�b@��@�K�@��@��@�ff@���@�G�@���@��/@���@���@�Q�@�1@��P@�t�@�;d@�o@��@�~�@�E�@�5?@�-@��@��-@��@�?}@�%@��j@��D@�r�@�(�@��m@��w@���@�\)@�o@���@���@���@��+@�v�@���@��^@���@��7@�`B@�%@��D@�I�@� �@�b@�  @��@�P@~��@~v�@~$�@}�-@}`B@}V@{ƨ@{33@z��@z�\@z^5@y�@yx�@x��@x1'@w�@w|�@w�@v�R@v5?@u�-@t�/@t��@t�@st�@sC�@so@r��@r=q@q�7@o�w@o
=@o
=@n��@n�@nȴ@n�y@n��@nV@m��@mp�@m?}@l�D@l9X@l�@k��@k@j-@j�@j-@i��@i��@i�#@i&�@h�`@h��@h��@h�u@gl�@f�@fE�@e�-@e�@e/@dz�@c�F@cS�@co@b~�@a��@a&�@a&�@a%@`��@`��@`��@`��@`r�@`1'@_�;@_�@_�@_�P@_l�@_+@_�@_
=@^�@^��@^$�@]V@[��@[ƨ@[�@[33@Z�@Z�!@Z�@Y7L@X�9@XbN@XQ�@X1'@W�@W��@W�@W|�@V�R@V�+@VE�@U�-@T��@T�D@TI�@S�m@S��@S�@SS�@S@R�!@RM�@Q�#@P�u@O��@O\)@OK�@N��@MO�@Lj@L1@Kƨ@K��@KdZ@KC�@K"�@J�H@J��@J�!@J��@J~�@J^5@I��@I��@IG�@I&�@H�`@H��@HbN@H  @G�@G
=@F$�@E��@D��@DI�@D1@C�F@CC�@B�\@B^5@B=q@A�@A��@Ax�@AG�@@�`@@Q�@@1'@?�@?|�@?;d@>��@=�T@=��@=�h@=�h@=�@=p�@=O�@=�@<�/@<��@<9X@;�m@;�F@:�@:=q@9X@9&�@9%@8�9@8b@7��@65?@5��@5`B@5?}@5/@5�@4�j@49X@3��@3��@2�@2��@2M�@1�@1��@1��@1��@1��@1�7@1x�@1x�@0�9@0�u@0�@0�@0r�@0r�@0Q�@0  @/��@/��@/|�@/l�@/+@.�@.��@.ff@-@-`B@,�/@,Z@+�m@+dZ@+"�@+o@+@*��@*��@*~�@*^5@*M�@*=q@*-@*J@)�7@)�7@)x�@)�@(�`@(Ĝ@(r�@(b@'��@'�@';d@'
=@&�y@&��@&v�@&v�@&ff@&V@&V@&V@&5?@%V@$9X@$1@#��@#t�@#C�@#"�@#o@#@"�@"�@#@"�@"�H@"�H@"��@"^5@"�@!�@!��@!�7@!hs@!&�@ ��@ �9@ �@ 1'@ b@�@�w@|�@K�@
=@�@�R@��@�+@v�@ff@ff@ff@ff@v�@�+@�+@V@5?@$�@{@��@`B@V@��@�@�@z�@9X@(�@1@1@�m@�m@�m@�m@�
@�F@��@�@dZ@C�@o@�H@�\@M�@-@J@�#@x�@G�@7L@�@�`@�9@r�@r�@A�@ �@  @  @�;@�w@��@l�@K�@;d@+@�@
=@�@ff@�@�@�T@��@��@p�@O�@/@�@V@�/@�/@�j@z�@(�@�
@�F@�F@��@�F@��@dZ@C�@33@�H@��@�!@��@~�@^5@M�@=q@-@�@hs@��@��@�`@��@Ĝ@�@r�@r�@bN@��@��@�R@�+@ff@V@E�@@�@�T@@�h@`B@`B@`B@`B@?}@/@/@�G�O�A�(�A�+A�"�A�oA��A�-A�{A�bA�VA�bA�bA�bA�oA�{A�{A��A��A��A�oA�oA�oA�{A�VA�
=A�JA�JA�JA�VA�
=A�JA�
=A�JA�
=A�JA�JA�VA�JA�JA�VA�JA�JA�JA�JA�JA�JA�JA�JA�
=A�
=A�1A�
=A�JA�JA�JA�JA�JA�JA�
=A�JA�JA�JA�VA�VA�JA�VA�VA�VA�VA�JA�
=A�1A�1A�1A�
=A�1A�
=A�
=A�
=A�
=A�JA�bA�VA�bA�oA�oA�VA�VA�VA�JA�
=A�VA�bA�JA�
=A�
=A�1A�
=A�
=A�1A�
=A�1A�1A�1A�
=A�1A�1A�1A�
=A�
=A�
=A�
=A�JA�
=A�1A�
=A�
=A�1A�1A�1A�
=A�1A�
=A�
=A�1A�1A�
=A�
=A�
=A�
=A�
=A�
=A�JA�JA�JA�JA�JA�JA�VA�bA�bA�bA�bA�oA�oA�VA�bA�oA�oA�bA�oA�bA�oA�{A�{A�oA�{A�{A�{A�{A�{A�{A��A�{A�{A��A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�{A�oA�{A�oA�bA�bA�bA�bA�oA�oA�oA�oA�oA�oA�{A�oA�oA�{A�oA�bA�oA�oA�bA�bA�bA�oA�oA�oA�{A�{A��A��A�{A�{A�{A�{A�{A��A��A�{A�{A�{A�{A�{A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A� �A�"�A� �A� �A�"�A� �A� �A� �A� �A��A� �A� �A� �A� �A� �A��A��A��A��A��A��A�"�A�"�A�"�A�"�A�$�A�$�A�$�A�"�A�"�A�$�A�$�A�"�A�$�A�$�A�"�A�"�A�"�A�"�A� �A�"�A� �A�"�A�"�A�$�A�$�A�$�A�&�A�&�A�&�A�&�A�$�A�$�A�"�A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�oA�{A�oA�bA�VA�VA�JA�VA�oA�oA�bA�oA�oA�bA�bA�
=A�JA�A�A�A�  A���A���A��A��A��A��A��A��A��A��A��`A��;A��/A��#A��/A��
A��
A��
A���A�ȴA�ȴA�ĜA�ĜA־wAָRAִ9A֥�A֣�A֥�A֡�A։7AցA�n�A�/A��A�{A��A�oA���A���AոRAՃA�7LA��A��AԸRAԑhA�dZA�=qA��A��#A�VA�&�AҾwA�S�A��A�I�A�-AϏ\A�Aκ^A��mA͏\AͅA�~�A�-A̸RA̋DA�\)A��yA�x�A��A��TA���Aʡ�A�p�A�{AɮAɡ�Aɗ�A�x�A�1'A��;Aȉ7A�G�A�1'A�A��;AǸRAǧ�Aǝ�AǑhAǅA�~�A�z�A�r�A�Q�A�7LA�"�A�%A��A��A��A��A��/Aơ�AƉ7A�^5A�(�Aź^A��HA�p�A�JAô9AÙ�A�v�A�JA���A¬A�A�A�z�A�XA�&�A���A��`A���A�Q�A�-A��A�VA���A��A��DA��A���A�5?A�
=A��;A���A���A��PA�v�A�K�A�bA���A��wA���A��A�l�A�1'A��DA�n�A���A��A��;A�jA�K�A�7LA�"�A��A��-A��A���A���A���A��uA�XA�ƨA�v�A�Q�A�+A��jA�jA�%A���A�33A���A��^A���A���A���A���A���A��\A��A�~�A�t�A�jA�bNA�G�A�1'A���A���A��9A���A�~�A�$�A��^A�dZA�oA��FA�K�A�A���A���A�jA�E�A��A���A��mA��/A���A�ƨA���A���A���A��7A�33A�%A�z�A�
=A��/A���A�I�A���A�x�A�n�A�7LA�+A��A��`A��A�=qA�
=A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                    ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B�BDB	�B	�B	�B
rB	�B
=B	lB	�B	�B	�B	�B
	B
	B	�B	�B
	B	�B
=B
	B
	B
	B
	B
	B
	B
=B
=B
=B
rB
=B
rB
=B
=B
	B
	B
	B
=B
=B
	B
=B
	B
=B
	B
=B
=B
=B
=B
=B
=B
=B
=B
=B
rB
rB
rB
rB
rB
rB
rB
rB
rB
=B	�B	lB�BfB�BYBB{BB
�>B
�/B
�5B
��B
��B
�0B
��B
��B
�UB
��B
ŢB
�B
ޞB
��B�B@�B[�B��B��B�MB�~B�XB��B�LB��B�IB��BƨB�EB֡B� B�TB� B��B�B�B��B�+B�\B��B�GBv`BlWBgmBP}B=qB4nB,=B%FB~BB
ԕB
��B
�[B
��B
�PB
i�B
EmB
.IB
\B
B	��B	�
B	�qB	�B	lWB	e,B	\�B	S�B	NB	EmB	B�B	C�B	5tB	5�B	/�B	/B	)�B	(�B	'B	$tB	#B	 �B	 �B	B	�B	~B	�B	&�B	"�B	#:B	-�B	,�B	+�B	*0B	*�B	,=B	6zB	9�B	:�B	?B	C�B	N�B	OvB	P�B	S�B	R B	V9B	Q�B	P�B	S�B	P�B	PHB	M6B	K�B	K�B	LdB	R B	a�B	qAB	}�B	�B	��B	��B	�B	� B	��B	��B	��B	�SB	�xB	��B	�!B	�\B	��B	��B	��B	�\B	�bB	��B	�0B	�B	�B	��B	��B	��B	��B	�$B	��B	�$B	��B	��B	�RB	��B	��B	��B	��B	�zB	�B	�zB	��B	�?B	�nB	��B	��B	��B	��B	��B	��B	��B	�B	�6B	�eB	��B	�RB	��B	�B	��B	�B	��B	��B	��B	�	B	�	B	��B	�xB	�IB	�VB	�\B	��B	��B	�_B	�6B	��B	��B	�RB	�RB	��B	��B	��B	�<B	�B	�}B	�}B	�B	��B	�B	��B	��B	��B	��B	�}B	�HB	�HB	�B	� B	��B	��B	��B	��B	��B	�gB	ÖB	��B	B	�'B	��B	�[B	�OB	�B	�B	�kB	�B	�B	�eB	�=B	�B	��B	��B	�B	�LB	��B	�B	��B	��B	��B	�B	�wB	�OB	��B	��B	�aB	�aB	�-B	��B	�B	�3B	��B	��B	��B	�zB	��B	�B	�$B	��B	�B	�B	�OB	�UB	�mB	�zB	�B	�RB	�B	�)B	�pB	�HB	бB	�B	��B	��B	՛B	՛B	�9B	�yB	�5B	�vB	��B	��B	�B	�B	�B	�TB	� B	��B	� B	�B	�B	��B	�B	�B	�B	��B	�B	��B	�B	�WB	�B	��B	�B	�cB	��B	�B	��B	�B	�AB	�|B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�fB	�2B	�fB	�2B	�2B	�fB	�2B	�	B	��B	�rB	�	B	�>B	�B	�B	�DB	�xB	�xB	�xB	�DB	�B	��B	��B	��B	��B	��B	�(B	�.B	�.B	��B	��B
 iB
 �B
B
�B
B
;B
�B
�B
AB
�B
�B
�B
�B
SB
SB
B
�B
�B
�B
�B
�B
%B
�B
�B
�B
fB
�B
	lB
	7B

�B
B
JB
�B
xB
DB
B
JB
PB
B
B
PB
PB
�B
�B
�B
.B
�B
:B
B
{B
{B
�B
B
�B
B
�B
+B
�B
�B
�B
�B
eB
1B
eB
�B
1B
�B
=B
�B
�B
�B
	B
	B
qB
�B
�B
CB
�B
�B
xB
B
~B
OB
�B
�B
 �B
!bB
!�B
"4B
"4B
"4B
#B
#nB
#�B
$@B
$�B
$�B
%zB
%FB
%zB
%zB
%�B
&�B
&�B
'B
'B
'�B
'RB
'�B
(�B
'�B
($B
'�B
(�B
(�B
(�B
)_B
)�B
*�B
*0B
*eB
+kB
+6B
+�B
+�B
,qB
,=B
-CB
-B
,�B
-B
,�B
.IB
.IB
.B
.IB
.IB
/B
/�B
/�B
0�B
0UB
0!B
0UB
0�B
1'B
1[B
1'B
1�B
1�B
0�B
33B
2�B
33B
2�B
2�B
33B
2�B
3�B
49B
4�B
5B
4nB
4nB
5B
5�B
5�B
4�B
6B
6�B
6zB
6FB
6�B
6�B
8�B
9�B
:�B
9�B
:^B
9�B
:�B
;0B
=qB
<�B
<�B
<jB
<B
;�B
;�B
=B
?B
>B
>wB
=�B
=<B
=�B
=�B
?HB
A�B
B'B
A�B
AUB
A B
B�B
B�B
CaB
B[B
B[B
B[B
C�B
E9B
E9B
D�B
C�B
D�B
D�B
E9B
EmB
EmB
EmB
E9B
EmB
FB
F?B
F�B
F�B
F?B
F?B
GB
G�B
G�B
GEB
GEB
GB
G�B
I�B
IB
H�B
HKB
G�B
HB
HB
IB
IB
I�B
IB
H�B
H�B
H�B
H�B
H�B
I�B
JXB
JXB
J#B
J�B
J�B
J�B
K^B
K�B
LdB
LdB
LdB
L�B
L�B
MB
L�B
NB
OBB
N�B
N<B
OBB
QNB
Q�B
QNB
Q�B
R B
Q�B
Q�B
R�B
S�B
S&B
S[B
S&B
S�B
S�B
TaB
T�B
T,B
S�B
TaB
T�B
T�B
UgB
U2B
V�B
W?B
VmB
W
B
W�B
W�B
W�B
XEB
X�B
YB
YB
YB
YB
YB
YKB
ZQB
[#B
Z�B
[�B
[#B
Z�B
\)B
\]B
\]B
\�B
\�B
]/B
\�B
\]B
\]B
\�B
\�B
\�B
]/B
]/B
^�B
^�B
^jB
^5B
^5B
^�B
_;B
^jB
_pB
^�B
_;B
^�B
_;B
_B
_;B
_�B
_;B
_�B
`BB
`�B
a�B
bNB
b�B
b�B
b�B
b�B
c�B
d&B
d�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
e`B
e�B
e�B
e�B
e�B
e�B
f�B
g8B
f�B
gB
gB
g�B
hsB
hsB
iB
iDB
iDB
iyB
jKB
jB
jB
jKB
j�B
kB
kQB
kQB
k�B
j�B
j�B
j�B
j�B
j�B
kB
k�B
k�B
kQB
k�B
k�B
lWB
l�B
l�B
l�B
m)B
m)B
m)B
l�B
lWB
n/B
n�B
n�B
o�B
oiB
oiB
oiB
o5B
o�B
pB
pB
oiB
o�B
pB
oiB
o�B
poB
p;B
p�B
qAB
qB
qAB
q�B
qB
qB
rGB
r�B
q�B
qvB
qvB
rB
r|B
r�B
r|B
r�B
sB
r�B
sB
r�B
r�B
s�B
tTB
t�B
s�B
tB
t�B
t�B
t�B
t�B
u�B
v+B
v`B
v`B
v+B
v`B
v`B
v`B
v+B
v`B
v`B
v�B
v`B
u�B
u�B
v+B
v`B
v+B
v`B
v�B
v�B
w2B
w2B
wfB
w�B
w�B
xB
x8B
y	B
y>B
y	B
yrB
zDB
zB
z�B
zB
z�B
z�B
{B
zxB
{B
{B
z�B
{�B
{JB
{�B
{B
{JB
{�B
|B
|�B
|B
{�B
|�B
|�B
|�B
}"B
}VB
|�B
|�B
}VB
}"B
}�B
~�B
~�B
.B
~]B
~�B
~]B
~�B
~�B
.B
.B
�B
�B
�iB
� B
�4B
�iB
�iB
��B
�iB
��B
��B
�;B
�B
�B
�oB
��B
�;B
�AB
��B
�AB
��B
��B
��B
�uB
�uB
�uB
�AB
�uB
��B
�B
�B
��B
�B
��B
�{B
��B
��B
��B
��B
�B
��B
�MB
��B	B	�B�BB	lB
=BhB
rB
=B
�B	�B
rB	�B	7B	7B�B1B	lB	7B	7B
=B
rBDB
�B
rB
rB
�B
�B
rB
=B
rB
=B
�B
	B
	B	�B
	B	�B	�B	�B
	B�B	B	7B	7B�B	7B	�B	�B	�B	�B	�B	lB	lB	lB	7B	lB
	B	B	lB	B�B	B�BfB�B�B�B	lB	�B	7B	7B	�B	B	B	7B	B�B�BfB�B	7B	BfB	�B
rB
=B
rBxB
�B
	B
	B	�B
�BBB
�B
=BDB
�BBDB
�B
�B
�B
�B
�B
�B
rB
=B
�B
=B
�BDB
�B
�BDBBDB
rB
�B
rB
�B
�B
�B
rBB
�BB
�B
�B
	B
	B
rB
=B
=B
=B
	B
	B	lB	�B	�B	7B	�B
�B
=B�B	7B	7B	7B	lB	7B�B	lB	lB�B	lB	B	7B	lB	�B	7B	�B�B	lB	�B	�B	7B	lB	lB	7B	lB	lB	�B	lB	7B	7B	lB	lB	�B
=B
=B
	B
=B
rBDBB
�B
rBB
�B
�B
�B
=B
rB
�B
=B
	B
	B
�B
rB
	B
	B
=B
	B
=B
	B
	B	�B	B	7B	lB	�B
	B
�B	�B	lB	�B	B	lB	�B	�B	�B	�B	B	lB	lB	7B	7B	lB	B	lB	7B	7B	7B	B	lB	B	B	B	7B	7B	7B	7B	B	B	7B	lB	7B	lB	�B	�B	lB	lB	7B	lB	7B	lB	�B	�B	�B	�B
=B
=B
rB
�B
=B
rB
�BBDBBDBDBDBB
�BDBDBBBB
�B
�B
rB
rB	lB	lB	lB	7B	7B	�B	7B	7B	�B
=B
rB
=B
�BBDBDBDBDBxBxBfBB
�B	�B
rB
=B	�B	�B	lB	�B	lB	�B	�B	�B	�B	�B
rB
=B
rB
�BDBxBxBDBB
�B
�B
�B
�B
�B
�B	lB
	B
	B
	B
=B	7B	�B	�B
rB
�B
�BBBDB�BB�B
rB
�B
	B	�B	�B	7B	�B	�B	�B	�B
=B
=B
rB
=B
rB
rBDBxBxBxBB
�B
=B
	B
	B
=B	�B	lB	7B	�B
	B
=B
	B
=B
=B
	B
�BBBxB�B�BxBDB
�B
rB	�B
	B	7B	7B	lB	lB
=B
=B
�BDB�BB�B
�B
rB	7B	�B	lB�B
	B�BfB1BfB�B�B�BfB�B	lB	lB	�B	lB	�BfBfB�B�B�B�B�B�B1BfB	�B	B�B�B�B�B�B�B�B�B�BGB�B�B�B�B�B�B�B�B�BB�B�BoBoB  B �B;B�B�B iB
��BBB
�"B
�]B
��B
�	B
�+B
��B
��B
��B
�2B
�AB
�xB
�GB
�KB
��B
�&B
�>B
�8B
��B
��B
��B
�]B
ϫB
��B
�#B
ҽB
�B
�EB
�B
��B
ߤB
��B
�UB
�B
�zB
��B
�B
�*B
��B
��B
��B
�B
�*B
��B
��B
�qB
��B
��B
�zB
�zB
�eB
�UB
�aB
�B
��B
��B
��B
�aB
�UB
��B
��B
�B
��B
�B
�B
�!B
��B
��B
�-B
��B
�tB
�hB
�aB
��B
�3B
�6B
��B
��B
�BB
�}B
� B
�aB
��B
՛B
ԕB
�gB
�
B
�/B
�WB
�#B
�WB
��B
�B
�&B
��B
�B
��B
�VBB �B;B�B1B�B(�B5?B6zB<B>BB?}BB�BC�BB�BIRBMjBR�Bz�Ba�BbNBb�Bk�B��B�aB� B�	B�@B�MB�{B��B�FB�=B��B��B��B��B�@B�oB�VB��B��B��B�_B�eB��B��B��B�qB��B�B��B��B��B�FB�zB�B��B�B�zB�LB�B�XB�RB��B��B��B�B��B��B��B�B�*B�B�3B��B��B��B��B�XBǮB�EB�B�BȀB��B�BȴBȀB�EBߤBΥB�B�BیB��B�>B�B�BݘB��B�;B�jB�B�"B�B�NB�&B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                    BuBoB�BoBUB�B[BoB'B BUB�B�B�B�B�B�B�B�B�BB�B�B�B�B�B�B�B�B�B'B�B'B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B'B'B'B'B'B'B'BABAB'B�B;B �B �B 4B
�wB
��B
�jB
�jB
�RB
�GB
�)B
��B
��B
�qB
��B
�"B
�B
��B
̘B
�KB
��B
�B!-BCBcnB��B�B��B��B��B��B�B�mB��B��B�AB�SBچB�B�B�B�KB�B�#B��B��B��B�_B�Bp�BkkBi�BR B8�B/ B&2B!HB!�BNB
��B
�B
�B
�@B
�SB
o B
G�B
2�B
�B
�B	�3B	�B	ňB	�uB	i�B	gRB	[�B	R:B	I�B	@4B	C�B	D�B	2�B	5?B	.cB	+6B	#�B	"�B	 �B	 'B	�B	 �B	OB	B	�B	B	 �B	"4B	xB	 �B	)*B	(>B	&�B	$@B	$@B	(>B	0�B	2aB	4TB	9rB	AUB	I�B	I�B	MB	NpB	N�B	S�B	L0B	NB	U�B	L�B	J�B	G�B	F�B	E9B	D�B	K^B	[=B	l�B	x�B	{�B	~(B	�;B	��B	�DB	��B	��B	��B	��B	��B	��B	��B	�dB	�~B	��B	�1B	��B	�7B	��B	��B	��B	��B	��B	�B	�nB	��B	�vB	�[B	��B	�B	�'B	�[B	�-B	��B	��B	��B	�UB	�oB	�B	�B	�/B	�)B	�qB	�"B	��B	�qB	��B	�B	��B	��B	�zB	��B	��B	��B	��B	�:B	�tB	��B	��B	�&B	�pB	�9B	�mB	�[B	��B	��B	�sB	��B	�QB	��B	�:B	��B	��B	��B	�B	�B	�'B	��B	�9B	�8B	�B	��B	�lB	��B	��B	�lB	��B	��B	��B	�rB	��B	��B	�B	�rB	�dB	��B	��B	�fB	��B	�B	��B	�jB	�B	��B	��B	��B	�}B	�B	��B	�RB	�tB	�TB	�TB	��B	�zB	��B	�B	�>B	�WB	�5B	�iB	��B	��B	�fB	�hB	�mB	�B	�sB	��B	�B	��B	��B	�kB	�]B	�iB	�]B	��B	��B	�/B	� B	�OB	��B	��B	�B	�FB	�lB	�DB	��B	�HB	��B	�UB	��B	�AB	żB	�B	��B	ɆB	ʌB	̘B	͹B	�vB	�<B	�\B	�B	�mB	�yB	��B	�B	��B	�kB	�WB	��B	��B	�xB	ۦB	�]B	��B	�B	��B	��B	�:B	��B	�B	��B	�ZB	�B	��B	�FB	�B	�
B	�>B	�sB	�>B	�B	�B	�B	�B	�B	�B	�B	�B	�qB	��B	�B	�}B	��B	�iB	�B	�OB	�iB	�B	�B	�B	�-B	��B	�B	��B	�GB	�-B	�aB	�B	�hB	�B	��B	�B	�B	��B	�?B	��B	�B	��B	��B	�fB	��B	��B	��B	��B	�rB	��B	�$B	�rB	�B	�xB	��B	��B	�B	�VB	��B	��B	�qB	�<B	�VB	�(B	�BB	�(B	�BB	�]B	�wB	�}B
 iB
 �B
;B
�B
�B
�B
�B
9B
B
�B
3B
�B
B
�B
9B
SB
�B
�B
�B
1B
�B
	B
	�B
�B
�B
�B
�B
�B
�B
\B
�B
�B
}B
.B
.B
bB
4B
�B
�B
�B
4B
B
�B
�B
�B
�B
B
&B
B
�B
,B
aB
�B
�B
�B
B
B
B
YB
�B
+B
�B
�B
�B
	B
�B
	B
�B
)B
)B
IB
�B
B
B
~B
�B
�B
jB
!B
�B
VB
�B
�B
�B
 vB
!bB
 B
 BB
 vB
!B
!bB
!|B
!�B
"4B
#B
"�B
#B
#�B
#�B
$&B
$@B
%,B
$�B
%zB
%B
$�B
%`B
%�B
&�B
&fB
&fB
&�B
'B
($B
(sB
(>B
(�B
(sB
(XB
(�B
)*B
)�B
)�B
)�B
)�B
*KB
*eB
+�B
+QB
+kB
+B
+�B
+�B
+�B
,�B
,�B
-B
-wB
,�B
-B
-�B
.�B
./B
-wB
.�B
.�B
.�B
.�B
/iB
/�B
2�B
2�B
2�B
1�B
2|B
1�B
2�B
3�B
5�B
5ZB
5B
4�B
4�B
4B
3�B
5�B
7�B
6�B
6zB
5�B
5tB
5�B
5�B
8B
9�B
:*B
9�B
9�B
:xB
;�B
;�B
<B
:�B
:�B
;0B
<�B
=�B
=�B
=VB
=B
=VB
<�B
=VB
=qB
=�B
=qB
=<B
=�B
>]B
>�B
>�B
>�B
>]B
>wB
?HB
?�B
?�B
?}B
?�B
?�B
AUB
B�B
AUB
@�B
@�B
@4B
@�B
@�B
B'B
A�B
A�B
A;B
@�B
A;B
AB
AB
AB
BuB
B�B
B�B
B�B
C�B
CGB
CB
C�B
DMB
D�B
D�B
D�B
E9B
ESB
E�B
FtB
F�B
G�B
GB
GEB
H�B
JXB
JXB
I�B
I�B
JXB
I�B
J#B
K)B
K�B
K)B
K^B
KDB
K�B
LB
L�B
L�B
LJB
K�B
L�B
L�B
MPB
M�B
N"B
O�B
O�B
OvB
O�B
O�B
PHB
PHB
QB
QB
QNB
Q�B
Q�B
QNB
QhB
Q�B
R�B
S[B
S@B
TB
S�B
S[B
T�B
T�B
TaB
T�B
T�B
U2B
T�B
T{B
T�B
T�B
UB
T�B
UMB
U�B
W?B
W�B
V�B
VSB
VmB
W$B
W�B
W�B
W�B
V�B
W?B
V�B
W?B
WYB
W�B
W�B
W�B
X+B
XyB
YB
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
[�B
\)B
]B
\B
[�B
[qB
[�B
[�B
\�B
]/B
]~B
]�B
^B
]�B
^5B
^5B
^�B
_VB
_!B
_VB
_pB
`B
`�B
`�B
aHB
aHB
aHB
a�B
bNB
b�B
b�B
bNB
b�B
c B
cnB
c�B
cnB
b�B
cB
cB
b�B
c:B
cnB
c�B
c�B
c�B
c�B
c�B
d�B
eB
d�B
d�B
e,B
eB
e,B
e,B
e�B
gB
f�B
gB
g�B
g�B
g�B
gmB
g8B
g�B
g�B
g�B
gmB
g�B
g�B
g�B
h$B
h�B
hsB
h�B
iDB
i*B
iyB
i�B
iDB
iDB
jB
j�B
i�B
i�B
i�B
jKB
j�B
j�B
j�B
kB
kB
j�B
kB
j�B
j�B
k�B
l=B
lqB
k�B
l=B
l�B
l�B
l�B
m)B
n/B
n}B
ncB
ncB
ncB
n}B
n�B
ncB
nIB
nIB
n}B
n�B
nIB
m�B
m�B
nIB
ncB
nIB
n}B
n�B
o B
oiB
o�B
o�B
o�B
o�B
p;B
p�B
q'B
qAB
q'B
q�B
r|B
rGB
r�B
rGB
r�B
r�B
r�B
r�B
s3B
s3B
sB
s�B
sMB
s�B
s�B
sMB
s�B
t�B
t�B
tB
s�B
t�B
t�B
t�B
u?B
utB
t�B
t�B
utB
u%B
u�B
v�B
v�B
w�B
vzB
v�B
v`B
v�B
v�B
wLB
wLB
w�B
w�B
xlB
xB
x8B
x�B
x�B
x�B
xlB
x�B
x�B
y�B
zxB
y�B
yrB
y�B
yrB
zxB
y�B
z*B
zB
z�B
z^B
z�B
z�B
z�B
zDB
zxB
{B
{B
{B
z�B
{JB
{�B
{dB
{�B
{�B
{�B
{�B
|B
{�B
|PG�O�B �B�BmB�B B�B	B'B�B[B�B'B�B �B �B �B
��B B �B �B�B'B�B[B'B'B[B[B'B�B'B�B[B�B�BUB�B�B�B�B�B �B �B �B �B �B �B�BUB�B�BUB B B B �B B�B �B B �B �B �B OB B �B �B �B BUB �B �BUB �B �B �B �B �B �B B �B �B �B B�B'B�B'B-B[B�B�B�B[B�B�B�B�B�B[B�B�B�B�B[B�B[B[B'B�B[B�B[B�B[B[B�B�B�B'B[B'B[B�B�B'B�B�B�B�B[B�B�B'B�B�B�B�B�B BUBUB �B�B�B�B �B �B �B �B B �B �B B B �B B �B �B BUB �BUB OB BUBUB �B B B �B B B�B B �B �B B B�B�B�B�B�B'B�B�B[B'B�B�B�B�B�B'B�B�B�B�B[B'B�B�B�B�B�B�B�BUB �B �B BUB�B[BUB B�B �B B�BUBUB�B �B B B �B �B B �B B �B �B �B �B B �B �B �B �B �B �B �B �B �B �B B �B BUBUB B B �B B �B BUBUBUB�B�B�B'B[B�B'B[B�B�B�B�B�B�B�B�B�B�B�B�B�B�B[B'B'B B B B �B �BUB �B �B�B�B'B�B�B�B�B�B�B�B-B-B B�B[B�B'B�B�B�B B�B BUBUBUBUB�B'B�B'B�B�B-B-B�B�B[B[B[B�B[B[B B�B�B�B�B �BUB�B'B�B�B�B�B�BaB�BaB'B[B�BUBUB �B�B�B�B�B�B�B'B�B'B'B�B-B-B-B�B�B�B�B�B�B�B B �B�B�B�B�B�B�B�B�B�B�B-B�BaB-B�B�B'B�B�B �B �B B B�B�B�B�B�B�B�B�B'B �B�B B �B�B
��B B
��B B OB �B
�HB B
�}B B B�B B�B B B
��B
��B
�HB
��B
�wB
��B
��B B�B �B OB �B
�BB
�}B
��B
�6B
�}B
�wB
�jB
��B
�<B
�qB
��B
�<B
�qB
�6B
�dB
�qB
��B
��B
��B
��B
�$B
�$B
��B
�RB
��B
�XB
�jB
�B
�?B
��B
��B
��B
�B
�B
�B
��B
�CB
�qB
�nB
��B
��B
�-B
��B
��B
��B
��B
��B
��B
ܬB
רB
֡B
�,B
�zB
ӏB
��B
ʌB
��B
�B
��B
��B
�sB
�}B
�$B
��B
�IB
�_B
��B
��B
��B
��B
�hB
��B
��B
�kB
��B
�@B
��B
��B
�IB
�IB
�4B
�$B
�0B
��B
�qB
��B
�zB
�0B
�$B
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
�CB
�6B
�0B
�_B
�B
�B
��B
��B
�B
�KB
��B
�0B
ΥB
�jB
�dB
�6B
��B
��B
�&B
��B
�&B
՛B
��B
��B
��B
�dB
�vB
�B
��B
�RB
�	B
�6B
��BpB vB,�B./B3�B5�B72B:�B;JB:xBABEBJ=Br�BYeBZBZ�Bc�B{�B�B��B��B��B�B�0B�dB��B��B��B�jB��B�dB��B�#B�
B�KB��B�BB�B�B�KB��B�TB�&B�|B��B�pB�pB��B��B�/B��B��B��B�/B�B��B�B�B�`B��B�;B��B�HB�QB�iB��B��B��B��B�rB��B�B�>B�B�cB��B��B��B�4B��B��B�iB�4B��B�YB�YB�@BּB�@BٚB��B�VB��B�MBٚB��B�B޸B��B�IB�B��B�7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<N�<��0<�"<k�g<2T�<#�
<#�
<#�
<#�
<z��<#�
<#�
<#�
<3և<2\�<�?�<��<#�
<#�
<C��<#�
<#�
<#�
<#�
<RLX<#�
<#�
<#�
<LD�<#�
<E~�<���<#�
<OÉ<�Mv<#�
<#�
<#�
<#�
<#�
<#�
<#�
<7�<)�U<#�
<#�
<#�
<#�
<S'f<��<#�
<#�
<#�
<2��<���<`��<6��<V�)<#�
<H�<}��<�m=�?<#�
<#�
<.�|<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<$ �<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0079(+/-0.0035)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NO correction for Conductivity Thermal Mass (CTM) is applied;    OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0079(+/-0.0035)                                                                                                                     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2021112200221320211122002213IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021120123314820211201233148QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021120123314820211201233148QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022050407565520220504075655IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242420230426192424IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242420230426192424IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242420230426192424IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                