CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-02-13T12:24:25Z creation; 2023-05-01T21:35:41Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210213122425  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               r   rAA  AOAO7314_008642_114                 7314_008642_114                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�^�iv@�^�iv11  @�^�D�@�^�D�@0�B����@0�B�����b�^ F�e�b�^ F�e11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@=p�@}p�@�G�@�G�@�  @��RA��A ��A,(�A@  A_\)A�  A�  A�  A��A�Q�A�  A�Q�A�Q�A��B  B(�B�
B (�B(  B/�B7�
B?�
BH  BP(�BX  B`  Bh(�Bp(�Bw�
B�B��B��B�  B�  B�  B�  B��B��
B��
B��
B��
B��
B��
B�{B�ffB�  B�  B�  B�  B�  B�  B��B��B��B��B�  B�  B�  B�{B�{B�  C 
=C{C  C��C��C	��C
=C  C  C  C  C  C  C��C  C  C   C"  C$  C%��C'��C)��C+��C-��C/�C1��C3��C6  C8  C:  C<
=C=��C@  CB  CD  CE��CG��CJ  CL
=CN
=CP
=CR  CT  CV  CW��CZ  C\  C^  C`  Cb
=Cd
=Cf  Cg��Cj  Cl  Cm��Cp
=Cr  Cs��Cu��Cx  Cy��C{��C}��C�  C�
=C�  C���C�  C�  C���C���C�  C�  C���C���C�C�C���C���C�  C�C���C�C�C�C�C�C�C�
=C�
=C�C�  C���C�C�  C���C�  C���C�  C�
=C�C�  C���C�  C�C�  C�  C�C�
=C�  C�  C�  C���C���C���C���C���C���C�C�  C�  C�C�C���C�  C�C�C�C�  C�C�C�  C���C�  C�C�C�C�  C�C�
=C�  C�  C�C���C�  C�C���C���C���C���C���C�  C�C�  C�  C�  C���C�  C�  C�  C�C�  C���C�  C�C�C�C�  C�  C�  C���C���C���C���C�  C�C�
=C�  C�  C���C���C���C�C�  C�  C�  C�  C���C���C�  C���C���D � D �qD}qD�D�DD��D�D��D  D� D  D}qD�qDz�D�qD}qD�qD	� D
  D
��D�D��DD��D��Dz�D�qD� D�D� D�qD��D  D� D�D� D�qD��D�qD}qD�qDz�D�qD��D  D� D�qD}qD�qD}qD�D� D��Dz�D�D��D�D�D  D� D�D� D�qD ��D!�D!� D!�qD"� D#�D#}qD#��D$� D%�D%� D%�qD&}qD'  D'�D(�D(��D)�D)� D*  D*� D+  D+}qD+��D,� D-D-��D.�D.� D/  D/}qD/��D0z�D0�RD1xRD2  D2�D3D3��D4�D4� D4�qD5}qD5��D6z�D6��D7� D8D8�D9�D9�D:D:� D;  D;��D<�D<� D<�qD=}qD=�qD>� D?�D?� D?�qD@� DA  DA� DB  DB� DC�DC��DD  DD}qDE  DE� DF  DF��DG  DG� DG�qDH}qDI  DI��DJ�DJ��DK�DK� DL  DL��DL�qDM}qDN  DN� DO  DO� DP  DP� DP�qDQz�DQ�qDR� DR�qDS}qDT�DT� DT�qDU}qDU�qDV� DV�qDW}qDW�qDX� DY  DY� DZ  DZ}qDZ��D[}qD\  D\}qD]  D]� D^  D^� D_  D_��D`  D`� Da�Da��Da�qDb}qDb�qDc� Dd  Dd}qDe  De��Df�Df� Dg�Dg}qDh  Dh�Di  Di}qDi�qDj��DkDk��Dl  Dl��DmDm�Dm�qDn}qDn�qDoz�Do�qDp}qDp�qDq� Dr�Dr��Ds  Dsz�Dt  Dt��Du�Du� Dv  Dv}qDv�qDw��Dx  Dx� Dx�qDy}qDz  Dz� Dz�qD{}qD|  D|}qD|�qD}� D}�qD~}qD  D��D��D�AHD�� D�� D�  D�>�D�~�D��qD��qD�@ D�� D��qD��qD�=qD�}qD���D���D�>�D�~�D���D�  D�@ D�� D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�� D���D�HD�@ D�~�D���D��qD�>�D�~�D��qD��qD�@ D���D�D��D�AHD��HD�D��D�B�D���D�D��D�B�D�� D���D�HD�@ D�~�D��qD��qD�>�D��HD�� D���D�=qD�~�D�� D�HD�B�D��HD�� D�  D�@ D��HD��HD�  D�AHD���D��HD���D�>�D�� D���D���D�@ D��HD�� D�HD�B�D��HD���D�  D�AHD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D���D�  D�AHD��HD�� D�  D�AHD�~�D��qD���D�@ D�}qD��qD�  D�AHD�� D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD�~�D���D���D�@ D���D�D�  D�@ D��HD��HD�  D�@ D�~�D���D�  D�=qD�~�D�� D���D�@ D��HD��HD�  D�@ D�� D��HD�HD�AHD���D�� D��qD�=qD�}qD���D���D�@ D�� D���D��qD�>�D�~�D�� D�HD�B�D��HD��HD�  D�@ D�~�D���D���D�>�D�~�D��qD��qD�>�D��HD��HD�HD�AHD�� D��HD�HD�>�D�~�D��HD�  D�>�D�}qD�� D�HD�@ D�� D���D���D�>�D�~�D�� D�  D�AHD���D�� D���D�>�D�� D��HD�  D�=qD�~�D���D���D�AHD��HD�� D�  D�AHD��HD��HD�  D�>�D�� D�� D�HD�@ D�~�D���D�  D�@ D�� D���D���D�>�D�� D��HD�HD�@ D�� D��HD�HD�@ D�� D�� D�  D�AHD�� D���D�HD�@ D�� D�� D�  D�AHD D�� D�  D�AHDÀ D�� D�  D�@ DĀ D��HD�HD�AHDŀ Dž�D�HD�AHDƁHD�� D�HD�>�D�~�D��HD�HD�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�~�D�� D�  D�>�Dˀ D��HD�HD�@ D̀ D̾�D�  D�AHD́HD�� D�  D�@ D΀ D��HD�HD�B�Dπ DϾ�D�  D�@ DЁHD�� D���D�@ DсHD�D�HD�AHDҁHD��HD�  D�>�D�}qDӾ�D�HD�AHDԁHD��HD���D�>�DՁHD��HD���D�@ Dր D־�D�  D�AHD׀ D�� D�HD�@ D�}qDؾ�D�  D�@ Dـ Dپ�D�  D�AHDڀ Dھ�D���D�@ D�~�D�� D�HD�@ D�~�DܽqD���D�@ D݀ Dݾ�D��qD�=qD�}qD�� D�HD�AHD߁HD��HD�HD�@ D��HD�� D�  D�@ D� DᾸD���D�>�D�~�D�� D��D�@ D�|)D㾸D�HD�AHD�HD��HD��D�B�D�HD徸D�  D�@ D� D�� D���D�=qD�~�D��HD�HD�@ D�~�D��HD�HD�@ D� D龸D�HD�@ D�~�D꾸D�  D�@ D� D�� D�  D�@ D� D�� D�HD�AHD�HD���D�  D�@ D�~�D�� D�HD�@ D� DﾸD���D�=qD�� D��HD�HD�@ D�~�D�D��qD�@ D�D��HD�  D�@ D�~�D�� D�  D�@ D� D�� D�  D�@ D�� D��HD�HD�AHD���D�D�  D�>�D�~�D���D���D�@ D��HD��HD�  D�@ D�� D��HD�  D�AHD�q�D���?\)?��?W
=?�  ?�{?\?�(�@�\@�@!G�@5@B�\@W
=@h��@z�H@��@�\)@�Q�@�G�@��@�33@��H@�ff@�\)@�
=@�  @���@��@���A�AA�A��A�A��A{A#33A'�A,(�A0��A5A:=qA@  AE�AJ=qAN�RAS�
AXQ�A\��Ab�\Ag
=Ak�Ap��AvffAz�HA�  A��HA��A�\)A��A�(�A��RA�G�A��A�{A���A�33A�A�Q�A��\A�z�A�
=A�G�A��A�{A�Q�A��\A��A�\)A��A�z�A��RA���A�33A��A�\)A��A�(�AθRA���A��
A�{Aأ�A�33A��A߮A��A�(�A�RA���A�A�{A�Q�A�\A��A��A��A�(�A��RB z�B��B�RB(�BG�B�\B�
B��B
=qB\)BQ�BB�HB(�Bp�B�\B�
B��B{B\)Bz�B��B�RB(�BG�B�RB   B!G�B"=qB#\)B$��B%B'
=B(Q�B)��B*�HB,Q�B-G�B.�\B/�B0��B2=qB3\)B4��B5B7
=B8Q�B9G�B:�\B;�B<��B=�B?33B@z�BABB�HBD  BEG�BFffBG�BH��BI�BK33BL��BM�BO
=BP(�BQG�BRffBS�BT��BV=qBW�BX��BYBZ�RB\(�B]p�B^�RB_�
B`��Bb{Bc33Bdz�Be��Bg
=BhQ�Bi��Bj�RBk�
Bm�Bn=qBo\)Bpz�Bq�Bs33Btz�Bup�Bv�\Bw�
By�BzffB{�B|��B}�B
=B�(�B���B�p�B�{B��\B��B�B�Q�B���B���B�=qB���B�\)B��B�z�B��B�B�Q�B���B��B�{B���B�\)B��B�z�B��B���B�=qB���B�p�B�{B��\B�33B��B�=qB��RB�p�B�  B��\B��B���B�{B���B�33B�B�Q�B��HB�G�B�B�=qB���B�\)B��
B�=qB��RB��B���B�(�B��RB�G�B��
B�ffB���B��B�(�B��RB�\)B�  B��\B��B�B�ffB��B�B�ffB���B���B�=qB�
=B�B�Q�B�
=B���B�Q�B���B�B�ffB�
=B��B�Q�B�
=B�B�ffB���B��B�=qB���B�G�B�B�(�B��RB��B��B��
B�  B�Q�B���B��HB��B�33B�\)B���B��
B�{B�=qB�z�B��\B��HB��B�G�B�p�B���B��
B�(�B�Q�B�z�B���B��HB�33B�p�B���B�B�  B�=qB�z�B£�B��HB��B�\)B�B�  B�(�B�ffBģ�B���B�G�Bř�B��
B�{B�Q�BƸRB�
=B�G�BǅB��
B�(�B�z�B���B���B�\)BɮB��B�(�B�ffB���B��B�p�BˮB��B�Q�Ḅ�B���B��B�p�B�B�(�B�ffBΣ�B���B�\)BϮB�  B�Q�BУ�B�
=B�\)BѮB�  B�ffB���B�
=B�p�B�B�(�Bԏ\B���B�33Bՙ�B�  B�Q�B֏\B�
=B�p�B�B�{B�z�B��HB�33BمB��B�Q�BڸRB�
=B�G�B�B�(�B�z�B���B�G�BݮB�  B�Q�B���B�33B߅B��
B�Q�B�RB��B�\)B��
B�Q�B��B���B�\)B��
B�Q�B��B���B�p�B��B�Q�B��B��B癚B�  B�Q�B���B�\)B�B�{B�\B�
=B�B��
B�=qB���B�G�B��B�{B�\B�
=B�p�B��
B�=qB���B�G�B�B�(�B��B�
=B�p�B��B�z�B��HB�33B�B�=qB���B���B��B�  B�Q�B���B�G�B��B�{B�z�B�
=B�\)B�B�=qB���B�
=B�p�B��B�Q�B���B���B�p�B��
C �C G�C �C C �HC�C\)C�C�C�C(�CG�Cz�C�RC�C{CG�Cz�C�C�
C
=C=qCp�C��CC  C(�CQ�C�CC�C
=CG�Cz�C��CC  C(�CQ�Cz�C�C�HC  C33Cp�C��CC	  C	33C	ffC	�\C	��C
  C
(�C
\)C
��C
C
��C33C\)C�C��C��C(�CffC��C�RC  C=qC\)C�\C�
C  C33Cz�C��C��C{CG�Cp�C�RC�C�CQ�C��CC  C=qCffC��C�HC
=CG�C�C�RC�
C(�CQ�C�CC  C(�C\)C��C��C  C=qCp�C��C�HC
=CG�C�C�RC�HC(�C\)C�\C��C��C(�Cp�C��C��C{CG�C�C��C��C33Cp�C��C��C(�C\)C��C�
C�C\)C�\C�
C
=CQ�C��C�
C{C\)C�\C�
C�CQ�C�\C�HC 
=C Q�C ��C ��C!{C!Q�C!�C!�
C"{C"G�C"��C"��C#
=C#Q�C#��C#C${C$Q�C$�\C$�HC%{C%Q�C%��C%�
C&�C&\)C&��C&�
C'�C'Q�C'��C'�
C({C(\)C(��C(�
C)�C)Q�C)��C)�
C*�C*\)C*��C*�HC+�C+Q�C+�C+�
C,(�C,ffC,��C,��C-(�C-p�C-�RC-�C.33C.�C.�RC/  C/=qC/�C/�
C0
=C0\)C0��C0�
C1(�C1p�C1��C1��C233C2�C2��C3
=C3Q�C3��C3�HC433C4p�C4�RC5
=C5=qC5�\C5�
C6{C6ffC6�C6��C7G�C7z�C7��C8{C8\)C8�C8�HC933C9z�C9�RC:{C:G�C:�\C:�
C;{C;p�C;�C;��C<=qC<z�C<�
C={C=\)C=��C=�HC>33C>p�C>�RC?
=C?=qC?��C?�
C@(�C@ffC@�CA  CA33CA�CA��CB{CBffCB��CB�CC=qCCz�CC��CD
=CD\)CD��CD�CE33CEp�CE��CF  CF\)CF��CF�HCG(�CGp�CGCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                ?�  ?��H@=p�@}p�@�G�@�G�@�  @��RA��A ��A,(�A@  A_\)A�  A�  A�  A��A�Q�A�  A�Q�A�Q�A��B  B(�B�
B (�B(  B/�B7�
B?�
BH  BP(�BX  B`  Bh(�Bp(�Bw�
B�B��B��B�  B�  B�  B�  B��B��
B��
B��
B��
B��
B��
B�{B�ffB�  B�  B�  B�  B�  B�  B��B��B��B��B�  B�  B�  B�{B�{B�  C 
=C{C  C��C��C	��C
=C  C  C  C  C  C  C��C  C  C   C"  C$  C%��C'��C)��C+��C-��C/�C1��C3��C6  C8  C:  C<
=C=��C@  CB  CD  CE��CG��CJ  CL
=CN
=CP
=CR  CT  CV  CW��CZ  C\  C^  C`  Cb
=Cd
=Cf  Cg��Cj  Cl  Cm��Cp
=Cr  Cs��Cu��Cx  Cy��C{��C}��C�  C�
=C�  C���C�  C�  C���C���C�  C�  C���C���C�C�C���C���C�  C�C���C�C�C�C�C�C�C�
=C�
=C�C�  C���C�C�  C���C�  C���C�  C�
=C�C�  C���C�  C�C�  C�  C�C�
=C�  C�  C�  C���C���C���C���C���C���C�C�  C�  C�C�C���C�  C�C�C�C�  C�C�C�  C���C�  C�C�C�C�  C�C�
=C�  C�  C�C���C�  C�C���C���C���C���C���C�  C�C�  C�  C�  C���C�  C�  C�  C�C�  C���C�  C�C�C�C�  C�  C�  C���C���C���C���C�  C�C�
=C�  C�  C���C���C���C�C�  C�  C�  C�  C���C���C�  C���C���D � D �qD}qD�D�DD��D�D��D  D� D  D}qD�qDz�D�qD}qD�qD	� D
  D
��D�D��DD��D��Dz�D�qD� D�D� D�qD��D  D� D�D� D�qD��D�qD}qD�qDz�D�qD��D  D� D�qD}qD�qD}qD�D� D��Dz�D�D��D�D�D  D� D�D� D�qD ��D!�D!� D!�qD"� D#�D#}qD#��D$� D%�D%� D%�qD&}qD'  D'�D(�D(��D)�D)� D*  D*� D+  D+}qD+��D,� D-D-��D.�D.� D/  D/}qD/��D0z�D0�RD1xRD2  D2�D3D3��D4�D4� D4�qD5}qD5��D6z�D6��D7� D8D8�D9�D9�D:D:� D;  D;��D<�D<� D<�qD=}qD=�qD>� D?�D?� D?�qD@� DA  DA� DB  DB� DC�DC��DD  DD}qDE  DE� DF  DF��DG  DG� DG�qDH}qDI  DI��DJ�DJ��DK�DK� DL  DL��DL�qDM}qDN  DN� DO  DO� DP  DP� DP�qDQz�DQ�qDR� DR�qDS}qDT�DT� DT�qDU}qDU�qDV� DV�qDW}qDW�qDX� DY  DY� DZ  DZ}qDZ��D[}qD\  D\}qD]  D]� D^  D^� D_  D_��D`  D`� Da�Da��Da�qDb}qDb�qDc� Dd  Dd}qDe  De��Df�Df� Dg�Dg}qDh  Dh�Di  Di}qDi�qDj��DkDk��Dl  Dl��DmDm�Dm�qDn}qDn�qDoz�Do�qDp}qDp�qDq� Dr�Dr��Ds  Dsz�Dt  Dt��Du�Du� Dv  Dv}qDv�qDw��Dx  Dx� Dx�qDy}qDz  Dz� Dz�qD{}qD|  D|}qD|�qD}� D}�qD~}qD  D��D��D�AHD�� D�� D�  D�>�D�~�D��qD��qD�@ D�� D��qD��qD�=qD�}qD���D���D�>�D�~�D���D�  D�@ D�� D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�� D���D�HD�@ D�~�D���D��qD�>�D�~�D��qD��qD�@ D���D�D��D�AHD��HD�D��D�B�D���D�D��D�B�D�� D���D�HD�@ D�~�D��qD��qD�>�D��HD�� D���D�=qD�~�D�� D�HD�B�D��HD�� D�  D�@ D��HD��HD�  D�AHD���D��HD���D�>�D�� D���D���D�@ D��HD�� D�HD�B�D��HD���D�  D�AHD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D���D�  D�AHD��HD�� D�  D�AHD�~�D��qD���D�@ D�}qD��qD�  D�AHD�� D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD�~�D���D���D�@ D���D�D�  D�@ D��HD��HD�  D�@ D�~�D���D�  D�=qD�~�D�� D���D�@ D��HD��HD�  D�@ D�� D��HD�HD�AHD���D�� D��qD�=qD�}qD���D���D�@ D�� D���D��qD�>�D�~�D�� D�HD�B�D��HD��HD�  D�@ D�~�D���D���D�>�D�~�D��qD��qD�>�D��HD��HD�HD�AHD�� D��HD�HD�>�D�~�D��HD�  D�>�D�}qD�� D�HD�@ D�� D���D���D�>�D�~�D�� D�  D�AHD���D�� D���D�>�D�� D��HD�  D�=qD�~�D���D���D�AHD��HD�� D�  D�AHD��HD��HD�  D�>�D�� D�� D�HD�@ D�~�D���D�  D�@ D�� D���D���D�>�D�� D��HD�HD�@ D�� D��HD�HD�@ D�� D�� D�  D�AHD�� D���D�HD�@ D�� D�� D�  D�AHD D�� D�  D�AHDÀ D�� D�  D�@ DĀ D��HD�HD�AHDŀ Dž�D�HD�AHDƁHD�� D�HD�>�D�~�D��HD�HD�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�~�D�� D�  D�>�Dˀ D��HD�HD�@ D̀ D̾�D�  D�AHD́HD�� D�  D�@ D΀ D��HD�HD�B�Dπ DϾ�D�  D�@ DЁHD�� D���D�@ DсHD�D�HD�AHDҁHD��HD�  D�>�D�}qDӾ�D�HD�AHDԁHD��HD���D�>�DՁHD��HD���D�@ Dր D־�D�  D�AHD׀ D�� D�HD�@ D�}qDؾ�D�  D�@ Dـ Dپ�D�  D�AHDڀ Dھ�D���D�@ D�~�D�� D�HD�@ D�~�DܽqD���D�@ D݀ Dݾ�D��qD�=qD�}qD�� D�HD�AHD߁HD��HD�HD�@ D��HD�� D�  D�@ D� DᾸD���D�>�D�~�D�� D��D�@ D�|)D㾸D�HD�AHD�HD��HD��D�B�D�HD徸D�  D�@ D� D�� D���D�=qD�~�D��HD�HD�@ D�~�D��HD�HD�@ D� D龸D�HD�@ D�~�D꾸D�  D�@ D� D�� D�  D�@ D� D�� D�HD�AHD�HD���D�  D�@ D�~�D�� D�HD�@ D� DﾸD���D�=qD�� D��HD�HD�@ D�~�D�D��qD�@ D�D��HD�  D�@ D�~�D�� D�  D�@ D� D�� D�  D�@ D�� D��HD�HD�AHD���D�D�  D�>�D�~�D���D���D�@ D��HD��HD�  D�@ D�� D��HD�  D�AHD�q�G�O�?\)?��?W
=?�  ?�{?\?�(�@�\@�@!G�@5@B�\@W
=@h��@z�H@��@�\)@�Q�@�G�@��@�33@��H@�ff@�\)@�
=@�  @���@��@���A�AA�A��A�A��A{A#33A'�A,(�A0��A5A:=qA@  AE�AJ=qAN�RAS�
AXQ�A\��Ab�\Ag
=Ak�Ap��AvffAz�HA�  A��HA��A�\)A��A�(�A��RA�G�A��A�{A���A�33A�A�Q�A��\A�z�A�
=A�G�A��A�{A�Q�A��\A��A�\)A��A�z�A��RA���A�33A��A�\)A��A�(�AθRA���A��
A�{Aأ�A�33A��A߮A��A�(�A�RA���A�A�{A�Q�A�\A��A��A��A�(�A��RB z�B��B�RB(�BG�B�\B�
B��B
=qB\)BQ�BB�HB(�Bp�B�\B�
B��B{B\)Bz�B��B�RB(�BG�B�RB   B!G�B"=qB#\)B$��B%B'
=B(Q�B)��B*�HB,Q�B-G�B.�\B/�B0��B2=qB3\)B4��B5B7
=B8Q�B9G�B:�\B;�B<��B=�B?33B@z�BABB�HBD  BEG�BFffBG�BH��BI�BK33BL��BM�BO
=BP(�BQG�BRffBS�BT��BV=qBW�BX��BYBZ�RB\(�B]p�B^�RB_�
B`��Bb{Bc33Bdz�Be��Bg
=BhQ�Bi��Bj�RBk�
Bm�Bn=qBo\)Bpz�Bq�Bs33Btz�Bup�Bv�\Bw�
By�BzffB{�B|��B}�B
=B�(�B���B�p�B�{B��\B��B�B�Q�B���B���B�=qB���B�\)B��B�z�B��B�B�Q�B���B��B�{B���B�\)B��B�z�B��B���B�=qB���B�p�B�{B��\B�33B��B�=qB��RB�p�B�  B��\B��B���B�{B���B�33B�B�Q�B��HB�G�B�B�=qB���B�\)B��
B�=qB��RB��B���B�(�B��RB�G�B��
B�ffB���B��B�(�B��RB�\)B�  B��\B��B�B�ffB��B�B�ffB���B���B�=qB�
=B�B�Q�B�
=B���B�Q�B���B�B�ffB�
=B��B�Q�B�
=B�B�ffB���B��B�=qB���B�G�B�B�(�B��RB��B��B��
B�  B�Q�B���B��HB��B�33B�\)B���B��
B�{B�=qB�z�B��\B��HB��B�G�B�p�B���B��
B�(�B�Q�B�z�B���B��HB�33B�p�B���B�B�  B�=qB�z�B£�B��HB��B�\)B�B�  B�(�B�ffBģ�B���B�G�Bř�B��
B�{B�Q�BƸRB�
=B�G�BǅB��
B�(�B�z�B���B���B�\)BɮB��B�(�B�ffB���B��B�p�BˮB��B�Q�Ḅ�B���B��B�p�B�B�(�B�ffBΣ�B���B�\)BϮB�  B�Q�BУ�B�
=B�\)BѮB�  B�ffB���B�
=B�p�B�B�(�Bԏ\B���B�33Bՙ�B�  B�Q�B֏\B�
=B�p�B�B�{B�z�B��HB�33BمB��B�Q�BڸRB�
=B�G�B�B�(�B�z�B���B�G�BݮB�  B�Q�B���B�33B߅B��
B�Q�B�RB��B�\)B��
B�Q�B��B���B�\)B��
B�Q�B��B���B�p�B��B�Q�B��B��B癚B�  B�Q�B���B�\)B�B�{B�\B�
=B�B��
B�=qB���B�G�B��B�{B�\B�
=B�p�B��
B�=qB���B�G�B�B�(�B��B�
=B�p�B��B�z�B��HB�33B�B�=qB���B���B��B�  B�Q�B���B�G�B��B�{B�z�B�
=B�\)B�B�=qB���B�
=B�p�B��B�Q�B���B���B�p�B��
C �C G�C �C C �HC�C\)C�C�C�C(�CG�Cz�C�RC�C{CG�Cz�C�C�
C
=C=qCp�C��CC  C(�CQ�C�CC�C
=CG�Cz�C��CC  C(�CQ�Cz�C�C�HC  C33Cp�C��CC	  C	33C	ffC	�\C	��C
  C
(�C
\)C
��C
C
��C33C\)C�C��C��C(�CffC��C�RC  C=qC\)C�\C�
C  C33Cz�C��C��C{CG�Cp�C�RC�C�CQ�C��CC  C=qCffC��C�HC
=CG�C�C�RC�
C(�CQ�C�CC  C(�C\)C��C��C  C=qCp�C��C�HC
=CG�C�C�RC�HC(�C\)C�\C��C��C(�Cp�C��C��C{CG�C�C��C��C33Cp�C��C��C(�C\)C��C�
C�C\)C�\C�
C
=CQ�C��C�
C{C\)C�\C�
C�CQ�C�\C�HC 
=C Q�C ��C ��C!{C!Q�C!�C!�
C"{C"G�C"��C"��C#
=C#Q�C#��C#C${C$Q�C$�\C$�HC%{C%Q�C%��C%�
C&�C&\)C&��C&�
C'�C'Q�C'��C'�
C({C(\)C(��C(�
C)�C)Q�C)��C)�
C*�C*\)C*��C*�HC+�C+Q�C+�C+�
C,(�C,ffC,��C,��C-(�C-p�C-�RC-�C.33C.�C.�RC/  C/=qC/�C/�
C0
=C0\)C0��C0�
C1(�C1p�C1��C1��C233C2�C2��C3
=C3Q�C3��C3�HC433C4p�C4�RC5
=C5=qC5�\C5�
C6{C6ffC6�C6��C7G�C7z�C7��C8{C8\)C8�C8�HC933C9z�C9�RC:{C:G�C:�\C:�
C;{C;p�C;�C;��C<=qC<z�C<�
C={C=\)C=��C=�HC>33C>p�C>�RC?
=C?=qC?��C?�
C@(�C@ffC@�CA  CA33CA�CA��CB{CBffCB��CB�CC=qCCz�CC��CD
=CD\)CD��CD�CE33CEp�CE��CF  CF\)CF��CF�HCG(�CGp�CGCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�aG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�?}A�=qA�?}A�?}A�=qA�C�A�C�A�E�A�?}A�?}A�C�A�C�A�I�A�M�A�K�A�M�A�M�A�O�A�O�A�M�A�M�A�I�A�E�A�C�A�K�A�jA��A��PA�A���A��wA���A��hA���A���A���A���A��DA�~�A�|�A�x�A�n�A�ffA�ffA�I�A�
=A�S�A���A��7A��A��wA�p�A� �A���A�A���A���A�1A�dZA���A�;dA�r�A�^5A�M�A�C�A��yA�oA��hA��-A��\A�K�A�1'A��`A��wA�;dA��A�jA��TA���A�VAx��Au�TAmK�Ah�RAf��Ae�#Ae�hAb��A`JA^ �AZ�AW;dAS�AP(�AK"�AH�HAGXAD~�AC|�ADAB�uA=��A;|�A7��A4v�A1t�A0�`A1oA1`BA1�FA0bA.v�A-��A+"�A)\)A&�/A&�\A&ĜA&9XA%
=A#��A!%AG�A�AA�\Ap�A�9A�AbAA�A33AK�A�TA�wA�-A�A�A��A�!A�AS�Ax�AbA�A"�AS�A�A�A�
AM�A�mA�A�A�#Al�AJAoA�/A�jA�AA;dA��A=qA�;A?}A ��A �!A v�@��@���@�
=@��7@�1@�J@�  @�E�@��@��`@�w@�;d@�
=@�@�ƨ@�1@��@���@��@���@�^5@�F@�1'@䛦@��@��`@�V@�@�|�@�~�@�@�C�@�r�@�  @�9@�%@�ȴ@��@���@�@�E�@�^5@��@�7L@�ƨ@�b@�V@�G�@�~�@��@�9X@�b@�K�@�;d@�33@���@�n�@�M�@�^@���@�(�@��;@�+@�-@ݩ�@�?}@�V@��`@�Q�@�9X@�  @۝�@ڰ!@ٲ-@��@�33@���@���@�ff@թ�@�z�@��@�I�@�ƨ@�
=@�$�@�O�@�j@υ@�o@���@�ȴ@Ώ\@���@�o@�
=@�
=@���@Ο�@�ȴ@ΰ!@�V@�$�@��T@�V@��m@��@ʟ�@�5?@ɲ-@��/@�z�@Ǖ�@�;d@��@�ȴ@�{@�hs@�Ĝ@�bN@�1'@�  @þw@Õ�@�S�@�|�@�
=@��@�@��-@�&�@��/@���@�Z@� �@�t�@��@��!@���@�=q@��@��h@�O�@���@�9X@��;@�|�@�K�@�@�5?@��#@�@���@�&�@���@���@�S�@�+@��\@���@��@�  @���@�;d@���@�~�@��@���@���@���@���@�/@�A�@��@��@�@��H@�$�@�@�@��7@�hs@�hs@�X@�?}@�?}@�/@���@��D@�bN@�1'@��m@�ƨ@���@��H@�^5@���@�x�@�?}@��@���@�9X@���@�\)@�@��!@���@��^@�G�@��@���@�j@�9X@��;@�\)@��@���@��!@�M�@�$�@��^@��7@�`B@�G�@�%@���@�I�@��;@��@�ȴ@�v�@�J@��T@�hs@�?}@��/@�  @�\)@�;d@�33@��@��@�=q@��@���@�hs@�?}@�&�@��@���@�Ĝ@��@��m@�l�@�"�@���@��R@��R@�n�@�5?@��^@��h@��@�`B@��@��j@�A�@�1@��;@��F@�C�@��H@���@�n�@��@��-@���@��7@�`B@�&�@��j@��D@�j@�1@�|�@�+@��y@���@���@���@�M�@���@���@��-@�p�@�V@���@���@��D@�bN@� �@��w@��P@�\)@�@��R@�=q@���@��-@��h@�hs@��@��`@�Z@� �@���@���@�l�@�33@��@�
=@���@�v�@�$�@���@���@�`B@��@���@��`@��9@��D@�Z@�(�@���@��w@��F@�l�@�C�@�C�@��@��y@���@�V@�{@���@���@�p�@��@��9@��@�bN@�Q�@�A�@�1@~��@~E�@~{@}�T@}�T@}��@}�h@}O�@|��@|j@{�m@{t�@{C�@{33@{o@z��@z~�@zM�@z-@z�@y��@y��@yX@x�@w�@w�@v�+@vff@vE�@u�@u�h@u/@tj@t�@s��@s�F@s33@s"�@s@r�@r�H@r��@rn�@r-@rJ@q�@q��@qhs@qG�@p�9@pQ�@p1'@p  @o�@o|�@o\)@n�y@n��@nff@n{@m�-@m?}@m/@mV@l�@kS�@j�H@j��@j��@jn�@i�@i��@i%@h�u@h1'@g�@g|�@g
=@fff@f$�@e��@e`B@eV@d�@d1@ct�@b��@bn�@a�#@a��@a7L@`��@`Q�@_�;@_�P@^��@^5?@^{@]�@]�@\I�@[dZ@[@Z�!@Z�\@Z^5@Z-@Y��@X��@X�9@XQ�@W�w@W;d@Vv�@V@U��@U��@U`B@U�@T��@T�@T�@Tz�@T1@S��@So@R��@R�\@R�@Q��@Qhs@P��@PbN@O�w@Ol�@O+@N�y@N�R@Nv�@N@M@Mp�@M/@L�/@LI�@K��@Kt�@KS�@J��@J-@JJ@I�@I��@IG�@H�`@H�@Hb@G\)@G�@F�R@F�+@FV@F$�@E�-@E?}@D��@D�/@D��@D�@D��@DI�@C�
@C�@CC�@B�H@B��@B^5@A��@AX@A&�@@Q�@?��@?|�@?;d@>ȴ@>ff@>V@>E�@>@=�@<��@<�/@<��@;��@;�F@;�@;dZ@;"�@:�!@:M�@:=q@:J@9�#@9x�@9x�@9x�@9X@97L@8��@8r�@8 �@7\)@7�@6�@6ȴ@6V@5��@5`B@4��@4��@4z�@41@3ƨ@3��@3dZ@2�@2��@2n�@2J@1�^@1G�@0�`@0�9@0 �@/��@/|�@/�@.��@.ȴ@.�+@.5?@-�@-�T@-�-@-V@,j@,(�@+�m@+dZ@+o@*�@*�H@*��@*�\@*M�@*�@)��@)�7@)X@)%@(��@(Ĝ@(��@(A�@'�w@'�P@'l�@'K�@';d@'�@&�@&v�@&$�@%�@%�-@%O�@%V@$�j@$�@$�D@$Z@$(�@$1@#ƨ@#dZ@#C�@#33@#33@#"�@#@"�@"��@"^5@"J@!��@!�7@!X@ ��@ ��@ r�@   @��@�w@|�@+@�@ff@$�@�@�T@��@��@�@O�@/@�@�/@�D@I�@I�@9X@�@��@t�@"�@o@��@^5@=q@�@�#@��@�^@��@�7@&�@��@Q�@  @�;@�@l�@K�@�y@�R@$�@�T@�T@��@@p�@��@�@��@�D@9X@�m@��@S�@C�@o@�H@��@^5@�@��@��@�7@�7@7L@�`@��@��@�u@bN@Q�@b@��@�@|�@\)@;d@
=@�@ȴ@��@V@$�@�@��@@`B@V@�@z�@��@�m@ƨ@�@"�@@
�H@
��@
�!@
��@
n�@
n�@
=q@
-@
�@
J@	��@	�@	�#@	��@	��@	�7@	G�@��@�`@Ĝ@�9@�9@�9@��@�u@�u@bN@ �@b@�;@��@�@l�@+@�@
=@�@�R@��@ff@5?@{@�@��@�-@p�@`B@�@�@��@�j@�j@�j@�j@�j@��@z�@z�@j@j@Z@9X@1@��@�
@�F@�F@�F@dZ@C�@C�@"�@�H@��@�!@��@~�@=q@-@-@-@-A�/A�/A�;dA�=qA�?}A�=qA�?}A�?}A�?}A�=qA�=qA�=qA�=qA�?}A�=qA�A�A�A�A�A�A�?}A�?}A�=qA�=qA�?}A�=qA�C�A�C�A�A�A�A�A�C�A�C�A�C�A�E�A�E�A�C�A�C�A�?}A�?}A�?}A�?}A�A�A�C�A�E�A�E�A�C�A�A�A�?}A�A�A�C�A�A�A�E�A�E�A�G�A�G�A�K�A�I�A�G�A�G�A�I�A�K�A�M�A�O�A�M�A�K�A�K�A�K�A�K�A�I�A�G�A�I�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�O�A�Q�A�Q�A�O�A�O�A�O�A�O�A�M�A�O�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�O�A�O�A�O�A�O�A�Q�A�M�A�E�A�E�A�E�A�G�A�G�A�G�A�E�A�E�A�C�A�C�A�A�A�A�A�A�A�C�A�A�A�E�A�G�A�G�A�I�A�K�A�I�A�K�A�M�A�Q�A�VA�`BA�bNA�l�A�t�A�v�A��A��A��A��A��7A��7A��7A��7A��+A��DA��DA��\A��hA���A���A��A�A�ȴA��
A��#A��A��
A���A���A���A���A���A�ƨA�A���A��^A��RA��^A��^A��9A��A���A���A���A���A���A���A���A��hA��PA��7A��DA��DA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��hA��\A��\A��PA��DA��7A��+A��A�~�A�~�A��A��A�~�A�~�A�|�A�z�A�|�A�~�A�~�A�|�A�x�A�x�A�z�A�z�A�|�A�x�A�t�A�r�A�p�A�n�A�n�A�n�A�n�A�l�A�jA�ffA�dZA�ffA�hsA�hsA�hsA�hsA�ffA�ffA�hsA�jA�hsA�hsA�dZA�bNA�\)A�5?A�{A��#A��uA�
=A��mA��A��DA���A�
=A�ffA��A�VA���A��`A���A���A�/A�G�A�9XA��A��!A�bNA��RA��FA��A�1A��-A���A�x�A�"�A���A�S�A��A��A�9XA��A���A�G�A�
=A��wA�|�A��7A��DA�x�A�\)A�;dA�%A��A�-A�A�A�A�A�?}A�?}A�=qA�?}A�A�A�?}A�7LA�33A�/A�-A�-A�$�A�"�A��A��A��A�A�  A���A��A�A�bA�-A�E�A�O�A�K�A�&�A�"�A�bA���A��/A�ƨA���A�^5A�
=A��
A��A��A�E�A�
=A���A��A��uA�$�A���A�ȴA��A���A�p�A�`BA�hsA��hA���A��;A��;A��/A���A�ĜA�A��-A���A���A���A���A��A�p�A�dZA�bNA�^5A�VA�M�A�/A��;A�jA�=qA�&�A��;A�ƨA�t�A��/A�=qA���A��mA�`BA���A�E�A��A�bNA�M�A�1'A�+A��;A���A���A���A��A�|�A�hsA�^5A�O�A�A�A�1'A�(�A�"�A��A�bA�
=A���A��A��mA��HA���A��;A���A�|�A�ĜA���A�"�A�5?A�`BA�v�A���A���A�r�A�C�A��A���A��!A�v�A�\)A�I�A�"�A��A�VA�A��A��FA��7A��+A�hsA�;dA�{A���A��;A��-A��A�p�A�dZA�K�A�;dA�-A� �A�VA��A��wA���A�r�A�Q�A�(�A��A��A��9A�x�A�E�A�=qA�5?A�%A���A��A�|�A�O�A�7LA�(�A��A��A��A��A�oA�  A��mA���A��wA��A���A��hA��A�x�A�t�A�n�A�bNA�Q�A�?}A�(�A���A���A���A�jA�G�A�&�A�oA��A�A���A�dZA�ȴA�A�ĜA���A���A���A��!A���A��DA�z�A�t�A�ffA�E�A��A��yA��FA�ffA��A��`A��wA��\A�S�A�-A��A��-A�O�A�&�A��A��hA�ffA�1'A���A��A�A�A���A��A���A�z�A�A�A��A��A�(�A��jA��A���A�x�A�v�A�n�A�bNA�G�A�E�A�A�A�C�A�7LA�&�A�|�A���A�S�A�A�hsA��A��wA��A���A�|�A�A�S�A��wA��`A�n�A�`BA�Q�A�M�A�;dA��A�VA�%A��A��A��yA��`A��`A��TA���A���A���A�ĜA��wA��jA��9A��A���A���A��DA��A�v�A�l�A�`BA�ZA�A�A�=qA�+A��AXA|�uAy�-AyoAx��Ax�Ax�Ax�Ax�HAx�jAxQ�Ax{Aw��Aw;dAvz�AvAu��At�As�;Ar�+ApjAn�`AnJAmK�Al��Al�Ak�FAj�Ai�#Ai��AiO�Ai
=Ah�!Ah-Ag�Ag�Ag`BAg7LAg
=Af�HAfȴAf��Af��Af~�Af1Ae�mAe�TAe�#Ae��Ae�
Ae��AeƨAeƨAe�-Ae��Ae��Ae��Ae�PAet�AeG�Ad��Ac��Ac�Ab��AbZAa�mAax�AaS�Aa33A`�jA`�A_A_A_�-A_��A_t�A_O�A_O�A_;dA^��A^9XA]p�A\~�A[��A[�AZ�AZffAZJAY�^AY��AY�AY�AX�uAXQ�AW��AW�hAW&�AU��AUXAUVAT��AT�+ATjAS�TAS7LAR�AR��AR1AQ�AP��AP9XAO�PAN�`ANAM;dALjAK�wAKt�AJ�AJ  AI��AIhsAIXAIC�AI"�AH��AH�AH5?AG�TAGAGAG�FAGx�AGVAF�yAFbNAE`BAD�yADr�AD5?AD(�AD�ADAC�#ACAC�FAC�AC?}AC�AC7LAD1AD �AD�AD1AC��AC�AC�AC��AC��AC+AB�AB�9ABM�AAdZA@ffA@1A?dZA>5?A=��A<jA<1A;��A;�#A;��A;��A;t�A;XA;"�A:��A:ffA9�
A8��A6�`A5�A533A5+A5�A5%A4��A4�!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                A�;dA�?}A�=qA�?}A�?}A�=qA�C�A�C�A�E�A�?}A�?}A�C�A�C�A�I�A�M�A�K�A�M�A�M�A�O�A�O�A�M�A�M�A�I�A�E�A�C�A�K�A�jA��A��PA�A���A��wA���A��hA���A���A���A���A��DA�~�A�|�A�x�A�n�A�ffA�ffA�I�A�
=A�S�A���A��7A��A��wA�p�A� �A���A�A���A���A�1A�dZA���A�;dA�r�A�^5A�M�A�C�A��yA�oA��hA��-A��\A�K�A�1'A��`A��wA�;dA��A�jA��TA���A�VAx��Au�TAmK�Ah�RAf��Ae�#Ae�hAb��A`JA^ �AZ�AW;dAS�AP(�AK"�AH�HAGXAD~�AC|�ADAB�uA=��A;|�A7��A4v�A1t�A0�`A1oA1`BA1�FA0bA.v�A-��A+"�A)\)A&�/A&�\A&ĜA&9XA%
=A#��A!%AG�A�AA�\Ap�A�9A�AbAA�A33AK�A�TA�wA�-A�A�A��A�!A�AS�Ax�AbA�A"�AS�A�A�A�
AM�A�mA�A�A�#Al�AJAoA�/A�jA�AA;dA��A=qA�;A?}A ��A �!A v�@��@���@�
=@��7@�1@�J@�  @�E�@��@��`@�w@�;d@�
=@�@�ƨ@�1@��@���@��@���@�^5@�F@�1'@䛦@��@��`@�V@�@�|�@�~�@�@�C�@�r�@�  @�9@�%@�ȴ@��@���@�@�E�@�^5@��@�7L@�ƨ@�b@�V@�G�@�~�@��@�9X@�b@�K�@�;d@�33@���@�n�@�M�@�^@���@�(�@��;@�+@�-@ݩ�@�?}@�V@��`@�Q�@�9X@�  @۝�@ڰ!@ٲ-@��@�33@���@���@�ff@թ�@�z�@��@�I�@�ƨ@�
=@�$�@�O�@�j@υ@�o@���@�ȴ@Ώ\@���@�o@�
=@�
=@���@Ο�@�ȴ@ΰ!@�V@�$�@��T@�V@��m@��@ʟ�@�5?@ɲ-@��/@�z�@Ǖ�@�;d@��@�ȴ@�{@�hs@�Ĝ@�bN@�1'@�  @þw@Õ�@�S�@�|�@�
=@��@�@��-@�&�@��/@���@�Z@� �@�t�@��@��!@���@�=q@��@��h@�O�@���@�9X@��;@�|�@�K�@�@�5?@��#@�@���@�&�@���@���@�S�@�+@��\@���@��@�  @���@�;d@���@�~�@��@���@���@���@���@�/@�A�@��@��@�@��H@�$�@�@�@��7@�hs@�hs@�X@�?}@�?}@�/@���@��D@�bN@�1'@��m@�ƨ@���@��H@�^5@���@�x�@�?}@��@���@�9X@���@�\)@�@��!@���@��^@�G�@��@���@�j@�9X@��;@�\)@��@���@��!@�M�@�$�@��^@��7@�`B@�G�@�%@���@�I�@��;@��@�ȴ@�v�@�J@��T@�hs@�?}@��/@�  @�\)@�;d@�33@��@��@�=q@��@���@�hs@�?}@�&�@��@���@�Ĝ@��@��m@�l�@�"�@���@��R@��R@�n�@�5?@��^@��h@��@�`B@��@��j@�A�@�1@��;@��F@�C�@��H@���@�n�@��@��-@���@��7@�`B@�&�@��j@��D@�j@�1@�|�@�+@��y@���@���@���@�M�@���@���@��-@�p�@�V@���@���@��D@�bN@� �@��w@��P@�\)@�@��R@�=q@���@��-@��h@�hs@��@��`@�Z@� �@���@���@�l�@�33@��@�
=@���@�v�@�$�@���@���@�`B@��@���@��`@��9@��D@�Z@�(�@���@��w@��F@�l�@�C�@�C�@��@��y@���@�V@�{@���@���@�p�@��@��9@��@�bN@�Q�@�A�@�1@~��@~E�@~{@}�T@}�T@}��@}�h@}O�@|��@|j@{�m@{t�@{C�@{33@{o@z��@z~�@zM�@z-@z�@y��@y��@yX@x�@w�@w�@v�+@vff@vE�@u�@u�h@u/@tj@t�@s��@s�F@s33@s"�@s@r�@r�H@r��@rn�@r-@rJ@q�@q��@qhs@qG�@p�9@pQ�@p1'@p  @o�@o|�@o\)@n�y@n��@nff@n{@m�-@m?}@m/@mV@l�@kS�@j�H@j��@j��@jn�@i�@i��@i%@h�u@h1'@g�@g|�@g
=@fff@f$�@e��@e`B@eV@d�@d1@ct�@b��@bn�@a�#@a��@a7L@`��@`Q�@_�;@_�P@^��@^5?@^{@]�@]�@\I�@[dZ@[@Z�!@Z�\@Z^5@Z-@Y��@X��@X�9@XQ�@W�w@W;d@Vv�@V@U��@U��@U`B@U�@T��@T�@T�@Tz�@T1@S��@So@R��@R�\@R�@Q��@Qhs@P��@PbN@O�w@Ol�@O+@N�y@N�R@Nv�@N@M@Mp�@M/@L�/@LI�@K��@Kt�@KS�@J��@J-@JJ@I�@I��@IG�@H�`@H�@Hb@G\)@G�@F�R@F�+@FV@F$�@E�-@E?}@D��@D�/@D��@D�@D��@DI�@C�
@C�@CC�@B�H@B��@B^5@A��@AX@A&�@@Q�@?��@?|�@?;d@>ȴ@>ff@>V@>E�@>@=�@<��@<�/@<��@;��@;�F@;�@;dZ@;"�@:�!@:M�@:=q@:J@9�#@9x�@9x�@9x�@9X@97L@8��@8r�@8 �@7\)@7�@6�@6ȴ@6V@5��@5`B@4��@4��@4z�@41@3ƨ@3��@3dZ@2�@2��@2n�@2J@1�^@1G�@0�`@0�9@0 �@/��@/|�@/�@.��@.ȴ@.�+@.5?@-�@-�T@-�-@-V@,j@,(�@+�m@+dZ@+o@*�@*�H@*��@*�\@*M�@*�@)��@)�7@)X@)%@(��@(Ĝ@(��@(A�@'�w@'�P@'l�@'K�@';d@'�@&�@&v�@&$�@%�@%�-@%O�@%V@$�j@$�@$�D@$Z@$(�@$1@#ƨ@#dZ@#C�@#33@#33@#"�@#@"�@"��@"^5@"J@!��@!�7@!X@ ��@ ��@ r�@   @��@�w@|�@+@�@ff@$�@�@�T@��@��@�@O�@/@�@�/@�D@I�@I�@9X@�@��@t�@"�@o@��@^5@=q@�@�#@��@�^@��@�7@&�@��@Q�@  @�;@�@l�@K�@�y@�R@$�@�T@�T@��@@p�@��@�@��@�D@9X@�m@��@S�@C�@o@�H@��@^5@�@��@��@�7@�7@7L@�`@��@��@�u@bN@Q�@b@��@�@|�@\)@;d@
=@�@ȴ@��@V@$�@�@��@@`B@V@�@z�@��@�m@ƨ@�@"�@@
�H@
��@
�!@
��@
n�@
n�@
=q@
-@
�@
J@	��@	�@	�#@	��@	��@	�7@	G�@��@�`@Ĝ@�9@�9@�9@��@�u@�u@bN@ �@b@�;@��@�@l�@+@�@
=@�@�R@��@ff@5?@{@�@��@�-@p�@`B@�@�@��@�j@�j@�j@�j@�j@��@z�@z�@j@j@Z@9X@1@��@�
@�F@�F@�F@dZ@C�@C�@"�@�H@��@�!@��@~�@=q@-@-@-G�O�A�/A�/A�;dA�=qA�?}A�=qA�?}A�?}A�?}A�=qA�=qA�=qA�=qA�?}A�=qA�A�A�A�A�A�A�?}A�?}A�=qA�=qA�?}A�=qA�C�A�C�A�A�A�A�A�C�A�C�A�C�A�E�A�E�A�C�A�C�A�?}A�?}A�?}A�?}A�A�A�C�A�E�A�E�A�C�A�A�A�?}A�A�A�C�A�A�A�E�A�E�A�G�A�G�A�K�A�I�A�G�A�G�A�I�A�K�A�M�A�O�A�M�A�K�A�K�A�K�A�K�A�I�A�G�A�I�A�K�A�M�A�M�A�M�A�M�A�M�A�M�A�O�A�O�A�M�A�M�A�K�A�M�A�M�A�M�A�M�A�M�A�O�A�Q�A�Q�A�O�A�O�A�O�A�O�A�M�A�O�A�M�A�O�A�O�A�O�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�K�A�M�A�M�A�O�A�O�A�O�A�O�A�Q�A�M�A�E�A�E�A�E�A�G�A�G�A�G�A�E�A�E�A�C�A�C�A�A�A�A�A�A�A�C�A�A�A�E�A�G�A�G�A�I�A�K�A�I�A�K�A�M�A�Q�A�VA�`BA�bNA�l�A�t�A�v�A��A��A��A��A��7A��7A��7A��7A��+A��DA��DA��\A��hA���A���A��A�A�ȴA��
A��#A��A��
A���A���A���A���A���A�ƨA�A���A��^A��RA��^A��^A��9A��A���A���A���A���A���A���A���A��hA��PA��7A��DA��DA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��hA��\A��\A��PA��DA��7A��+A��A�~�A�~�A��A��A�~�A�~�A�|�A�z�A�|�A�~�A�~�A�|�A�x�A�x�A�z�A�z�A�|�A�x�A�t�A�r�A�p�A�n�A�n�A�n�A�n�A�l�A�jA�ffA�dZA�ffA�hsA�hsA�hsA�hsA�ffA�ffA�hsA�jA�hsA�hsA�dZA�bNA�\)A�5?A�{A��#A��uA�
=A��mA��A��DA���A�
=A�ffA��A�VA���A��`A���A���A�/A�G�A�9XA��A��!A�bNA��RA��FA��A�1A��-A���A�x�A�"�A���A�S�A��A��A�9XA��A���A�G�A�
=A��wA�|�A��7A��DA�x�A�\)A�;dA�%A��A�-A�A�A�A�A�?}A�?}A�=qA�?}A�A�A�?}A�7LA�33A�/A�-A�-A�$�A�"�A��A��A��A�A�  A���A��A�A�bA�-A�E�A�O�A�K�A�&�A�"�A�bA���A��/A�ƨA���A�^5A�
=A��
A��A��A�E�A�
=A���A��A��uA�$�A���A�ȴA��A���A�p�A�`BA�hsA��hA���A��;A��;A��/A���A�ĜA�A��-A���A���A���A���A��A�p�A�dZA�bNA�^5A�VA�M�A�/A��;A�jA�=qA�&�A��;A�ƨA�t�A��/A�=qA���A��mA�`BA���A�E�A��A�bNA�M�A�1'A�+A��;A���A���A���A��A�|�A�hsA�^5A�O�A�A�A�1'A�(�A�"�A��A�bA�
=A���A��A��mA��HA���A��;A���A�|�A�ĜA���A�"�A�5?A�`BA�v�A���A���A�r�A�C�A��A���A��!A�v�A�\)A�I�A�"�A��A�VA�A��A��FA��7A��+A�hsA�;dA�{A���A��;A��-A��A�p�A�dZA�K�A�;dA�-A� �A�VA��A��wA���A�r�A�Q�A�(�A��A��A��9A�x�A�E�A�=qA�5?A�%A���A��A�|�A�O�A�7LA�(�A��A��A��A��A�oA�  A��mA���A��wA��A���A��hA��A�x�A�t�A�n�A�bNA�Q�A�?}A�(�A���A���A���A�jA�G�A�&�A�oA��A�A���A�dZA�ȴA�A�ĜA���A���A���A��!A���A��DA�z�A�t�A�ffA�E�A��A��yA��FA�ffA��A��`A��wA��\A�S�A�-A��A��-A�O�A�&�A��A��hA�ffA�1'A���A��A�A�A���A��A���A�z�A�A�A��A��A�(�A��jA��A���A�x�A�v�A�n�A�bNA�G�A�E�A�A�A�C�A�7LA�&�A�|�A���A�S�A�A�hsA��A��wA��A���A�|�A�A�S�A��wA��`A�n�A�`BA�Q�A�M�A�;dA��A�VA�%A��A��A��yA��`A��`A��TA���A���A���A�ĜA��wA��jA��9A��A���A���A��DA��A�v�A�l�A�`BA�ZA�A�A�=qA�+A��AXA|�uAy�-AyoAx��Ax�Ax�Ax�Ax�HAx�jAxQ�Ax{Aw��Aw;dAvz�AvAu��At�As�;Ar�+ApjAn�`AnJAmK�Al��Al�Ak�FAj�Ai�#Ai��AiO�Ai
=Ah�!Ah-Ag�Ag�Ag`BAg7LAg
=Af�HAfȴAf��Af��Af~�Af1Ae�mAe�TAe�#Ae��Ae�
Ae��AeƨAeƨAe�-Ae��Ae��Ae��Ae�PAet�AeG�Ad��Ac��Ac�Ab��AbZAa�mAax�AaS�Aa33A`�jA`�A_A_A_�-A_��A_t�A_O�A_O�A_;dA^��A^9XA]p�A\~�A[��A[�AZ�AZffAZJAY�^AY��AY�AY�AX�uAXQ�AW��AW�hAW&�AU��AUXAUVAT��AT�+ATjAS�TAS7LAR�AR��AR1AQ�AP��AP9XAO�PAN�`ANAM;dALjAK�wAKt�AJ�AJ  AI��AIhsAIXAIC�AI"�AH��AH�AH5?AG�TAGAGAG�FAGx�AGVAF�yAFbNAE`BAD�yADr�AD5?AD(�AD�ADAC�#ACAC�FAC�AC?}AC�AC7LAD1AD �AD�AD1AC��AC�AC�AC��AC��AC+AB�AB�9ABM�AAdZA@ffA@1A?dZA>5?A=��A<jA<1A;��A;�#A;��A;��A;t�A;XA;"�A:��A:ffA9�
A8��A6�`A5�A533A5+A5�A5%A4��A4�!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bw2Bv�Bv�Bv`Bw2Bw�Bw�Bv`Bv�BxBw�Bv�BwfBx8Bw�ByrBy�B{B{�B~(B~�B~�B�B�7B��B�B�B��BŢB�5B�GB�8B��B	oB	
�B	 B	uB	�B	+B	�B	kB	�B	�B	�B	B	'B	��B
��BDgBx�BZQB]dBwfB�B�FB��B��B՛B��B\]BPB�tB��B�<B��B�aB��B�tB�B��B}�Bh>BD3B
�)B
��B
��B
=�B
B	��B	��B	�B	�qB	�6B	zB	[WB	\�B	XB	T�B	\)B	MjB	OvB	@�B	;0B	'�B	7B	�B��B��B�|B�DB	 B	B��B�B�B��B��B�0B��B�qBǮB�yBרB��B�,B�B�B�>B	  B	�B		7B	�B	�B�B�;B�B�B�B	�B	bB	SB	�B	2�B	;dB	P�B	OB	<�B	C-B	TaB	e�B	y>B	�B	�B	�lB	x�B	M6B	L�B	OvB	PHB	B[B	6�B	�B	�B	xB	JB	PB	(B	VB	B	{B	fB	�B	xB		lB	"B	�B	�B	�B	�B	SB		B	~B	�B	�B	�B��B��B�|B�B�GB�|B�|B�B�xB	;B	1B	(B	�B	IB	"�B	�B	�B	 �B	�B	
=B	�B	B	�B	B	VB	;�B	=�B	<�B	.B	.IB	7�B	>BB	UgB	`vB	i�B	}"B	�B	�B	��B	��B	}�B	|�B	{�B	s�B	zxB	�B	�xB	�OB	�VB	��B	��B	��B	��B	��B	��B	�B	�tB	�B	�B	�$B	��B	�=B	�B	��B	�FB	��B	�B	��B	�B	��B	��B	��B	��B	�B	�LB	�LB	��B	��B	��B	��B	��B	��B	�HB	�wB	��B	��B	�OB	��B	��B	��B	�#B	�<B	ΥB	�HB	�B	� B	�[B	ӏB	��B	��B	��B	�&B	҉B	ӏB	�&B	�,B	��B	��B	��B	��B	�,B	��B	՛B	՛B	�9B	��B	��B	�?B	��B	רB	�yB	ںB	��B	ܒB	��B	�|B	�vB	�B	�vB	��B	�TB	��B	��B	�BB	��B	�B	�fB	��B	��B	��B	�fB	��B	�fB	�2B	�B	�B	��B	�8B	�8B	��B	�yB	�DB	�sB	�B	�B	�mB	�8B	��B	�B	�
B	��B	�B	�B	�yB	�DB	�B	��B	�]B	�;B	�iB	��B	�iB	�B	�|B	�|B	��B	��B	�`B	��B	�B	�PB	�"B	�]B
  B
B
B
uB
uB
AB
AB
�B
B
AB
B
uB
B
�B
B
SB
SB
YB
%B
�B
_B
�B
_B
_B
�B
�B
�B
	�B
	�B
	�B
	�B

rB

rB
B
DB
DB
DB
DB
�B
JB
�B
"B
�B
�B
�B
�B
�B
(B
�B
:B
�B
�B
�B
�B
@B
{B
FB
B
�B
SB
�B
�B
�B
�B
�B
�B
eB
7B
�B
�B
�B
�B
�B
�B
B
B
B
�B
B
~B
~B
~B
�B
�B
 'B
�B
 \B
 'B
�B
 �B
!bB
!bB
!�B
"hB
"hB
"4B
#nB
#nB
#�B
$�B
%FB
%�B
%�B
&B
&�B
&�B
&�B
'RB
'�B
($B
($B
($B
($B
(�B
(�B
)_B
)�B
)�B
*0B
,B
,B
,=B
,=B
,=B
,�B
,�B
-wB
-B
-�B
.}B
.�B
/B
/B
/OB
/�B
0!B
0�B
1[B
0�B
1�B
1�B
2-B
2-B
2�B
2aB
2�B
2�B
3hB
3�B
3hB
4B
3�B
3�B
4B
4B
4nB
5?B
5?B
6B
6zB
6�B
8RB
8�B
8�B
8�B
8�B
8�B
9$B
9XB
8�B
9$B
9$B
8�B
9$B
9$B
9XB
:�B
<6B
<�B
=�B
=qB
=�B
=�B
>�B
?B
>�B
>�B
>�B
?B
?B
?HB
@�B
A�B
A�B
B[B
B'B
B[B
B�B
B�B
C-B
C�B
C�B
C�B
C�B
C�B
C�B
EB
EB
E9B
EmB
E�B
FB
FB
E�B
E�B
EmB
F�B
GEB
GB
GB
GB
GzB
GEB
GzB
G�B
GzB
GzB
G�B
HKB
G�B
G�B
GEB
G�B
IB
H�B
H�B
H�B
JXB
JXB
J�B
K)B
K)B
K^B
K�B
K�B
L0B
L�B
L�B
L�B
M6B
M6B
MB
M�B
L�B
M6B
MB
M�B
N<B
O�B
PHB
P�B
P�B
P�B
Q�B
RTB
R B
R B
RTB
T,B
S�B
TaB
TaB
TaB
T�B
T�B
U�B
VB
U�B
U�B
UgB
VmB
W?B
WsB
WsB
WsB
W�B
XB
XEB
XEB
XyB
X�B
YKB
Y�B
ZQB
ZQB
ZQB
Z�B
[#B
[WB
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
]dB
]�B
^B
^�B
^�B
_B
_pB
_;B
`vB
`vB
`�B
`vB
`vB
aHB
aHB
a�B
a�B
b�B
b�B
c B
b�B
c B
cTB
c�B
c�B
c�B
d&B
c�B
d&B
c�B
dZB
d�B
d�B
d�B
e`B
e`B
e�B
f�B
ffB
f2B
gB
f�B
gB
gB
gmB
g�B
g�B
g�B
h>B
h�B
h�B
h>B
iB
h�B
iB
iB
iDB
iDB
jB
i�B
i�B
i�B
jKB
jB
jB
jKB
jKB
jB
kQB
kQB
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o B
oiB
o�B
oiB
o�B
poB
p;B
p�B
p�B
p�B
q�B
qvB
q�B
rGB
rGB
r�B
sB
r�B
sB
sMB
s�B
s�B
s�B
s�B
t�B
t�B
t�B
uZB
v+B
v+B
v+B
v+B
v`B
v�B
v�B
v�B
wfB
wfB
wfB
w�B
w�B
w�B
w�B
x8B
x�B
x�B
y	B
y	B
y>B
y>B
yrB
zB
zDB
zDB
zxB
{B
{B
{�B
{B
{�B
|B
|B
|B
|�B
}"B
}"B
}"B
}"B
}VB
}VB
}VB
}�B
~(B
~(B
~]B
~�B
~�B
�B
�B
� B
�iB
�iB
�iB
��B
��B
�B
�oB
�oB
��B
��B
��B
�B
��B
�GB
�GB
�{B
��B
�MB
�MB
�MB
��B
�MB
��B
�SB
�SB
�B
��B
�%B
�%B
�YB
��B
��B
��B
��B
��B
�_B
��B
�1B
�fB
�1B
��B
��B
�B
�B
�B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�B
�xB
��B
��B
��B
�B
�JB
�JB
�JB
�~B
��B
�PB
�PB
�PB
�PB
��B
�"B
�VB
�VB
�VB
��B
�VB
��B
�(B
�(B
��B
��B
��B
�.B
�.B
�.B
�bB
� B
�4B
�:B
�:B
�:B
�:B
�oB
�:B
��B
�uB
��B
��B
��B
�FB
�{B
�{B
�{B
�FB
�{B
�{B
�{B
��B
��B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
�$B
�YB
�YB
�YB
�YB
�YB
�YB
��B
��B
�+B
�_B
�_B
��B
��B
��B
�eB
��B
�B
�B
�7B
�kB
��B
��B
��B
�	B
�qB
�qB
�qB
��B
�B
�xB
�xB
�xB
�xB
�xB
�xB
��B
��B
��B
��B
��B
��B
�B
�~B
�~B
��B
��B
�B
�OB
��B
��B
��B
�VB
��B
��B
�'B
�'B
��B
��B
��B
��B
��B
��Bv�Bv�BwfBwfBv�Bv�Bu�Bv�Bv�Bw2Bv�Bv�Bv�Bv+Bv�Bv+Bv�Bv�BwfBwfBw2Bw�Bw�Bw�By	Bw�Bw�Bv+Bv`Bv�Bu�Bv�Bv�Bw2BwfBxBw�BxlBxBv�Bv�Bv+Bv�Bw�Bw�BxlBwfBwfBw�BwfBv�Bv�Bv�BxBx8Bx�Bx�By	Bx8Bw2Bv�BxlBwfBwfBwfBw�By	BzDBzDBz�By	Bx�By	ByrBy	By�Bx�B{B{JB{�B}�B}"B|�Bx�By>ByrBz�Bz�B{B{�B|�B}VB}�B~]B}VB~�B~(B}�B~(B}�B}�B~(B~]B~�B~�BcB�B�B~�B~(B}�B}�B}VB}"B~�B��B�+B��B��B�_B��B��B�fB�7B�	B��B��B�FB��B��B�B��B�_B�YB�IB��B��B�VB��B�tB�*B��B�B��B�^B�B��B��B��B�'B�UB��BB�3BŢBƨB�tB�KB��B̘BӏB��B�B�B��B��B��B�vB�GB��B��B�B��B��B��B�DB�DB�	B�>B��B��B��B�]B	 4B��B��B	 �B	;B	B	�B	�B	B	�B�]B	�B	~B	VB	�B	.B	bB	�B	�B	�B	oB	�B	uB	@B	uB	B	@B	�B	�B	�B	B	MB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	1B	�B	+B	�B	1B	B	kB	kB	eB	7B		B	B	CB	B	�B	�B	~B	�B	B	~B	xB	�B	�B	�B	�B	�B	CB	qB		B	kB	B	�B	kB	�B	xB	�B	�B	B	�B	qB	�B	B	VB	YB	�B	+B	+B	33B	`�B	�+B	�tB	�zB
�B
K�B
O�B
OvB
LdB
E�B
Z�B
��B
��B
��BBU�B8�B6zBj�B�(B��Bp;BpoBw�Br|Bp�Bd&BcTBV�BR�BS�BVB]�B\�BbNBZQBY�B_�BcTBb�Bc�Be�BsBUgB~�B�B�iB��B��B� BcB�iB.B�B.B}�B|PB}VB~(B.B}"B|�B~�B|�B~�Bx8Bv�B~�B��B��B��B��B�eB��B��B��B��B��B��B��B�XB��B�bB��B�LB��B��B�!B��B��B��B��B��B��B�B��B�OB�B�BΥB��B��B՛BѷB�
B�sB��B�?B�mBרB��B�#B��B��BӏB��B�,B��B�]B��BуB�BB��B�dB��B��B��B��B��B�eB�-B�B��Bt�ByrBq�Bo�Bv�Bm�Bd�B`�BbB`�BZ�BZBW?BYBVmBV9BS�BU�BS�BPHBR�BOBBQ�BN<BMjBM6BQ�Ba|B�4B��B��B��B�hB�RB�B�B�mB�mB�jB�[B͟B�aB�B��BB��B��B��B��B�B��B�LB��B��B�dB��B��B��B�B��B�?B�tB��B��B��B�B��B�tB�B�'B�'B�nB�B�B��B��B�OB�0B�'B�B��B�RB��B�B��B��B��B�aB�9B�B��B��B�^B��B��B�FB�?B��B��B��B��B�!B��B�[B�!B��B��B��B��B�-B��B��B��B�B��B�xB��B��B�Bm)Bf2Bd�Be`BkQBl�BlWBiBf�BgmBh>Bh
Bd�Bf�BbNBX�BUgBOBBO�BHKBCaBA�B@OB2�B!bB'�B!-B	�B�B  B
��B
��B�B
�9B
ÖB
��B
��B
��B
�B
�@B
��B
�+B
�\B
�B
��B
�	B
�+B
��B
�B
~�B
|PB
y>B
xlB
�B
�1B
e`B
Q�B
S�B
W�B
I�B
6B
3�B
.}B
-�B
;0B
5?B
'�B
$B

	B	�(B
�B
B
 �B
�B
�B	�]B
�B	��B	�PB	��B	��B	�B	�]B	�xB	�rB	�B	�>B	��B	�>B	��B	�B	��B	��B	��B	�B	�B	�B	�/B	�B	�B	�mB	��B	�QB
($B	��B	��B	��B	��B	�?B	�B	��B	��B	��B	��B	�B	�3B	�OB	�B	��B	�zB	��B	�3B	��B	��B	y>B	rB	kB	iB	e,B	y>B	aHB	YB	]dB	[WB	ZB	]�B	X�B	Z�B	^5B	[�B	\�B	]/B	]/B	]�B	Y�B	]�B	e,B	XB	W
B	W
B	W�B	UgB	V�B	V�B	T�B	W?B	U�B	R�B	T,B	S�B	S[B	RTB	\]B	e,B	Z�B	\�B	]�B	^�B	U�B	P�B	N�B	W
B	NpB	M�B	DgB	IRB	IRB	LdB	LdB	H�B	OBB	OBB	U�B	M6B	VB	I�B	DgB	A�B	>B	B'B	AUB	=�B	:�B	C-B	@OB	9�B	<jB	5?B	8�B	M�B	/B	/�B	+�B	&�B	%�B	-�B	($B	�B	 \B	 'B	 �B	qB	�B	CB	�B	�B	~B		�B	B	AB	B�B��B�B��B�B�B��B��B��B�B�oB�cB�B��B��B��B��B	B�%B��B��B��B�B��B��B��B�B��B�B�2B��B�B	�B	�B	�B	uB	�B	4B	�B	�B	B	�B	1B	+B	&�B	�B	�B	
�B	�B�B�2B��B��BخB�mB�sB��B҉B��B҉B��B�B�}B�B��B��B��B��B�XB��B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2021021312242520210213122425IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022311003920210223110039QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022311003920210223110039QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014420210427140144IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20220504162926              CF      PSAL                            ?�  G�O�D���G�O�?�  G�O�Sensor Failure                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20220504163438    CF                  PSAL            G�O�?\)G�O�CG��G�O�?�                  Sensor Failure  SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                