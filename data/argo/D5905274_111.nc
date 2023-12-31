CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-01-18T08:20:53Z creation; 2023-04-26T19:24:31Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20210118082053  20230426192431  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               o   oAA  AOAO7315_008643_111                 7315_008643_111                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�W�I�l�@�W�I�l�11  @�W��6z@�W��6z@1�o�5�J@1�o�5�J�e��1e��e��1e�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?��H@@  @�G�@�G�@��R@�  AG�A  A{A,(�A@��A^{A~{A�  A�  A��A�\)A�  A��A�  A��B�
B(�B  B (�B((�B/�
B7�B@  BH(�BO�
BW�
B_�
Bg�Bp  Bw�
B�  B�  B��B�  B��B��
B��
B�{B�(�B�{B��B�{B�(�B��
B�  B�(�B�{B�{B��B��
B�  B�  B��
B�  B�{B�  B��B�  B�(�B�(�B�{B��C   C  C{C�C  C	�HC�C��C�C��C��C��C��C��C{C  C   C"  C#��C&  C'��C)��C,  C.
=C0  C2  C4  C6  C8  C:  C<
=C>
=C@
=CA��CC�CF  CH  CJ
=CL
=CN  CP  CR
=CT  CV  CX  CZ  C\  C]��C`  Cb
=Cd
=Cf  Cg��Ci�Cl  Cn
=Cp
=Cr
=Ct  Cv  Cx  Cy��C|  C}��C��C���C�  C�
=C�  C�  C�C�  C���C�  C���C�  C�C�  C�  C�  C�C���C�  C�
=C�
=C�
=C���C���C�C�C���C���C�  C���C���C�C�
=C�  C���C�  C���C�  C�
=C�
=C���C���C�C�  C��C���C�C�\C�  C���C�
=C���C���C���C���C�C�  C�
=C�
=C�
=C�  C���C���C�  C�C�C�  C�  C�  C���C���C�  C���C�  C�C�  C���C���C�  C�  C���C���C�  C�  C�  C�C�
=C�
=C�  C�  C�  C�  C���C���C���C���C�
=C�{C�\C�C�  C�C�C���C���C�  C�C�C���C���C���C�C�
=C�  C�C�
=C�C���C�C�
=C�C���C���C���C�  C�C�
=C�  D �D �D �qD}qD�D� D�qD�D�D��D
=D� D�qD�D�qD}qD  D� D	�D	� D
  D
�D  D�D  D� D  D� D�qD��D�qDz�D�qD}qDD� D�qD� D�D�D�D� D  D��D�qD}qDD�D�D�D  D��D  Dz�D�qD}qD�qD��D�qD}qD  D� D  D�D �D ��D!D!��D"�D"��D"�qD#� D$�D$��D%D%��D%�qD&}qD'�D'��D(�D(�D)�D)��D*�D*��D+�D+��D,D,�D-D-�D.�D.��D/D/��D/�qD0z�D0�qD1}qD2  D2� D2��D3z�D4  D4��D5  D5}qD6  D6��D7  D7� D8D8}qD8�qD9� D:  D:��D;D;��D;�qD<}qD=  D=��D>�D>� D?  D?��D@�D@��DA�DA}qDB  DB��DC  DCz�DC�qDD}qDE  DEz�DE��DF}qDG  DG� DH�DH�DIDI��DJDJ��DK�DK�DL�DL��DL�qDM� DN  DN� DN�qDO}qDO�qDP}qDQ  DQ��DR�DR� DS�DS��DTDT�DT�qDUz�DV  DV��DW  DW� DX�DX� DX��DY��DZ  DZ��D[�D[}qD[�qD\� D]  D]� D^�D^� D_�D_}qD_��D`��Da�Daz�Db  Db}qDc  Dc}qDd  Dd��De�De� Df  Df� Dg  Dg� Dh  Dh��Di�Di��Dj�Djz�Dk  Dk� Dk��Dl��DmDm��Dn  Dn��DoDo� Dp  Dp� Dp�qDq� Dq�qDr� Ds�Dsz�Ds�qDt}qDt�qDu�Dv�Dv��Dw�Dw��Dx�Dx��Dy  Dy� Dz�Dz��Dz�qD{��D{�qD|� D}�D}}qD}��D~}qDD� D�qD�=qD�~�D���D�  D�B�D�~�D���D��D�B�D��HD��HD�  D�@ D��HD��HD�HD�@ D�}qD���D�HD�@ D���D��HD�  D�AHD�� D���D�  D�@ D��HD�D�  D�=qD�~�D�� D���D�@ D���D��HD�  D�@ D�~�D�� D�  D�@ D��HD�D�HD�@ D��HD���D���D�>�D�� D�� D�  D�@ D�~�D��HD�  D�>�D�~�D��HD�  D�<)D�~�D��HD�HD�@ D�}qD���D�HD�@ D��HD��HD�  D�>�D�}qD�� D��D�@ D�~�D�� D���D�@ D�~�D��)D���D�>�D�}qD�� D��D�B�D��HD�D��D�B�D��HD�� D�  D�@ D��HD�D��D�B�D��HD�� D���D�@ D�~�D��qD���D�>�D�}qD��qD���D�AHD���D��HD�  D�AHD���D���D�HD�AHD�~�D��qD�  D�@ D�� D�D��D�AHD��HD�� D�  D�>�D�}qD��qD�  D�@ D�� D���D��D�AHD��HD��HD��qD�=qD�� D��qD��qD�>�D��HD��HD�  D�AHD�|)D���D�HD�AHD��HD�D��D�AHD�}qD��HD��D�@ D��HD��HD��D�C�D�� D��qD���D�@ D�}qD���D��qD�AHD�� D��qD�  D�>�D�|)D���D��D�@ D�~�D�� D��qD�@ D��HD��HD��D�@ D�~�D�� D�  D�>�D��HD��qD�HD�AHD�� D�� D��)D�@ D�� D��HD�HD�=qD�� D��HD���D�=qD�� D�� D��)D�@ D��HD���D�  D�B�D�� D�� D��D�@ D�}qD���D���D�@ D�~�D��HD�HD�>�D���D���D�HD�@ D�~�D���D�HD�B�D��HD�� D���D�>�D�~�D�� D�HD�C�D��HD�� D���D�@ D��HD��HD�  D�>�D�� D�� D�HD�@ D�~�D�� D�  D�>�DÀ D��HD�  D�>�DāHD��HD��D�AHD�~�D�� D��D�AHDƁHD�� D���D�@ DǁHD��HD�HD�@ DȀ D��HD�HD�>�D�}qDɾ�D�  D�AHDʀ Dʾ�D�  D�AHD˂�D�� D���D�=qD�~�D��HD��D�B�D́HD��HD�HD�AHD΁HD��HD��D�B�Dς�D�� D�  D�@ DЀ D��HD�HD�AHDр DѽqD��qD�@ DҀ DҾ�D�  D�<)D�|)DӽqD��qD�>�DԁHD�D�HD�AHDՂ�D�D�  D�=qDր D־�D�HD�AHD׀ D��HD���D�@ D؀ Dؾ�D�  D�AHDـ D�� D�HD�AHD�~�D�� D�  D�>�DہHD��HD�  D�@ D�~�Dܾ�D�HD�@ D�~�D�� D��qD�>�D�~�D޾�D�  D�>�D߂�D߽qD���D�AHD�~�D�� D�  D�@ DႏDᾸD���D�>�D� D�� D���D�AHD� D�� D�HD�AHD�~�D��HD�  D�=qD�~�D�� D���D�B�D�HD��HD�  D�AHD�HD�D�HD�AHD� D�� D�  D�=qD� D�� D��qD�@ D� D꾸D���D�=qD� D뾸D�  D�>�D�}qD�� D�  D�=qD� D��HD�HD�AHD�~�D��HD�HD�>�D�~�DﾸD�  D�AHD�� D��HD�  D�@ D�HD�� D�HD�AHD�HD�� D�  D�@ D� D�� D�HD�>�D�~�D�D�  D�>�D�� D�� D��D�AHD�� D���D��D�AHD���D�� D�HD�@ D�}qD��qD��qD�>�D�}qD���D��D�9�?\)?��?B�\?�  ?�=q?�33?��?�@�@z�@+�@:�H@G�@Tz�@k�@z�H@�ff@�\)@�Q�@��R@��@�{@�@��R@��
@�=q@�z�@ٙ�@�p�@��@���@�@��RA�
A
=A�A\)A�AQ�A(�A   A$z�A(Q�A,(�A/\)A333A6ffA;�A>�RAB�\AFffAI��AN�RAQ�AUAY��A]p�AaG�Ag
=Ak�AqG�AvffA{�A�  A��\A��A�Q�A�33A�p�A�  A��\A�A���A�33A�ffA���A��
A��RA���A��
A�ffA���A��A�{A�Q�A��HA��A��A���A��
A�{A���A˅A�{A�Q�A�33A��A�\)A��A�(�A�ffA���A��HA��A�\)A��A�A�p�A�\)A��A�z�A��RA���A�33A�p�A��B ��B�B�HB  Bp�B�RB  B	G�B
�RB  Bp�B�RB(�BG�B�RB�
B��B=qB�B�BffB�B��B�B
=B (�B!p�B"�\B#�B$��B&=qB'\)B(z�B)B+
=B,Q�B-��B.�\B0  B0��B1�B3
=B4(�B4��B6=qB733B8Q�B9�B:=qB;
=B<(�B<��B>=qB?
=B@(�B@��BA�BB�HBC�BDz�BEp�BF=qBG33BG�
BH��BI��BJ=qBK
=BK�
BLz�BMp�BNffBO33BPQ�BQ�BR{BS33BTQ�BUG�BV=qBW�BX��BZ=qB[33B\z�B]p�B^�\B_�
B`��Bb{Bb�HBd  BeG�Bf=qBg�Bh��BiBk
=Bl(�Bmp�Bn�RBo�
Bq�Br=qBs�Bt��Bu�Bv�HBx  ByG�Bz=qB{\)B|��B}�B33B�{B��RB�\)B�  B��\B��B��B�Q�B��HB��B�{B���B�G�B��
B�Q�B��HB�p�B��B�ffB�
=B��B�{B��\B�33B�B�=qB���B�\)B��B�z�B���B���B�(�B��RB�G�B��B�z�B�
=B���B�=qB���B�\)B��B�z�B�
=B���B�(�B��HB���B�{B��\B�33B��
B�z�B��B���B�(�B��RB�33B��
B�Q�B��HB��B�  B��\B��B��B�=qB���B�\)B��B�ffB�
=B���B�(�B���B�G�B��
B�Q�B��HB�p�B�  B��\B�
=B���B�{B���B�33B��B�(�B��RB�33B��
B�Q�B��HB�p�B�{B��\B�
=B��B�=qB��RB�\)B��B�z�B�
=B���B�(�B��RB�33B��
B�Q�B���B�G�B��
B�ffB�
=B��B�{B£�B�33B��
B�ffB���BŅB�(�BƸRB�G�B��
B�Q�B�
=Bə�B�(�BʸRB�\)B��
B�z�B���B͙�B�=qB��HBυB�{BиRB�G�B��B�ffB���B�\)B�{Bԣ�B��Bՙ�B�=qB���B�\)B��
B�z�B���Bٙ�B�{BڸRB�G�B�B�Q�B���B�\)B��B�z�B�
=Bߙ�B�=qB���B�G�B�B�ffB���B�B�{B�z�B�
=B�B�  B�\B�
=B癚B�{B��B�
=B陚B�{B�\B�
=B�B�=qB��HB�p�B��B�ffB���B�B�  B��\B���B�\)B��
B�ffB��HB�p�B��
B�z�B���B�p�B��B�ffB��HB�p�B��B�ffB�
=B���B�{B���B�G�B��B�(�B��RB�G�B��
B�z�B�
=B��
C 33C z�C ��C{CQ�C�C��C{C\)C��C��CQ�C��C�HC33C�CC��CG�C��C�HC33C��C��CG�C��C�HC�Cp�CC	
=C	ffC	��C
�C
ffC
�RC
=C\)C�C�C33C�C�
C33C��C�CG�C�\C�HC33C�\C�HC=qC�\C  CffC�C  CQ�C�C{Cz�C�
C33Cz�CC�C�C�CG�C��C�C33C�\C�HCQ�C�C  CG�C��C�
C{C\)C�\C��C
=CG�Cz�C��C�RC��C�HC��C�CQ�CffC�C��C�RC�
C�HC�C  C�C33CG�Cz�C��CC��C��C�C��C(�CQ�CffC�C��CCC��C�C
=C�C=qCQ�C�C��CCC�
C��C 
=C G�C \)C z�C �\C ��C �RC C ��C!�C!=qC!Q�C!p�C!�\C!�\C!�C!��C!�HC!��C"�C"=qC"p�C"�\C"�C"��C"�
C"�C#  C#�C#=qC#ffC#��C#�RC#C#�HC$  C$�C$\)C$z�C$��C$��C$��C$�HC%
=C%G�C%ffC%�\C%��C%�RC%�
C%��C&33C&\)C&z�C&�\C&��C&C&�HC'
=C'(�C'\)C'�C'��C'��C'�HC'�C(
=C(33C(G�C(p�C(��C(C(�C)  C)
=C)(�C)G�C)ffC)��C)C)�HC)�C*  C*(�C*\)C*z�C*��C*��C*��C*�C+(�C+G�C+ffC+z�C+��C+�RC+��C,{C,G�C,\)C,ffC,�C,�C,�C-
=C-33C-33C-\)C-p�C-�RC-�
C-��C.  C.(�C.Q�C.p�C.�C.��C.�HC.��C/{C/=qC/\)C/�C/��C/�
C0
=C0(�C0Q�C0\)C0z�C0�C0�HC1
=C1{C133C1G�C1�\C1�RC1��C1�HC2  C2=qC2ffC2�\C2��C2�RC2�HC3{C333C3=qC3\)C3�\C3C3�HC3��C4
=C4Q�C4z�C4�C4��C4�C5  C5(�C533C5G�C5p�C5�C5�
C5�C6  C6{C6\)C6�C6��C6��C6��C7  C733C7Q�C7ffC7z�C7�C7�HC8  C8
=C8(�C8Q�C8�\C8�C8�RC8�
C8��C9(�C9\)C9�C9�\C9�RC9��C:{C:33C:33C:\)C:��C:��C:�C:��C;{C;Q�C;z�C;��C;��C;��C<
=C<33C<G�C<\)C<�C<C<�HC<��C={C=\)C=�C=�\C=�C=�HC>{C>=qC>G�C>ffC>��C>��C>��C?  C?�C?ffC?z�C?��C?C?��C@
=C@(�C@\)C@��C@�RC@��C@�CA(�CA\)CA\)CAz�CACA�CB  CB{CBQ�CB�CB�\CB�RCC  CC(�CCG�CCffCC��CC�HCD{CD(�CD\)CD�\CD�CD�
CE�CE\)CEffCE��CE��CF�CF33CFffCF�RCF��CG  CG33CG�CG�RCG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                               ?u?��H@@  @�G�@�G�@��R@�  AG�A  A{A,(�A@��A^{A~{A�  A�  A��A�\)A�  A��A�  A��B�
B(�B  B (�B((�B/�
B7�B@  BH(�BO�
BW�
B_�
Bg�Bp  Bw�
B�  B�  B��B�  B��B��
B��
B�{B�(�B�{B��B�{B�(�B��
B�  B�(�B�{B�{B��B��
B�  B�  B��
B�  B�{B�  B��B�  B�(�B�(�B�{B��C   C  C{C�C  C	�HC�C��C�C��C��C��C��C��C{C  C   C"  C#��C&  C'��C)��C,  C.
=C0  C2  C4  C6  C8  C:  C<
=C>
=C@
=CA��CC�CF  CH  CJ
=CL
=CN  CP  CR
=CT  CV  CX  CZ  C\  C]��C`  Cb
=Cd
=Cf  Cg��Ci�Cl  Cn
=Cp
=Cr
=Ct  Cv  Cx  Cy��C|  C}��C��C���C�  C�
=C�  C�  C�C�  C���C�  C���C�  C�C�  C�  C�  C�C���C�  C�
=C�
=C�
=C���C���C�C�C���C���C�  C���C���C�C�
=C�  C���C�  C���C�  C�
=C�
=C���C���C�C�  C��C���C�C�\C�  C���C�
=C���C���C���C���C�C�  C�
=C�
=C�
=C�  C���C���C�  C�C�C�  C�  C�  C���C���C�  C���C�  C�C�  C���C���C�  C�  C���C���C�  C�  C�  C�C�
=C�
=C�  C�  C�  C�  C���C���C���C���C�
=C�{C�\C�C�  C�C�C���C���C�  C�C�C���C���C���C�C�
=C�  C�C�
=C�C���C�C�
=C�C���C���C���C�  C�C�
=C�  D �D �D �qD}qD�D� D�qD�D�D��D
=D� D�qD�D�qD}qD  D� D	�D	� D
  D
�D  D�D  D� D  D� D�qD��D�qDz�D�qD}qDD� D�qD� D�D�D�D� D  D��D�qD}qDD�D�D�D  D��D  Dz�D�qD}qD�qD��D�qD}qD  D� D  D�D �D ��D!D!��D"�D"��D"�qD#� D$�D$��D%D%��D%�qD&}qD'�D'��D(�D(�D)�D)��D*�D*��D+�D+��D,D,�D-D-�D.�D.��D/D/��D/�qD0z�D0�qD1}qD2  D2� D2��D3z�D4  D4��D5  D5}qD6  D6��D7  D7� D8D8}qD8�qD9� D:  D:��D;D;��D;�qD<}qD=  D=��D>�D>� D?  D?��D@�D@��DA�DA}qDB  DB��DC  DCz�DC�qDD}qDE  DEz�DE��DF}qDG  DG� DH�DH�DIDI��DJDJ��DK�DK�DL�DL��DL�qDM� DN  DN� DN�qDO}qDO�qDP}qDQ  DQ��DR�DR� DS�DS��DTDT�DT�qDUz�DV  DV��DW  DW� DX�DX� DX��DY��DZ  DZ��D[�D[}qD[�qD\� D]  D]� D^�D^� D_�D_}qD_��D`��Da�Daz�Db  Db}qDc  Dc}qDd  Dd��De�De� Df  Df� Dg  Dg� Dh  Dh��Di�Di��Dj�Djz�Dk  Dk� Dk��Dl��DmDm��Dn  Dn��DoDo� Dp  Dp� Dp�qDq� Dq�qDr� Ds�Dsz�Ds�qDt}qDt�qDu�Dv�Dv��Dw�Dw��Dx�Dx��Dy  Dy� Dz�Dz��Dz�qD{��D{�qD|� D}�D}}qD}��D~}qDD� D�qD�=qD�~�D���D�  D�B�D�~�D���D��D�B�D��HD��HD�  D�@ D��HD��HD�HD�@ D�}qD���D�HD�@ D���D��HD�  D�AHD�� D���D�  D�@ D��HD�D�  D�=qD�~�D�� D���D�@ D���D��HD�  D�@ D�~�D�� D�  D�@ D��HD�D�HD�@ D��HD���D���D�>�D�� D�� D�  D�@ D�~�D��HD�  D�>�D�~�D��HD�  D�<)D�~�D��HD�HD�@ D�}qD���D�HD�@ D��HD��HD�  D�>�D�}qD�� D��D�@ D�~�D�� D���D�@ D�~�D��)D���D�>�D�}qD�� D��D�B�D��HD�D��D�B�D��HD�� D�  D�@ D��HD�D��D�B�D��HD�� D���D�@ D�~�D��qD���D�>�D�}qD��qD���D�AHD���D��HD�  D�AHD���D���D�HD�AHD�~�D��qD�  D�@ D�� D�D��D�AHD��HD�� D�  D�>�D�}qD��qD�  D�@ D�� D���D��D�AHD��HD��HD��qD�=qD�� D��qD��qD�>�D��HD��HD�  D�AHD�|)D���D�HD�AHD��HD�D��D�AHD�}qD��HD��D�@ D��HD��HD��D�C�D�� D��qD���D�@ D�}qD���D��qD�AHD�� D��qD�  D�>�D�|)D���D��D�@ D�~�D�� D��qD�@ D��HD��HD��D�@ D�~�D�� D�  D�>�D��HD��qD�HD�AHD�� D�� D��)D�@ D�� D��HD�HD�=qD�� D��HD���D�=qD�� D�� D��)D�@ D��HD���D�  D�B�D�� D�� D��D�@ D�}qD���D���D�@ D�~�D��HD�HD�>�D���D���D�HD�@ D�~�D���D�HD�B�D��HD�� D���D�>�D�~�D�� D�HD�C�D��HD�� D���D�@ D��HD��HD�  D�>�D�� D�� D�HD�@ D�~�D�� D�  D�>�DÀ D��HD�  D�>�DāHD��HD��D�AHD�~�D�� D��D�AHDƁHD�� D���D�@ DǁHD��HD�HD�@ DȀ D��HD�HD�>�D�}qDɾ�D�  D�AHDʀ Dʾ�D�  D�AHD˂�D�� D���D�=qD�~�D��HD��D�B�D́HD��HD�HD�AHD΁HD��HD��D�B�Dς�D�� D�  D�@ DЀ D��HD�HD�AHDр DѽqD��qD�@ DҀ DҾ�D�  D�<)D�|)DӽqD��qD�>�DԁHD�D�HD�AHDՂ�D�D�  D�=qDր D־�D�HD�AHD׀ D��HD���D�@ D؀ Dؾ�D�  D�AHDـ D�� D�HD�AHD�~�D�� D�  D�>�DہHD��HD�  D�@ D�~�Dܾ�D�HD�@ D�~�D�� D��qD�>�D�~�D޾�D�  D�>�D߂�D߽qD���D�AHD�~�D�� D�  D�@ DႏDᾸD���D�>�D� D�� D���D�AHD� D�� D�HD�AHD�~�D��HD�  D�=qD�~�D�� D���D�B�D�HD��HD�  D�AHD�HD�D�HD�AHD� D�� D�  D�=qD� D�� D��qD�@ D� D꾸D���D�=qD� D뾸D�  D�>�D�}qD�� D�  D�=qD� D��HD�HD�AHD�~�D��HD�HD�>�D�~�DﾸD�  D�AHD�� D��HD�  D�@ D�HD�� D�HD�AHD�HD�� D�  D�@ D� D�� D�HD�>�D�~�D�D�  D�>�D�� D�� D��D�AHD�� D���D��D�AHD���D�� D�HD�@ D�}qD��qD��qD�>�D�}qD���D��G�O�?\)?��?B�\?�  ?�=q?�33?��?�@�@z�@+�@:�H@G�@Tz�@k�@z�H@�ff@�\)@�Q�@��R@��@�{@�@��R@��
@�=q@�z�@ٙ�@�p�@��@���@�@��RA�
A
=A�A\)A�AQ�A(�A   A$z�A(Q�A,(�A/\)A333A6ffA;�A>�RAB�\AFffAI��AN�RAQ�AUAY��A]p�AaG�Ag
=Ak�AqG�AvffA{�A�  A��\A��A�Q�A�33A�p�A�  A��\A�A���A�33A�ffA���A��
A��RA���A��
A�ffA���A��A�{A�Q�A��HA��A��A���A��
A�{A���A˅A�{A�Q�A�33A��A�\)A��A�(�A�ffA���A��HA��A�\)A��A�A�p�A�\)A��A�z�A��RA���A�33A�p�A��B ��B�B�HB  Bp�B�RB  B	G�B
�RB  Bp�B�RB(�BG�B�RB�
B��B=qB�B�BffB�B��B�B
=B (�B!p�B"�\B#�B$��B&=qB'\)B(z�B)B+
=B,Q�B-��B.�\B0  B0��B1�B3
=B4(�B4��B6=qB733B8Q�B9�B:=qB;
=B<(�B<��B>=qB?
=B@(�B@��BA�BB�HBC�BDz�BEp�BF=qBG33BG�
BH��BI��BJ=qBK
=BK�
BLz�BMp�BNffBO33BPQ�BQ�BR{BS33BTQ�BUG�BV=qBW�BX��BZ=qB[33B\z�B]p�B^�\B_�
B`��Bb{Bb�HBd  BeG�Bf=qBg�Bh��BiBk
=Bl(�Bmp�Bn�RBo�
Bq�Br=qBs�Bt��Bu�Bv�HBx  ByG�Bz=qB{\)B|��B}�B33B�{B��RB�\)B�  B��\B��B��B�Q�B��HB��B�{B���B�G�B��
B�Q�B��HB�p�B��B�ffB�
=B��B�{B��\B�33B�B�=qB���B�\)B��B�z�B���B���B�(�B��RB�G�B��B�z�B�
=B���B�=qB���B�\)B��B�z�B�
=B���B�(�B��HB���B�{B��\B�33B��
B�z�B��B���B�(�B��RB�33B��
B�Q�B��HB��B�  B��\B��B��B�=qB���B�\)B��B�ffB�
=B���B�(�B���B�G�B��
B�Q�B��HB�p�B�  B��\B�
=B���B�{B���B�33B��B�(�B��RB�33B��
B�Q�B��HB�p�B�{B��\B�
=B��B�=qB��RB�\)B��B�z�B�
=B���B�(�B��RB�33B��
B�Q�B���B�G�B��
B�ffB�
=B��B�{B£�B�33B��
B�ffB���BŅB�(�BƸRB�G�B��
B�Q�B�
=Bə�B�(�BʸRB�\)B��
B�z�B���B͙�B�=qB��HBυB�{BиRB�G�B��B�ffB���B�\)B�{Bԣ�B��Bՙ�B�=qB���B�\)B��
B�z�B���Bٙ�B�{BڸRB�G�B�B�Q�B���B�\)B��B�z�B�
=Bߙ�B�=qB���B�G�B�B�ffB���B�B�{B�z�B�
=B�B�  B�\B�
=B癚B�{B��B�
=B陚B�{B�\B�
=B�B�=qB��HB�p�B��B�ffB���B�B�  B��\B���B�\)B��
B�ffB��HB�p�B��
B�z�B���B�p�B��B�ffB��HB�p�B��B�ffB�
=B���B�{B���B�G�B��B�(�B��RB�G�B��
B�z�B�
=B��
C 33C z�C ��C{CQ�C�C��C{C\)C��C��CQ�C��C�HC33C�CC��CG�C��C�HC33C��C��CG�C��C�HC�Cp�CC	
=C	ffC	��C
�C
ffC
�RC
=C\)C�C�C33C�C�
C33C��C�CG�C�\C�HC33C�\C�HC=qC�\C  CffC�C  CQ�C�C{Cz�C�
C33Cz�CC�C�C�CG�C��C�C33C�\C�HCQ�C�C  CG�C��C�
C{C\)C�\C��C
=CG�Cz�C��C�RC��C�HC��C�CQ�CffC�C��C�RC�
C�HC�C  C�C33CG�Cz�C��CC��C��C�C��C(�CQ�CffC�C��CCC��C�C
=C�C=qCQ�C�C��CCC�
C��C 
=C G�C \)C z�C �\C ��C �RC C ��C!�C!=qC!Q�C!p�C!�\C!�\C!�C!��C!�HC!��C"�C"=qC"p�C"�\C"�C"��C"�
C"�C#  C#�C#=qC#ffC#��C#�RC#C#�HC$  C$�C$\)C$z�C$��C$��C$��C$�HC%
=C%G�C%ffC%�\C%��C%�RC%�
C%��C&33C&\)C&z�C&�\C&��C&C&�HC'
=C'(�C'\)C'�C'��C'��C'�HC'�C(
=C(33C(G�C(p�C(��C(C(�C)  C)
=C)(�C)G�C)ffC)��C)C)�HC)�C*  C*(�C*\)C*z�C*��C*��C*��C*�C+(�C+G�C+ffC+z�C+��C+�RC+��C,{C,G�C,\)C,ffC,�C,�C,�C-
=C-33C-33C-\)C-p�C-�RC-�
C-��C.  C.(�C.Q�C.p�C.�C.��C.�HC.��C/{C/=qC/\)C/�C/��C/�
C0
=C0(�C0Q�C0\)C0z�C0�C0�HC1
=C1{C133C1G�C1�\C1�RC1��C1�HC2  C2=qC2ffC2�\C2��C2�RC2�HC3{C333C3=qC3\)C3�\C3C3�HC3��C4
=C4Q�C4z�C4�C4��C4�C5  C5(�C533C5G�C5p�C5�C5�
C5�C6  C6{C6\)C6�C6��C6��C6��C7  C733C7Q�C7ffC7z�C7�C7�HC8  C8
=C8(�C8Q�C8�\C8�C8�RC8�
C8��C9(�C9\)C9�C9�\C9�RC9��C:{C:33C:33C:\)C:��C:��C:�C:��C;{C;Q�C;z�C;��C;��C;��C<
=C<33C<G�C<\)C<�C<C<�HC<��C={C=\)C=�C=�\C=�C=�HC>{C>=qC>G�C>ffC>��C>��C>��C?  C?�C?ffC?z�C?��C?C?��C@
=C@(�C@\)C@��C@�RC@��C@�CA(�CA\)CA\)CAz�CACA�CB  CB{CBQ�CB�CB�\CB�RCC  CC(�CCG�CCffCC��CC�HCD{CD(�CD\)CD�\CD�CD�
CE�CE\)CEffCE��CE��CF�CF33CFffCF�RCF��CG  CG33CG�CG�RCG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                               @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�lG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�
=A�1A�A���A���A���A�JA�
=A�1A�bA�{A�oA� �A�"�A�$�A�$�A�&�A�&�A�(�A�(�A�(�A�+A�(�A�+A�+A�+A�-A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�1'A�33A�33A�33A�33A�/A�-A�-A�&�A�-A�5?A�?}A�Q�A�XA�dZA�x�A�hsA�\)A�S�A�M�A�M�A�K�A�?}A�/A�$�A��A�  A���A�VAϥ�A��Aʹ9A��TA��#A��mAƾwA��`A�C�A�Q�A�G�A�;dA�jA��jA�n�A�I�A���A��A��^A���A��A��mA���A��A�ffA��`A�`BA��;A�ZA���A���A��A��9A�ffA�A�A�S�A���A�ffA�K�A�oA�33A���A��uA�7LA��A���A���A���A�hsA�K�A��A�7LA�p�A�1A�ĜA�M�A���A��DA�(�A�dZA��A�bNA��+A�7A~��A{�mAx��At~�Ar�+Ao�hAn��AlVAj1'Ae�TA]oAU�AR�\AQ�^AM�wAKp�AJ�DAIG�AH�jAF~�AC;dAA��A@�jA?7LA>z�A=�FA<��A:��A7��A6JA5�A4A17LA.Q�A,�DA*�`A)�FA)O�A(��A'�;A&A�A%��A%p�A$��A$-A#l�A"��A"~�A"(�A ��A�wA��A�An�A;dA�!A~�A��A33A~�A �A�FAJAS�A�A�#A��AbNAE�A  AS�An�A-A1'AXA��A�\A�Az�Ap�A
�`A�AƨA�RA�PA�!A �A�FA�-A�-A�PA
=A�RA{A�hAG�A �HA Q�@���@��@��@��#@��w@���@�bN@��+@���@�9X@�C�@�hs@�P@�@�^5@���@�@���@�Q�@�I�@�@�~�@�X@�
=@�\@�v�@�n�@�=q@�  @�C�@�=q@���@���@�dZ@�K�@�J@� �@�dZ@ڰ!@�^5@�$�@��@ٙ�@��/@؛�@؛�@�A�@׶F@�K�@�v�@���@�x�@��`@ԓu@�  @��@ѡ�@Гu@�A�@��@��
@�;d@���@�+@���@�1'@Ͼw@�ƨ@ϥ�@ΰ!@��@�@̛�@��m@� �@�1'@�1'@��;@���@�J@ɩ�@ɩ�@�@��T@��@ʧ�@��H@��y@���@�v�@�V@�=q@�5?@�{@ɉ7@���@ȋD@��@�ȴ@�ff@�ff@�V@�5?@�J@Ų-@�%@å�@���@�V@���@�p�@�/@���@��j@���@��@���@�v�@�=q@�J@���@���@��j@���@�dZ@�+@�o@�@���@��\@�^5@��T@�%@���@��D@�bN@���@�J@�p�@�?}@���@���@�b@���@�33@��R@��+@�E�@��#@���@�O�@��`@���@�(�@���@�C�@��y@��T@��@��@�z�@�r�@�j@�j@�j@�9X@���@��P@�K�@�33@��@�V@��@��^@�O�@��j@��@��F@�l�@�33@���@��!@�{@�hs@��@��@���@��9@��D@�z�@��@��D@�r�@��@�z�@�r�@�Z@�A�@�1'@�(�@��;@�C�@�-@��#@��^@��h@�p�@�`B@�%@�A�@��@�1@�t�@�"�@���@��@�ȴ@��R@���@���@�ff@�J@��h@�(�@�1@�  @��m@���@�ƨ@��w@��@���@�|�@�K�@�@��y@��@�v�@�@��-@��^@�x�@��j@�z�@�I�@�1'@� �@��@���@��F@��!@�@���@��@���@���@���@��9@��u@�bN@�9X@��@���@���@�l�@�K�@�+@�
=@��H@���@��@��@��T@���@�@���@���@��h@��7@�?}@�I�@�  @��m@��m@���@�ƨ@��w@���@�l�@�\)@�K�@�K�@�+@��!@���@�x�@��j@��9@�Q�@��F@�t�@�l�@�dZ@�K�@�"�@�o@��@���@�M�@���@��@�p�@�G�@�/@��@���@�j@�b@��
@���@�C�@��H@���@��R@��\@�V@�5?@�@��T@���@��h@�?}@���@�Ĝ@���@��D@�bN@�  @��w@���@�l�@�C�@�;d@�+@���@���@�^5@��@���@�x�@�`B@�&�@��@��`@��@�r�@�9X@�  @�@��@�@;d@~�R@~E�@~@}�T@}��@|z�@|9X@|(�@{��@{�F@{@z=q@zJ@yhs@xĜ@x�9@w��@w
=@u@u`B@u�@t�/@t�j@t�D@tZ@t9X@t�@t1@s�F@r��@q��@q��@q��@qx�@qhs@qX@q7L@q%@p��@pĜ@p  @o�P@o\)@o;d@o
=@n�y@n��@n��@n�+@nv�@n$�@m�T@m��@m@mp�@mV@l��@l�@lz�@lZ@k��@j��@j=q@i��@i%@h��@hbN@g��@g;d@f��@e�-@e`B@d��@d�@d�/@d��@d�D@d(�@d1@c�m@c�@b��@b��@b�!@b�@`��@`bN@` �@_�P@^�R@^V@^{@]��@]/@\�D@["�@ZM�@Y�@W�@W|�@V�y@V�+@VE�@V@U@U�-@U�h@U�@U`B@U�@T�/@T�j@T�D@S�F@SC�@R��@Q�#@Q7L@P��@Pr�@O��@O\)@N�@M�-@L�/@LZ@K�
@J=q@I��@I��@I�7@I�7@I&�@H�`@H��@HQ�@G\)@G�@G+@G+@F��@E��@E��@F$�@E�-@D�@Dj@DI�@D(�@D�@D�@D1@Cƨ@C��@CdZ@C"�@B�@B��@Bn�@A�#@A�^@A��@Ahs@AG�@A�@@��@@Ĝ@@bN@?�@?l�@>�y@>ȴ@>�R@>$�@=@=@=��@=O�@=/@=/@<�@<j@<Z@<Z@<Z@<I�@<�@;��@;�
@;t�@;S�@;33@;@:�H@:��@:��@9��@97L@9%@8��@8Ĝ@8�9@8��@8r�@8Q�@8Q�@8A�@8A�@81'@8 �@7��@7l�@6��@6�@6��@6v�@6ff@6{@5��@5��@5�h@5`B@5`B@5O�@4��@4z�@3o@2�@2�@2�H@2�H@2��@2��@2�\@2M�@17L@0��@0Ĝ@0Ĝ@0Ĝ@0�9@0��@0��@0��@0��@0��@01'@/�P@.�@.V@-�@-�h@-V@,�D@,9X@+��@+�
@+ƨ@+�F@+t�@*�@*��@*�\@*n�@*^5@*M�@*M�@*=q@)�^@)hs@(1'@'|�@'+@&�y@&ȴ@&ȴ@&ȴ@&�R@&ȴ@&�R@&��@&�+@&ff@&V@&$�@%�@%�T@%��@%��@%p�@%?}@$��@$��@$�j@$�D@$j@$1@#"�@"��@"�!@"n�@"=q@"�@"J@"J@!��@!�^@!hs@!&�@!%@ ��@ Ĝ@ �u@ �@ A�@|�@V@5?@{@@�T@��@�-@/@V@��@�/@j@(�@�
@��@t�@"�@��@�@X@�@��@��@bN@�w@�@l�@�@�y@�R@��@E�@��@V@Z@�
@ƨ@ƨ@ƨ@ƨ@dZ@"�@^5@�@�@�#@�^@��@�7@hs@X@7L@�@�@�@%@%@%@%@��@��@bN@1'@b@b@b@�;@�@��@�P@l�@;d@�@��@5?@$�@$�@5?@$�@{@��@@��@@�h@p�@p�@`B@`B@V@��@�/@��@��@�j@�@z�@9X@�m@�F@t�@t�A�A�%A�
=A�JA�VA�VA�1A�%A�VA�{A�JA�A�%A�A�A�A�A���A���A���A���A���A���A���A���A���A���A�  A�A�  A���A�A�oA��A�JA�VA�JA�
=A�A�A�A�%A�A�bA�{A�VA�oA��A��A��A�oA�bA�bA�bA�JA�
=A�VA� �A��A��A��A� �A�"�A�"�A�$�A�&�A�$�A�$�A�$�A�"�A�"�A� �A� �A� �A� �A�"�A�$�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�$�A�&�A�$�A�(�A�+A�+A�+A�(�A�&�A�&�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�+A�(�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�+A�+A�(�A�+A�(�A�(�A�(�A�(�A�(�A�+A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�+A�-A�+A�+A�+A�-A�-A�+A�+A�+A�+A�+A�+A�+A�+A�-A�-A�-A�-A�-A�-A�-A�+A�-A�+A�-A�-A�-A�/A�-A�-A�/A�/A�-A�-A�/A�/A�/A�/A�/A�/A�1'A�1'A�1'A�/A�1'A�1'A�33A�1'A�1'A�1'A�1'A�33A�33A�1'A�/A�1'A�33A�33A�1'A�1'A�1'A�1'A�1'A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�/A�1'A�1'A�1'A�1'A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�33A�33A�1'A�33A�33A�33A�1'A�33A�5?A�1'A�1'A�1'A�+A�+A�-A�-A�-A�+A�/A�1'A�1'A�33A�1'A�1'A�33A�1'A�/A�/A�-A�-A�+A�&�A�$�A�"�A�$�A�&�A�&�A�&�A�(�A�-A�-A�/A�/A�1'A�1'A�1'A�33A�5?A�5?A�7LA�7LA�7LA�9XA�9XA�?}A�A�A�E�A�I�A�M�A�O�A�O�A�Q�A�M�A�O�A�M�A�O�A�I�A�K�A�K�A�Q�A�dZA�ffA�`BA�XA�XA�ZA�bNA�t�A�jA�n�A�~�A�v�AӅA�~�A�t�A�p�A�n�A�n�A�hsA�ffA�ffA�bNA�bNA�`BA�`BA�bNA�`BA�^5A�VA�VA�VA�XA�VA�XA�VA�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�M�A�E�A�A�A�A�A�A�A�?}A�?}A�=qA�;dA�=qA�;dA�9XA�5?A�/A�-A�(�A�(�A�(�A�&�A�(�A�(�A�(�A�(�A�&�A�&�A�$�A�"�A��A��A��A�bA�bA�JA�1A�A�A�  A�  A���A���A���A���A���A���A���A���A��A��A��A��A��A��AҸRA�hsA�r�AЧ�A�E�A���A϶FAϛ�Aω7A�x�A�jA�`BA�M�A�=qA�bA��A���A�ĜAΑhA�l�A�^5A�S�A�G�A��HAͩ�A�oȀ\A�r�A�\)A˰!A�oAɣ�Aǟ�A�$�A��A��`A��;A��
A��
A���A��
A���A��A��/A��mA��A��A���A�A�VA�JA��AƧ�Aƙ�AƅA�9XAœuA��A��Aĺ^AčPA�r�A�VA�E�A�=qA�=qA�?}A�C�A�?}A�S�A�K�A�M�A�M�A�jAāA�x�A�dZA�hsA�ZA�/A�VA�A�A�7LA��#A��A���A�|�A�l�A�hsA�dZA�`BA�S�A���A��9A��PA��+A��A�~�A�x�A�t�A�dZA�^5A�`BA�bNA�ffA�XA�E�A�(�A��A��A�ƨA��hA�`BA�Q�A�A�A��A��A��/A��
A���A���A���A�ƨA��wA��^A��FA��FA��^A��^A��^A��^A��RA��FA��FA��FA��9A��FA��FA��FA��FA��FA��FA��FA��9A��!A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A��uA��\A��DA��+A��A��A��A��A�~�A�|�A�t�A�t�A�r�A�p�A�jA�XA�O�A�C�A�=qA�1'A�/A�&�A��A�{A�VA�%A���A��;A���A��9A�n�A�E�A�;dA�5?A�1'A�+A� �A��A�%A���A��A��;A���A��jA���A�ZA�JA���A�A��/A��A���A�~�A�bNA�S�A�C�A�&�A��A�-A���A���A���A���A��hA��hA��\A��PA��7A��A��A�z�A�t�A�ffA�ZA�K�A�I�A�G�A�A�A�?}A�?}A�?}A�;dA�;dA�+A� �A�{A���A��`A��#A���A�A��^A��-A��^A��FA��A�r�A�O�A�{A��#A��A�%A�C�A��wA���A���A��\A��+A��A��A�?}A�;dA�7LA�5?A�$�A�bA���A��A��
A���A��HA��9A��-A��A���A���A�hsA�Q�A�E�A�&�A�1A���A��A�E�A��A�A��HA��RA���A�p�A�\)A��A�1A��;A�`BA��A��mA�A�n�A�dZA�S�A�7LA�1A���A���A���A��-A��A��^A��FA���A���A���A��uA��A�~�A�|�A�|�A�v�A�hsA�jA�bNA�O�A�33A��A�oA�VA�
=A�%A���A��A��A��A���A��/A��HA�ȴA��jA��wA��!A���A��A��A��!A��A���A���A���A���A���A��7A�z�A�p�A�`BA�S�A��PA���A���A���A���A���A��A��A��A��A�z�A�t�A�`BA�S�A�VA�?}A�9XA�9XA�1'A�1A��A�%A���A���A��A��
A�A��!A���A��uA�v�A�hsA�{A��A�G�A�(�A��A��A�{A�JA���A��mA��
A��
A��
A�ĜA��9A���A��7A�K�A�5?A��9A�|�A�|�A�t�A�jA�`BA�VA�I�A�1'A�"�A�oA��A���A�x�A�ZA���A��HA��A���A���A�VA���A��RA���A��PA�l�A�/A�9XA��A�?}A�~�A�t�A�v�A�l�A�l�A�jA�bNA�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                               A�
=A�
=A�1A�A���A���A���A�JA�
=A�1A�bA�{A�oA� �A�"�A�$�A�$�A�&�A�&�A�(�A�(�A�(�A�+A�(�A�+A�+A�+A�-A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�1'A�33A�33A�33A�33A�/A�-A�-A�&�A�-A�5?A�?}A�Q�A�XA�dZA�x�A�hsA�\)A�S�A�M�A�M�A�K�A�?}A�/A�$�A��A�  A���A�VAϥ�A��Aʹ9A��TA��#A��mAƾwA��`A�C�A�Q�A�G�A�;dA�jA��jA�n�A�I�A���A��A��^A���A��A��mA���A��A�ffA��`A�`BA��;A�ZA���A���A��A��9A�ffA�A�A�S�A���A�ffA�K�A�oA�33A���A��uA�7LA��A���A���A���A�hsA�K�A��A�7LA�p�A�1A�ĜA�M�A���A��DA�(�A�dZA��A�bNA��+A�7A~��A{�mAx��At~�Ar�+Ao�hAn��AlVAj1'Ae�TA]oAU�AR�\AQ�^AM�wAKp�AJ�DAIG�AH�jAF~�AC;dAA��A@�jA?7LA>z�A=�FA<��A:��A7��A6JA5�A4A17LA.Q�A,�DA*�`A)�FA)O�A(��A'�;A&A�A%��A%p�A$��A$-A#l�A"��A"~�A"(�A ��A�wA��A�An�A;dA�!A~�A��A33A~�A �A�FAJAS�A�A�#A��AbNAE�A  AS�An�A-A1'AXA��A�\A�Az�Ap�A
�`A�AƨA�RA�PA�!A �A�FA�-A�-A�PA
=A�RA{A�hAG�A �HA Q�@���@��@��@��#@��w@���@�bN@��+@���@�9X@�C�@�hs@�P@�@�^5@���@�@���@�Q�@�I�@�@�~�@�X@�
=@�\@�v�@�n�@�=q@�  @�C�@�=q@���@���@�dZ@�K�@�J@� �@�dZ@ڰ!@�^5@�$�@��@ٙ�@��/@؛�@؛�@�A�@׶F@�K�@�v�@���@�x�@��`@ԓu@�  @��@ѡ�@Гu@�A�@��@��
@�;d@���@�+@���@�1'@Ͼw@�ƨ@ϥ�@ΰ!@��@�@̛�@��m@� �@�1'@�1'@��;@���@�J@ɩ�@ɩ�@�@��T@��@ʧ�@��H@��y@���@�v�@�V@�=q@�5?@�{@ɉ7@���@ȋD@��@�ȴ@�ff@�ff@�V@�5?@�J@Ų-@�%@å�@���@�V@���@�p�@�/@���@��j@���@��@���@�v�@�=q@�J@���@���@��j@���@�dZ@�+@�o@�@���@��\@�^5@��T@�%@���@��D@�bN@���@�J@�p�@�?}@���@���@�b@���@�33@��R@��+@�E�@��#@���@�O�@��`@���@�(�@���@�C�@��y@��T@��@��@�z�@�r�@�j@�j@�j@�9X@���@��P@�K�@�33@��@�V@��@��^@�O�@��j@��@��F@�l�@�33@���@��!@�{@�hs@��@��@���@��9@��D@�z�@��@��D@�r�@��@�z�@�r�@�Z@�A�@�1'@�(�@��;@�C�@�-@��#@��^@��h@�p�@�`B@�%@�A�@��@�1@�t�@�"�@���@��@�ȴ@��R@���@���@�ff@�J@��h@�(�@�1@�  @��m@���@�ƨ@��w@��@���@�|�@�K�@�@��y@��@�v�@�@��-@��^@�x�@��j@�z�@�I�@�1'@� �@��@���@��F@��!@�@���@��@���@���@���@��9@��u@�bN@�9X@��@���@���@�l�@�K�@�+@�
=@��H@���@��@��@��T@���@�@���@���@��h@��7@�?}@�I�@�  @��m@��m@���@�ƨ@��w@���@�l�@�\)@�K�@�K�@�+@��!@���@�x�@��j@��9@�Q�@��F@�t�@�l�@�dZ@�K�@�"�@�o@��@���@�M�@���@��@�p�@�G�@�/@��@���@�j@�b@��
@���@�C�@��H@���@��R@��\@�V@�5?@�@��T@���@��h@�?}@���@�Ĝ@���@��D@�bN@�  @��w@���@�l�@�C�@�;d@�+@���@���@�^5@��@���@�x�@�`B@�&�@��@��`@��@�r�@�9X@�  @�@��@�@;d@~�R@~E�@~@}�T@}��@|z�@|9X@|(�@{��@{�F@{@z=q@zJ@yhs@xĜ@x�9@w��@w
=@u@u`B@u�@t�/@t�j@t�D@tZ@t9X@t�@t1@s�F@r��@q��@q��@q��@qx�@qhs@qX@q7L@q%@p��@pĜ@p  @o�P@o\)@o;d@o
=@n�y@n��@n��@n�+@nv�@n$�@m�T@m��@m@mp�@mV@l��@l�@lz�@lZ@k��@j��@j=q@i��@i%@h��@hbN@g��@g;d@f��@e�-@e`B@d��@d�@d�/@d��@d�D@d(�@d1@c�m@c�@b��@b��@b�!@b�@`��@`bN@` �@_�P@^�R@^V@^{@]��@]/@\�D@["�@ZM�@Y�@W�@W|�@V�y@V�+@VE�@V@U@U�-@U�h@U�@U`B@U�@T�/@T�j@T�D@S�F@SC�@R��@Q�#@Q7L@P��@Pr�@O��@O\)@N�@M�-@L�/@LZ@K�
@J=q@I��@I��@I�7@I�7@I&�@H�`@H��@HQ�@G\)@G�@G+@G+@F��@E��@E��@F$�@E�-@D�@Dj@DI�@D(�@D�@D�@D1@Cƨ@C��@CdZ@C"�@B�@B��@Bn�@A�#@A�^@A��@Ahs@AG�@A�@@��@@Ĝ@@bN@?�@?l�@>�y@>ȴ@>�R@>$�@=@=@=��@=O�@=/@=/@<�@<j@<Z@<Z@<Z@<I�@<�@;��@;�
@;t�@;S�@;33@;@:�H@:��@:��@9��@97L@9%@8��@8Ĝ@8�9@8��@8r�@8Q�@8Q�@8A�@8A�@81'@8 �@7��@7l�@6��@6�@6��@6v�@6ff@6{@5��@5��@5�h@5`B@5`B@5O�@4��@4z�@3o@2�@2�@2�H@2�H@2��@2��@2�\@2M�@17L@0��@0Ĝ@0Ĝ@0Ĝ@0�9@0��@0��@0��@0��@0��@01'@/�P@.�@.V@-�@-�h@-V@,�D@,9X@+��@+�
@+ƨ@+�F@+t�@*�@*��@*�\@*n�@*^5@*M�@*M�@*=q@)�^@)hs@(1'@'|�@'+@&�y@&ȴ@&ȴ@&ȴ@&�R@&ȴ@&�R@&��@&�+@&ff@&V@&$�@%�@%�T@%��@%��@%p�@%?}@$��@$��@$�j@$�D@$j@$1@#"�@"��@"�!@"n�@"=q@"�@"J@"J@!��@!�^@!hs@!&�@!%@ ��@ Ĝ@ �u@ �@ A�@|�@V@5?@{@@�T@��@�-@/@V@��@�/@j@(�@�
@��@t�@"�@��@�@X@�@��@��@bN@�w@�@l�@�@�y@�R@��@E�@��@V@Z@�
@ƨ@ƨ@ƨ@ƨ@dZ@"�@^5@�@�@�#@�^@��@�7@hs@X@7L@�@�@�@%@%@%@%@��@��@bN@1'@b@b@b@�;@�@��@�P@l�@;d@�@��@5?@$�@$�@5?@$�@{@��@@��@@�h@p�@p�@`B@`B@V@��@�/@��@��@�j@�@z�@9X@�m@�F@t�G�O�A�A�%A�
=A�JA�VA�VA�1A�%A�VA�{A�JA�A�%A�A�A�A�A���A���A���A���A���A���A���A���A���A���A�  A�A�  A���A�A�oA��A�JA�VA�JA�
=A�A�A�A�%A�A�bA�{A�VA�oA��A��A��A�oA�bA�bA�bA�JA�
=A�VA� �A��A��A��A� �A�"�A�"�A�$�A�&�A�$�A�$�A�$�A�"�A�"�A� �A� �A� �A� �A�"�A�$�A�"�A�"�A�"�A�"�A�"�A�"�A�"�A�$�A�&�A�$�A�(�A�+A�+A�+A�(�A�&�A�&�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�+A�+A�(�A�(�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�&�A�(�A�+A�+A�(�A�+A�(�A�(�A�(�A�(�A�(�A�+A�(�A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�-A�-A�-A�-A�-A�-A�-A�-A�-A�-A�+A�-A�+A�+A�+A�-A�-A�+A�+A�+A�+A�+A�+A�+A�+A�-A�-A�-A�-A�-A�-A�-A�+A�-A�+A�-A�-A�-A�/A�-A�-A�/A�/A�-A�-A�/A�/A�/A�/A�/A�/A�1'A�1'A�1'A�/A�1'A�1'A�33A�1'A�1'A�1'A�1'A�33A�33A�1'A�/A�1'A�33A�33A�1'A�1'A�1'A�1'A�1'A�/A�/A�/A�/A�/A�/A�/A�/A�/A�/A�1'A�1'A�/A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�1'A�/A�1'A�1'A�1'A�1'A�/A�/A�/A�1'A�1'A�1'A�1'A�1'A�33A�33A�1'A�33A�33A�33A�1'A�33A�5?A�1'A�1'A�1'A�+A�+A�-A�-A�-A�+A�/A�1'A�1'A�33A�1'A�1'A�33A�1'A�/A�/A�-A�-A�+A�&�A�$�A�"�A�$�A�&�A�&�A�&�A�(�A�-A�-A�/A�/A�1'A�1'A�1'A�33A�5?A�5?A�7LA�7LA�7LA�9XA�9XA�?}A�A�A�E�A�I�A�M�A�O�A�O�A�Q�A�M�A�O�A�M�A�O�A�I�A�K�A�K�A�Q�A�dZA�ffA�`BA�XA�XA�ZA�bNA�t�A�jA�n�A�~�A�v�AӅA�~�A�t�A�p�A�n�A�n�A�hsA�ffA�ffA�bNA�bNA�`BA�`BA�bNA�`BA�^5A�VA�VA�VA�XA�VA�XA�VA�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�M�A�E�A�A�A�A�A�A�A�?}A�?}A�=qA�;dA�=qA�;dA�9XA�5?A�/A�-A�(�A�(�A�(�A�&�A�(�A�(�A�(�A�(�A�&�A�&�A�$�A�"�A��A��A��A�bA�bA�JA�1A�A�A�  A�  A���A���A���A���A���A���A���A���A��A��A��A��A��A��AҸRA�hsA�r�AЧ�A�E�A���A϶FAϛ�Aω7A�x�A�jA�`BA�M�A�=qA�bA��A���A�ĜAΑhA�l�A�^5A�S�A�G�A��HAͩ�A�oȀ\A�r�A�\)A˰!A�oAɣ�Aǟ�A�$�A��A��`A��;A��
A��
A���A��
A���A��A��/A��mA��A��A���A�A�VA�JA��AƧ�Aƙ�AƅA�9XAœuA��A��Aĺ^AčPA�r�A�VA�E�A�=qA�=qA�?}A�C�A�?}A�S�A�K�A�M�A�M�A�jAāA�x�A�dZA�hsA�ZA�/A�VA�A�A�7LA��#A��A���A�|�A�l�A�hsA�dZA�`BA�S�A���A��9A��PA��+A��A�~�A�x�A�t�A�dZA�^5A�`BA�bNA�ffA�XA�E�A�(�A��A��A�ƨA��hA�`BA�Q�A�A�A��A��A��/A��
A���A���A���A�ƨA��wA��^A��FA��FA��^A��^A��^A��^A��RA��FA��FA��FA��9A��FA��FA��FA��FA��FA��FA��FA��9A��!A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A��uA��\A��DA��+A��A��A��A��A�~�A�|�A�t�A�t�A�r�A�p�A�jA�XA�O�A�C�A�=qA�1'A�/A�&�A��A�{A�VA�%A���A��;A���A��9A�n�A�E�A�;dA�5?A�1'A�+A� �A��A�%A���A��A��;A���A��jA���A�ZA�JA���A�A��/A��A���A�~�A�bNA�S�A�C�A�&�A��A�-A���A���A���A���A��hA��hA��\A��PA��7A��A��A�z�A�t�A�ffA�ZA�K�A�I�A�G�A�A�A�?}A�?}A�?}A�;dA�;dA�+A� �A�{A���A��`A��#A���A�A��^A��-A��^A��FA��A�r�A�O�A�{A��#A��A�%A�C�A��wA���A���A��\A��+A��A��A�?}A�;dA�7LA�5?A�$�A�bA���A��A��
A���A��HA��9A��-A��A���A���A�hsA�Q�A�E�A�&�A�1A���A��A�E�A��A�A��HA��RA���A�p�A�\)A��A�1A��;A�`BA��A��mA�A�n�A�dZA�S�A�7LA�1A���A���A���A��-A��A��^A��FA���A���A���A��uA��A�~�A�|�A�|�A�v�A�hsA�jA�bNA�O�A�33A��A�oA�VA�
=A�%A���A��A��A��A���A��/A��HA�ȴA��jA��wA��!A���A��A��A��!A��A���A���A���A���A���A��7A�z�A�p�A�`BA�S�A��PA���A���A���A���A���A��A��A��A��A�z�A�t�A�`BA�S�A�VA�?}A�9XA�9XA�1'A�1A��A�%A���A���A��A��
A�A��!A���A��uA�v�A�hsA�{A��A�G�A�(�A��A��A�{A�JA���A��mA��
A��
A��
A�ĜA��9A���A��7A�K�A�5?A��9A�|�A�|�A�t�A�jA�`BA�VA�I�A�1'A�"�A�oA��A���A�x�A�ZA���A��HA��A���A���A�VA���A��RA���A��PA�l�A�/A�9XA��A�?}A�~�A�t�A�v�A�l�A�l�A�jA�bNA�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
"B
"B
�B
�B
"B
�B
B
�B
B
PB
PB
B
PB
B
B
�B
B
�B
PB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
(B
�B
�B
hB
:B
�B
1B
xB
IB
%�B
4�B
:*B
=�B
D3B
F�B
J�B
XyB
[�B
[�B
\)B
\�B
\�B
\�B
^�B
`B
`BB
a�B
c�B
gmB
�{B
�B
��B
��B
�B(B�B4�B^jBcTBu�B�MBÖB�9B��B�B�B�xBMB�BB$@B'�B#B�B�]B�`B�BB��B��B��B��B�bB��B�.B�;By�Bo�Bd�BOvB33B-wB'RB&�B$@B!�B!bB�B7BMB1B
�B
�3B
�qB
��B
�B
�B
�FB
�IB
^5B
Q�B
F�B
_B	�B	�B	��B	ٴB	�0B	��B	�B	�4B	��B	�B	��B	xlB	^B	A�B	.�B	-�B	'�B	�B	�B	�B	uB	+B	�B	�B	oB	 �B�B�>B��B��B��B�B�2B�&B�B�pB��B�#B�EBרB�
B�BخB�9B�sB�mB�yB�yBخB�yB�sB�QBخB�EB��BݘB��B�jB�B��B��B�B��B��B�)B�QB��B� B�B�B��B�GB�B��B��B�8B��B�DB��B�]B��B�%B�2B��B�B� B�B��B�;B�iB��B�/B�cB�iB�B�	B��B��B�B	 4B��B��B�]B�fB��B��B�%B�2B��B��B�DB�(B��B�DB��B�DB��B��B�B�B��B��B	 4B	;B�cB��B��B	 4B	�B	�B	
rB	B	�B	�B	~B	uB	�B	qB	OB	OB	VB	 �B	"hB	"4B	#B	#�B	%FB	$�B	%B	(�B	*eB	,�B	0!B	0�B	3hB	6zB	;0B	>�B	?HB	@B	B[B	HKB	NpB	WsB	`�B	f2B	m�B	o�B	xB	y	B	x8B	x8B	y	B	xlB	}"B	~]B	~�B	�B	��B	��B	�SB	�%B	��B	��B	�	B	��B	��B	��B	�qB	�VB	��B	��B	��B	�hB	�zB	�RB	�B	��B	��B	��B	�IB	��B	�B	�B	��B	��B	��B	��B	��B	��B	�qB	��B	��B	�BB	��B	��B	�aB	ÖB	��B	�gB	ĜB	�B	��B	�)B	��B	̘B	��B	�B	͟B	�pB	ΥB	�HB	�[B	�,B	�,B	�,B	�B	خB	�KB	��B	��B	��B	��B	�B	�;B	�B	�|B	�B	�B	��B	� B	��B	��B	�mB	�B	�DB	�B	�)B	�]B	�/B	��B	�B	��B	�B	�]B	��B	��B	�B	�cB	�/B	�B	�B	�B	�vB	�|B	�MB	�B	�MB	��B	�`B	�fB	�lB	��B	�rB	��B	��B	��B	��B	�DB	�xB	�DB	�DB	�DB	�B	�xB	�B	�B	�B	�B	�rB	�DB	�B	�(B	�(B	�]B	�]B	�(B	�]B	�cB
 iB	��B	��B
oB
�B
�B
�B
�B
�B
�B
�B
AB
AB
�B
�B
�B
�B
%B
YB
YB
%B
YB
%B
�B
�B
_B
�B
�B
�B
	�B
�B
�B
	lB

rB

	B

rB

=B

=B

	B
	�B

rB
B
PB
PB
PB
B
�B
\B
\B
�B
�B
�B
�B
bB
�B
�B
4B
 B
hB
hB
oB
:B
�B
oB
�B
oB
�B
oB
�B
B
uB
B
�B
�B
�B
B
�B
B
B
�B
�B
�B
MB
�B
YB
�B
�B
�B
�B
�B
�B
�B
�B
kB
�B
	B
�B
	B
	B
CB
xB
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 'B
 �B
 �B
 �B
 �B
 �B
!-B
"4B
!�B
"hB
"hB
"�B
"�B
#�B
#�B
$B
$@B
$tB
$tB
$@B
$�B
%FB
%zB
&�B
&�B
&�B
&�B
'RB
&�B
'�B
'�B
($B
(�B
(�B
(�B
(�B
(�B
)_B
)�B
)�B
*eB
*0B
*�B
+�B
+�B
+�B
+�B
+�B
,�B
-B
,�B
-�B
.B
.B
/B
/�B
0�B
0�B
0�B
1'B
0�B
1'B
1[B
1�B
1�B
1'B
1[B
33B
3�B
3hB
3hB
3�B
3hB
3�B
3�B
49B
3�B
4B
5B
5B
5?B
4�B
5tB
5tB
5�B
5�B
5tB
5�B
6B
6B
5�B
6FB
6zB
6�B
6�B
6�B
6�B
6�B
8�B
8RB
8�B
9$B
9�B
9�B
9XB
:�B
:*B
;�B
;�B
;�B
;�B
;�B
;�B
;�B
<B
<jB
<6B
<6B
<�B
<�B
<�B
<jB
=B
?�B
>�B
>�B
?}B
@B
@�B
@�B
@OB
A�B
@�B
C�B
C-B
EmB
EmB
EmB
F?B
FB
F�B
FtB
F�B
F�B
F�B
F�B
F�B
GB
GzB
GB
GB
H�B
HB
H�B
I�B
I�B
I�B
J�B
J�B
K)B
K�B
L�B
M�B
M6B
NpB
O�B
PB
OBB
O�B
OBB
PHB
PB
O�B
P}B
Q�B
QNB
QB
P}B
R�B
RTB
R B
Q�B
R B
S[B
S�B
S�B
S[B
S�B
S�B
S�B
S�B
T,B
TaB
TaB
T�B
TaB
T�B
U�B
U2B
U�B
U�B
UgB
VB
U�B
U�B
V�B
VmB
W?B
W�B
WsB
WsB
XEB
XEB
XEB
XEB
X�B
X�B
X�B
YB
YKB
YB
YB
YB
YB
YB
Y�B
Y�B
ZQB
ZB
ZQB
Z�B
Z�B
ZQB
ZB
[�B
[�B
[�B
\)B
\)B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]dB
]�B
]�B
^B
^B
^jB
^�B
^�B
^�B
_pB
_pB
_;B
_pB
_pB
`vB
bNB
a�B
bB
bB
a�B
a�B
a�B
bB
bNB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
dZB
e,B
e`B
e`B
e�B
f2B
f�B
gB
gB
gB
g8B
g8B
g8B
h
B
h
B
h
B
hsB
hsB
hsB
h>B
h�B
h�B
h�B
j�B
j�B
kQB
kB
k�B
kQB
k�B
k�B
kQB
k�B
kB
k�B
k�B
k�B
l"B
l�B
l"B
k�B
lWB
l�B
l�B
l�B
l�B
l�B
m)B
m]B
n/B
o B
o B
n�B
oiB
oiB
o5B
o�B
o�B
o�B
oiB
p;B
p�B
poB
p�B
poB
p�B
p�B
p�B
q�B
s�B
sB
sMB
sMB
sMB
r�B
s�B
t�B
s�B
tB
tB
t�B
t�B
uZB
u%B
uZB
u�B
u�B
v`B
w�B
w2B
w�B
w�B
x8B
xlB
x�B
x�B
yrB
y	B
y	B
y	B
y	B
zB
{B
|PB
|B
{�B
|B
|B
{B
|PB
|B
}"B
}VB
~(B
}VB
}�B
~(B
}�B
}�B
~]B
~�B
~�B
~]B
~(B
~]B
~]B
~�B
~�B
~�B
~�B
�B
�B
.B
.B
�B
� B
�4B
�4B
�B
� B
�B
�iB
�iB
�;B
��B
�oB
�;B
��B
�oB
��B
�oB
��B
�oB
�AB
��B
�B
��B
��B
�B
��B
��B
�uB
�B
�B
�AB
��B
�GB
�GB
��B
�B
��B
�B
�B
�B
PB
~B
�B
\B
�B
DB
B
"B
(B
�B
�B
"B
�B
�B
\B
"B
\B
�B
VB
�B
VB
VB
�B
�B
�B
�B
B
"B
�B
�B

�B
(B
B
�B
�B
�B
�B
�B
�B
�B
JB
VB
�B
�B
PB
"B
�B
VB
PB
�B
�B
(B
�B
~B
B
~B
JB
JB
�B
JB
�B
JB
�B
B
JB
�B
B
PB
�B
�B
�B
VB
�B
�B
PB
�B
�B
"B
VB
�B
�B
�B
B
PB
~B
�B
�B
�B
~B
~B
�B
�B
~B
JB
JB
JB
�B
�B
�B
B
~B
JB
~B
�B
�B
�B
�B
�B
VB
VB
VB
�B
�B
VB
�B
�B
B
�B
�B
�B
�B
"B
�B
VB
�B
"B
�B
�B
�B
�B
PB
PB
�B
�B
PB
�B
B
�B
PB
�B
PB
�B
B
�B
B
PB
�B
�B
�B
"B
"B
VB
VB
VB
�B
�B
�B
�B
�B
�B
�B
VB
�B
�B
�B
�B
"B
VB
�B
�B
�B
�B
�B
"B
"B
"B
"B
�B
�B
�B
�B
�B
�B
�B
"B
VB
"B
PB
PB
�B
�B
B
PB
JB
B
�B
�B
PB
�B
�B
�B
�B
VB
�B
PB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
.B
�B
bB
bB
bB
�B
bB
bB
�B
bB
hB
:B
4B
hB
B
B
�B
�B
B
B
oB
B
uB
B
:B
B
�B
@B
B
B
�B
uB
B
@B
B
�B
MB
�B
@B
MB
�B
MB
B
IB
B
�B
B
IB
�B
�B
	B
�B
	B
�B
kB
=B
IB
�B
�B
�B
�B
 �B
"4B
(�B
*0B
/�B
1�B
1�B
2aB
5�B
4�B
8�B
8�B
8�B
9XB
9�B
:*B
:�B
<�B
=B
=B
<�B
=�B
>�B
=<B
>wB
A�B
C-B
CaB
D�B
FB
E�B
E�B
EmB
EmB
F?B
EmB
EB
EB
C�B
OB
NB
O�B
K�B
H�B
H�B
J�B
I�B
LdB
N�B
P�B
MjB
aHB
`�B
bNB
^B
^5B
^B
^jB
\�B
\�B
[�B
[�B
[�B
[�B
[#B
[#B
[�B
\�B
\)B
[�B
[�B
\)B
[�B
[�B
\)B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\)B
[WB
\)B
\)B
\]B
\�B
_pB
^�B
_;B
_;B
_;B
_;B
_�B
_pB
_;B
^�B
_pB
`vB
`�B
`�B
`vB
`B
`BB
`B
_�B
_pB
_pB
_B
_pB
_pB
_;B
`B
`�B
`�B
a�B
bB
aHB
bNB
b�B
cTB
b�B
c B
b�B
cTB
c�B
d�B
e,B
d�B
e�B
f�B
gmB
h�B
iyB
jB
pB
q�B
uZB
�uB
��B
�'B
��B
�\B
��B
��B
��B
�:B
��B
�@B
�nB
�B
��B
��B
�FB
�LB
��B
�OB
��B
��B
�B
��B
��B
�CB
ѷB
��B
��B
�B
�B
��B
�B9XB�B�B�B�B(B�B�BhB�BBBFB�B�BqBOB"�B+kB8�B9$B8�B=BOvBbNB[#B\)B[�B[#B\�B]/BbNBa�Bc�Be�Bh�BkQBp�Bw�By�By�B�B�7B�uB��B�\B�SB�:B��B�XB�BɺB҉BуB� BҽB�&B��B�?B�B��B�	B��B�+B�B��B�oB��B�AB��B�B��B�B��B��B��B�B�MB��B��B�B�B�>B�B 4B�BSB%B�B+B1B	lB�BPB.B�B�B�BbB�B�BB�B�B$B$B$B�B�B+B�B�B7BqBxB�B�BBB�BVB �B �B �B!bB �B \B �B �B!-B!-B!-B"�B#nB$tB$B#B"�B#nB#�B$tB&B&�B%zB&B&�B)�B'�B(�B)_B)*B($B(XB'RB&�B&LB%�B$�B,�B,�B$@B-B'�B$�B#�B"�B"hB"4B#:B$tB#nB#B �B�B!-B!�B$@B$�B�BSBD�B0�B�BBDBABGB�B�"BDBBBoBoB  B��B��B�.B�cB��B��B  B��B�.B��B��B��B��B�"B��B�B�B�B�lB�B��B�DB�B�B�2B��B�B�ZB�AB�;B��B��B�B�DB�B�B�B�`B��B�DB��B՛B֡B�aB��B�TB�B�TB��B�HB�,B�}B�TB�NB�gBɺB�-B��B�)B�^B��B�0B�)B�KB��B�BƨB��BɺB�B��B��B�'B�BB��B�qB��B� B��B��B��B��B��B�B�B��B��B��B��B�eB��B��B��B��B��B��B��B��B�4B�6B�B�B��B�FB��B��B��B�RB��B��B�B�\B�4B�OB��B��B�CB�bB�B�xB��B�'B��B��B�7B��B�B��B�1B��B�$B�+B�_B��B��B��B�SB��B��B�4B� B�0B�B�%B�+B�1B��B�B��B�GB�B��B��B��B��B.B��B~�B~�B�B}"BxlBy�By�BzxB~(B��B|B|BxlBxB|�Bu�B{JB{�Bo Bx�BpBn�Bq�BpoBo�Bq�BoiBn�Bm�Bo�Bn�Bn�Bs�Bk�Bl�B�;Bf2Ba�Bd�Bc�Bb�BaB_pB`�B]dBY�BcTB^BU�BP}BOvBd�Bh
BI�BG�BB[B<6B:*B)*B7LB@�B)�B'�B9�B*0B,qB2�B0�B,qB*�B,qB,B,=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                               B
�B
�B
�B
tB
%B
B
�B
9B
SB
�B
9B
�B
9B
9B
mB
SB
9B
�B
9B
B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
_B
�B
	B
	�B

rB
0B
hB
�B
�B
�B
,�B
2B
5tB
<6B
>�B
B�B
P�B
T,B
TB
T{B
T�B
T�B
UgB
WYB
XyB
X�B
ZkB
\�B
d&B
�B
��B
�XB
�B
�aB_B�B6�BY�B[�BoB�yB�OB�B�KB�DB�!B�FB�qB
XB9B 'B($B-�B�B�xB�6B�B��B�)B�+B�TB�CB�B��BcBz�Bq�Bo�BY�B1[B)B�B �BB�B�B?BFBSB;B
��B
��B
�B
�vB
�OB
��B
�]B
�B
]�B
U�B
`�B
"hB	�B	��B	�B	�CB	��B	�XB	��B	�B	�kB	��B	��B	��B	n�B	D�B	+6B	33B	'�B	�B	yB	�B	�B	�B	�B	MB��B��B��B��B��B��B�B�B�B��B�B�B��B�sB�TB�oB�oB��BңB�BѝB��B�uB��B�TB�oB��B��B��B�@B�B��B�?BרB�yB�BߊB�vB�B��B�>B��B��B��B�CB�B�eB�)B�)B�B�B�B��B��B�B�6B�B�oB��B��B�B�B��B�$B��B��B�RB�8B�B�*B�qB�3B�-B��B��B��B��B�LB�	B��B�B��B��B�GB�B��B��B��B�MB��B�B�B�9B��B�B��B�8B��B��B�xB�B�B��B��B	�B	�B	B	?B	_B	�B	EB	\B	aB	B	YB	$B	+B	�B	B		B	�B	�B	�B	B	B	"B	#�B	&LB	)*B	*KB	-wB	1�B	5ZB	7�B	8B	9	B	;�B	A B	FtB	N�B	X_B	_VB	e�B	hsB	rB	r|B	q[B	r�B	r�B	poB	uZB	v�B	xB	|jB	|�B	}�B	}�B	~]B	�B	��B	�UB	��B	��B	�@B	�aB	��B	�B	�7B	��B	��B	�B	�BB	�vB	��B	��B	�2B	��B	��B	��B	�>B	�_B	��B	��B	��B	�TB	��B	�FB	��B	��B	�RB	��B	��B	�B	�PB	��B	�"B	�qB	��B	��B	�3B	ĶB	�B	�9B	żB	�tB	�+B	��B	�#B	�dB	̳B	��B	�"B	�4B	�B	�B	ѷB	ЗB	уB	�,B	�
B	�_B	��B	�QB	ںB	��B	��B	�)B	��B	�B	��B	�B	�NB	�@B	��B	�B	��B	�2B	��B	�B	��B	�B	��B	�B	�fB	��B	��B	�XB	�B	��B	�B	��B	��B	�B	�"B	�wB	�B	�UB	��B	�3B	�aB	�|B	�-B	�aB	�|B	�B	�B	�|B	�B	�|B	�|B	��B	�B	�B	�|B	�|B	�aB	��B	�`B	�B	��B	��B	��B	��B	�fB	�	B	�	B	�RB	�	B	�DB	�DB	�^B	�B	�DB	�B	�DB	��B	�JB	��B	�}B	�]B	�]B	�wB	��B	��B	��B	��B	��B	��B	�}B	��B	��B	�}B
 B
UB
AB
 B
oB
B
GB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
EB
1B
�B
1B
fB
fB
�B
	B
	�B
	RB
	�B
	�B

	B

#B
�B

�B
B

�B
)B

�B
DB

�B
B

�B
�B
�B
6B
B
PB
jB
PB
�B
�B
VB
"B
<B
�B
�B
�B
�B
oB
hB
�B
@B
[B
�B
B
�B
@B
uB
[B
�B
FB
�B
MB
MB
�B
�B
�B
�B
mB
sB
sB
sB
�B
yB
_B
EB
�B
�B
B
1B
B
�B
B
B
	B
�B
	B
�B
=B
�B
xB
xB
�B
�B
�B
�B
�B
�B
B
�B
pB
pB
B
pB
�B
�B
 BB
 vB
 �B
!HB
 �B
 �B
!-B
!|B
"B
"�B
"�B
"�B
"�B
$&B
$ZB
#�B
$&B
$tB
$�B
%�B
%�B
%�B
&�B
&�B
'8B
($B
(�B
)_B
)DB
)DB
)�B
)yB
)�B
)�B
)�B
)�B
)�B
*�B
,qB
+�B
+�B
+�B
+�B
+�B
,B
,"B
,�B
,"B
-B
-�B
-�B
-�B
-]B
-�B
-�B
-�B
-�B
-�B
.IB
.�B
.cB
.IB
.�B
/B
/5B
/B
/5B
/OB
/�B
1�B
1B
1vB
1�B
2�B
2-B
2aB
3hB
3B
4�B
4nB
4nB
4B
4B
4B
4TB
4�B
4�B
4�B
4�B
5�B
5%B
5B
5ZB
6`B
8�B
7LB
7�B
8�B
8�B
9>B
9$B
9>B
:�B
:xB
<�B
<�B
>�B
=�B
>BB
>�B
>�B
?.B
>�B
>�B
?B
?.B
?B
?.B
?�B
?�B
?�B
@4B
A�B
@�B
BB
B�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
FtB
F%B
H1B
H�B
HfB
G�B
H1B
G�B
H�B
H�B
H�B
I�B
J=B
I�B
IlB
IRB
L0B
JrB
J#B
J=B
K)B
LB
K�B
LdB
K�B
K�B
K�B
LB
L0B
L�B
L�B
L�B
MB
MB
M�B
N<B
M�B
NB
N<B
M�B
N�B
N<B
NpB
O\B
OBB
O�B
PB
O�B
PHB
P�B
P�B
P�B
P�B
QNB
Q B
QhB
RB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S[B
TaB
T{B
TaB
T{B
T{B
T�B
T�B
T�B
T�B
T�B
UB
UB
T�B
UgB
U�B
VB
V9B
VSB
VmB
VSB
W
B
W$B
W?B
W$B
W�B
W�B
W�B
XB
XEB
ZB
Z�B
Z7B
ZkB
ZkB
Z7B
Z7B
ZkB
Z�B
[�B
\B
[�B
\B
\B
\B
\B
\CB
\B
\B
\B
\CB
]B
]IB
]�B
^B
^B
^�B
^�B
_;B
_�B
_pB
_VB
_�B
_�B
_�B
`�B
`vB
`vB
`�B
`�B
`�B
`�B
abB
aHB
bNB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
c�B
d&B
d@B
c�B
d�B
d�B
dtB
dZB
d�B
eB
eFB
d�B
d�B
e,B
e�B
fB
gRB
g�B
gRB
gRB
g�B
g�B
g�B
g�B
h$B
h$B
h
B
h�B
iB
h�B
i*B
h�B
h�B
iDB
i�B
k6B
l"B
k�B
k�B
k�B
k�B
kQB
l=B
l�B
l=B
l�B
l�B
mwB
m�B
m�B
m�B
m�B
ncB
n�B
o5B
pUB
o�B
pUB
pUB
qB
p�B
q'B
qvB
q�B
qvB
qvB
q�B
q�B
sB
tB
uB
tnB
tB
tnB
tnB
tB
t�B
uB
u�B
u�B
vzB
u�B
v`B
vzB
v`B
vB
v�B
w2B
v�B
v�B
vzB
v�B
v�B
v�B
wB
wB
w�B
xB
xB
w�B
w�B
xB
xlB
x�B
x�B
x8B
x�B
x8B
x�B
y$B
y�B
y$B
y�B
y�B
y$B
y�B
z*B
y�B
y�B
y�B
z�B
y�B
z^B
y�B
z^B
z^B
{JB
{0B
z�B
z^B
z^B
z�B
{0B
{�B
{�B
|6B
|jG�O�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
SB
YB
_B
�B
�B
YB
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
1B
�B
�B
�B
SB
YB
B
B
B
_B
SB
�B
�B
+B
�B
+B
�B
+B
�B
�B
�B
B
�B
YB
�B
�B
�B
+B
�B
_B
�B
�B
MB
�B
�B
�B
�B
�B
�B
�B
B
MB
�B
�B
SB
�B
%B
�B
%B
�B
�B
B
�B
�B
�B
YB
�B
%B
%B
%B
SB
�B
�B
�B
B
B
�B
�B
B
�B
�B
�B
�B
�B
B
B
B
MB
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
%B
�B
�B
�B
B
SB
�B
�B
%B
�B
YB
�B
�B
�B
YB
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
SB
�B
�B
B
�B
B
SB
�B
SB
�B
%B
�B
�B
YB
YB
�B
�B
�B
�B
�B
+B
+B
�B
�B
�B
�B
�B
�B
+B
�B
YB
�B
�B
�B
�B
�B
�B
YB
YB
YB
YB
%B
%B
%B
%B
�B
�B
%B
YB
�B
YB
�B
�B
�B
%B
SB
�B
�B
SB
�B
�B
�B
B
B
�B
�B
�B
B
�B
�B
+B
+B
+B
+B
�B
�B
�B
1B
1B
fB
�B
�B
�B
�B
1B
�B
�B
1B
�B
	�B

rB
	lB
	�B

=B

=B
�B

�B

=B

=B

�B

=B
�B
DB

rB
DB
B
xB
DB
DB
�B
�B
JB
xB
JB
�B
�B
B
xB
�B
.B
�B
MB
�B
MB
�B
MB
�B
�B
B
@B
�B
@B
�B
�B
uB
�B
�B
$B
�B
�B
�B
kB
 �B
"NB
'�B
)�B
*B
*B
-�B
,�B
0�B
0�B
0�B
1vB
2B
2GB
2�B
4�B
5%B
5%B
4�B
5�B
6�B
5ZB
6�B
:B
;JB
;B
<�B
>(B
=�B
=�B
=�B
=�B
>]B
=�B
="B
="B
;�B
G+B
F%B
G�B
DB
@�B
@�B
CB
A�B
D�B
F�B
IB
E�B
YeB
X�B
ZkB
VB
VSB
VB
V�B
T�B
T�B
TB
TB
TB
S�B
S@B
S@B
S�B
T�B
TFB
TB
S�B
TFB
TB
S�B
TFB
S�B
S�B
S�B
S�B
S�B
TB
S�B
S�B
TB
TB
S�B
S�B
TB
S�B
TFB
SuB
TFB
TFB
T{B
UB
W�B
V�B
WYB
WYB
WYB
WYB
W�B
W�B
WYB
V�B
W�B
X�B
X�B
X�B
X�B
X+B
X_B
X+B
W�B
W�B
W�B
W$B
W�B
W�B
WYB
X+B
X�B
X�B
ZB
Z7B
YeB
ZkB
[	B
[qB
[	B
[=B
[	B
[qB
\B
\�B
]IB
\�B
]�B
^�B
_�B
`�B
a�B
b4B
h$B
i�B
mwB
z�B
��B
�DB
��B
�yB
��B
��B
��B
�WB
��B
�]B
��B
�)B
��B
��B
�dB
�jB
�B
�mB
��B
��B
�/B
�B
��B
�`B
��B
� B
��B
�/B
�+B
�B
�0B1vB�B�B�BBEBB�B	�B	�B)B6BdB�B�B�BmB�B#�B0�B1AB0�B5%BG�BZkBS@BTFBS�BS@BUBUMBZkBY�B[�B]�B`�BcnBh�Bo�Bq�Bq�Bw�B�;B�xB��B�_B�VB�=B��B�\B�B��BʌBɆB�#B��B�)B��B�BBуB��B�B��B�/B�B��B�sB��B�DB��B�B��B�B��B��B��B�B�QB��B��B��B�B�AB�B�8B��B�VB�(B��B�.B 4BoB�BSB1B�B�B�BfB�B�BB�B�B(B(B(B�B�B.B�B�B:BuB{B�B�BBB�BYB�B�B�BeB�B_B�B�B1B1B1B�BqBxBB	B�BqB�BxBB�B~BB�B!�B�B �B!bB!-B 'B \BVB�BOB�B�B$�B$�BCB%B�B�B�B�BkB7B=BxBqB	B�B�B1B�BCB�B�BVB<�B(�B��BBGB�DB�JB �B�%BGB�"B�B�rB�rB�B��B��B�2B�fB��B��B�B��B�2B��B��B�B�B�%B��B�B�B�B�oB�B�B�GB�B�B�5B��B�B�]B�DB�>B��B�B�B�HB�BٴB�B�cB��B�HB��B͟BΥB�dB��B�XBخB�XB��B�KB�0BȀB�XB�RB�jB��B�0B��B�-B�aB��B�3B�-B�OB��B�B��B��B��B� B��B��B�*B�FB��B�tB��B�$B��B��B��B��B��B�!B�B��B��B��B�B�hB��B��B��B��B��B��B��B��B�QB�:B�/B�5B��B�dB��B��B��B�VB��B��B�5B�yB�QB�mB��B��B�aB�B�B��B��B�EB��B��B�TB��B� B�B�NB�B�BB�HB�}B�B�B��B�pB��B��B�RB�B�3B�#B~BBHB�OB~�B}<B~B{dB{0B|�B|�B|�B{�BwLB}�Bv�Bv�Bw�Bu?Bp�Bq�Bq�Br�BvFBx�Bt9Bt9Bp�Bp!BuBm�BshBtBgBp�Bh$Bf�Bi�Bh�Bg�Bi�Bg�Bf�Be�Bg�Bf�Bf�BlBc�Bd�ByXB^OBZB\�B[�B[	BY1BW�BX�BU�BQ�B[qBVBM�BH�BG�B]B`'BBB?�B:xB4TB2GB!HB/iB8�B"B B2B"NB$�B*�B(�B$�B"�B$�B$&B$ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<jTs<#�
<#�
<��6<�v�<#�
<#�
<5
�<#�
<#�
<#�
<G��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<o�<,�<5̲<u��<#�
<#�
<#�
<#�
<#�
<#�
<-�<8��<�o <�SG<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(X^<�U�<���<#�
<#�
<#�
<#�
<#�
<���<�&<#�
<N�=��<��<#�
<#�
<.�U<;I%<j5�<#�
<*�<#�
<#�
<#�
<�o<�*�<ɐ9<;#<#�
<b��<#�
<#�
<#�
<#�
<#�
<8��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<1�<#�
<#�
<#�
<0 <60�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0076(+/-0.0052)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NO correction for Conductivity Thermal Mass (CTM) is applied;    OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.0076(+/-0.0052)                                                                                                                     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2021011808205320210118082053IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021012810004420210128100044QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021012810004420210128100044QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714015320210427140153IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242320230426192423IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242320230426192423IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242320230426192423IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                