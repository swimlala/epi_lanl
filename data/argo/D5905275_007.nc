CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-03-24T22:27:28Z creation; 2023-04-26T19:14:25Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180324222728  20230426191425  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_007                 7316_008644_007                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�U���l@�U���l11  @�U�
�L0@�U�
�L0@)��r\=�@)��r\=��d(�`�G��d(�`�G�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @B�\@�G�@�G�@��R@�  AG�A��A   A,��AAG�A`��A�  A��A�  A�  A�  A�  A߮A�\)A�\)B  B�
B  B Q�B(Q�B0Q�B8(�B@  BH  BP  BX(�B_�
Bg�
Bo�
Bw�
B�
B�  B�  B�  B�  B�  B�{B�Q�B�{B��
B��
B�  B��B��B�  B�  B�  B��B�  B�  B��B�  B�  B�  B�  B�{B�  B��B�  B�{B�  B��B��C��C
=C  C  C
  C
=C�C��C��C��C��C  C  C��C��C��C"  C$
=C&  C(  C*  C+��C-��C0  C2  C4  C5��C7��C9��C;��C>
=C@  CA��CC��CF
=CH
=CJ  CL  CN  CO��CR  CT  CV
=CX
=CZ
=C\  C^  C`  Cb  Cd  Cf  Cg��Cj  Cl
=Cn
=Cp  Cr  Cs��Cv  Cx{Cz
=C|
=C~
=C��C�  C�C�  C�  C���C���C�  C�  C�C�  C�  C�C�C�C�  C���C���C�  C���C���C�  C�C�  C���C�  C�  C�  C���C���C�  C�
=C�C�  C���C���C���C�
=C�C�  C���C���C���C���C���C���C���C���C�  C�  C�  C���C���C���C�  C�C�
=C�  C�C�C���C���C���C�  C�C�  C���C�  C�C�  C���C���C�  C�C�C���C���C�  C�C�C�C�
=C�C�  C�  C���C���C���C���C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�  C���C�C�C�  C�  C���C�C�C�  C�C�  C���C���C�  C�  C���C���C�  C�  C�  C���C�C�
=C�  C���C���C�  C�D �D ��D  D}qD  D��D�D��D�D��D�D� D  D� D  D� D�qD� D	  D	��D
D
��D
�qDz�D�qD� D�qD}qD�qD� DD�D�D��D�D}qD  D��D�D� D  D}qD�qD}qD�qD��DD�D�D� D�qD}qD�qD� D  D� D  D��D�qDz�D�qD��D  D}qD�qD z�D!  D!��D"�D"� D"�qD#z�D$  D$��D%  D%}qD%�qD&}qD'�D'� D(  D(� D)  D)� D)��D*� D+�D+��D+�qD,� D-�D-��D.  D.��D/D/��D0�D0� D1  D1� D2  D2� D3  D3��D4D4� D4��D5z�D5�qD6��D6�qD7z�D7�qD8� D9  D9}qD9��D:}qD;  D;��D<  D<� D<�qD=}qD>�D>��D?  D?� D@  D@��DADA��DA�qDB}qDC  DC��DD�DD}qDD�qDE��DFDF��DF�qDG� DG�qDH}qDI  DI� DJ  DJ� DK  DK� DL  DL}qDM  DM��DM�qDN� DO�DO� DP  DP� DQ�DQ� DR  DR�DS  DS��DT  DT}qDT�qDU}qDV  DV��DW  DW}qDX  DX� DY  DY� DZ�DZ� DZ�qD[� D[�qD\}qD\�qD]��D^�D^��D_�D_}qD_�qD`� D`�qDa}qDb�Db�Dc�Dc��DdDd��De�De}qDf  Df��Df�qDg� Dh�Dh��Di  Di� Di�qDj}qDk  Dk� Dl  Dl}qDl�qDm}qDm�qDn}qDn�qDo� Do�qDp}qDq  Dq� Dr  Dr� Dr�qDsz�Ds�qDt� Dt�qDu}qDu�qDv}qDv�qDw� Dx  Dx}qDx�qDy}qDy�qDz� D{  D{}qD{��D|}qD|�qD}}qD~  D~��D  D� D�  D�AHD��HD�� D�  D�@ D��HD��HD�HD�AHD��HD�D�HD�@ D��HD�� D���D�@ D��HD�� D�  D�@ D�� D�� D���D�>�D�� D���D�  D�AHD�� D���D��qD�@ D�� D�� D��D�AHD��HD�� D�  D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD��HD��HD��D�AHD�~�D���D���D�@ D�� D�� D�  D�>�D�~�D���D���D�@ D�~�D��qD��qD�@ D�� D�� D�HD�AHD�� D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D���D�=qD�� D���D���D�>�D�~�D��HD�HD�AHD��HD�� D�  D�B�D�� D���D�  D�AHD��HD�� D���D�@ D���D�D��D�B�D��HD��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�� D��HD�HD�>�D�}qD���D�  D�@ D��HD��HD���D�@ D��HD��HD��D�@ D�~�D�� D�HD�B�D��HD��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�=qD�~�D�� D�HD�B�D�~�D�� D�HD�@ D�}qD�� D�HD�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D���D��HD�  D�>�D�� D��HD�  D�>�D�}qD�� D�HD�>�D�}qD�� D��D�AHD��HD�� D��qD�=qD�~�D�� D�  D�>�D�� D��HD���D�>�D�}qD��qD��qD�>�D��HD�D�  D�@ D���D�� D���D�=qD�}qD�� D�HD�@ D�~�D��HD��D�AHD�� D���D���D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�� D���D�  D�AHD�� D���D�HD�@ D�� D�� D���D�>�D�~�D���D�  D�@ D�� D���D�  D�@ D�~�D�� D���D�@ DÁHD�D�HD�>�DĀ Dľ�D�  D�@ Dŀ Dž�D�  D�AHDƀ D�� D�HD�@ Dǀ DǾ�D�HD�AHDȀ D�� D�HD�>�D�~�D�� D�HD�@ D�~�Dʾ�D���D�@ DˁHD��HD�  D�>�D�~�D̾�D�  D�AHD̀ D�� D�  D�@ D�~�D�� D�HD�AHDρHD��HD�HD�AHD�~�D�� D�  D�@ D�~�DѾ�D���D�>�DҀ D�� D�  D�@ DӀ D��HD�HD�@ D�~�DԾ�D���D�@ DՁHD�� D�  D�@ Dւ�D�� D�  D�>�D�~�D׾�D���D�@ D؀ D�� D���D�=qDـ Dپ�D��qD�@ DځHD��HD�  D�AHDۀ D�� D�HD�AHD܀ D�� D�  D�@ D݀ D�� D�HD�@ Dހ D�� D�  D�>�D߀ D�� D��qD�>�D�~�D��HD��D�AHD�HD��HD�HD�@ D�~�D�� D�  D�>�D�~�D�� D���D�@ D�HD��HD���D�@ D� D徸D�  D�AHD� D澸D���D�>�D�~�D�� D�  D�@ D� D�qD�  D�AHD� D�� D�  D�AHD�HD��HD�HD�@ D� D��HD�HD�AHD� D�� D��qD�>�D�~�D���D�  D�AHD�HD��HD��D�@ D�~�DﾸD�  D�@ D�� D��HD�HD�@ D�~�D�� D�HD�AHD�HD�D��qD�>�D� D�D��qD�@ D�~�D���D���D�=qD�� D��HD��D�B�D��HD���D���D�@ D�~�D�D�HD�AHD��HD�� D�HD�@ D�� D�� D�  D�>�D�~�D��\>�G�?.{?�  ?���?Ǯ?�@
=q@(�@.{@B�\@W
=@k�@z�H@���@�@��R@��@��@��H@�ff@��@�(�@��
@�{@�
=AG�A
=A
�HA\)A33AQ�A{A"�\A'
=A+�A0  A5�A9��A>�RAC33AG�AL(�AQG�AVffA\(�A`��Ae�Ai��Ao\)Au�Ay��A~�RA���A�(�A��RA�G�A�(�A�ffA���A��HA�A�Q�A�33A�A�  A��A���A��A�=qA�(�A�ffA�G�A��A�{A�Q�A��HA��A�
=A���A�z�AƸRA�G�A˅A�A�Q�AӅA�{Aأ�A��HA�p�A�  A��HA�A�Q�A�\A��A�  A�\A�p�A�Q�A��\A��A��Bp�B�RB(�Bp�B�\B�
B	�B
�\B  BG�B�RB�
B�B�\B  Bp�B�RB  B�B�\B�
Bp�B�RB   B!G�B"�\B#�
B%G�B&�RB((�B)G�B*�\B+�
B-G�B.�HB0  B1G�B2�\B4  B5G�B6�HB8(�B9��B:�RB<  B=p�B>�HB@z�BA�BC33BDz�BEBG
=BH��BJ{BK�BL��BN=qBO�BP��BRffBS�
BUp�BV�HBX  BYG�BZ�RB\(�B]B_\)B`��Ba�Bc33Bdz�Bf{Bg�Bh��BjffBk�Bm�Bn=qBo�BqG�Br�RBt(�Bu��Bv�RBx(�Byp�Bz�HB|z�B}�B33B�=qB���B���B�ffB��B��
B��\B�G�B��B��\B�G�B�{B���B��B�(�B���B�p�B�(�B��HB��B�Q�B�
=B��B�Q�B���B��B�Q�B��B�B�ffB���B���B�ffB�
=B��B�Q�B��HB�p�B�{B���B�33B�B�=qB��\B��HB�33B��B��
B�{B�=qB�ffB��\B���B��RB��HB�
=B�33B�\)B�p�B��B���B��B�B��
B��B�{B�(�B�Q�B�ffB��\B��\B��\B��RB���B��HB�
=B��B�G�B�p�B���B���B��B��B�{B�=qB��\B��RB���B���B��B�\)B��B��B��B�(�B�ffB���B��HB�
=B�33B�\)B���B��B�{B�ffB���B��HB�
=B�33B�p�B��B��B�=qB�z�B���B��B�\)B���B��
B�(�B�z�B���B�33B��B��
B�{B�ffB��RB�
=B�\)B��B�{B�ffB��RB�33B��B��
B�=qB��\B���B�\)B��B�  B�ffB���B��B�p�B��
B�(�B��\B�
=B�p�B��
B�ffB��HB�G�B�B�Q�B��RB�33B��B�(�B��\B�
=B��B��B�z�B���B�p�B�  B��\B�
=B���B�{B��RB�G�B��
B�ffB���BÅB�{BĸRB�G�B��
B�ffB�
=BǙ�B�(�BȸRB�\)B��Bʏ\B��B�B�ffB�
=BͮB�Q�B���Bϙ�B�=qB��HBхB�(�B���BӅB�(�B���BՅB�{B���B�p�B�{BظRB�p�B�{B���B�p�B�(�B���B݅B�(�B���B߅B�(�B��HB�B�=qB��HB㙚B�Q�B���B�B�ffB�
=B�B�ffB��B�B�z�B��B��
B�z�B��B�B�z�B�
=B�B�ffB�
=B�B�Q�B���B�B�=qB���B��B�(�B���B�p�B�{B��RB�p�B�{B��RB�G�B�  B���B�G�B��B��\B�33B�C 33C z�C ��C�CffCC
=C\)C�C  CQ�C��C�C33Cz�CC{C\)C�C  CQ�C��C��CG�C�\C�
C(�Cp�CC	
=C	\)C	�C

=C
Q�C
��C
�C33Cp�C�RC��C=qCz�C�RC��C(�C\)C�C�C��C�HC  C{C=qCG�C\)Cz�C��CC�
C��C{C�C=qC\)Cp�C�C��C�C��C��C
=C33CQ�Cp�C�C��C�RC��C�C
=C(�CG�CffC�\C�CC�
C��C{C33C\)C�C��CC�
C  C{C33C\)Cz�C��C��C�C
=C�CG�Cp�C�\CC�C
=C(�CG�Cp�C��C��C��C{C33C\)Cz�C�C�
C  C(�CG�Cp�C�\C�RC�
C  C(�C\)Cz�C��C�RC�
C  C(�CQ�Cp�C��C�RC�
C��C{CG�Cp�C�C��CC�HC  C�CG�Cp�C�\C�RC�
C��C{C(�CQ�Cp�C��CC�C
=C(�CG�CffC�C�C�
C��C�C=qC\)Cp�C�\C�RC�HC  C(�C=qC\)Cz�C��CC�C 
=C (�C G�C p�C ��C C �C!
=C!�C!G�C!p�C!��C!C!�C"
=C"(�C"G�C"p�C"��C"��C"��C#�C#=qC#ffC#�C#�RC#�HC${C$33C$Q�C$p�C$��C$��C$��C%�C%G�C%p�C%�\C%�RC%�
C&  C&(�C&\)C&�C&��C&��C&�C'
=C'=qC'ffC'�\C'�C'�
C(  C((�C(Q�C(p�C(�\C(�RC(�C){C)=qC)Q�C)z�C)�C)�HC*  C*(�C*G�C*z�C*�C*�
C+  C+�C+=qC+p�C+��C+��C+�C,
=C,=qC,p�C,��C,�RC,�HC-{C-G�C-p�C-�\C-�RC-�C.�C.G�C.ffC.��C.��C.��C/�C/G�C/p�C/��C/�
C0
=C033C0G�C0z�C0�RC0�C1
=C133C1\)C1��C1��C1��C2�C2G�C2z�C2�RC2�HC3
=C333C3p�C3��C3��C3��C433C4p�C4��C4��C4��C533C5p�C5��C5��C6  C6=qC6z�C6�C6�
C7{C7Q�C7�C7�RC7�C833C8p�C8��C8��C9{C9G�C9�C9�C9�C:(�C:p�C:��C:��C;
=C;Q�C;�C;�RC;�C<33C<p�C<��C<�
C=�C=\)C=�C=C>
=C>G�C>z�C>�RC>��C?33C?ffC?��C?�HC@(�C@\)C@�\C@��CA�CAQ�CA�CACB
=CBG�CB�CB�RCC  CCG�CCz�CC�CD  CD=qCDp�CD�CD��CE33CEffCE��CE�CF(�CFffCF��CF�HCG(�CGffCG��CG��CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           ?��@   @B�\@�G�@�G�@��R@�  AG�A��A   A,��AAG�A`��A�  A��A�  A�  A�  A�  A߮A�\)A�\)B  B�
B  B Q�B(Q�B0Q�B8(�B@  BH  BP  BX(�B_�
Bg�
Bo�
Bw�
B�
B�  B�  B�  B�  B�  B�{B�Q�B�{B��
B��
B�  B��B��B�  B�  B�  B��B�  B�  B��B�  B�  B�  B�  B�{B�  B��B�  B�{B�  B��B��C��C
=C  C  C
  C
=C�C��C��C��C��C  C  C��C��C��C"  C$
=C&  C(  C*  C+��C-��C0  C2  C4  C5��C7��C9��C;��C>
=C@  CA��CC��CF
=CH
=CJ  CL  CN  CO��CR  CT  CV
=CX
=CZ
=C\  C^  C`  Cb  Cd  Cf  Cg��Cj  Cl
=Cn
=Cp  Cr  Cs��Cv  Cx{Cz
=C|
=C~
=C��C�  C�C�  C�  C���C���C�  C�  C�C�  C�  C�C�C�C�  C���C���C�  C���C���C�  C�C�  C���C�  C�  C�  C���C���C�  C�
=C�C�  C���C���C���C�
=C�C�  C���C���C���C���C���C���C���C���C�  C�  C�  C���C���C���C�  C�C�
=C�  C�C�C���C���C���C�  C�C�  C���C�  C�C�  C���C���C�  C�C�C���C���C�  C�C�C�C�
=C�C�  C�  C���C���C���C���C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�  C���C�C�C�  C�  C���C�C�C�  C�C�  C���C���C�  C�  C���C���C�  C�  C�  C���C�C�
=C�  C���C���C�  C�D �D ��D  D}qD  D��D�D��D�D��D�D� D  D� D  D� D�qD� D	  D	��D
D
��D
�qDz�D�qD� D�qD}qD�qD� DD�D�D��D�D}qD  D��D�D� D  D}qD�qD}qD�qD��DD�D�D� D�qD}qD�qD� D  D� D  D��D�qDz�D�qD��D  D}qD�qD z�D!  D!��D"�D"� D"�qD#z�D$  D$��D%  D%}qD%�qD&}qD'�D'� D(  D(� D)  D)� D)��D*� D+�D+��D+�qD,� D-�D-��D.  D.��D/D/��D0�D0� D1  D1� D2  D2� D3  D3��D4D4� D4��D5z�D5�qD6��D6�qD7z�D7�qD8� D9  D9}qD9��D:}qD;  D;��D<  D<� D<�qD=}qD>�D>��D?  D?� D@  D@��DADA��DA�qDB}qDC  DC��DD�DD}qDD�qDE��DFDF��DF�qDG� DG�qDH}qDI  DI� DJ  DJ� DK  DK� DL  DL}qDM  DM��DM�qDN� DO�DO� DP  DP� DQ�DQ� DR  DR�DS  DS��DT  DT}qDT�qDU}qDV  DV��DW  DW}qDX  DX� DY  DY� DZ�DZ� DZ�qD[� D[�qD\}qD\�qD]��D^�D^��D_�D_}qD_�qD`� D`�qDa}qDb�Db�Dc�Dc��DdDd��De�De}qDf  Df��Df�qDg� Dh�Dh��Di  Di� Di�qDj}qDk  Dk� Dl  Dl}qDl�qDm}qDm�qDn}qDn�qDo� Do�qDp}qDq  Dq� Dr  Dr� Dr�qDsz�Ds�qDt� Dt�qDu}qDu�qDv}qDv�qDw� Dx  Dx}qDx�qDy}qDy�qDz� D{  D{}qD{��D|}qD|�qD}}qD~  D~��D  D� D�  D�AHD��HD�� D�  D�@ D��HD��HD�HD�AHD��HD�D�HD�@ D��HD�� D���D�@ D��HD�� D�  D�@ D�� D�� D���D�>�D�� D���D�  D�AHD�� D���D��qD�@ D�� D�� D��D�AHD��HD�� D�  D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD��HD��HD��D�AHD�~�D���D���D�@ D�� D�� D�  D�>�D�~�D���D���D�@ D�~�D��qD��qD�@ D�� D�� D�HD�AHD�� D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D���D�=qD�� D���D���D�>�D�~�D��HD�HD�AHD��HD�� D�  D�B�D�� D���D�  D�AHD��HD�� D���D�@ D���D�D��D�B�D��HD��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�� D��HD�HD�>�D�}qD���D�  D�@ D��HD��HD���D�@ D��HD��HD��D�@ D�~�D�� D�HD�B�D��HD��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�=qD�~�D�� D�HD�B�D�~�D�� D�HD�@ D�}qD�� D�HD�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D���D��HD�  D�>�D�� D��HD�  D�>�D�}qD�� D�HD�>�D�}qD�� D��D�AHD��HD�� D��qD�=qD�~�D�� D�  D�>�D�� D��HD���D�>�D�}qD��qD��qD�>�D��HD�D�  D�@ D���D�� D���D�=qD�}qD�� D�HD�@ D�~�D��HD��D�AHD�� D���D���D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�� D���D�  D�AHD�� D���D�HD�@ D�� D�� D���D�>�D�~�D���D�  D�@ D�� D���D�  D�@ D�~�D�� D���D�@ DÁHD�D�HD�>�DĀ Dľ�D�  D�@ Dŀ Dž�D�  D�AHDƀ D�� D�HD�@ Dǀ DǾ�D�HD�AHDȀ D�� D�HD�>�D�~�D�� D�HD�@ D�~�Dʾ�D���D�@ DˁHD��HD�  D�>�D�~�D̾�D�  D�AHD̀ D�� D�  D�@ D�~�D�� D�HD�AHDρHD��HD�HD�AHD�~�D�� D�  D�@ D�~�DѾ�D���D�>�DҀ D�� D�  D�@ DӀ D��HD�HD�@ D�~�DԾ�D���D�@ DՁHD�� D�  D�@ Dւ�D�� D�  D�>�D�~�D׾�D���D�@ D؀ D�� D���D�=qDـ Dپ�D��qD�@ DځHD��HD�  D�AHDۀ D�� D�HD�AHD܀ D�� D�  D�@ D݀ D�� D�HD�@ Dހ D�� D�  D�>�D߀ D�� D��qD�>�D�~�D��HD��D�AHD�HD��HD�HD�@ D�~�D�� D�  D�>�D�~�D�� D���D�@ D�HD��HD���D�@ D� D徸D�  D�AHD� D澸D���D�>�D�~�D�� D�  D�@ D� D�qD�  D�AHD� D�� D�  D�AHD�HD��HD�HD�@ D� D��HD�HD�AHD� D�� D��qD�>�D�~�D���D�  D�AHD�HD��HD��D�@ D�~�DﾸD�  D�@ D�� D��HD�HD�@ D�~�D�� D�HD�AHD�HD�D��qD�>�D� D�D��qD�@ D�~�D���D���D�=qD�� D��HD��D�B�D��HD���D���D�@ D�~�D�D�HD�AHD��HD�� D�HD�@ D�� D�� D�  D�>�D�~�G�O�>�G�?.{?�  ?���?Ǯ?�@
=q@(�@.{@B�\@W
=@k�@z�H@���@�@��R@��@��@��H@�ff@��@�(�@��
@�{@�
=AG�A
=A
�HA\)A33AQ�A{A"�\A'
=A+�A0  A5�A9��A>�RAC33AG�AL(�AQG�AVffA\(�A`��Ae�Ai��Ao\)Au�Ay��A~�RA���A�(�A��RA�G�A�(�A�ffA���A��HA�A�Q�A�33A�A�  A��A���A��A�=qA�(�A�ffA�G�A��A�{A�Q�A��HA��A�
=A���A�z�AƸRA�G�A˅A�A�Q�AӅA�{Aأ�A��HA�p�A�  A��HA�A�Q�A�\A��A�  A�\A�p�A�Q�A��\A��A��Bp�B�RB(�Bp�B�\B�
B	�B
�\B  BG�B�RB�
B�B�\B  Bp�B�RB  B�B�\B�
Bp�B�RB   B!G�B"�\B#�
B%G�B&�RB((�B)G�B*�\B+�
B-G�B.�HB0  B1G�B2�\B4  B5G�B6�HB8(�B9��B:�RB<  B=p�B>�HB@z�BA�BC33BDz�BEBG
=BH��BJ{BK�BL��BN=qBO�BP��BRffBS�
BUp�BV�HBX  BYG�BZ�RB\(�B]B_\)B`��Ba�Bc33Bdz�Bf{Bg�Bh��BjffBk�Bm�Bn=qBo�BqG�Br�RBt(�Bu��Bv�RBx(�Byp�Bz�HB|z�B}�B33B�=qB���B���B�ffB��B��
B��\B�G�B��B��\B�G�B�{B���B��B�(�B���B�p�B�(�B��HB��B�Q�B�
=B��B�Q�B���B��B�Q�B��B�B�ffB���B���B�ffB�
=B��B�Q�B��HB�p�B�{B���B�33B�B�=qB��\B��HB�33B��B��
B�{B�=qB�ffB��\B���B��RB��HB�
=B�33B�\)B�p�B��B���B��B�B��
B��B�{B�(�B�Q�B�ffB��\B��\B��\B��RB���B��HB�
=B��B�G�B�p�B���B���B��B��B�{B�=qB��\B��RB���B���B��B�\)B��B��B��B�(�B�ffB���B��HB�
=B�33B�\)B���B��B�{B�ffB���B��HB�
=B�33B�p�B��B��B�=qB�z�B���B��B�\)B���B��
B�(�B�z�B���B�33B��B��
B�{B�ffB��RB�
=B�\)B��B�{B�ffB��RB�33B��B��
B�=qB��\B���B�\)B��B�  B�ffB���B��B�p�B��
B�(�B��\B�
=B�p�B��
B�ffB��HB�G�B�B�Q�B��RB�33B��B�(�B��\B�
=B��B��B�z�B���B�p�B�  B��\B�
=B���B�{B��RB�G�B��
B�ffB���BÅB�{BĸRB�G�B��
B�ffB�
=BǙ�B�(�BȸRB�\)B��Bʏ\B��B�B�ffB�
=BͮB�Q�B���Bϙ�B�=qB��HBхB�(�B���BӅB�(�B���BՅB�{B���B�p�B�{BظRB�p�B�{B���B�p�B�(�B���B݅B�(�B���B߅B�(�B��HB�B�=qB��HB㙚B�Q�B���B�B�ffB�
=B�B�ffB��B�B�z�B��B��
B�z�B��B�B�z�B�
=B�B�ffB�
=B�B�Q�B���B�B�=qB���B��B�(�B���B�p�B�{B��RB�p�B�{B��RB�G�B�  B���B�G�B��B��\B�33B�C 33C z�C ��C�CffCC
=C\)C�C  CQ�C��C�C33Cz�CC{C\)C�C  CQ�C��C��CG�C�\C�
C(�Cp�CC	
=C	\)C	�C

=C
Q�C
��C
�C33Cp�C�RC��C=qCz�C�RC��C(�C\)C�C�C��C�HC  C{C=qCG�C\)Cz�C��CC�
C��C{C�C=qC\)Cp�C�C��C�C��C��C
=C33CQ�Cp�C�C��C�RC��C�C
=C(�CG�CffC�\C�CC�
C��C{C33C\)C�C��CC�
C  C{C33C\)Cz�C��C��C�C
=C�CG�Cp�C�\CC�C
=C(�CG�Cp�C��C��C��C{C33C\)Cz�C�C�
C  C(�CG�Cp�C�\C�RC�
C  C(�C\)Cz�C��C�RC�
C  C(�CQ�Cp�C��C�RC�
C��C{CG�Cp�C�C��CC�HC  C�CG�Cp�C�\C�RC�
C��C{C(�CQ�Cp�C��CC�C
=C(�CG�CffC�C�C�
C��C�C=qC\)Cp�C�\C�RC�HC  C(�C=qC\)Cz�C��CC�C 
=C (�C G�C p�C ��C C �C!
=C!�C!G�C!p�C!��C!C!�C"
=C"(�C"G�C"p�C"��C"��C"��C#�C#=qC#ffC#�C#�RC#�HC${C$33C$Q�C$p�C$��C$��C$��C%�C%G�C%p�C%�\C%�RC%�
C&  C&(�C&\)C&�C&��C&��C&�C'
=C'=qC'ffC'�\C'�C'�
C(  C((�C(Q�C(p�C(�\C(�RC(�C){C)=qC)Q�C)z�C)�C)�HC*  C*(�C*G�C*z�C*�C*�
C+  C+�C+=qC+p�C+��C+��C+�C,
=C,=qC,p�C,��C,�RC,�HC-{C-G�C-p�C-�\C-�RC-�C.�C.G�C.ffC.��C.��C.��C/�C/G�C/p�C/��C/�
C0
=C033C0G�C0z�C0�RC0�C1
=C133C1\)C1��C1��C1��C2�C2G�C2z�C2�RC2�HC3
=C333C3p�C3��C3��C3��C433C4p�C4��C4��C4��C533C5p�C5��C5��C6  C6=qC6z�C6�C6�
C7{C7Q�C7�C7�RC7�C833C8p�C8��C8��C9{C9G�C9�C9�C9�C:(�C:p�C:��C:��C;
=C;Q�C;�C;�RC;�C<33C<p�C<��C<�
C=�C=\)C=�C=C>
=C>G�C>z�C>�RC>��C?33C?ffC?��C?�HC@(�C@\)C@�\C@��CA�CAQ�CA�CACB
=CBG�CB�CB�RCC  CCG�CCz�CC�CD  CD=qCDp�CD�CD��CE33CEffCE��CE�CF(�CFffCF��CF�HCG(�CGffCG��CG��CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�4@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�1'A�1'A�1'A�33A�1'A�33A�33A�33A�33A�33A�5?A�5?A�5?A�7LA�7LA�5?A�"�A�K�Aԩ�Aԕ�A�ffA�+A��A�JA���A��A���A�XA�{A�ƨA�v�A�XA�O�A�?}A�;dA�/A�A���A��yA��
AѼjAѓuA�`BA�;dA��HA�A�A���A�|�A�ȴA��A��^A���A�  A�|�A��A�E�A���A��A�\)A���A�t�A�K�A�z�A���A��RA���A�{A��yA�A�A�`BA�ffA��RA��wA���A���A��A���A�=qA��A��PA��A��\A�XA���A|$�At5?AnȴAl�AlI�AiƨAe�Ac?}A_p�A\Q�AW��AT�AP-AM�;AK�^AJ=qAF��AEl�AC
=AA��A@E�A?S�A>9XA=�mA<��A:�DA:��A9�^A6=qA4ZA4�A41A1�
A1G�A0��A0VA0�A1p�A1��A1�;A1C�A0��A/�A.��A.$�A-��A,��A+�mA+�wA+��A+x�A+C�A+�A*ĜA*9XA)�hA'��A&z�A& �A& �A%p�A$  A#`BA#�A"^5A �+A��A��A  A5?A��AbAQ�Av�AbNA�wA%AA�A|�Ar�A�A%A~�AȴA�AĜA��AbAƨA�uA��A��AbAƨAXAȴA�!AVA$�A�A�TA�^AO�A�RAr�A=qA�A�;A�A/A��A�RAjA1'AA��AXAA��A�uA~�AbNA=qA1A�mAO�A�AA�RA�#Al�A/A
��A
�+A
9XA	ƨA	�PA��A�AE�A�AC�A��A�DAv�AVAE�A5?A(�A1A�A��A�HA�mA&�A�uAM�AA�A �A�A�7A ��@�^5@��h@�9X@��!@�M�@���@�r�@��y@��h@��@��@��@�v�@�-@���@�V@�Q�@�"�@�@�ȴ@�ff@�-@���@��@�@�|�@�V@�@�G�@�%@���@�bN@�1@�t�@�ff@�7@��/@�1@㝲@�;d@��H@�v�@��@�X@�r�@�1@߶F@ߍP@�;d@���@���@�^5@���@��@ۍP@�S�@�+@�
=@ڸR@�M�@�@ى7@�hs@�7L@ؓu@�  @�\)@��H@�E�@���@���@Դ9@ԛ�@ԛ�@� �@�@��y@Ұ!@�=q@љ�@��@мj@�r�@�9X@��;@ϝ�@�;d@�^5@́@���@�(�@˅@ʏ\@ɡ�@�X@���@�I�@Ǖ�@�ȴ@Ɨ�@�E�@�J@��@��#@Ų-@�/@�j@��;@�;d@��@���@�V@��@��@�bN@�b@���@�
=@���@���@�~�@�ff@�E�@��@��h@�O�@��@�j@�1'@��;@�t�@�
=@�ff@��@���@�G�@��@���@�z�@�I�@�9X@���@�;d@��R@�~�@�{@���@�O�@��@���@��@�r�@�Q�@� �@��
@��H@�5?@��@���@�p�@�hs@��@��j@��D@�I�@���@���@���@���@�@���@���@�x�@�`B@��@��j@�j@�9X@��w@�+@��@���@�5?@��#@��h@�G�@��9@�bN@�I�@� �@�1@���@��@�K�@�@��@�5?@��#@���@��7@�%@�j@��@���@�|�@�K�@�o@���@�~�@�V@�-@��@�{@���@��T@��h@��9@�Z@���@���@�|�@���@�n�@���@���@�`B@��9@�bN@��@��m@���@��P@�"�@���@�~�@�E�@�5?@��@�@��#@��h@�X@�/@�Ĝ@�bN@���@��@���@�
=@�5?@���@��h@�V@�%@��@���@�Ĝ@��j@��@�9X@�b@�ƨ@�;d@���@�ȴ@�~�@�{@��@���@�x�@�&�@��9@�r�@�I�@��m@�l�@���@��+@�J@��h@�G�@�7L@�/@�&�@���@���@���@�(�@���@��F@�t�@�+@���@�M�@���@��7@�hs@�O�@�%@��j@�r�@�Z@���@�t�@�33@���@�V@�{@�J@�@���@��T@�@���@�&�@��j@��u@�Q�@��@���@�+@���@�M�@�-@���@�hs@�7L@�V@��9@�bN@�1'@�@�@~ff@}�@|�@|��@|�j@|��@|z�@{�m@{@y�#@y��@y�7@yhs@x�`@w�;@w��@w��@v��@vff@u��@uO�@t�@t�D@t�@sƨ@s�@sS�@r��@q�^@qG�@p��@p�@p  @oK�@n��@n�R@nV@nE�@m��@l�/@l�@lI�@l9X@l�@k�m@k�F@k��@k��@kdZ@j�@jM�@i�@i��@ihs@h��@hb@g�w@g+@f�y@fȴ@f��@fE�@e�T@e/@d�/@d��@dj@dI�@dI�@d(�@c�
@c�@co@a�^@a&�@`�`@`�9@`  @^��@]?}@\�j@\��@\(�@\1@[�F@[C�@[@Z�H@Z��@Z=q@ZJ@ZJ@Y��@Y�@Y�@Y�#@Y��@Y��@X�`@W\)@W�@W
=@V��@V��@VV@U�@U`B@T�@T�D@Tz�@T�@R��@Q��@Qhs@Q�@PQ�@P  @O�w@O�P@Ol�@O;d@N��@NV@N{@N{@N@M��@M��@M�@M`B@L�/@L��@K�F@KS�@K33@Ko@J��@I��@I7L@I&�@I%@H�`@H��@H�@Hr�@Hr�@Hr�@Hr�@HbN@GK�@F5?@E`B@D��@Dj@D�@Cƨ@C�@CdZ@B�H@Bn�@BJ@A�7@A&�@A%@@��@@��@@�9@@bN@@ �@?��@?�@?��@?l�@?K�@>�y@>��@>�+@>E�@=��@=�@<��@<j@;�
@;S�@:��@:��@9�@9X@8Ĝ@8Q�@8 �@8  @7�@7
=@6�R@65?@5?}@4�@4��@4��@4I�@41@3�@3S�@3o@2��@2^5@2�@1�^@0�9@01'@/�;@/\)@/
=@.V@-�@,�@,�D@,�@+t�@*�@*�!@*^5@)�@)X@)G�@(��@( �@'��@'�P@'l�@'l�@'�P@'|�@'|�@'|�@'\)@';d@';d@'+@'
=@'
=@&�R@&V@&5?@&$�@%�-@%V@$Z@$(�@$1@#�F@#��@#o@"�H@"�H@"�\@"M�@"=q@!�7@!7L@!%@ �`@ �9@ ��@ ��@ �9@ �9@ �9@ �9@ ��@ bN@  �@ b@�;@K�@�@��@�+@V@{@@�@p�@/@�@��@��@I�@9X@9X@9X@�@��@�F@��@�@dZ@S�@C�@@�\@M�@�@��@x�@X@X@G�@G�@7L@&�@�@�@�`@�9@A�@�P@+@
=@ȴ@��@��@V@�@��@�-@�h@�@`B@O�@O�@?}@�@�@z�@I�@(�@��@�
@��@C�@33@"�@�@��@��@�!@~�@M�@=q@�@�@�#@��@x�@hs@G�@�@��@�9@�9@��@Q�@ �@ �@b@  @�;@�@|�@K�@�@
=@��@��@�@�R@ff@5?@{@�@p�@��@��@�j@��@I�@9X@(�@�@1@ƨ@ƨ@ƨ@ƨ@�F@��@t�@t�@�@�@t�@S�@"�@
��@
��@
�\@
~�@
^5@
=q@
�@
J@
J@	��@	�#@	��@	�7@	hs@��@�`@Ĝ@�9@�u@A�@ �@b@  @  A�1'A�1'A�1'A�5?A�33A�1'A�1'A�/A�1'A�33A�33A�/A�/A�5?A�5?A�1'A�/A�/A�1'A�5?A�5?A�1'A�1'A�1'A�33A�5?A�7LA�33A�1'A�1'A�33A�7LA�5?A�5?A�1'A�1'A�5?A�7LA�5?A�1'A�1'A�5?A�7LA�7LA�5?A�1'A�1'A�5?A�7LA�7LA�7LA�33A�33A�5?A�9XA�9XA�7LA�33A�33A�7LA�9XA�9XA�9XA�5?A�5?A�5?A�9XA�9XA�7LA�5?A�7LA�9XA�5?A�33A�/A�/A�/A�-A��A��A�{A�1A���A��#A�r�A��yA�oA�ĜAԴ9AԬAԥ�Aԣ�Aԝ�Aԗ�Aԕ�Aԕ�Aԗ�Aԗ�Aԕ�Aԉ7A�|�A�l�A�^5A�S�A�O�A�C�A�;dA�-A�$�A�"�A� �A��A��A��A�oA�oA�{A�oA�bA�VA�
=A�1A�1A�1A�%A�A���A���A���A���A���A���A��A��A��yA��mA��yA��TA��#A���A�ƨAө�AӉ7A�jA�O�A�A�A�1'A�/A�1'A�1'A��A���A��mA��A���A���A���A���Aҥ�Aҙ�AҁA�v�A�dZA�dZA�^5A�\)A�XA�S�A�S�A�VA�XA�VA�Q�A�M�A�I�A�E�A�E�A�C�A�A�A�?}A�;dA�9XA�9XA�;dA�;dA�;dA�;dA�9XA�7LA�1'A�+A�(�A�$�A��A�VA���A���A���A���A���A���A���A���A��A��A��A��A��A��yA��mA��TA��#A��A��A���A���A�ȴA�AѼjAѸRAѰ!AѮAѝ�Aї�Aѕ�Aя\AэPAыDAхA�|�A�hsA�VA�K�A�G�A�G�A�E�A�E�A�E�A�A�A�=qA�9XA�1'A�33A�1'A�+A�&�A� �A� �A��A��A��A��A��A��A�{A��A�{A�oA�VA�bA�oA�bA�oA�A�  A���A��/A��TA���A���AЧ�AЍPA�z�AЇ+AЉ7A�jA�(�A�JA���A�A�S�A���AΗ�A�ZA��A�&�A�S�A�AˋDA�p�A�S�A�G�A�"�A���A�AʅA�&�A�p�A���A�I�A���Aǣ�AǗ�AǑhA�~�A�n�A�Q�A��A���A��;AƉ7A�1AŶFA�x�A�"�A���A��yA��!A���A���A�5?A�  A�ĜA��PA�ZA�C�A�&�A���A���A�bNA��A��A���A��A�p�A�Q�A�JA��A��A�t�A�{A��A���A��!A���A�t�A�K�A��A�O�A���A�oA���A��-A�hsA�S�A�M�A�/A��A�%A��A��`A���A��hA�ZA�(�A��A���A���A�M�A��`A���A�VA��A��A��uA�ffA�XA�Q�A�M�A�M�A�\)A�S�A�=qA��A��`A���A��wA���A��PA�ffA�$�A��#A���A���A��A���A�  A�
=A�bA�oA�A���A���A��A��jA�/A�A��yA��A�r�A��FA�z�A���A��A��9A��A�;dA��A��`A���A�Q�A�VA���A��/A���A�Q�A�5?A�JA���A��`A�ȴA���A�Q�A�/A��A�A��A��
A�A��A��\A�ffA�$�A��A��PA�Q�A� �A��hA�K�A���A��jA�M�A���A��A�t�A�A���A�ȴA��9A���A�|�A�&�A��TA�ƨA�Q�A��A��A��FA�v�A�(�A��A��A���A���A��A�K�A�/A�{A��mA��FA���A�x�A�ZA�=qA��A��A�5?A��9A�I�A�A�ƨA�dZA�  A��PA��TA�bNA��A���A���A�ZA��A���A�G�A��A��hA�A���A�Q�A�=qA���A�=qA�  A��HA���A�ffA�"�A��yA���A�K�A�/A�A��#A��wA��7A�l�A�XA�5?A�&�A�A�  A��`A��A���A�ĜA��jA��-A��A���A���A���A���A��7A�v�A�r�A�Q�A�K�A�(�A���A��mA���A��!A��A�r�A�VA�;dA�+A�&�A�VA�%A���A��HA��A��yA��TA���A��A���A��uA��7A��A�z�A�hsA�M�A�-A���A��FA�t�A�7LA�A��A�=qA��jA�?}A��`A��jA���A�r�A�
=A���A�ffA�?}A�-A�$�A�oA�1A�%A�A��A��`A���A��jA��-A��!A��A���A���A���A���A��hA��uA��uA���A��PA��DA��+A��+A��7A��+A��+A��7A��+A��A�x�A�v�A�v�A�n�A�p�A�p�A�p�A�jA�ffA�VA�Q�A�/A�bA��A��RA�ZA��A�z�A�JA��!A�&�A�dZA��AVA~��A~~�A~v�A~n�A~9XA}�-A}S�A}O�A}G�A}7LA}�A|��A|�A|��A|E�A|�A{��Az��Ay��Ax��AxZAv��Au��AuO�At��At�RAt�AtQ�At �As�Ar^5Ar5?Ar(�Ar�Ar1Aq�^ApffAo��Ao+An��An�DAn1'Am��AmO�Am"�Am
=Am%AmAmAmAl��Al��Al��Al�Al�Al�Al�`Al�HAl�Al��AlĜAl�RAl�9Al�Al�uAljAl^5AlQ�Al9XAlbAk�#Ak��Ak�7Ak�Akl�AkK�Ak+Aj�Aj��AjjAi��AidZAh�+Ag�wAg33AfbNAf=qAf$�AfbAf1AfAf  Ae�Ae�TAe�wAe�-Ae��Ae|�AeC�Ad�/AdffAc�AcO�AcC�Ac�Ab��Ab��Aa��AaG�A`�`A`��A`�+A`n�A`A_l�A_XA_+A^��A^�A^VA^1'A^�A^  A]��A]p�A\��A\$�A[��A[�-A[G�A[%AZ��AZ5?AY��AY|�AY\)AX��AW��AW7LAV��AV�AV{AU��AU�PAUK�AT�yAT�\AT1AS��AS�AR��AR=qAQ�TAQx�AQC�AP��AP�uAO�AN�/AN��AN~�ANn�ANZAN=qAN{AM�TAMƨAM��AMdZAMAL��AL^5AL�AK�;AK��AKhsAKC�AK�AKAJ�`AJ��AJ�AJ�DAJn�AJE�AJ  AI|�AHȴAG�AG�AF�HAF�RAF�uAF�AFv�AF^5AFA�AF$�AE�AE��AE`BAE/AD�`AD~�AC�ACl�AC;dAC�ACAB�AB��AB��ABn�AB=qAA��AA��AAp�AAhsAAhsAA`BAAS�AA?}AAoA@�DA@=qA@G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           A�1'A�1'A�1'A�1'A�33A�1'A�33A�33A�33A�33A�33A�5?A�5?A�5?A�7LA�7LA�5?A�"�A�K�Aԩ�Aԕ�A�ffA�+A��A�JA���A��A���A�XA�{A�ƨA�v�A�XA�O�A�?}A�;dA�/A�A���A��yA��
AѼjAѓuA�`BA�;dA��HA�A�A���A�|�A�ȴA��A��^A���A�  A�|�A��A�E�A���A��A�\)A���A�t�A�K�A�z�A���A��RA���A�{A��yA�A�A�`BA�ffA��RA��wA���A���A��A���A�=qA��A��PA��A��\A�XA���A|$�At5?AnȴAl�AlI�AiƨAe�Ac?}A_p�A\Q�AW��AT�AP-AM�;AK�^AJ=qAF��AEl�AC
=AA��A@E�A?S�A>9XA=�mA<��A:�DA:��A9�^A6=qA4ZA4�A41A1�
A1G�A0��A0VA0�A1p�A1��A1�;A1C�A0��A/�A.��A.$�A-��A,��A+�mA+�wA+��A+x�A+C�A+�A*ĜA*9XA)�hA'��A&z�A& �A& �A%p�A$  A#`BA#�A"^5A �+A��A��A  A5?A��AbAQ�Av�AbNA�wA%AA�A|�Ar�A�A%A~�AȴA�AĜA��AbAƨA�uA��A��AbAƨAXAȴA�!AVA$�A�A�TA�^AO�A�RAr�A=qA�A�;A�A/A��A�RAjA1'AA��AXAA��A�uA~�AbNA=qA1A�mAO�A�AA�RA�#Al�A/A
��A
�+A
9XA	ƨA	�PA��A�AE�A�AC�A��A�DAv�AVAE�A5?A(�A1A�A��A�HA�mA&�A�uAM�AA�A �A�A�7A ��@�^5@��h@�9X@��!@�M�@���@�r�@��y@��h@��@��@��@�v�@�-@���@�V@�Q�@�"�@�@�ȴ@�ff@�-@���@��@�@�|�@�V@�@�G�@�%@���@�bN@�1@�t�@�ff@�7@��/@�1@㝲@�;d@��H@�v�@��@�X@�r�@�1@߶F@ߍP@�;d@���@���@�^5@���@��@ۍP@�S�@�+@�
=@ڸR@�M�@�@ى7@�hs@�7L@ؓu@�  @�\)@��H@�E�@���@���@Դ9@ԛ�@ԛ�@� �@�@��y@Ұ!@�=q@љ�@��@мj@�r�@�9X@��;@ϝ�@�;d@�^5@́@���@�(�@˅@ʏ\@ɡ�@�X@���@�I�@Ǖ�@�ȴ@Ɨ�@�E�@�J@��@��#@Ų-@�/@�j@��;@�;d@��@���@�V@��@��@�bN@�b@���@�
=@���@���@�~�@�ff@�E�@��@��h@�O�@��@�j@�1'@��;@�t�@�
=@�ff@��@���@�G�@��@���@�z�@�I�@�9X@���@�;d@��R@�~�@�{@���@�O�@��@���@��@�r�@�Q�@� �@��
@��H@�5?@��@���@�p�@�hs@��@��j@��D@�I�@���@���@���@���@�@���@���@�x�@�`B@��@��j@�j@�9X@��w@�+@��@���@�5?@��#@��h@�G�@��9@�bN@�I�@� �@�1@���@��@�K�@�@��@�5?@��#@���@��7@�%@�j@��@���@�|�@�K�@�o@���@�~�@�V@�-@��@�{@���@��T@��h@��9@�Z@���@���@�|�@���@�n�@���@���@�`B@��9@�bN@��@��m@���@��P@�"�@���@�~�@�E�@�5?@��@�@��#@��h@�X@�/@�Ĝ@�bN@���@��@���@�
=@�5?@���@��h@�V@�%@��@���@�Ĝ@��j@��@�9X@�b@�ƨ@�;d@���@�ȴ@�~�@�{@��@���@�x�@�&�@��9@�r�@�I�@��m@�l�@���@��+@�J@��h@�G�@�7L@�/@�&�@���@���@���@�(�@���@��F@�t�@�+@���@�M�@���@��7@�hs@�O�@�%@��j@�r�@�Z@���@�t�@�33@���@�V@�{@�J@�@���@��T@�@���@�&�@��j@��u@�Q�@��@���@�+@���@�M�@�-@���@�hs@�7L@�V@��9@�bN@�1'@�@�@~ff@}�@|�@|��@|�j@|��@|z�@{�m@{@y�#@y��@y�7@yhs@x�`@w�;@w��@w��@v��@vff@u��@uO�@t�@t�D@t�@sƨ@s�@sS�@r��@q�^@qG�@p��@p�@p  @oK�@n��@n�R@nV@nE�@m��@l�/@l�@lI�@l9X@l�@k�m@k�F@k��@k��@kdZ@j�@jM�@i�@i��@ihs@h��@hb@g�w@g+@f�y@fȴ@f��@fE�@e�T@e/@d�/@d��@dj@dI�@dI�@d(�@c�
@c�@co@a�^@a&�@`�`@`�9@`  @^��@]?}@\�j@\��@\(�@\1@[�F@[C�@[@Z�H@Z��@Z=q@ZJ@ZJ@Y��@Y�@Y�@Y�#@Y��@Y��@X�`@W\)@W�@W
=@V��@V��@VV@U�@U`B@T�@T�D@Tz�@T�@R��@Q��@Qhs@Q�@PQ�@P  @O�w@O�P@Ol�@O;d@N��@NV@N{@N{@N@M��@M��@M�@M`B@L�/@L��@K�F@KS�@K33@Ko@J��@I��@I7L@I&�@I%@H�`@H��@H�@Hr�@Hr�@Hr�@Hr�@HbN@GK�@F5?@E`B@D��@Dj@D�@Cƨ@C�@CdZ@B�H@Bn�@BJ@A�7@A&�@A%@@��@@��@@�9@@bN@@ �@?��@?�@?��@?l�@?K�@>�y@>��@>�+@>E�@=��@=�@<��@<j@;�
@;S�@:��@:��@9�@9X@8Ĝ@8Q�@8 �@8  @7�@7
=@6�R@65?@5?}@4�@4��@4��@4I�@41@3�@3S�@3o@2��@2^5@2�@1�^@0�9@01'@/�;@/\)@/
=@.V@-�@,�@,�D@,�@+t�@*�@*�!@*^5@)�@)X@)G�@(��@( �@'��@'�P@'l�@'l�@'�P@'|�@'|�@'|�@'\)@';d@';d@'+@'
=@'
=@&�R@&V@&5?@&$�@%�-@%V@$Z@$(�@$1@#�F@#��@#o@"�H@"�H@"�\@"M�@"=q@!�7@!7L@!%@ �`@ �9@ ��@ ��@ �9@ �9@ �9@ �9@ ��@ bN@  �@ b@�;@K�@�@��@�+@V@{@@�@p�@/@�@��@��@I�@9X@9X@9X@�@��@�F@��@�@dZ@S�@C�@@�\@M�@�@��@x�@X@X@G�@G�@7L@&�@�@�@�`@�9@A�@�P@+@
=@ȴ@��@��@V@�@��@�-@�h@�@`B@O�@O�@?}@�@�@z�@I�@(�@��@�
@��@C�@33@"�@�@��@��@�!@~�@M�@=q@�@�@�#@��@x�@hs@G�@�@��@�9@�9@��@Q�@ �@ �@b@  @�;@�@|�@K�@�@
=@��@��@�@�R@ff@5?@{@�@p�@��@��@�j@��@I�@9X@(�@�@1@ƨ@ƨ@ƨ@ƨ@�F@��@t�@t�@�@�@t�@S�@"�@
��@
��@
�\@
~�@
^5@
=q@
�@
J@
J@	��@	�#@	��@	�7@	hs@��@�`@Ĝ@�9@�u@A�@ �@b@  G�O�A�1'A�1'A�1'A�5?A�33A�1'A�1'A�/A�1'A�33A�33A�/A�/A�5?A�5?A�1'A�/A�/A�1'A�5?A�5?A�1'A�1'A�1'A�33A�5?A�7LA�33A�1'A�1'A�33A�7LA�5?A�5?A�1'A�1'A�5?A�7LA�5?A�1'A�1'A�5?A�7LA�7LA�5?A�1'A�1'A�5?A�7LA�7LA�7LA�33A�33A�5?A�9XA�9XA�7LA�33A�33A�7LA�9XA�9XA�9XA�5?A�5?A�5?A�9XA�9XA�7LA�5?A�7LA�9XA�5?A�33A�/A�/A�/A�-A��A��A�{A�1A���A��#A�r�A��yA�oA�ĜAԴ9AԬAԥ�Aԣ�Aԝ�Aԗ�Aԕ�Aԕ�Aԗ�Aԗ�Aԕ�Aԉ7A�|�A�l�A�^5A�S�A�O�A�C�A�;dA�-A�$�A�"�A� �A��A��A��A�oA�oA�{A�oA�bA�VA�
=A�1A�1A�1A�%A�A���A���A���A���A���A���A��A��A��yA��mA��yA��TA��#A���A�ƨAө�AӉ7A�jA�O�A�A�A�1'A�/A�1'A�1'A��A���A��mA��A���A���A���A���Aҥ�Aҙ�AҁA�v�A�dZA�dZA�^5A�\)A�XA�S�A�S�A�VA�XA�VA�Q�A�M�A�I�A�E�A�E�A�C�A�A�A�?}A�;dA�9XA�9XA�;dA�;dA�;dA�;dA�9XA�7LA�1'A�+A�(�A�$�A��A�VA���A���A���A���A���A���A���A���A��A��A��A��A��A��yA��mA��TA��#A��A��A���A���A�ȴA�AѼjAѸRAѰ!AѮAѝ�Aї�Aѕ�Aя\AэPAыDAхA�|�A�hsA�VA�K�A�G�A�G�A�E�A�E�A�E�A�A�A�=qA�9XA�1'A�33A�1'A�+A�&�A� �A� �A��A��A��A��A��A��A�{A��A�{A�oA�VA�bA�oA�bA�oA�A�  A���A��/A��TA���A���AЧ�AЍPA�z�AЇ+AЉ7A�jA�(�A�JA���A�A�S�A���AΗ�A�ZA��A�&�A�S�A�AˋDA�p�A�S�A�G�A�"�A���A�AʅA�&�A�p�A���A�I�A���Aǣ�AǗ�AǑhA�~�A�n�A�Q�A��A���A��;AƉ7A�1AŶFA�x�A�"�A���A��yA��!A���A���A�5?A�  A�ĜA��PA�ZA�C�A�&�A���A���A�bNA��A��A���A��A�p�A�Q�A�JA��A��A�t�A�{A��A���A��!A���A�t�A�K�A��A�O�A���A�oA���A��-A�hsA�S�A�M�A�/A��A�%A��A��`A���A��hA�ZA�(�A��A���A���A�M�A��`A���A�VA��A��A��uA�ffA�XA�Q�A�M�A�M�A�\)A�S�A�=qA��A��`A���A��wA���A��PA�ffA�$�A��#A���A���A��A���A�  A�
=A�bA�oA�A���A���A��A��jA�/A�A��yA��A�r�A��FA�z�A���A��A��9A��A�;dA��A��`A���A�Q�A�VA���A��/A���A�Q�A�5?A�JA���A��`A�ȴA���A�Q�A�/A��A�A��A��
A�A��A��\A�ffA�$�A��A��PA�Q�A� �A��hA�K�A���A��jA�M�A���A��A�t�A�A���A�ȴA��9A���A�|�A�&�A��TA�ƨA�Q�A��A��A��FA�v�A�(�A��A��A���A���A��A�K�A�/A�{A��mA��FA���A�x�A�ZA�=qA��A��A�5?A��9A�I�A�A�ƨA�dZA�  A��PA��TA�bNA��A���A���A�ZA��A���A�G�A��A��hA�A���A�Q�A�=qA���A�=qA�  A��HA���A�ffA�"�A��yA���A�K�A�/A�A��#A��wA��7A�l�A�XA�5?A�&�A�A�  A��`A��A���A�ĜA��jA��-A��A���A���A���A���A��7A�v�A�r�A�Q�A�K�A�(�A���A��mA���A��!A��A�r�A�VA�;dA�+A�&�A�VA�%A���A��HA��A��yA��TA���A��A���A��uA��7A��A�z�A�hsA�M�A�-A���A��FA�t�A�7LA�A��A�=qA��jA�?}A��`A��jA���A�r�A�
=A���A�ffA�?}A�-A�$�A�oA�1A�%A�A��A��`A���A��jA��-A��!A��A���A���A���A���A��hA��uA��uA���A��PA��DA��+A��+A��7A��+A��+A��7A��+A��A�x�A�v�A�v�A�n�A�p�A�p�A�p�A�jA�ffA�VA�Q�A�/A�bA��A��RA�ZA��A�z�A�JA��!A�&�A�dZA��AVA~��A~~�A~v�A~n�A~9XA}�-A}S�A}O�A}G�A}7LA}�A|��A|�A|��A|E�A|�A{��Az��Ay��Ax��AxZAv��Au��AuO�At��At�RAt�AtQ�At �As�Ar^5Ar5?Ar(�Ar�Ar1Aq�^ApffAo��Ao+An��An�DAn1'Am��AmO�Am"�Am
=Am%AmAmAmAl��Al��Al��Al�Al�Al�Al�`Al�HAl�Al��AlĜAl�RAl�9Al�Al�uAljAl^5AlQ�Al9XAlbAk�#Ak��Ak�7Ak�Akl�AkK�Ak+Aj�Aj��AjjAi��AidZAh�+Ag�wAg33AfbNAf=qAf$�AfbAf1AfAf  Ae�Ae�TAe�wAe�-Ae��Ae|�AeC�Ad�/AdffAc�AcO�AcC�Ac�Ab��Ab��Aa��AaG�A`�`A`��A`�+A`n�A`A_l�A_XA_+A^��A^�A^VA^1'A^�A^  A]��A]p�A\��A\$�A[��A[�-A[G�A[%AZ��AZ5?AY��AY|�AY\)AX��AW��AW7LAV��AV�AV{AU��AU�PAUK�AT�yAT�\AT1AS��AS�AR��AR=qAQ�TAQx�AQC�AP��AP�uAO�AN�/AN��AN~�ANn�ANZAN=qAN{AM�TAMƨAM��AMdZAMAL��AL^5AL�AK�;AK��AKhsAKC�AK�AKAJ�`AJ��AJ�AJ�DAJn�AJE�AJ  AI|�AHȴAG�AG�AF�HAF�RAF�uAF�AFv�AF^5AFA�AF$�AE�AE��AE`BAE/AD�`AD~�AC�ACl�AC;dAC�ACAB�AB��AB��ABn�AB=qAA��AA��AAp�AAhsAAhsAA`BAAS�AA?}AAoA@�DA@=qA@G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B4B4nB49B49B4B4nB4B4B49B49B49B4B3�B3�B3�B3hB3�B:*BjB��B�1B�kB��B��B�$B��B�{B��B��B��B��B��B��B�YB�B�B�B��B�B��B�AB�_B�.B�_B��B�6B	��B
\)B
��B
��B
�'B
�9B
�B
ȀB
��B
�KB�Br�B�B�*B��B�B(B
rB�B��B��BɆB�XB��B�iBp;BNB+B
�WB
��B
xlB
[#B
I�B
+�B
SB	�>B	�B	�BB	�aB	�eB	qB	V�B	EmB	@OB	8�B	#nB	qB	
	B��B��B�B��B	 iB��B�2B	YB	�B	�B	7�B	@OB	AUB	B�B	@�B	;�B	<6B	V�B	e`B	H�B	@�B	d�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�VB
B
�B
DB
�B
�B
VB
OB
!�B
+kB
7�B
>BB
B'B
D�B
G�B
J�B
D�B
9�B
6FB
6B
6B
2�B
/�B
,qB
*�B
!-B
�B
�B
�B
)*B
0!B
5tB
>B
D�B
GEB
F�B
B'B
?HB
;dB
QB
U�B
OvB
RTB
C�B
P�B
T�B
VmB
T�B
P�B
LdB
H�B
OvB
S�B
VB
W?B
WsB
W?B
V�B
W?B
WsB
W�B
W�B
YB
V�B
VB
VB
V�B
U�B
V�B
T�B
T�B
U�B
S�B
S&B
S�B
T�B
S�B
S�B
R�B
Q�B
R B
S�B
S�B
RTB
RTB
R B
P�B
P�B
R�B
Q�B
P�B
PHB
OB
M�B
MB
J�B
I�B
IB
F�B
FtB
E�B
F�B
C�B
CaB
B�B
B�B
B'B
A�B
A�B
A B
?�B
?HB
>�B
=B
;�B
8�B
7B
6FB
5�B
4�B
49B
4�B
0�B
.IB
.}B
+�B
*0B
)�B
(�B
'�B
&LB
#�B
#nB
#:B
 �B
 �B
�B
VB
�B
�B
CB
CB
�B
	B
qB
7B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
FB
�B
�B
uB
@B
�B
@B
B
B
�B
�B
{B
{B
B
B
�B
B
uB
�B
�B
�B
B
�B
�B
B
@B
@B
�B
oB
:B
�B
�B
B
�B
B
�B
�B
�B
�B
B
�B
MB
�B
B
MB
�B
�B
�B
MB
B
�B
{B
{B
MB
�B
�B
�B
B
oB
 B
.B
�B
�B
�B
�B
\B
�B
�B
(B
�B
�B
�B
�B
�B
\B
VB
"B
�B
(B
�B
\B
�B
\B
\B
�B
�B
"B
"B
�B
�B
�B
�B
�B
\B
VB
�B
�B
\B
�B
\B
�B
�B
(B
�B
.B
\B
\B
�B
�B
�B
�B
4B
�B
�B
B
:B
:B
:B
B
:B
oB
FB
oB
oB
�B
�B
MB
�B
$B
�B
�B
_B
�B
�B
eB
eB
eB
eB
eB
eB
�B
�B
kB
B
	B
	B
	B
�B
B
qB
�B
�B
xB
CB
CB
CB
B
�B
�B
�B
xB
xB
�B
~B
B
�B
~B
~B
~B
~B
IB
IB
~B
�B
IB
IB
B
�B
B
B
B
B
B
~B
~B
B
�B
OB
OB
OB
~B
�B
�B
OB
�B
OB
�B
�B
 �B
!-B
"hB
"�B
"�B
"�B
#B
#nB
$B
#�B
$tB
$tB
#�B
#�B
"�B
#�B
#:B
#nB
#B
$@B
#B
#:B
#�B
#�B
#�B
#�B
$@B
#�B
$tB
$�B
$�B
$tB
$�B
%FB
%�B
%�B
&LB
%�B
&�B
&�B
&�B
&�B
'�B
&�B
'�B
(XB
(�B
)�B
)_B
)_B
)*B
(�B
)*B
(�B
(�B
*�B
+B
*�B
+6B
+6B
,=B
-B
-CB
-�B
-�B
-�B
-wB
.IB
-�B
-�B
/�B
.�B
.�B
/B
/�B
.�B
/B
.�B
.�B
.�B
.�B
/B
/�B
/�B
/OB
0!B
0UB
0UB
1'B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
3�B
3�B
3�B
5B
5B
5�B
6�B
6�B
6zB
6zB
6zB
6B
6�B
7LB
7�B
7�B
7B
7LB
7�B
8�B
8B
8B
9�B
8�B
9XB
9$B
9XB
9�B
:*B
:^B
:*B
:*B
;dB
<jB
<6B
<jB
=<B
>B
=�B
>B
>wB
>BB
>BB
>B
>B
>B
>BB
>�B
?�B
@B
@OB
@B
@B
@B
@�B
A�B
B[B
B'B
B[B
B�B
C-B
CaB
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
FB
F?B
F?B
E�B
E�B
FB
FtB
FtB
HKB
HB
G�B
G�B
H�B
I�B
K)B
J�B
J�B
J�B
J�B
K)B
K�B
K�B
K^B
K�B
L0B
LdB
K�B
K�B
L0B
K�B
K�B
K�B
K^B
L�B
N<B
M6B
M�B
MjB
M�B
M�B
N<B
NpB
N�B
N�B
NB
N<B
PB
PHB
PHB
P�B
Q�B
P�B
P�B
P}B
P�B
P�B
QNB
QNB
Q�B
Q�B
RTB
S&B
R�B
R�B
S[B
S�B
S�B
T,B
S�B
TaB
T�B
UgB
V�B
VB
UgB
VB
U�B
V9B
V�B
V�B
VmB
VmB
U�B
U�B
W�B
XEB
W�B
XyB
X�B
XB
X�B
YKB
X�B
YB
YB
ZB
Z�B
Z�B
Z�B
ZB
ZB
Z�B
[#B
[�B
[WB
[�B
[�B
[�B
[WB
\)B
\]B
[�B
[�B
\�B
\�B
]/B
^B
]dB
^jB
^jB
^jB
^�B
_pB
`vB
`vB
`vB
`B
`BB
`�B
`�B
aHB
bB
a�B
a�B
b�B
b�B
c B
c�B
c B
b�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
e�B
e,B
f2B
f�B
gB
f�B
f�B
gmB
h
B
gmB
h>B
h�B
iDB
h�B
i�B
jKB
kQB
j�B
kQB
k�B
k�B
lWB
l"B
l�B
lWB
l�B
m�B
m�B
m�B
m)B
l�B
l�B
m)B
m)B
m�B
n�B
n�B
o5B
o�B
poB
poB
pB
o5B
o5B
o�B
poB
p;B
qB
q�B
r|B
q�B
sB
s�B
s�B
s�B
s�B
tB
s�B
s�B
t�B
u%B
t�B
t�B
u�B
v�B
v�B
v�B
wfB
v�B
wfB
xlB
w�B
w�B
x8B
xlB
xlB
y>B
y	B
y	B
yrB
y�B
y�B
zB
zDB
zB
zxB
{B
z�B
z�B
{�B
{�B
|B
|B
|PB
}"B
|�B
|�B
|�B
|�B
|�B
}"B
|�B
|�B
}"B
}�B
~�B
~�B
~(B
~�B
.B
~�B
~�B
.B
.B
cB
�4B
�4B
�iB
� B
cB
� B
�iB
�iB
��B
��B
�oB
�oB
�;B
�oB
��B
�;B
�B
��B
�B
�oB
�oB
��B
��B
�;B
�oB
�;B
�oB
�AB
�uB
��B
��B
��B
��B
�GB
��B
�B
�GB
�GB
��B
��B
�uB
��B
�B
��B
�MB
��B
��B
��B
��B
��B
�B
�B
��B
�B
��B
�B
�SB
�SB
��B
�B
�%B
��B
�%B
�%B
�%B
�%B
��B
��B
��B
�YB
�YB
��B
��B
��B
��B
��B
��B
��B
�_B
�_B
��B
�_B
��B
�_B
��B
�+B
�+B
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
��B
��B
��B
��B
�1B5B4�B49B2�B3�B49B5B4�B4B3hB3hB4�B5B2�B33B49B5B5?B4nB3hB2�B4nB4�B5B4nB33B33B49B5tB4�B4B33B33B4B5?B5B3�B3hB4B5B4�B49B33B2�B4B4�B4�B3�B2�B2�B2�B4nB4�B49B2�B2aB33B5B4nB3�B33B2�B2�B4B49B49B2�B2�B33B3�B3hB2aB3�B4�B6B8RB8RB7�B<jB=qB=<B@OBC-BI�Bh>B��B��B�xB�CB��B�7B�1B��B�7B�kB��B�_B��B��B�B�kB�B�B�_B�1B��B�kB�	B�1B�_B�$B��B��B��B�1B��B��B�SB��B��B��B�+B��B��B��B��B�SB��B�B�B�B��B�B�B�{B�@B�B��B�B��B��B�B��B��B�B��B��B��B��B��B�FB�uB�hB�.B�VB�PB�JB�"B�SB��B��B��B�fB��B��B�%B��B�fB��B�YB�SB��B�%B�YB�+B��B��B�SB��B��B�B��B��B�%B��B�SB�B�MB��B��B�YB��B�MB�GB�B�B�uB�B�oB��B� B�4B��B�oB�B�oB�B�B�iB�B�AB��B�AB��B��B��B��B��B��B�fB�JB�B��B�VB�(B��B� B�4B��B�B��B�B��B��B�CB�xB�B�B�B�VB��B�bB��B��B�-B�4B�B�B�tB��B�hB�-B��B�bB��B��B�\B��B��B�-B��B�'B�!B��B��B�nB�IB��B�OB�!B�B��B��B��B��B��B�B�HB��B��B	{B	1B	6FB	B�B	^�B	��B	��B	�B	�OB	ÖB	�B	��B	��B	уB	��B	�vB	�oB
DB
,=B
<B
OBB
PB
P}B
Q�B
XyB
YKB
\�B
d�B
\�B
Y�B
`BB
sMB
m�B
v+B
�MB
��B
��B
��B
B
��B
�B
�B
�hB
��B
��B
�kB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�nB
��B
�'B
��B
�_B
�'B
��B
��B
�CB
�~B
�xB
��B
�6B
�#B
�UB
�*B
�B
ΥB
��B
�-B
�?B
�3B
�9B
ŢB
ÖB
ǮB
��B
�9B
�gB
�0B
�,B
�3B
ɆB
��B
�<B
��B
�KB
�-B
�B
��B
�B
�FB
��B
�B
ĜB
ʌB
͟B
�ZB
�KB
�sB
�TB
��BB1B�BE�BJ#BI�B[�B{�B�;B�B��B�'B��B�-B��B�aB��B�B�eB�1B{B�(B�B��B�
B�B��BAB��B��B�B�B4BfBMB�BeBuBPB"BDB	�B�B�B�B�B1B+BSB�B�BAB�BoB{B  B �B�B�TB�B��B�B�ZB�WB�B�]BܒB̘B��B�wB�wB�B�B�B�hB�?B��B�*B�B��B��B��B�B��B��B��B�GB�Bz�BzB{�By�BsBrGBl�Bj�BiDBg8B�iBiDB;�B8B-B(�B�BB�B
�PB
��B
�rB
�rB
�B
�BB
�B
ҽB
�B
��B
��B
�nB
��B
��B
�XB
�OB
��B
��B
�(B
��B
��B
��B
�uB
��B
z�B
zB
rGB
r|B
v�B
m]B
iyB
k�B
f�B
l"B
c B
d&B
`BB
\�B
[�B
[�B
[#B
Y�B
ZB
Z�B
Y�B
Y�B
Z�B
^jB
U�B
ZQB
RTB
V�B
[#B
Z�B
XB
XB
b�B
XB
R�B
Q�B
J�B
E�B
FtB
DgB
>�B
D3B
8�B
9XB
3hB
7B
7B
49B
2-B
-B
-CB
'�B
*�B
)*B
,qB
+6B
*�B
%FB
'B
�B
�B
B
�B
{B

�B
B
B	��B
_B	�PB	��B	�B	�iB	�)B	��B	�DB	�B	�B	��B	�DB	�B	�B	�fB	��B	�`B	�`B	�2B	�`B	�B	�yB	�,B	� B	�TB	�B	�B	�ZB	�&B	�B	�NB	�B	�B	�NB	�TB	�TB	� B	�B	�B	��B	�B	ߤB	ݘB	�jB	�B	�dB	�B	�B	уB	�)B	�KB	�#B	�B	ҽB	�vB	�NB	҉B	��B	��B	�CB	��B	��B	�hB	��B	��B	��B	��B	��B	�	B	��B	��B	�$B	�B	��B	� B	��B	��B	��B	��B	��B	��B	|PB	s�B	o B	j�B	oiB	h
B	lWB	y>B	h
B	`�B	]dB	[�B	YB	a|B	uZB	e`B	YB	WsB	P�B	TaB	S�B	N�B	K)B	IB	GEB	GB	F?B	EmB	E�B	EB	E�B	FB	E�B	E9B	D3B	CaB	C�B	C�B	CaB	CaB	A�B	A�B	A�B	AUB	?}B	?B	?HB	?}B	>wB	>wB	:�B	8�B	;0B	:�B	7�B	8RB	7�B	8RB	9�B	33B	?}B	4B	8B	.�B	&�B	&�B	&�B	%FB	"hB	"hB	#B	"�B	&�B	 'B	!�B	VB	!�B	"�B	�B	#�B	�B	1B	B	MB	SB	"hB	SB	:B	�B	�B		lB	B	VB	%B		B	YB	�B	B	�B�]B	B	 4B	MB	GB	uB��B�rB�	B�rB�B�.B�B�B�;B��B�rB��B�B�B�B��B�B�B�fB�B��B�|B��B�B�GB��B��B��B�PB��B	
�B	 �B��B�VB�]B��B	 4B	oB��B	 4B��B��B	B	�B�.B��B�xB��B�>B��B��B��B�B��B�B�B�B�B�fB�%B	YB	B	AB	B	�B	�B	MB	SB	+B	�B	�B	
�B	�B	\B	\B	�B	�B	!bB	�B	�B	�B	�B	!B	 'B	%B	%�B	.IB	6zB	8�B	<6B	8�B	8B	:*B	:^B	:�B	<�B	I�B	B'B	@G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           B4B4nB49B4B4B4TB4B4B49B49B4B4B3�B3�B3�B3�B4�B?cBr�B�#B�QB��B�KB�EB��B�SB�MB��B�hB��B�\B�rB��B��B�9B�mB��B��B�UB�;B��B�KB�hB�B��B�	B	��B
�'B
�VB
��B
��B
̘B
ԕB
ևB
��B
�5B�Bs�B��B�B�<B~BB�BBDB��BѝBðB� B��B��B`�B�B
�"B
��B
��B
c�B
RTB
AB
�B	�B	�8B	�FB	��B	�B	��B	]B	HfB	IB	ESB	,�B	'�B	�B	JB	AB�B	�B	�B	 iB	�B	�B	�B	$�B	<6B	C�B	EB	C�B	E9B	B�B	;�B	[�B	p!B	OBB	A�B	f�B	�@B	��B	�B	�B	�$B	��B	��B	�!B	�B	��B
�B
�B
B
�B
!�B
"B
B
"hB
+�B
8�B
>�B
C{B
F�B
JXB
Q B
H�B
;dB
6�B
8�B
:�B
4�B
0�B
/iB
0�B
$ZB
B
eB
eB
)�B
/�B
4�B
=�B
E9B
I�B
I�B
D�B
A�B
8�B
R�B
X�B
Q�B
W�B
DMB
Q4B
UMB
X_B
VB
UB
NpB
H�B
OBB
T�B
W�B
YB
W�B
XyB
W�B
W�B
W�B
X_B
YeB
[	B
W�B
V�B
W$B
V�B
V�B
X�B
U�B
U�B
V�B
T�B
S�B
T�B
U�B
U2B
T{B
S&B
R:B
R�B
TB
TFB
R�B
TFB
R�B
QhB
Q�B
U�B
S�B
Q�B
Q�B
PHB
N�B
N�B
K�B
LJB
J=B
G�B
G�B
HB
H�B
D�B
C�B
C{B
B�B
B[B
A�B
BB
A�B
A B
B'B
BuB
?�B
=�B
9�B
7LB
6�B
6�B
6`B
7�B
9�B
2�B
0�B
1'B
,�B
+�B
,=B
+�B
)�B
(
B
%B
%zB
$&B
!�B
!bB
!bB
 �B
 �B
5B
�B
�B
)B
B
xB
#B
CB
�B
�B
�B
sB
mB
SB
mB
�B
�B
�B
�B
2B
FB
�B
�B
B
B
,B
gB
�B
B
�B
�B
�B
@B
B
9B
2B
�B
@B
[B
�B
@B
�B
,B
�B
&B
�B
uB
�B
B
B
�B
B
+B

B
B
�B
B
�B
�B
gB
B
�B
�B
�B
9B
�B
�B
gB
MB
B
�B
�B
2B
�B
�B
B
�B
 B
B
�B
B
.B
�B
.B
�B
\B
\B
�B
 B
�B
�B
�B
�B
vB
NB
B
HB
�B
\B
}B
�B
B
�B
VB
pB
pB
vB
<B
B
.B
�B
B
�B
�B
�B
�B
�B
HB
�B
�B
bB
�B
�B
.B
�B
�B
NB
�B
 B
oB
 B
�B
�B
oB
�B
oB
�B
FB
�B
B
B
B
�B
�B
�B
�B
�B
_B
B
7B
�B
�B
�B
�B
�B
�B
�B
�B
kB
�B
�B
B
�B
qB
�B
�B
B
CB
�B
B
xB
�B
xB
�B
/B
/B
dB
�B
�B
OB
�B
dB
�B
�B
B
B
B
�B
�B
5B
B
�B
�B
/B
�B
IB
IB
�B
�B
�B
5B
�B
�B
pB
�B
!B
�B
B
�B
OB
�B
OB
�B
;B
 \B
!bB
!�B
"�B
"�B
"�B
#B
#TB
#�B
$tB
$B
%FB
%FB
$�B
$ZB
"�B
$�B
$�B
#�B
#�B
%,B
# B
#nB
$B
#�B
#�B
$@B
$�B
$B
%B
%�B
%`B
$�B
%FB
&B
&2B
&LB
&�B
&�B
'�B
'8B
'B
'�B
(sB
'�B
(XB
)DB
)�B
*B
)�B
)yB
)DB
)DB
)yB
)yB
)�B
+6B
+6B
+QB
+�B
+�B
-CB
-�B
.B
./B
-�B
.cB
.B
.�B
./B
.�B
0UB
/iB
/5B
0;B
0oB
/ B
/5B
/ B
/B
/ B
/ B
0B
0UB
0;B
/�B
0�B
1B
1AB
2-B
2�B
2GB
2�B
3�B
33B
3B
3MB
49B
49B
4�B
5�B
5�B
6�B
7fB
6�B
6�B
6�B
6�B
6�B
7�B
8RB
8B
7�B
7LB
7�B
8�B
8�B
88B
8�B
:DB
9�B
9�B
9�B
9�B
:^B
:xB
:�B
:^B
:�B
<jB
<�B
<�B
<�B
=�B
>�B
>(B
>]B
>�B
>]B
>�B
>�B
>BB
>wB
>]B
>�B
?�B
@OB
@iB
@B
@OB
@�B
AUB
B[B
B�B
B�B
CB
C�B
C�B
C�B
DMB
DB
D3B
D3B
EB
ESB
F%B
F%B
F?B
FYB
F?B
FB
F%B
FYB
F�B
G�B
H�B
HfB
HB
HfB
I�B
K)B
K�B
J�B
J�B
K)B
KDB
K�B
LB
K�B
K�B
LdB
LdB
LdB
LB
LB
L0B
LB
K�B
L0B
LJB
N"B
NpB
MPB
M�B
M�B
N"B
N<B
N�B
O(B
N�B
N�B
N�B
O�B
QB
P}B
P�B
Q�B
RB
Q B
QB
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S[B
R�B
R�B
S�B
TFB
TaB
T�B
T,B
T�B
T�B
V�B
W?B
VB
U�B
VB
VB
VSB
V�B
V�B
VmB
VmB
VB
V�B
X�B
YB
XEB
X�B
X�B
X_B
Y1B
YB
YeB
YB
YB
Z�B
[WB
Z�B
[	B
Z7B
ZkB
[=B
[qB
\B
[qB
[�B
[�B
[�B
[�B
\xB
\xB
\B
\]B
]B
]IB
]�B
^�B
]�B
^�B
^�B
_!B
_pB
`B
`�B
`�B
`�B
`vB
`�B
a-B
abB
b4B
bhB
bB
a�B
b�B
c:B
c�B
c�B
cnB
c:B
c�B
c�B
c�B
d�B
ezB
eB
ezB
fB
e�B
gB
g8B
gmB
gRB
g8B
g�B
hXB
g�B
h�B
i_B
i_B
iDB
jB
j�B
kkB
kB
kQB
k�B
lB
lWB
l"B
l�B
lqB
l�B
m�B
m�B
m�B
mwB
m]B
l�B
mCB
m�B
n�B
oiB
o B
oiB
o�B
p�B
p�B
p;B
oOB
o�B
p!B
p�B
p�B
q[B
q�B
r�B
rB
s3B
s�B
s�B
s�B
s�B
tB
tB
t9B
t�B
u?B
t�B
uZB
v`B
v�B
wB
w2B
w�B
wLB
w�B
x�B
w�B
xB
xRB
x�B
x�B
yXB
y	B
y	B
y�B
y�B
y�B
z*B
z^B
z*B
z�B
{0B
z�B
{0B
|B
|PB
|jB
|PB
|jB
}"B
|�B
|�B
|�B
|�B
|�B
}"B
|�B
|�B
}�B
~BB
.B
~�B
~wB
~�B
HB
B
.B
HB
HB
}B
�OB
�OB
��B
� B
}B
�OB
��B
��B
��B
�B
��B
��B
�oB
��B
��B
�UB
�;B
�B
�B
��B
��B
�B
��B
�oB
��B
�UB
��B
�[B
��B
��B
��B
��B
��B
�GB
��B
�aB
�{B
�GB
��B
��B
��B
��B
�GB
��B
��B
��B
��B
��B
��B
��B
�gB
�MB
��B
�MB
�gB
��B
��B
�mB
��B
�mB
�?B
�B
�?B
�?B
�YB
�%B
��B
��B
�B
�tB
��B
��B
��B
��B
�B
�%B
��B
��B
��B
�zB
�B
�zB
��B
�zB
�B
�+B
�EB
��B
��B
�zB
��B
�1B
��B
��B
�KB
�1B
��B
��B
�B
��B
��G�O�B5B4�B49B2�B3�B49B5B4�B4B3hB3hB4�B5B2�B33B49B5B5?B4nB3hB2�B4nB4�B5B4nB33B33B49B5tB4�B4B33B33B4B5?B5B3�B3hB4B5B4�B49B33B2�B4B4�B4�B3�B2�B2�B2�B4nB4�B49B2�B2aB33B5B4nB3�B33B2�B2�B4B49B49B2�B2�B33B3�B3hB2aB3�B4�B6B8RB8RB7�B<jB=qB=<B@OBC-BI�Bh>B��B��B�xB�CB��B�7B�1B��B�7B�kB��B�_B��B��B�B�kB�B�B�_B�1B��B�kB�	B�1B�_B�$B��B��B��B�1B��B��B�SB��B��B��B�+B��B��B��B��B�SB��B�B�B�B��B�B�B�{B�@B�B��B�B��B��B�B��B��B�B��B��B��B��B��B�FB�uB�hB�.B�VB�PB�JB�"B�SB��B��B��B�fB��B��B�%B��B�fB��B�YB�SB��B�%B�YB�+B��B��B�SB��B��B�B��B��B�%B��B�SB�B�MB��B��B�YB��B�MB�GB�B�B�uB�B�oB��B� B�4B��B�oB�B�oB�B�B�iB�B�AB��B�AB��B��B��B��B��B��B�fB�JB�B��B�VB�(B��B� B�4B��B�B��B�B��B��B�CB�xB�B�B�B�VB��B�bB��B��B�-B�4B�B�B�tB��B�hB�-B��B�bB��B��B�\B��B��B�-B��B�'B�!B��B��B�nB�IB��B�OB�!B�B��B��B��B��B��B�B�HB��B��B	{B	1B	6FB	B�B	^�B	��B	��B	�B	�OB	ÖB	�B	��B	��B	уB	��B	�vB	�oB
DB
,=B
<B
OBB
PB
P}B
Q�B
XyB
YKB
\�B
d�B
\�B
Y�B
`BB
sMB
m�B
v+B
�MB
��B
��B
��B
B
��B
�B
�B
�hB
��B
��B
�kB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
�nB
��B
�'B
��B
�_B
�'B
��B
��B
�CB
�~B
�xB
��B
�6B
�#B
�UB
�*B
�B
ΥB
��B
�-B
�?B
�3B
�9B
ŢB
ÖB
ǮB
��B
�9B
�gB
�0B
�,B
�3B
ɆB
��B
�<B
��B
�KB
�-B
�B
��B
�B
�FB
��B
�B
ĜB
ʌB
͟B
�ZB
�KB
�sB
�TB
��BB1B�BE�BJ#BI�B[�B{�B�;B�B��B�'B��B�-B��B�aB��B�B�eB�1B{B�(B�B��B�
B�B��BAB��B��B�B�B4BfBMB�BeBuBPB"BDB	�B�B�B�B�B1B+BSB�B�BAB�BoB{B  B �B�B�TB�B��B�B�ZB�WB�B�]BܒB̘B��B�wB�wB�B�B�B�hB�?B��B�*B�B��B��B��B�B��B��B��B�GB�Bz�BzB{�By�BsBrGBl�Bj�BiDBg8B�iBiDB;�B8B-B(�B�BB�B
�PB
��B
�rB
�rB
�B
�BB
�B
ҽB
�B
��B
��B
�nB
��B
��B
�XB
�OB
��B
��B
�(B
��B
��B
��B
�uB
��B
z�B
zB
rGB
r|B
v�B
m]B
iyB
k�B
f�B
l"B
c B
d&B
`BB
\�B
[�B
[�B
[#B
Y�B
ZB
Z�B
Y�B
Y�B
Z�B
^jB
U�B
ZQB
RTB
V�B
[#B
Z�B
XB
XB
b�B
XB
R�B
Q�B
J�B
E�B
FtB
DgB
>�B
D3B
8�B
9XB
3hB
7B
7B
49B
2-B
-B
-CB
'�B
*�B
)*B
,qB
+6B
*�B
%FB
'B
�B
�B
B
�B
{B

�B
B
B	��B
_B	�PB	��B	�B	�iB	�)B	��B	�DB	�B	�B	��B	�DB	�B	�B	�fB	��B	�`B	�`B	�2B	�`B	�B	�yB	�,B	� B	�TB	�B	�B	�ZB	�&B	�B	�NB	�B	�B	�NB	�TB	�TB	� B	�B	�B	��B	�B	ߤB	ݘB	�jB	�B	�dB	�B	�B	уB	�)B	�KB	�#B	�B	ҽB	�vB	�NB	҉B	��B	��B	�CB	��B	��B	�hB	��B	��B	��B	��B	��B	�	B	��B	��B	�$B	�B	��B	� B	��B	��B	��B	��B	��B	��B	|PB	s�B	o B	j�B	oiB	h
B	lWB	y>B	h
B	`�B	]dB	[�B	YB	a|B	uZB	e`B	YB	WsB	P�B	TaB	S�B	N�B	K)B	IB	GEB	GB	F?B	EmB	E�B	EB	E�B	FB	E�B	E9B	D3B	CaB	C�B	C�B	CaB	CaB	A�B	A�B	A�B	AUB	?}B	?B	?HB	?}B	>wB	>wB	:�B	8�B	;0B	:�B	7�B	8RB	7�B	8RB	9�B	33B	?}B	4B	8B	.�B	&�B	&�B	&�B	%FB	"hB	"hB	#B	"�B	&�B	 'B	!�B	VB	!�B	"�B	�B	#�B	�B	1B	B	MB	SB	"hB	SB	:B	�B	�B		lB	B	VB	%B		B	YB	�B	B	�B�]B	B	 4B	MB	GB	uB��B�rB�	B�rB�B�.B�B�B�;B��B�rB��B�B�B�B��B�B�B�fB�B��B�|B��B�B�GB��B��B��B�PB��B	
�B	 �B��B�VB�]B��B	 4B	oB��B	 4B��B��B	B	�B�.B��B�xB��B�>B��B��B��B�B��B�B�B�B�B�fB�%B	YB	B	AB	B	�B	�B	MB	SB	+B	�B	�B	
�B	�B	\B	\B	�B	�B	!bB	�B	�B	�B	�B	!B	 'B	%B	%�B	.IB	6zB	8�B	<6B	8�B	8B	:*B	:^B	:�B	<�B	I�B	B'B	@G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'r<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�e<�=y<��<q�(<���<#�
<�<�<w޴<#�
<#�
<#�
<#�
<)�<#�
<#�
<#�
<#�
<#�
<#�
<eWE<�sI<%Ӫ<C�N<N�J<#�
<��<��<�H�<�e><��)<F` </p<,_�<�_}<D!*<#�
<#�
<��4<�>�<�r�<���<#�
<#�
<,��<bi�<7��<^�<K8�<~��<o��<hO�<#�
<#�
<#�
<P��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<H_�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018032422272820180324222728IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018040400033120180404000331QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018040400033120180404000331QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550620190521075506IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                