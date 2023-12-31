CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-01-10T11:45:11Z creation; 2021-03-26T17:01:01Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210110114511  20210326170211  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               >   >AA  AOAO7836_008777_062                 7836_008777_062                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�U��hs@�U��hs11  @�U��@�U��@;�f�P�@;�f�P��d�����8�d�����811  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @B�\@�  @�  @��R@�G�AG�AG�A ��A,(�A?\)A_\)A�  A��A�  A���A�Q�A�Q�A�  A�\)A��BQ�B  B  B (�B(  B0(�B8(�B@  BH  BPQ�BX(�B`  Bh  Bo�
Bw�
B�
B��B��B��
B��B��B��B�{B�(�B�{B��B�{B�  B��B�  B�  B�  B��B�  B�  B��B�  B�(�B�  B��B�  B�{B�{B�  B�  B�  B��B��C
=C��C��C  C

=C
=C
=C
=C{C  C  C
=C��C  C  C��C!�C#��C%��C'��C)��C+��C-��C/��C1��C3��C6
=C8  C9��C<
=C=��C?�CB  CD
=CE��CG��CJ  CL  CN  CP  CR  CS��CV
=CX
=CZ
=C\  C^  C`  Cb
=Cd{Cf
=Ch  Cj  Ck��Cn
=Cp  Cq��Ct  Cv  Cw��Cy��C{��C~  C�  C���C���C���C�C�  C�C�C�
=C�C�  C�  C���C�C�  C��C���C�C�
=C�C���C�C�  C�C���C�C���C�  C�C���C�C�C�C�C�  C���C�  C���C���C���C���C�  C�C�C���C���C���C�  C�C�
=C�  C�  C�  C�C�C���C���C���C�  C�C�  C�C�C�  C���C�  C�  C���C���C�C�  C�C�C�  C���C���C���C���C���C�  C�C�C�
=C�
=C�  C���C���C�  C�  C�  C���C���C�
=C�  C���C�C�  C���C���C�C�
=C�
=C�C���C�  C�  C�  C�C�  C�  C�C�  C�  C�  C���C�  C�C�  C���C�  C�  C�  C���C���C�  C���C���C�  D   D � D �qD� D�qD}qD�qD}qD�qD� DD�D�D��D�qD� D  D��D	�D	}qD
  D
� D�D��D  D}qD  D� D�qDz�D�qD� D�D� D��D}qD  D��D�D� D�qDz�D��D� D  D}qD  D��D�D��D�qD}qD�qD� D�D� D�qD��DD� D  D��DD��D �D � D �qD!}qD"  D"��D#�D#��D$�D$�D%�D%}qD%�qD&}qD'�D'��D'�qD(� D)�D)��D*  D*� D+  D+� D,  D,� D-�D-��D-��D.}qD/  D/� D/�qD0}qD1  D1��D2�D2��D3  D3� D4�D4��D4�qD5z�D5�qD6}qD7  D7� D7��D8z�D8�qD9}qD:  D:� D;  D;� D<  D<� D<�qD=}qD=�qD>}qD>�qD?z�D@�D@��D@�qDA� DB  DB}qDC  DC��DD�DD��DE  DE��DF�DF� DF�qDG��DH  DH� DI  DI� DJ  DJ� DJ�qDK}qDL�DL��DL�qDM}qDM�qDN� DO  DOz�DP  DP� DP�qDQ��DRDR� DS�DS�DT  DT� DUDU}qDU�qDV}qDV�qDW� DX  DX� DY  DYxRDY�RDZz�DZ��D[z�D[�RD\}qD]D]��D^  D^��D^�qD_z�D`�D`��Da  Da}qDa�qDb�Dc  Dc}qDc�qDdz�De  De}qDf  Df� Dg  Dg� Dg��Dh}qDi  Di� Dj  Dj��Dk  Dk� Dl  Dl��Dm  Dm� Dm�qDnz�Dn��Do� Dp�Dp� Dp�qDq}qDr  Dr� Ds  Ds}qDs�RDt� Du  Du}qDv  Dv��Dw  Dw}qDx  Dx� Dx�qDy� Dz�Dzz�Dz�qD{�D|D|��D}D}��D}��D~z�D~�qD� D�HD�AHD�� D�D�  D�@ D��HD���D�HD�AHD�� D�� D�HD�AHD�� D��HD�HD�>�D�~�D���D�  D�@ D�~�D�� D�  D�>�D�� D��HD�HD�B�D��HD�� D�  D�AHD��HD�� D�HD�@ D�� D���D���D�AHD�� D��qD���D�>�D�� D�� D���D�AHD��HD�� D�  D�@ D�� D���D�HD�B�D��HD�� D���D�AHD�� D��qD��qD�@ D�� D���D�HD�@ D�� D��HD�  D�>�D�� D�� D�  D�>�D�� D��HD�  D�@ D�~�D�� D���D�=qD�� D��HD�  D�@ D�~�D�� D�  D�@ D��HD��HD���D�=qD�~�D���D���D�>�D�}qD���D�  D�B�D���D��HD�  D�@ D��HD��HD�  D�AHD��HD��HD���D�AHD��HD��HD�  D�>�D�}qD��qD�  D�B�D��HD��HD�  D�=qD�� D��HD�  D�>�D�}qD���D�  D�>�D�}qD���D�  D�=qD�}qD�� D�  D�>�D�~�D���D�  D�AHD�� D�� D�HD�AHD�� D�� D��D�B�D��HD�� D�HD�B�D��HD�� D���D�>�D�� D��HD�HD�@ D�� D�D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D��qD��qD�@ D��HD�� D�  D�AHD�� D���D���D�>�D�~�D��qD���D�=qD�}qD���D�HD�B�D�� D���D���D�>�D�}qD�� D���D�AHD���D�� D�  D�@ D�� D��HD���D�>�D�~�D���D��D�AHD�� D��HD�HD�@ D�� D���D�HD�AHD�}qD��)D���D�>�D�}qD���D�  D�B�D��HD��HD���D�>�D�� D�� D�HD�>�D�� D�D�  D�=qD�}qD�� D��D�B�D���D��HD�HD�AHD�� D�� D�HD�AHD��HD�� D�HD�@ DHD��HD�HD�AHD�~�DýqD���D�>�DĀ Dľ�D���D�>�Dŀ D�� D�  D�AHDƀ D�� D�  D�AHDǀ D��HD�  D�@ DȁHD��HD�  D�AHDɀ Dɾ�D�HD�AHDʁHD��HD��D�AHDˀ D�� D�  D�>�D̀ D��HD�  D�=qD�~�D;�D�  D�@ D�}qDνqD��qD�=qD�}qD�� D�  D�@ DЁHD�� D���D�=qD�~�D�� D���D�@ D҂�D�D��D�AHDӂ�D��HD�  D�@ DԀ DԾ�D�HD�AHDՀ D�� D�  D�>�D�~�D־�D���D�@ D׀ D�� D���D�=qD�~�D�� D�HD�AHD�~�Dپ�D���D�>�Dڀ D�� D���D�=qD�}qD۾�D�  D�AHD܀ D�� D�HD�AHD݀ D��HD�  D�=qD�~�D��HD�  D�@ D߀ D�� D�  D�@ D���D�D�  D�@ D� D��HD��D�AHD� D⾸D���D�AHD�HD�� D���D�@ D�~�D�� D���D�@ D� D徸D���D�=qD�~�D�� D���D�=qD�}qD羸D���D�>�D�HD�� D�HD�AHD� D�� D�HD�@ D�~�D�� D�  D�@ D� D�� D�  D�>�D� D�� D���D�>�D� D�� D�  D�>�D�~�D��HD�HD�>�D�~�D�qD�  D�@ D�� D��HD�  D�@ D�~�D�D���D�AHD�HD�� D��D�@ D�~�D�D�  D�AHD�HD��HD�  D�@ D�~�D��qD���D�@ D�� D��HD��D�@ D�� D�� D���D�@ D��HD�� D�  D�C�D��HD���D���D�>�D�� D��HD�HD�4{?.{?8Q�?u?���?�p�?�
=?��H@\)@�R@.{@=p�@L��@aG�@p��@�  @���@�\)@�Q�@��\@�=q@�33@�(�@\@˅@�33@��H@��
@�@�z�@�(�A33AffA
�HA�RA33A�A(�A ��A%�A)��A.{A2�\A7�A<(�A@  ADz�AHQ�AL��AQG�AVffAZ�HA_\)Ac�
AhQ�Amp�Aq�AvffAz�HA\)A��A��A��RA�G�A��A�p�A�  A��A�(�A�A�  A�=qA�z�A�ffA�  A��A�(�A�A�\)A���A��
A�{A�Q�A�=qA���A��RA���A��\A���A��RA���A\A���AƸRA���A��HA�z�A�\)Aљ�AӅA�p�A׮Aٙ�AۅA�p�A߮AᙚA�A�A�  A��A�A�p�A�\)A�A��
A�A�  A��A�(�A�ffB Q�BG�B�\B33B��BB�HB  B��B	�B
�HB�
B��B=qB\)B��BB33BQ�Bp�BffB�B��B��B�RB�B��B{B\)B ��B!�B#
=B$Q�B%��B&�RB'�B(��B)�B+
=B,  B-G�B.�\B/�
B1�B2ffB3�
B4��B5B7
=B8(�B9�B:ffB;�B<��B>=qB?�B@��BB{BC
=BD(�BE�BFffBG�BH��BJ{BK�BL��BMBN�RBO�BQ�BRffBS�BT��BU�BV�RBX  BYp�BZ�HB\  B]�B^=qB_33B`z�Ba�Bc
=Bd��Be�Bf�HBh  Bh��Bj=qBk�Bl��BnffBo33Bp(�BqG�Br�HBtQ�Bu��BvffBw�Bx��ByB{\)B|z�B}��B~�\B�B�z�B�
=B���B�{B���B�G�B��B��\B�33B��
B�ffB��HB�\)B��B���B�G�B�  B�z�B�
=B��B�(�B��HB��B�(�B��HB�\)B��B�z�B�
=B�B��\B�33B��
B�ffB��HB��B�=qB�
=B��B�=qB��RB�p�B�{B��HB��B�{B��\B�33B�  B��RB�\)B��
B�Q�B��B��B�z�B��HB���B�ffB��HB�\)B�{B���B�p�B��
B��\B�G�B��B�Q�B�
=B��
B�z�B��HB��B�=qB�
=B��B�  B���B��B�{B�z�B�33B�  B��RB�33B�B�Q�B�33B��B�ffB�
=B��
B�Q�B���B�B�Q�B���B���B�ffB���B�\)B�(�B���B�p�B�  B¸RB�p�B��
Bď\B�\)B��
B�Q�B��BǮB�=qB���BɮB�(�B���B˙�B�=qḄ�BͅB�=qBΣ�B�\)B�=qBиRB�G�B�{BҸRB��B��Bԣ�B�
=B��B֏\B���B�B�ffB���BٮB�Q�BڸRBۅB�=qBܣ�B�p�B�(�Bޏ\B�p�B��
B��\B�G�BᙚB�\B�
=B�B�ffB���B噚B�{B�RB�\)B��
B�RB�
=B��
B�Q�B���B�B�(�B���B�p�B�(�B��HB�\)B�(�B�z�B�\)B�B�\B�
=B�B�Q�B���B���B�  B���B�G�B�(�B�z�B�p�B�B���B���B��
B�Q�B�
=B���B�=qB���B�p�C {C Q�C �C ��CG�C��C�C33Cz�C�HC{Cp�C�RC{C\)C��C  C33C��C�
C33Cz�CC�CffC�C
=C=qC�RC�
C	G�C	�C	�C
(�C
�C
��C
=Cz�C��C�CG�C�C��CG�C�\C�HC33Cz�C�
C{C�C�C�CG�C�RC�C\)C�C��C(�C�\C�HC(�C�\CC=qCffC�
C�CffC��C
=CffC�RC��CffC�\C��CQ�C�C  C33C�\C�HC(�C�\C��C=qCp�C�HC�Cz�C�
C
=Cz�CC{Cz�C�RC  Cp�C�RC��C \)C ��C!  C!Q�C!C"  C"=qC"��C#
=C#=qC#��C$  C$33C$��C$�C%33C%��C%�HC&=qC&��C&��C'=qC'�\C'�
C(G�C(�C(�HC)33C)z�C)�C*(�C*�C*��C+(�C+�\C+�C,(�C,�\C,�C-(�C-�C-��C.33C.�C.��C/(�C/�\C/�C0(�C0��C0�HC1�C1��C1�HC2(�C2�\C2�C3(�C3�\C3��C433C4�\C4��C5(�C5�\C5�HC6(�C6��C6�HC7(�C7��C7�C833C8�C9  C9=qC9�\C:  C:G�C:�\C;  C;ffC;��C;��C<ffC<�
C={C=\)C=��C>33C>z�C>�
C?=qC?�C?�C@33C@��CA
=CAQ�CA��CB
=CBp�CB�RCC  CC\)CCCD(�CDffCD��CE
=CEffCE�CE�CF(�CFQ�CF��CF�CG�CGQ�CGz�CGCH{CHQ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 ?��@   @B�\@�  @�  @��R@�G�AG�AG�A ��A,(�A?\)A_\)A�  A��A�  A���A�Q�A�Q�A�  A�\)A��BQ�B  B  B (�B(  B0(�B8(�B@  BH  BPQ�BX(�B`  Bh  Bo�
Bw�
B�
B��B��B��
B��B��B��B�{B�(�B�{B��B�{B�  B��B�  B�  B�  B��B�  B�  B��B�  B�(�B�  B��B�  B�{B�{B�  B�  B�  B��B��C
=C��C��C  C

=C
=C
=C
=C{C  C  C
=C��C  C  C��C!�C#��C%��C'��C)��C+��C-��C/��C1��C3��C6
=C8  C9��C<
=C=��C?�CB  CD
=CE��CG��CJ  CL  CN  CP  CR  CS��CV
=CX
=CZ
=C\  C^  C`  Cb
=Cd{Cf
=Ch  Cj  Ck��Cn
=Cp  Cq��Ct  Cv  Cw��Cy��C{��C~  C�  C���C���C���C�C�  C�C�C�
=C�C�  C�  C���C�C�  C��C���C�C�
=C�C���C�C�  C�C���C�C���C�  C�C���C�C�C�C�C�  C���C�  C���C���C���C���C�  C�C�C���C���C���C�  C�C�
=C�  C�  C�  C�C�C���C���C���C�  C�C�  C�C�C�  C���C�  C�  C���C���C�C�  C�C�C�  C���C���C���C���C���C�  C�C�C�
=C�
=C�  C���C���C�  C�  C�  C���C���C�
=C�  C���C�C�  C���C���C�C�
=C�
=C�C���C�  C�  C�  C�C�  C�  C�C�  C�  C�  C���C�  C�C�  C���C�  C�  C�  C���C���C�  C���C���C�  D   D � D �qD� D�qD}qD�qD}qD�qD� DD�D�D��D�qD� D  D��D	�D	}qD
  D
� D�D��D  D}qD  D� D�qDz�D�qD� D�D� D��D}qD  D��D�D� D�qDz�D��D� D  D}qD  D��D�D��D�qD}qD�qD� D�D� D�qD��DD� D  D��DD��D �D � D �qD!}qD"  D"��D#�D#��D$�D$�D%�D%}qD%�qD&}qD'�D'��D'�qD(� D)�D)��D*  D*� D+  D+� D,  D,� D-�D-��D-��D.}qD/  D/� D/�qD0}qD1  D1��D2�D2��D3  D3� D4�D4��D4�qD5z�D5�qD6}qD7  D7� D7��D8z�D8�qD9}qD:  D:� D;  D;� D<  D<� D<�qD=}qD=�qD>}qD>�qD?z�D@�D@��D@�qDA� DB  DB}qDC  DC��DD�DD��DE  DE��DF�DF� DF�qDG��DH  DH� DI  DI� DJ  DJ� DJ�qDK}qDL�DL��DL�qDM}qDM�qDN� DO  DOz�DP  DP� DP�qDQ��DRDR� DS�DS�DT  DT� DUDU}qDU�qDV}qDV�qDW� DX  DX� DY  DYxRDY�RDZz�DZ��D[z�D[�RD\}qD]D]��D^  D^��D^�qD_z�D`�D`��Da  Da}qDa�qDb�Dc  Dc}qDc�qDdz�De  De}qDf  Df� Dg  Dg� Dg��Dh}qDi  Di� Dj  Dj��Dk  Dk� Dl  Dl��Dm  Dm� Dm�qDnz�Dn��Do� Dp�Dp� Dp�qDq}qDr  Dr� Ds  Ds}qDs�RDt� Du  Du}qDv  Dv��Dw  Dw}qDx  Dx� Dx�qDy� Dz�Dzz�Dz�qD{�D|D|��D}D}��D}��D~z�D~�qD� D�HD�AHD�� D�D�  D�@ D��HD���D�HD�AHD�� D�� D�HD�AHD�� D��HD�HD�>�D�~�D���D�  D�@ D�~�D�� D�  D�>�D�� D��HD�HD�B�D��HD�� D�  D�AHD��HD�� D�HD�@ D�� D���D���D�AHD�� D��qD���D�>�D�� D�� D���D�AHD��HD�� D�  D�@ D�� D���D�HD�B�D��HD�� D���D�AHD�� D��qD��qD�@ D�� D���D�HD�@ D�� D��HD�  D�>�D�� D�� D�  D�>�D�� D��HD�  D�@ D�~�D�� D���D�=qD�� D��HD�  D�@ D�~�D�� D�  D�@ D��HD��HD���D�=qD�~�D���D���D�>�D�}qD���D�  D�B�D���D��HD�  D�@ D��HD��HD�  D�AHD��HD��HD���D�AHD��HD��HD�  D�>�D�}qD��qD�  D�B�D��HD��HD�  D�=qD�� D��HD�  D�>�D�}qD���D�  D�>�D�}qD���D�  D�=qD�}qD�� D�  D�>�D�~�D���D�  D�AHD�� D�� D�HD�AHD�� D�� D��D�B�D��HD�� D�HD�B�D��HD�� D���D�>�D�� D��HD�HD�@ D�� D�D�  D�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D��qD��qD�@ D��HD�� D�  D�AHD�� D���D���D�>�D�~�D��qD���D�=qD�}qD���D�HD�B�D�� D���D���D�>�D�}qD�� D���D�AHD���D�� D�  D�@ D�� D��HD���D�>�D�~�D���D��D�AHD�� D��HD�HD�@ D�� D���D�HD�AHD�}qD��)D���D�>�D�}qD���D�  D�B�D��HD��HD���D�>�D�� D�� D�HD�>�D�� D�D�  D�=qD�}qD�� D��D�B�D���D��HD�HD�AHD�� D�� D�HD�AHD��HD�� D�HD�@ DHD��HD�HD�AHD�~�DýqD���D�>�DĀ Dľ�D���D�>�Dŀ D�� D�  D�AHDƀ D�� D�  D�AHDǀ D��HD�  D�@ DȁHD��HD�  D�AHDɀ Dɾ�D�HD�AHDʁHD��HD��D�AHDˀ D�� D�  D�>�D̀ D��HD�  D�=qD�~�D;�D�  D�@ D�}qDνqD��qD�=qD�}qD�� D�  D�@ DЁHD�� D���D�=qD�~�D�� D���D�@ D҂�D�D��D�AHDӂ�D��HD�  D�@ DԀ DԾ�D�HD�AHDՀ D�� D�  D�>�D�~�D־�D���D�@ D׀ D�� D���D�=qD�~�D�� D�HD�AHD�~�Dپ�D���D�>�Dڀ D�� D���D�=qD�}qD۾�D�  D�AHD܀ D�� D�HD�AHD݀ D��HD�  D�=qD�~�D��HD�  D�@ D߀ D�� D�  D�@ D���D�D�  D�@ D� D��HD��D�AHD� D⾸D���D�AHD�HD�� D���D�@ D�~�D�� D���D�@ D� D徸D���D�=qD�~�D�� D���D�=qD�}qD羸D���D�>�D�HD�� D�HD�AHD� D�� D�HD�@ D�~�D�� D�  D�@ D� D�� D�  D�>�D� D�� D���D�>�D� D�� D�  D�>�D�~�D��HD�HD�>�D�~�D�qD�  D�@ D�� D��HD�  D�@ D�~�D�D���D�AHD�HD�� D��D�@ D�~�D�D�  D�AHD�HD��HD�  D�@ D�~�D��qD���D�@ D�� D��HD��D�@ D�� D�� D���D�@ D��HD�� D�  D�C�D��HD���D���D�>�D�� D��HD�HG�O�?.{?8Q�?u?���?�p�?�
=?��H@\)@�R@.{@=p�@L��@aG�@p��@�  @���@�\)@�Q�@��\@�=q@�33@�(�@\@˅@�33@��H@��
@�@�z�@�(�A33AffA
�HA�RA33A�A(�A ��A%�A)��A.{A2�\A7�A<(�A@  ADz�AHQ�AL��AQG�AVffAZ�HA_\)Ac�
AhQ�Amp�Aq�AvffAz�HA\)A��A��A��RA�G�A��A�p�A�  A��A�(�A�A�  A�=qA�z�A�ffA�  A��A�(�A�A�\)A���A��
A�{A�Q�A�=qA���A��RA���A��\A���A��RA���A\A���AƸRA���A��HA�z�A�\)Aљ�AӅA�p�A׮Aٙ�AۅA�p�A߮AᙚA�A�A�  A��A�A�p�A�\)A�A��
A�A�  A��A�(�A�ffB Q�BG�B�\B33B��BB�HB  B��B	�B
�HB�
B��B=qB\)B��BB33BQ�Bp�BffB�B��B��B�RB�B��B{B\)B ��B!�B#
=B$Q�B%��B&�RB'�B(��B)�B+
=B,  B-G�B.�\B/�
B1�B2ffB3�
B4��B5B7
=B8(�B9�B:ffB;�B<��B>=qB?�B@��BB{BC
=BD(�BE�BFffBG�BH��BJ{BK�BL��BMBN�RBO�BQ�BRffBS�BT��BU�BV�RBX  BYp�BZ�HB\  B]�B^=qB_33B`z�Ba�Bc
=Bd��Be�Bf�HBh  Bh��Bj=qBk�Bl��BnffBo33Bp(�BqG�Br�HBtQ�Bu��BvffBw�Bx��ByB{\)B|z�B}��B~�\B�B�z�B�
=B���B�{B���B�G�B��B��\B�33B��
B�ffB��HB�\)B��B���B�G�B�  B�z�B�
=B��B�(�B��HB��B�(�B��HB�\)B��B�z�B�
=B�B��\B�33B��
B�ffB��HB��B�=qB�
=B��B�=qB��RB�p�B�{B��HB��B�{B��\B�33B�  B��RB�\)B��
B�Q�B��B��B�z�B��HB���B�ffB��HB�\)B�{B���B�p�B��
B��\B�G�B��B�Q�B�
=B��
B�z�B��HB��B�=qB�
=B��B�  B���B��B�{B�z�B�33B�  B��RB�33B�B�Q�B�33B��B�ffB�
=B��
B�Q�B���B�B�Q�B���B���B�ffB���B�\)B�(�B���B�p�B�  B¸RB�p�B��
Bď\B�\)B��
B�Q�B��BǮB�=qB���BɮB�(�B���B˙�B�=qḄ�BͅB�=qBΣ�B�\)B�=qBиRB�G�B�{BҸRB��B��Bԣ�B�
=B��B֏\B���B�B�ffB���BٮB�Q�BڸRBۅB�=qBܣ�B�p�B�(�Bޏ\B�p�B��
B��\B�G�BᙚB�\B�
=B�B�ffB���B噚B�{B�RB�\)B��
B�RB�
=B��
B�Q�B���B�B�(�B���B�p�B�(�B��HB�\)B�(�B�z�B�\)B�B�\B�
=B�B�Q�B���B���B�  B���B�G�B�(�B�z�B�p�B�B���B���B��
B�Q�B�
=B���B�=qB���B�p�C {C Q�C �C ��CG�C��C�C33Cz�C�HC{Cp�C�RC{C\)C��C  C33C��C�
C33Cz�CC�CffC�C
=C=qC�RC�
C	G�C	�C	�C
(�C
�C
��C
=Cz�C��C�CG�C�C��CG�C�\C�HC33Cz�C�
C{C�C�C�CG�C�RC�C\)C�C��C(�C�\C�HC(�C�\CC=qCffC�
C�CffC��C
=CffC�RC��CffC�\C��CQ�C�C  C33C�\C�HC(�C�\C��C=qCp�C�HC�Cz�C�
C
=Cz�CC{Cz�C�RC  Cp�C�RC��C \)C ��C!  C!Q�C!C"  C"=qC"��C#
=C#=qC#��C$  C$33C$��C$�C%33C%��C%�HC&=qC&��C&��C'=qC'�\C'�
C(G�C(�C(�HC)33C)z�C)�C*(�C*�C*��C+(�C+�\C+�C,(�C,�\C,�C-(�C-�C-��C.33C.�C.��C/(�C/�\C/�C0(�C0��C0�HC1�C1��C1�HC2(�C2�\C2�C3(�C3�\C3��C433C4�\C4��C5(�C5�\C5�HC6(�C6��C6�HC7(�C7��C7�C833C8�C9  C9=qC9�\C:  C:G�C:�\C;  C;ffC;��C;��C<ffC<�
C={C=\)C=��C>33C>z�C>�
C?=qC?�C?�C@33C@��CA
=CAQ�CA��CB
=CBp�CB�RCC  CC\)CCCD(�CDffCD��CE
=CEffCE�CE�CF(�CFQ�CF��CF�CG�CGQ�CGz�CGCH{CHQ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��@��@Ȟ@�aG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA��#A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��`A�ĜA�Q�A��jA�z�A�bA��+A��A��HA��jA���A���A���A���A��\A��7A�VA���A��;A�A�A���A�t�A�ZA�A��A���A��A�"�A�A�O�A�ȴA�(�A��A���A�XA� �A�Q�A�jA���A��yA��RA�?}A���A�+A��A�v�A���A���A�M�A��HA�\)A��A���A���A�1'A���A��A�Q�A���A�ƨA��A��-A���A���A�bA��-A�oAC�A~��A~(�A|�A{�PAz��Aw�-Au�Atz�As��AshsAs?}Ar{Aq�Ao�-An�Al�Aj$�Ah��Ah �Af��AfAe�Ae7LAd�/Ac��Ab(�Aa&�A`��A_�A_��A^jA]�A\�9A[��AZ9XAX�9AWO�AV1AU�-AU�AUdZAT��AT^5AS�-ARĜARr�AR�AQ��AQ7LAP�APM�ANALAK�PAK+AJJAH��AHAFĜAFbNAD�uAA��A@JA?VA>�HA>��A>�RA>��A>�A>ffA>^5A=A<ZA;&�A:�!A9��A9�A8JA7|�A6��A6jA6I�A6$�A5�hA5G�A4jA3��A3x�A2�`A2r�A1ƨA0(�A.�A-/A,�9A,A+�A*��A*�A)C�A(�A(1A'p�A'/A&ȴA&A�A%��A%��A$ �A#?}A"�A"I�A!��A!&�A �uA�TAr�A�A7LAȴAZA�A�A�A%A�\A�Al�A�A/AO�A{A�FAVAbA�yAE�A{A��AK�AȴAVA�AƨA��A\)A	"�AffA��AG�A�A�`A�A33A(�A��A�yAz�A1Ap�A�A ��A z�@��@��@�?}@�1@��-@���@�+@���@�9@�+@�Q�@���@�v�@�7@�ƨ@��#@߾w@ޏ\@���@�V@��
@�33@�=q@�hs@�j@�t�@֗�@�ff@�x�@Ӯ@�o@ҏ\@�@�7L@��/@��
@�C�@�ȴ@�@�?}@�r�@˾w@�;d@��@ʰ!@�E�@ɺ^@�7L@�A�@�;d@ư!@�E�@��@�&�@�j@þw@�ff@���@�G�@�%@�Q�@�l�@��@��j@���@�n�@�-@���@��u@�dZ@�"�@���@���@��7@�Ĝ@�1'@�o@�J@���@���@��m@���@�dZ@�
=@��h@��@���@�-@��@�O�@�Ĝ@��@���@��#@�?}@�V@��u@�|�@�~�@���@���@���@�Z@��;@�\)@��@��-@���@��7@�p�@�O�@�V@�Ĝ@�A�@�b@��@��w@�l�@�@�v�@��@��T@�/@��j@��D@��m@�S�@��@�{@�&�@��9@��u@�1'@��@�|�@�\)@�33@��@�~�@���@���@�  @���@���@��@���@�t�@�C�@��@��R@�ff@��@��T@�p�@��@��/@�Ĝ@�j@�b@��;@��P@�\)@�K�@�+@��H@��\@�ff@�J@���@���@��h@�&�@���@�r�@��@��@���@��@�S�@�@���@�{@���@�p�@�7L@�V@��`@���@�r�@�I�@���@�33@�+@�@��\@�J@�hs@�X@��@���@�I�@��@+@~{@}@}��@}�h@}O�@|�@|�j@|j@{t�@{"�@{o@zn�@zJ@yhs@y7L@x��@xĜ@x�@xA�@xb@w��@w�P@wK�@v�+@u�@u��@u/@t��@t1@s��@s��@s�
@s�F@s�F@s��@sC�@r��@r�@q�7@qhs@p��@pĜ@pbN@o�@o�@o�w@o�P@o
=@n��@n@mp�@m`B@m`B@m/@l��@mV@l�/@l9X@kdZ@j��@jJ@iG�@h��@h �@g�w@g��@gK�@g+@f�@f5?@e��@eO�@d�@dz�@dj@dI�@d1@c�@cS�@c33@c"�@b�@b=q@bJ@a��@a&�@`��@`�9@`1'@`  @_�;@_��@_��@_�w@_�@_�P@_\)@_+@^��@^ȴ@^�+@^V@]�@]�@\�j@\I�@\1@[ƨ@[��@[t�@[dZ@[t�@[t�@[33@Z�\@Z^5@Z�@Y��@Yx�@YG�@Y&�@X�u@W|�@Vv�@Vff@Vff@Vff@V5?@V$�@U��@U�@U`B@U�@T�@TZ@T1@Sƨ@SC�@So@R�@R�\@RM�@R=q@R�@R�@RJ@RJ@Q��@Q��@Qx�@QX@Q�@P��@P��@P  @O�w@O�P@Ol�@O
=@N�@N�+@N{@M�@M`B@M`B@M/@L��@LI�@K�@K"�@J�@J�!@Jn�@I��@I��@I�7@I�@H�9@H��@HQ�@Hb@G�@G�w@G��@G�P@G�P@G\)@G�@F��@F�y@F�@F�R@F��@FE�@E@E/@D��@D�/@D�/@D��@D�j@Dz�@D9X@C��@C"�@C33@C33@B�H@Bn�@B�@A�7@Ahs@Ahs@AG�@@�9@@bN@@A�@@b@?��@?|�@?+@>�y@>�y@>ȴ@>��@=�@=�-@=�-@=/@<�j@<Z@<9X@<(�@;�
@;�F@;�F@;�F@;��@;��@;dZ@;o@:�!@:��@:�\@:~�@:-@9��@9x�@9&�@8��@8�`@8��@8�9@8��@8Q�@7�w@7��@7�P@7|�@7+@6v�@65?@6$�@6$�@6{@5�@5�T@5�T@5��@5@5@5��@5`B@4�/@4�j@4Z@4(�@3��@3dZ@3@2�H@2��@2��@2��@2�\@2n�@2M�@2J@1��@1�7@1x�@17L@17L@1&�@1&�@1%@0��@0 �@/��@/K�@.�y@.��@.V@.$�@-��@-�-@-�-@-�h@-�@-`B@-?}@,��@,�D@,I�@,(�@,�@+��@+ƨ@+�@+33@+o@*�@*��@*��@*��@*��@*��@*~�@*�@)��@)�7@(�@(b@(  @'�;@'�@'l�@'+@'�@&��@&��@&v�@&5?@&@%�T@%�h@%`B@%?}@$�j@$Z@#�m@#��@#��@#dZ@#33@"��@"M�@!�^@!��@!hs@!X@!G�@!%@ ��@ �9@ ��@ r�@   @�w@�P@l�@\)@K�@;d@�@��@�@�+@5?@�T@�-@`B@V@�@�@z�@��@��@�m@�m@��@�H@��@�@�^@��@x�@X@��@�u@�@�@�@�@Q�@Q�@A�@A�@1'@ �@b@  @�@��@�P@+@�@
=@��@�y@�@�R@ff@5?@@��@��@�@`B@?}@��@I�@(�@1@��@ƨ@t�@dZ@C�@33@"�@o@@�@�H@�H@�H@��@��@-@�7@hs@G�@7L@&�@&�@%@Ĝ@�u@Q�@  @��@�;@��@��@l�@l�@l�@l�@\)@�@��@��@E�@$�@�T@@�-@?}@�@��@�D@�D@z�@j@Z@I�@I�@9X@(�@�@�m@��@t�@S�@C�@33@"�@o@
��@
�\@
^5@
=q@
=q@
J@	�#@	�#@	��@	�^@	hs@	7L@Ĝ@�u@r�@bN@b@��@�P@\)@
=@�@ȴ@�R@��@v�@E�@{@�@�@�@�@�@�@�T@�T@�T@�T@�T@@��@`B@�@��@��@�D@�D@�D@�D@�D@1@�F@��@�@�@t�@S�@33@33@33@33@33A��^A��9A���A���A��wA�ĜA��
A��/A��A��A��A��A��A��A��A��A���A��A��A��A��A���A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��TA��yA��A��TA��HA��TA��`A��HA��mA��`A��TA���A���A���A���A���A��hA�|�A�\)A�I�A�?}A�/A�JA��A���A�A��^A��-A���A���A���A���A��DA��+A�z�A�v�A�l�A�I�A�=qA�9XA�5?A�&�A�  A�A���A��HA���A��-A���A��7A�v�A�\)A�^5A�VA�A�A�-A��A�A���A���A��A��TA��HA��;A��;A��;A��A��
A���A�ĜA��wA��RA��!A��!A��A��A���A���A���A���A���A���A���A���A���A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��PA��PA��\A��\A��\A��\A��DA��DA��DA��DA��7A��A�~�A�n�A�dZA�S�A�A�A�A�A�-A�VA���A��jA�ƨA�ƨA�ƨA��yA�oA��A�{A��A��DA�n�A�n�A�Q�A�G�A�1'A�+A�&�A�"�A�
=A���A��wA�t�A� �A�bA���A���A��PA�z�A�n�A�=qA�ȴA�dZA��HA��+A�Q�A��A���A���A�M�A�1A�`BA��;A�VA���A�I�A��
A�bNA��A�ƨA��7A�\)A�&�A��TA��^A���A�~�A�A�A��;A�A��A��+A�$�A��A��PA�Q�A�A�A�$�A�1A�A�A�A���A���A��yA���A�r�A�{A��#A���A�I�A�bA�%A���A��A��TA���A�A��RA���A��\A�|�A�\)A�$�A�%A��A��A�`BA��A��A�-A��yA���A��RA��!A���A���A��7A��+A��+A�z�A�z�A�ffA�E�A�(�A��A���A���A��A��mA���A��A��A��PA�n�A�9XA�oA��A���A���A��+A�t�A�9XA�1A��A��
A��PA�|�A�x�A�v�A�n�A�l�A�dZA�VA� �A���A���A��A�bNA�5?A��A�XA��A���A�z�A�O�A�+A�
=A�A���A��mA�ĜA��!A���A�z�A�\)A�K�A�5?A�/A�$�A��A��A��A�%A���A��A��#A���A��FA���A��7A�p�A�l�A�K�A�/A�1A���A���A��A��A��A��A��A��/A��A��A�A���A���A��hA�~�A�VA�+A��mA���A�VA���A��^A���A���A��A�z�A�p�A�jA�K�A�E�A�;dA�/A�+A� �A�ȴA�A��RA��A��A���A���A��hA�~�A�r�A�Q�A���A���A��FA���A��PA�t�A�ffA�ZA�?}A� �A�
=A���A��A��A���A�A��RA��9A���A��hA�G�A���A��TA���A�ĜA���A�t�A�VA�C�A�9XA�5?A�(�A��A��A��RA���A�`BA��A�JA���A��A��/A���A�~�A�=qA�bA�
=A���A��;A���A��A�?}A��A��A���A�bNA�^5A�9XA��/A��\A�;dA��HA��A��PA�O�A��A���A���A���A��+A�|�A�^5A�C�A�+A�%A��A�O�A��A���A��9A���A�z�A�n�A�G�A�9XA�+A�
=A��A|�At�AdZAC�A33A&�AVA~��A~�A~�HA~��A~ĜA~��A~�uA~z�A~A�A~(�A}�mA}��A}�wA}��A}+A|�RA|z�A|bNA{�mA{A{��A{�PA{dZA{S�A{K�A{/Az�Az�RAz�DAzE�Ay�Ax��Aw�Aw��AwdZAw"�AvȴAv�DAu�-AuXAu&�At��At�At�9At�uAt�Atv�AtQ�At-As��As��As��As�PAs�7As�7Ast�Ast�Ast�AshsAsS�AsS�AsC�AsC�AsG�AsC�As;dAsVAr�9ArA�Aq�Aq�
Aq�wAq��Aq�Aql�AqK�Aq�Ap�yApjAp �Ao��Ao�wAo�hAox�AoO�AoK�Ao;dAo%An�An�uAn�Am��Am�Al��Al9XAk�
AkS�Aj�yAj9XAi�FAix�AiS�Ai/Ah��Ah�jAh��Ah�\Ah�\Ahv�AhQ�AhJAhAg�TAg�7AghsAgoAf�Af��Af�9Af�DAfz�AfI�Af$�AeƨAe�-Ae��Ae��Ae��Ae��Ae�PAe�Ae�Aex�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 A�ƨA��#A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��`A�ĜA�Q�A��jA�z�A�bA��+A��A��HA��jA���A���A���A���A��\A��7A�VA���A��;A�A�A���A�t�A�ZA�A��A���A��A�"�A�A�O�A�ȴA�(�A��A���A�XA� �A�Q�A�jA���A��yA��RA�?}A���A�+A��A�v�A���A���A�M�A��HA�\)A��A���A���A�1'A���A��A�Q�A���A�ƨA��A��-A���A���A�bA��-A�oAC�A~��A~(�A|�A{�PAz��Aw�-Au�Atz�As��AshsAs?}Ar{Aq�Ao�-An�Al�Aj$�Ah��Ah �Af��AfAe�Ae7LAd�/Ac��Ab(�Aa&�A`��A_�A_��A^jA]�A\�9A[��AZ9XAX�9AWO�AV1AU�-AU�AUdZAT��AT^5AS�-ARĜARr�AR�AQ��AQ7LAP�APM�ANALAK�PAK+AJJAH��AHAFĜAFbNAD�uAA��A@JA?VA>�HA>��A>�RA>��A>�A>ffA>^5A=A<ZA;&�A:�!A9��A9�A8JA7|�A6��A6jA6I�A6$�A5�hA5G�A4jA3��A3x�A2�`A2r�A1ƨA0(�A.�A-/A,�9A,A+�A*��A*�A)C�A(�A(1A'p�A'/A&ȴA&A�A%��A%��A$ �A#?}A"�A"I�A!��A!&�A �uA�TAr�A�A7LAȴAZA�A�A�A%A�\A�Al�A�A/AO�A{A�FAVAbA�yAE�A{A��AK�AȴAVA�AƨA��A\)A	"�AffA��AG�A�A�`A�A33A(�A��A�yAz�A1Ap�A�A ��A z�@��@��@�?}@�1@��-@���@�+@���@�9@�+@�Q�@���@�v�@�7@�ƨ@��#@߾w@ޏ\@���@�V@��
@�33@�=q@�hs@�j@�t�@֗�@�ff@�x�@Ӯ@�o@ҏ\@�@�7L@��/@��
@�C�@�ȴ@�@�?}@�r�@˾w@�;d@��@ʰ!@�E�@ɺ^@�7L@�A�@�;d@ư!@�E�@��@�&�@�j@þw@�ff@���@�G�@�%@�Q�@�l�@��@��j@���@�n�@�-@���@��u@�dZ@�"�@���@���@��7@�Ĝ@�1'@�o@�J@���@���@��m@���@�dZ@�
=@��h@��@���@�-@��@�O�@�Ĝ@��@���@��#@�?}@�V@��u@�|�@�~�@���@���@���@�Z@��;@�\)@��@��-@���@��7@�p�@�O�@�V@�Ĝ@�A�@�b@��@��w@�l�@�@�v�@��@��T@�/@��j@��D@��m@�S�@��@�{@�&�@��9@��u@�1'@��@�|�@�\)@�33@��@�~�@���@���@�  @���@���@��@���@�t�@�C�@��@��R@�ff@��@��T@�p�@��@��/@�Ĝ@�j@�b@��;@��P@�\)@�K�@�+@��H@��\@�ff@�J@���@���@��h@�&�@���@�r�@��@��@���@��@�S�@�@���@�{@���@�p�@�7L@�V@��`@���@�r�@�I�@���@�33@�+@�@��\@�J@�hs@�X@��@���@�I�@��@+@~{@}@}��@}�h@}O�@|�@|�j@|j@{t�@{"�@{o@zn�@zJ@yhs@y7L@x��@xĜ@x�@xA�@xb@w��@w�P@wK�@v�+@u�@u��@u/@t��@t1@s��@s��@s�
@s�F@s�F@s��@sC�@r��@r�@q�7@qhs@p��@pĜ@pbN@o�@o�@o�w@o�P@o
=@n��@n@mp�@m`B@m`B@m/@l��@mV@l�/@l9X@kdZ@j��@jJ@iG�@h��@h �@g�w@g��@gK�@g+@f�@f5?@e��@eO�@d�@dz�@dj@dI�@d1@c�@cS�@c33@c"�@b�@b=q@bJ@a��@a&�@`��@`�9@`1'@`  @_�;@_��@_��@_�w@_�@_�P@_\)@_+@^��@^ȴ@^�+@^V@]�@]�@\�j@\I�@\1@[ƨ@[��@[t�@[dZ@[t�@[t�@[33@Z�\@Z^5@Z�@Y��@Yx�@YG�@Y&�@X�u@W|�@Vv�@Vff@Vff@Vff@V5?@V$�@U��@U�@U`B@U�@T�@TZ@T1@Sƨ@SC�@So@R�@R�\@RM�@R=q@R�@R�@RJ@RJ@Q��@Q��@Qx�@QX@Q�@P��@P��@P  @O�w@O�P@Ol�@O
=@N�@N�+@N{@M�@M`B@M`B@M/@L��@LI�@K�@K"�@J�@J�!@Jn�@I��@I��@I�7@I�@H�9@H��@HQ�@Hb@G�@G�w@G��@G�P@G�P@G\)@G�@F��@F�y@F�@F�R@F��@FE�@E@E/@D��@D�/@D�/@D��@D�j@Dz�@D9X@C��@C"�@C33@C33@B�H@Bn�@B�@A�7@Ahs@Ahs@AG�@@�9@@bN@@A�@@b@?��@?|�@?+@>�y@>�y@>ȴ@>��@=�@=�-@=�-@=/@<�j@<Z@<9X@<(�@;�
@;�F@;�F@;�F@;��@;��@;dZ@;o@:�!@:��@:�\@:~�@:-@9��@9x�@9&�@8��@8�`@8��@8�9@8��@8Q�@7�w@7��@7�P@7|�@7+@6v�@65?@6$�@6$�@6{@5�@5�T@5�T@5��@5@5@5��@5`B@4�/@4�j@4Z@4(�@3��@3dZ@3@2�H@2��@2��@2��@2�\@2n�@2M�@2J@1��@1�7@1x�@17L@17L@1&�@1&�@1%@0��@0 �@/��@/K�@.�y@.��@.V@.$�@-��@-�-@-�-@-�h@-�@-`B@-?}@,��@,�D@,I�@,(�@,�@+��@+ƨ@+�@+33@+o@*�@*��@*��@*��@*��@*��@*~�@*�@)��@)�7@(�@(b@(  @'�;@'�@'l�@'+@'�@&��@&��@&v�@&5?@&@%�T@%�h@%`B@%?}@$�j@$Z@#�m@#��@#��@#dZ@#33@"��@"M�@!�^@!��@!hs@!X@!G�@!%@ ��@ �9@ ��@ r�@   @�w@�P@l�@\)@K�@;d@�@��@�@�+@5?@�T@�-@`B@V@�@�@z�@��@��@�m@�m@��@�H@��@�@�^@��@x�@X@��@�u@�@�@�@�@Q�@Q�@A�@A�@1'@ �@b@  @�@��@�P@+@�@
=@��@�y@�@�R@ff@5?@@��@��@�@`B@?}@��@I�@(�@1@��@ƨ@t�@dZ@C�@33@"�@o@@�@�H@�H@�H@��@��@-@�7@hs@G�@7L@&�@&�@%@Ĝ@�u@Q�@  @��@�;@��@��@l�@l�@l�@l�@\)@�@��@��@E�@$�@�T@@�-@?}@�@��@�D@�D@z�@j@Z@I�@I�@9X@(�@�@�m@��@t�@S�@C�@33@"�@o@
��@
�\@
^5@
=q@
=q@
J@	�#@	�#@	��@	�^@	hs@	7L@Ĝ@�u@r�@bN@b@��@�P@\)@
=@�@ȴ@�R@��@v�@E�@{@�@�@�@�@�@�@�T@�T@�T@�T@�T@@��@`B@�@��@��@�D@�D@�D@�D@�D@1@�F@��@�@�@t�@S�@33@33@33@33G�O�A��^A��9A���A���A��wA�ĜA��
A��/A��A��A��A��A��A��A��A��A���A��A��A��A��A���A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��TA��yA��A��TA��HA��TA��`A��HA��mA��`A��TA���A���A���A���A���A��hA�|�A�\)A�I�A�?}A�/A�JA��A���A�A��^A��-A���A���A���A���A��DA��+A�z�A�v�A�l�A�I�A�=qA�9XA�5?A�&�A�  A�A���A��HA���A��-A���A��7A�v�A�\)A�^5A�VA�A�A�-A��A�A���A���A��A��TA��HA��;A��;A��;A��A��
A���A�ĜA��wA��RA��!A��!A��A��A���A���A���A���A���A���A���A���A���A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��PA��PA��\A��\A��\A��\A��DA��DA��DA��DA��7A��A�~�A�n�A�dZA�S�A�A�A�A�A�-A�VA���A��jA�ƨA�ƨA�ƨA��yA�oA��A�{A��A��DA�n�A�n�A�Q�A�G�A�1'A�+A�&�A�"�A�
=A���A��wA�t�A� �A�bA���A���A��PA�z�A�n�A�=qA�ȴA�dZA��HA��+A�Q�A��A���A���A�M�A�1A�`BA��;A�VA���A�I�A��
A�bNA��A�ƨA��7A�\)A�&�A��TA��^A���A�~�A�A�A��;A�A��A��+A�$�A��A��PA�Q�A�A�A�$�A�1A�A�A�A���A���A��yA���A�r�A�{A��#A���A�I�A�bA�%A���A��A��TA���A�A��RA���A��\A�|�A�\)A�$�A�%A��A��A�`BA��A��A�-A��yA���A��RA��!A���A���A��7A��+A��+A�z�A�z�A�ffA�E�A�(�A��A���A���A��A��mA���A��A��A��PA�n�A�9XA�oA��A���A���A��+A�t�A�9XA�1A��A��
A��PA�|�A�x�A�v�A�n�A�l�A�dZA�VA� �A���A���A��A�bNA�5?A��A�XA��A���A�z�A�O�A�+A�
=A�A���A��mA�ĜA��!A���A�z�A�\)A�K�A�5?A�/A�$�A��A��A��A�%A���A��A��#A���A��FA���A��7A�p�A�l�A�K�A�/A�1A���A���A��A��A��A��A��A��/A��A��A�A���A���A��hA�~�A�VA�+A��mA���A�VA���A��^A���A���A��A�z�A�p�A�jA�K�A�E�A�;dA�/A�+A� �A�ȴA�A��RA��A��A���A���A��hA�~�A�r�A�Q�A���A���A��FA���A��PA�t�A�ffA�ZA�?}A� �A�
=A���A��A��A���A�A��RA��9A���A��hA�G�A���A��TA���A�ĜA���A�t�A�VA�C�A�9XA�5?A�(�A��A��A��RA���A�`BA��A�JA���A��A��/A���A�~�A�=qA�bA�
=A���A��;A���A��A�?}A��A��A���A�bNA�^5A�9XA��/A��\A�;dA��HA��A��PA�O�A��A���A���A���A��+A�|�A�^5A�C�A�+A�%A��A�O�A��A���A��9A���A�z�A�n�A�G�A�9XA�+A�
=A��A|�At�AdZAC�A33A&�AVA~��A~�A~�HA~��A~ĜA~��A~�uA~z�A~A�A~(�A}�mA}��A}�wA}��A}+A|�RA|z�A|bNA{�mA{A{��A{�PA{dZA{S�A{K�A{/Az�Az�RAz�DAzE�Ay�Ax��Aw�Aw��AwdZAw"�AvȴAv�DAu�-AuXAu&�At��At�At�9At�uAt�Atv�AtQ�At-As��As��As��As�PAs�7As�7Ast�Ast�Ast�AshsAsS�AsS�AsC�AsC�AsG�AsC�As;dAsVAr�9ArA�Aq�Aq�
Aq�wAq��Aq�Aql�AqK�Aq�Ap�yApjAp �Ao��Ao�wAo�hAox�AoO�AoK�Ao;dAo%An�An�uAn�Am��Am�Al��Al9XAk�
AkS�Aj�yAj9XAi�FAix�AiS�Ai/Ah��Ah�jAh��Ah�\Ah�\Ahv�AhQ�AhJAhAg�TAg�7AghsAgoAf�Af��Af�9Af�DAfz�AfI�Af$�AeƨAe�-Ae��Ae��Ae��Ae��Ae�PAe�Ae�Aex�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B�%B�ZB��B��B�B�B�B�B�B�B�B�B�GB�B�MB�B�rB�2B�TB�AB�]B�B�B��B�B�B�8BیB�
B��B��BƨB��B��B�B{�BK�BfB�cB��B��BÖB�B��B��B��B��B�B��Bp�Bc�BT�BM�B1�B_B��B��B�B� B��B�sB�BҽBĜB�<B�nB�}B�VB�MB�PB��B{JBj�B`vBR�BB�B 'BB�B��B�TB�B�"B�ZB�sB�[BǮB��B�OB�RB�B�VB��B�:B�YB�;Bu�Bc�BUgBQBK�BFB?�BC-BA B9�B.�B,�B/�B,qB)*B%B	BB�B�B
�B
�AB
�8B
�B
� B
�B
�vB
�]B
�yB
��B
��B
ϫB
̘B
�KB
��B
�HB
��B
�wB
��B
��B
��B
��B
� B
��B
�B
|B
ncB
f2B
]dB
\)B
[WB
ZQB
ZB
X�B
W�B
T�B
TaB
OB
HB
DgB
A�B
<�B
9�B
6B
2�B
/�B
.}B
-wB
+6B
(�B
'�B
!bB
 \B
B
�B
�B
B
�B
�B
AB	��B	�xB	��B	�B	�B	�cB	�/B	�B	�B	�mB	�mB	��B	�B	�B	�#B	�B	�WB	�EB	�sB	�aB	�B	�pB	��B	��B	�B	��B	��B	�OB	��B	��B	��B	��B	��B	�qB	�B	�B	��B	�	B	�B	��B	��B	��B	��B	��B	��B	��B	�@B	��B	��B	�B	�B	�VB	��B	��B	�MB	��B	�;B	�B	�4B	{JB	z�B	y>B	w2B	u�B	u%B	sB	q�B	o B	r�B	k�B	h�B	gB	e�B	aB	]/B	`�B	^�B	YB	[�B	W
B	U�B	VmB	TaB	R�B	T,B	QB	Q�B	P�B	PHB	OB	O�B	OvB	O�B	O�B	OBB	M6B	P}B	NpB	NB	N�B	N<B	M�B	M6B	OB	NB	M�B	N�B	N�B	O�B	O�B	O�B	OBB	OBB	OvB	OBB	N�B	O�B	QB	P}B	PHB	OvB	P�B	P�B	O�B	S�B	S�B	TaB	S[B	T,B	T�B	W�B	X�B	[�B	Z�B	Z�B	[�B	^5B	`�B	`BB	`BB	`�B	c�B	d�B	e�B	jB	m�B	n�B	qAB	s�B	u�B	u�B	u�B	y�B	}"B	�;B	�uB	�B	�B	��B	��B	�7B	�B	��B	�B	��B	��B	�:B	��B	�kB	�B	�OB	�bB	�:B	��B	��B	�*B	��B	��B	��B	�_B	�_B	��B	�=B	��B	�B	�CB	��B	��B	�OB	��B	�hB	�LB	��B	�0B	��B	�qB	�-B	�B	�B	�RB	��B	��B	�B	бB	уB	�&B	�aB	�B	�5B	�,B	��B	��B	�B	�mB	�B	�yB	�B	�]B	��B	�AB	�B	�+B	��B	��B	�xB	�"B	�cB
 �B
B
�B
�B
�B
�B
	B

=B
�B
�B
VB
\B
�B
{B
YB
�B
1B
B
�B
�B
!-B
"4B
(�B
+�B
-CB
/B
0UB
1�B
4B
5tB
6zB
<6B
?B
>�B
?�B
C-B
G�B
L�B
L�B
N�B
R�B
UgB
W
B
Z�B
_�B
`�B
aHB
a|B
b�B
d�B
e`B
f�B
kB
l"B
l"B
n�B
qvB
sMB
tTB
u%B
v+B
x8B
y�B
{JB
|�B
}�B
�B
��B
��B
��B
��B
�\B
�uB
�FB
�B
�YB
�+B
�+B
�+B
�eB
�=B
��B
��B
�\B
��B
��B
�FB
��B
��B
��B
�*B
��B
��B
�OB
�'B
�[B
��B
�-B
��B
�B
�nB
��B
�B
�0B
�jB
��B
�BB
��B
��B
�[B
�aB
ÖB
�mB
�B
��B
�dB
�B
��B
�NB
ѷB
ҽB
�2B
��B
֡B
�9B
��B
��B
�B
�QB
�]B
��B
�5B
�vB
�|B
�B
�B
�NB
�B
�B
� B
�B
��B
�B
�fB
�
B
�B
��B
��B
�/B
�iB
��B
�B
�B
�GB
�B
�B
��B
��B
��B
�+B
��B
�fB
��B
�	B
�rB
�B
��BABABABAB�B�B{B�B�B�B�B�B1B	�B
�BDBBJB�B�BPB�B�BBVB�B�B�B.B�BB�BB�B{BFBB�B�BkB�B�BqB�BIB�B�B�B \B �B!bB!�B!�B#:B#nB$B$�B%�B&�B(XB(�B)*B(�B)�B*eB*�B*0B*0B)�B*0B*�B,B-B.�B/�B0UB0!B0!B0�B0�B1�B1�B2�B2�B3�B3�B3�B49B4�B49B5B6zB8RB9�B9�B:^B;dB;�B<B;�B;�B;�B<�B<�B<�B=�B>B>�B>wB>�B?}B?}B?B?B?}B?HB?HB@B@�B@�B@�B@�BAUBA�BA�BB�BB�BC-BCaBCaBC-BD3BEmBEmBEmBEmBF?BG�BHKBH�BIRBI�BJXBJ�BJXBJ�BK)BK^BK�BL�BM�BNBNpBOBPBP�BRTBRTBR�BR�BR�BS&BS&BS[BT,BTaBT�BT�BU�BU�BU�BU2BU2BU�BW?BXBX�BY�BZ�BZ�B[#B[�B[�B\)B\]B\]B\)B\]B]/B^jB^�B_B_B_;B_�B`BBaBaBaBaHBa|BaHBaHBa|Ba|BbNBbBbBd�Bd�Bd�BdZBd�Be`Be�Be�BffBgBgmBh
Bg�Bh
Bh�BhsBhsBiyBjKBj�BkBkBkQBkBk�Bk�Bl�Bl�Bl�Bl�Bl�Bm)Bm]Bm)Bm)Bm�Bn�Bo BoiBoiBo�BpBo�BpBp;BpoBqBq�BrGBr|BsMBs�Bs�Bs�BtBu%Bu%Bu%Bt�Bu%Bu�Bv`BwfBwfBw2Bw2Bv�Bw�Bx8BxlBx8BxBxBx�Bx�Bx�Bx�BxlBx�Bx�Bx�Bx�Bx�ByrBy�By�By�By�By�BzBzDBzBzBzDBzxBz�Bz�B{B{B|B|�B|�B|�B|�B}�B}�B~(B~(B~]B~]B~(B~]B~]B~]B~(B}�B}�B}�B.B� B� B� B�4B�4B�B�B�iB�iB�iB�B�;B�B�;B��B�B�AB�B��B�AB�uB�uB�B�{B�{B��B��B��B��B�MB��B�B�B�B�B�B�SB�SB��B��B�SB��B�%B�%B�%B�YB�YB��B��B�+B�_B��B��B��B��B�1B��B��B��B��B��B�B�B�B�B��B��B�	B�	B��B��B��B�B�DB�xB��B�JB�~B�~B�JB�~B��B��B��B�~B�~B�~B�~B��B��B�PB�PB��B��B�"B�"B�"B��B��B�(B��B��B��B��B��B�"B�VB�VB��B��B�VB�8B��B�B��B�"B��B��B�	B��B�B��B�TB�B��B��B��B�B�B��B��B��B�TB�%B�TB�ZB�%B��B�ZB�ZB�%B��B�B�ZB�%B��B��B�B�ZB�%B�%B��B��B�B�%B�%B�B��B��B�B�B�TB��B��B��B�B�B�|B�B�|B�|B�B�B�|B��B�GB�B�|B�GB�B�B�B�|B�|B�B�B�B��B�B�B�;B��B�B�B�B�B�AB�B�B�oB��B�%B�%B�TB�B��B�%B��B��B��B�2B��B�B�B�B��B��B�B�lB�8B��B�8B��B��B��B�fB�>B�`B�B�B�2B�B�iB�vB�B�5B�B��B��B�B��B�/B�B�cB�cB�5B��B�QB�B�)B�B�yB�KB��B�B�QB�B�B�KB�B�B�DB�B�DB�B�B�B�B�B�B�KB�B�B�yB�B�B�B�DB�KB�B��B�KB��B�DB�>B�B�B�fB��B�fB��B�jB�]B��B�KB�yB�EB�EB�EB�yB�?B֡B�gB�aB��B�aB֡B��BуB�}B��B�vB��BȴB�9B�9B�HB��B��B�B��BҽB�dB��B�?B��B��B�jB��B�[B��B�CB�wB��B��B��B��B�B�uB��B��B��B��B��B|�B��By�B}VBs�Bq�Bo�Bf�B\]Bg�BC�B>�B3�B!BB;BB�B��B�lB��B�rB�/B�B�>B�cB�B@B�BݘB�,B�6B�B��B��B��BŢB��B�HB��B� B�B�HB��B�0B�gB�3B�9BƨB��B�LB��B��B�B�tB��B��B��B��B�UB�aB��B��B�0B�hB��B�!B�kB�B�tB��B�1B��B��B�eB�B��B�FB��B�@B�4B��B�\B�DB��B��B��B��B��B�BzDBpBqABrBl�BjKBh
BjB`BB`BBc�BaBYBZ�B\�BR�BOBNpBN�BK�BI�BHKBL�BYBQ�B*�B*0B1[B($BDgB#nBBJB�B+B{B 4B��B  BAB��B��B��B��B�	B��B�B�+B�TB��B�B��B�AB�cB��B� B�|B�B�B�B�>B�+B�B� B�B�KB��B�B�KB�B�B��B�B�B�DB�TB�&B�;B��B�B�B��B��B�BخBΥB�^B�B��B�tB�?B�BȴB��B�OB��B��B�B�B�B��B�B��B�zB��B�B��B�!B�?B��B�OB��B�$B�XB�zB��B��B�B��B�B�7B�kB�B�kB��B�B��B�bB�4B��B��B�=B��B��B�lB��B��B�iB~�B|�B|�B|�B��B}"B|By>Bv�Bh
Bh�Bl"Bj�BkBo5BhsBbNB\)BcTBb�BaB\�B\�BWsBV�B]/BK�BGEBMjBOB@�B@�BF�BK�B/OB,�B&�B!�B	BeB�BB�B�B\BSBCB
�B+BB;B iBGB�B�B��B�JB��B��B��B�+B��B�%B�TB�TB��B��B�B�B�B�B��B�)B�cB�vB��B��B��B�2B�sB�,B��B�KB�jB�BچB�?BרBخB�gBӏB��B�gB��B��BӏB�B�#B˒B�9B��B�}B�UB�B��B��B�B�LB�nB��B��B��B�B�OB�wB��B�*B�B�B��B�nB��B�-B�B�B��B�-B�'B��B��B�OB�~B�4B��B��B��B�SB�SB�{B��B�:B�:B�bB�B��B�"B��B��B�MB��B�;B~]B�{B��BcB��B��B}�By�Bs�BrBm�BjKBl�Bk�B`�BY�BZBW�BW�BU�BS&BR�BP}BR�BRTBR�BNBQBMjBNBOBJ�BIRBIRBH�BGBF�BIRBHBB[BA�BA B@OB?}B@�B@�B?�B?�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2021011011451120210110114511IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143320210220011433QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143320210220011433QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164520210325101645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                