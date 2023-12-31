CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-10-26T03:21:23Z creation; 2022-02-04T23:30:05Z DMQC;      
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
_FillValue        G�O�     0  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \8   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     0  �    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 6�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 >T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 eP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20211026032123  20220204223518  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_186                 6810_008521_186                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٝ�.�S@ٝ�.�S11  @ٝ�`A�7@ٝ�`A�7@0Tw�@0Tw��dg�p����dg�p���11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@�G�@��\@�G�@޸R@�p�A\)A   A+�A@  A`  A\)A�Q�A���A�  A�  A�  A�  A�  A��B  B  B  B (�B'�
B/�B8  B@  BH  BP  BX  B_�
Bg�Bo�
Bx  B�
B�  B��B��B�  B�  B��B��B�  B�{B�{B�  B�  B�  B��B�  B��B�  B�(�B�{B��
B�  B�{B�{B�{B�  B��
B�  B�{B��B��B�  C 
=C  C��C  C  C	��C�C��C
=C{C
=C
=C��C��C  C��C   C!��C#��C&  C(
=C)��C+��C.  C0  C2
=C4{C6
=C8
=C:
=C;��C=�C?��CB{CD{CF{CH
=CJ  CK��CN  CP
=CR{CT  CV  CX{CZ
=C\  C^  C_��Cb  Cd  Ce��Ch
=Cj
=Ck��Cn{Cp  Cq��Cs�Cv
=Cx  Cy��C{�C}��C��C���C�  C���C���C���C���C���C�  C�  C�C�  C�C�  C�C�  C�  C�
=C�  C���C�  C���C�C�  C�  C�C�  C�  C�
=C�
=C���C�  C�  C�C���C���C���C�  C�  C�  C���C�  C�  C�C�C�  C�  C�  C�C�  C�C�
=C���C�  C���C�  C�  C�C���C�  C���C�  C�  C�  C���C�  C�  C���C�\C�  C�  C�
=C�  C���C���C���C���C�  C�  C�C�C�C�C�  C���C�C���C�  C�  C���C�C�C���C�  C�
=C�
=C���C�C�C�  C�
=C�
=C�C�  C�  C�  C���C�  C�  C�  C�
=C�
=C�  C�  C�  C�  C�  C���C�  C�  C�  C�C�C�
=C�C�  C�  C�  C���D }qD �qD� D  D� D  D� D�qD}qD��D� D�D��D�D� D  D}qD	  D	� D	�qD
z�D
�qD� D�D� D�qD��D�D}qD  D��D�qD� D  D� D�qD� D  D}qD�qD��D�D��D�D� D�qD}qD�qD��D  D� D�D� D  D� D  D}qD��D}qD�qD}qD  D��D �D � D �qD!� D!�qD"� D#  D#� D$  D$� D%�D%� D%�qD&� D'�D'��D(�D(��D)�D)��D*  D*}qD+  D+� D,  D,� D-  D-� D-�qD.}qD/  D/}qD/�qD0}qD0�qD1� D2  D2}qD2�qD3��D4D4� D4�qD5� D6  D6}qD6�qD7}qD8  D8��D9�D9� D9�qD:� D;D;�D<�D<� D=�D=� D=�qD>}qD>��D?}qD@�D@� DA  DA��DB  DB}qDC�DC��DD�DD�DE�DE� DF  DF� DGDG� DH�DH�DH�qDI}qDJ  DJ��DKDK��DL  DL� DM  DM}qDM��DN}qDO  DO� DP  DP��DQ  DQ� DR  DR� DS�DS� DS�qDT}qDT��DU� DV  DV� DW�DW}qDX  DX� DX�qDY��DZDZ��DZ�qD[� D\  D\}qD\�qD]� D^�D^��D_�D_��D`  D`� Da  Da��Db�Db� Dc�Dc��Dd  Dd}qDe�De� Df�Df�Dg�Dg� Dh  Dhz�Dh�qDi��DjDj� Dk  Dk��Dl�Dl}qDl�qDm��Dn�Dn� Do�Do� Do�qDp� Dq  Dqz�Dq��Dr}qDs  Ds��Dt  Dt}qDu  Du}qDu�qDv}qDw�Dw�Dx  Dx� Dy�Dy� Dy�qDzz�Dz��D{}qD|  D|� D|�qD}}qD}��D~��D�D��D�HD�AHD���D��HD�  D�@ D�}qD��qD��qD�@ D�~�D�� D��D�@ D�� D���D�  D�@ D�� D��HD�  D�@ D�}qD�� D���D�=qD�� D��HD���D�@ D�� D��HD���D�>�D�� D�� D�  D�@ D�}qD���D��D�B�D�� D�� D���D�>�D���D��HD�  D�@ D�~�D��qD���D�AHD�� D���D�  D�AHD�� D��qD�  D�AHD�~�D�� D�HD�>�D�� D���D���D�@ D���D��HD���D�@ D��HD�� D�  D�AHD��HD�D�HD�@ D�� D�D�  D�>�D��HD���D���D�AHD���D�� D��qD�@ D�� D��HD�HD�@ D��HD��HD�HD�AHD�~�D���D��qD�>�D�� D���D�  D�AHD��HD�D��D�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D��HD��HD�  D�>�D�~�D�� D�  D�AHD��HD���D���D�@ D�� D�D�HD�AHD�� D��HD��D�@ D�~�D���D�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�@ D�~�D���D�  D�@ D�~�D���D�HD�@ D�� D�� D�HD�AHD�� D�� D���D�AHD��HD���D�  D�B�D��HD��qD��qD�>�D�� D��HD�HD�AHD�� D��HD�HD�>�D�� D��HD�  D�>�D�~�D���D�  D�>�D�~�D�D��D�>�D�}qD���D�HD�B�D��HD���D��qD�>�D��HD��HD�HD�B�D���D�� D���D�@ D��HD�� D�  D�@ D�� D�� D�HD�>�D�~�D��HD�HD�AHD�� D�� D���D�>�D��HD�� D�  D�=qD��HD��HD���D�@ D�� D�� D�  D�@ D�� D���D�  D�>�D�~�D�� D���D�>�D�� D�� D���D�>�D���D�� D�  D�AHD�� D���D���D�@ D�� D�� D�HD�AHD D��HD�HD�AHDÁHDþ�D���D�AHD�~�Dľ�D�  D�@ Dŀ D�� D��qD�@ DƁHD��HD�  D�@ Dǀ D��HD�HD�@ DȀ D�� D���D�>�D�}qD�� D�HD�AHDʁHD�� D�  D�=qD�}qD˾�D���D�@ D̂�D�� D�  D�AHD�~�D;�D�HD�AHD΁HD�D�HD�AHDπ DϾ�D�  D�AHDЂ�D��HD���D�=qDр D��HD�  D�@ D�~�DҽqD��qD�@ DӁHD�D�  D�@ DԂ�D��HD��qD�=qD�}qD��HD��D�>�D�}qD־�D�  D�@ Dׂ�D�� D�  D�@ D�~�D��HD�HD�@ D�~�D�� D�HD�AHDځHD��HD�HD�AHDۂ�D�� D���D�@ D�~�D�� D�HD�@ D݁HD�� D���D�@ Dހ D�� D���D�>�D߁HD��HD�HD�AHD���D�� D�  D�AHD�HD�D��D�@ D� D�� D�  D�AHD�HD㾸D�  D�B�D�HD��HD�  D�>�D�~�D�� D��D�@ D� D��HD�  D�@ D�HD�� D�  D�@ D� D��HD�HD�AHD�HD龸D���D�@ D� D�� D���D�>�D�HD뾸D���D�@ D� D쾸D���D�@ D�HD��HD�  D�AHD� DD�HD�@ D�~�D��HD�  D�AHD�� D�D���D�=qD�~�D�� D�  D�AHD� D�D�  D�@ D�~�D�� D�  D�@ D� D��HD�HD�>�D�}qD��qD���D�@ D��HD���D���D�@ D�� D��HD��{>�G�?L��?�  ?�z�?\?�(�?��H@�@�R@.{@E�@Tz�@aG�@z�H@��@�=q@�z�@�  @��@�\)@��H@��
@Ǯ@У�@ٙ�@�  @���@��@�Q�A�AffA	��A��A�\A�A=qA\)A#33A'
=A,(�A0  A3�
A6ffA;�A?\)AA�AG
=AK�AN{AR�\AVffAX��A\��Aa�AfffAi��Amp�As33AvffAy��A~�RA��A��A��A��A���A��HA�p�A��A���A�33A�A��RA���A��A��A�ffA�Q�A��HA�(�A�p�A�  A��A��HA���A�
=A���A���A�(�A�ffA�\)A���A��A�z�A��RA�G�A\A�(�AƸRA���A��A�z�A�ffA�  Aљ�A�(�A�ffA�\)A�G�A��
A�A�
=A���A��HA�(�A�A��A�=qA�33A�A�  A���A�\A��A��RA�Q�A��\A�z�A�A��B ��B�BffB�B��B��BffB
=B(�B	G�B	�B
�RB�
B��Bp�B�RB\)B(�BG�B{B�RB�
B��Bp�B=qB�B(�B��B=qB
=B�B��BBffB�B z�B!�B!�B#33B#�
B$��B%B&�RB'\)B(Q�B)p�B)B*�HB,  B,��B-p�B.�RB/\)B0  B1�B2{B2�RB3�B4��B5��B6=qB7\)B8Q�B8��B9�B;
=B;�
B<Q�B=��B>�RB?33B@(�BAG�BB=qBB�RBC�
BD��BE��BF=qBG\)BHz�BI�BIBJ�RBL  BL��BMG�BN�\BO�BPQ�BP��BQ�BS
=BT  BT��BUp�BV�\BW�BX(�BY�BZ=qB[
=B[�B\��B]B^ffB_33B`z�Bap�Ba�Bc
=Bd(�Bd��Bep�BfffBg�Bh��BiG�Bi�Bk33Bl(�Bl��Bm�Bn�HBo�Bp��BqBr=qBs33Btz�Bup�Bv{Bv�HBx(�By�By�Bz�\B{�B|��B}p�B~=qB�B�=qB��\B���B��B�{B�ffB���B�\)B��B�Q�B���B�33B�B�{B�ffB���B�p�B�  B�=qB��RB�G�B��B�=qB��\B�33B��B��B�ffB��B��B��
B�Q�B��HB�p�B��B�=qB���B�33B���B�  B���B�
=B�\)B��B�z�B��HB�33B��B�=qB���B�
=B�p�B�{B�ffB���B�33B�B�Q�B��RB�
=B���B�(�B�z�B��HB��B�  B�=qB���B�\)B�B�{B��\B�33B�B�  B�z�B�
=B���B��B�Q�B��HB�p�B��B�{B���B�G�B���B��B�z�B��B��B��
B�ffB���B�G�B��B�Q�B���B�33B���B�(�B��RB�
=B��B�{B���B���B�G�B��
B�z�B���B�33B��
B�=qB���B���B���B�{B�Q�B��HB�p�B��B�(�B���B�G�B�B�  B�ffB��HB�p�B��
B�(�B���B�33B��B��
B�ffB��HB��B��B�{B�z�B���B�G�B��
B�(�B�z�B��HB�p�B�  B�=qBģ�B�33BŮB��B�Q�B���B�p�B�B�  Bȏ\B��B�p�B�B�=qBʸRB��B�\)B�B�=qB�z�B���B�33BͮB�{B�Q�BΣ�B��Bϙ�B��
B�Q�B���B�G�Bљ�B�  B�ffB���BӅB��
B�Q�B��HB�G�BծB�{B֏\B��BׅB��
B�Q�B���B�\)BٮB�{Bڣ�B��B�p�B��
B�z�B���B�\)B�B�{Bޣ�B�33Bߙ�B��B�ffB���B�p�B�B�(�B��B�33B�B�{B�z�B���B噚B�{B�ffB���B�\)B��B�ffB���B�33B��
B�ffB���B�33B뙚B�(�B�RB�G�B��B�{B��B�G�B�B�(�B��B�G�B�B�{B��B�G�B�B�=qB���B�G�B��
B�Q�B���B��B��B�Q�B��RB��B���B�(�B���B��B��B�  B���B�33B��B�{B��\B���B�\)C 
=C G�C �\C ��C  C33C�C��C  C33Cz�CC{CG�Cz�C�RC  C\)C��C��C
=C\)C��C�HC{C\)C��C��C(�C\)C��C��C=qCp�C��C�C	G�C	�\C	C
  C
=qC
�\C
�HC(�Cp�C��C�HC(�Cz�C��C  C33Cp�C�RC{C\)C��C��C{CQ�C��C��C=qCz�C�RC��CQ�C��C�
C{CffC�RC  C33Cz�CC�CffC��C�
C(�Cz�CC  C33Cz�CC{CffC�C�HC�Cp�C�RC  C=qCp�C��C�C33C�C�RC��C33C�C�
C�C\)C��C�HC33C�\C��C
=CG�C�\C�HC33Cz�C�RC��C 33C z�C �
C!{C!Q�C!�C!C"
=C"Q�C"��C"�C#�C#Q�C#��C#�HC$33C$p�C$��C$�HC%33C%z�C%C&
=C&G�C&�C&C'�C'p�C'�RC'��C(33C(z�C(��C)�C)ffC)��C)�
C*�C*p�C*�RC+  C+G�C+�\C+C,  C,Q�C,��C-  C-33C-p�C-�RC.{C.ffC.�C.�C/(�C/p�C/��C0�C0ffC0��C0�HC1(�C1p�C1C2{C2Q�C2�\C2��C3�C3p�C3C4{C4Q�C4��C4�
C5(�C5z�C5��C6{C6Q�C6�\C6�HC7=qC7�\C7�HC8(�C8p�C8�C9  C9Q�C9��C9�C:(�C:ffC:C;{C;ffC;��C;��C<33C<�\C<�C==qC=z�C=�RC>
=C>Q�C>�C?  C?Q�C?��C?�HC@(�C@p�C@�RCA
=CAffCA�RCB
=CBQ�CB�\CB�
CC�CCp�CCCD�CDp�CD�RCE  CEG�CE��CE�CFQ�CF�\CF��CG{CGffCG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                         ?�  @   @B�\@�G�@��\@�G�@޸R@�p�A\)A   A+�A@  A`  A\)A�Q�A���A�  A�  A�  A�  A�  A��B  B  B  B (�B'�
B/�B8  B@  BH  BP  BX  B_�
Bg�Bo�
Bx  B�
B�  B��B��B�  B�  B��B��B�  B�{B�{B�  B�  B�  B��B�  B��B�  B�(�B�{B��
B�  B�{B�{B�{B�  B��
B�  B�{B��B��B�  C 
=C  C��C  C  C	��C�C��C
=C{C
=C
=C��C��C  C��C   C!��C#��C&  C(
=C)��C+��C.  C0  C2
=C4{C6
=C8
=C:
=C;��C=�C?��CB{CD{CF{CH
=CJ  CK��CN  CP
=CR{CT  CV  CX{CZ
=C\  C^  C_��Cb  Cd  Ce��Ch
=Cj
=Ck��Cn{Cp  Cq��Cs�Cv
=Cx  Cy��C{�C}��C��C���C�  C���C���C���C���C���C�  C�  C�C�  C�C�  C�C�  C�  C�
=C�  C���C�  C���C�C�  C�  C�C�  C�  C�
=C�
=C���C�  C�  C�C���C���C���C�  C�  C�  C���C�  C�  C�C�C�  C�  C�  C�C�  C�C�
=C���C�  C���C�  C�  C�C���C�  C���C�  C�  C�  C���C�  C�  C���C�\C�  C�  C�
=C�  C���C���C���C���C�  C�  C�C�C�C�C�  C���C�C���C�  C�  C���C�C�C���C�  C�
=C�
=C���C�C�C�  C�
=C�
=C�C�  C�  C�  C���C�  C�  C�  C�
=C�
=C�  C�  C�  C�  C�  C���C�  C�  C�  C�C�C�
=C�C�  C�  C�  C���D }qD �qD� D  D� D  D� D�qD}qD��D� D�D��D�D� D  D}qD	  D	� D	�qD
z�D
�qD� D�D� D�qD��D�D}qD  D��D�qD� D  D� D�qD� D  D}qD�qD��D�D��D�D� D�qD}qD�qD��D  D� D�D� D  D� D  D}qD��D}qD�qD}qD  D��D �D � D �qD!� D!�qD"� D#  D#� D$  D$� D%�D%� D%�qD&� D'�D'��D(�D(��D)�D)��D*  D*}qD+  D+� D,  D,� D-  D-� D-�qD.}qD/  D/}qD/�qD0}qD0�qD1� D2  D2}qD2�qD3��D4D4� D4�qD5� D6  D6}qD6�qD7}qD8  D8��D9�D9� D9�qD:� D;D;�D<�D<� D=�D=� D=�qD>}qD>��D?}qD@�D@� DA  DA��DB  DB}qDC�DC��DD�DD�DE�DE� DF  DF� DGDG� DH�DH�DH�qDI}qDJ  DJ��DKDK��DL  DL� DM  DM}qDM��DN}qDO  DO� DP  DP��DQ  DQ� DR  DR� DS�DS� DS�qDT}qDT��DU� DV  DV� DW�DW}qDX  DX� DX�qDY��DZDZ��DZ�qD[� D\  D\}qD\�qD]� D^�D^��D_�D_��D`  D`� Da  Da��Db�Db� Dc�Dc��Dd  Dd}qDe�De� Df�Df�Dg�Dg� Dh  Dhz�Dh�qDi��DjDj� Dk  Dk��Dl�Dl}qDl�qDm��Dn�Dn� Do�Do� Do�qDp� Dq  Dqz�Dq��Dr}qDs  Ds��Dt  Dt}qDu  Du}qDu�qDv}qDw�Dw�Dx  Dx� Dy�Dy� Dy�qDzz�Dz��D{}qD|  D|� D|�qD}}qD}��D~��D�D��D�HD�AHD���D��HD�  D�@ D�}qD��qD��qD�@ D�~�D�� D��D�@ D�� D���D�  D�@ D�� D��HD�  D�@ D�}qD�� D���D�=qD�� D��HD���D�@ D�� D��HD���D�>�D�� D�� D�  D�@ D�}qD���D��D�B�D�� D�� D���D�>�D���D��HD�  D�@ D�~�D��qD���D�AHD�� D���D�  D�AHD�� D��qD�  D�AHD�~�D�� D�HD�>�D�� D���D���D�@ D���D��HD���D�@ D��HD�� D�  D�AHD��HD�D�HD�@ D�� D�D�  D�>�D��HD���D���D�AHD���D�� D��qD�@ D�� D��HD�HD�@ D��HD��HD�HD�AHD�~�D���D��qD�>�D�� D���D�  D�AHD��HD�D��D�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D��HD��HD�  D�>�D�~�D�� D�  D�AHD��HD���D���D�@ D�� D�D�HD�AHD�� D��HD��D�@ D�~�D���D�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�@ D�~�D���D�  D�@ D�~�D���D�HD�@ D�� D�� D�HD�AHD�� D�� D���D�AHD��HD���D�  D�B�D��HD��qD��qD�>�D�� D��HD�HD�AHD�� D��HD�HD�>�D�� D��HD�  D�>�D�~�D���D�  D�>�D�~�D�D��D�>�D�}qD���D�HD�B�D��HD���D��qD�>�D��HD��HD�HD�B�D���D�� D���D�@ D��HD�� D�  D�@ D�� D�� D�HD�>�D�~�D��HD�HD�AHD�� D�� D���D�>�D��HD�� D�  D�=qD��HD��HD���D�@ D�� D�� D�  D�@ D�� D���D�  D�>�D�~�D�� D���D�>�D�� D�� D���D�>�D���D�� D�  D�AHD�� D���D���D�@ D�� D�� D�HD�AHD D��HD�HD�AHDÁHDþ�D���D�AHD�~�Dľ�D�  D�@ Dŀ D�� D��qD�@ DƁHD��HD�  D�@ Dǀ D��HD�HD�@ DȀ D�� D���D�>�D�}qD�� D�HD�AHDʁHD�� D�  D�=qD�}qD˾�D���D�@ D̂�D�� D�  D�AHD�~�D;�D�HD�AHD΁HD�D�HD�AHDπ DϾ�D�  D�AHDЂ�D��HD���D�=qDр D��HD�  D�@ D�~�DҽqD��qD�@ DӁHD�D�  D�@ DԂ�D��HD��qD�=qD�}qD��HD��D�>�D�}qD־�D�  D�@ Dׂ�D�� D�  D�@ D�~�D��HD�HD�@ D�~�D�� D�HD�AHDځHD��HD�HD�AHDۂ�D�� D���D�@ D�~�D�� D�HD�@ D݁HD�� D���D�@ Dހ D�� D���D�>�D߁HD��HD�HD�AHD���D�� D�  D�AHD�HD�D��D�@ D� D�� D�  D�AHD�HD㾸D�  D�B�D�HD��HD�  D�>�D�~�D�� D��D�@ D� D��HD�  D�@ D�HD�� D�  D�@ D� D��HD�HD�AHD�HD龸D���D�@ D� D�� D���D�>�D�HD뾸D���D�@ D� D쾸D���D�@ D�HD��HD�  D�AHD� DD�HD�@ D�~�D��HD�  D�AHD�� D�D���D�=qD�~�D�� D�  D�AHD� D�D�  D�@ D�~�D�� D�  D�@ D� D��HD�HD�>�D�}qD��qD���D�@ D��HD���D���D�@ D�� D��HG�O�>�G�?L��?�  ?�z�?\?�(�?��H@�@�R@.{@E�@Tz�@aG�@z�H@��@�=q@�z�@�  @��@�\)@��H@��
@Ǯ@У�@ٙ�@�  @���@��@�Q�A�AffA	��A��A�\A�A=qA\)A#33A'
=A,(�A0  A3�
A6ffA;�A?\)AA�AG
=AK�AN{AR�\AVffAX��A\��Aa�AfffAi��Amp�As33AvffAy��A~�RA��A��A��A��A���A��HA�p�A��A���A�33A�A��RA���A��A��A�ffA�Q�A��HA�(�A�p�A�  A��A��HA���A�
=A���A���A�(�A�ffA�\)A���A��A�z�A��RA�G�A\A�(�AƸRA���A��A�z�A�ffA�  Aљ�A�(�A�ffA�\)A�G�A��
A�A�
=A���A��HA�(�A�A��A�=qA�33A�A�  A���A�\A��A��RA�Q�A��\A�z�A�A��B ��B�BffB�B��B��BffB
=B(�B	G�B	�B
�RB�
B��Bp�B�RB\)B(�BG�B{B�RB�
B��Bp�B=qB�B(�B��B=qB
=B�B��BBffB�B z�B!�B!�B#33B#�
B$��B%B&�RB'\)B(Q�B)p�B)B*�HB,  B,��B-p�B.�RB/\)B0  B1�B2{B2�RB3�B4��B5��B6=qB7\)B8Q�B8��B9�B;
=B;�
B<Q�B=��B>�RB?33B@(�BAG�BB=qBB�RBC�
BD��BE��BF=qBG\)BHz�BI�BIBJ�RBL  BL��BMG�BN�\BO�BPQ�BP��BQ�BS
=BT  BT��BUp�BV�\BW�BX(�BY�BZ=qB[
=B[�B\��B]B^ffB_33B`z�Bap�Ba�Bc
=Bd(�Bd��Bep�BfffBg�Bh��BiG�Bi�Bk33Bl(�Bl��Bm�Bn�HBo�Bp��BqBr=qBs33Btz�Bup�Bv{Bv�HBx(�By�By�Bz�\B{�B|��B}p�B~=qB�B�=qB��\B���B��B�{B�ffB���B�\)B��B�Q�B���B�33B�B�{B�ffB���B�p�B�  B�=qB��RB�G�B��B�=qB��\B�33B��B��B�ffB��B��B��
B�Q�B��HB�p�B��B�=qB���B�33B���B�  B���B�
=B�\)B��B�z�B��HB�33B��B�=qB���B�
=B�p�B�{B�ffB���B�33B�B�Q�B��RB�
=B���B�(�B�z�B��HB��B�  B�=qB���B�\)B�B�{B��\B�33B�B�  B�z�B�
=B���B��B�Q�B��HB�p�B��B�{B���B�G�B���B��B�z�B��B��B��
B�ffB���B�G�B��B�Q�B���B�33B���B�(�B��RB�
=B��B�{B���B���B�G�B��
B�z�B���B�33B��
B�=qB���B���B���B�{B�Q�B��HB�p�B��B�(�B���B�G�B�B�  B�ffB��HB�p�B��
B�(�B���B�33B��B��
B�ffB��HB��B��B�{B�z�B���B�G�B��
B�(�B�z�B��HB�p�B�  B�=qBģ�B�33BŮB��B�Q�B���B�p�B�B�  Bȏ\B��B�p�B�B�=qBʸRB��B�\)B�B�=qB�z�B���B�33BͮB�{B�Q�BΣ�B��Bϙ�B��
B�Q�B���B�G�Bљ�B�  B�ffB���BӅB��
B�Q�B��HB�G�BծB�{B֏\B��BׅB��
B�Q�B���B�\)BٮB�{Bڣ�B��B�p�B��
B�z�B���B�\)B�B�{Bޣ�B�33Bߙ�B��B�ffB���B�p�B�B�(�B��B�33B�B�{B�z�B���B噚B�{B�ffB���B�\)B��B�ffB���B�33B��
B�ffB���B�33B뙚B�(�B�RB�G�B��B�{B��B�G�B�B�(�B��B�G�B�B�{B��B�G�B�B�=qB���B�G�B��
B�Q�B���B��B��B�Q�B��RB��B���B�(�B���B��B��B�  B���B�33B��B�{B��\B���B�\)C 
=C G�C �\C ��C  C33C�C��C  C33Cz�CC{CG�Cz�C�RC  C\)C��C��C
=C\)C��C�HC{C\)C��C��C(�C\)C��C��C=qCp�C��C�C	G�C	�\C	C
  C
=qC
�\C
�HC(�Cp�C��C�HC(�Cz�C��C  C33Cp�C�RC{C\)C��C��C{CQ�C��C��C=qCz�C�RC��CQ�C��C�
C{CffC�RC  C33Cz�CC�CffC��C�
C(�Cz�CC  C33Cz�CC{CffC�C�HC�Cp�C�RC  C=qCp�C��C�C33C�C�RC��C33C�C�
C�C\)C��C�HC33C�\C��C
=CG�C�\C�HC33Cz�C�RC��C 33C z�C �
C!{C!Q�C!�C!C"
=C"Q�C"��C"�C#�C#Q�C#��C#�HC$33C$p�C$��C$�HC%33C%z�C%C&
=C&G�C&�C&C'�C'p�C'�RC'��C(33C(z�C(��C)�C)ffC)��C)�
C*�C*p�C*�RC+  C+G�C+�\C+C,  C,Q�C,��C-  C-33C-p�C-�RC.{C.ffC.�C.�C/(�C/p�C/��C0�C0ffC0��C0�HC1(�C1p�C1C2{C2Q�C2�\C2��C3�C3p�C3C4{C4Q�C4��C4�
C5(�C5z�C5��C6{C6Q�C6�\C6�HC7=qC7�\C7�HC8(�C8p�C8�C9  C9Q�C9��C9�C:(�C:ffC:C;{C;ffC;��C;��C<33C<�\C<�C==qC=z�C=�RC>
=C>Q�C>�C?  C?Q�C?��C?�HC@(�C@p�C@�RCA
=CAffCA�RCB
=CBQ�CB�\CB�
CC�CCp�CCCD�CDp�CD�RCE  CEG�CE��CE�CFQ�CF�\CF��CG{CGffCG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                         @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JA�
=A�1A�%A�
=A�JA�JA�%A���A�  A�A��A��mA��HA���A�ȴA�ȴA�ĜAؾwAؼjAغ^AظRAز-Aز-Aز-Aز-AخAذ!AخAة�AجAجAجAجAجAجAجAجAجAجAجAجAجAجAخAخAخAا�Aأ�Aأ�Aأ�Aء�Aؗ�Aؗ�Aؕ�A�x�A�I�A��
AЧ�A��TAϲ-A�l�A�  A͍PA���A�VA��A�ƨA�C�Aȏ\A�ƨA�%A��A��A��A�dZA¾wA��RA���A�
=A���A��A�`BA�bNA�O�A��A��hA��jA��wA�|�A��DA��HA��A�JA�ffA��wA��/A�"�A�A��-A���A��A���A��;A��hA��A���A�dZA�XA��A�%A��FA�1A���A��A���A��#A��#A���A��9A�C�A��A��A�XA��A���A���A��A�G�A��uA}��AyC�Ar�+Aq
=Ap�9ApffAm�TAi7LAg�^Ae
=Ac;dAb�uAbbA`z�A\�AZ�AX �AV�+AT�HAQVAO
=AL�!AJ�+AG��AD�AC7LABVA@�!A>�\A=VA:-A8��A7��A7A6E�A4�A3dZA1��A0(�A/A.9XA,I�A*$�A)XA)�PA)l�A(��A(A�A'�^A&�A%�mA%��A%A%;dA$~�A"��A"z�A"ĜA"�jA"I�A!�A!�hA   A��A�wA��A\)A��A�!A�AVA�;A�A�A�hA`BA�HA�A��AXA��A��AoA��A&�A��A�A�yA
��A�+A"�AjAM�A�Ap�A"�A�;A�A ��@�@�1'@�  @�Q�@��9@��/@���@�b@��@��@�@�;d@�{@�n�@���@���@�D@�ȴ@��j@�F@�l�@��
@�F@��@�O�@� �@��@��/@�z�@� �@��@�@�K�@�@�$�@��@�@�^5@�R@�@܃@�ƨ@ڇ+@�=q@ى7@�/@��@��T@�@��@� �@��
@���@ް!@��@���@�b@�{@�M�@�@�p�@�&�@��/@ܛ�@܃@�Q�@ۍP@�E�@ץ�@��@�  @ԃ@Դ9@Դ9@ԛ�@�b@��@��y@�E�@�x�@�r�@�(�@�b@ϝ�@��@Ο�@θR@Ο�@·+@�V@�V@��#@ͩ�@͉7@�7L@�1@�+@�^5@ə�@�r�@�@�v�@�$�@��@�V@���@�r�@ļj@��@Ĭ@�z�@�b@�S�@���@�n�@�5?@��^@�hs@�?}@���@��^@��@�?}@��@���@���@��H@�v�@�E�@�{@��@�hs@��9@�Z@�33@�5?@��@��7@��@���@��@�9X@��;@�S�@�C�@�33@�+@�o@��@�=q@��@��#@���@�@��-@���@���@���@��@���@�z�@�I�@��w@�S�@�
=@��H@��R@�ff@��@���@�p�@�&�@�Ĝ@�bN@�1'@�  @��F@�t�@�33@��!@��@��h@��@��/@��D@��
@���@��F@�;d@���@�ff@�=q@��@��^@�?}@��@�V@���@�Q�@�1@���@��w@�|�@�K�@�33@�
=@��H@���@�ff@�J@��@���@�/@�%@���@��j@�  @�dZ@���@���@�n�@�ff@�E�@�@�@��@��u@�A�@��@���@�33@�ȴ@��\@��T@�O�@��@���@�bN@�9X@��m@���@�t�@�K�@�33@��@��R@�^5@�=q@��@��@�@��7@�x�@�hs@���@�Q�@�(�@���@��
@�ƨ@���@�C�@��H@�V@�J@��@���@���@�?}@��9@�z�@�Z@�Q�@�A�@���@�l�@�;d@��@��y@���@��\@�ff@�J@��@�@��@�G�@��@���@�bN@�A�@��;@���@�o@��y@��@��y@���@�v�@�E�@��@�@��#@�x�@�&�@��`@��@�z�@�bN@� �@���@�K�@�33@�o@��@���@�E�@���@��7@�X@���@���@�I�@��@��;@���@�K�@�33@���@�ff@�J@��@�@��@�`B@�G�@��@���@��/@���@��@��@��;@��@�|�@�C�@��@���@�v�@��@���@�x�@�7L@���@��j@��@���@�I�@�1@�P@~�y@~�R@~�+@~@}��@}�@|I�@|1@{�F@{dZ@{C�@{o@z��@z-@y�@yhs@y�@xĜ@x�9@xbN@w��@w
=@vv�@v@u�@u?}@u�@u�@tz�@s�F@st�@s33@r��@r��@r-@q��@p��@pQ�@p  @o�P@o;d@o�@n�y@nȴ@nv�@n5?@m�@m�@l�j@lz�@l(�@kƨ@kdZ@j�@jM�@i��@i�^@i7L@hĜ@h1'@h  @g�P@f��@f�+@e�@e�h@e�@dI�@d1@c��@cS�@b�!@b~�@bM�@a��@aX@aX@`�u@_�w@_l�@_+@^��@^�@^��@]��@\��@\j@\1@[��@Z�@Zn�@Z��@Z�@Z��@Z��@Z��@Z~�@Z-@Y��@Yhs@Y�@XĜ@X  @W�@W�w@W�P@W|�@W
=@U�T@T�@Tj@TZ@Tj@S��@Sƨ@SC�@R�!@Q��@PA�@O��@O��@Ol�@OK�@O
=@N5?@M�@M`B@MO�@M/@MV@L�@LI�@K�F@Kt�@KdZ@J��@Jn�@JM�@JJ@I�^@H�9@H  @G;d@F�@FV@FV@E�T@E?}@D��@D��@D��@D9X@C��@C33@Co@C@A�@B�@A��@AG�@@r�@@1'@?�;@?��@@b@?�@?�P@?+@>@=�@=�@=V@<��@<j@<�@;�m@;�@;dZ@;"�@:��@:~�@:n�@:M�@9�@9��@9hs@9G�@97L@8�9@8b@7��@7K�@6��@6V@6@5�@5�T@5��@5�@5?}@4�@49X@3�
@3dZ@3C�@3o@2~�@2=q@2-@2-@2�@2J@2J@1��@1��@1�@1�#@1�^@1x�@0�`@0��@0�@0Q�@0 �@/�P@/+@.�y@.�R@.V@-��@-p�@-�@,�/@,�D@,j@,I�@,(�@+�@+o@*�H@*��@*�!@*n�@)��@)x�@)X@)G�@(��@(1'@'�@'
=@&�y@&�@&�+@&@%��@%`B@%?}@$��@$�D@$z�@$j@$(�@$1@#��@#ƨ@#��@#dZ@#@"�!@"�\@"M�@"J@!�@!��@!�^@!��@!hs@ ��@ ��@ bN@   @   @�@��@�P@
=@�y@ȴ@V@@�T@�-@�h@�h@`B@/@V@��@��@I�@�
@��@t�@dZ@C�@C�@33@�@��@��@M�@�@�@�^@G�@Ĝ@��@�u@�u@�u@bN@��@�w@��@\)@+@
=@�@��@v�@V@$�@@��@��@p�@�@��@��@��@��@j@�@�
@��@��@��@�@"�@�H@��@^5@J@��@�@�^@G�@%@�9@�@A�@ �@ �@�@|�@;d@�@�@�+@E�@5?@5?@@��@��@��@`B@V@�@�@�D@z�@Z@�@1@�m@�
@�F@��@�@t�@dZ@C�@"�@@
�@
�H@
��@
�\@
n�@
n�@
^5@
�@	�#@	��@	x�@	G�@	7LA�1A�bA�
=A�
=A�VA�JA�%A�
=A�
=A�%A�JA�%A�A�1A�1A�A�
=A�JA�
=A�
=A�VA�JA�
=A�VA�VA�
=A�JA�bA�%A�A���A���A���A���A���A���A�
=A�1A�  A�A�%A���A��A��A���A��A��A���A��/A��HA��yA��TA��TA��`A��yA��yA��A��A��TA��/A��HA��TA��HA��#A��A��A���A��
A��
A���A�ȴA���A���A�ȴA���A���A�ƨA�ȴA���A�ƨA�ĜA�ȴA�ȴA�ƨA�ƨA���A�ȴA�ƨA�ȴA�ƨA�A���A���A�ĜA�ĜA�ȴAؼjAؼjA�A���AؼjA���A�AؾwAؼjA���A���AؾwAغ^AؼjAؾwAؼjAؾwAؾwAظRAظRAؾwAؼjAظRAؼjAؼjAغ^AظRAؼjAظRAظRAؼjAؼjAظRAظRAغ^Aغ^Aذ!AخAش9Aش9Aذ!AخAذ!Aش9AخAذ!Aش9Aذ!Aذ!Aز-Aذ!AخAش9Aز-AخAز-Aش9AخAذ!Aش9Aذ!Aذ!Aز-Aز-AخAش9Aز-AخAش9Aز-Aذ!AخAش9AخAجAذ!Aذ!AجAذ!Aز-AجAخAز-Aذ!AجAز-Aذ!AخAز-Aز-AخAجAذ!AخAة�AخAخAة�AجAذ!AجAا�AجAخAا�Aا�AجAخAا�AجAخAة�Aا�Aة�AخAة�Aا�Aة�AخAجAا�AجAخAجAة�Aة�AخAخAة�Aا�AخAخAة�AجAخAخAة�AجAخAة�Aة�AخAخAة�AجAذ!AجAة�Aة�AخAذ!AجAة�AخAخAة�AخAخAا�AجAخAة�Aة�AجAذ!AجAة�AخAذ!AجAة�AخAذ!AجAة�AخAذ!Aة�Aة�AخAذ!Aة�Aة�AخAذ!AخAة�AجAذ!AجAا�Aة�AخAخAة�Aة�AخAذ!AجAة�AخAخAة�Aة�AخAخAا�Aة�AجAخAة�AجAخAخAة�Aة�Aذ!AخAة�AجAذ!AخAة�AجAذ!AخAة�AجAذ!AخAة�AجAخAذ!AخAة�AخAذ!Aة�AجAذ!AخAجAجAذ!Aذ!AجAة�AخAذ!AجAة�Aذ!AخAجAجAذ!Aذ!AجAا�AخAذ!Aة�Aأ�Aإ�Aة�Aا�Aء�Aإ�Aة�Aأ�Aء�Aإ�Aا�Aأ�A؟�Aأ�Aإ�Aأ�Aء�Aإ�Aا�Aأ�Aء�Aأ�Aإ�Aأ�Aء�Aإ�Aا�Aأ�Aء�Aإ�Aأ�Aء�Aأ�Aإ�Aإ�A؟�A؝�Aء�Aإ�A؛�Aؕ�Aؕ�Aء�A؛�Aؕ�Aؕ�Aؙ�Aؕ�AؓuAؙ�Aؙ�Aؕ�Aؗ�A؛�Aؙ�AؓuAؓuAؙ�Aؗ�AؓuAؓuAؗ�Aؙ�Aؕ�AؓuAؗ�Aؗ�A؋DA؇+A؉7A؋DA�~�A�t�A�v�A�t�A�hsA�bNA�^5A�\)A�O�A�C�A�G�A�G�A�E�A�?}A�A�A�C�A�7LA�(�A�A׋DA�|�A��A�(�A��A�Q�A��Aѕ�A�G�A��A���AУ�A�dZA�?}A�"�A�{A�  A���A��A��mA��/A��/A��A���A���A�ƨA�ȴA�ƨA���AϸRAϲ-Aϥ�Aϙ�AϓuAϏ\AύPAύPAσA�v�A�\)A�S�A�M�A�G�A�A�A�?}A�=qA�9XA�"�A���AζFA�v�A�I�A�{A���Aͩ�A�z�A�hsA�33A�{A���A̼jA�dZA�=qA�&�A�bA���A�~�A��A�A�A�"�A� �A��A�A���A���A��A��A��A��A��A��A��mA��mA��`A��TA��A���A���A���A�Aɲ-Aɝ�Aɉ7A�t�A�`BA�S�A�I�A�$�A��A�1A��A���A���AȮAȍPA�x�A�^5A�7LA�"�A�VA��A��/AǺ^AǬAǝ�AǋDAǁA�bNA�K�A�+A���A���Aƴ9AƝ�A�n�A�A�A��A���AŴ9Aş�Aŏ\AŅA�dZA�G�A�33A��A�%A��A��/A�ȴA�~�A�33A�  A���AìAÃA�z�A�v�A�p�A�hsA�dZA�dZA�ffA�M�A��A�1A�1A��A�ƨADA�hsA�S�A�?}A�JA��
A��A��jA�x�A�K�A�E�A�E�A�%A��uA�ffA�\)A�dZA�ffA�`BA�1'A���A���A��A��`A��#A���A���A�ƨA��RA���A��A�ZA�A�A��A�A��HA�ȴA�ȴA�ĜA��-A��+A�t�A�ffA�XA�S�A�K�A�?}A�+A�VA��A��A�hsA�{A���A�z�A�z�A�\)A�I�A�E�A�?}A�;dA�7LA�5?A��A�JA���A���A��-A���A���A���A���A���A���A���A�p�A�S�A�A�A�9XA�&�A��yA��7A�\)A�5?A��A���A��A�VA���A�K�A�bA��A��;A��wA���A��A�`BA�G�A�33A��A��mA��9A��uA�r�A�O�A��A���A�E�A�=qA�33A�%A�A��A���A��7A�VA�5?A��A�  A���A���A�t�A�33A�oA��A��;A���A���A��^A��9A���A���A�n�A���A���A�r�A��A��mA��FA���A�r�A�O�A�33A�oA��A���A���A��-A���A���A��7A�-A�bA��/A�A��!A��PA�n�A�A�A���A��\A�O�A�9XA��A��mA��wA��uA�7LA�1A��A��HA���A�ĜA��wA��RA���A���A���A��A�oA�z�A�oA��FA�ZA��#A��A�K�A�-A�"�A�{A�A���A��HA��
A�A��DA��FA��A���A�(�A�ȴA��A��A�A�A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                         A�JA�
=A�1A�%A�
=A�JA�JA�%A���A�  A�A��A��mA��HA���A�ȴA�ȴA�ĜAؾwAؼjAغ^AظRAز-Aز-Aز-Aز-AخAذ!AخAة�AجAجAجAجAجAجAجAجAجAجAجAجAجAجAخAخAخAا�Aأ�Aأ�Aأ�Aء�Aؗ�Aؗ�Aؕ�A�x�A�I�A��
AЧ�A��TAϲ-A�l�A�  A͍PA���A�VA��A�ƨA�C�Aȏ\A�ƨA�%A��A��A��A�dZA¾wA��RA���A�
=A���A��A�`BA�bNA�O�A��A��hA��jA��wA�|�A��DA��HA��A�JA�ffA��wA��/A�"�A�A��-A���A��A���A��;A��hA��A���A�dZA�XA��A�%A��FA�1A���A��A���A��#A��#A���A��9A�C�A��A��A�XA��A���A���A��A�G�A��uA}��AyC�Ar�+Aq
=Ap�9ApffAm�TAi7LAg�^Ae
=Ac;dAb�uAbbA`z�A\�AZ�AX �AV�+AT�HAQVAO
=AL�!AJ�+AG��AD�AC7LABVA@�!A>�\A=VA:-A8��A7��A7A6E�A4�A3dZA1��A0(�A/A.9XA,I�A*$�A)XA)�PA)l�A(��A(A�A'�^A&�A%�mA%��A%A%;dA$~�A"��A"z�A"ĜA"�jA"I�A!�A!�hA   A��A�wA��A\)A��A�!A�AVA�;A�A�A�hA`BA�HA�A��AXA��A��AoA��A&�A��A�A�yA
��A�+A"�AjAM�A�Ap�A"�A�;A�A ��@�@�1'@�  @�Q�@��9@��/@���@�b@��@��@�@�;d@�{@�n�@���@���@�D@�ȴ@��j@�F@�l�@��
@�F@��@�O�@� �@��@��/@�z�@� �@��@�@�K�@�@�$�@��@�@�^5@�R@�@܃@�ƨ@ڇ+@�=q@ى7@�/@��@��T@�@��@� �@��
@���@ް!@��@���@�b@�{@�M�@�@�p�@�&�@��/@ܛ�@܃@�Q�@ۍP@�E�@ץ�@��@�  @ԃ@Դ9@Դ9@ԛ�@�b@��@��y@�E�@�x�@�r�@�(�@�b@ϝ�@��@Ο�@θR@Ο�@·+@�V@�V@��#@ͩ�@͉7@�7L@�1@�+@�^5@ə�@�r�@�@�v�@�$�@��@�V@���@�r�@ļj@��@Ĭ@�z�@�b@�S�@���@�n�@�5?@��^@�hs@�?}@���@��^@��@�?}@��@���@���@��H@�v�@�E�@�{@��@�hs@��9@�Z@�33@�5?@��@��7@��@���@��@�9X@��;@�S�@�C�@�33@�+@�o@��@�=q@��@��#@���@�@��-@���@���@���@��@���@�z�@�I�@��w@�S�@�
=@��H@��R@�ff@��@���@�p�@�&�@�Ĝ@�bN@�1'@�  @��F@�t�@�33@��!@��@��h@��@��/@��D@��
@���@��F@�;d@���@�ff@�=q@��@��^@�?}@��@�V@���@�Q�@�1@���@��w@�|�@�K�@�33@�
=@��H@���@�ff@�J@��@���@�/@�%@���@��j@�  @�dZ@���@���@�n�@�ff@�E�@�@�@��@��u@�A�@��@���@�33@�ȴ@��\@��T@�O�@��@���@�bN@�9X@��m@���@�t�@�K�@�33@��@��R@�^5@�=q@��@��@�@��7@�x�@�hs@���@�Q�@�(�@���@��
@�ƨ@���@�C�@��H@�V@�J@��@���@���@�?}@��9@�z�@�Z@�Q�@�A�@���@�l�@�;d@��@��y@���@��\@�ff@�J@��@�@��@�G�@��@���@�bN@�A�@��;@���@�o@��y@��@��y@���@�v�@�E�@��@�@��#@�x�@�&�@��`@��@�z�@�bN@� �@���@�K�@�33@�o@��@���@�E�@���@��7@�X@���@���@�I�@��@��;@���@�K�@�33@���@�ff@�J@��@�@��@�`B@�G�@��@���@��/@���@��@��@��;@��@�|�@�C�@��@���@�v�@��@���@�x�@�7L@���@��j@��@���@�I�@�1@�P@~�y@~�R@~�+@~@}��@}�@|I�@|1@{�F@{dZ@{C�@{o@z��@z-@y�@yhs@y�@xĜ@x�9@xbN@w��@w
=@vv�@v@u�@u?}@u�@u�@tz�@s�F@st�@s33@r��@r��@r-@q��@p��@pQ�@p  @o�P@o;d@o�@n�y@nȴ@nv�@n5?@m�@m�@l�j@lz�@l(�@kƨ@kdZ@j�@jM�@i��@i�^@i7L@hĜ@h1'@h  @g�P@f��@f�+@e�@e�h@e�@dI�@d1@c��@cS�@b�!@b~�@bM�@a��@aX@aX@`�u@_�w@_l�@_+@^��@^�@^��@]��@\��@\j@\1@[��@Z�@Zn�@Z��@Z�@Z��@Z��@Z��@Z~�@Z-@Y��@Yhs@Y�@XĜ@X  @W�@W�w@W�P@W|�@W
=@U�T@T�@Tj@TZ@Tj@S��@Sƨ@SC�@R�!@Q��@PA�@O��@O��@Ol�@OK�@O
=@N5?@M�@M`B@MO�@M/@MV@L�@LI�@K�F@Kt�@KdZ@J��@Jn�@JM�@JJ@I�^@H�9@H  @G;d@F�@FV@FV@E�T@E?}@D��@D��@D��@D9X@C��@C33@Co@C@A�@B�@A��@AG�@@r�@@1'@?�;@?��@@b@?�@?�P@?+@>@=�@=�@=V@<��@<j@<�@;�m@;�@;dZ@;"�@:��@:~�@:n�@:M�@9�@9��@9hs@9G�@97L@8�9@8b@7��@7K�@6��@6V@6@5�@5�T@5��@5�@5?}@4�@49X@3�
@3dZ@3C�@3o@2~�@2=q@2-@2-@2�@2J@2J@1��@1��@1�@1�#@1�^@1x�@0�`@0��@0�@0Q�@0 �@/�P@/+@.�y@.�R@.V@-��@-p�@-�@,�/@,�D@,j@,I�@,(�@+�@+o@*�H@*��@*�!@*n�@)��@)x�@)X@)G�@(��@(1'@'�@'
=@&�y@&�@&�+@&@%��@%`B@%?}@$��@$�D@$z�@$j@$(�@$1@#��@#ƨ@#��@#dZ@#@"�!@"�\@"M�@"J@!�@!��@!�^@!��@!hs@ ��@ ��@ bN@   @   @�@��@�P@
=@�y@ȴ@V@@�T@�-@�h@�h@`B@/@V@��@��@I�@�
@��@t�@dZ@C�@C�@33@�@��@��@M�@�@�@�^@G�@Ĝ@��@�u@�u@�u@bN@��@�w@��@\)@+@
=@�@��@v�@V@$�@@��@��@p�@�@��@��@��@��@j@�@�
@��@��@��@�@"�@�H@��@^5@J@��@�@�^@G�@%@�9@�@A�@ �@ �@�@|�@;d@�@�@�+@E�@5?@5?@@��@��@��@`B@V@�@�@�D@z�@Z@�@1@�m@�
@�F@��@�@t�@dZ@C�@"�@@
�@
�H@
��@
�\@
n�@
n�@
^5@
�@	�#@	��@	x�@	G�G�O�A�1A�bA�
=A�
=A�VA�JA�%A�
=A�
=A�%A�JA�%A�A�1A�1A�A�
=A�JA�
=A�
=A�VA�JA�
=A�VA�VA�
=A�JA�bA�%A�A���A���A���A���A���A���A�
=A�1A�  A�A�%A���A��A��A���A��A��A���A��/A��HA��yA��TA��TA��`A��yA��yA��A��A��TA��/A��HA��TA��HA��#A��A��A���A��
A��
A���A�ȴA���A���A�ȴA���A���A�ƨA�ȴA���A�ƨA�ĜA�ȴA�ȴA�ƨA�ƨA���A�ȴA�ƨA�ȴA�ƨA�A���A���A�ĜA�ĜA�ȴAؼjAؼjA�A���AؼjA���A�AؾwAؼjA���A���AؾwAغ^AؼjAؾwAؼjAؾwAؾwAظRAظRAؾwAؼjAظRAؼjAؼjAغ^AظRAؼjAظRAظRAؼjAؼjAظRAظRAغ^Aغ^Aذ!AخAش9Aش9Aذ!AخAذ!Aش9AخAذ!Aش9Aذ!Aذ!Aز-Aذ!AخAش9Aز-AخAز-Aش9AخAذ!Aش9Aذ!Aذ!Aز-Aز-AخAش9Aز-AخAش9Aز-Aذ!AخAش9AخAجAذ!Aذ!AجAذ!Aز-AجAخAز-Aذ!AجAز-Aذ!AخAز-Aز-AخAجAذ!AخAة�AخAخAة�AجAذ!AجAا�AجAخAا�Aا�AجAخAا�AجAخAة�Aا�Aة�AخAة�Aا�Aة�AخAجAا�AجAخAجAة�Aة�AخAخAة�Aا�AخAخAة�AجAخAخAة�AجAخAة�Aة�AخAخAة�AجAذ!AجAة�Aة�AخAذ!AجAة�AخAخAة�AخAخAا�AجAخAة�Aة�AجAذ!AجAة�AخAذ!AجAة�AخAذ!AجAة�AخAذ!Aة�Aة�AخAذ!Aة�Aة�AخAذ!AخAة�AجAذ!AجAا�Aة�AخAخAة�Aة�AخAذ!AجAة�AخAخAة�Aة�AخAخAا�Aة�AجAخAة�AجAخAخAة�Aة�Aذ!AخAة�AجAذ!AخAة�AجAذ!AخAة�AجAذ!AخAة�AجAخAذ!AخAة�AخAذ!Aة�AجAذ!AخAجAجAذ!Aذ!AجAة�AخAذ!AجAة�Aذ!AخAجAجAذ!Aذ!AجAا�AخAذ!Aة�Aأ�Aإ�Aة�Aا�Aء�Aإ�Aة�Aأ�Aء�Aإ�Aا�Aأ�A؟�Aأ�Aإ�Aأ�Aء�Aإ�Aا�Aأ�Aء�Aأ�Aإ�Aأ�Aء�Aإ�Aا�Aأ�Aء�Aإ�Aأ�Aء�Aأ�Aإ�Aإ�A؟�A؝�Aء�Aإ�A؛�Aؕ�Aؕ�Aء�A؛�Aؕ�Aؕ�Aؙ�Aؕ�AؓuAؙ�Aؙ�Aؕ�Aؗ�A؛�Aؙ�AؓuAؓuAؙ�Aؗ�AؓuAؓuAؗ�Aؙ�Aؕ�AؓuAؗ�Aؗ�A؋DA؇+A؉7A؋DA�~�A�t�A�v�A�t�A�hsA�bNA�^5A�\)A�O�A�C�A�G�A�G�A�E�A�?}A�A�A�C�A�7LA�(�A�A׋DA�|�A��A�(�A��A�Q�A��Aѕ�A�G�A��A���AУ�A�dZA�?}A�"�A�{A�  A���A��A��mA��/A��/A��A���A���A�ƨA�ȴA�ƨA���AϸRAϲ-Aϥ�Aϙ�AϓuAϏ\AύPAύPAσA�v�A�\)A�S�A�M�A�G�A�A�A�?}A�=qA�9XA�"�A���AζFA�v�A�I�A�{A���Aͩ�A�z�A�hsA�33A�{A���A̼jA�dZA�=qA�&�A�bA���A�~�A��A�A�A�"�A� �A��A�A���A���A��A��A��A��A��A��A��mA��mA��`A��TA��A���A���A���A�Aɲ-Aɝ�Aɉ7A�t�A�`BA�S�A�I�A�$�A��A�1A��A���A���AȮAȍPA�x�A�^5A�7LA�"�A�VA��A��/AǺ^AǬAǝ�AǋDAǁA�bNA�K�A�+A���A���Aƴ9AƝ�A�n�A�A�A��A���AŴ9Aş�Aŏ\AŅA�dZA�G�A�33A��A�%A��A��/A�ȴA�~�A�33A�  A���AìAÃA�z�A�v�A�p�A�hsA�dZA�dZA�ffA�M�A��A�1A�1A��A�ƨADA�hsA�S�A�?}A�JA��
A��A��jA�x�A�K�A�E�A�E�A�%A��uA�ffA�\)A�dZA�ffA�`BA�1'A���A���A��A��`A��#A���A���A�ƨA��RA���A��A�ZA�A�A��A�A��HA�ȴA�ȴA�ĜA��-A��+A�t�A�ffA�XA�S�A�K�A�?}A�+A�VA��A��A�hsA�{A���A�z�A�z�A�\)A�I�A�E�A�?}A�;dA�7LA�5?A��A�JA���A���A��-A���A���A���A���A���A���A���A�p�A�S�A�A�A�9XA�&�A��yA��7A�\)A�5?A��A���A��A�VA���A�K�A�bA��A��;A��wA���A��A�`BA�G�A�33A��A��mA��9A��uA�r�A�O�A��A���A�E�A�=qA�33A�%A�A��A���A��7A�VA�5?A��A�  A���A���A�t�A�33A�oA��A��;A���A���A��^A��9A���A���A�n�A���A���A�r�A��A��mA��FA���A�r�A�O�A�33A�oA��A���A���A��-A���A���A��7A�-A�bA��/A�A��!A��PA�n�A�A�A���A��\A�O�A�9XA��A��mA��wA��uA�7LA�1A��A��HA���A�ĜA��wA��RA���A���A���A��A�oA�z�A�oA��FA�ZA��#A��A�K�A�-A�"�A�{A�A���A��HA��
A�A��DA��FA��A���A�(�A�ȴA��A��A�A�A�-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�BSB�B�B�B�BSBYB�B�B�BB�B�B�BBBMBBB�BB�B�B�B�BB�B�BBBBBBMB�BBMBMB�BMBMBMBMB�BMBMB�B{BGBGB�B�BB �B
��B
��B
�B
�B
�B
��B
�B
��B
�RB
��B
�<B
�jB
ΥB
רB
�vB
��B�B	�B�B	B#:B6BLdB^5B`B_;Bl"Bx�B�B�aBBʌB�BoB�B 'B)�B'�B'RB$@B%�B$@B%�B(�B)*B#B%FB�B�B�B�B��B�>B�QB�B��B�UB��B��B�B��BsBa�BN�B5tBVB�B�B
�B
�B
�B
�9B
�}B
��B
�oB
u%B
p;B
>BB
-B
'RB
!�B
�B	��B	�8B	�8B	ݘB	��B	�NB	�0B	�B	�B	�FB	�B	��B	��B	o�B	k�B	ZB	V�B	C-B	7�B	4nB	/�B	&�B	!�B	YB	B	JB	
=B	�B	~B		7B	4B	JB	�B	�B	qB	"�B	5B	FtB	PB	_�B	iyB	}�B	��B	��B	��B	�=B	�OB	��B	��B	��B	�B	ŢB	ȀB	�B	̘B	ӏB	�BB	��B	�/B	��B	�B	�&B	�B	�"B	�"B	�mB	��B	ݘB	��B	�2B	�B	ƨB	��B	�UB	��B	��B	�@B	��B	|�B	y>B	v�B	k�B	XB	NB	C�B	B[B	A B	N�B	<jB	<B	;0B	FB	A�B	:�B	C�B	I�B	RTB	VmB	W�B	VB	T�B	^jB	c�B	Z�B	WsB	]�B	e,B	r�B	sMB	sMB	��B	��B	� B	�=B	��B	��B	�YB	�{B	�~B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�tB	��B	��B	��B	��B	�$B	��B	�kB	�B	��B	��B	��B	��B	�^B	��B	�B	�B	�RB	��B	خB	��B	��B	�vB	ߤB	ޞB	�dB	ܒB	�#B	خB	�&B	��B	�tB	�'B	��B	��B	�pB	ϫB	�,B	��B	خB	՛B	�9B	ӏB	��B	�2B	��B	��B	�sB	�pB	�B	��B	�B	��B	�)B	��B	��B	�oB	�iB	�B	�B	�B	��B	�KB	�B	�vB	�vB	�B	�|B	�ZB	�>B	��B	��B	��B	��B	�cB	��B	��B	�"B	��B	��B	�(B
�B

rB
DB
DB
B

�B
�B
�B
�B
�B
�B
�B
�B
.B
�B
�B
�B
B
$B
$B
�B
_B
1B
B
kB
7B
�B
=B
�B
xB
B
�B
qB
qB
qB
�B
�B
�B
B
�B
�B
�B
~B
�B
�B
B
�B
�B
�B
B
�B
�B
B
�B
 \B
 \B
 �B
!-B
!-B
!�B
!bB
#B
$tB
$�B
%�B
%B
$�B
#nB
'�B
&LB
%FB
%�B
&B
&�B
'�B
(XB
(XB
(�B
)_B
)�B
)�B
*eB
+6B
+6B
,B
,qB
-CB
-�B
-�B
-�B
.�B
.IB
.}B
/�B
/OB
/OB
/�B
0UB
1'B
1[B
1�B
1�B
1�B
2-B
2aB
2aB
49B
3hB
49B
49B
5?B
6zB
6zB
7�B
8�B
8�B
8�B
:*B
:�B
;�B
<�B
=B
=<B
=<B
=qB
=�B
>BB
>�B
>�B
?B
?}B
?}B
?�B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B[B
B�B
C�B
CaB
C�B
C�B
C�B
DgB
E�B
EmB
E�B
EmB
E9B
FB
F�B
F�B
GB
GB
GEB
GEB
G�B
HKB
G�B
HB
I�B
I�B
I�B
I�B
J#B
I�B
J�B
J�B
K�B
K�B
M�B
OB
OvB
O�B
OvB
O�B
PB
P}B
P�B
QNB
QNB
Q�B
Q�B
Q�B
R B
R�B
R�B
S&B
S&B
R�B
S&B
T�B
T�B
T�B
U2B
V9B
V�B
W?B
W?B
W�B
XB
XB
W�B
WsB
X�B
Y�B
YB
ZB
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
\�B
\�B
\�B
\�B
\�B
]�B
]dB
]/B
]�B
]�B
^B
^B
^B
^5B
^5B
^5B
_;B
_;B
`B
`vB
`B
`B
`vB
`BB
`vB
aB
`�B
`�B
aHB
`�B
aB
aHB
a�B
a�B
b�B
b�B
b�B
b�B
b�B
cTB
c�B
c�B
d�B
d�B
d�B
d�B
dZB
e�B
e�B
e�B
ffB
f�B
ffB
f�B
gmB
g�B
h>B
h�B
h�B
h�B
h�B
h�B
iB
i�B
i�B
i�B
jB
j�B
j�B
j�B
kB
j�B
k�B
l"B
l"B
lWB
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
m�B
m�B
n/B
ncB
n/B
ncB
o B
o B
p;B
o�B
n�B
ncB
n�B
n�B
o�B
poB
qvB
qAB
p�B
qB
qAB
q�B
r|B
tTB
v`B
v�B
v�B
v�B
w2B
w2B
wfB
w�B
wfB
w2B
w2B
w�B
x�B
xlB
x8B
x�B
w�B
v�B
v�B
v�B
xlB
x�B
xB
w�B
wfB
w�B
v�B
v�B
v�B
v�B
v�B
wfB
xlB
x�B
y>B
y�B
y�B
y�B
zxB
{JB
{�B
|PB
|�B
~�B
~(B
~�B
~�B
.B
�B
.B
~�B
~�B
cB
cB
�4B
��B
�B
��B
��B
��B
�B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�B
��B
�1B
��B
�B
�B
�=B
�=B
�	B
�=B
�=B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�JB
��B
��B
��B
��B
�~B
��B
�B
�B
��B
��B
��B
�"B
�"B
��B
��B
��B
�VB
��B
��B
��B
��B
��B
��B
�oB
�:B
�oB
�B
�oB
�oB
�:B
�oB
�oB
�oB
�:B
�oB
�B
�@B
��B
�uB
��B
��B
��B
��B
�B
�MB
��B
��B
��B
��B
��B
�$B
��B
�$B
�YB
��B
��B
��B
��B
��B
��B
�kB
��B
�kB
��B
��B
�	B
��B
��B
��B
�CB
�xB
�xB
��B
�B
��B
�~B
�~B
�B
�IB
��B
��B
��B
�B
�B
�OB
��B
��B
�VB
��B
��B
��B
�'B
�'B
�'B
�'B
�'B
�'B
�\B
��B
��B
��B
��B
��B
��B
�bB
�bB
�4B
��B
�hB
��B
�B
��B
�:B
�:B
�nB
�nB
�nB
��B
�tB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�zB
�zB
��B
�B
��B
�B
�B
�RB
�B
�B
��B
��B
�$B
�XB
�XB
��B
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
��B
�6B
�6B
�6B
�6B
��B
��B
��B
�B
�B
�B
�B
�qB
��B
��B
�wB
�wB
�wB
�wB
��B
�B
�}B
��B
��B
�OB
�B
�B
�B
��B
�OB
��B
��B
�!B
�UB
��B
��B
��B
��B
��B
�'B
��B
��B
��B
��B
��B
��B
��B
�3B
�3B
�3B
�hB
��B
��B
��B
��B
��B
�9B
�9B
�9B
�9B
�9B
�nB
�B
�B
�?B
�?B
�tB
��B
�B
��B
��B
��B_B�B%B%BBSB�BMB�B�B�B�B�B�BB�BBMBSB%BMB�B�BSBMB�B�B�B�B�B�BBSB�BB�B�B�B�BSBB�B�B�B �B�B�B�BxBMB�B�BYBMB{B�B�BuB�B�B�BGB�B+BB�B�B�B�B%BMBB�B�BB{B�B�BAB�B�B�B{B�B�B�BMBB�BGB�B�BGBSBB�BBB{BGBSB{B�B�BB{B�B�BSBGBGB�BGB�B�BMB�B{B�B{B�B�B�BB�B�BGBGB�B�B�B{B%B�B�B�B�B�B�BGB�B�B�BBMBGB�B�B�B�B�BGBGB�BMBuBB�BB�B�BB�B�BuBGB�B�BBMBBGBGBB{B�B�BB�BB�B�BB�BBB�BB�B{B�B�B�BSB�B�B�BBGBB�B�B{BB�BBB�B�BMB�BMBB�BGB�BB�B�B�BB�BGBGB�BB{BGB�B�BGB�BSBBGB�B�BB�BBB�BMBSBSB�BBMBSB�B�BSB�B{BSB�B�B�BB�BBMBSBMBBMBSB�BGB�B�BBGBBSB�BGB�B�BBGB�BSB�BGB�B�B�B{BB�BB�B�B�BB{BGBBSBGB�BSB�B�BGBSBMBGB�BB�BB�BSB�BGB�BSB�BGB�B�B�B�B�BSB�B{BB�BBGBGB�B�B{B�BSB�BB�B�B�BGBB�B�B�B�B{BSB�BGB�BB{BuBB�BBABGB�BBuB�B�BBAB�B�B{BuB�B�B�BAB�BMB�BuB{B�B�BuB�BB�B�B�B�BuBuBBB;BBB�B�B
�.BoB{B�B;B�B�B�B;B{B�BBoBB�B �B �BuBB �B
��BoB�B
��B
��B
��B iB
��B
��BoB
�(B
��B
��B
��B
��B
��B
�2B
�	B
��B
�fB
��B
��B
��B
��B
�B
�B
�B
�iB
��B	lB
��B
��B
�>B
ϫB
�B
�)B
�B
��B
��B
��B
��B
��B
�	B
��B
�	B
��B
��B
�B
��B
��B
��B
��B
�7B
��B
��B
�_B
�7B
�B
�7B
��B
�eB
��B
�7B
�B
�+B
��B
�B
�~B
��B
�+B
�1B
�B
�1B
��B
�$B
�kB
�xB
��B
��B
�@B
��B
��B
�RB
�zB
�B
�6B
��B
�*B
��B
�?B
�tB
��B
��B
��B
�B
��B
�B
͟B
��B
��B
�B
�6B
�dB
�jB
��B
��B
��B
�B
�jB
�<B
�jB
�0B
�B
�B
�pB
�6B
�dB
͟B
�B
�NB
ѷB
�B
�mB
�EB
��B
�5B
�B
�yB
چB
چB
��B
�pB
�vB
�B
�B
��B
�8B
�B
�B
��B
�B
�B
��B
��B
�%B
�lB
�B�B�B�B%B
	B"B	�B�B�B�B�B�BSBDB+B�B�B1BB
=B�B�B�B1B~B"�B 'B!�B \B!-B!�B"�B"hB �B)�B/�B-wB-B6FB8RB7�B<6B=<B>�BHKBB'B@�BMjBW?BY�BWsBVBk�Bb�B`�B[WB[�B\�B^5BffBg�B]/B\�B]�B^B\)BZQBZ�B[�B]�Bc�Bh�Bh�Bh�Bh�Bn�Bo5Bm�Bm�BqABu�Bu�BxBy�By>By�B|�BcB�B��B��B��B�mB��B�#B�B�?BB�B� B��B�B��B��B�6B�HB�B��B��B��B�zBȀB��BȴB��BѷB�NB�TB�TB�B��B��B�B�B��B��B  BVB#�B�BOB~BxB�B!�BOB!�B�B�BIB!�B�B�B�B�B"4B+B#�B�B�B(XB(�BD�B/�B*0B(XB'�B%zB#B)�B)*B(�B.�B%�B'�B$�B%B&LB#�B"�B�B �B%FB+�B"hB%FB(�B$�B%B#B$�B&LB$�B%�B%B%zB$@B$tB!�B �B#�B*eB"hB)�B$tB&�B"�B%�B%�B,qB:*B%FB%zB$tB,qB$�B/OB,�B'�B$B#�B%B#�B"4B \B!�BB~BOB2�B-�B'RB$B"hB*0B	B�B�B B�BoB�B�BB�BbB!-BhBB 'B��B�PB�B�cB�8G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021102603212320211026032123IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021110507011020211105070110QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021110507011020211105070110QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365420220126093654IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295620220204232956IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295620220204232956IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295620220204232956IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                