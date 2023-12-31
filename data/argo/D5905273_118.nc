CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-03-25T05:45:58Z creation; 2023-05-01T21:35:41Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20210325054558  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               v   vAA  AOAO7314_008642_118                 7314_008642_118                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�h�\(�@�h�\(�11  @�h͞��@�h͞��@1�G�z�@1�G�z��b�ݬ����b�ݬ���11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�?�@=p�@}p�@�  @�G�@�G�A ��A  A   A+�A?\)A`��A�  A�Q�A�Q�A�Q�A�Q�A�Q�A�  A�  B   B�
B  B  B�
B'�
B0(�B8  B@  BH  BP  BX(�B`(�BhQ�BpQ�Bx(�B�(�B�  B��B�{B�{B�{B�  B��B��B��B��B�  B�{B��B�{B�(�B�  B�{B��B��
B�  B�  B�{B��B��
B�  B��B��
B��B�  B�  B�{C 
=C
=C  C
=C  C

=C  C  C
=C
=C
=C  C
=C
=C��C  C   C"  C$  C&  C(  C*  C+��C.{C/C2  C4
=C6
=C8{C:
=C<  C=��C@  CB
=CD
=CE��CH
=CJ
=CL  CM��CO��CR
=CT
=CV
=CX  CZ  C\  C]��C_��Cb  Cd  Cf  Ch
=Cj  Ck��Cm��Co��Cq��Cs�Cu��Cx  Cz
=C|
=C~  C��C�  C�C�  C���C�  C�  C���C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C�  C�C�  C�  C�  C�C�
=C�
=C�
=C�C���C�  C�C�C�C�C�C�
=C�C���C���C�  C�  C�
=C�C�C�C�C�C���C��C�  C�
=C�
=C�
=C�  C���C���C�  C�C���C���C���C�  C�C���C���C���C�  C���C���C���C���C�  C�  C�C�C�
=C�
=C�  C�  C�C�  C�  C�C���C�  C�  C���C���C�  C���C�  C�C���C���C�  C�
=C�C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�  C���C���C���C���C���C���C�  C�C���C���C�  C�
=C�C�  C�C�C�  C���C���D ��D  D� D�D��D�D��D�D��D�qD� D  D� D�qD��DD��D	  D	��D
D
� D
�qD� D�D}qD  D��D��D}qD  Dz�D�qD� D�qDz�D  D��D  D� D�D� D  D� D  D��D�D��D  D� D  D� D�D��D  D��D�D}qD�qD}qD�qD}qD�qD� D�qD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&��D'  D'}qD'�qD(� D)  D)� D)�qD*� D+�D+� D+�qD,}qD,�qD-}qD-�qD.}qD/  D/� D0�D0� D0�qD1� D2�D2� D2�qD3� D4  D4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD9  D9� D:  D:� D;�D;� D<  D<� D<�qD=}qD>  D>� D?  D?}qD?�qD@� D@�qDA� DB�DB� DC  DC��DD  DD��DE�DE��DF�DF� DG�DG��DH�DH��DI  DI}qDI��DJ}qDK  DK}qDL  DL� DL�qDM��DM�qDN� DO�DO� DP  DP}qDQ  DQ��DQ�qDR}qDR�qDS}qDS�qDT� DU  DU��DV  DV� DW  DW}qDX  DX� DY  DY}qDY�qDZ� D[�D[� D\  D\� D]�D]��D^  D^}qD_  D_� D_�qD`� Da  Da� Db  Db� Dc�Dc� Dc�qDd� De�De��DfDf��Df�qDg� Dh  Dh}qDi  Di��Dj  Dj}qDk�Dk��Dl  Dl� Dm  Dm��Dn  Dn}qDo  Do��Dp  Dp� Dq  Dq� Dr  Dr��Ds�Ds� Ds�qDt}qDu  Du� Dv  Dv��Dw  Dw� Dw�qDxz�Dy  Dy}qDy�qDz� D{�D{��D|  D|� D|�qD}� D~  D~}qD~�qD}qD��D�@ D�� D�� D���D�>�D�~�D���D���D�@ D�� D�� D�  D�AHD��HD��HD�HD�@ D�~�D��qD���D�@ D�~�D���D�  D�>�D�}qD�� D�  D�>�D�� D�� D�  D�AHD�� D��HD�HD�@ D��HD�D��D�>�D�~�D��HD�HD�@ D�� D�� D�  D�@ D��HD��HD��D�@ D�~�D���D�  D�AHD�� D��qD���D�@ D�� D��HD���D�=qD�� D�� D���D�AHD��HD��HD�HD�@ D�� D�� D���D�@ D��HD�� D���D�>�D�� D��HD�  D�@ D�~�D���D���D�@ D�� D��HD�HD�B�D��HD���D�  D�AHD���D��HD�  D�@ D�~�D��HD�HD�@ D�� D�� D�HD�>�D�� D��HD�HD�B�D�� D���D�  D�B�D��HD�� D���D�@ D��HD�� D���D�@ D��HD�� D�HD�AHD��HD��HD���D�@ D�� D��qD���D�@ D��HD��HD�  D�>�D�}qD�� D�  D�>�D�~�D��HD�  D�>�D��HD��HD�HD�B�D��HD�� D��D�AHD�� D�D�HD�@ D��HD�� D���D�>�D�� D��HD��D�@ D�~�D���D�  D�B�D�� D���D���D�@ D�~�D���D���D�@ D��HD��HD���D�@ D��HD���D�  D�>�D�}qD��qD���D�@ D��HD�� D���D�AHD�� D�� D�HD�@ D�� D�D�HD�@ D�~�D��qD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�� D�  D�B�D��HD�� D�  D�>�D��HD��HD�  D�@ D�~�D���D�HD�B�D��HD�� D�  D�@ D��HD�D�HD�AHD�� D���D�  D�>�D�~�D�� D�  D�>�D�� D�� D���D�@ D��HD�� D���D�>�D�}qD���D�  D�@ D�� D�� D�  D�@ DHD��HD�HD�AHDÀ D�� D�  D�@ DĀ D�� D�HD�@ D�~�Dž�D��qD�@ DƁHD��HD�  D�AHDǀ DǾ�D�  D�@ D�~�D��HD�  D�@ Dɀ D�� D�HD�@ D�~�D�� D�  D�>�DˁHD��HD���D�@ D̀ D̽qD�  D�B�D̀ D;�D�  D�AHD΀ Dξ�D���D�>�Dπ D�� D�  D�@ D�~�D��HD�HD�@ D�~�D�� D�HD�AHDҀ D�� D�  D�@ D�~�D��HD�HD�>�D�}qDԾ�D�  D�>�D�~�D�� D�HD�AHDցHD��HD�  D�AHDׁHD�� D���D�>�D؀ D��HD�HD�AHDـ D�� D�HD�@ D�~�D�� D�  D�>�Dۀ D�� D�  D�AHD܀ D�� D���D�>�D݀ D�� D�  D�@ D�~�D޾�D�  D�>�D�}qD߾�D�  D�@ D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D�� D�  D�@ D�HD��HD�HD�AHD�HD�� D�  D�>�D�~�D徸D�  D�>�D�~�D�� D���D�>�D� D��HD�HD�>�D�}qD�� D�HD�>�D�~�D�� D�  D�@ D� D�� D�HD�AHD�~�D�� D�  D�@ D�HD�� D�  D�AHD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�qD�  D�AHD��HD�� D���D�=qD�~�D��HD�HD�AHD�HD�� D�  D�@ D�~�D�D��qD�=qD�HD�� D���D�>�D�~�D���D�  D�@ D��HD��HD�  D�>�D�� D��HD�  D�>�D�� D��HD���D�=qD�}qD���D��D�4{?�?#�
?�  ?�z�?\?�
=@   @\)@#�
@333@G�@W
=@h��@}p�@��@���@���@��\@��@�z�@�(�@�ff@�\)@�Q�@�G�@�@�z�@�(�A33A
=A�A  Az�A��Ap�A!�A'
=A*�HA0  A4z�A9��A>{AB�\AG
=AL(�AP��ATz�AY��A^{Ac33Ag
=Al(�Ap  Au�Ax��A}p�A�G�A��A�{A���A��HA�p�A��A��A�z�A��RA���A�33A�A�  A��A�z�A��RA���A��A�A�Q�A��HA��A��A��A���A�
=A���A�z�AƸRA���A˅A�{A�Q�Aҏ\A��A׮A�=qA�z�A޸RA�G�A��
A�{A��A�33A�p�A�  A�\A��A��A��A�z�A��RB z�BB�HB(�BG�B�\B�B��B
=qB�B��B{B\)B��B�B33BQ�B��B�HB(�BG�BffB�B��B{B33B z�B!��B"�HB$(�B%G�B&�\B'�
B)�B*ffB+�B,��B.=qB/�B0��B1�B333B4Q�B5��B6�RB8  B9�B:ffB;�B<��B>{B?\)B@z�BA�BC33BDQ�BEBF�RBH(�BIG�BJ�\BK�BL��BN{BO\)BP��BQ�BS33BTQ�BUBV�HBXQ�BYp�BZ�\B[�
B]�B^=qB_�B`��Bb{Bc33Bdz�BeBg
=BhQ�Bip�Bj�RBl  BmG�Bn�\Bo�Bp��Br{Bs33Btz�Bu��Bv�HBx  Byp�Bz�\B{�
B}G�B~�\B�B�ffB�
=B���B�=qB���B��B�(�B���B�\)B�  B���B�33B�B�ffB�
=B���B�(�B��RB�p�B�  B��RB�\)B��B��\B��B��B�ffB�
=B���B�ffB���B���B�=qB��HB��B�(�B���B�p�B�(�B���B��B�=qB���B�p�B�{B��RB�\)B�  B��RB�\)B�{B��RB�p�B�{B���B�p�B�{B���B�\)B�  B���B�\)B�{B��RB�p�B�(�B���B�\)B�  B���B�G�B��B��\B�G�B�  B���B�G�B��B�z�B��B�B�ffB��B�B�z�B��B�B�ffB���B���B�=qB��HB���B�Q�B�
=B��B�Q�B�
=B��B�ffB�
=B��B�ffB��B��
Bģ�B�\)B�  Bƣ�B�G�B��Bȏ\B�\)B�  BʸRB�\)B�  Ḅ�B�\)B�  B���BυB�(�B���B�p�B�{B���Bә�B�ffB��B�B�z�B�G�B�{B���Bٙ�B�Q�B���BۮB�z�B�G�B�  B޸RB߅B�(�B���B�B�\B�G�B�{B���B�p�B�=qB�
=B�B�\B�G�B�  B�RB�p�B�(�B�
=B��
B�z�B�33B��B��B�p�B�=qB���B�B�ffB��B��
B���B�p�B�=qB���B��B�ffB�G�B�{B��HB��B�ffB��B��C \)C �RC{Cp�C�
C=qC��C��CQ�C�C  C\)C�C
=CG�C�CC
=CQ�C�\C�RC�C{CG�Cz�C�C��C��C�CQ�C�C�C�
C��C	33C	\)C	��C	C	�HC

=C
=qC
ffC
��C
C
�C
=C=qCp�C��CC�C�CQ�Cp�C�\CC��C(�CG�Cp�C��C��C  C(�CG�Cp�C��C�
C  C(�CG�CffC��C��C  C�C=qCp�C��C��C�C{C=qCp�C��CC�C{C=qCz�C�C�
C��C�C\)C�\C�RC�C
=C=qCp�C��C��C��C(�CffC�\C�RC�HC�CQ�C�C��C��C
=C=qCp�C��C��C�C�C\)C�\C�RC�C{CQ�C�C�C�
C  C=qCp�C��CC��C33C\)C�C�C�HC�CG�CffC��C��C  C(�CQ�C�CC��C{C=qCz�C�C�
C  C(�C\)C��CC�C {C Q�C �C �C �
C!  C!33C!ffC!��C!��C!��C"{C"Q�C"�C"�C"�
C#
=C#G�C#z�C#��C#C$  C$33C$\)C$�\C$�RC$�C%(�C%Q�C%z�C%�RC%�C&�C&G�C&z�C&�RC&�C'{C'Q�C'�C'�C'�HC({C(Q�C(z�C(��C(�C){C)=qC)ffC)��C)��C)��C*33C*ffC*�C*�C*�HC+  C+�C+Q�C+z�C+�\C+�C+�
C+��C,
=C,33C,\)C,z�C,��C,�
C,��C-(�C-Q�C-p�C-�\C-C-�HC.  C.{C.{C.(�C.�C.(�C.�C.33C.G�C.Q�C.ffC.z�C.��C.��C.�HC/
=C/{C/=qC/=qC/\)C/p�C/z�C/�\C/�C/��C033C0ffC0��C0�
C1
=C1=qC1�C1�RC1�HC2�C2\)C2�\C2C3  C3=qC3ffC3��C3�HC4{C4=qC4�C4C4�C5�C5Q�C5��C5��C5��C6(�C6ffC6��C6�
C7  C7=qC7p�C7��C7�
C8�C8=qC8p�C8�C8�HC9{C9Q�C9�C9�C9�HC:�C:Q�C:z�C:�RC:�C;{C;G�C;�C;�C;�
C<�C<Q�C<�C<�RC<��C=�C=Q�C=��C=�
C>  C>=qC>z�C>�C>�HC?(�C?\)C?�\C?��C@{C@G�C@z�C@�RC@��CA(�CAffCA��CA�HCB{CBG�CB��CBCC  CCG�CC�CC�RCC��CD=qCDffCD�CD�CE(�CEffCE��CE�
CF{CFQ�CF��CFCG  CGG�CG�CG�RCH  CH33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                       ?k�?�@=p�@}p�@�  @�G�@�G�A ��A  A   A+�A?\)A`��A�  A�Q�A�Q�A�Q�A�Q�A�Q�A�  A�  B   B�
B  B  B�
B'�
B0(�B8  B@  BH  BP  BX(�B`(�BhQ�BpQ�Bx(�B�(�B�  B��B�{B�{B�{B�  B��B��B��B��B�  B�{B��B�{B�(�B�  B�{B��B��
B�  B�  B�{B��B��
B�  B��B��
B��B�  B�  B�{C 
=C
=C  C
=C  C

=C  C  C
=C
=C
=C  C
=C
=C��C  C   C"  C$  C&  C(  C*  C+��C.{C/C2  C4
=C6
=C8{C:
=C<  C=��C@  CB
=CD
=CE��CH
=CJ
=CL  CM��CO��CR
=CT
=CV
=CX  CZ  C\  C]��C_��Cb  Cd  Cf  Ch
=Cj  Ck��Cm��Co��Cq��Cs�Cu��Cx  Cz
=C|
=C~  C��C�  C�C�  C���C�  C�  C���C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C�  C�C�  C�  C�  C�C�
=C�
=C�
=C�C���C�  C�C�C�C�C�C�
=C�C���C���C�  C�  C�
=C�C�C�C�C�C���C��C�  C�
=C�
=C�
=C�  C���C���C�  C�C���C���C���C�  C�C���C���C���C�  C���C���C���C���C�  C�  C�C�C�
=C�
=C�  C�  C�C�  C�  C�C���C�  C�  C���C���C�  C���C�  C�C���C���C�  C�
=C�C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�  C���C���C���C���C���C���C�  C�C���C���C�  C�
=C�C�  C�C�C�  C���C���D ��D  D� D�D��D�D��D�D��D�qD� D  D� D�qD��DD��D	  D	��D
D
� D
�qD� D�D}qD  D��D��D}qD  Dz�D�qD� D�qDz�D  D��D  D� D�D� D  D� D  D��D�D��D  D� D  D� D�D��D  D��D�D}qD�qD}qD�qD}qD�qD� D�qD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&��D'  D'}qD'�qD(� D)  D)� D)�qD*� D+�D+� D+�qD,}qD,�qD-}qD-�qD.}qD/  D/� D0�D0� D0�qD1� D2�D2� D2�qD3� D4  D4}qD4�qD5}qD5�qD6}qD6�qD7}qD7�qD8}qD9  D9� D:  D:� D;�D;� D<  D<� D<�qD=}qD>  D>� D?  D?}qD?�qD@� D@�qDA� DB�DB� DC  DC��DD  DD��DE�DE��DF�DF� DG�DG��DH�DH��DI  DI}qDI��DJ}qDK  DK}qDL  DL� DL�qDM��DM�qDN� DO�DO� DP  DP}qDQ  DQ��DQ�qDR}qDR�qDS}qDS�qDT� DU  DU��DV  DV� DW  DW}qDX  DX� DY  DY}qDY�qDZ� D[�D[� D\  D\� D]�D]��D^  D^}qD_  D_� D_�qD`� Da  Da� Db  Db� Dc�Dc� Dc�qDd� De�De��DfDf��Df�qDg� Dh  Dh}qDi  Di��Dj  Dj}qDk�Dk��Dl  Dl� Dm  Dm��Dn  Dn}qDo  Do��Dp  Dp� Dq  Dq� Dr  Dr��Ds�Ds� Ds�qDt}qDu  Du� Dv  Dv��Dw  Dw� Dw�qDxz�Dy  Dy}qDy�qDz� D{�D{��D|  D|� D|�qD}� D~  D~}qD~�qD}qD��D�@ D�� D�� D���D�>�D�~�D���D���D�@ D�� D�� D�  D�AHD��HD��HD�HD�@ D�~�D��qD���D�@ D�~�D���D�  D�>�D�}qD�� D�  D�>�D�� D�� D�  D�AHD�� D��HD�HD�@ D��HD�D��D�>�D�~�D��HD�HD�@ D�� D�� D�  D�@ D��HD��HD��D�@ D�~�D���D�  D�AHD�� D��qD���D�@ D�� D��HD���D�=qD�� D�� D���D�AHD��HD��HD�HD�@ D�� D�� D���D�@ D��HD�� D���D�>�D�� D��HD�  D�@ D�~�D���D���D�@ D�� D��HD�HD�B�D��HD���D�  D�AHD���D��HD�  D�@ D�~�D��HD�HD�@ D�� D�� D�HD�>�D�� D��HD�HD�B�D�� D���D�  D�B�D��HD�� D���D�@ D��HD�� D���D�@ D��HD�� D�HD�AHD��HD��HD���D�@ D�� D��qD���D�@ D��HD��HD�  D�>�D�}qD�� D�  D�>�D�~�D��HD�  D�>�D��HD��HD�HD�B�D��HD�� D��D�AHD�� D�D�HD�@ D��HD�� D���D�>�D�� D��HD��D�@ D�~�D���D�  D�B�D�� D���D���D�@ D�~�D���D���D�@ D��HD��HD���D�@ D��HD���D�  D�>�D�}qD��qD���D�@ D��HD�� D���D�AHD�� D�� D�HD�@ D�� D�D�HD�@ D�~�D��qD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�� D�  D�B�D��HD�� D�  D�>�D��HD��HD�  D�@ D�~�D���D�HD�B�D��HD�� D�  D�@ D��HD�D�HD�AHD�� D���D�  D�>�D�~�D�� D�  D�>�D�� D�� D���D�@ D��HD�� D���D�>�D�}qD���D�  D�@ D�� D�� D�  D�@ DHD��HD�HD�AHDÀ D�� D�  D�@ DĀ D�� D�HD�@ D�~�Dž�D��qD�@ DƁHD��HD�  D�AHDǀ DǾ�D�  D�@ D�~�D��HD�  D�@ Dɀ D�� D�HD�@ D�~�D�� D�  D�>�DˁHD��HD���D�@ D̀ D̽qD�  D�B�D̀ D;�D�  D�AHD΀ Dξ�D���D�>�Dπ D�� D�  D�@ D�~�D��HD�HD�@ D�~�D�� D�HD�AHDҀ D�� D�  D�@ D�~�D��HD�HD�>�D�}qDԾ�D�  D�>�D�~�D�� D�HD�AHDցHD��HD�  D�AHDׁHD�� D���D�>�D؀ D��HD�HD�AHDـ D�� D�HD�@ D�~�D�� D�  D�>�Dۀ D�� D�  D�AHD܀ D�� D���D�>�D݀ D�� D�  D�@ D�~�D޾�D�  D�>�D�}qD߾�D�  D�@ D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D�� D�  D�@ D�HD��HD�HD�AHD�HD�� D�  D�>�D�~�D徸D�  D�>�D�~�D�� D���D�>�D� D��HD�HD�>�D�}qD�� D�HD�>�D�~�D�� D�  D�@ D� D�� D�HD�AHD�~�D�� D�  D�@ D�HD�� D�  D�AHD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�qD�  D�AHD��HD�� D���D�=qD�~�D��HD�HD�AHD�HD�� D�  D�@ D�~�D�D��qD�=qD�HD�� D���D�>�D�~�D���D�  D�@ D��HD��HD�  D�>�D�� D��HD�  D�>�D�� D��HD���D�=qD�}qD���D��G�O�?�?#�
?�  ?�z�?\?�
=@   @\)@#�
@333@G�@W
=@h��@}p�@��@���@���@��\@��@�z�@�(�@�ff@�\)@�Q�@�G�@�@�z�@�(�A33A
=A�A  Az�A��Ap�A!�A'
=A*�HA0  A4z�A9��A>{AB�\AG
=AL(�AP��ATz�AY��A^{Ac33Ag
=Al(�Ap  Au�Ax��A}p�A�G�A��A�{A���A��HA�p�A��A��A�z�A��RA���A�33A�A�  A��A�z�A��RA���A��A�A�Q�A��HA��A��A��A���A�
=A���A�z�AƸRA���A˅A�{A�Q�Aҏ\A��A׮A�=qA�z�A޸RA�G�A��
A�{A��A�33A�p�A�  A�\A��A��A��A�z�A��RB z�BB�HB(�BG�B�\B�B��B
=qB�B��B{B\)B��B�B33BQ�B��B�HB(�BG�BffB�B��B{B33B z�B!��B"�HB$(�B%G�B&�\B'�
B)�B*ffB+�B,��B.=qB/�B0��B1�B333B4Q�B5��B6�RB8  B9�B:ffB;�B<��B>{B?\)B@z�BA�BC33BDQ�BEBF�RBH(�BIG�BJ�\BK�BL��BN{BO\)BP��BQ�BS33BTQ�BUBV�HBXQ�BYp�BZ�\B[�
B]�B^=qB_�B`��Bb{Bc33Bdz�BeBg
=BhQ�Bip�Bj�RBl  BmG�Bn�\Bo�Bp��Br{Bs33Btz�Bu��Bv�HBx  Byp�Bz�\B{�
B}G�B~�\B�B�ffB�
=B���B�=qB���B��B�(�B���B�\)B�  B���B�33B�B�ffB�
=B���B�(�B��RB�p�B�  B��RB�\)B��B��\B��B��B�ffB�
=B���B�ffB���B���B�=qB��HB��B�(�B���B�p�B�(�B���B��B�=qB���B�p�B�{B��RB�\)B�  B��RB�\)B�{B��RB�p�B�{B���B�p�B�{B���B�\)B�  B���B�\)B�{B��RB�p�B�(�B���B�\)B�  B���B�G�B��B��\B�G�B�  B���B�G�B��B�z�B��B�B�ffB��B�B�z�B��B�B�ffB���B���B�=qB��HB���B�Q�B�
=B��B�Q�B�
=B��B�ffB�
=B��B�ffB��B��
Bģ�B�\)B�  Bƣ�B�G�B��Bȏ\B�\)B�  BʸRB�\)B�  Ḅ�B�\)B�  B���BυB�(�B���B�p�B�{B���Bә�B�ffB��B�B�z�B�G�B�{B���Bٙ�B�Q�B���BۮB�z�B�G�B�  B޸RB߅B�(�B���B�B�\B�G�B�{B���B�p�B�=qB�
=B�B�\B�G�B�  B�RB�p�B�(�B�
=B��
B�z�B�33B��B��B�p�B�=qB���B�B�ffB��B��
B���B�p�B�=qB���B��B�ffB�G�B�{B��HB��B�ffB��B��C \)C �RC{Cp�C�
C=qC��C��CQ�C�C  C\)C�C
=CG�C�CC
=CQ�C�\C�RC�C{CG�Cz�C�C��C��C�CQ�C�C�C�
C��C	33C	\)C	��C	C	�HC

=C
=qC
ffC
��C
C
�C
=C=qCp�C��CC�C�CQ�Cp�C�\CC��C(�CG�Cp�C��C��C  C(�CG�Cp�C��C�
C  C(�CG�CffC��C��C  C�C=qCp�C��C��C�C{C=qCp�C��CC�C{C=qCz�C�C�
C��C�C\)C�\C�RC�C
=C=qCp�C��C��C��C(�CffC�\C�RC�HC�CQ�C�C��C��C
=C=qCp�C��C��C�C�C\)C�\C�RC�C{CQ�C�C�C�
C  C=qCp�C��CC��C33C\)C�C�C�HC�CG�CffC��C��C  C(�CQ�C�CC��C{C=qCz�C�C�
C  C(�C\)C��CC�C {C Q�C �C �C �
C!  C!33C!ffC!��C!��C!��C"{C"Q�C"�C"�C"�
C#
=C#G�C#z�C#��C#C$  C$33C$\)C$�\C$�RC$�C%(�C%Q�C%z�C%�RC%�C&�C&G�C&z�C&�RC&�C'{C'Q�C'�C'�C'�HC({C(Q�C(z�C(��C(�C){C)=qC)ffC)��C)��C)��C*33C*ffC*�C*�C*�HC+  C+�C+Q�C+z�C+�\C+�C+�
C+��C,
=C,33C,\)C,z�C,��C,�
C,��C-(�C-Q�C-p�C-�\C-C-�HC.  C.{C.{C.(�C.�C.(�C.�C.33C.G�C.Q�C.ffC.z�C.��C.��C.�HC/
=C/{C/=qC/=qC/\)C/p�C/z�C/�\C/�C/��C033C0ffC0��C0�
C1
=C1=qC1�C1�RC1�HC2�C2\)C2�\C2C3  C3=qC3ffC3��C3�HC4{C4=qC4�C4C4�C5�C5Q�C5��C5��C5��C6(�C6ffC6��C6�
C7  C7=qC7p�C7��C7�
C8�C8=qC8p�C8�C8�HC9{C9Q�C9�C9�C9�HC:�C:Q�C:z�C:�RC:�C;{C;G�C;�C;�C;�
C<�C<Q�C<�C<�RC<��C=�C=Q�C=��C=�
C>  C>=qC>z�C>�C>�HC?(�C?\)C?�\C?��C@{C@G�C@z�C@�RC@��CA(�CAffCA��CA�HCB{CBG�CB��CBCC  CCG�CC�CC�RCC��CD=qCDffCD�CD�CE(�CEffCE��CE�
CF{CFQ�CF��CFCG  CGG�CG�CG�RCH  CH33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                       @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�lG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A�x�A�v�A�x�A�p�A�t�A�t�A�`BA�XA�O�A�VA�E�A�5?A�&�A��yA�ȴA©�ADA�z�A�t�A�p�A�jA�hsA�bNA�\)A�\)A�XA�Q�A�O�A�O�A�M�A�M�A�I�A�I�A�E�A�E�A�I�A�K�A�S�A�jA©�A�+A�v�A�jA�
=A��A��HA��/A���A���AîA��mA��A�  A��A���A�ZA��A��!A���A��A��A�ffA��jA���A���A��A�A�A� �A�ȴA�C�A�v�A�-A�+A�K�A���A��7A��/A��-A���A�l�A��A�E�A�ĜA��A���A���A�`BA��A�bNA�ĜA��/A�%A��`A�&�A�r�A��A��A�z�A���A�\)A��7A���A��A�ĜA���A\)Az9XAv��At��As��Ar1'AqoAo��Al�!Aj �AgAd^5Ab�DA_�-A^�HA[��AX�HAW�AR��AQ��AOK�AKt�AHI�AFZAE��AEO�AE;dAE�AD9XAA�^A=�TA<�HA=�PA=��A;p�A6��A3�TA1K�A.~�A-S�A+��A*9XA)�A'�A&r�A$��A#C�A"�\A�-AI�At�AVA�hA��A��A��A��A�HA\)AE�A��A{An�AA�A+A�AbA�A�A�7A�AI�A�uAXA�hAt�An�A��A+A�A�A�A
�`A
ZA
��Al�A&�A
n�A	��A	�A�7A=qA�AJA1A�hA/A��A
=A�A��AffAx�A�A�;A|�AO�AC�A n�@���@�`B@� �@��@�x�@�j@���@�bN@�&�@��@���@��`@�|�@��@���@�M�@�$�@�ff@�1'@�;d@�S�@�~�@�$�@�
=@�V@�@��H@�J@���@��;@���@�r�@�O�@��@�I�@߶F@�1@�j@�Z@�9X@��
@��@߮@�\)@�;d@�t�@�33@��@�+@�1'@�X@�V@�1'@���@��@�5?@ݩ�@���@�r�@��@�-@�@ٙ�@�X@���@�1@ץ�@�C�@֧�@�M�@���@�x�@��@���@�Q�@�1@ӕ�@�l�@�C�@�v�@�v�@Ѻ^@�dZ@���@Ώ\@���@���@� �@˝�@�+@�^5@�X@��@ȼj@�r�@�A�@�  @���@�t�@�-@���@�X@���@�(�@��m@�K�@���@�@�@���@��`@�1@��\@�p�@�Q�@��w@��P@�S�@�
=@�@���@�E�@�@�x�@�%@�(�@��m@��@�A�@��@��@��;@��@�C�@��H@��R@��\@�ff@�{@���@�`B@�&�@���@�r�@�I�@��@�+@�~�@��@��-@���@���@��h@�x�@�G�@��`@��@��@�ȴ@��@���@���@�ff@�J@�@���@���@���@���@��7@�p�@�O�@�Ĝ@�z�@�A�@���@��m@���@��R@��@��#@��h@�p�@�`B@�/@���@�b@�;d@�@�ȴ@�~�@�ff@�5?@��@��#@��-@��7@�X@���@�r�@�1@��w@�|�@�
=@���@�ff@�@�@���@���@�x�@���@�bN@�I�@�9X@���@�"�@���@���@��\@�v�@�M�@�5?@��@��h@�?}@��j@��u@��D@�z�@�j@�1@�\)@�"�@��y@���@�n�@���@���@��h@��@�O�@��@�V@�V@��/@��D@�(�@��m@�dZ@��@��y@���@�ff@�@���@���@���@���@��^@�x�@�X@�?}@�V@��`@��/@�z�@�A�@��
@��P@�t�@�S�@�K�@�+@��@��H@���@��+@�~�@�n�@�V@���@�G�@���@�1@�ƨ@���@�\)@�33@��y@���@��+@�=q@��@�@��-@�`B@�7L@���@���@���@�Z@��@�b@��;@��P@�33@�
=@��y@��!@�M�@��@��^@��h@��7@�hs@�Ĝ@���@�r�@�I�@�(�@��@��
@�\)@�"�@��H@���@�V@�M�@�E�@��@�x�@�hs@�`B@�G�@�%@���@��/@��j@���@��
@�\)@��@�
=@��@�ff@�@���@���@��-@���@��7@�X@�7L@��@���@��`@�Ĝ@���@�z�@�A�@�(�@��@|�@~��@}�@}��@}�@}O�@}V@|z�@{�F@{t�@z�H@z�\@zM�@y��@y��@yhs@x��@w�@wK�@w;d@w+@v�y@v{@u�-@t��@st�@rn�@r-@r�@rJ@q�@q�#@q�#@q��@q��@p��@p1'@p  @o�w@o\)@n��@nȴ@n{@m@mO�@l�/@l�j@lz�@k��@k�@k�@kt�@k�@k"�@jn�@j�@i��@i�^@ihs@iG�@i�@hĜ@h1'@g\)@f��@fff@e�T@e�@d��@dj@d(�@d1@c��@cƨ@c�F@c�@cdZ@c33@c33@c33@c@c@b�@b��@b�!@bM�@`��@`��@`�@`Q�@`1'@`1'@` �@`  @_�w@_+@^ff@^E�@^$�@^@]�T@]�@\��@\��@\��@\��@\Z@\1@[�F@[t�@Z��@Z~�@Z�@Y�^@Y�@X��@XQ�@W��@W|�@W+@V�y@V�R@V��@Vv�@Vff@V@U�-@U`B@U/@T��@T�@S�F@S@R�\@R=q@Q�#@Q��@QG�@P��@P��@P�u@P1'@Pb@O�@O��@O�@OK�@N�@N5?@M��@M�-@M��@M�@MO�@M?}@M/@L�@L��@L��@Lz�@L9X@L�@K�m@KC�@J^5@I��@I��@I�7@Ix�@Ihs@IG�@I%@H�9@H�u@Hr�@G��@F�@F�R@F�+@FV@F5?@F{@E�T@E`B@D�D@D�@C�F@Ct�@CS�@C"�@Co@B�\@BJ@A%@@��@@ �@?�@?l�@?�@>�R@>@=��@=@=/@<�j@<��@<Z@;�m@;�F@;�@;S�@;o@:��@:-@9��@9�#@9X@8��@8�@8 �@8  @7��@7�w@7�P@7|�@7\)@6�y@6�+@6E�@6$�@5@5p�@5`B@5�@4�j@4j@4(�@3�
@3"�@2��@2-@1��@1G�@1&�@1�@1�@0��@0�`@0Ĝ@0�9@0��@0�u@0�u@0�u@0r�@01'@0b@/�w@/�P@/l�@/;d@/�@.ȴ@.��@.v�@-O�@-/@-/@,��@,��@,9X@,(�@,�@,1@+��@+�m@+�
@+��@+t�@+33@*��@*�\@*M�@*J@)��@)�7@)&�@(��@(A�@(  @'K�@&��@&v�@%�T@%`B@$��@$�j@$I�@$(�@$1@#�@#33@"�H@"��@"~�@"�@!�@!��@!��@!�7@!hs@!&�@!%@ Ĝ@ bN@ Q�@ b@�@��@��@|�@\)@�y@ȴ@�R@��@v�@ff@V@5?@@�-@`B@?}@V@��@�j@��@z�@Z@(�@��@�m@ƨ@��@t�@33@33@33@"�@@��@��@��@�7@�7@�7@hs@%@��@��@ �@��@�@�@�;@�;@��@|�@;d@
=@�y@�y@ȴ@�+@{@@�@�T@��@��@��@?}@�@�@V@�@�D@I�@1@�F@��@C�@"�@@�@�H@��@�!@��@�\@M�@=q@�@��@hs@%@�`@��@��@r�@r�@bN@Q�@ �@  @��@�w@�@��@�P@\)@K�@�@��@��@v�@E�@E�A�x�A�z�A�z�A�x�A�x�A�x�A�x�A�v�A�v�A�v�A�v�A�x�A�x�A�x�A�x�A�t�A�r�A�r�A�n�A�t�A�r�A�r�A�v�A�v�A�v�A�t�A�p�A�jA�Q�A�O�A�^5A�Q�A�ZA�XA�O�A�I�A�M�A�XA�VA�I�A�=qA�C�A�E�A�A�A�C�A�9XA�;dA�33A�7LA�33A�1'A�9XA�+A�+A�(�A��A�&�A�&�A�(�A�
=A��A��yA��yA��TA��TA��;A���A���A�ȴA�ƨA���A���A�ĜA¸RA®A¬A©�A£�A£�A£�AuA\ADA+AAA�~�A�~�A�|�A�z�A�x�A�v�A�v�A�t�A�t�A�v�A�v�A�t�A�t�A�t�A�r�A�r�A�p�A�n�A�n�A�l�A�l�A�l�A�jA�l�A�hsA�jA�jA�jA�jA�hsA�hsA�ffA�ffA�dZA�bNA�bNA�`BA�`BA�^5A�^5A�^5A�^5A�\)A�\)A�\)A�ZA�ZA�\)A�\)A�\)A�\)A�ZA�ZA�XA�XA�XA�XA�VA�VA�S�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�Q�A�O�A�O�A�O�A�O�A�Q�A�Q�A�O�A�O�A�O�A�O�A�M�A�K�A�K�A�M�A�M�A�M�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�K�A�I�A�I�A�G�A�E�A�G�A�I�A�I�A�G�A�I�A�I�A�G�A�G�A�G�A�G�A�E�A�E�A�E�A�C�A�C�A�A�A�A�A�E�A�G�A�G�A�I�A�I�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�M�A�O�A�O�A�Q�A�VA�VA�VA�VA�XA�\)A�`BA�^5A�n�A�v�A�~�AAuA©�A²-A¶FA¸RA¾wA�ĜA���A���A�oA�/A���A�O�A�ffA�r�A�~�Aĉ7Ać+AāA�~�A�v�A�l�A�ffA�I�A�"�A��A�
=A�A�  A�  A�  A���A���A��A��A��`A��`A��TA��TA��TA��;A��;A��;A��;A��;A��/A��#A���A���A���A���A���A���A���A���A���A�ĜA���AþwAú^Aú^Aú^Aú^Aú^AøRAá�A�~�A�\)A�;dA�{A�ƨA�A�n�A�M�A�33A���A��HA���A��FA��\A�I�A�bA�A��FA��-A��-A��!A��-A��A���A���A���A���A���A��A���A���A���A���A�p�A��A��A��A�G�A���A��A�ĜA��7A�r�A��FA�E�A�|�A�~�A��7A�/A��A���A�ZA��A���A��#A��A�Q�A�1A��A�bNA�33A��A���A�ƨA���A��A�dZA�G�A��A�A���A���A���A�p�A�M�A�5?A��A���A�ȴA�p�A�&�A���A��A��A��`A��A�%A��RA�$�A���A��^A��7A�jA�E�A�VA���A��hA�A�A�A���A��#A�ZA��mA�ĜA���A��+A�x�A�hsA�M�A�/A� �A��A�1A���A�p�A�/A���A��uA�r�A�`BA�K�A�+A�  A���A��A��A���A��A��PA�jA�(�A��A�%A���A��A���A��9A���A�x�A�ffA�\)A�XA�VA�Q�A�I�A�?}A�5?A�&�A� �A��A�{A�
=A�1A�%A�A���A��A��A��yA��`A��/A���A��jA��!A���A��hA��7A�z�A�l�A�bNA�\)A�S�A�K�A�C�A�A�A�=qA�1'A�&�A�VA��A�ȴA��FA��uA�O�A�bA��^A��DA�dZA�M�A�1'A�%A��;A��DA��A��RA��PA�S�A�K�A�9XA�JA�A���A��A��A��`A���A���A��FA��!A���A���A�t�A�^5A�S�A�K�A�G�A�G�A�A�A�E�A�9XA�+A�$�A��A�bA�1A��`A�ȴA��FA�O�A��#A��A��A�ffA�VA�?}A� �A�1A���A��A��yA��;A��#A��
A���A�A��RA��RA��RA��9A��A���A���A���A���A���A��PA�|�A�x�A�jA�XA�S�A�M�A�E�A�(�A�oA�VA���A���A��hA��A�n�A�hsA�ffA�bNA�ZA�5?A�
=A��mA��#A�ƨA��wA���A���A���A���A��PA��+A�l�A�`BA�?}A�7LA�;dA�5?A��A�
=A���A��HA��9A��uA�K�A��mA�Q�A��`A���A�l�A�jA�l�A�t�A�x�A�n�A�dZA�O�A�?}A�-A� �A�bA���A��`A���A�A��RA��A���A��A�S�A�=qA�5?A�/A�&�A�&�A� �A�
=A���A���A��/A���A���A�z�A�;dA��A���A��PA��A�n�A�l�A�-A�+A�{A�oA��A���A�oA���A��
A���A��9A��!A���A���A�dZA�`BA�M�A�7LA�7LA�33A��yA��A���A��DA�XA�=qA�
=A���A���A�VA�$�A�VA���A�A�A�A�l�A�(�A���A��TA���A���A���A��A�S�A�C�A�9XA�5?A�/A�(�A�&�A�"�A� �A��A��A�bA�1A���A���A��A��HA��#A���A�ƨA��A�t�A�hsA�dZA�?}A�$�A�oA�  A���A�G�A�  A��A��A���A���A��9A�z�A�oA���A�K�A�  A��TA��
A��7A�?}A�bA���A���A�|�A��A���A�v�A�?}A��jA�+A���A�hsA�=qA��A�A�  A���A���A��A��mA��;A���A��!A��7A�33A��/A��A�=qA�oA��mA��^A���A��A�t�A�t�A�^5A�%A���A��\A�x�A�ZA�9XA��A��mA���A�A��PA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                       A�z�A�x�A�v�A�x�A�p�A�t�A�t�A�`BA�XA�O�A�VA�E�A�5?A�&�A��yA�ȴA©�ADA�z�A�t�A�p�A�jA�hsA�bNA�\)A�\)A�XA�Q�A�O�A�O�A�M�A�M�A�I�A�I�A�E�A�E�A�I�A�K�A�S�A�jA©�A�+A�v�A�jA�
=A��A��HA��/A���A���AîA��mA��A�  A��A���A�ZA��A��!A���A��A��A�ffA��jA���A���A��A�A�A� �A�ȴA�C�A�v�A�-A�+A�K�A���A��7A��/A��-A���A�l�A��A�E�A�ĜA��A���A���A�`BA��A�bNA�ĜA��/A�%A��`A�&�A�r�A��A��A�z�A���A�\)A��7A���A��A�ĜA���A\)Az9XAv��At��As��Ar1'AqoAo��Al�!Aj �AgAd^5Ab�DA_�-A^�HA[��AX�HAW�AR��AQ��AOK�AKt�AHI�AFZAE��AEO�AE;dAE�AD9XAA�^A=�TA<�HA=�PA=��A;p�A6��A3�TA1K�A.~�A-S�A+��A*9XA)�A'�A&r�A$��A#C�A"�\A�-AI�At�AVA�hA��A��A��A��A�HA\)AE�A��A{An�AA�A+A�AbA�A�A�7A�AI�A�uAXA�hAt�An�A��A+A�A�A�A
�`A
ZA
��Al�A&�A
n�A	��A	�A�7A=qA�AJA1A�hA/A��A
=A�A��AffAx�A�A�;A|�AO�AC�A n�@���@�`B@� �@��@�x�@�j@���@�bN@�&�@��@���@��`@�|�@��@���@�M�@�$�@�ff@�1'@�;d@�S�@�~�@�$�@�
=@�V@�@��H@�J@���@��;@���@�r�@�O�@��@�I�@߶F@�1@�j@�Z@�9X@��
@��@߮@�\)@�;d@�t�@�33@��@�+@�1'@�X@�V@�1'@���@��@�5?@ݩ�@���@�r�@��@�-@�@ٙ�@�X@���@�1@ץ�@�C�@֧�@�M�@���@�x�@��@���@�Q�@�1@ӕ�@�l�@�C�@�v�@�v�@Ѻ^@�dZ@���@Ώ\@���@���@� �@˝�@�+@�^5@�X@��@ȼj@�r�@�A�@�  @���@�t�@�-@���@�X@���@�(�@��m@�K�@���@�@�@���@��`@�1@��\@�p�@�Q�@��w@��P@�S�@�
=@�@���@�E�@�@�x�@�%@�(�@��m@��@�A�@��@��@��;@��@�C�@��H@��R@��\@�ff@�{@���@�`B@�&�@���@�r�@�I�@��@�+@�~�@��@��-@���@���@��h@�x�@�G�@��`@��@��@�ȴ@��@���@���@�ff@�J@�@���@���@���@���@��7@�p�@�O�@�Ĝ@�z�@�A�@���@��m@���@��R@��@��#@��h@�p�@�`B@�/@���@�b@�;d@�@�ȴ@�~�@�ff@�5?@��@��#@��-@��7@�X@���@�r�@�1@��w@�|�@�
=@���@�ff@�@�@���@���@�x�@���@�bN@�I�@�9X@���@�"�@���@���@��\@�v�@�M�@�5?@��@��h@�?}@��j@��u@��D@�z�@�j@�1@�\)@�"�@��y@���@�n�@���@���@��h@��@�O�@��@�V@�V@��/@��D@�(�@��m@�dZ@��@��y@���@�ff@�@���@���@���@���@��^@�x�@�X@�?}@�V@��`@��/@�z�@�A�@��
@��P@�t�@�S�@�K�@�+@��@��H@���@��+@�~�@�n�@�V@���@�G�@���@�1@�ƨ@���@�\)@�33@��y@���@��+@�=q@��@�@��-@�`B@�7L@���@���@���@�Z@��@�b@��;@��P@�33@�
=@��y@��!@�M�@��@��^@��h@��7@�hs@�Ĝ@���@�r�@�I�@�(�@��@��
@�\)@�"�@��H@���@�V@�M�@�E�@��@�x�@�hs@�`B@�G�@�%@���@��/@��j@���@��
@�\)@��@�
=@��@�ff@�@���@���@��-@���@��7@�X@�7L@��@���@��`@�Ĝ@���@�z�@�A�@�(�@��@|�@~��@}�@}��@}�@}O�@}V@|z�@{�F@{t�@z�H@z�\@zM�@y��@y��@yhs@x��@w�@wK�@w;d@w+@v�y@v{@u�-@t��@st�@rn�@r-@r�@rJ@q�@q�#@q�#@q��@q��@p��@p1'@p  @o�w@o\)@n��@nȴ@n{@m@mO�@l�/@l�j@lz�@k��@k�@k�@kt�@k�@k"�@jn�@j�@i��@i�^@ihs@iG�@i�@hĜ@h1'@g\)@f��@fff@e�T@e�@d��@dj@d(�@d1@c��@cƨ@c�F@c�@cdZ@c33@c33@c33@c@c@b�@b��@b�!@bM�@`��@`��@`�@`Q�@`1'@`1'@` �@`  @_�w@_+@^ff@^E�@^$�@^@]�T@]�@\��@\��@\��@\��@\Z@\1@[�F@[t�@Z��@Z~�@Z�@Y�^@Y�@X��@XQ�@W��@W|�@W+@V�y@V�R@V��@Vv�@Vff@V@U�-@U`B@U/@T��@T�@S�F@S@R�\@R=q@Q�#@Q��@QG�@P��@P��@P�u@P1'@Pb@O�@O��@O�@OK�@N�@N5?@M��@M�-@M��@M�@MO�@M?}@M/@L�@L��@L��@Lz�@L9X@L�@K�m@KC�@J^5@I��@I��@I�7@Ix�@Ihs@IG�@I%@H�9@H�u@Hr�@G��@F�@F�R@F�+@FV@F5?@F{@E�T@E`B@D�D@D�@C�F@Ct�@CS�@C"�@Co@B�\@BJ@A%@@��@@ �@?�@?l�@?�@>�R@>@=��@=@=/@<�j@<��@<Z@;�m@;�F@;�@;S�@;o@:��@:-@9��@9�#@9X@8��@8�@8 �@8  @7��@7�w@7�P@7|�@7\)@6�y@6�+@6E�@6$�@5@5p�@5`B@5�@4�j@4j@4(�@3�
@3"�@2��@2-@1��@1G�@1&�@1�@1�@0��@0�`@0Ĝ@0�9@0��@0�u@0�u@0�u@0r�@01'@0b@/�w@/�P@/l�@/;d@/�@.ȴ@.��@.v�@-O�@-/@-/@,��@,��@,9X@,(�@,�@,1@+��@+�m@+�
@+��@+t�@+33@*��@*�\@*M�@*J@)��@)�7@)&�@(��@(A�@(  @'K�@&��@&v�@%�T@%`B@$��@$�j@$I�@$(�@$1@#�@#33@"�H@"��@"~�@"�@!�@!��@!��@!�7@!hs@!&�@!%@ Ĝ@ bN@ Q�@ b@�@��@��@|�@\)@�y@ȴ@�R@��@v�@ff@V@5?@@�-@`B@?}@V@��@�j@��@z�@Z@(�@��@�m@ƨ@��@t�@33@33@33@"�@@��@��@��@�7@�7@�7@hs@%@��@��@ �@��@�@�@�;@�;@��@|�@;d@
=@�y@�y@ȴ@�+@{@@�@�T@��@��@��@?}@�@�@V@�@�D@I�@1@�F@��@C�@"�@@�@�H@��@�!@��@�\@M�@=q@�@��@hs@%@�`@��@��@r�@r�@bN@Q�@ �@  @��@�w@�@��@�P@\)@K�@�@��@��@v�@E�G�O�A�x�A�z�A�z�A�x�A�x�A�x�A�x�A�v�A�v�A�v�A�v�A�x�A�x�A�x�A�x�A�t�A�r�A�r�A�n�A�t�A�r�A�r�A�v�A�v�A�v�A�t�A�p�A�jA�Q�A�O�A�^5A�Q�A�ZA�XA�O�A�I�A�M�A�XA�VA�I�A�=qA�C�A�E�A�A�A�C�A�9XA�;dA�33A�7LA�33A�1'A�9XA�+A�+A�(�A��A�&�A�&�A�(�A�
=A��A��yA��yA��TA��TA��;A���A���A�ȴA�ƨA���A���A�ĜA¸RA®A¬A©�A£�A£�A£�AuA\ADA+AAA�~�A�~�A�|�A�z�A�x�A�v�A�v�A�t�A�t�A�v�A�v�A�t�A�t�A�t�A�r�A�r�A�p�A�n�A�n�A�l�A�l�A�l�A�jA�l�A�hsA�jA�jA�jA�jA�hsA�hsA�ffA�ffA�dZA�bNA�bNA�`BA�`BA�^5A�^5A�^5A�^5A�\)A�\)A�\)A�ZA�ZA�\)A�\)A�\)A�\)A�ZA�ZA�XA�XA�XA�XA�VA�VA�S�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�Q�A�O�A�O�A�O�A�O�A�Q�A�Q�A�O�A�O�A�O�A�O�A�M�A�K�A�K�A�M�A�M�A�M�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�K�A�I�A�I�A�G�A�E�A�G�A�I�A�I�A�G�A�I�A�I�A�G�A�G�A�G�A�G�A�E�A�E�A�E�A�C�A�C�A�A�A�A�A�E�A�G�A�G�A�I�A�I�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�K�A�K�A�K�A�M�A�O�A�O�A�Q�A�VA�VA�VA�VA�XA�\)A�`BA�^5A�n�A�v�A�~�AAuA©�A²-A¶FA¸RA¾wA�ĜA���A���A�oA�/A���A�O�A�ffA�r�A�~�Aĉ7Ać+AāA�~�A�v�A�l�A�ffA�I�A�"�A��A�
=A�A�  A�  A�  A���A���A��A��A��`A��`A��TA��TA��TA��;A��;A��;A��;A��;A��/A��#A���A���A���A���A���A���A���A���A���A�ĜA���AþwAú^Aú^Aú^Aú^Aú^AøRAá�A�~�A�\)A�;dA�{A�ƨA�A�n�A�M�A�33A���A��HA���A��FA��\A�I�A�bA�A��FA��-A��-A��!A��-A��A���A���A���A���A���A��A���A���A���A���A�p�A��A��A��A�G�A���A��A�ĜA��7A�r�A��FA�E�A�|�A�~�A��7A�/A��A���A�ZA��A���A��#A��A�Q�A�1A��A�bNA�33A��A���A�ƨA���A��A�dZA�G�A��A�A���A���A���A�p�A�M�A�5?A��A���A�ȴA�p�A�&�A���A��A��A��`A��A�%A��RA�$�A���A��^A��7A�jA�E�A�VA���A��hA�A�A�A���A��#A�ZA��mA�ĜA���A��+A�x�A�hsA�M�A�/A� �A��A�1A���A�p�A�/A���A��uA�r�A�`BA�K�A�+A�  A���A��A��A���A��A��PA�jA�(�A��A�%A���A��A���A��9A���A�x�A�ffA�\)A�XA�VA�Q�A�I�A�?}A�5?A�&�A� �A��A�{A�
=A�1A�%A�A���A��A��A��yA��`A��/A���A��jA��!A���A��hA��7A�z�A�l�A�bNA�\)A�S�A�K�A�C�A�A�A�=qA�1'A�&�A�VA��A�ȴA��FA��uA�O�A�bA��^A��DA�dZA�M�A�1'A�%A��;A��DA��A��RA��PA�S�A�K�A�9XA�JA�A���A��A��A��`A���A���A��FA��!A���A���A�t�A�^5A�S�A�K�A�G�A�G�A�A�A�E�A�9XA�+A�$�A��A�bA�1A��`A�ȴA��FA�O�A��#A��A��A�ffA�VA�?}A� �A�1A���A��A��yA��;A��#A��
A���A�A��RA��RA��RA��9A��A���A���A���A���A���A��PA�|�A�x�A�jA�XA�S�A�M�A�E�A�(�A�oA�VA���A���A��hA��A�n�A�hsA�ffA�bNA�ZA�5?A�
=A��mA��#A�ƨA��wA���A���A���A���A��PA��+A�l�A�`BA�?}A�7LA�;dA�5?A��A�
=A���A��HA��9A��uA�K�A��mA�Q�A��`A���A�l�A�jA�l�A�t�A�x�A�n�A�dZA�O�A�?}A�-A� �A�bA���A��`A���A�A��RA��A���A��A�S�A�=qA�5?A�/A�&�A�&�A� �A�
=A���A���A��/A���A���A�z�A�;dA��A���A��PA��A�n�A�l�A�-A�+A�{A�oA��A���A�oA���A��
A���A��9A��!A���A���A�dZA�`BA�M�A�7LA�7LA�33A��yA��A���A��DA�XA�=qA�
=A���A���A�VA�$�A�VA���A�A�A�A�l�A�(�A���A��TA���A���A���A��A�S�A�C�A�9XA�5?A�/A�(�A�&�A�"�A� �A��A��A�bA�1A���A���A��A��HA��#A���A�ƨA��A�t�A�hsA�dZA�?}A�$�A�oA�  A���A�G�A�  A��A��A���A���A��9A�z�A�oA���A�K�A�  A��TA��
A��7A�?}A�bA���A���A�|�A��A���A�v�A�?}A��jA�+A���A�hsA�=qA��A�A�  A���A���A��A��mA��;A���A��!A��7A�33A��/A��A�=qA�oA��mA��^A���A��A�t�A�t�A�^5A�%A���A��\A�x�A�ZA�9XA��A��mA���A�A��PA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	Q�B	RTB	Q�B	P�B	P�B	PHB	P}B	Q�B	PHB	Q�B	PHB	Q�B	P}B	N�B	PHB	O�B	NpB	O�B	O�B	N�B	OB	O�B	N�B	OB	PB	OBB	OB	P�B	PB	O�B	P�B	PB	QNB	PB	P�B	R�B	T,B	VmB	W�B	_pB	yrB	��B
�AB
�B
�B
��B
��B
��B
��B
�B
��B
�)B
�B
�EB
�vB
�B
�B�B?HBh>By�B�_B�lB�bB�_B��B�$B�aB��B��B��BϫB��B��B̘B��B��BɺB��BخB�B��B��B��BȀBB��B��B�_B��B��B�BkBK�B"�BSB
��B
�lB
��B
ʌB
��B
�B
� B
k�B
YKB
D�B
�B	��B	�)B	˒B	�B	�$B	��B	�FB	��B	�B	u�B	d&B	\�B	K�B	D�B	;�B	&B	!�B	�B	
�B	B	uB	  B��B�B�rB�lB�fB�rB	B��B��B	�B	�B	�B�PB�B˒B�qB�zB�LB�3B��B�'B��B�6B�B��B�*B�B�*B�HBרB�HB�EB�[B��B�BخB�vB��B��B�fB�TB�MB�B��B	  B	�B	�B	_B	DB	�B	2aB	?B	M6B	FB	=B	-B	&�B	"4B	+B	�B	�B	$@B	9$B	C�B	CaB	@OB	>wB	?HB	-wB	-B	2aB	B[B	CaB	G�B	K�B	P�B	XEB	WsB	W�B	S�B	O�B	K)B	G�B	F?B	L�B	FtB	EB	@OB	B�B	E�B	M6B	UgB	O�B	cTB	_B	TaB	WsB	T�B	x�B	y�B	zB	zDB	|PB	��B	�B	}"B	�uB	�YB	�;B	�_B	�_B	�oB	}"B	�B	}�B	}�B	{�B	x8B	n�B	f2B	\�B	]dB	a�B	g�B	iB	kQB	l�B	k�B	t�B	{B	|PB	��B	�iB	�~B	��B	�=B	�B	��B	�B	�eB	�IB	�wB	��B	�IB	�}B	��B	��B	�'B	��B	�[B	�-B	�hB	��B	�B	�zB	��B	��B	�^B	�0B	�^B	��B	��B	�B	�HB	�[B	�[B	��B	��B	�B	�dB	�*B	��B	��B	�-B	�[B	��B	�B	�FB	�B	��B	��B	��B	��B	�6B	�B	�'B	�UB	��B	�UB	��B	�OB	��B	�}B	�B	��B	�B	�6B	��B	��B	�9B	�nB	��B	�B	��B	��B	��B	�B	��B	�OB	��B	��B	�OB	ƨB	��B	� B	ԕB	�QB	רB	�sB	��B	خB	��B	�BB	�B	�TB	�ZB	��B	� B	�B	�B	�B	�B	� B	�B	�B	�&B	��B	�B	�B	�B	��B	�ZB	�mB	�8B	�B	�B	�)B	��B	�]B	��B	�;B	��B	��B	��B	�B	�B	�TB	��B	�+B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�2B	�lB	�>B	�	B	�rB	�B	��B	�B	�DB	�DB	�DB	�B	�B	�B	�B	��B	��B	��B	�B	�PB	��B	�VB	��B	�"B	�"B	�VB	�.B	��B	�]B	�(B
 iB
  B
;B
�B
�B
AB
B
B
�B
�B
�B
+B
�B
�B
�B
�B
_B
1B
�B
1B
1B
�B
	lB
	�B
	�B
	lB

	B
	�B
	�B
	�B
	�B

=B

�B
B
B
�B
JB
�B
PB
VB
�B
VB
VB
VB
�B
�B
(B
(B
�B
.B
 B
oB
�B
B
{B
{B
�B
�B
�B
�B
MB
�B
MB
MB
B
B
B
B
�B
�B
�B
�B
+B
�B
1B
eB
�B
kB
7B
7B
	B
=B
=B
�B
�B
B
xB
�B
xB
B
B
!B
�B
�B
�B
�B
 \B
 \B
 \B
 \B
 �B
"hB
!�B
"�B
"�B
"�B
"hB
"�B
#nB
#nB
#�B
$@B
$�B
$�B
$�B
%�B
&�B
&B
&B
&LB
&�B
&�B
&�B
&�B
&�B
(�B
(�B
)*B
)*B
)_B
*�B
*�B
*eB
*�B
*�B
*�B
*�B
+kB
+kB
+�B
,=B
+�B
,B
,�B
-CB
,�B
-CB
-wB
.B
.IB
.IB
.}B
.�B
.�B
/B
0!B
0�B
1�B
1�B
1'B
1'B
0�B
1'B
1�B
2�B
2�B
3hB
3hB
3�B
5?B
6FB
5�B
6FB
6FB
6FB
6FB
6B
5�B
6B
6B
6B
5�B
5�B
7LB
7LB
7B
7LB
7�B
8RB
8RB
8�B
8�B
9�B
9XB
9�B
:^B
:�B
;�B
<B
;�B
;�B
<�B
>B
>B
=�B
>B
=�B
=�B
=�B
=qB
>�B
=�B
=�B
>wB
>BB
@B
@OB
@OB
@�B
@�B
@�B
@�B
@�B
A B
@�B
A B
@�B
A B
A B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
DgB
DgB
D�B
D�B
D�B
D�B
E9B
FB
F?B
FB
E�B
F�B
F�B
GB
F�B
GB
F�B
GEB
G�B
HB
HB
H�B
IB
IRB
I�B
I�B
I�B
J#B
I�B
I�B
J�B
JXB
K)B
J�B
K�B
LdB
L�B
MB
M�B
M�B
NB
N<B
N�B
OB
N�B
O�B
PHB
P}B
P}B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S&B
S[B
S[B
T,B
T�B
UgB
U�B
U�B
U�B
U�B
W
B
WsB
XEB
XyB
XyB
XyB
X�B
X�B
X�B
YB
YB
X�B
Y�B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[�B
\)B
\)B
\)B
\)B
\]B
\)B
[�B
[�B
[�B
[�B
\)B
\]B
\�B
\�B
\�B
]�B
]�B
]�B
^jB
^�B
^jB
_B
_;B
_pB
_�B
_�B
`B
`vB
`�B
`�B
`�B
a�B
bB
bNB
b�B
b�B
b�B
b�B
b�B
b�B
c B
c�B
d&B
dZB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
e,B
e�B
ffB
f�B
gB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h
B
g�B
h
B
h
B
h
B
g�B
h>B
hsB
hsB
h�B
h�B
iB
iB
iDB
i�B
iyB
i�B
kQB
kB
j�B
kQB
k�B
l"B
l"B
l"B
lWB
l"B
l"B
l"B
l�B
lWB
l�B
l�B
m)B
m]B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
poB
p�B
qB
qAB
q�B
q�B
q�B
rB
r|B
r�B
r�B
r�B
sB
sB
sMB
s�B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
u%B
uZB
uZB
uZB
u�B
v+B
u�B
v+B
v`B
v+B
v`B
v`B
v�B
w2B
v�B
w2B
w2B
wfB
w�B
w�B
w�B
w�B
xlB
x�B
x�B
y	B
y>B
y�B
y�B
y�B
zB
zDB
zDB
z�B
{�B
{JB
{B
{B
{B
{B
{�B
{�B
{�B
|B
|PB
|�B
|�B
}"B
~]B
~�B
�B
�4B
��B
��B
��B
��B
�B
�oB
�oB
�oB
�oB
�oB
�;B
��B
��B
�B
��B
�B
��B
�AB
�AB
�uB
��B
��B
�GB
�B
�{B
�{B
�{B
��B
�{B
�GB
�{B
��B
��B
��B
��B
��B
��B
�SB
��B
��B
��B
��B
��B
�%B
�%B
�YB
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
�fB
�1B	Q�B	Q�B	Q�B	R B	Q�B	Q�B	R�B	R�B	R B	Q�B	QB	QNB	P�B	P�B	P�B	P�B	P�B	O�B	QNB	O�B	P�B	PHB	PHB	P�B	OvB	Q�B	PHB	P}B	VmB	Q�B	M�B	R B	OB	Q�B	OvB	T,B	O�B	R B	R�B	T�B	QNB	OBB	QNB	PB	Q�B	S&B	O�B	P�B	OvB	PB	Q�B	O�B	Q�B	P�B	N<B	OBB	L�B	N�B	NB	U2B	NpB	Q�B	N�B	O�B	O�B	O�B	R�B	OBB	PB	OvB	M6B	NpB	QNB	N<B	N<B	N<B	MjB	N<B	NpB	NB	O�B	N�B	O�B	O�B	O�B	P}B	O�B	O�B	PB	PB	O�B	O�B	OvB	OvB	N�B	NpB	N<B	N<B	NpB	M�B	N�B	N�B	OB	OBB	OvB	O�B	PB	O�B	PB	OvB	OvB	OBB	N�B	N�B	OB	NpB	N<B	N�B	NB	NpB	OB	OBB	OBB	O�B	O�B	O�B	O�B	O�B	O�B	O�B	PHB	P}B	PB	OBB	N�B	OvB	N�B	N�B	N�B	N�B	N�B	OBB	OB	O�B	O�B	PHB	P�B	P�B	P�B	P}B	QB	P�B	P}B	O�B	PHB	PB	O�B	O�B	OBB	OvB	PB	O�B	PHB	PHB	P�B	Q�B	QNB	QB	P�B	P�B	P}B	O�B	O�B	O�B	PB	PB	PB	P}B	QB	Q�B	Q�B	Q�B	QNB	P�B	P}B	PHB	OvB	OvB	O�B	PHB	PHB	PHB	P�B	QB	P�B	Q�B	QNB	RTB	R�B	R�B	S�B	T,B	S[B	S�B	S�B	S�B	T,B	T�B	TaB	U2B	U�B	V�B	V�B	V9B	VB	VB	T�B	W?B	VB	W�B	Y�B	Z�B	YKB	\]B	]/B	\)B	_B	e`B	d�B	gB	h�B	sB	|PB	��B	��B	�fB	��B	�(B	��B	��B	��B
�B
[WB
t�B
�B
�B
�B
��B
�(B
��B
�FB
��B
�SB
��B
�LB
�zB
��B
��B
�FB
��B
�LB
��B
��B
�$B
��B
��B
�$B
�$B
��B
��B
�XB
�B
�RB
��B
��B
��B
��B
��B
�*B
��B
�$B
��B
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
�UB
��B
��B
��B
�[B
�B
�#B
�dB
��B
˒B
�RB
�zB
�B
̘B
��B
ĜB
��B
��B
�aB
�B
��B
�;B
�;B
�B
�pB
�jB
�pB
�B
�B
�B
�B
�B
��B
��B
�8B
��B
�8B
�B
��B
��B	�B�B	�BSB�B�B
��B-�B8�BC�B}�Bc�Ba|Be�Bh
BlWBpoBncBq�Bv`B��B��B��B�YB�_B��B�{B�B��B��B�=B�B�~B�7B�lB��B�oB��B�{B��B�MB��B�qB�nB��B��B��B��B�OB��B�!B��B� B��B��B�B�}B�[B�EB��B�RB�B�KB�EB�RB��B� B�)B�B��B��B��B��B˒BɆB�BʌBԕB��B�B�gB�6BǮBȴB�XB�B�B˒B�jB�6B�B�BB�aBбB�]B�#B��B�0B�pBҽB�BϫB�B̘B�6B��B˒B��B˒B�0B��B�jB�)BʌB�)B��B˒B�XB�#BʌB˒B��B�#BɆBʌB�0B̘BʌB�XB��B�B�^B˒B�KB�B�B�B�KB�EB�?B��B�B˒BʌB��B��B�B�}B��B�B�B� B� B��B�}B��BȴB��B��B��B҉B�HB�gB�]B�KB�sB�BخB�QB�]B��B�)B�)B��B��B�B�B�B�B�B��B��B�BB�B�B�B�B�pB�B�NB��B�B�B�B�B�EB�aBҽB��B��BбB�B��B͟B�<B�6B��B̘B�dB�0B�)B��B�XB�XB˒BɺBȀB�KB�B�XB��BƨBȀB�zB�?B�B��B�KB�'B��B�-B��B�B�B��B��B��B��B�^B��B��B��B��B�nB�LB��B��B�[B�aB��B��B��B�aB�B��B�B��B�?B�?B��B�_B��B��B��B��B��B��B�FB��B��B��B�1B��B�=B�B�'B�IB��B��B��B�_B��B�YB��B�FB��B��B�B��B�4B��B�"B��B��B�JB��B�=B��B��B��B��B�1B��Bx�B�fB}�BzBn�BuZB�BrBk�Bp�BrGBffBc�Bn/Bf2Bb�B`BB`�B_pB_;Be�BY�B\]BN�BS[BT�BW?B]�BB�BJXBD3BB'B:�B@B5�B@OB0�B.B+kB%zB-BG�B�B�B
	B�B	�B�BGB
�BBGB iB�BAB
�.B
�]B
��B
��B
��B
�cB
��B  B
��B
�JB
��B
�B
�lB
�lB
�>BoB
�TB
�B
��B
�2B
�oB
�cB
�B
�B
��B
��B
ݘB
�WB
��B
��B
�EB
�B
ںB
�B
��B
��B
�B
�B
B
�RB
��B
��B
�-B
��B
��B
��B
��B
�bB
��B
�+B
��B
�"B
��B
��B
��B
�B
��B
��B
�B
��B
�4B
��B
{JB
�;B
�GB
y�B
u�B
t�B
lWB
l�B
jB
d�B
bNB
_�B
\�B
^jB
h�B
[#B
T�B
Q�B
Q�B
OvB
OvB
NpB
E9B
@�B
K�B
T�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2021032505455820210325054558IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021040402021520210404020215QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021040402021520210404020215QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014520210427140145IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20220504162936              CF      PSAL                            ?k�G�O�D�4{G�O�?�  G�O�Sensor Failure                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20220504163446    CF                  PSAL            G�O�?�G�O�CH33G�O�?�                  Sensor Failure  SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                