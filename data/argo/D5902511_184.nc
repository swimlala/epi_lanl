CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-10-06T01:29:49Z creation; 2022-02-04T23:30:04Z DMQC;      
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
_FillValue        G�O�       =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \    PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       c�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�       ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 5�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      =�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � \�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      dx   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20211006012949  20220204223518  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_184                 6810_008521_184                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @٘�u%F@٘�u%F11  @٘�7��4@٘�7��4@0��N;�6@0��N;�6�d�_o���d�_o��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@=p�@�  @�  @�  @�  A   A\)A   A+�A?\)A`��A�Q�A��A�  A���A�  A�\)A߮A�A�\)B�B  B  B   B(  B0  B8  B?�
BH  BP  BW�B`  BhQ�Bp(�Bw�
B�
B�  B�  B�  B�  B�  B�  B�  B��
B��
B��B�  B�  B��B��B��B�  B�  B�  B�{B�  B�{B�{B�  B��B�  B�  B��
B��
B��
B�  B�{C   C  C��C��C  C	��C  C
=C��C  C  C��C  C  C��C��C   C"{C$
=C&  C(
=C)��C,  C.  C/�C2  C4
=C5��C8  C:  C;��C=��C@  CB  CD
=CF
=CG��CI��CL
=CM��CO��CQ��CS�CV  CX
=CZ  C\  C^  C_��Ca��Cd
=Cf
=Ch
=Cj
=Cl{Cn
=Cp  Cr
=Ct  Cv  Cx
=Cz  C|  C~
=C�  C�  C�  C���C���C�  C�  C�  C�C�
=C�
=C�C�
=C���C�  C�  C���C�  C�C�C�  C���C���C�  C���C���C�C���C�C�  C�  C�
=C�  C�  C�C�C�  C���C���C�C�  C���C�  C�  C���C�  C�C���C���C���C���C�C�C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C�  C�C�C���C���C���C���C���C�C�C���C���C�  C�
=C�  C���C�  C�  C�C�
=C�C���C���C���C�  C�  C���C�  C�  C�  C�C�  C�  C���C���C���C�  C���C���C���C���C���C�
=C�
=C���C���C�  C�  C�C�  C���C�  C�  C�C�  C�C�
=C�C���C���C���C���C���D }qD  D� D  D}qD��Dz�D�qD� D�qD}qD  D��D�D�D  D}qD	  D	��D
�D
� D�D��D�qDz�D  D��D�qDz�D  D��D�qD� D  D� D  D� D  D� D  D� D�D��D  D� DD��D  D� DD��D�qD� D�qDz�D�qD� D  D� D�qDz�D�qD� D �D ��D!�D!}qD!��D"}qD"�qD#� D$�D$� D%  D%� D%��D&}qD&��D'}qD(�D(�D)  D)��D*  D*}qD*�qD+� D+�qD,z�D,��D-��D.D.��D/�D/�D0�D0}qD1  D1��D1�qD2}qD3  D3� D3�qD4� D5�D5� D6  D6}qD6�qD7� D8  D8��D9�D9}qD:  D:��D;�D;� D<  D<��D=�D=��D>�D>�D?�D?� D@  D@� DA  DA� DB  DB� DB�qDC��DD  DD� DE  DE� DF  DF� DG�DG}qDH  DH��DI�DI� DJ�DJ� DJ�qDK� DL�DL��DL�qDM}qDN�DN��DO�DO��DP  DP}qDP�qDQ}qDR�DR��DR�qDS� DT  DT� DU  DU}qDV  DV�DW�DW� DW��DXxRDX�RDY}qDZDZ��D[�D[��D\  D\}qD]  D]� D]�qD^}qD_  D_��D`D`� Da�Da��Db  Db��Db�qDc}qDc�qDd}qDe  De� Df  Df� Dg  Dg��Dg�qDhz�Dh��DixRDi��Dj}qDk�Dkz�Dk�RDlz�Dl�qDm��DnDn��Do�Do� Dp  Dp}qDq  Dq}qDr  Drz�Ds  Ds��Ds�qDt� Dt�qDu}qDv�Dv�Dw  Dw� Dx�Dx�Dy�Dyz�Dy�qDz� D{�D{� D|  D|� D|�qD}}qD~  D~��D~��Dz�D�  D�>�D�� D�� D�  D�AHD��HD�� D���D�@ D�~�D��)D��qD�>�D�� D�� D�  D�>�D�� D���D���D�AHD��HD�� D�HD�@ D�}qD���D�  D�@ D�~�D��HD��D�C�D���D��HD���D�@ D�� D���D���D�AHD�~�D���D�  D�>�D�~�D�� D�  D�@ D�~�D���D�  D�AHD���D�� D���D�@ D�|)D���D�HD�@ D�~�D��HD�HD�>�D�� D��HD�  D�>�D�� D���D���D�@ D���D��HD�HD�@ D�� D�D��D�@ D�~�D�� D�  D�AHD�}qD���D�  D�@ D�~�D��HD�  D�@ D���D�� D��qD�>�D�� D�� D��D�AHD�� D��HD�HD�AHD��HD�� D���D�>�D�� D�D���D�>�D�� D�� D�HD�AHD���D�� D��qD�=qD�� D���D��)D�>�D��HD�� D�  D�AHD�}qD���D�  D�>�D��HD��HD�  D�AHD�� D��HD�  D�>�D��HD�� D���D�@ D��HD�D�HD�@ D�� D��qD���D�>�D�~�D���D�HD�B�D�~�D���D�  D�AHD�~�D�� D�  D�>�D�~�D��qD��qD�@ D�~�D���D���D�@ D�� D�� D���D�>�D��HD�D�  D�>�D�� D���D��qD�>�D�� D��HD��D�B�D�� D��HD�HD�>�D�� D���D��D�B�D���D��HD�HD�AHD�� D��qD��qD�>�D�}qD��qD���D�>�D�~�D�� D���D�@ D��HD�� D���D�@ D�~�D�� D���D�@ D�� D���D���D�@ D�� D�� D�  D�>�D��HD��HD�  D�@ D�� D��HD�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�B�D��HD��HD�HD�B�D��HD�� D��qD�>�D�� D�� D���D�AHD��HD�� D���D�=qD�}qD�� D�HD�@ D D��HD���D�@ DÁHD��HD���D�@ DāHD�� D�HD�@ D�~�Dž�D�HD�AHDƀ D�� D���D�@ DǁHDǾ�D���D�@ DȀ D�� D�HD�B�Dɂ�D��HD�HD�B�DʁHD��HD��qD�=qD�}qD˾�D���D�=qD̀ D��HD�HD�@ D�~�D�� D�  D�AHD΁HD��HD��D�AHDπ DϽqD��qD�>�DЀ Dо�D�HD�AHDр D��HD��D�AHD҂�D�D���D�>�DӀ D��HD���D�>�DԀ D�� D���D�@ DՁHD��HD�HD�AHD�~�D־�D�  D�AHDׁHD��HD�  D�@ D؁HD��HD�  D�@ Dـ D�� D�  D�@ D�}qDڼ)D��qD�>�D�~�D�� D�  D�AHD܂�D�� D�  D�>�D�}qD�� D��D�>�D�~�D޾�D�  D�AHD߀ D�� D�  D�=qD�~�D�� D���D�>�D� D�� D���D�AHD�HD�� D���D�@ D�HD�� D�  D�@ D�HD侸D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�HD�� D�HD�@ D�HD�� D�  D�AHD�HD��HD�HD�@ DꂏD��HD�  D�AHD� D��HD��D�@ D�~�D쾸D���D�@ D� D�� D�HD�@ D�~�D��HD���D�>�D�~�D�� D�HD�B�D���D�D�  D�@ D� D�D���D�@ D� D�� D�HD�B�D� D�qD���D�>�D� D�� D�HD�AHD�� D�� D�  D�AHD��HD�� D�  D�8R?�?8Q�?u?���?�33?�(�?��H@
=q@#�
@+�@:�H@O\)@^�R@h��@�G�@�ff@���@�
=@�  @��@�{@�
=@��H@��@�{@�33@�(�@��@�=q@��@�p�AG�AffA
=qAp�A�\A
=A=qA�RA#�
A'
=A+�A0  A5�A8Q�A<��AB�\AEAI��AO\)AR�\AVffA\(�A`  Ab�\Ag�Amp�Ao\)Au�Az=qA|��A���A��A�p�A�
=A�G�A��
A�p�A��A�=qA��
A�ffA���A�=qA���A�\)A���A�33A�A��A�G�A��
A�ffA��A��A�z�A�{A�  A�=qA���A�A�Q�A��HA�z�AƸRA�G�A��HA���A�\)A��AӅA�Aأ�Aڏ\A�(�A�
=A���A��HA��A�A��A�A�{A��A�=qA�(�A��RA�G�A��HA��A��B ��B��B
=B(�B��B=qB�B��B	��B
�HB(�B�B=qB�BQ�Bp�B�RB  B��BB33Bz�Bp�BffB�
B��BB33B (�B!�B"ffB#�B$��B%��B&�HB((�B)G�B*{B+
=B,z�B-B.�RB/�B0��B2{B3\)B4(�B5G�B6�\B7�
B8��B9�B;33B<z�B=p�B>ffB@  BA�BA�BC33BD��BE�BF�RBG�
BIG�BJffBK\)BLz�BN{BO33BP(�BQG�BR�RBT  BT��BU�BW�BX��BYp�BZ�RB\  B]G�B^ffB_33B`z�Ba�Bb�HBc�
BeG�Bf�\Bg�Bhz�Bi�Bk33Bl  Bl��BnffBo�Bp��Bq��Bs33Bt(�Bu�Bv�\Bw�
Bx��ByB{33B|Q�B}�B~=qB�B�ffB���B��B�(�B��\B�
=B��B�Q�B��HB�G�B�B�z�B�
=B�\)B�  B���B��B��B�{B��RB�33B���B�=qB��HB�G�B�B�ffB���B��B��B��\B�33B��B�{B���B�33B��B�ffB���B�G�B�  B���B�
=B��B�=qB���B�33B�B�ffB�
=B��B�  B�ffB���B���B�{B�z�B��B�B�{B��\B��B�B�(�B��\B�33B�B�=qB���B�33B��
B�Q�B��RB�G�B��B�ffB���B�G�B��B�z�B��HB�\)B�B�ffB���B��B��
B�Q�B��HB��B��B�Q�B���B�\)B��B�ffB���B�p�B�  B��\B���B�\)B�  B���B�33B���B�{B��RB�\)B��
B�=qB���B�p�B�  B�ffB��HB��B�{B��\B�
=B�p�B�{B��RB�G�B���B�(�B���B�p�B��
B�=qB��HBŅB�{B�ffB���B�p�B�{Bȏ\B�
=B�p�B�  Bʏ\B��B˅B�  B̏\B��BͮB�{BΣ�B�33B��
B�ffB��HB�G�B��
B�z�B�33B�B�(�BԸRB�p�B��B�z�B��HB�p�B�{Bأ�B�
=Bٙ�B�Q�B���B�G�B�B�=qB���B݅B�{B�z�B���Bߙ�B�=qB���B�G�B�B�=qB��HB�p�B�(�B�RB��B噚B�Q�B���B癚B�{B�\B�
=B�B�ffB���B�p�B�  B�ffB�
=B�B�Q�B��HB�G�B��B��\B�33B�B�=qB���B�\)B�  B���B�G�B�B�=qB���B�p�B�{B���B�33B���B�(�B���B���B�{B��\B��B�B�ffB���B�p�B��C 33C �C �
C(�Cp�C�C�C(�Cz�CC�CffC��C�HC�CffC�RC  CQ�C�\C�
C
=CG�C��C�C=qCp�C�C�C=qC�C�
C	�C	\)C	��C	�
C
�C
ffC
�C  CG�C�\C�
C
=CG�C�C�
C�Cp�C�RC��C33CffC��C�C33Cz�CC��C33Cp�CC
=C\)C��C�HC�C\)C�C��C=qCz�C�C��C33Cz�C��C�CffC�C�C33Cz�C�
C33C�C��C{C\)C��C�HC33C�\C�
C�Cz�CC  CQ�C�\C�
C(�C�C��C�C\)C��C�C33C�C�HC33C�C��C 
=C \)C �C!  C!\)C!�C"  C"=qC"�C"��C#
=C#\)C#C${C$\)C$��C$��C%=qC%�C%�
C&(�C&p�C&C'�C'ffC'C(
=C(\)C(��C(�C)=qC)�C)�HC*33C*�\C*�HC+=qC+�\C+�
C,�C,ffC,�RC-{C-ffC-C.�C.z�C.�RC/
=C/\)C/��C/��C0Q�C0�C1
=C1ffC1�C2  C2G�C2��C2�C3G�C3��C3��C4G�C4��C4��C5=qC5�C5��C6{C6ffC6�RC7{C7ffC7C8{C8p�C8C9{C9ffC9�RC:  C:Q�C:��C:��C;=qC;��C;��C<Q�C<�C=  C=Q�C=��C=��C>G�C>��C>�C?G�C?��C@
=C@\)C@�RCA
=CA\)CA�CB  CBQ�CB��CB�CC=qCC�\CC��CDG�CD��CD��CEG�CE��CE��CFQ�CF��CF�CG=qCG�\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                ?�  ?��H@=p�@�  @�  @�  @�  A   A\)A   A+�A?\)A`��A�Q�A��A�  A���A�  A�\)A߮A�A�\)B�B  B  B   B(  B0  B8  B?�
BH  BP  BW�B`  BhQ�Bp(�Bw�
B�
B�  B�  B�  B�  B�  B�  B�  B��
B��
B��B�  B�  B��B��B��B�  B�  B�  B�{B�  B�{B�{B�  B��B�  B�  B��
B��
B��
B�  B�{C   C  C��C��C  C	��C  C
=C��C  C  C��C  C  C��C��C   C"{C$
=C&  C(
=C)��C,  C.  C/�C2  C4
=C5��C8  C:  C;��C=��C@  CB  CD
=CF
=CG��CI��CL
=CM��CO��CQ��CS�CV  CX
=CZ  C\  C^  C_��Ca��Cd
=Cf
=Ch
=Cj
=Cl{Cn
=Cp  Cr
=Ct  Cv  Cx
=Cz  C|  C~
=C�  C�  C�  C���C���C�  C�  C�  C�C�
=C�
=C�C�
=C���C�  C�  C���C�  C�C�C�  C���C���C�  C���C���C�C���C�C�  C�  C�
=C�  C�  C�C�C�  C���C���C�C�  C���C�  C�  C���C�  C�C���C���C���C���C�C�C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C�  C�C�C���C���C���C���C���C�C�C���C���C�  C�
=C�  C���C�  C�  C�C�
=C�C���C���C���C�  C�  C���C�  C�  C�  C�C�  C�  C���C���C���C�  C���C���C���C���C���C�
=C�
=C���C���C�  C�  C�C�  C���C�  C�  C�C�  C�C�
=C�C���C���C���C���C���D }qD  D� D  D}qD��Dz�D�qD� D�qD}qD  D��D�D�D  D}qD	  D	��D
�D
� D�D��D�qDz�D  D��D�qDz�D  D��D�qD� D  D� D  D� D  D� D  D� D�D��D  D� DD��D  D� DD��D�qD� D�qDz�D�qD� D  D� D�qDz�D�qD� D �D ��D!�D!}qD!��D"}qD"�qD#� D$�D$� D%  D%� D%��D&}qD&��D'}qD(�D(�D)  D)��D*  D*}qD*�qD+� D+�qD,z�D,��D-��D.D.��D/�D/�D0�D0}qD1  D1��D1�qD2}qD3  D3� D3�qD4� D5�D5� D6  D6}qD6�qD7� D8  D8��D9�D9}qD:  D:��D;�D;� D<  D<��D=�D=��D>�D>�D?�D?� D@  D@� DA  DA� DB  DB� DB�qDC��DD  DD� DE  DE� DF  DF� DG�DG}qDH  DH��DI�DI� DJ�DJ� DJ�qDK� DL�DL��DL�qDM}qDN�DN��DO�DO��DP  DP}qDP�qDQ}qDR�DR��DR�qDS� DT  DT� DU  DU}qDV  DV�DW�DW� DW��DXxRDX�RDY}qDZDZ��D[�D[��D\  D\}qD]  D]� D]�qD^}qD_  D_��D`D`� Da�Da��Db  Db��Db�qDc}qDc�qDd}qDe  De� Df  Df� Dg  Dg��Dg�qDhz�Dh��DixRDi��Dj}qDk�Dkz�Dk�RDlz�Dl�qDm��DnDn��Do�Do� Dp  Dp}qDq  Dq}qDr  Drz�Ds  Ds��Ds�qDt� Dt�qDu}qDv�Dv�Dw  Dw� Dx�Dx�Dy�Dyz�Dy�qDz� D{�D{� D|  D|� D|�qD}}qD~  D~��D~��Dz�D�  D�>�D�� D�� D�  D�AHD��HD�� D���D�@ D�~�D��)D��qD�>�D�� D�� D�  D�>�D�� D���D���D�AHD��HD�� D�HD�@ D�}qD���D�  D�@ D�~�D��HD��D�C�D���D��HD���D�@ D�� D���D���D�AHD�~�D���D�  D�>�D�~�D�� D�  D�@ D�~�D���D�  D�AHD���D�� D���D�@ D�|)D���D�HD�@ D�~�D��HD�HD�>�D�� D��HD�  D�>�D�� D���D���D�@ D���D��HD�HD�@ D�� D�D��D�@ D�~�D�� D�  D�AHD�}qD���D�  D�@ D�~�D��HD�  D�@ D���D�� D��qD�>�D�� D�� D��D�AHD�� D��HD�HD�AHD��HD�� D���D�>�D�� D�D���D�>�D�� D�� D�HD�AHD���D�� D��qD�=qD�� D���D��)D�>�D��HD�� D�  D�AHD�}qD���D�  D�>�D��HD��HD�  D�AHD�� D��HD�  D�>�D��HD�� D���D�@ D��HD�D�HD�@ D�� D��qD���D�>�D�~�D���D�HD�B�D�~�D���D�  D�AHD�~�D�� D�  D�>�D�~�D��qD��qD�@ D�~�D���D���D�@ D�� D�� D���D�>�D��HD�D�  D�>�D�� D���D��qD�>�D�� D��HD��D�B�D�� D��HD�HD�>�D�� D���D��D�B�D���D��HD�HD�AHD�� D��qD��qD�>�D�}qD��qD���D�>�D�~�D�� D���D�@ D��HD�� D���D�@ D�~�D�� D���D�@ D�� D���D���D�@ D�� D�� D�  D�>�D��HD��HD�  D�@ D�� D��HD�HD�@ D�~�D�� D�HD�@ D�� D��HD�HD�B�D��HD��HD�HD�B�D��HD�� D��qD�>�D�� D�� D���D�AHD��HD�� D���D�=qD�}qD�� D�HD�@ D D��HD���D�@ DÁHD��HD���D�@ DāHD�� D�HD�@ D�~�Dž�D�HD�AHDƀ D�� D���D�@ DǁHDǾ�D���D�@ DȀ D�� D�HD�B�Dɂ�D��HD�HD�B�DʁHD��HD��qD�=qD�}qD˾�D���D�=qD̀ D��HD�HD�@ D�~�D�� D�  D�AHD΁HD��HD��D�AHDπ DϽqD��qD�>�DЀ Dо�D�HD�AHDр D��HD��D�AHD҂�D�D���D�>�DӀ D��HD���D�>�DԀ D�� D���D�@ DՁHD��HD�HD�AHD�~�D־�D�  D�AHDׁHD��HD�  D�@ D؁HD��HD�  D�@ Dـ D�� D�  D�@ D�}qDڼ)D��qD�>�D�~�D�� D�  D�AHD܂�D�� D�  D�>�D�}qD�� D��D�>�D�~�D޾�D�  D�AHD߀ D�� D�  D�=qD�~�D�� D���D�>�D� D�� D���D�AHD�HD�� D���D�@ D�HD�� D�  D�@ D�HD侸D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�HD�� D�HD�@ D�HD�� D�  D�AHD�HD��HD�HD�@ DꂏD��HD�  D�AHD� D��HD��D�@ D�~�D쾸D���D�@ D� D�� D�HD�@ D�~�D��HD���D�>�D�~�D�� D�HD�B�D���D�D�  D�@ D� D�D���D�@ D� D�� D�HD�B�D� D�qD���D�>�D� D�� D�HD�AHD�� D�� D�  D�AHD��HD�� D�  G�O�?�?8Q�?u?���?�33?�(�?��H@
=q@#�
@+�@:�H@O\)@^�R@h��@�G�@�ff@���@�
=@�  @��@�{@�
=@��H@��@�{@�33@�(�@��@�=q@��@�p�AG�AffA
=qAp�A�\A
=A=qA�RA#�
A'
=A+�A0  A5�A8Q�A<��AB�\AEAI��AO\)AR�\AVffA\(�A`  Ab�\Ag�Amp�Ao\)Au�Az=qA|��A���A��A�p�A�
=A�G�A��
A�p�A��A�=qA��
A�ffA���A�=qA���A�\)A���A�33A�A��A�G�A��
A�ffA��A��A�z�A�{A�  A�=qA���A�A�Q�A��HA�z�AƸRA�G�A��HA���A�\)A��AӅA�Aأ�Aڏ\A�(�A�
=A���A��HA��A�A��A�A�{A��A�=qA�(�A��RA�G�A��HA��A��B ��B��B
=B(�B��B=qB�B��B	��B
�HB(�B�B=qB�BQ�Bp�B�RB  B��BB33Bz�Bp�BffB�
B��BB33B (�B!�B"ffB#�B$��B%��B&�HB((�B)G�B*{B+
=B,z�B-B.�RB/�B0��B2{B3\)B4(�B5G�B6�\B7�
B8��B9�B;33B<z�B=p�B>ffB@  BA�BA�BC33BD��BE�BF�RBG�
BIG�BJffBK\)BLz�BN{BO33BP(�BQG�BR�RBT  BT��BU�BW�BX��BYp�BZ�RB\  B]G�B^ffB_33B`z�Ba�Bb�HBc�
BeG�Bf�\Bg�Bhz�Bi�Bk33Bl  Bl��BnffBo�Bp��Bq��Bs33Bt(�Bu�Bv�\Bw�
Bx��ByB{33B|Q�B}�B~=qB�B�ffB���B��B�(�B��\B�
=B��B�Q�B��HB�G�B�B�z�B�
=B�\)B�  B���B��B��B�{B��RB�33B���B�=qB��HB�G�B�B�ffB���B��B��B��\B�33B��B�{B���B�33B��B�ffB���B�G�B�  B���B�
=B��B�=qB���B�33B�B�ffB�
=B��B�  B�ffB���B���B�{B�z�B��B�B�{B��\B��B�B�(�B��\B�33B�B�=qB���B�33B��
B�Q�B��RB�G�B��B�ffB���B�G�B��B�z�B��HB�\)B�B�ffB���B��B��
B�Q�B��HB��B��B�Q�B���B�\)B��B�ffB���B�p�B�  B��\B���B�\)B�  B���B�33B���B�{B��RB�\)B��
B�=qB���B�p�B�  B�ffB��HB��B�{B��\B�
=B�p�B�{B��RB�G�B���B�(�B���B�p�B��
B�=qB��HBŅB�{B�ffB���B�p�B�{Bȏ\B�
=B�p�B�  Bʏ\B��B˅B�  B̏\B��BͮB�{BΣ�B�33B��
B�ffB��HB�G�B��
B�z�B�33B�B�(�BԸRB�p�B��B�z�B��HB�p�B�{Bأ�B�
=Bٙ�B�Q�B���B�G�B�B�=qB���B݅B�{B�z�B���Bߙ�B�=qB���B�G�B�B�=qB��HB�p�B�(�B�RB��B噚B�Q�B���B癚B�{B�\B�
=B�B�ffB���B�p�B�  B�ffB�
=B�B�Q�B��HB�G�B��B��\B�33B�B�=qB���B�\)B�  B���B�G�B�B�=qB���B�p�B�{B���B�33B���B�(�B���B���B�{B��\B��B�B�ffB���B�p�B��C 33C �C �
C(�Cp�C�C�C(�Cz�CC�CffC��C�HC�CffC�RC  CQ�C�\C�
C
=CG�C��C�C=qCp�C�C�C=qC�C�
C	�C	\)C	��C	�
C
�C
ffC
�C  CG�C�\C�
C
=CG�C�C�
C�Cp�C�RC��C33CffC��C�C33Cz�CC��C33Cp�CC
=C\)C��C�HC�C\)C�C��C=qCz�C�C��C33Cz�C��C�CffC�C�C33Cz�C�
C33C�C��C{C\)C��C�HC33C�\C�
C�Cz�CC  CQ�C�\C�
C(�C�C��C�C\)C��C�C33C�C�HC33C�C��C 
=C \)C �C!  C!\)C!�C"  C"=qC"�C"��C#
=C#\)C#C${C$\)C$��C$��C%=qC%�C%�
C&(�C&p�C&C'�C'ffC'C(
=C(\)C(��C(�C)=qC)�C)�HC*33C*�\C*�HC+=qC+�\C+�
C,�C,ffC,�RC-{C-ffC-C.�C.z�C.�RC/
=C/\)C/��C/��C0Q�C0�C1
=C1ffC1�C2  C2G�C2��C2�C3G�C3��C3��C4G�C4��C4��C5=qC5�C5��C6{C6ffC6�RC7{C7ffC7C8{C8p�C8C9{C9ffC9�RC:  C:Q�C:��C:��C;=qC;��C;��C<Q�C<�C=  C=Q�C=��C=��C>G�C>��C>�C?G�C?��C@
=C@\)C@�RCA
=CA\)CA�CB  CBQ�CB��CB�CC=qCC�\CC��CDG�CD��CD��CEG�CE��CE��CFQ�CF��CF�CG=qCG�\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ĜA�ĜAܾwAܶFAܙ�A�|�A�p�A�t�A�`BA�C�A�?}A�5?A�1'A�(�A��A��A��A��A��A��A�{A�{A�{A�{A�oA�oA�bA�bA�bA�bA�bA�bA�bA�bA�oA�bA�VA�bA�bA�bA�bA�bA�VA�A���A���A��yA���AۑhA��TA�  Aו�A�  A���A� �Aԝ�A�E�AҮA� �A��/A�`BA���AͰ!A�VA��A�%A� �A��A�I�Aǣ�A�VA���A��HA��A�9XA¾wA�$�A��^A��A��wA�ȴA��A�%A��-A�v�A��jA���A��jA��hA��A�hsA��9A���A�v�A���A���A��A�JA�%A���A��A���A�dZA�bNA�bNA��-A�^5A���A���A�VA�ȴA��A�  A���A���A��jA�VA�bNA�E�A�=qA�r�A�bA��DA��A�VA���A`BA{��Axv�Av��AtE�AqXAm��Am�Aix�Ab�A_�AZ�AX�AW7LAT�HAS�AO��AK��AI�AG+AE|�AD�\AC`BA@�A@$�A=�;A;hsA:�9A7�mA6��A5+A37LA2I�A2(�A1��A1`BA1&�A0Q�A/��A.v�A,��A+�TA+`BA+�A*��A*z�A* �A)�hA(ZA'�-A'%A&$�A&��A'��A(�yA(�A(E�A( �A($�A&v�A%��A%�#A#�
A ȴA;dA �9A#+A"�9A!"�A�-A �Av�A�/A�uA��A�9A=qA1AG�A
�A�A|�AK�A+A��An�A=qA��A��A�9AbA�PA/A�A �\A ZA E�@���@��@���@�K�@���@�E�@��7@��@�Q�@�;d@���@�?}@�+@���@���@���@��m@�+@��@�V@��@�&�@���@��@�1@�K�@��@�j@�A�@�t�@�$�@���@�@�9@畁@�?}@�D@�z�@��@��m@���@��
@�F@㝲@�(�@�A�@��@�%@�z�@�/@�&�@��@��@���@ߥ�@�;d@�+@���@ޗ�@��@�X@�Q�@���@ڧ�@ڟ�@�E�@�@��@؃@��@�dZ@��H@�V@�{@��@�@���@ա�@��@���@���@��`@ԛ�@� �@��
@ӍP@�S�@��@�ȴ@���@�hs@�%@ϕ�@�33@�-@̓u@�S�@ʟ�@ʇ+@ʰ!@�n�@ɲ-@�O�@�hs@�x�@�/@��@��/@���@���@�9X@Ǖ�@���@�~�@��@�O�@ļj@�z�@î@�"�@�
=@�@��y@�ȴ@�{@���@�p�@��@���@���@�I�@���@��w@���@��@���@�&�@��@�j@��@���@�+@�
=@�@���@��R@���@�^5@��^@�A�@���@�\)@�C�@�@���@�=q@��#@�`B@�z�@�1@���@�+@��@�J@�p�@�?}@��j@��@�r�@�1@�ƨ@���@�K�@��y@���@���@���@�^5@�^5@�-@�@�?}@��@�Z@�A�@� �@��
@�;d@���@���@���@�J@�p�@�O�@�O�@��@� �@�b@�1@��@��
@���@�\)@��y@���@�ff@�5?@�@��@��^@�p�@�&�@�%@��/@��u@�(�@��@�@��H@���@�ff@�5?@���@��-@�`B@��@�z�@�9X@��@�1@�  @���@���@�dZ@�\)@�"�@��\@�=q@���@��T@���@�`B@�G�@�&�@���@�z�@�A�@�b@��@�\)@���@�V@�5?@���@�V@�Q�@�ƨ@��w@���@�C�@��y@��\@�-@���@��-@���@�?}@���@��/@��@��@��w@�\)@�"�@�
=@��@�~�@�5?@�J@���@�X@�V@�%@���@��@��@��`@���@��@� �@���@�dZ@�"�@��@�M�@��#@��#@���@��@�&�@���@��9@��D@�r�@�Q�@�1'@�1@��m@��
@��@�S�@�o@��@���@�ff@�5?@�@���@�G�@�7L@��@���@�Q�@�  @��F@��@�;d@�+@�@���@���@��\@�E�@���@��#@��-@�`B@�?}@��@��@��@��/@��@��D@�z�@�j@�(�@��@��w@�\)@��@�ȴ@���@��@�%@��`@���@��@�j@�1'@�1@~��@~�R@~�y@
=@~��@~E�@~@}�@}�-@}V@|�@|�j@|9X@{�F@{@z��@z=q@yG�@x��@x��@x1'@w|�@w
=@v��@vff@vE�@u/@tj@t9X@s��@s33@r��@q��@q��@q��@q�7@qx�@q��@pĜ@o��@o�@o;d@n�y@nv�@nE�@n@m�T@m�h@m�@l�D@lZ@lI�@k�
@kt�@k"�@j�@j~�@j�@ihs@h��@h�9@hA�@g�@g�w@g\)@g+@g�@g�@f�y@f�R@fV@e�T@e�-@e�h@e`B@d��@dz�@dj@c��@cC�@b�H@b��@b�\@b^5@a��@ax�@ahs@a7L@`��@`��@`�u@`  @_l�@^��@^��@^v�@^E�@]@]/@\��@\j@\�@[ƨ@[o@Z��@Z�!@Z�\@Z~�@Zn�@ZJ@Y��@ZJ@ZJ@Y�#@Yx�@X�`@X�9@XbN@W�w@Wl�@WK�@V�@Vff@V@U�@U�T@U@U?}@T��@T�@S"�@S"�@R�@RM�@RM�@RJ@Q%@P �@O�P@N5?@N{@M�@M@M?}@L�D@K�m@Kƨ@KS�@K@J��@J-@J�@I�@IG�@H�u@HA�@G�;@G��@G�@E�T@E�@EV@Dj@D�@Cƨ@B��@B�@A��@A�^@Ax�@AG�@@Ĝ@@A�@@  @?�;@?�@?;d@>�y@>v�@>{@=�-@=p�@=/@<��@<Z@;��@;��@;S�@;"�@:��@:^5@9�#@97L@8Ĝ@8bN@81'@7��@7�P@7\)@7�@6��@6�@6E�@5@5`B@5�@4�j@4z�@4Z@3�
@333@3o@2��@2��@2�\@2�\@2n�@2-@1��@1�#@1�^@1��@1�^@1�@1��@1hs@1�@0��@0A�@/�P@/\)@/;d@/+@/;d@.ȴ@.v�@.5?@-�-@-/@,�@,�@,�D@,z�@,j@,Z@,9X@+ƨ@+33@*��@*n�@*�@)��@)��@)X@(��@(�u@(bN@(1'@(  @'�w@'�P@'|�@'|�@'|�@';d@'+@'+@'
=@&ȴ@&��@&��@&��@&��@&�+@&ff@&V@&$�@%�@%�-@%p�@$�/@$��@$z�@$Z@$(�@$(�@#��@#ƨ@#��@#C�@"�H@"��@"^5@"J@!�@!G�@!�@ �`@ Ĝ@ �9@ �u@ bN@  �@ b@�@��@�@l�@;d@��@��@{@�T@��@��@`B@�@��@�/@�j@�@�D@z�@(�@�m@�F@�@t�@S�@33@o@�H@��@~�@=q@J@�@�@�#@��@x�@7L@Ĝ@��@r�@bN@A�@b@�P@;d@�@�@
=@
=@��@�@v�@5?@{@��@��@`B@V@�@��@(�@��@�m@ƨ@��@dZ@C�@o@��@~�@n�@^5@=q@=q@-@�#@�7@X@�@�9@�u@�@r�@Q�@b@�@�;@��@�P@\)@K�@;d@
=@��@�@ȴ@�R@��@��@v�@$�@{@@�T@@�AܾwA�ĜA�ƨA�ĜA�ƨA���A���A���AܾwAܼjAܾwA���AܾwAܸRAܺ^AܶFAܬAܥ�Aܣ�Aܙ�A�p�A�z�A�v�A�z�AܓuA�x�A�l�A�ffA�l�A�r�A�z�A�n�A�z�A�~�A�bNA�I�A�I�A�A�A�A�A�G�A�E�A�?}A�;dA�7LA�1'A�33A�7LA�1'A�1'A�7LA�33A�1'A�33A�1'A�-A�1'A�33A�-A�1'A�/A�+A�&�A�$�A�"�A��A� �A�"�A��A� �A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA�oA��A�{A�bA�{A��A�{A�bA�{A��A�oA�oA��A�oA�bA��A��A�bA�{A��A�{A�bA�{A��A�oA�{A��A�oA�oA�{A��A�oA�VA�oA�{A�bA�bA�{A�oA�oA�{A�bA�VA�bA�oA�bA�JA�bA�bA�oA�VA�VA�oA�{A�oA�bA�VA�oA�bA�JA�JA�oA�VA�JA�VA�oA�oA�VA�VA�{A�{A�VA�VA�oA�oA�JA�VA�bA�oA�VA�VA�oA�oA�JA�bA�oA�{A�bA�VA�oA�oA�VA�VA�oA�oA�bA�VA�bA�{A�bA�VA�oA�{A�bA�VA�{A�oA�VA�VA�oA�oA�VA�VA�oA�bA�JA�VA�oA�VA�VA�oA�bA�VA�VA�oA�oA�JA�bA�{A�bA�JA�bA�oA�{A�VA�JA�bA�oA�JA�VA�{A�oA�VA�VA�oA�bA�VA�bA�{A�VA�VA�VA�oA�oA�JA�VA�oA�VA�JA�JA�VA�oA�bA�JA�JA�bA�oA�JA�  A�A���A���A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��TA��/A��HA��/A��#A��
A���A���A���A�ȴA�ȴA���A�ȴA�ĜAۼjAۑhAۅA�z�A�K�A�?}A� �A���A��A��;A���A���Aڴ9AڶFAڰ!Aڟ�AځA���A٬AٓuA�=qAز-A� �AבhA�ffA�VA�I�A�?}A�-A�{A�bA�{A�VA���A��mA��TA֛�A�=qA� �A��#Aէ�AՕ�A�r�A�ZA�A�A�9XA�5?A� �A��A�
=A�  A���A��yA���AԾwAԸRAԡ�A�x�A�9XA���AӋDA�t�A�C�A�;dA�=qA�oA���A��A��#A��A�ĜAҩ�AғuA�p�A�ZA�G�A�5?A�$�A��A�JA�
=A�A��A��A��HA��TA��/A���A���AѮAѥ�Aљ�A�r�A�C�A� �A�VA��;Aв-AЍPA�jAϏ\A�l�A�;dA��A��yA���A�x�A�z�A��
A̩�Ȁ\ÃA�r�A�dZA�XA�;dA�33A�-A�$�A��A�bA�%A���A˧�A˓uA�l�A�1'A���A��`A��;A��A�ĜAʺ^Aʕ�A�jA�K�A�ȴA�~�A�&�A�VA��A��HA���A���A���AȮAȑhA�x�A�^5A�K�A�5?A��A�%A��;AǬAǥ�Aǝ�AǓuAǋDAǅAǁA�z�A�p�A�bNA�K�A�5?A�(�A��A��A�  A��mAƓuAƏ\AƃA�Q�A�9XA�1A��HA���AŰ!Aŝ�Aŉ7A�l�A�XA�;dA��A��`A���A���AĬA�bNA�Q�A�C�A�&�A��A���Aá�A�VA��A°!A�~�A�r�A�VA�G�A�C�A�?}A�1'A�"�A��A�{A�{A�{A�{A�oA�
=A���A�ƨA��A���A��A���A��A�jA�^5A�VA�S�A�C�A�9XA�"�A���A��
A��!A�dZA�JA��/A�`BA�oA��;A���A�VA���A��A�VA�VA���A���A��A�XA�A�A�1'A�&�A��A��`A���A�VA��A��A�ȴA���A�bNA�bA��^A�S�A��
A��hA�A�A�n�A�1A��yA���A��wA��9A���A���A���A���A��+A�I�A��jA��A�I�A�5?A�$�A�bA��`A��-A�9XA��!A��A���A���A��7A�ZA�9XA�"�A�A���A��A��HA��A���A�ȴA���A�x�A�jA��A�ȴA��DA�ZA��`A��A�;dA��^A�ZA��A���A�p�A�(�A�1A��A��HA���A��DA��A���A���A�\)A��A��mA���A�5?A��wA�ffA�&�A���A��hA�\)A��A��^A�M�A�JA��A���A���A�r�A�M�A�"�A���A��A��RA���A�x�A�C�A�JA��TA�~�A�VA��A���A���A�\)A���A��A�bNA�A���A��A�z�A�A�A���A�~�A��7A��A��A��A��+A��7A��7A�~�A�1'A�A�33A���A�O�A�1A��yA��9A�p�A��A��+A��/A��A�33A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                A�ĜA�ĜAܾwAܶFAܙ�A�|�A�p�A�t�A�`BA�C�A�?}A�5?A�1'A�(�A��A��A��A��A��A��A�{A�{A�{A�{A�oA�oA�bA�bA�bA�bA�bA�bA�bA�bA�oA�bA�VA�bA�bA�bA�bA�bA�VA�A���A���A��yA���AۑhA��TA�  Aו�A�  A���A� �Aԝ�A�E�AҮA� �A��/A�`BA���AͰ!A�VA��A�%A� �A��A�I�Aǣ�A�VA���A��HA��A�9XA¾wA�$�A��^A��A��wA�ȴA��A�%A��-A�v�A��jA���A��jA��hA��A�hsA��9A���A�v�A���A���A��A�JA�%A���A��A���A�dZA�bNA�bNA��-A�^5A���A���A�VA�ȴA��A�  A���A���A��jA�VA�bNA�E�A�=qA�r�A�bA��DA��A�VA���A`BA{��Axv�Av��AtE�AqXAm��Am�Aix�Ab�A_�AZ�AX�AW7LAT�HAS�AO��AK��AI�AG+AE|�AD�\AC`BA@�A@$�A=�;A;hsA:�9A7�mA6��A5+A37LA2I�A2(�A1��A1`BA1&�A0Q�A/��A.v�A,��A+�TA+`BA+�A*��A*z�A* �A)�hA(ZA'�-A'%A&$�A&��A'��A(�yA(�A(E�A( �A($�A&v�A%��A%�#A#�
A ȴA;dA �9A#+A"�9A!"�A�-A �Av�A�/A�uA��A�9A=qA1AG�A
�A�A|�AK�A+A��An�A=qA��A��A�9AbA�PA/A�A �\A ZA E�@���@��@���@�K�@���@�E�@��7@��@�Q�@�;d@���@�?}@�+@���@���@���@��m@�+@��@�V@��@�&�@���@��@�1@�K�@��@�j@�A�@�t�@�$�@���@�@�9@畁@�?}@�D@�z�@��@��m@���@��
@�F@㝲@�(�@�A�@��@�%@�z�@�/@�&�@��@��@���@ߥ�@�;d@�+@���@ޗ�@��@�X@�Q�@���@ڧ�@ڟ�@�E�@�@��@؃@��@�dZ@��H@�V@�{@��@�@���@ա�@��@���@���@��`@ԛ�@� �@��
@ӍP@�S�@��@�ȴ@���@�hs@�%@ϕ�@�33@�-@̓u@�S�@ʟ�@ʇ+@ʰ!@�n�@ɲ-@�O�@�hs@�x�@�/@��@��/@���@���@�9X@Ǖ�@���@�~�@��@�O�@ļj@�z�@î@�"�@�
=@�@��y@�ȴ@�{@���@�p�@��@���@���@�I�@���@��w@���@��@���@�&�@��@�j@��@���@�+@�
=@�@���@��R@���@�^5@��^@�A�@���@�\)@�C�@�@���@�=q@��#@�`B@�z�@�1@���@�+@��@�J@�p�@�?}@��j@��@�r�@�1@�ƨ@���@�K�@��y@���@���@���@�^5@�^5@�-@�@�?}@��@�Z@�A�@� �@��
@�;d@���@���@���@�J@�p�@�O�@�O�@��@� �@�b@�1@��@��
@���@�\)@��y@���@�ff@�5?@�@��@��^@�p�@�&�@�%@��/@��u@�(�@��@�@��H@���@�ff@�5?@���@��-@�`B@��@�z�@�9X@��@�1@�  @���@���@�dZ@�\)@�"�@��\@�=q@���@��T@���@�`B@�G�@�&�@���@�z�@�A�@�b@��@�\)@���@�V@�5?@���@�V@�Q�@�ƨ@��w@���@�C�@��y@��\@�-@���@��-@���@�?}@���@��/@��@��@��w@�\)@�"�@�
=@��@�~�@�5?@�J@���@�X@�V@�%@���@��@��@��`@���@��@� �@���@�dZ@�"�@��@�M�@��#@��#@���@��@�&�@���@��9@��D@�r�@�Q�@�1'@�1@��m@��
@��@�S�@�o@��@���@�ff@�5?@�@���@�G�@�7L@��@���@�Q�@�  @��F@��@�;d@�+@�@���@���@��\@�E�@���@��#@��-@�`B@�?}@��@��@��@��/@��@��D@�z�@�j@�(�@��@��w@�\)@��@�ȴ@���@��@�%@��`@���@��@�j@�1'@�1@~��@~�R@~�y@
=@~��@~E�@~@}�@}�-@}V@|�@|�j@|9X@{�F@{@z��@z=q@yG�@x��@x��@x1'@w|�@w
=@v��@vff@vE�@u/@tj@t9X@s��@s33@r��@q��@q��@q��@q�7@qx�@q��@pĜ@o��@o�@o;d@n�y@nv�@nE�@n@m�T@m�h@m�@l�D@lZ@lI�@k�
@kt�@k"�@j�@j~�@j�@ihs@h��@h�9@hA�@g�@g�w@g\)@g+@g�@g�@f�y@f�R@fV@e�T@e�-@e�h@e`B@d��@dz�@dj@c��@cC�@b�H@b��@b�\@b^5@a��@ax�@ahs@a7L@`��@`��@`�u@`  @_l�@^��@^��@^v�@^E�@]@]/@\��@\j@\�@[ƨ@[o@Z��@Z�!@Z�\@Z~�@Zn�@ZJ@Y��@ZJ@ZJ@Y�#@Yx�@X�`@X�9@XbN@W�w@Wl�@WK�@V�@Vff@V@U�@U�T@U@U?}@T��@T�@S"�@S"�@R�@RM�@RM�@RJ@Q%@P �@O�P@N5?@N{@M�@M@M?}@L�D@K�m@Kƨ@KS�@K@J��@J-@J�@I�@IG�@H�u@HA�@G�;@G��@G�@E�T@E�@EV@Dj@D�@Cƨ@B��@B�@A��@A�^@Ax�@AG�@@Ĝ@@A�@@  @?�;@?�@?;d@>�y@>v�@>{@=�-@=p�@=/@<��@<Z@;��@;��@;S�@;"�@:��@:^5@9�#@97L@8Ĝ@8bN@81'@7��@7�P@7\)@7�@6��@6�@6E�@5@5`B@5�@4�j@4z�@4Z@3�
@333@3o@2��@2��@2�\@2�\@2n�@2-@1��@1�#@1�^@1��@1�^@1�@1��@1hs@1�@0��@0A�@/�P@/\)@/;d@/+@/;d@.ȴ@.v�@.5?@-�-@-/@,�@,�@,�D@,z�@,j@,Z@,9X@+ƨ@+33@*��@*n�@*�@)��@)��@)X@(��@(�u@(bN@(1'@(  @'�w@'�P@'|�@'|�@'|�@';d@'+@'+@'
=@&ȴ@&��@&��@&��@&��@&�+@&ff@&V@&$�@%�@%�-@%p�@$�/@$��@$z�@$Z@$(�@$(�@#��@#ƨ@#��@#C�@"�H@"��@"^5@"J@!�@!G�@!�@ �`@ Ĝ@ �9@ �u@ bN@  �@ b@�@��@�@l�@;d@��@��@{@�T@��@��@`B@�@��@�/@�j@�@�D@z�@(�@�m@�F@�@t�@S�@33@o@�H@��@~�@=q@J@�@�@�#@��@x�@7L@Ĝ@��@r�@bN@A�@b@�P@;d@�@�@
=@
=@��@�@v�@5?@{@��@��@`B@V@�@��@(�@��@�m@ƨ@��@dZ@C�@o@��@~�@n�@^5@=q@=q@-@�#@�7@X@�@�9@�u@�@r�@Q�@b@�@�;@��@�P@\)@K�@;d@
=@��@�@ȴ@�R@��@��@v�@$�@{@@�T@G�O�AܾwA�ĜA�ƨA�ĜA�ƨA���A���A���AܾwAܼjAܾwA���AܾwAܸRAܺ^AܶFAܬAܥ�Aܣ�Aܙ�A�p�A�z�A�v�A�z�AܓuA�x�A�l�A�ffA�l�A�r�A�z�A�n�A�z�A�~�A�bNA�I�A�I�A�A�A�A�A�G�A�E�A�?}A�;dA�7LA�1'A�33A�7LA�1'A�1'A�7LA�33A�1'A�33A�1'A�-A�1'A�33A�-A�1'A�/A�+A�&�A�$�A�"�A��A� �A�"�A��A� �A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA�oA��A�{A�bA�{A��A�{A�bA�{A��A�oA�oA��A�oA�bA��A��A�bA�{A��A�{A�bA�{A��A�oA�{A��A�oA�oA�{A��A�oA�VA�oA�{A�bA�bA�{A�oA�oA�{A�bA�VA�bA�oA�bA�JA�bA�bA�oA�VA�VA�oA�{A�oA�bA�VA�oA�bA�JA�JA�oA�VA�JA�VA�oA�oA�VA�VA�{A�{A�VA�VA�oA�oA�JA�VA�bA�oA�VA�VA�oA�oA�JA�bA�oA�{A�bA�VA�oA�oA�VA�VA�oA�oA�bA�VA�bA�{A�bA�VA�oA�{A�bA�VA�{A�oA�VA�VA�oA�oA�VA�VA�oA�bA�JA�VA�oA�VA�VA�oA�bA�VA�VA�oA�oA�JA�bA�{A�bA�JA�bA�oA�{A�VA�JA�bA�oA�JA�VA�{A�oA�VA�VA�oA�bA�VA�bA�{A�VA�VA�VA�oA�oA�JA�VA�oA�VA�JA�JA�VA�oA�bA�JA�JA�bA�oA�JA�  A�A���A���A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��TA��/A��HA��/A��#A��
A���A���A���A�ȴA�ȴA���A�ȴA�ĜAۼjAۑhAۅA�z�A�K�A�?}A� �A���A��A��;A���A���Aڴ9AڶFAڰ!Aڟ�AځA���A٬AٓuA�=qAز-A� �AבhA�ffA�VA�I�A�?}A�-A�{A�bA�{A�VA���A��mA��TA֛�A�=qA� �A��#Aէ�AՕ�A�r�A�ZA�A�A�9XA�5?A� �A��A�
=A�  A���A��yA���AԾwAԸRAԡ�A�x�A�9XA���AӋDA�t�A�C�A�;dA�=qA�oA���A��A��#A��A�ĜAҩ�AғuA�p�A�ZA�G�A�5?A�$�A��A�JA�
=A�A��A��A��HA��TA��/A���A���AѮAѥ�Aљ�A�r�A�C�A� �A�VA��;Aв-AЍPA�jAϏ\A�l�A�;dA��A��yA���A�x�A�z�A��
A̩�Ȁ\ÃA�r�A�dZA�XA�;dA�33A�-A�$�A��A�bA�%A���A˧�A˓uA�l�A�1'A���A��`A��;A��A�ĜAʺ^Aʕ�A�jA�K�A�ȴA�~�A�&�A�VA��A��HA���A���A���AȮAȑhA�x�A�^5A�K�A�5?A��A�%A��;AǬAǥ�Aǝ�AǓuAǋDAǅAǁA�z�A�p�A�bNA�K�A�5?A�(�A��A��A�  A��mAƓuAƏ\AƃA�Q�A�9XA�1A��HA���AŰ!Aŝ�Aŉ7A�l�A�XA�;dA��A��`A���A���AĬA�bNA�Q�A�C�A�&�A��A���Aá�A�VA��A°!A�~�A�r�A�VA�G�A�C�A�?}A�1'A�"�A��A�{A�{A�{A�{A�oA�
=A���A�ƨA��A���A��A���A��A�jA�^5A�VA�S�A�C�A�9XA�"�A���A��
A��!A�dZA�JA��/A�`BA�oA��;A���A�VA���A��A�VA�VA���A���A��A�XA�A�A�1'A�&�A��A��`A���A�VA��A��A�ȴA���A�bNA�bA��^A�S�A��
A��hA�A�A�n�A�1A��yA���A��wA��9A���A���A���A���A��+A�I�A��jA��A�I�A�5?A�$�A�bA��`A��-A�9XA��!A��A���A���A��7A�ZA�9XA�"�A�A���A��A��HA��A���A�ȴA���A�x�A�jA��A�ȴA��DA�ZA��`A��A�;dA��^A�ZA��A���A�p�A�(�A�1A��A��HA���A��DA��A���A���A�\)A��A��mA���A�5?A��wA�ffA�&�A���A��hA�\)A��A��^A�M�A�JA��A���A���A�r�A�M�A�"�A���A��A��RA���A�x�A�C�A�JA��TA�~�A�VA��A���A���A�\)A���A��A�bNA�A���A��A�z�A�A�A���A�~�A��7A��A��A��A��+A��7A��7A�~�A�1'A�A�33A���A�O�A�1A��yA��9A�p�A��A��+A��/A��A�33A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B>wB>�B>B>B@B<B>wB=B>wB<�B>wB<�B<�B<�B<�B<�B<�B<�B<�B<�B=B<�B<�B<�B<�B<�B=B<�B<�B=B<�B<�B=B<�B<�B<�B<�B<�B<�B<�B<�B<jB<�B<B:^B9�B8�B5�B0�B$BCB
�B
��B
�&B
�mB
�&B
�dB
�?B
��B
�-B
��B
B
��B
�B
�?B
��B
��B
��B
��B
�B
�B
��BB\BCB8�B>�BG�Bk�Bt�B��B�eB�UB��B҉B�vBԕB�WB�B��B��B��B��B�5B��B�B�#BʌB�gB��B�dB��B��B��B��B��B~�B~�BjKBF?B9XB&�B'�B#nB�B�B
�B
�,B
�B
�dB
ĜB
�B
��B
��B
��B
��B
�uB
y>B
\�B
O�B
@B
6FB
�B
�B
�B	�)B	ɆB	��B	��B	�B	��B	�B	|�B	c�B	Z�B	MB	GEB	?B	<�B	2aB	+kB	(�B	+B	MB	B	MB��B�rB�MB�AB�oB�cB��B��B�B�`B�B�WB��B�rB	�B	SB	B	�B��B��B�rB	 �B	�B	&LB	H�B	i�B	iDB	m�B	u�B	tTB	o B	xB	s�B	_�B	P�B	iB	�nB	��B	��B	��B	��B	T�B	@�B	*�B	OB	�B	 �B�cB��B	 �B	�B	 �B	;B	 iB	�B	�B	�B	�B	%B		�B	�B	hB	�B	�B	�B	~B	"hB	"�B	%�B	%B	&�B	,=B	1�B	5�B	7�B	9�B	9XB	7�B	=qB	A�B	A�B	A�B	EB	HKB	G�B	HB	J�B	N<B	MjB	LdB	S�B	Y�B	[�B	]�B	]dB	[�B	\�B	\�B	]/B	_;B	b�B	b�B	`B	b�B	gmB	l"B	t�B	z�B	|PB	~]B	�B	�+B	��B	��B	��B	�"B	��B	��B	��B	�B	��B	��B	�[B	��B	��B	�6B	�qB	�B	��B	�gB	�KB	ȀB	�6B	�B	�vB	�}B	ѷB	�TB	��B	ԕB	�gB	�
B	�#B	ܒB	��B	�vB	�B	��B	�B	�B	�B	�KB	��B	�B	�B	�B	�iB	�B	�;B	�B	��B	��B	��B	�B	�MB	�B	�`B	��B	��B	�fB	��B	��B
B
�B
�B
�B
�B
SB
%B
�B
_B
�B
	7B

	B
	�B
B
�B
PB
B
B
B
�B
�B
"B
�B
�B
�B
.B
4B
.B
�B
oB
�B
B
�B
MB
B
�B
�B
{B
�B
�B
�B
{B
�B
$B
+B
�B
B
{B
{B
B
B
MB
�B
�B
�B
�B
�B
�B
kB
_B
�B
_B
�B
7B
�B
kB
�B
�B
CB
�B
�B
!bB
"�B
"�B
#:B
$tB
$�B
#�B
#�B
%B
&�B
(XB
)*B
)*B
(�B
(XB
)_B
'�B
'RB
'�B
)�B
)�B
)�B
*�B
+B
+�B
,qB
,�B
.IB
.IB
-�B
-�B
.�B
.�B
0�B
0�B
0UB
0!B
/�B
0UB
0�B
0�B
/�B
/�B
0�B
0UB
0�B
1�B
3hB
5tB
6B
6zB
6FB
6�B
6�B
6�B
6�B
7�B
7�B
8�B
8�B
8�B
8�B
9�B
9�B
:*B
:�B
;0B
:�B
;�B
<6B
<jB
<�B
>B
=�B
?}B
?}B
?�B
AUB
A�B
B�B
A�B
A�B
B'B
B[B
C�B
C�B
DgB
DgB
D�B
E9B
E�B
F?B
FB
FB
GB
GEB
GzB
F�B
GB
HB
G�B
H�B
HKB
I�B
J#B
J�B
JXB
J�B
J�B
J�B
J�B
JXB
J�B
K�B
J�B
J�B
K)B
K)B
LdB
L0B
K�B
L0B
L0B
L�B
L�B
L�B
L�B
MB
MB
M6B
M�B
M�B
M�B
M�B
NpB
N�B
OBB
O�B
O�B
PHB
P�B
QNB
Q�B
Q�B
R B
S�B
T�B
U�B
U�B
U�B
VmB
VB
VmB
VmB
V�B
V�B
W
B
WsB
W
B
W?B
W�B
W�B
XEB
XyB
YB
ZQB
Z�B
Z�B
Z�B
[#B
[#B
Z�B
[WB
Z�B
[#B
Z�B
ZQB
[�B
[WB
[#B
[�B
[#B
[WB
Z�B
[�B
[WB
Z�B
[WB
^5B
^�B
^jB
^5B
^jB
`�B
bNB
a�B
b�B
b�B
cTB
c�B
d�B
dZB
d�B
cTB
d&B
d�B
e�B
e�B
f2B
e�B
e�B
g�B
gB
gB
g8B
h>B
g�B
gmB
ffB
f2B
f�B
f�B
f�B
iyB
h�B
h>B
hsB
jKB
jKB
jB
i�B
iyB
jKB
kQB
k�B
k�B
kQB
k�B
k�B
k�B
k�B
l�B
l"B
l�B
l�B
l�B
m]B
l�B
m]B
m�B
m�B
m]B
m]B
m�B
m�B
n/B
n/B
ncB
ncB
oiB
p;B
p;B
poB
p�B
pB
poB
p�B
rGB
r|B
r�B
rGB
r�B
sB
s�B
sMB
s�B
t�B
uZB
t�B
tTB
tTB
tTB
s�B
tB
v+B
u�B
uZB
u�B
u�B
uZB
v+B
wfB
wfB
wfB
x�B
xlB
x8B
x8B
x�B
y	B
y>B
y	B
y	B
yrB
y�B
zDB
z�B
{JB
{B
{B
{B
{�B
{B
{�B
{�B
{�B
{�B
|PB
|B
{�B
|PB
{�B
|B
{�B
|PB
|PB
|PB
|�B
}�B
~]B
~�B
~�B
~�B
cB
�B
� B
�B
�B
�4B
��B
�B
��B
�oB
�B
��B
��B
��B
�B
�;B
�;B
�uB
��B
��B
�B
�B
�GB
��B
�B
��B
�MB
�MB
��B
�B
��B
��B
��B
�YB
�YB
��B
�+B
��B
��B
��B
��B
�fB
��B
�7B
��B
�=B
�rB
��B
�B
�B
�DB
�xB
�xB
�xB
�JB
�~B
��B
�B
�PB
��B
��B
�VB
��B
��B
��B
��B
��B
��B
�.B
��B
�4B
�4B
�hB
��B
��B
�B
�B
��B
��B
��B
��B
�B
��B
�B
�SB
��B
�$B
��B
��B
�_B
�_B
�+B
�_B
�_B
��B
��B
�_B
��B
��B
�eB
��B
�eB
��B
��B
��B
�B
��B
��B
�	B
��B
��B
�kB
��B
��B
��B
��B
�	B
�=B
�=B
��B
��B
�CB
�B
�CB
�CB
�xB
��B
��B
�IB
�IB
�~B
�B
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
��B
��B
��B
��B
��B
�-B
�-B
�bB
��B
��B
�4B
��B
�4B
�hB
�hB
��B
��B
��B
��B
�nB
��B
�@B
�tB
�tB
�tB
��B
��B
��B
��B
�zB
�zB
��B
��B
��B
��B
��B
��B
�LB
��B
��B
��B
�RB
�RB
�B
�B
��B
��B
��B
�XB
��B
��B
��B
��B
��B
��B
��B
�0B
��B
�0B
�eB
�eB
�eB
��B
��B
�B
�B
�6B
�kB
��B
��B
�B
��B
�qB
��B
��B
��B
��B
��B
�IB
��B
��B
�B
�B
�B
��B
��B
�B
��B
��B
��B
��B
�'B
�[B
�[B
��B
��B
��B
�-B
�-B
��B
��B
��B
�3B
�hB
�hB
��B
�B
�B
�B
�B
�nB
��B
��B
��B
��B
��B
�B>�B>BB=�B>wB>�B=B@B@B=�B>wB>�B=qB=�B>wB;�B>�B=<B=�B=B?�BK^B;�B?BB'B5?B?}BA�B;dB?}B<�B>BB?�B8�B=BE9B<�B<B=�B=<B;0B<�B>wB<�B=qB=qB<�B;�B<�B<�B:�B<�B=�B<6B;�B=qB<�B;0B>B<jB<jB=�B>�B<B=B>B=B;�B=�B<�B<B>wB<jB<B>�B<6B<jB>BB<6B;�B<�B>B<�B;�B=�B=qB;�B<�B=�B>B;�B>BB=B;�B=qB<�B;�B<�B=�B<�B;�B=�B=qB;�B<�B=�B<B<jB=�B=qB;dB<�B>BB<�B;�B<�B>B<jB;�B=�B=qB;�B=qB>B<6B<jB>B=B;�B=B>B<6B;�B=�B=B;�B=qB=�B<�B;�B=B>B<�B;�B=qB=�B;dB<�B=<B<B=B>BB<�B;�B<�B>BB<�B<jB<B=qB>BB<jB;�B<jB=qB=�B<jB<jB=qB=qB<B<�B=�B=B<B<jB=�B=�B<B;�B=�B=<B;�B;�B>B=�B<�B;�B=qB=qB<B<6B=�B=B;�B;�B=<B=�B;�B<B=�B=�B<jB<B<�B=�B=qB;�B=B>B<jB;�B=<B>B<B<6B=�B>BB;�B;dB=<B=<B;�B<jB=�B=B;�B=<B=qB<6B;�B=qB=qB;�B<6B>B<�B;�B=B=�B<�B<B;�B=<B>B<jB;�B>B=<B;�B<jB>BB=<B;�B<�B=�B=B;�B=�B=�B=<B;�B;�B=qB<jB;dB<B=<B=�B=B;0B<6B=<B=�B<6B;dB<�B@�B:�B=�B;�B;dB:�B9$B9XB8�B;�B<6B8�B9�B:�B8�B9XB9�B;0B:�B8�B:^B9�B8�B9�B9XB7�B7�B5B8RB7LB6B3�B3�B5�B5B2�B1�B2�B3hB:�B.B-B4B)�B,B%�B#nB$@B#nB!bBOB�BB�B�B:^B�B�B�B	B'RB
��B
��B
�+B
��B
�B
�B
�B
�cB
��B
��B
�;B
�B
�KB
��B
�mB
�B
�B
�BB
ܒB
�dB
��B
ٴB
�
B
�mB
�?B
�mB
�9B
��B
҉B
��B
��B
бB
�BB
�NB
҉B
�B
֡B
خB
��B
�aB
ɺB
ʌB
�B
�tB
�EB
�B
��B
�zB
�mB
�KB
��B
�3B
��B
�-B
�3B
�3B
��B
B
��B
��B
��B
��B
��B
B
�-B
�gB
�tB
��B
B
�KB
��B
��B
��B
�B
��B
��B
��B
�BB
�wB
��B
��B
�B
��B
�UB
�B
��B
��B
��B
��B
�LB
�B
��B
��B
��B
�?B
�hB
��B
��B
��B
��B
��B
��B
��B
��B
�0B
��B
��B
��B
��B
��B
�BB
��B
�B
��B
�dB
бB
�dB
ҽB
�B
ϫB
�B
уB
՛B
خB
�9B
�aB
�mB
ٴB
�#B
�B
�|B
�NB
�HB
�B
�,B
�B
��B
��B
�fB
�
B
�B
��B
�B
�B
�B
�)B
�B
�B 4B
��B
��B
�cB
��B�B�B%B	7B�B
	B	�BBB�B�B B�B�BVB�B�BB�B�B,B.}B0UBB'B<�B;�B@B>�B>B<�B?B?HB?�B?�B?�B>wB<�B<B<B>BGzBM�BdZBqvBl�Bm]BkBl"BkBjBk�Bi�Bl�BsBrGBrB��B��B~]B��B�VB��B�xB��B��B��B��B�6B�IB�6B��B�'B��B�CB��B�=B�B�B�jB��B�RB�XB�$B�^B��B�B��B��B�XBԕB�B�mB�NB��B�BB�vB��B�<B�0B��B�B�
B�|B��B�EB��B�aBӏBخB�gB�"B�ZB iB�B�B��B�B�TB�,B�B�B�NB�B�`B��B��B�B�B��B��B�cB��B�iB��B�rB��B�]B�PB�B��B��B�TB�cB��B�B�B�B��B�B��B�2B�B�B�MB�B�JB��B�;B�B��B�B�B�B�B֡B��B�TB��B��B̘B�XB�BŢB�KB��B�[B�B�wB�aB�HB�[BB�BB��B�3B�mB�?B��B��B��B�9B�RB��B�3B��B��B�B��B�OB�B��B��B��B��B�UB�B�<B�B��B��B��B��B��B��B��B�B�oB��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021100601294920211006012949IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021101602004420211016020044QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021101602004420211016020044QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365320220126093653IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295520220204232955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295520220204232955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295520220204232955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                