CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-11-05T05:19:16Z creation; 2022-02-04T23:30:05Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  cP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ΀   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 2   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 9�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � XP   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � _�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ~�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ~�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20211105051916  20220204223519  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_187                 6810_008521_187                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @٠C��d@٠C��d11  @٠C�/�W@٠C�/�W@0&yg��t@0&yg��t�dY�����dY����11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @G�@��\@�G�@�  @�p�@�p�A\)A\)A,��A?\)A_\)A���A���A�  A�  A�  AϮA�  A�\)A��B(�B(�B  B   B'�
B0  B8(�B@(�BG�BO�
BX  B`  Bh  Bo�Bw�B�  B�  B�{B�{B�  B�{B�{B��
B��B��B�  B��B��B��B�  B�(�B�{B�{B�  B�{B�  B�  B�(�B�  B�  B�  B�  B�  B��B�  B�  B�  C {C  C  C��C��C

=C{C  C�C�C�HC��C  C
=C  C{C   C"
=C${C&  C(  C*  C,
=C.
=C0
=C2
=C4  C6  C8
=C:
=C;��C>  C@
=CA��CD  CF
=CH  CI��CK��CM��CO��CQ��CT{CV{CX  CZ  C\  C^  C`
=Cb
=Cd  Ce�Cg��Cj  Ck��Cm��Cp  Cr  Ct  Cv
=Cx  Cz
=C|
=C~  C�  C�  C�C�C�  C�  C�C�  C�  C���C�C���C���C���C�  C���C�  C�C�C���C�  C�  C�  C���C�  C�  C���C�  C�C�
=C���C�C���C�  C���C�C�C���C�  C�  C�  C�  C�  C�  C�C���C�  C�C���C�
=C�
=C���C���C�  C���C�  C���C�  C�C�C�C�  C�  C�  C���C�  C�  C�  C�  C�  C���C���C���C�  C�  C�
=C�C�C�  C�  C�C�C���C���C�  C�C�C�C�
=C�C�  C�  C���C�  C�  C���C�C�C�C�C�  C���C���C�  C�  C�
=C�  C���C���C���C�  C�C�C�
=C�C�C�  C���C�  C�  C�C�C�C�C�  C�  C���C���D   D �DD��D�D� D�D� D�qD}qD�qD��D�D� D  D� D�qDz�D�qD	�D
�D
��D�D� D�D�D�D��D  D� D�qDz�D��Dz�D  D�DD��DD��D  Dz�D�qD}qD�qD}qD�qD��D�D}qD��D� D  D}qD  D� D  D� D�qD� D  D� D�D� D�qD ��D �qD!z�D!�qD"}qD#�D#��D$  D$�D%�D%� D%�qD&� D&�qD'� D(�D(��D)  D)� D*  D*}qD+  D+��D,  D,}qD,�qD-� D.�D.� D/  D/��D0  D0��D1  D1� D1�qD2}qD2�qD3� D4�D4� D5  D5}qD5�qD6��D7�D7��D8�D8��D8�qD9z�D9�qD:��D;�D;}qD;��D<}qD=  D=}qD=�qD>}qD>�qD?��D@  D@� DA  DAz�DA�qDB�DC�DC� DD  DD}qDE�DE��DF�DF��DG�DG� DH  DH��DI  DI}qDI�qDJ� DK  DK��DL  DL� DM�DM}qDN  DN��DN�qDO��DP�DP� DP�qDQ� DR�DR�DS  DS� DT�DT� DT�qDUz�DU��DV}qDV�qDW� DX  DX� DX�qDY}qDY�qDZ� D[D[�D\�D\��D]�D]��D^�D^��D_�D_� D`  D`}qD`�qDa� Da�qDb� Db�qDc}qDd�Dd��De  De}qDf�Df}qDf�qDg��Dh  Dh}qDi  Di��Dj  Djz�Dj��Dk}qDk��Dlz�Dl�qDm� Dn  Dn}qDn�qDo� Dp�Dpz�Dq  Dq��Dr�Dr� Dr��Dsz�Dt  Dt��Du�Du��Du�qDv}qDw  Dw��Dx�Dx� Dy  Dy��Dz�Dz��D{�D{��D|  D|� D}  D}� D~  D~�D~�qD}qD�HD�>�D�� D�� D���D�@ D�~�D�� D�HD�@ D�~�D���D�  D�AHD�� D���D�  D�@ D�� D���D���D�>�D�~�D���D���D�>�D�~�D�� D���D�=qD�� D�� D���D�>�D��HD�� D�  D�@ D�~�D��qD�  D�B�D�~�D���D�  D�>�D�~�D�� D�HD�@ D��HD��HD�HD�C�D���D�� D�HD�AHD��HD���D��qD�>�D�� D�� D�  D�@ D��HD��HD�HD�AHD�� D�� D�HD�>�D�� D���D���D�@ D�~�D��qD�  D�@ D�}qD���D��D�@ D�|)D���D�  D�AHD���D�D���D�=qD�~�D���D�  D�>�D�}qD���D�  D�AHD��HD��HD�HD�AHD���D�D��D�B�D�� D���D�  D�>�D�~�D���D���D�@ D��HD��HD��D�AHD�~�D�� D�HD�@ D�� D��HD�  D�>�D�~�D���D�  D�AHD��HD��HD�HD�@ D�� D���D���D�>�D�~�D���D�  D�AHD�� D�� D�HD�AHD��HD���D��qD�>�D�� D��HD��D�@ D��HD�D�  D�AHD���D�D�HD�@ D�~�D��qD���D�@ D�� D�� D���D�>�D��HD�� D�  D�AHD��HD��HD�HD�C�D�~�D��qD�  D�>�D�� D�� D�  D�B�D���D�� D�  D�@ D�� D�� D�  D�B�D��HD��HD��D�AHD��HD�� D�  D�>�D�� D��HD�HD�@ D�� D�� D��D�@ D�� D�D��D�C�D���D�� D���D�AHD���D�� D��D�B�D���D��HD�  D�AHD�� D�� D�HD�B�D��HD�� D�  D�@ D�� D��HD�  D�@ D�� D��HD�HD�@ D��HD�� D���D�>�D��HD�D�  D�@ D��HD��HD�  D�=qD�}qD�� D�HD�@ D�� D�� D�HD�AHD D�� D���D�>�DÀ Dþ�D�  D�AHDāHD�D��D�B�DŁHD��HD�  D�@ DƁHD��HD�HD�AHDǁHD��HD�  D�@ DȁHD��HD�  D�>�D�~�D�� D�HD�@ Dʀ D�� D���D�>�Dˀ D˾�D���D�@ D̀ D�� D��D�@ D̀ D;�D���D�@ D�~�D�� D�  D�@ Dπ D��HD�HD�AHDЂ�D�� D��qD�>�D�}qD�� D�HD�AHDҀ DҽqD���D�@ D�~�D�� D�HD�@ D�~�DԾ�D�HD�AHDՁHDվ�D���D�>�D�}qD�� D�HD�@ DׁHD�D�HD�B�D؁HD��HD��D�B�DفHD��HD�  D�>�D�~�D�� D�HD�B�Dۃ�D���D��D�@ D܀ Dܾ�D���D�>�D�~�D�� D�  D�@ Dހ D��HD��D�@ D�~�D߾�D�HD�AHD�� D�qD�  D�AHD�HD��HD���D�@ D� D⾸D��qD�>�D�~�D㾸D�  D�@ D� D��HD���D�>�D� D徸D�  D�@ D� D�� D�  D�AHD�HD�� D���D�AHD� D�qD���D�@ D� D��HD�  D�@ DꂏD��HD�  D�>�D�}qD��HD��D�@ D� D��HD�  D�>�D� D�� D���D�@ D�HD�� D�  D�AHD� DﾸD�  D�@ D�}qD�� D�HD�@ D�HD�� D���D�AHD� D�D�HD�C�D�|)>�Q�>���?8Q�?�  ?��R?��?�@�@�R@=p�@G�@^�R@n{@��
@���@��@�G�@�=q@��@�(�@���@��@ٙ�@��@�33@��HAG�A��A��A��A
=A(�A\)A%�A,(�A0  A333A9��A?\)AB�\AG�AMp�AR�\AVffAZ�HAaG�AfffAi��Ao\)Au�Aw�A|��A���A��
A�p�A�  A��HA�p�A�\)A�=qA��A��RA���A��
A��RA���A��HA�A�Q�A��A��A�
=A�G�A�(�A��RA���A��HA�A���A\A�z�A�
=A�=qA��
A�ffA�G�A��
A�p�A�Q�A�33A��A�\)A�\A��A�\)A陚A���A�
=A���A��
A��RA���A��HA�B z�Bp�B�\B  BG�B=qB\)B��B
=qB
=BQ�BB
=B�
BG�B�RB�
B��B=qB�B��BB33Bz�Bp�B�\B (�B!�B"=qB#�
B$��B%�B'�B(��B)��B+
=B,z�B-p�B.�\B0  B1p�B2ffB3\)B4��B6=qB733B8Q�B9B;
=B<  B=p�B>�RB?�B@��BB=qBC�BD��BEBG33BHz�BIp�BJ�HBLQ�BMG�BN=qBP  BQ�BR{BS�BT��BU�BW33BX��BY�BZ�HB\(�B]B^�RB_�
Bap�Bb�RBc�Be�Bf�\Bg�Bh��Bj=qBk�Bl��BmBo\)Bp��BqBs33Bt��Bu��Bv�HBxz�By��Bz�\B|(�B}��B~�\B�B��\B�G�B��
B�ffB��B��
B�ffB��HB��B�Q�B���B�\)B�(�B���B�33B��
B���B�G�B�B�=qB�
=B�B�=qB���B�\)B�{B��RB��B��
B��\B�
=B���B�Q�B���B�\)B��B��RB�\)B��
B�ffB��B��
B�=qB���B��B�=qB���B�G�B�  B��RB�G�B�B��\B�33B�B�=qB�
=B�B�=qB���B�p�B�(�B��HB�\)B��
B�z�B�G�B��
B�Q�B���B�B�=qB���B�p�B�(�B���B�33B�  B��\B�
=B��B�Q�B�
=B���B�{B���B��B�{B��\B�G�B��B���B�33B��B�Q�B�
=B���B�{B���B�G�B��B��\B���B�p�B�{B���B�33B���B�=qB��HB�p�B��
B�z�B��B�B�(�Bƣ�B�G�B�  Bȏ\B���Bə�B�Q�B��HB�33B��
B̏\B��BͅB�(�B��HB�p�B��B�ffB��B��
B�Q�B��HBӅB�=qB���B�G�B��
B֣�B�33Bי�B�(�B���BمB��B�ffB��BۮB�{B܏\B�33B��
B�z�B���B�p�B��
B��\B�
=BᙚB�{B�RB�\)B��B�Q�B��HB�B�(�B�\B�
=B�B�ffB���B�p�B��B�z�B�33B��
B�z�B��HB�p�B�{B���B�p�B��B�ffB�
=B�B�Q�B�RB�p�B�{B��RB�
=B���B�=qB���B�p�B��
B�ffB��B�B�=qB��RB�\)B�{B��RB�G�B�B�Q�B���B��C {C \)C ��C ��CG�C��C�HC�CffCC{C\)C�\C�HC=qC�\CC
=C\)C�RC  C=qCz�C�
C(�Cz�C�C��CQ�C��C��C	33C	p�C	C
�C
p�C
C  CG�C��C  CG�C�\C��C(�C�CC  C\)C�RC
=CQ�C��C�HC=qC��C�C(�Cp�C�RC
=CffCC
=CQ�C��C�
C(�C�C�
C33C�CC
=CffC��C{C\)C��C  CQ�C�C  C=qCz�C��C(�Cz�C�C�C(�C�CC��C(�CffC�RC�C(�CG�Cz�C�RC�HC  C�CG�Cz�C��CC�
C��C�C=qCffCz�C�\C�RC��C 
=C �C =qC p�C ��C �C ��C ��C!(�C!Q�C!p�C!�C!��C!�HC"
=C"�C"=qC"p�C"��C"��C"�HC#  C#(�C#ffC#�C#��C#C#��C$(�C$G�C$\)C$�C$�C$�HC%  C%{C%=qC%ffC%��C%C%��C%�C&{C&G�C&p�C&�\C&��C&C&�C'�C'G�C'\)C'z�C'��C'C'�C({C(=qC(\)C(�C(��C(�C(��C(�C){C)Q�C)p�C)��C)�RC)��C)�C*
=C*=qC*ffC*�\C*C*�HC*��C+�C+G�C+�C+��C+��C+�HC,  C,=qC,ffC,��C,C,�
C,��C-(�C-\)C-�C-�C-C-�C.
=C.G�C.z�C.��C.�RC.�
C/
=C/33C/ffC/��C/�RC/�
C/��C0�C0Q�C0�C0�C0C0�C1
=C1G�C1z�C1��C1C1�HC2  C2(�C2Q�C2�C2�RC2�HC3  C3{C3=qC3p�C3��C3��C3��C4
=C433C4\)C4�C4�RC4�C5{C533C5\)C5z�C5��C5C6  C633C6ffC6�C6��C6��C6��C733C7ffC7�\C7�C7�
C8{C8G�C8�C8��C8C8��C9(�C9ffC9��C9C9�C:{C:G�C:�C:�RC:�C;
=C;33C;ffC;��C;�
C<
=C<(�C<\)C<��C<�
C=
=C=33C=\)C=�C=�RC=�C>(�C>ffC>�\C>�RC>�
C?
=C?=qC?z�C?�C?�C@{C@33C@\)C@�\C@C@��CA=qCAp�CA��CACA�CB�CBQ�CB�CB��CC
=CC33CC\)CC�CC�RCC�HCD�CD\)CD�\CDCD�CE{CE=qCEp�CE��CE�HCF{CFQ�CF�CF�CF�
CF��CG�CG\)CG��CG�
CH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                   ?u@   @G�@��\@�G�@�  @�p�@�p�A\)A\)A,��A?\)A_\)A���A���A�  A�  A�  AϮA�  A�\)A��B(�B(�B  B   B'�
B0  B8(�B@(�BG�BO�
BX  B`  Bh  Bo�Bw�B�  B�  B�{B�{B�  B�{B�{B��
B��B��B�  B��B��B��B�  B�(�B�{B�{B�  B�{B�  B�  B�(�B�  B�  B�  B�  B�  B��B�  B�  B�  C {C  C  C��C��C

=C{C  C�C�C�HC��C  C
=C  C{C   C"
=C${C&  C(  C*  C,
=C.
=C0
=C2
=C4  C6  C8
=C:
=C;��C>  C@
=CA��CD  CF
=CH  CI��CK��CM��CO��CQ��CT{CV{CX  CZ  C\  C^  C`
=Cb
=Cd  Ce�Cg��Cj  Ck��Cm��Cp  Cr  Ct  Cv
=Cx  Cz
=C|
=C~  C�  C�  C�C�C�  C�  C�C�  C�  C���C�C���C���C���C�  C���C�  C�C�C���C�  C�  C�  C���C�  C�  C���C�  C�C�
=C���C�C���C�  C���C�C�C���C�  C�  C�  C�  C�  C�  C�C���C�  C�C���C�
=C�
=C���C���C�  C���C�  C���C�  C�C�C�C�  C�  C�  C���C�  C�  C�  C�  C�  C���C���C���C�  C�  C�
=C�C�C�  C�  C�C�C���C���C�  C�C�C�C�
=C�C�  C�  C���C�  C�  C���C�C�C�C�C�  C���C���C�  C�  C�
=C�  C���C���C���C�  C�C�C�
=C�C�C�  C���C�  C�  C�C�C�C�C�  C�  C���C���D   D �DD��D�D� D�D� D�qD}qD�qD��D�D� D  D� D�qDz�D�qD	�D
�D
��D�D� D�D�D�D��D  D� D�qDz�D��Dz�D  D�DD��DD��D  Dz�D�qD}qD�qD}qD�qD��D�D}qD��D� D  D}qD  D� D  D� D�qD� D  D� D�D� D�qD ��D �qD!z�D!�qD"}qD#�D#��D$  D$�D%�D%� D%�qD&� D&�qD'� D(�D(��D)  D)� D*  D*}qD+  D+��D,  D,}qD,�qD-� D.�D.� D/  D/��D0  D0��D1  D1� D1�qD2}qD2�qD3� D4�D4� D5  D5}qD5�qD6��D7�D7��D8�D8��D8�qD9z�D9�qD:��D;�D;}qD;��D<}qD=  D=}qD=�qD>}qD>�qD?��D@  D@� DA  DAz�DA�qDB�DC�DC� DD  DD}qDE�DE��DF�DF��DG�DG� DH  DH��DI  DI}qDI�qDJ� DK  DK��DL  DL� DM�DM}qDN  DN��DN�qDO��DP�DP� DP�qDQ� DR�DR�DS  DS� DT�DT� DT�qDUz�DU��DV}qDV�qDW� DX  DX� DX�qDY}qDY�qDZ� D[D[�D\�D\��D]�D]��D^�D^��D_�D_� D`  D`}qD`�qDa� Da�qDb� Db�qDc}qDd�Dd��De  De}qDf�Df}qDf�qDg��Dh  Dh}qDi  Di��Dj  Djz�Dj��Dk}qDk��Dlz�Dl�qDm� Dn  Dn}qDn�qDo� Dp�Dpz�Dq  Dq��Dr�Dr� Dr��Dsz�Dt  Dt��Du�Du��Du�qDv}qDw  Dw��Dx�Dx� Dy  Dy��Dz�Dz��D{�D{��D|  D|� D}  D}� D~  D~�D~�qD}qD�HD�>�D�� D�� D���D�@ D�~�D�� D�HD�@ D�~�D���D�  D�AHD�� D���D�  D�@ D�� D���D���D�>�D�~�D���D���D�>�D�~�D�� D���D�=qD�� D�� D���D�>�D��HD�� D�  D�@ D�~�D��qD�  D�B�D�~�D���D�  D�>�D�~�D�� D�HD�@ D��HD��HD�HD�C�D���D�� D�HD�AHD��HD���D��qD�>�D�� D�� D�  D�@ D��HD��HD�HD�AHD�� D�� D�HD�>�D�� D���D���D�@ D�~�D��qD�  D�@ D�}qD���D��D�@ D�|)D���D�  D�AHD���D�D���D�=qD�~�D���D�  D�>�D�}qD���D�  D�AHD��HD��HD�HD�AHD���D�D��D�B�D�� D���D�  D�>�D�~�D���D���D�@ D��HD��HD��D�AHD�~�D�� D�HD�@ D�� D��HD�  D�>�D�~�D���D�  D�AHD��HD��HD�HD�@ D�� D���D���D�>�D�~�D���D�  D�AHD�� D�� D�HD�AHD��HD���D��qD�>�D�� D��HD��D�@ D��HD�D�  D�AHD���D�D�HD�@ D�~�D��qD���D�@ D�� D�� D���D�>�D��HD�� D�  D�AHD��HD��HD�HD�C�D�~�D��qD�  D�>�D�� D�� D�  D�B�D���D�� D�  D�@ D�� D�� D�  D�B�D��HD��HD��D�AHD��HD�� D�  D�>�D�� D��HD�HD�@ D�� D�� D��D�@ D�� D�D��D�C�D���D�� D���D�AHD���D�� D��D�B�D���D��HD�  D�AHD�� D�� D�HD�B�D��HD�� D�  D�@ D�� D��HD�  D�@ D�� D��HD�HD�@ D��HD�� D���D�>�D��HD�D�  D�@ D��HD��HD�  D�=qD�}qD�� D�HD�@ D�� D�� D�HD�AHD D�� D���D�>�DÀ Dþ�D�  D�AHDāHD�D��D�B�DŁHD��HD�  D�@ DƁHD��HD�HD�AHDǁHD��HD�  D�@ DȁHD��HD�  D�>�D�~�D�� D�HD�@ Dʀ D�� D���D�>�Dˀ D˾�D���D�@ D̀ D�� D��D�@ D̀ D;�D���D�@ D�~�D�� D�  D�@ Dπ D��HD�HD�AHDЂ�D�� D��qD�>�D�}qD�� D�HD�AHDҀ DҽqD���D�@ D�~�D�� D�HD�@ D�~�DԾ�D�HD�AHDՁHDվ�D���D�>�D�}qD�� D�HD�@ DׁHD�D�HD�B�D؁HD��HD��D�B�DفHD��HD�  D�>�D�~�D�� D�HD�B�Dۃ�D���D��D�@ D܀ Dܾ�D���D�>�D�~�D�� D�  D�@ Dހ D��HD��D�@ D�~�D߾�D�HD�AHD�� D�qD�  D�AHD�HD��HD���D�@ D� D⾸D��qD�>�D�~�D㾸D�  D�@ D� D��HD���D�>�D� D徸D�  D�@ D� D�� D�  D�AHD�HD�� D���D�AHD� D�qD���D�@ D� D��HD�  D�@ DꂏD��HD�  D�>�D�}qD��HD��D�@ D� D��HD�  D�>�D� D�� D���D�@ D�HD�� D�  D�AHD� DﾸD�  D�@ D�}qD�� D�HD�@ D�HD�� D���D�AHD� D�D�HD�C�G�O�>�Q�>���?8Q�?�  ?��R?��?�@�@�R@=p�@G�@^�R@n{@��
@���@��@�G�@�=q@��@�(�@���@��@ٙ�@��@�33@��HAG�A��A��A��A
=A(�A\)A%�A,(�A0  A333A9��A?\)AB�\AG�AMp�AR�\AVffAZ�HAaG�AfffAi��Ao\)Au�Aw�A|��A���A��
A�p�A�  A��HA�p�A�\)A�=qA��A��RA���A��
A��RA���A��HA�A�Q�A��A��A�
=A�G�A�(�A��RA���A��HA�A���A\A�z�A�
=A�=qA��
A�ffA�G�A��
A�p�A�Q�A�33A��A�\)A�\A��A�\)A陚A���A�
=A���A��
A��RA���A��HA�B z�Bp�B�\B  BG�B=qB\)B��B
=qB
=BQ�BB
=B�
BG�B�RB�
B��B=qB�B��BB33Bz�Bp�B�\B (�B!�B"=qB#�
B$��B%�B'�B(��B)��B+
=B,z�B-p�B.�\B0  B1p�B2ffB3\)B4��B6=qB733B8Q�B9B;
=B<  B=p�B>�RB?�B@��BB=qBC�BD��BEBG33BHz�BIp�BJ�HBLQ�BMG�BN=qBP  BQ�BR{BS�BT��BU�BW33BX��BY�BZ�HB\(�B]B^�RB_�
Bap�Bb�RBc�Be�Bf�\Bg�Bh��Bj=qBk�Bl��BmBo\)Bp��BqBs33Bt��Bu��Bv�HBxz�By��Bz�\B|(�B}��B~�\B�B��\B�G�B��
B�ffB��B��
B�ffB��HB��B�Q�B���B�\)B�(�B���B�33B��
B���B�G�B�B�=qB�
=B�B�=qB���B�\)B�{B��RB��B��
B��\B�
=B���B�Q�B���B�\)B��B��RB�\)B��
B�ffB��B��
B�=qB���B��B�=qB���B�G�B�  B��RB�G�B�B��\B�33B�B�=qB�
=B�B�=qB���B�p�B�(�B��HB�\)B��
B�z�B�G�B��
B�Q�B���B�B�=qB���B�p�B�(�B���B�33B�  B��\B�
=B��B�Q�B�
=B���B�{B���B��B�{B��\B�G�B��B���B�33B��B�Q�B�
=B���B�{B���B�G�B��B��\B���B�p�B�{B���B�33B���B�=qB��HB�p�B��
B�z�B��B�B�(�Bƣ�B�G�B�  Bȏ\B���Bə�B�Q�B��HB�33B��
B̏\B��BͅB�(�B��HB�p�B��B�ffB��B��
B�Q�B��HBӅB�=qB���B�G�B��
B֣�B�33Bי�B�(�B���BمB��B�ffB��BۮB�{B܏\B�33B��
B�z�B���B�p�B��
B��\B�
=BᙚB�{B�RB�\)B��B�Q�B��HB�B�(�B�\B�
=B�B�ffB���B�p�B��B�z�B�33B��
B�z�B��HB�p�B�{B���B�p�B��B�ffB�
=B�B�Q�B�RB�p�B�{B��RB�
=B���B�=qB���B�p�B��
B�ffB��B�B�=qB��RB�\)B�{B��RB�G�B�B�Q�B���B��C {C \)C ��C ��CG�C��C�HC�CffCC{C\)C�\C�HC=qC�\CC
=C\)C�RC  C=qCz�C�
C(�Cz�C�C��CQ�C��C��C	33C	p�C	C
�C
p�C
C  CG�C��C  CG�C�\C��C(�C�CC  C\)C�RC
=CQ�C��C�HC=qC��C�C(�Cp�C�RC
=CffCC
=CQ�C��C�
C(�C�C�
C33C�CC
=CffC��C{C\)C��C  CQ�C�C  C=qCz�C��C(�Cz�C�C�C(�C�CC��C(�CffC�RC�C(�CG�Cz�C�RC�HC  C�CG�Cz�C��CC�
C��C�C=qCffCz�C�\C�RC��C 
=C �C =qC p�C ��C �C ��C ��C!(�C!Q�C!p�C!�C!��C!�HC"
=C"�C"=qC"p�C"��C"��C"�HC#  C#(�C#ffC#�C#��C#C#��C$(�C$G�C$\)C$�C$�C$�HC%  C%{C%=qC%ffC%��C%C%��C%�C&{C&G�C&p�C&�\C&��C&C&�C'�C'G�C'\)C'z�C'��C'C'�C({C(=qC(\)C(�C(��C(�C(��C(�C){C)Q�C)p�C)��C)�RC)��C)�C*
=C*=qC*ffC*�\C*C*�HC*��C+�C+G�C+�C+��C+��C+�HC,  C,=qC,ffC,��C,C,�
C,��C-(�C-\)C-�C-�C-C-�C.
=C.G�C.z�C.��C.�RC.�
C/
=C/33C/ffC/��C/�RC/�
C/��C0�C0Q�C0�C0�C0C0�C1
=C1G�C1z�C1��C1C1�HC2  C2(�C2Q�C2�C2�RC2�HC3  C3{C3=qC3p�C3��C3��C3��C4
=C433C4\)C4�C4�RC4�C5{C533C5\)C5z�C5��C5C6  C633C6ffC6�C6��C6��C6��C733C7ffC7�\C7�C7�
C8{C8G�C8�C8��C8C8��C9(�C9ffC9��C9C9�C:{C:G�C:�C:�RC:�C;
=C;33C;ffC;��C;�
C<
=C<(�C<\)C<��C<�
C=
=C=33C=\)C=�C=�RC=�C>(�C>ffC>�\C>�RC>�
C?
=C?=qC?z�C?�C?�C@{C@33C@\)C@�\C@C@��CA=qCAp�CA��CACA�CB�CBQ�CB�CB��CC
=CC33CC\)CC�CC�RCC�HCD�CD\)CD�\CDCD�CE{CE=qCEp�CE��CE�HCF{CFQ�CF�CF�CF�
CF��CG�CG\)CG��CG�
CH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                   @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��;A��HA��`A��`A��mA��A���A���A���A���A�  A���A��A�ƨA؁A�~�A�v�A�n�A�hsA�bNA�ZA�VA�Q�A�Q�A�O�A�O�A�M�A�M�A�K�A�K�A�K�A�K�A�M�A�M�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�G�A�G�A�I�A�G�A�I�A�I�A�I�A�E�A�E�A�?}A�33A�{A���A�&�A��A�+A��#A�ffA�S�Aʟ�Aɡ�A�ZAơ�A��Aħ�A�  A��A��9A���A�l�A�9XA�A�(�A���A�"�A�C�A�
=A���A���A���A�XA�1'A�z�A�A��A�bA��uA��A��-A��-A�O�A��HA�/A�&�A�7LA�=qA��A� �A���A�1A��A�1A�r�A�=qA�{A��hA�7LA�7LA���A���A���A��hA��RA��^A��!A�dZA���A��jA��jA��A�A�A��A}��A{�#Ax�Aw�-AuhsAt�9Ar�/Ao�
Al��AkXAi
=Ag33Ae�-Aa��A_��A\9XAX�HAU�7AT�AQt�AO�mAO�AM�TAJ��AJ{AI��AHZAFQ�AD~�AB�DAAhsA?��A=��A=XA<�jA:�!A8(�A5��A5K�A5VA3t�A3�A2��A1��A/�PA.�A-p�A,�A,1A,�/A,�HA,jA+33A)�mA(VA&��A$��A#;dA"v�A!C�A ��A�FA�/A�\AA��A -A JA M�A �\A"ZA"�A"bA!�hA!7LA!�A��A|�A�A�A5?Ap�A33AhsAbA;dA��A��A��A?}A
ffAz�A(�A$�A�A1A�\A�mA�#A�hAS�A��An�A�hA��AA~�AbNA�FA+A �9A n�A A�@�@���@���@��u@�z�@�l�@���@��@���@��9@��
@��@�j@@�!@�M�@�E�@�@�\)@�t�@�S�@��@���@�j@�j@�o@��`@�ƨ@�v�@�ff@��T@�@���@�Q�@�C�@�v�@�+@�V@��D@�9@�M�@܃@�Z@�I�@�A�@�A�@�Q�@��@ڸR@�$�@�`B@�@���@���@ղ-@ա�@Ӯ@���@�Ĝ@� �@Ѓ@�p�@мj@�9X@ϝ�@θR@�ff@��@���@́@�x�@�@�v�@��@ύP@���@�5?@�x�@�  @˾w@�\)@ʸR@�5?@�hs@Ȭ@ǅ@���@ƸR@�^5@���@�/@ģ�@��;@���@�5?@�@��@�`B@���@�Z@�1@���@�S�@�ȴ@�-@�?}@�%@���@���@�z�@�9X@��w@��@�^5@���@�&�@��`@�Q�@�33@��+@�@�`B@��@�9X@�K�@���@�E�@��@��@�G�@�/@��@���@�I�@��@�
=@���@�@�?}@��@���@��`@���@�r�@�Q�@�ƨ@�dZ@�ȴ@�^5@�J@�@��7@�/@��u@�j@�I�@�(�@� �@�Q�@�9X@�b@�ƨ@��P@�S�@��@��y@��R@�^5@�{@���@�hs@��@��j@�A�@��;@��F@���@�C�@��@��@���@�M�@�-@�-@�J@��@�7L@�Ĝ@�z�@� �@��w@��P@�t�@�K�@��@�=q@��@��@�/@��@�z�@�9X@���@���@���@�t�@�S�@��y@���@��+@�E�@�J@��T@���@�V@��9@��@��@�1@��m@�|�@��H@��+@�~�@�ff@�J@��-@���@�`B@�G�@�7L@�/@��@��`@��j@�z�@�1'@���@��F@���@�l�@�o@���@���@�=q@���@��T@�@���@�hs@�O�@��@���@��u@�A�@�  @��F@��@�C�@��@�@��H@��!@�E�@��@���@�hs@�?}@�%@���@��@�r�@�A�@��@��m@���@���@�C�@�"�@���@��@�n�@�@���@���@�p�@�G�@���@�9X@�(�@��@��@�+@�ȴ@���@�n�@�$�@��T@��-@�p�@��@��@��j@�j@�A�@��;@��F@��@�C�@�o@�ȴ@��\@�=q@�J@���@�G�@�%@���@��/@���@�z�@�Z@�A�@� �@��@�@�P@�@~5?@}�-@}�@|j@{C�@z��@zn�@zJ@y�@zJ@z-@z=q@y��@xĜ@w�;@w��@w+@v5?@u�T@u��@t�j@tZ@t(�@s��@so@r�!@r^5@q�^@q7L@pĜ@p�@p �@o�@o\)@o
=@n�y@n�y@n�R@n5?@n@m@m?}@l�j@l�@kS�@j�H@jn�@j-@i��@i�7@ix�@i7L@h�9@hA�@g�;@g�w@gK�@f�y@fȴ@fff@f$�@e�@e�@d9X@c�m@b=q@`�9@`Q�@_��@_\)@_;d@^��@^�R@^{@]�@]��@]O�@\��@\�D@\Z@\(�@[ƨ@[�
@[�
@[dZ@["�@Z�!@Z~�@Y��@Y&�@X�9@Xb@W�;@W�@W+@W
=@V�R@VE�@U�T@U�-@U��@UO�@T�@TI�@T1@S��@S"�@R�@R�!@Q�@Q��@Q�7@Qx�@QX@Q�@P��@P�`@P��@P�9@P�u@PbN@Pb@O�@O��@OK�@O�@N��@N�y@N�R@NE�@M�T@M�@MV@L�j@L�D@Lz�@L9X@K�m@Kt�@K@J�!@Jn�@J=q@I�@I��@I��@I�^@IX@H��@H��@H�9@Hr�@G�;@GK�@F��@F�R@Fȴ@F��@E��@E�h@Ep�@E?}@D��@DZ@D�@C�m@Cƨ@C��@C��@C��@C��@C�@CdZ@C"�@B�\@BM�@A��@A��@Ahs@AX@A7L@@Ĝ@@bN@?�@?�w@?l�@>��@>�R@>v�@=�@=��@=/@<j@<1@;��@:�@:^5@:J@9�7@97L@8��@8r�@8b@7�;@7\)@6��@6�y@6ȴ@6��@6V@6$�@5�h@5V@4�j@4�D@3�F@3C�@2�@2�!@2~�@2J@1��@1&�@0Ĝ@0�9@0�u@0Q�@0 �@0  @/�;@/��@/|�@.�R@.{@-��@-�@-O�@-�@,�/@,�j@,Z@,�@+ƨ@+�F@+��@+t�@+o@*�@*�!@*�!@*M�@)�#@)��@)�^@)��@)hs@)%@(�u@(A�@(b@'��@'�@'��@'�P@';d@&�@&��@&5?@%@%�-@%�@%p�@%O�@%V@$��@$z�@$I�@$(�@#��@#��@#�@#�@#t�@#dZ@#dZ@#33@"��@"�\@"M�@!�@!��@!hs@!�@ ��@ �9@ �@ bN@ A�@ 1'@�@�@�P@|�@K�@��@��@E�@@�@��@�h@�h@`B@/@V@�j@z�@I�@�@�m@�
@��@S�@o@�@��@�!@��@~�@�@�@�^@��@hs@X@&�@%@��@��@��@��@�u@A�@  @�;@��@�w@|�@�@ȴ@�+@ff@5?@{@�T@��@�-@�h@p�@�@�j@�D@z�@I�@9X@�@�m@��@C�@o@�H@��@��@n�@M�@-@��@��@�^@��@�7@x�@hs@G�@�@��@��@bN@A�@�@�;@��@�w@��@�P@|�@\)@\)@�@�@ȴ@ȴ@�R@��@v�@ff@ff@ff@5?@�@��@�@�@p�@p�@O�A��mA��yA��HA��/A��;A��TA��TA��/A��HA��mA��TA��TA��`A��`A��TA��HA��yA��yA��`A��yA���A���A��A���A���A���A���A���A���A���A���A���A���A�  A�A���A���A�  A�  A���A���A�  A�  A���A��HA���A���A��A��`A��A��TA�ĜA�A���Aء�A؏\A؅A؃A�~�A�~�A؁A�z�A�x�A؃A�|�A؇+A؅A�|�A�x�A�t�A�x�A�v�A�r�A�t�A�x�A�r�A�n�A�p�A�p�A�l�A�jA�l�A�l�A�ffA�ffA�jA�hsA�bNA�dZA�dZA�`BA�`BA�bNA�`BA�\)A�ZA�^5A�\)A�XA�ZA�^5A�ZA�S�A�VA�XA�Q�A�O�A�VA�VA�Q�A�O�A�S�A�S�A�O�A�O�A�S�A�Q�A�O�A�Q�A�S�A�O�A�M�A�O�A�S�A�S�A�M�A�O�A�S�A�O�A�M�A�Q�A�M�A�M�A�Q�A�M�A�K�A�M�A�M�A�K�A�O�A�Q�A�M�A�K�A�M�A�O�A�K�A�G�A�K�A�M�A�K�A�I�A�K�A�M�A�I�A�K�A�M�A�I�A�I�A�K�A�O�A�K�A�I�A�M�A�M�A�I�A�K�A�O�A�K�A�I�A�O�A�M�A�I�A�M�A�O�A�K�A�K�A�M�A�M�A�K�A�K�A�O�A�K�A�K�A�M�A�O�A�I�A�K�A�M�A�K�A�I�A�K�A�O�A�K�A�I�A�K�A�O�A�I�A�K�A�O�A�I�A�I�A�O�A�M�A�I�A�M�A�M�A�K�A�I�A�I�A�O�A�K�A�I�A�M�A�O�A�M�A�I�A�K�A�O�A�K�A�I�A�K�A�K�A�E�A�E�A�I�A�K�A�G�A�E�A�K�A�K�A�I�A�E�A�E�A�I�A�I�A�E�A�G�A�I�A�E�A�E�A�I�A�K�A�E�A�E�A�I�A�K�A�E�A�E�A�K�A�K�A�E�A�E�A�I�A�K�A�I�A�E�A�I�A�K�A�I�A�E�A�I�A�M�A�G�A�G�A�K�A�K�A�I�A�E�A�G�A�I�A�K�A�I�A�E�A�E�A�I�A�G�A�C�A�E�A�K�A�G�A�C�A�E�A�I�A�E�A�E�A�G�A�E�A�A�A�?}A�A�A�E�A�?}A�;dA�?}A�?}A�7LA�5?A�5?A�7LA�9XA�/A�$�A�$�A�&�A�"�A�JA�
=A�
=A���A��A��mA��`A��TA��
A״9A�z�A�ZA���A�ƨA��TA�bNAӓuAҁA�1'Aщ7A�33A��A��
A�ĜAХ�AН�AЗ�AЃA�l�A�S�A��A�7LA�hsA�9XA�oA���A�ƨA�bNA��A���A̸RA�^5A�I�A�$�A��A���A��AˬA�`BA�$�A�JA�A��A��TA���Aʛ�Aʏ\Aʉ7A�~�A�S�A�+A���A���Aɟ�A�x�A�33A��A�%A��A�ƨAȓuA�5?A��
AǛ�A�C�A��AƮAƇ+AƁA�~�A�r�A�bNA�S�A�7LA�bA�ȴA�|�A�M�A�A��;AĲ-Ağ�AăA�r�A�t�A�n�A�hsA�dZA�I�A�JAÃA��A´9A�E�A��`A��#A�ƨA��FA���A�^5A�A��A���A�n�A�33A��A�A���A��A�A��jA��^A��A��A���A���A���A�O�A��;A���A��PA�z�A�S�A�"�A���A��jA��\A�ffA�A�A�%A���A���A�hsA�XA�C�A�(�A��A�{A�VA�A��A��A��A�A��^A��!A�z�A�VA�
=A��yA�~�A�n�A�dZA�VA�K�A�E�A�A�A�9XA�5?A�5?A�+A�{A�
=A���A��A��
A�ĜA��RA��A���A���A��DA�`BA��A�A�t�A�?}A�+A�{A�{A�
=A�1A���A���A���A���A��+A�n�A�ZA�A�A�$�A�VA��TA��^A�n�A��A�ƨA���A�S�A��A��A���A�p�A�33A��A���A���A��A��A��yA��;A���A�ȴA��9A��A��A��A���A���A��+A�t�A�l�A�VA�+A��A�A��A��/A��uA�`BA�=qA�=qA���A���A�VA��A��A�Q�A�7LA��A�
=A���A��HA���A���A�ĜA��^A��!A��!A���A���A���A���A��hA��\A��PA��DA��A��A��+A��DA��7A��+A��A��7A��PA��\A��DA��+A��A�z�A�v�A�x�A��A��A��7A��+A�|�A�\)A�;dA�
=A���A���A��A�l�A�VA�K�A�+A��A�VA�A���A���A�l�A�K�A�/A�{A�%A��A��-A��A���A��A�jA�M�A�  A��\A���A��-A�v�A�?}A��A��mA��^A���A�bNA�O�A�G�A�?}A�"�A�A���A��A��A��mA��`A��;A���A��hA�M�A�"�A��
A���A�t�A�ZA�G�A�&�A�
=A��mA���A�A��^A��A���A���A���A���A��\A��A�r�A�ffA�G�A��A�ffA�bA���A��7A�z�A�hsA�^5A�^5A�XA�Q�A�5?A�1A��yA��;A���A��RA���A�z�A���A�hsA�1'A�
=A��/A���A�hsA�\)A�Q�A�G�A�(�A�{A���A��yA��/A��RA���A�r�A�E�A�{A��A���A��RA��A���A�ffA�A��wA���A�t�A�VA�5?A�%A��A��TA��TA��A�A��!A���A��A�p�A�`BA�?}A�-A� �A�oA�  A��A��A���A���A��+A�^5A�1'A��A�bA���A��HA�ĜA���A�r�A� �A��jA���A��A�M�A� �A���A���A�^5A�=qA�oA���A��!A��A���A���A�z�A�G�A�+A���A���A���A�;dA���A�G�A��A�oA���A��
A��jA��RA���A�t�A�l�A�G�A�A�A�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                   A��;A��HA��`A��`A��mA��A���A���A���A���A�  A���A��A�ƨA؁A�~�A�v�A�n�A�hsA�bNA�ZA�VA�Q�A�Q�A�O�A�O�A�M�A�M�A�K�A�K�A�K�A�K�A�M�A�M�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�G�A�G�A�I�A�G�A�I�A�I�A�I�A�E�A�E�A�?}A�33A�{A���A�&�A��A�+A��#A�ffA�S�Aʟ�Aɡ�A�ZAơ�A��Aħ�A�  A��A��9A���A�l�A�9XA�A�(�A���A�"�A�C�A�
=A���A���A���A�XA�1'A�z�A�A��A�bA��uA��A��-A��-A�O�A��HA�/A�&�A�7LA�=qA��A� �A���A�1A��A�1A�r�A�=qA�{A��hA�7LA�7LA���A���A���A��hA��RA��^A��!A�dZA���A��jA��jA��A�A�A��A}��A{�#Ax�Aw�-AuhsAt�9Ar�/Ao�
Al��AkXAi
=Ag33Ae�-Aa��A_��A\9XAX�HAU�7AT�AQt�AO�mAO�AM�TAJ��AJ{AI��AHZAFQ�AD~�AB�DAAhsA?��A=��A=XA<�jA:�!A8(�A5��A5K�A5VA3t�A3�A2��A1��A/�PA.�A-p�A,�A,1A,�/A,�HA,jA+33A)�mA(VA&��A$��A#;dA"v�A!C�A ��A�FA�/A�\AA��A -A JA M�A �\A"ZA"�A"bA!�hA!7LA!�A��A|�A�A�A5?Ap�A33AhsAbA;dA��A��A��A?}A
ffAz�A(�A$�A�A1A�\A�mA�#A�hAS�A��An�A�hA��AA~�AbNA�FA+A �9A n�A A�@�@���@���@��u@�z�@�l�@���@��@���@��9@��
@��@�j@@�!@�M�@�E�@�@�\)@�t�@�S�@��@���@�j@�j@�o@��`@�ƨ@�v�@�ff@��T@�@���@�Q�@�C�@�v�@�+@�V@��D@�9@�M�@܃@�Z@�I�@�A�@�A�@�Q�@��@ڸR@�$�@�`B@�@���@���@ղ-@ա�@Ӯ@���@�Ĝ@� �@Ѓ@�p�@мj@�9X@ϝ�@θR@�ff@��@���@́@�x�@�@�v�@��@ύP@���@�5?@�x�@�  @˾w@�\)@ʸR@�5?@�hs@Ȭ@ǅ@���@ƸR@�^5@���@�/@ģ�@��;@���@�5?@�@��@�`B@���@�Z@�1@���@�S�@�ȴ@�-@�?}@�%@���@���@�z�@�9X@��w@��@�^5@���@�&�@��`@�Q�@�33@��+@�@�`B@��@�9X@�K�@���@�E�@��@��@�G�@�/@��@���@�I�@��@�
=@���@�@�?}@��@���@��`@���@�r�@�Q�@�ƨ@�dZ@�ȴ@�^5@�J@�@��7@�/@��u@�j@�I�@�(�@� �@�Q�@�9X@�b@�ƨ@��P@�S�@��@��y@��R@�^5@�{@���@�hs@��@��j@�A�@��;@��F@���@�C�@��@��@���@�M�@�-@�-@�J@��@�7L@�Ĝ@�z�@� �@��w@��P@�t�@�K�@��@�=q@��@��@�/@��@�z�@�9X@���@���@���@�t�@�S�@��y@���@��+@�E�@�J@��T@���@�V@��9@��@��@�1@��m@�|�@��H@��+@�~�@�ff@�J@��-@���@�`B@�G�@�7L@�/@��@��`@��j@�z�@�1'@���@��F@���@�l�@�o@���@���@�=q@���@��T@�@���@�hs@�O�@��@���@��u@�A�@�  @��F@��@�C�@��@�@��H@��!@�E�@��@���@�hs@�?}@�%@���@��@�r�@�A�@��@��m@���@���@�C�@�"�@���@��@�n�@�@���@���@�p�@�G�@���@�9X@�(�@��@��@�+@�ȴ@���@�n�@�$�@��T@��-@�p�@��@��@��j@�j@�A�@��;@��F@��@�C�@�o@�ȴ@��\@�=q@�J@���@�G�@�%@���@��/@���@�z�@�Z@�A�@� �@��@�@�P@�@~5?@}�-@}�@|j@{C�@z��@zn�@zJ@y�@zJ@z-@z=q@y��@xĜ@w�;@w��@w+@v5?@u�T@u��@t�j@tZ@t(�@s��@so@r�!@r^5@q�^@q7L@pĜ@p�@p �@o�@o\)@o
=@n�y@n�y@n�R@n5?@n@m@m?}@l�j@l�@kS�@j�H@jn�@j-@i��@i�7@ix�@i7L@h�9@hA�@g�;@g�w@gK�@f�y@fȴ@fff@f$�@e�@e�@d9X@c�m@b=q@`�9@`Q�@_��@_\)@_;d@^��@^�R@^{@]�@]��@]O�@\��@\�D@\Z@\(�@[ƨ@[�
@[�
@[dZ@["�@Z�!@Z~�@Y��@Y&�@X�9@Xb@W�;@W�@W+@W
=@V�R@VE�@U�T@U�-@U��@UO�@T�@TI�@T1@S��@S"�@R�@R�!@Q�@Q��@Q�7@Qx�@QX@Q�@P��@P�`@P��@P�9@P�u@PbN@Pb@O�@O��@OK�@O�@N��@N�y@N�R@NE�@M�T@M�@MV@L�j@L�D@Lz�@L9X@K�m@Kt�@K@J�!@Jn�@J=q@I�@I��@I��@I�^@IX@H��@H��@H�9@Hr�@G�;@GK�@F��@F�R@Fȴ@F��@E��@E�h@Ep�@E?}@D��@DZ@D�@C�m@Cƨ@C��@C��@C��@C��@C�@CdZ@C"�@B�\@BM�@A��@A��@Ahs@AX@A7L@@Ĝ@@bN@?�@?�w@?l�@>��@>�R@>v�@=�@=��@=/@<j@<1@;��@:�@:^5@:J@9�7@97L@8��@8r�@8b@7�;@7\)@6��@6�y@6ȴ@6��@6V@6$�@5�h@5V@4�j@4�D@3�F@3C�@2�@2�!@2~�@2J@1��@1&�@0Ĝ@0�9@0�u@0Q�@0 �@0  @/�;@/��@/|�@.�R@.{@-��@-�@-O�@-�@,�/@,�j@,Z@,�@+ƨ@+�F@+��@+t�@+o@*�@*�!@*�!@*M�@)�#@)��@)�^@)��@)hs@)%@(�u@(A�@(b@'��@'�@'��@'�P@';d@&�@&��@&5?@%@%�-@%�@%p�@%O�@%V@$��@$z�@$I�@$(�@#��@#��@#�@#�@#t�@#dZ@#dZ@#33@"��@"�\@"M�@!�@!��@!hs@!�@ ��@ �9@ �@ bN@ A�@ 1'@�@�@�P@|�@K�@��@��@E�@@�@��@�h@�h@`B@/@V@�j@z�@I�@�@�m@�
@��@S�@o@�@��@�!@��@~�@�@�@�^@��@hs@X@&�@%@��@��@��@��@�u@A�@  @�;@��@�w@|�@�@ȴ@�+@ff@5?@{@�T@��@�-@�h@p�@�@�j@�D@z�@I�@9X@�@�m@��@C�@o@�H@��@��@n�@M�@-@��@��@�^@��@�7@x�@hs@G�@�@��@��@bN@A�@�@�;@��@�w@��@�P@|�@\)@\)@�@�@ȴ@ȴ@�R@��@v�@ff@ff@ff@5?@�@��@�@�@p�@p�G�O�A��mA��yA��HA��/A��;A��TA��TA��/A��HA��mA��TA��TA��`A��`A��TA��HA��yA��yA��`A��yA���A���A��A���A���A���A���A���A���A���A���A���A���A�  A�A���A���A�  A�  A���A���A�  A�  A���A��HA���A���A��A��`A��A��TA�ĜA�A���Aء�A؏\A؅A؃A�~�A�~�A؁A�z�A�x�A؃A�|�A؇+A؅A�|�A�x�A�t�A�x�A�v�A�r�A�t�A�x�A�r�A�n�A�p�A�p�A�l�A�jA�l�A�l�A�ffA�ffA�jA�hsA�bNA�dZA�dZA�`BA�`BA�bNA�`BA�\)A�ZA�^5A�\)A�XA�ZA�^5A�ZA�S�A�VA�XA�Q�A�O�A�VA�VA�Q�A�O�A�S�A�S�A�O�A�O�A�S�A�Q�A�O�A�Q�A�S�A�O�A�M�A�O�A�S�A�S�A�M�A�O�A�S�A�O�A�M�A�Q�A�M�A�M�A�Q�A�M�A�K�A�M�A�M�A�K�A�O�A�Q�A�M�A�K�A�M�A�O�A�K�A�G�A�K�A�M�A�K�A�I�A�K�A�M�A�I�A�K�A�M�A�I�A�I�A�K�A�O�A�K�A�I�A�M�A�M�A�I�A�K�A�O�A�K�A�I�A�O�A�M�A�I�A�M�A�O�A�K�A�K�A�M�A�M�A�K�A�K�A�O�A�K�A�K�A�M�A�O�A�I�A�K�A�M�A�K�A�I�A�K�A�O�A�K�A�I�A�K�A�O�A�I�A�K�A�O�A�I�A�I�A�O�A�M�A�I�A�M�A�M�A�K�A�I�A�I�A�O�A�K�A�I�A�M�A�O�A�M�A�I�A�K�A�O�A�K�A�I�A�K�A�K�A�E�A�E�A�I�A�K�A�G�A�E�A�K�A�K�A�I�A�E�A�E�A�I�A�I�A�E�A�G�A�I�A�E�A�E�A�I�A�K�A�E�A�E�A�I�A�K�A�E�A�E�A�K�A�K�A�E�A�E�A�I�A�K�A�I�A�E�A�I�A�K�A�I�A�E�A�I�A�M�A�G�A�G�A�K�A�K�A�I�A�E�A�G�A�I�A�K�A�I�A�E�A�E�A�I�A�G�A�C�A�E�A�K�A�G�A�C�A�E�A�I�A�E�A�E�A�G�A�E�A�A�A�?}A�A�A�E�A�?}A�;dA�?}A�?}A�7LA�5?A�5?A�7LA�9XA�/A�$�A�$�A�&�A�"�A�JA�
=A�
=A���A��A��mA��`A��TA��
A״9A�z�A�ZA���A�ƨA��TA�bNAӓuAҁA�1'Aщ7A�33A��A��
A�ĜAХ�AН�AЗ�AЃA�l�A�S�A��A�7LA�hsA�9XA�oA���A�ƨA�bNA��A���A̸RA�^5A�I�A�$�A��A���A��AˬA�`BA�$�A�JA�A��A��TA���Aʛ�Aʏ\Aʉ7A�~�A�S�A�+A���A���Aɟ�A�x�A�33A��A�%A��A�ƨAȓuA�5?A��
AǛ�A�C�A��AƮAƇ+AƁA�~�A�r�A�bNA�S�A�7LA�bA�ȴA�|�A�M�A�A��;AĲ-Ağ�AăA�r�A�t�A�n�A�hsA�dZA�I�A�JAÃA��A´9A�E�A��`A��#A�ƨA��FA���A�^5A�A��A���A�n�A�33A��A�A���A��A�A��jA��^A��A��A���A���A���A�O�A��;A���A��PA�z�A�S�A�"�A���A��jA��\A�ffA�A�A�%A���A���A�hsA�XA�C�A�(�A��A�{A�VA�A��A��A��A�A��^A��!A�z�A�VA�
=A��yA�~�A�n�A�dZA�VA�K�A�E�A�A�A�9XA�5?A�5?A�+A�{A�
=A���A��A��
A�ĜA��RA��A���A���A��DA�`BA��A�A�t�A�?}A�+A�{A�{A�
=A�1A���A���A���A���A��+A�n�A�ZA�A�A�$�A�VA��TA��^A�n�A��A�ƨA���A�S�A��A��A���A�p�A�33A��A���A���A��A��A��yA��;A���A�ȴA��9A��A��A��A���A���A��+A�t�A�l�A�VA�+A��A�A��A��/A��uA�`BA�=qA�=qA���A���A�VA��A��A�Q�A�7LA��A�
=A���A��HA���A���A�ĜA��^A��!A��!A���A���A���A���A��hA��\A��PA��DA��A��A��+A��DA��7A��+A��A��7A��PA��\A��DA��+A��A�z�A�v�A�x�A��A��A��7A��+A�|�A�\)A�;dA�
=A���A���A��A�l�A�VA�K�A�+A��A�VA�A���A���A�l�A�K�A�/A�{A�%A��A��-A��A���A��A�jA�M�A�  A��\A���A��-A�v�A�?}A��A��mA��^A���A�bNA�O�A�G�A�?}A�"�A�A���A��A��A��mA��`A��;A���A��hA�M�A�"�A��
A���A�t�A�ZA�G�A�&�A�
=A��mA���A�A��^A��A���A���A���A���A��\A��A�r�A�ffA�G�A��A�ffA�bA���A��7A�z�A�hsA�^5A�^5A�XA�Q�A�5?A�1A��yA��;A���A��RA���A�z�A���A�hsA�1'A�
=A��/A���A�hsA�\)A�Q�A�G�A�(�A�{A���A��yA��/A��RA���A�r�A�E�A�{A��A���A��RA��A���A�ffA�A��wA���A�t�A�VA�5?A�%A��A��TA��TA��A�A��!A���A��A�p�A�`BA�?}A�-A� �A�oA�  A��A��A���A���A��+A�^5A�1'A��A�bA���A��HA�ĜA���A�r�A� �A��jA���A��A�M�A� �A���A���A�^5A�=qA�oA���A��!A��A���A���A�z�A�G�A�+A���A���A���A�;dA���A�G�A��A�oA���A��
A��jA��RA���A�t�A�l�A�G�A�A�A�?}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                   ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�BeB�BeBeB�B7B�B7B7BeBBB�BSB�B�B�BB�B�B{BFBBFBBFBB�B�BBB�B�B�BB�B�BBBuB�B�BuB�B@B�B�B�BoB4B�B�B�B
�rB
ŢB
�B
��B
�aB
�$B
� B
��B
��B_B�B0�B:�BgBp�By>B��B�{B��B�wB��B��B��B��B��BȴB��B�B��BیB�B��B�ZB��B��B�"B��B��B�B��B�B�&B�?B�[B�[B�tB�IB��B�"B�BsMBh�BP}B=<B0�B!-B�B_B�B
��B
�%B
�yB
�fB
ںB
��B
ÖB
��B
��B
��B
��B
}VB
m)B
c�B
T�B
N�B
D�B
6�B
�B
$B
xB	�]B	�B	��B	�jB	�B	��B	��B	�	B	��B	u%B	qvB	m�B	aB	[#B	WsB	T�B	I�B	B�B	8�B	1�B	.�B	"4B	!B	�B	�B	"B	�B	oB	;B	AB	  B��B	AB	�B	B	�B	�B	�B	5?B	C�B	L�B	J�B	@�B	6B	%�B	 �B	�B	�B	@B	�B	(B	JB	.B	$�B	:�B	bB	`�B	hsB	rGB	��B	��B	�0B	�OB	�wB	��B	��B	��B	�{B	{�B	u�B	x�B	ffB	_;B	c�B	a�B	S[B	?�B	7�B	:*B	8RB	7�B	+B	 �B	B	5�B	>�B	`�B	t�B	{B	u�B	hsB	xB	�oB	�uB	��B	�kB	��B	�@B	��B	��B	�RB	�eB	��B	�XB	��B	��B	��B	�-B	��B	��B	�0B	�IB	��B	��B	�=B	��B	��B	�B	�tB	�HB	�B	ȴB	ʌB	�B	�B	�jB	�jB	�XB	��B	�'B	�aB	��B	�B	�mB	֡B	��B	��B	ϫB	�NB	�WB	�KB	�)B	ںB	бB	�BB	��B	��B	� B	��B	�B	�
B	�TB	�BB	�#B	��B	��B	� B	�?B	ȴB	��B	��B	�jB	��B	ɆB	�dB	͟B	˒B	��B	�B	ǮB	ȀB	�RB	��B	�aB	��B	�`B	��B	��B	�B	�]B	�cB	��B	��B	�"B	��B	��B	�B	�|B	��B	��B	��B	��B	��B	�VB	�.B	��B
 4B
  B
 4B
oB
uB
{B
�B
�B
YB
�B
�B
	7B
	�B
~B
.B
�B
�B
uB
B
�B
�B
�B
�B
�B
�B
_B
1B
eB
1B
�B
B
xB
xB
�B
VB
�B
�B
�B
 \B
!�B
!�B
"hB
!�B
 �B
�B
VB
�B
�B
!bB
 �B
 \B
!�B
!bB
!�B
"�B
#nB
$B
#B
$@B
$�B
%FB
&B
'B
(XB
-�B
-�B
.B
.�B
.�B
.�B
.�B
/B
/B
0!B
0�B
0�B
1[B
1�B
1�B
1�B
1'B
1�B
1�B
2�B
2�B
2�B
4B
4B
49B
5B
5tB
5tB
4�B
5�B
5�B
6B
6�B
6zB
6FB
6�B
7�B
7�B
7�B
8�B
9�B
:�B
:�B
;�B
<6B
<6B
<jB
<�B
=B
=�B
>B
>�B
>�B
@B
@B
?�B
@OB
@B
@�B
A B
A B
A�B
B'B
C-B
A�B
A�B
B[B
C�B
CaB
C�B
C�B
D3B
D3B
C�B
D�B
D�B
EB
E�B
E�B
E�B
F?B
F?B
GEB
HKB
G�B
G�B
H�B
G�B
HB
JXB
J�B
L0B
LdB
L�B
MjB
MjB
NB
NB
NB
M�B
N�B
NpB
NpB
N�B
N�B
O�B
OBB
O�B
P}B
P}B
P�B
P}B
PB
PB
PHB
P�B
QB
QB
Q�B
R�B
R�B
S&B
R�B
S[B
S&B
R�B
S[B
S�B
T,B
U2B
T�B
TaB
UgB
VB
V�B
V�B
V�B
V�B
W
B
W?B
W�B
XEB
X�B
YB
Y�B
Y�B
ZB
Z�B
[#B
[WB
[WB
\)B
\]B
[�B
[�B
[WB
[�B
\)B
[�B
[�B
[�B
\]B
\�B
\�B
]/B
]�B
^5B
^jB
_�B
aHB
`vB
a|B
a�B
a|B
_�B
`BB
`�B
`�B
aHB
c�B
e,B
e�B
f�B
ffB
gB
g8B
g8B
g�B
gB
f�B
f�B
ffB
h
B
hsB
h�B
i�B
j�B
kQB
k�B
l"B
l"B
l"B
l�B
l�B
m)B
m)B
l�B
l�B
m]B
l�B
m)B
m�B
n/B
n�B
n�B
ncB
ncB
n/B
n�B
m�B
m�B
m�B
n/B
o�B
p�B
p;B
p�B
qB
q�B
q�B
q�B
qvB
rB
r|B
q�B
sMB
qAB
p�B
p�B
qvB
qAB
q�B
rB
r�B
r|B
r�B
sB
sMB
sMB
sMB
s�B
tB
t�B
tTB
v+B
u�B
u�B
uZB
u�B
u�B
u�B
v+B
v+B
v`B
v�B
v�B
v�B
w2B
wfB
w�B
xB
x�B
y�B
y�B
y�B
y�B
y�B
zDB
z�B
{B
{B
{JB
{B
|B
{�B
|B
|PB
|PB
|�B
|�B
|�B
}VB
}"B
}"B
}�B
}�B
}�B
}�B
}�B
~�B
~]B
~�B
~�B
.B
cB
�B
�iB
��B
�B
��B
�iB
��B
��B
�oB
�;B
�oB
��B
��B
�B
��B
��B
�uB
��B
��B
��B
�GB
�{B
��B
��B
��B
��B
��B
�MB
�B
��B
��B
�+B
��B
��B
�_B
�_B
��B
�_B
��B
��B
�_B
��B
��B
�fB
��B
�lB
�lB
�B
�B
�B
�lB
�lB
�lB
��B
�rB
��B
�DB
�DB
�DB
�xB
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
�bB
��B
� B
��B
�B
��B
�:B
�:B
��B
�oB
�B
��B
��B
��B
��B
�{B
��B
�FB
�FB
�FB
�{B
�{B
�{B
�{B
�{B
��B
��B
��B
�SB
�SB
��B
��B
��B
�$B
��B
��B
�+B
�+B
�+B
�+B
��B
��B
��B
��B
�eB
��B
��B
�eB
�eB
��B
�7B
��B
�	B
�=B
�qB
��B
��B
�qB
�B
�CB
�CB
��B
�B
�B
�IB
�IB
�IB
��B
��B
��B
��B
�B
�B
�OB
�B
�B
�OB
�OB
�OB
��B
��B
�!B
�!B
��B
��B
��B
�\B
�'B
�\B
��B
��B
��B
��B
�-B
�-B
�bB
�bB
�bB
��B
�4B
�hB
�hB
�hB
��B
��B
��B
�B
��B
�nB
��B
��B
�B
�@B
�tB
�@B
�tB
��B
��B
�B
�B
�B
�B
�zB
��B
��B
�B
�B
��B
��B
��B
�B
�B
��B
�RB
�RB
�RB
��B
��B
��B
�$B
��B
��B
��B
�*B
�_B
�_B
�_B
�_B
��B
�0B
�0B
�0B
�eB
��B
�B
�6B
�6B
�kB
�kB
��B
��B
�=B
�qB
�qB
��B
��B
��B
�B
�CB
�wB
��B
�B
��B
��B
�B
��B
�B
�IB
�}B
��B
��B
��B
��B
�OB
��B
�B
�B
��B
��B
��B
��B
��B
�UB
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
�'B
��B
��B
��B
��B
��B
��BCBxB_B	B7B�BeB�B1B�B�B�B�B�B�B7B�B1BkB�B�BeB=B7B_BB	B1B�B	B1B�B	B�B1BB�B�B1B�BB�B�B=B�B�BeB�B�B�B�B7B�B�B&�B+B�BSBYB�BBYB�B@B�BMB�B{BMB�BFBMB�B�BFB�B�B�BFB�BBMBB�BB{B{B�B�BB�B�B�BB�BMB�BFB�B�BBBSB{B�B�BMBuB@B�B�B�BuB�B�B�B�BMB{BuB{B�BFB@BBB�BB{B�B@B�B�B@B�BB�BuBMBFB�B{B�BuBB�BBuB�BFB{BuB�B�B�BB�B�BuB�BFB�BB@B�B�B�BFB{BB�BMB�B�B�B�B@B@B�BBBFB�B@BuB�B�B@B�BB�B�BFB�B�B�B�B{B�B�B�BB@B�B�B�BFB�BB�BBMB�B�BuB�B{B@BFB�BB�B�B�BuB�BuB�BB�B�BBFB�BB�BuBoBFB�B�B�B{BFBB�B�B�B�B�B{B�BuBB�B�B@B�B@B�B@BoBBFBB�B�B{BFBoBBBFBuB�BoB�BuBhB�B�B�BhB�B@BhB:B�BBhB.B�BBbB�B BhBbB�B�B�B�B�B~BJB�B
�B
rB	lB	B�B�B�B�B�B
��B
��B
�.B
�B&B
��B
�GB
�B
�B
ԕB
�dB
�3B
��B
�jB
��B
�B
��B
�6B
��B
�$B
�tB
ŢB
՛B
�kB
��B
�LB
��B
�B
��B
��B
�9B
��B
��B
��B
�UB
��B
�B
��B
�dB
��B
��B
��B
��B
�BB
�B
�[B
��B
��B
�UB
��B
�EB
��B
��B
уB
ҽB
��B
ӏB
֡B
�WB
�
B
�B
�yB
��B
�|B
�.B1B�B	�B_B�B�B
=BB�BhB=B�B$B.IB-B49B0�B1�B2aB-�B.IB.�B-wB2�B:^BO�BPHB`BiDBiyBgBffBd�BcTBhsBqvBiBjBu%BuZBt�Bv�Bs�BzBz�B{B{JBy>By>Bw2Bw�Bw�B�7B��B�xB�4B�PB�B�FB��B��B�bB��B��B�XB�wB�B�B��B�wB�CB�'B�B��B��B��B�IB�hB��B�wB��B��B��B��B	�B�HBÖB��B�B�-B��B�'B��B�}B�}BB��B��B�B��B�B��B�aB��B��B�}B��B��B͟B�B��B�B��B�[B�B��B�UB�-B� BɆB�BƨB�B�9BƨB�mBŢB�XB̘B��B՛B֡BуB�B�[B�,BӏB�BߤB�jB�vBܒB�)BޞB��B�#BݘB��B�vB�B�/BیB��B�B�B��B�B�NB�yB�)B�B�/B��B�B�/B�>B�B�B��B;B_BoB�%B�fB�ZB�%B�vB�MB�B�oB��B��B�B� B�]B��B�/B��B�]B�B��B��B��B�oB�oB�5B�AB��B��B�B�B�MB��B��B��B�8B��B��B�]B 4BBB�B�BYB1B�BAB�B��B��B�AB��B�`B��B��BoB��B��B��B�rB�B��B�2B��B�B��B��B��B��B�B+B��B�B��B�ZB��B�lB�B��B�B�B�B�B��B�+B�AB�GB��B��B�/B��B�5B�%B�B��B�8B�2B�B� B��B�oB�B�B�B�WB�QB�B�"B��B�B�B�B��B�
B�2B��B�B��B��B�B�
B�>B��B�B�NB�B�B�B�,B�HB��BޞB�B�)B�HB�B��B�
B��BԕB՛BɺB�^B�BɆB͟B�aB��B�gB��B�zB��B��B�BB�B�^B�$B��B��B��B�LB�$B��B��B��B�B��B�*B�zB�hB�bB�nB�@B��B�:B��B�VB�\B��B��B�OB�B��B�	B�kB��B�	B�=B�xB��B�B�uB��B�B��B��B��B��B�B��B��B�"B��B�B��B�DB�+B�7B��B�B~�B~�B~]B�{B~�Bz�B}�Bw�B|�B�4B��Bw�BkQBl"Bq�Bl"Bk�Bh
BkBx�Bd�Bp;Bd&Ba|G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021110505191620211105051916IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021111505005920211115050059QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021111505005920211115050059QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365420220126093654IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295620220204232956IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295620220204232956IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295620220204232956IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                