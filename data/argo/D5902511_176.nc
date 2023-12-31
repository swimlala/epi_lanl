CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-07-17T17:23:31Z creation; 2022-02-04T23:30:02Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210717172331  20220204223515  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_176                 6810_008521_176                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ل����@ل����11  @ل�͞��@ل�͞��@2�6;%o�@2�6;%o��e䎊q��e䎊q�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�@E�@�G�@�G�@\@�G�@��RA�RA{A,(�A@  A`  A�  A�\)A��A���A���A�  A߮A�  B (�B  B�
B  B   B(  B/�B7�
B@(�BHQ�BP  BX  B`(�Bh(�Bo�
Bx  B�  B�  B��B�  B�  B��B��B��B��
B��
B��B�  B�{B�(�B�  B��B�  B�{B�(�B�  B�  B�{B�(�B�{B�{B�(�B�  B��B��B�  B�{B�  C   C��C��C�C�C	��C  C
=C  C�C  C  C��C��C��C  C��C!��C#��C&  C(
=C*{C,  C-��C/��C2  C4  C6
=C8  C:  C;��C=�C@  CB
=CD
=CF
=CG�CI��CK��CM�CO��CR  CS��CU��CX  CZ
=C\
=C]��C`
=Cb{Cd  Cf
=Ch  Cj  Ck��Cm��Co��Cq�Cs��Cu�Cx  Cz  C|  C}��C��C���C�C�C���C�  C���C�C�C�
=C�  C�C�  C�  C�  C���C�  C�C�C�  C�  C�C�C�  C�  C�
=C�C�  C�  C�  C�C�C���C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�  C�  C���C���C���C���C�  C�C�  C�  C�  C���C�  C�C���C���C�  C�C�  C�C�C�C���C�C�  C�
=C�C�C�
=C���C�  C�C�  C�C���C���C�  C�  C�  C���C���C�  C�C�C�  C�C���C���C���C�C�  C�  C���C���C�C�C�C�  C�  C�  C���C�  C�  C�C�  C�C�C�  C���C���C���C���C�  C���C���C���C���C�C�C�  C�C�C���D D ��DD��D��D}qD�qD� D�D��D�D�DD�DD��DD�D�qD	z�D	�qD
� D  D}qD  D� D  Dz�D�qD��D�D��D  D� D  D� D�qD}qD  D}qD�qD}qD  D� D�qD� DD��D�qD}qD�qDz�D��D� D�qDz�D  D� D  D� D�qD}qD��D}qD   D � D �qD!}qD"  D"� D"�qD#}qD$  D$}qD%  D%� D%�qD&��D'D'�D(�D(��D)  D)��D)�qD*}qD*�qD+}qD+�qD,��D-�D-� D-�qD.}qD/  D/��D0D0�D1�D1� D1��D2}qD2�qD3}qD4D4�D5  D5� D5��D6z�D7  D7}qD7�qD8��D9�D9� D:  D:��D:�qD;��D<D<�D=�D=� D>  D>� D>�qD?z�D?�RD@}qDADA� DA�qDBz�DC  DC� DD  DD�DE�DE� DFDF��DG  DG��DH�DH��DI�DI� DI�qDJ��DK�DK��DL�DL��DM�DM� DN  DN� DO  DO� DO�qDP� DQ  DQ��DR  DR� DS  DS� DS�qDT� DU  DU�DV�DV��DV�qDW}qDX  DX��DY  DY� DZ�DZ�D[�D[}qD\�D\�D]�D]��D^�D^��D_  D_}qD`  D`� Da  Da}qDa�qDb�DcDc��Dc�qDd� De�De� Df  Df��Dg�Dg� Dh  Dh��Di�Di� Dj�Dj� Dk�Dk}qDl  Dl��Dm�Dm��Dn  Dn� Do�Do��Dp  Dp}qDp��Dq}qDr�Dr��Ds�Ds��Dt�Dt��Du  Du��Dv  Dv� DwDw� Dw��Dx}qDy�Dy�Dz�Dz��D{D{��D{�qD|}qD}  D}z�D}�qD~� D~�qD}qD��D�@ D��HD�� D�  D�>�D�~�D���D���D�@ D��HD�� D���D�>�D�~�D���D�HD�B�D��HD���D�  D�>�D�}qD���D�  D�B�D�� D��qD�  D�@ D�� D��HD���D�>�D�~�D��qD���D�@ D�~�D�� D�HD�AHD��HD�� D�HD�B�D���D�D��D�>�D�}qD���D��qD�@ D��HD�� D���D�AHD�� D���D���D�>�D�� D�D�HD�@ D�� D�� D���D�>�D�~�D���D���D�@ D���D��HD�  D�@ D��HD��HD���D�@ D��HD���D��qD�>�D�~�D���D���D�@ D�� D�� D���D�>�D�~�D�� D�HD�>�D�~�D���D�  D�AHD�� D��qD���D�AHD���D���D��qD�>�D�� D��qD��qD�>�D�� D�� D�  D�>�D�~�D�� D�HD�AHD�� D���D��qD�=qD�~�D���D���D�>�D�~�D���D���D�>�D�}qD��)D��qD�=qD�}qD��qD���D�>�D�~�D��qD��qD�=qD�}qD�� D�HD�B�D��HD�� D�  D�@ D�~�D���D�  D�@ D�}qD���D���D�@ D�� D�� D�HD�@ D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D���D�  D�AHD��HD�� D���D�@ D��HD�� D��qD�=qD�~�D���D�HD�@ D�� D�� D�HD�AHD�~�D���D�HD�AHD�~�D�� D�HD�AHD��HD��HD�  D�@ D��HD�� D�HD�@ D�~�D�� D��D�AHD�� D��HD�HD�@ D�}qD���D�  D�AHD��HD�� D�HD�AHD��HD���D�  D�@ D�~�D��HD��D�@ D�� D�� D��qD�=qD�~�D���D�  D�AHD�� D���D�  D�@ D�� D��HD���D�@ D��HD���D�  D�AHD�� D�� D��D�AHD�� D�� D���D�<)D�� D�� D��qD�@ D D�� D��D�>�D�~�D��HD�HD�>�D�~�D�� D�  D�@ Dł�D�D�HD�@ D�~�Dƾ�D�HD�@ Dǀ D��HD�HD�>�D�~�D�� D�  D�AHDɀ Dɾ�D���D�>�D�~�Dʾ�D�  D�B�D˂�D��HD���D�@ D̀ D�� D���D�=qD�}qD;�D�  D�@ D΀ Dξ�D�HD�AHD�~�DϽqD�  D�AHDЀ Dо�D��D�B�Dр D�� D��D�B�DҀ DҾ�D�  D�B�Dӂ�D�� D���D�>�DԀ D�� D�  D�AHDՂ�D��HD���D�>�Dր D��HD�HD�@ D�~�D׽qD���D�>�D؀ D�� D�HD�AHD�~�Dپ�D�  D�@ DځHDھ�D���D�>�D�~�D�� D���D�>�D܁HD��HD��D�AHD݀ D�� D�  D�AHDށHD�� D�  D�>�D�}qD߾�D�  D�@ D�� DྸD��qD�=qD�~�D��HD�HD�@ D�~�D�qD���D�>�D� D�� D���D�>�D�~�D�� D�  D�>�D� D�� D�  D�AHD�HD��HD�HD�>�D�~�D��HD���D�@ D�HD�� D��D�AHD�}qD�� D��D�AHD�~�D꾸D���D�@ D� D�� D�  D�AHD� D�� D�  D�@ D� D���D��qD�>�D� DD�  D�AHD�}qDﾸD���D�@ D�� D�D�  D�AHD� D��HD�HD�>�D�~�D�� D�  D�@ D� D�D�  D�@ D� D�� D�HD�@ D�� D��HD�  D�>�D�~�D�� D�HD�AHD��HD��HD�HD�>�D�~�D�� D�  D�>�D�~�D�� D�HD�B�D���D��=>�?�?k�?�\)?�p�?�G�@�@�@&ff@=p�@L��@^�R@s33@��@��@�@��\@�=q@�33@�G�@���@У�@�  @�=q@��@��HA33AQ�A��A33A��A��A!G�A(Q�A-p�A1�A8Q�A>{AA�AG
=AN{AS�
AW�A\��Ac33Ag�Al(�Aq�Aw�A|(�A�Q�A��A�{A�  A�=qA�p�A�Q�A��\A�z�A�  A�=qA�(�A�
=A��A��
A�{A�G�A��
A�p�A�Q�A�33A�A��A��HA�p�A�
=A���A���A�\)A���A˅AθRA���Aҏ\A�A�Q�A��A�z�A�\)AᙚA�A�{A���A�\A�A��A�\A���A�\)A�=qA���A��RB z�B{B�Bz�B��B33B(�B	G�B
�RB(�B�B=qB�
B��B��B
=B��Bp�BffB�
B��B�B
=Bz�B��BffB�B!�B"{B"�HB$Q�B%��B&�\B'�B(��B)�B+33B,(�B-�B.=qB/�B0��B1p�B2�RB4  B4��B6{B7�B8��B9B:�HB<Q�B=��B>�\B?�BA�BBffBC\)BDQ�BE��BG
=BH(�BH��BJ=qBK\)BLQ�BMG�BN�RBO�
BP��BQ�BS33BT(�BU�BV�\BW�
BX��BYB[
=B\z�B]p�B^ffB_\)B`��Bb{Bb�HBc�
BeG�Bf�\Bg�Bhz�Bi�Bk33Bl(�Bm�BnffBo�Bp��BqBr�RBt  Bup�Bv=qBw33Bxz�By�Bz�HB{�B|��B~{B\)B�(�B���B��B��
B�ffB��HB�G�B��B���B��B��B�{B���B�p�B��B�ffB���B��B�=qB��RB�\)B�{B��RB��B���B�Q�B���B��B�(�B���B�\)B�{B��\B�
=B��B�Q�B�
=B��B�{B���B�p�B��B�ffB��B��B�Q�B��HB��B�Q�B���B��B�{B���B��B�  B��\B�G�B�  B���B��B�B�ffB��B��
B�Q�B��HB��B�Q�B���B�p�B�{B���B��B�{B���B��B�(�B��RB�G�B�  B���B�\)B��B���B�p�B�(�B���B�G�B�  B���B�\)B��B��\B�\)B�{B���B�p�B�  B���B�p�B�(�B���B�G�B�{B��HB���B�(�B���BÙ�B�Q�B��BŮB�Q�B��B��B�z�B��B��BʸRB�p�B�  Ḅ�B�\)B�=qB���Bϙ�B�=qB��B��
B�z�B��B�  B���B�G�B�  B���Bי�B�(�BظRBٙ�B�ffB���BۅB�ffB�33B��
B�z�B��B�  B�RB�33B��B���B�B�{B�RB噚B�Q�B��HB�B�Q�B��B�B�=qB���B��
B�\B��B��B�z�B�G�B�  B��\B�33B�(�B��HB�B�=qB���B���B�ffB�
=B��B�Q�B�
=B��
B�z�B�
=B�B��\B�\)B�  B��\B�G�C   C ffC C
=C\)CC{CQ�C�C{CffC�C��CQ�C�RC  CG�C�\C��CQ�C�C�
C33C�\C�
C�Cz�C�HC	G�C	��C	�HC
(�C
�\C
�CQ�C��C�HC33C��C  CQ�C��C�C=qC��C  C\)C��C�CQ�C�RC{C\)C��C��CQ�C�RC  CQ�C��C��C\)C�C��CG�C��C  CG�C�C�
C=qC��C�HC(�C�C�HC33Cp�C�RC{Cz�C��C{CffC�C  CQ�C�C
=CffC�RC��C=qC��C  CG�C�C�
C (�C z�C �HC!33C!p�C!C"{C"p�C"��C#{C#\)C#��C#��C$=qC$��C$��C%Q�C%�\C%�
C&33C&��C&�C'=qC'z�C'C({C(z�C(C)  C)Q�C)�C*  C*=qC*�C*C+{C+ffC+�C+�C,�C,Q�C,��C,�C-{C-G�C-z�C-C.  C.(�C.G�C.p�C.�C.�C/�C/=qC/ffC/��C/�
C0
=C0(�C0\)C0��C0�
C0��C1(�C1p�C1�\C1C2
=C2=qC2ffC2�\C2�HC3
=C333C3\)C3��C3�HC4{C4=qC4�C4C4�C5{C5Q�C5�\C5��C6
=C6(�C6\)C6��C6�HC7
=C733C7p�C7�C7�C8(�C8\)C8�C8�RC8�C9(�C9z�C9��C9��C:  C:=qC:�C:C:�HC;�C;\)C;��C;��C<  C<33C<z�C<�RC<�C=�C=G�C=�\C=��C>  C>(�C>\)C>��C>�HC?(�C?\)C?�\C?�RC@  C@G�C@z�C@��C@�
CA�CA\)CA��CA�HCB
=CB=qCBp�CB�RCB��CC33CCffCC�\CC��CD  CDG�CD�\CDCD�CE�CEffCE��CE�
CF  CFG�CF�CFCF�CG�CGQ�CG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                 ?��@�@E�@�G�@�G�@\@�G�@��RA�RA{A,(�A@  A`  A�  A�\)A��A���A���A�  A߮A�  B (�B  B�
B  B   B(  B/�B7�
B@(�BHQ�BP  BX  B`(�Bh(�Bo�
Bx  B�  B�  B��B�  B�  B��B��B��B��
B��
B��B�  B�{B�(�B�  B��B�  B�{B�(�B�  B�  B�{B�(�B�{B�{B�(�B�  B��B��B�  B�{B�  C   C��C��C�C�C	��C  C
=C  C�C  C  C��C��C��C  C��C!��C#��C&  C(
=C*{C,  C-��C/��C2  C4  C6
=C8  C:  C;��C=�C@  CB
=CD
=CF
=CG�CI��CK��CM�CO��CR  CS��CU��CX  CZ
=C\
=C]��C`
=Cb{Cd  Cf
=Ch  Cj  Ck��Cm��Co��Cq�Cs��Cu�Cx  Cz  C|  C}��C��C���C�C�C���C�  C���C�C�C�
=C�  C�C�  C�  C�  C���C�  C�C�C�  C�  C�C�C�  C�  C�
=C�C�  C�  C�  C�C�C���C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�  C�  C���C���C���C���C�  C�C�  C�  C�  C���C�  C�C���C���C�  C�C�  C�C�C�C���C�C�  C�
=C�C�C�
=C���C�  C�C�  C�C���C���C�  C�  C�  C���C���C�  C�C�C�  C�C���C���C���C�C�  C�  C���C���C�C�C�C�  C�  C�  C���C�  C�  C�C�  C�C�C�  C���C���C���C���C�  C���C���C���C���C�C�C�  C�C�C���D D ��DD��D��D}qD�qD� D�D��D�D�DD�DD��DD�D�qD	z�D	�qD
� D  D}qD  D� D  Dz�D�qD��D�D��D  D� D  D� D�qD}qD  D}qD�qD}qD  D� D�qD� DD��D�qD}qD�qDz�D��D� D�qDz�D  D� D  D� D�qD}qD��D}qD   D � D �qD!}qD"  D"� D"�qD#}qD$  D$}qD%  D%� D%�qD&��D'D'�D(�D(��D)  D)��D)�qD*}qD*�qD+}qD+�qD,��D-�D-� D-�qD.}qD/  D/��D0D0�D1�D1� D1��D2}qD2�qD3}qD4D4�D5  D5� D5��D6z�D7  D7}qD7�qD8��D9�D9� D:  D:��D:�qD;��D<D<�D=�D=� D>  D>� D>�qD?z�D?�RD@}qDADA� DA�qDBz�DC  DC� DD  DD�DE�DE� DFDF��DG  DG��DH�DH��DI�DI� DI�qDJ��DK�DK��DL�DL��DM�DM� DN  DN� DO  DO� DO�qDP� DQ  DQ��DR  DR� DS  DS� DS�qDT� DU  DU�DV�DV��DV�qDW}qDX  DX��DY  DY� DZ�DZ�D[�D[}qD\�D\�D]�D]��D^�D^��D_  D_}qD`  D`� Da  Da}qDa�qDb�DcDc��Dc�qDd� De�De� Df  Df��Dg�Dg� Dh  Dh��Di�Di� Dj�Dj� Dk�Dk}qDl  Dl��Dm�Dm��Dn  Dn� Do�Do��Dp  Dp}qDp��Dq}qDr�Dr��Ds�Ds��Dt�Dt��Du  Du��Dv  Dv� DwDw� Dw��Dx}qDy�Dy�Dz�Dz��D{D{��D{�qD|}qD}  D}z�D}�qD~� D~�qD}qD��D�@ D��HD�� D�  D�>�D�~�D���D���D�@ D��HD�� D���D�>�D�~�D���D�HD�B�D��HD���D�  D�>�D�}qD���D�  D�B�D�� D��qD�  D�@ D�� D��HD���D�>�D�~�D��qD���D�@ D�~�D�� D�HD�AHD��HD�� D�HD�B�D���D�D��D�>�D�}qD���D��qD�@ D��HD�� D���D�AHD�� D���D���D�>�D�� D�D�HD�@ D�� D�� D���D�>�D�~�D���D���D�@ D���D��HD�  D�@ D��HD��HD���D�@ D��HD���D��qD�>�D�~�D���D���D�@ D�� D�� D���D�>�D�~�D�� D�HD�>�D�~�D���D�  D�AHD�� D��qD���D�AHD���D���D��qD�>�D�� D��qD��qD�>�D�� D�� D�  D�>�D�~�D�� D�HD�AHD�� D���D��qD�=qD�~�D���D���D�>�D�~�D���D���D�>�D�}qD��)D��qD�=qD�}qD��qD���D�>�D�~�D��qD��qD�=qD�}qD�� D�HD�B�D��HD�� D�  D�@ D�~�D���D�  D�@ D�}qD���D���D�@ D�� D�� D�HD�@ D�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D���D�  D�AHD��HD�� D���D�@ D��HD�� D��qD�=qD�~�D���D�HD�@ D�� D�� D�HD�AHD�~�D���D�HD�AHD�~�D�� D�HD�AHD��HD��HD�  D�@ D��HD�� D�HD�@ D�~�D�� D��D�AHD�� D��HD�HD�@ D�}qD���D�  D�AHD��HD�� D�HD�AHD��HD���D�  D�@ D�~�D��HD��D�@ D�� D�� D��qD�=qD�~�D���D�  D�AHD�� D���D�  D�@ D�� D��HD���D�@ D��HD���D�  D�AHD�� D�� D��D�AHD�� D�� D���D�<)D�� D�� D��qD�@ D D�� D��D�>�D�~�D��HD�HD�>�D�~�D�� D�  D�@ Dł�D�D�HD�@ D�~�Dƾ�D�HD�@ Dǀ D��HD�HD�>�D�~�D�� D�  D�AHDɀ Dɾ�D���D�>�D�~�Dʾ�D�  D�B�D˂�D��HD���D�@ D̀ D�� D���D�=qD�}qD;�D�  D�@ D΀ Dξ�D�HD�AHD�~�DϽqD�  D�AHDЀ Dо�D��D�B�Dр D�� D��D�B�DҀ DҾ�D�  D�B�Dӂ�D�� D���D�>�DԀ D�� D�  D�AHDՂ�D��HD���D�>�Dր D��HD�HD�@ D�~�D׽qD���D�>�D؀ D�� D�HD�AHD�~�Dپ�D�  D�@ DځHDھ�D���D�>�D�~�D�� D���D�>�D܁HD��HD��D�AHD݀ D�� D�  D�AHDށHD�� D�  D�>�D�}qD߾�D�  D�@ D�� DྸD��qD�=qD�~�D��HD�HD�@ D�~�D�qD���D�>�D� D�� D���D�>�D�~�D�� D�  D�>�D� D�� D�  D�AHD�HD��HD�HD�>�D�~�D��HD���D�@ D�HD�� D��D�AHD�}qD�� D��D�AHD�~�D꾸D���D�@ D� D�� D�  D�AHD� D�� D�  D�@ D� D���D��qD�>�D� DD�  D�AHD�}qDﾸD���D�@ D�� D�D�  D�AHD� D��HD�HD�>�D�~�D�� D�  D�@ D� D�D�  D�@ D� D�� D�HD�@ D�� D��HD�  D�>�D�~�D�� D�HD�AHD��HD��HD�HD�>�D�~�D�� D�  D�>�D�~�D�� D�HD�B�D���G�O�>�?�?k�?�\)?�p�?�G�@�@�@&ff@=p�@L��@^�R@s33@��@��@�@��\@�=q@�33@�G�@���@У�@�  @�=q@��@��HA33AQ�A��A33A��A��A!G�A(Q�A-p�A1�A8Q�A>{AA�AG
=AN{AS�
AW�A\��Ac33Ag�Al(�Aq�Aw�A|(�A�Q�A��A�{A�  A�=qA�p�A�Q�A��\A�z�A�  A�=qA�(�A�
=A��A��
A�{A�G�A��
A�p�A�Q�A�33A�A��A��HA�p�A�
=A���A���A�\)A���A˅AθRA���Aҏ\A�A�Q�A��A�z�A�\)AᙚA�A�{A���A�\A�A��A�\A���A�\)A�=qA���A��RB z�B{B�Bz�B��B33B(�B	G�B
�RB(�B�B=qB�
B��B��B
=B��Bp�BffB�
B��B�B
=Bz�B��BffB�B!�B"{B"�HB$Q�B%��B&�\B'�B(��B)�B+33B,(�B-�B.=qB/�B0��B1p�B2�RB4  B4��B6{B7�B8��B9B:�HB<Q�B=��B>�\B?�BA�BBffBC\)BDQ�BE��BG
=BH(�BH��BJ=qBK\)BLQ�BMG�BN�RBO�
BP��BQ�BS33BT(�BU�BV�\BW�
BX��BYB[
=B\z�B]p�B^ffB_\)B`��Bb{Bb�HBc�
BeG�Bf�\Bg�Bhz�Bi�Bk33Bl(�Bm�BnffBo�Bp��BqBr�RBt  Bup�Bv=qBw33Bxz�By�Bz�HB{�B|��B~{B\)B�(�B���B��B��
B�ffB��HB�G�B��B���B��B��B�{B���B�p�B��B�ffB���B��B�=qB��RB�\)B�{B��RB��B���B�Q�B���B��B�(�B���B�\)B�{B��\B�
=B��B�Q�B�
=B��B�{B���B�p�B��B�ffB��B��B�Q�B��HB��B�Q�B���B��B�{B���B��B�  B��\B�G�B�  B���B��B�B�ffB��B��
B�Q�B��HB��B�Q�B���B�p�B�{B���B��B�{B���B��B�(�B��RB�G�B�  B���B�\)B��B���B�p�B�(�B���B�G�B�  B���B�\)B��B��\B�\)B�{B���B�p�B�  B���B�p�B�(�B���B�G�B�{B��HB���B�(�B���BÙ�B�Q�B��BŮB�Q�B��B��B�z�B��B��BʸRB�p�B�  Ḅ�B�\)B�=qB���Bϙ�B�=qB��B��
B�z�B��B�  B���B�G�B�  B���Bי�B�(�BظRBٙ�B�ffB���BۅB�ffB�33B��
B�z�B��B�  B�RB�33B��B���B�B�{B�RB噚B�Q�B��HB�B�Q�B��B�B�=qB���B��
B�\B��B��B�z�B�G�B�  B��\B�33B�(�B��HB�B�=qB���B���B�ffB�
=B��B�Q�B�
=B��
B�z�B�
=B�B��\B�\)B�  B��\B�G�C   C ffC C
=C\)CC{CQ�C�C{CffC�C��CQ�C�RC  CG�C�\C��CQ�C�C�
C33C�\C�
C�Cz�C�HC	G�C	��C	�HC
(�C
�\C
�CQ�C��C�HC33C��C  CQ�C��C�C=qC��C  C\)C��C�CQ�C�RC{C\)C��C��CQ�C�RC  CQ�C��C��C\)C�C��CG�C��C  CG�C�C�
C=qC��C�HC(�C�C�HC33Cp�C�RC{Cz�C��C{CffC�C  CQ�C�C
=CffC�RC��C=qC��C  CG�C�C�
C (�C z�C �HC!33C!p�C!C"{C"p�C"��C#{C#\)C#��C#��C$=qC$��C$��C%Q�C%�\C%�
C&33C&��C&�C'=qC'z�C'C({C(z�C(C)  C)Q�C)�C*  C*=qC*�C*C+{C+ffC+�C+�C,�C,Q�C,��C,�C-{C-G�C-z�C-C.  C.(�C.G�C.p�C.�C.�C/�C/=qC/ffC/��C/�
C0
=C0(�C0\)C0��C0�
C0��C1(�C1p�C1�\C1C2
=C2=qC2ffC2�\C2�HC3
=C333C3\)C3��C3�HC4{C4=qC4�C4C4�C5{C5Q�C5�\C5��C6
=C6(�C6\)C6��C6�HC7
=C733C7p�C7�C7�C8(�C8\)C8�C8�RC8�C9(�C9z�C9��C9��C:  C:=qC:�C:C:�HC;�C;\)C;��C;��C<  C<33C<z�C<�RC<�C=�C=G�C=�\C=��C>  C>(�C>\)C>��C>�HC?(�C?\)C?�\C?�RC@  C@G�C@z�C@��C@�
CA�CA\)CA��CA�HCB
=CB=qCBp�CB�RCB��CC33CCffCC�\CC��CD  CDG�CD�\CDCD�CE�CEffCE��CE�
CF  CFG�CF�CFCF�CG�CGQ�CG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                 @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�V@�_@�"G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�dZA�hsA�jA�hsA�jA�hsA�hsA�jA�jA�l�A�p�A�r�A�t�A�p�A�p�A�p�A�n�A�n�A�p�A�p�A�r�A�n�A�p�A�l�A�n�A�r�A���A�1A։7A�I�A��A��`AхA�-A�+Aϥ�A�/A�ffA�=qA�ȴA��
A�`BA�ƨA���A��`AìA�JA���A��
A� �A��+A�1A�t�A��A���A���A�  A���A��PA��`A�A��A��A�1'A��A��A���A��#A���A��A�S�A�%A���A��A��TA�l�A�bA���A�I�A�VA���A���A�{A�\)A���A��+A�  A��+A�oA�A��A�G�A��!A�I�A��
A�`BA�9XA���A��A��yA�bNA� �A�A��A�
=A�O�A��hA�jA��A�|�A��uA��DA�-A���A~�yA~A}�FA}33Av1'Ap~�Ak�Ag�7Ae&�Ab��A`Q�A^��AZ��AY�AX5?AW�mAV��ATffARv�AP9XAN��AN9XAM�AI�hAG�AE��AD9XAB��AAK�A?��A>n�A=�wA=�7A<1'A;x�A:Q�A9?}A8  A6��A4�uA37LA2ZA0�A/�A.bA,��A+O�A*z�A*$�A)�A)hsA&��A%�7A%+A$~�A#p�A"1'A ��A��A~�A{A�A�A7LAx�A�A��A�DA9XA`BA��An�A��A�A{A�;AXAjAVA�`At�A	ƨA	&�A��A�9A�A=qA~�A-A��AdZA?}A�jA{A��A��A7LA�`Az�A7LA M�@�;d@���@�J@�E�@�?}@�I�@�\)@�@�I�@�ȴ@�K�@��@��@�^5@�(�@�R@�b@���@��@�C�@�M�@�Z@�/@�V@�V@�G�@�-@�p�@��`@�J@�ƨ@◍@�n�@�$�@���@�x�@�X@���@�j@��@�@�dZ@�F@㝲@�dZ@�@�n�@��@�@�v�@���@�5?@�h@���@�j@ߍP@���@�=q@ܼj@۶F@�"�@�@ڏ\@�=q@�{@ٲ-@�O�@��@�l�@ְ!@�-@�J@��T@�/@�  @�dZ@��H@Ѻ^@���@�Q�@�1'@�(�@��@ϝ�@�
=@�@̴9@�z�@��;@�S�@�o@��H@�@Ɂ@��@�b@�|�@�33@��H@�~�@��@ź^@ź^@őh@�X@�7L@�bN@Õ�@�K�@�@�n�@�?}@��@���@�z�@��@��;@�t�@���@�^5@��T@�/@�V@�r�@�t�@��!@�^5@�=q@��@��@��h@�V@�9X@�ƨ@���@�C�@��H@�5?@��7@�&�@���@�z�@��
@�|�@�33@��R@�V@�^5@��\@���@�~�@�=q@�`B@���@��@��
@�ƨ@��w@���@�C�@�
=@���@�^5@��@��^@��h@�`B@�7L@��@�%@��j@�9X@���@�|�@�o@�ȴ@��R@���@�^5@�{@�X@��@�%@�Ĝ@��9@���@��D@�z�@�r�@�j@�Q�@��m@���@�\)@�"�@�o@�o@�@��@��+@�E�@��@���@�G�@�V@��9@��@�b@���@��P@�S�@�33@�
=@��@���@���@�ff@��@���@��h@�x�@���@���@��@�Z@�(�@�1@�  @��@��;@���@��P@�+@��y@���@�n�@�$�@��#@��7@���@�z�@� �@���@�t�@��@�V@��@���@�@��-@��-@���@���@�x�@�/@��@���@��@�9X@�  @��m@�ƨ@���@�S�@�
=@��!@�$�@���@��#@���@�@��^@���@��h@�O�@��9@�A�@��
@��@�+@��y@���@�n�@��@�@���@�hs@��@�r�@�b@��;@��F@���@�K�@���@���@���@��\@�v�@�ff@�M�@�{@���@�x�@�G�@�%@�Ĝ@���@�bN@�9X@��@�b@�1@�1@��m@��@�K�@��@��@��H@���@�=q@��#@���@��@�X@��@�%@�%@���@�Ĝ@��9@��9@���@��u@�z�@�Q�@�1'@��@���@��@��@�\)@�K�@�33@�
=@��y@���@�ff@�$�@�J@��^@�?}@�%@��`@��@�bN@�A�@�9X@�9X@�b@�ƨ@��P@�;d@��@���@�M�@���@���@���@���@��@�?}@���@��u@�r�@�Q�@�9X@� �@�  @��@K�@~5?@~{@~{@~{@~{@~@~{@~@}�-@}p�@|�/@|�@{��@{S�@{@z��@y��@x�`@w�;@vȴ@v5?@u�@u�-@t�/@tz�@t9X@t1@s��@s�
@sdZ@r=q@qhs@q�@pĜ@p��@p��@pb@ol�@n�@m�T@l�j@lj@k�m@k"�@j~�@j-@i&�@hA�@g�@g�@gl�@f��@f�+@fV@fV@f5?@e�T@ep�@d��@d�@d9X@d�@d1@c��@c��@cƨ@b�@a��@a%@`Ĝ@`�9@`��@`�@` �@_��@_�@_��@_;d@^�y@^�@^ȴ@^�+@^{@]@]�h@]p�@]/@\�j@\z�@[�F@["�@[@Z�\@Z=q@Z-@ZJ@Y�#@YX@X��@Xr�@XQ�@W�@W��@W\)@W�@V�y@V�R@Vff@V@U/@T�@S�m@S�@S@R��@R�\@Q��@QX@O��@N�y@N�R@NV@N{@M�@M�@M�@M�@M@Mp�@MV@L�@L�@L(�@K��@Kƨ@Kt�@K"�@J�@J��@J��@Jn�@JM�@J�@I��@I�7@IX@I%@H�9@H�u@H�u@HbN@G�@G�@G\)@Fȴ@Fv�@FE�@E��@E��@Ep�@E/@D�/@Dz�@DI�@Cƨ@CdZ@C"�@B��@Bn�@A��@Ax�@AG�@A%@@��@@��@@��@@1'@?��@?��@?K�@>��@>�y@>�R@>��@>�+@>ff@>5?@>$�@=�@=�T@=@=�@<��@<9X@;�
@;��@;33@:�@:��@:�\@:~�@:~�@:n�@:n�@:�@9�^@9x�@97L@9�@8��@8�`@8Ĝ@8r�@7��@7�P@7;d@6��@6v�@6$�@5�T@5�-@5O�@4��@4�D@4I�@3�m@3�@3C�@3o@2�!@2M�@2-@2�@2�@1�@1��@1&�@0�`@0A�@/l�@.��@.��@.E�@.@-�h@-p�@-O�@-�@,�@,��@,Z@,9X@+��@+ƨ@+S�@+"�@*�\@)��@)X@)G�@)G�@)7L@)7L@)�@)%@(Ĝ@(�9@(�9@(��@(bN@(A�@(1'@( �@( �@( �@( �@( �@(b@'�;@'�@'K�@'+@&��@&V@%��@%O�@$��@#�F@#dZ@#"�@#"�@#"�@#o@#@"��@"�!@"�!@"�!@"M�@!��@!��@!��@!�@!�@!�@!�@!�#@!��@!��@!x�@!&�@ �`@ �`@ ��@ �9@ �u@ bN@ Q�@ b@�w@�P@l�@�@�@�@$�@�T@@�h@V@�@�@�@�@�D@j@(�@��@dZ@o@�@��@�!@�\@M�@�@�#@hs@%@��@Ĝ@��@A�@ �@  @�@�;@�w@�@�P@K�@��@�y@�y@ȴ@��@��@ff@{@�T@{@{@�-@�@/@V@�@Z@�@�m@ƨ@dZ@C�@o@�@��@n�@^5@�@hs@7L@&�@�@Ĝ@�@  @�w@|�@\)@;d@+@��@
=@�@�+@5?@{@{@{@{@{A۬A�E�A�hsA�`BA�\)A�bNA�jA�dZA�hsA�jA�dZA�ffA�jA�l�A�hsA�ffA�jA�ffA�hsA�l�A�jA�ffA�jA�jA�ffA�dZA�jA�l�A�hsA�jA�n�A�hsA�hsA�n�A�l�A�hsA�n�A�r�A�p�A�p�A�t�A�t�A�p�A�p�A�t�A�t�A�p�A�t�A�v�A�p�A�r�A�v�A�t�A�p�A�p�A�r�A�t�A�p�A�n�A�r�A�p�A�l�A�p�A�r�A�n�A�n�A�r�A�r�A�n�A�p�A�t�A�r�A�n�A�t�A�r�A�jA�l�A�n�A�p�A�l�A�n�A�p�A�p�A�l�A�p�A�r�A�l�A�l�A�p�A�p�A�l�A�p�A�r�A�l�A�p�A�r�A�p�A�l�A�n�A�r�A�t�A�p�A�n�A�r�A�t�A�n�A�l�A�p�A�n�A�l�A�r�A�p�A�n�A�p�A�r�A�n�A�l�A�n�A�t�A�l�A�hsA�n�A�p�A�jA�l�A�p�A�n�A�l�A�p�A�p�A�jA�n�A�r�A�t�A�r�A�n�A�p�A�t�A�v�A�jA� �A���A��mA���AۑhA�G�A�33A��A�p�Aٛ�A׶FA�K�A�(�A�A��/Aև+A�t�A�ffA�ZA�S�A�M�A�I�A�M�A�I�A�E�A�G�A�I�A�C�A�A�A�?}A�+A��yAՓuA�VA�"�A�v�A�bNA��;A�ȴAѲ-AѶFAѴ9AѮAѧ�Aї�A�x�A�bNA�Q�A�XA�^5A�dZA�bNA�ZA�
=AиRA�bNA�O�A�=qA�"�A��A�bA�1A��A���Aϰ!AϓuAϑhAϑhAϋDAυA�r�A�dZA�-A��A��A�A��yA��
AΩ�AΡ�A΍PA�M�A���A��;Aͥ�A̓A�M�A�+A�VA�1A�1A�A���A���A��TA�ȴẠ�A�S�A˧�A���A���A�9XA�$�A��A�
=A��;A�r�A�C�A�(�A��A�A���A��TA��;A���AǬAǥ�AǙ�AǑhA�n�A�33A�A�ȴA�JAŝ�A�jA���AĸRAēuA�dZA�9XA���Að!Aã�AÉ7A�^5A�C�A�33A�-A�{A�  A���A��;A���A�A�bNA��HA��A���A���A���A�9XA��
A�ZA�$�A�
=A��#A���A�A��wA���A��A�G�A���A�`BA�+A��A�ƨA��!A���A�=qA�M�A�1A��A�ĜA�p�A�C�A�=qA�?}A�/A�/A�$�A��A��A�VA��TA�hsA�l�A�"�A�%A���A��HA��FA���A�l�A�XA�;dA��A���A���A��A���A��-A���A���A���A���A���A���A���A��\A��A�x�A�hsA�;dA���A��jA��A���A�`BA�M�A��!A��A�\)A�Q�A�O�A�-A�VA�1A�A���A��mA���A���A�p�A�M�A��#A��PA�=qA��yA���A�v�A�M�A�&�A���A��RA���A�I�A�A�A�7LA�(�A�A��A��;A���A��wA���A�x�A�E�A��A���A���A��FA��FA��-A���A���A���A���A���A��hA��A��A��A�~�A��A��7A��PA��\A�hsA��A�A���A��A��hA�Q�A���A�&�A��A��/A��FA�C�A��;A�v�A�"�A��A�jA�hsA��A��hA���A��7A�I�A��#A��+A�x�A�`BA�dZA�VA�\)A�r�A��DA�n�A�VA�7LA��A��TA��TA��HA��A���A���A���A���A��wA���A�x�A�I�A��A���A�ĜA���A�\)A�=qA���A��A�A�A�{A�oA�  A���A�ȴA��PA�hsA��A��A��A�hsA�/A�%A��7A�
=A��A��FA���A���A��A�p�A�O�A�?}A�;dA�5?A��A��/A��wA��A��-A��FA��hA��PA��DA��+A�|�A�r�A�Q�A�bA���A���A���A��`A���A��wA���A��DA�n�A�^5A�VA�M�A�G�A�9XA� �A�bA��mA�ZA�oA��A�VA��9A�5?A���A���A���A��uA��PA�r�A�z�A�v�A�v�A�ffA�bNA�\)A�I�A�C�A�=qA�5?A�5?A�5?A�(�A�{A�JA�A�  A��TA���A�r�A�`BA�S�A�K�A�9XA��A��jA�~�A�I�A�1'A�"�A�JA���A�ĜA�S�A�(�A��A�A��`A���A��-A���A��7A�x�A�r�A�p�A�n�A�hsA�`BA�ZA�XA�Q�A�M�A�E�A�C�A�A�A�A�A�?}A�;dA�1'A�/A�+A�$�A�&�A�(�A�oA�oA�JA���A���A��A��A�^5A�9XA�1A��HA���A���A�ȴA�A���A�|�A�M�A��A��TA��wA��A���A���A��hA���A��\A�~�A�p�A�ffA�K�A�?}A�5?A�/A�&�A�(�A�&�A�&�A� �A��A��A��A��A��A�oA�bA�
=A�A�A���A��A��TA��;A��HA��
A�A��-A��hA��A�v�A�l�A�\)A�K�A�33A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                 A�^5A�dZA�hsA�jA�hsA�jA�hsA�hsA�jA�jA�l�A�p�A�r�A�t�A�p�A�p�A�p�A�n�A�n�A�p�A�p�A�r�A�n�A�p�A�l�A�n�A�r�A���A�1A։7A�I�A��A��`AхA�-A�+Aϥ�A�/A�ffA�=qA�ȴA��
A�`BA�ƨA���A��`AìA�JA���A��
A� �A��+A�1A�t�A��A���A���A�  A���A��PA��`A�A��A��A�1'A��A��A���A��#A���A��A�S�A�%A���A��A��TA�l�A�bA���A�I�A�VA���A���A�{A�\)A���A��+A�  A��+A�oA�A��A�G�A��!A�I�A��
A�`BA�9XA���A��A��yA�bNA� �A�A��A�
=A�O�A��hA�jA��A�|�A��uA��DA�-A���A~�yA~A}�FA}33Av1'Ap~�Ak�Ag�7Ae&�Ab��A`Q�A^��AZ��AY�AX5?AW�mAV��ATffARv�AP9XAN��AN9XAM�AI�hAG�AE��AD9XAB��AAK�A?��A>n�A=�wA=�7A<1'A;x�A:Q�A9?}A8  A6��A4�uA37LA2ZA0�A/�A.bA,��A+O�A*z�A*$�A)�A)hsA&��A%�7A%+A$~�A#p�A"1'A ��A��A~�A{A�A�A7LAx�A�A��A�DA9XA`BA��An�A��A�A{A�;AXAjAVA�`At�A	ƨA	&�A��A�9A�A=qA~�A-A��AdZA?}A�jA{A��A��A7LA�`Az�A7LA M�@�;d@���@�J@�E�@�?}@�I�@�\)@�@�I�@�ȴ@�K�@��@��@�^5@�(�@�R@�b@���@��@�C�@�M�@�Z@�/@�V@�V@�G�@�-@�p�@��`@�J@�ƨ@◍@�n�@�$�@���@�x�@�X@���@�j@��@�@�dZ@�F@㝲@�dZ@�@�n�@��@�@�v�@���@�5?@�h@���@�j@ߍP@���@�=q@ܼj@۶F@�"�@�@ڏ\@�=q@�{@ٲ-@�O�@��@�l�@ְ!@�-@�J@��T@�/@�  @�dZ@��H@Ѻ^@���@�Q�@�1'@�(�@��@ϝ�@�
=@�@̴9@�z�@��;@�S�@�o@��H@�@Ɂ@��@�b@�|�@�33@��H@�~�@��@ź^@ź^@őh@�X@�7L@�bN@Õ�@�K�@�@�n�@�?}@��@���@�z�@��@��;@�t�@���@�^5@��T@�/@�V@�r�@�t�@��!@�^5@�=q@��@��@��h@�V@�9X@�ƨ@���@�C�@��H@�5?@��7@�&�@���@�z�@��
@�|�@�33@��R@�V@�^5@��\@���@�~�@�=q@�`B@���@��@��
@�ƨ@��w@���@�C�@�
=@���@�^5@��@��^@��h@�`B@�7L@��@�%@��j@�9X@���@�|�@�o@�ȴ@��R@���@�^5@�{@�X@��@�%@�Ĝ@��9@���@��D@�z�@�r�@�j@�Q�@��m@���@�\)@�"�@�o@�o@�@��@��+@�E�@��@���@�G�@�V@��9@��@�b@���@��P@�S�@�33@�
=@��@���@���@�ff@��@���@��h@�x�@���@���@��@�Z@�(�@�1@�  @��@��;@���@��P@�+@��y@���@�n�@�$�@��#@��7@���@�z�@� �@���@�t�@��@�V@��@���@�@��-@��-@���@���@�x�@�/@��@���@��@�9X@�  @��m@�ƨ@���@�S�@�
=@��!@�$�@���@��#@���@�@��^@���@��h@�O�@��9@�A�@��
@��@�+@��y@���@�n�@��@�@���@�hs@��@�r�@�b@��;@��F@���@�K�@���@���@���@��\@�v�@�ff@�M�@�{@���@�x�@�G�@�%@�Ĝ@���@�bN@�9X@��@�b@�1@�1@��m@��@�K�@��@��@��H@���@�=q@��#@���@��@�X@��@�%@�%@���@�Ĝ@��9@��9@���@��u@�z�@�Q�@�1'@��@���@��@��@�\)@�K�@�33@�
=@��y@���@�ff@�$�@�J@��^@�?}@�%@��`@��@�bN@�A�@�9X@�9X@�b@�ƨ@��P@�;d@��@���@�M�@���@���@���@���@��@�?}@���@��u@�r�@�Q�@�9X@� �@�  @��@K�@~5?@~{@~{@~{@~{@~@~{@~@}�-@}p�@|�/@|�@{��@{S�@{@z��@y��@x�`@w�;@vȴ@v5?@u�@u�-@t�/@tz�@t9X@t1@s��@s�
@sdZ@r=q@qhs@q�@pĜ@p��@p��@pb@ol�@n�@m�T@l�j@lj@k�m@k"�@j~�@j-@i&�@hA�@g�@g�@gl�@f��@f�+@fV@fV@f5?@e�T@ep�@d��@d�@d9X@d�@d1@c��@c��@cƨ@b�@a��@a%@`Ĝ@`�9@`��@`�@` �@_��@_�@_��@_;d@^�y@^�@^ȴ@^�+@^{@]@]�h@]p�@]/@\�j@\z�@[�F@["�@[@Z�\@Z=q@Z-@ZJ@Y�#@YX@X��@Xr�@XQ�@W�@W��@W\)@W�@V�y@V�R@Vff@V@U/@T�@S�m@S�@S@R��@R�\@Q��@QX@O��@N�y@N�R@NV@N{@M�@M�@M�@M�@M@Mp�@MV@L�@L�@L(�@K��@Kƨ@Kt�@K"�@J�@J��@J��@Jn�@JM�@J�@I��@I�7@IX@I%@H�9@H�u@H�u@HbN@G�@G�@G\)@Fȴ@Fv�@FE�@E��@E��@Ep�@E/@D�/@Dz�@DI�@Cƨ@CdZ@C"�@B��@Bn�@A��@Ax�@AG�@A%@@��@@��@@��@@1'@?��@?��@?K�@>��@>�y@>�R@>��@>�+@>ff@>5?@>$�@=�@=�T@=@=�@<��@<9X@;�
@;��@;33@:�@:��@:�\@:~�@:~�@:n�@:n�@:�@9�^@9x�@97L@9�@8��@8�`@8Ĝ@8r�@7��@7�P@7;d@6��@6v�@6$�@5�T@5�-@5O�@4��@4�D@4I�@3�m@3�@3C�@3o@2�!@2M�@2-@2�@2�@1�@1��@1&�@0�`@0A�@/l�@.��@.��@.E�@.@-�h@-p�@-O�@-�@,�@,��@,Z@,9X@+��@+ƨ@+S�@+"�@*�\@)��@)X@)G�@)G�@)7L@)7L@)�@)%@(Ĝ@(�9@(�9@(��@(bN@(A�@(1'@( �@( �@( �@( �@( �@(b@'�;@'�@'K�@'+@&��@&V@%��@%O�@$��@#�F@#dZ@#"�@#"�@#"�@#o@#@"��@"�!@"�!@"�!@"M�@!��@!��@!��@!�@!�@!�@!�@!�#@!��@!��@!x�@!&�@ �`@ �`@ ��@ �9@ �u@ bN@ Q�@ b@�w@�P@l�@�@�@�@$�@�T@@�h@V@�@�@�@�@�D@j@(�@��@dZ@o@�@��@�!@�\@M�@�@�#@hs@%@��@Ĝ@��@A�@ �@  @�@�;@�w@�@�P@K�@��@�y@�y@ȴ@��@��@ff@{@�T@{@{@�-@�@/@V@�@Z@�@�m@ƨ@dZ@C�@o@�@��@n�@^5@�@hs@7L@&�@�@Ĝ@�@  @�w@|�@\)@;d@+@��@
=@�@�+@5?@{@{@{@{G�O�A۬A�E�A�hsA�`BA�\)A�bNA�jA�dZA�hsA�jA�dZA�ffA�jA�l�A�hsA�ffA�jA�ffA�hsA�l�A�jA�ffA�jA�jA�ffA�dZA�jA�l�A�hsA�jA�n�A�hsA�hsA�n�A�l�A�hsA�n�A�r�A�p�A�p�A�t�A�t�A�p�A�p�A�t�A�t�A�p�A�t�A�v�A�p�A�r�A�v�A�t�A�p�A�p�A�r�A�t�A�p�A�n�A�r�A�p�A�l�A�p�A�r�A�n�A�n�A�r�A�r�A�n�A�p�A�t�A�r�A�n�A�t�A�r�A�jA�l�A�n�A�p�A�l�A�n�A�p�A�p�A�l�A�p�A�r�A�l�A�l�A�p�A�p�A�l�A�p�A�r�A�l�A�p�A�r�A�p�A�l�A�n�A�r�A�t�A�p�A�n�A�r�A�t�A�n�A�l�A�p�A�n�A�l�A�r�A�p�A�n�A�p�A�r�A�n�A�l�A�n�A�t�A�l�A�hsA�n�A�p�A�jA�l�A�p�A�n�A�l�A�p�A�p�A�jA�n�A�r�A�t�A�r�A�n�A�p�A�t�A�v�A�jA� �A���A��mA���AۑhA�G�A�33A��A�p�Aٛ�A׶FA�K�A�(�A�A��/Aև+A�t�A�ffA�ZA�S�A�M�A�I�A�M�A�I�A�E�A�G�A�I�A�C�A�A�A�?}A�+A��yAՓuA�VA�"�A�v�A�bNA��;A�ȴAѲ-AѶFAѴ9AѮAѧ�Aї�A�x�A�bNA�Q�A�XA�^5A�dZA�bNA�ZA�
=AиRA�bNA�O�A�=qA�"�A��A�bA�1A��A���Aϰ!AϓuAϑhAϑhAϋDAυA�r�A�dZA�-A��A��A�A��yA��
AΩ�AΡ�A΍PA�M�A���A��;Aͥ�A̓A�M�A�+A�VA�1A�1A�A���A���A��TA�ȴẠ�A�S�A˧�A���A���A�9XA�$�A��A�
=A��;A�r�A�C�A�(�A��A�A���A��TA��;A���AǬAǥ�AǙ�AǑhA�n�A�33A�A�ȴA�JAŝ�A�jA���AĸRAēuA�dZA�9XA���Að!Aã�AÉ7A�^5A�C�A�33A�-A�{A�  A���A��;A���A�A�bNA��HA��A���A���A���A�9XA��
A�ZA�$�A�
=A��#A���A�A��wA���A��A�G�A���A�`BA�+A��A�ƨA��!A���A�=qA�M�A�1A��A�ĜA�p�A�C�A�=qA�?}A�/A�/A�$�A��A��A�VA��TA�hsA�l�A�"�A�%A���A��HA��FA���A�l�A�XA�;dA��A���A���A��A���A��-A���A���A���A���A���A���A���A��\A��A�x�A�hsA�;dA���A��jA��A���A�`BA�M�A��!A��A�\)A�Q�A�O�A�-A�VA�1A�A���A��mA���A���A�p�A�M�A��#A��PA�=qA��yA���A�v�A�M�A�&�A���A��RA���A�I�A�A�A�7LA�(�A�A��A��;A���A��wA���A�x�A�E�A��A���A���A��FA��FA��-A���A���A���A���A���A��hA��A��A��A�~�A��A��7A��PA��\A�hsA��A�A���A��A��hA�Q�A���A�&�A��A��/A��FA�C�A��;A�v�A�"�A��A�jA�hsA��A��hA���A��7A�I�A��#A��+A�x�A�`BA�dZA�VA�\)A�r�A��DA�n�A�VA�7LA��A��TA��TA��HA��A���A���A���A���A��wA���A�x�A�I�A��A���A�ĜA���A�\)A�=qA���A��A�A�A�{A�oA�  A���A�ȴA��PA�hsA��A��A��A�hsA�/A�%A��7A�
=A��A��FA���A���A��A�p�A�O�A�?}A�;dA�5?A��A��/A��wA��A��-A��FA��hA��PA��DA��+A�|�A�r�A�Q�A�bA���A���A���A��`A���A��wA���A��DA�n�A�^5A�VA�M�A�G�A�9XA� �A�bA��mA�ZA�oA��A�VA��9A�5?A���A���A���A��uA��PA�r�A�z�A�v�A�v�A�ffA�bNA�\)A�I�A�C�A�=qA�5?A�5?A�5?A�(�A�{A�JA�A�  A��TA���A�r�A�`BA�S�A�K�A�9XA��A��jA�~�A�I�A�1'A�"�A�JA���A�ĜA�S�A�(�A��A�A��`A���A��-A���A��7A�x�A�r�A�p�A�n�A�hsA�`BA�ZA�XA�Q�A�M�A�E�A�C�A�A�A�A�A�?}A�;dA�1'A�/A�+A�$�A�&�A�(�A�oA�oA�JA���A���A��A��A�^5A�9XA�1A��HA���A���A�ȴA�A���A�|�A�M�A��A��TA��wA��A���A���A��hA���A��\A�~�A�p�A�ffA�K�A�?}A�5?A�/A�&�A�(�A�&�A�&�A� �A��A��A��A��A��A�oA�bA�
=A�A�A���A��A��TA��;A��HA��
A�A��-A��hA��A�v�A�l�A�\)A�K�A�33A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�B
�B
��B
�mB
�B
�mB
�mB
�
B
��B
�
B
�B
��B
��B
��B
�>B
��B
�B
�
B
�B
�B
��B
�8B
��B
�B
�mB
�2B
��B
��B'�B9�B@�BLdBcBx�B�SB�$B��B��B�RB��B��B�BxB.�BO�BS[B>wB0�BA�BQNBZ�B^5B_�B_;B[#Bf�BaHBbBaB^�Bc Bm�B^�BZ�BT,BIB@�B>B;dB;0BA�BJ#BI�Be`B0�BD3B>�BFB<jB8B �B~B��B��B��B� B�<B��B��B��B�}B��B�B��B{�Bl�Bd�BbNBbBZBOvBE�B@OB>BB=�B9XB0�B$@B�B
�+B
�yB
��B
͟B
�<B
�'B
��B
�(B
��B
��B
gB
=B
"�B
	�B	��B	�B	ߤB	�KB	ɆB	��B	��B	��B	��B	��B	�CB	�MB	�B	�YB	��B	u%B	jKB	c�B	ZB	S[B	OBB	E�B	A�B	;�B	9$B	7�B	5B	2�B	0�B	*0B	%FB	 'B	_B	MB	4B		�B	�B	{B	;B�JB��B��B��B�B��B��B�B��B��B�HB� B�&B�&B��B� B��B��B�B��B��B� B��BߤB�vBݘB��B��B�5B�B��B�GB�B�B�yB�oB��B��B��B	
rB	MB	'�B	,=B	+6B	+B	33B	3�B	33B	/�B	0!B	5�B	6zB	0�B	)�B	%�B	'�B	(�B	?B	=�B	=<B	9�B	IRB	I�B	C�B	G�B	J�B	B�B	:�B	7B	7�B	F?B	S�B	p�B	��B	�B	{JB	p�B	`vB	^�B	T�B	U2B	\)B	_B	^5B	WsB	P�B	Q�B	RTB	Q�B	QNB	Q�B	YB	f�B	k�B	t�B	��B	��B	��B	��B	��B	�+B	��B	�zB	�B	�FB	�XB	��B	��B	�FB	��B	�LB	�LB	�LB	�?B	��B	��B	�}B	�BB	�wB	�B	��B	�zB	�XB	�XB	��B	��B	��B	��B	�)B	�XB	��B	ʌB	��B	�#B	ʌB	�)B	��B	��B	��B	ϫB	уB	��B	ٴB	�#B	�WB	یB	ߤB	��B	��B	�B	�B	��B	�B	��B	��B	��B	�2B	��B	�	B	�>B	��B
 iB
;B
oB
�B
�B
�B
�B
_B
�B
	B
	�B
~B
B
�B
�B
�B
 B
oB
uB
�B
�B
B
�B
FB
SB
�B
�B
�B
eB
kB
	B
qB
qB
�B
�B
OB
�B
VB
 \B
 �B
!�B
$�B
&LB
&�B
'RB
(�B
'RB
&B
$�B
$�B
%�B
&B
&�B
&LB
'RB
'B
(�B
($B
(�B
(�B
)*B
)*B
(�B
)�B
*�B
*�B
+�B
,qB
-B
,�B
,qB
-CB
-�B
.B
.B
-�B
/B
/OB
/B
.�B
.�B
.}B
.}B
.}B
/B
/�B
0�B
0�B
0UB
0!B
0UB
0UB
0UB
/�B
1�B
3�B
4�B
5�B
7�B
7LB
9$B
9$B
9�B
9�B
9XB
9�B
:*B
:�B
;�B
<B
<�B
<jB
<�B
<�B
=�B
=�B
>�B
>wB
>�B
>�B
>wB
>wB
>B
>B
>�B
?B
?}B
?B
?�B
?�B
@OB
@�B
A�B
B'B
B�B
C�B
C-B
DgB
D�B
D�B
DgB
D3B
DgB
DgB
DgB
DgB
D�B
E�B
EmB
EmB
FB
FB
FtB
F�B
F�B
F�B
GzB
HB
IB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J#B
K^B
K�B
L0B
LdB
MB
M6B
M�B
MjB
N�B
N�B
NpB
N�B
O�B
OvB
P}B
PHB
P}B
P}B
Q�B
R B
RTB
R�B
R�B
RTB
R�B
R�B
S&B
S�B
S[B
T,B
TaB
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
VB
V�B
VmB
W
B
W
B
V�B
W�B
XB
XyB
X�B
X�B
YB
YB
YKB
YKB
Y�B
Y�B
Y�B
Y�B
ZQB
Y�B
ZQB
ZQB
Z�B
Z�B
Z�B
[WB
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\]B
\�B
\�B
]dB
]�B
]/B
\�B
]�B
\�B
\�B
\]B
\]B
\�B
\�B
]dB
^jB
^�B
_B
^�B
_�B
_�B
_pB
_�B
_;B
_�B
_�B
_�B
`B
`B
`BB
`vB
`vB
`�B
aHB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
bNB
a�B
b�B
c B
c B
b�B
c�B
d�B
e`B
ffB
f�B
f�B
gmB
iDB
iyB
iyB
i�B
jKB
j�B
kB
l"B
lWB
l�B
l"B
l"B
k�B
l�B
m]B
m�B
ncB
ncB
ncB
n�B
o5B
oiB
o5B
poB
p�B
qB
qB
qvB
q�B
rB
r|B
rGB
r|B
r�B
sMB
s�B
s�B
t�B
tTB
tTB
tTB
tB
tTB
t�B
v`B
v�B
v�B
v�B
v�B
v�B
wfB
w�B
wfB
wfB
x8B
x8B
xlB
x8B
xlB
y	B
yrB
y�B
y�B
zB
zB
zDB
z�B
{JB
{B
{�B
{B
{B
{�B
{�B
|PB
}"B
}VB
}�B
}�B
}�B
}�B
~]B
~(B
~]B
~�B
~�B
�B
�iB
��B
��B
��B
��B
�uB
�GB
�{B
�SB
�SB
�SB
��B
��B
�%B
��B
��B
��B
��B
�%B
��B
��B
�+B
�fB
��B
��B
��B
�	B
�rB
��B
��B
��B
��B
��B
�B
�B
��B
�PB
��B
��B
�PB
��B
��B
�"B
�VB
��B
��B
��B
�\B
�\B
�\B
��B
��B
�.B
��B
��B
� B
� B
�hB
��B
��B
��B
��B
��B
��B
��B
��B
�uB
��B
��B
�B
�FB
�FB
�FB
��B
��B
��B
��B
��B
��B
��B
��B
�MB
��B
��B
��B
��B
�SB
�SB
�B
��B
�SB
�B
�SB
�B
��B
��B
��B
��B
��B
��B
��B
��B
�YB
��B
��B
��B
�+B
�_B
�_B
�_B
�+B
�+B
��B
�+B
�YB
�+B
�_B
�1B
�1B
��B
�kB
�kB
��B
��B
��B
��B
�=B
�	B
�B
��B
�B
�IB
�B
��B
��B
�~B
�~B
��B
��B
�B
��B
��B
��B
��B
��B
�!B
�'B
��B
��B
��B
��B
�-B
��B
�-B
�-B
��B
��B
��B
�bB
��B
��B
�4B
��B
��B
�4B
��B
��B
��B
�4B
�hB
��B
�hB
��B
�:B
�nB
�nB
��B
�FB
�B
�B
�B
�B
�B
�FB
�FB
�FB
�B
�B
�zB
�zB
�FB
�zB
�zB
�FB
�zB
�FB
�zB
�FB
�FB
�zB
�zB
�FB
�FB
�zB
�zB
�zB
��B
�zB
�B
��B
��B
��B
��B
��B
�RB
�XB
�$B
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
�eB
�eB
��B
��B
�kB
�kB
��B
�B
�B
�=B
�=B
��B
�qB
�=B
��B
��B
�CB
�CB
�CB
�CB
�wB
�wB
�wB
�B
�B
�B
�IB
�}B
��B
��B
�OB
�B
�OB
��B
��B
��B
�'B
�[B
�aB
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
�3B
�3B
�hB
�B
�nB
�nB
��B
��B
��B
��B
�B
�B
�B
�B
�?B
��B
��B
�B
��B
��B
�?�1'@w!-B
��B
�B
�
B
�B
��B
�B
��B
�B
�DB
�sB
�mB
�fB
�sB
�sB
�fB
�
B
�
B
�B
��B
��B
��B
��B
�B
��B
�8B
�B
��B
�B
�B
�sB
��B
�B
��B
�B
�B
�B
�B
��B
�8B
�8B
��B
�B
�B
�
B
��B
��B
��B
�B
�sB
�B
��B
�B
�sB
�>B
�8B
�B
�sB
�B
�B
�DB
�B
�B
�>B
��B
�B
��B
�B
�
B
�B
�8B
��B
�B
�mB
�B
�B
�mB
�8B
�B
�>B
�8B
��B
�B
��B
�B
�B
�B
�B
�mB
��B
�8B
�B
�B
�mB
�2B
�mB
�B
�sB
��B
�B
��B
�B
�8B
�2B
�B
�sB
�8B
��B
�B
��B
�B
�B
�
B
�fB
�mB
�sB
�mB
�B
��B
�B
�B
�fB
�
B
��B
�2B
�B
��B
��B
�`B
�B
��B
�B
�TB
�B
�B
�B
�HB
��B
�NB
��B
�B
��B
��B�BfB�BMBB<B@B"hB&LB+B-wB8�B<�B=B?HB@�BA�BA�B@OB@�BA�B?�B?HB@B?HB=�BB�BPHBb�B^Bc�B��B��B~�B�\Bu�Bs�Bt�BsMBu�Bw�Bx�BuZB~(B��B�B�JB��B�FB��B��B��B�@B��B��B��B�=B��B��B�OB��B��B��B�[B�'B��B��B�!B�B��B�OB�=B�B��B�B��B��B��B��B�RB�eB��B��B�FB��B�B�zB�FB�B��B�!B�3B��BٴB�yB�B�B��B��B��B��B�B"�B&B&�B&B(XB,B*0B&�B0�B3hB1'B9�B:�BHKBNpBPBX�BpoBX�BP�Bk�BP�BM�BQNBN<BC-BD�B9�B<6B?�B0�B0�B1�B5tB.IB.IB1�B0�B33B4B?�BtTBC-BB[BDgBS�B[WB]/BR�BR�BW?B\]BpoBW
BV9BV9BW�Bl�B]/B\�B^5BYBWsBV9BhsBuZB[�BW
Bc�BgBbNB^BYKB`vBZ�B^jBZ�BXBX�B_Bq�Bu�BaB_�BZ�B_�B`BB`BBf2B`vBa|Bc�B_�B`BB`BBe�Bd�Ba|B_�B^�B`B`BB^5B]�B_B_�B_pB_pBdZBo B^�B`vB\�BjB��Bt�Ba|Bd�B]�B^5B`Ba|B\�BZQB[�B_�BZ�Bd&BVBZQBd�BT�BWsBNpBM�BJ�BGzBE�BJ#BM�BAUBO�B=�B=<B?�BC-B=�B<�B9�B<6BB[B?�B?�B>BB:�B@OB8�B8B9XB<B:�B;dB<B:^B>B@OB@�BB�BC�BC�BC�BC�BC�BTaBOvBJ#BH�BA�B?HBB[BK�BZQBW�BZBd�BjKBh�BdZBR�BCaB($B(�B,�B0UB3�BI�BPBQ�B=�B7�B2�B4�B6zB9�B>�BJ�BS[BIBN�BMB>�B=�B=�B<�B=qB<�B<�B9$B=B;0BC�B@�B33B0�B(XB)�B'�B�BB�B4B
	B1B
	B�B��B��B�MB�KB�rB�B�B��B��B�2B�]B�B��B� B�aB�yB��BϫB��B�jB�dB�B�
BӏB�vB�BB��BуB̘B͟B�<B��B�)B�B��B�9BƨB�9B�)BǮB�B�BB�B�B�B��B�6B�B��B��B�mB��B��B�4B��B�B�_B��B�B�"B��B��B��B��B��B��B�hB��B��B�rB�B�lB�xB�rB�lB�lB��B�7B��B��B��B�rB�fB��B��B~�B�B�B~�B� B}VBuZBuZBt�BtTBzB�iBo�BlWBoiBkQBi�Bm�Bj�BiyBf�BffBe�Bd�Bd�Bd�Be�Bd�BcTBc�Bc�Bd�Bb�BbBaHBbNBaHBa�BaBcTB`vB`Ba|B`vB`�Bd�Bd&BbNBdZBaB_pB`vB\]BX�BVBT,BT�BV�BU2BW�BU�BQNBMjBJXBIRBH�BFtBD�BGBHKBHBH�BD�BEmBD3BC�BA B@�B?}B?}B@OB@�B@�B?�B>BB>B>�B?B?�B>�B=B<6B=�B>wB=�B<�B=�B>wB@�BCaB<B;�B:�B>�B<B:�B9�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021071717233120210717172331IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021072722012220210727220122QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021072722012220210727220122QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365120220126093651IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                