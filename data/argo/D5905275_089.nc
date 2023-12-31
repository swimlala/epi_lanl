CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-06-15T08:59:55Z creation; 2023-04-26T19:14:29Z DMQC;      
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
_FillValue        G�O�     �  dh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҩ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � hH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20200615085955  20230426191429  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               Y   YAA  AOAO7316_008644_089                 7316_008644_089                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�!R>�/0@�!R>�/011  @�!R}Vl�@�!R}Vl�@*Y_��a@*Y_��a�c�hƒ���c�hƒ��11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��?��H@:�H@}p�@�G�@�  @�G�A ��A  A   A+�A@  A_\)A�Q�A�  A��A���A���A�Q�A�  A�  B   B�
B�B�
B (�B(Q�B0Q�B8(�B@  BH  BP(�BXQ�B`(�Bh(�Bp(�Bw�
B�  B��B�  B��B��B��
B�  B�{B��B��B�  B�{B�{B�{B��B��B��B��B�{B�{B�  B��B��
B�  B�{B�(�B�=qB�{B�{B�{B�{B�  B��
C��C��C  C��C	�C�C  C  C  C��C  C
=C  C  C  C 
=C!��C#��C&  C'��C*  C,  C.
=C0  C2  C4
=C6  C7��C9��C;��C=�C?��CB  CD  CF
=CH{CJ{CL  CM�CO��CR
=CT{CV
=CX
=CZ{C\{C^
=C`  Cb
=Cd  Ce��Ch  Cj
=Cl
=Cn
=Cp  Cr
=Ct{Cv
=Cx  Cy��C{��C~
=C�
=C�C�  C�C�  C���C�  C�C�C�C�C�  C�  C�  C�  C�C�  C���C���C�  C���C���C�C�C���C���C�  C�C�C�  C���C�  C�C�C�  C�C�C�  C���C�  C���C���C���C���C���C�C�
=C�C�C�  C�  C�  C�C�  C�  C�  C�C�C�
=C���C�  C�  C���C�  C�  C�  C�  C�  C�C���C�  C�  C�  C�  C�  C�C���C���C���C���C�  C�C�  C���C���C�  C�C���C�  C�C�  C�C�  C���C�  C���C���C���C�C�  C�  C�  C���C���C���C�C�
=C�
=C�
=C�  C�C�
=C�  C�C�
=C�C���C�C�
=C�C�  C���C���C�C�  C���C���C�  D �D � D  D� D�qD}qD�qD}qD��Dz�D��Dz�D  D��D  DxRD�qD��D	�D	� D
  D
� D  D� D�qD}qD�qD}qD��D� DD��D  D}qD��D� D�D��D  D}qD  D�D  D}qD�qD� D��D}qD  D� D  Dz�D��D}qD  D��DD�D�qD� D  D� D  D� D �D ��D!D!��D!�qD"}qD#�D#�D$  D$� D%�D%� D&  D&� D'  D'}qD'�qD(� D)  D)��D*  D*}qD*�qD+� D+�qD,}qD-  D-��D.  D.}qD/  D/� D/�qD0}qD0�qD1}qD1�qD2}qD2�qD3� D4  D4� D5  D5� D6  D6}qD6��D7}qD8  D8}qD8��D9� D:�D:� D;  D;}qD;�qD<}qD=  D=}qD>  D>�D?  D?� D@  D@� DA�DA� DA��DB� DC�DC� DD�DD� DD�qDE��DF  DF� DG�DG��DH  DH� DI�DI}qDJ  DJ��DK  DK��DL�DL��DM  DMz�DN  DN� DO  DO}qDP  DP��DQ�DQ� DR  DR��DSDS��DT  DT� DU  DU}qDU�qDV� DW�DW� DX  DX� DX�qDY}qDZ  DZ��D[  D[� D\  D\}qD\�qD]}qD]�qD^��D_D_��D`  D`� Da�Da��Db�Db}qDc  Dc� Dc�qDd}qDd�qDe� De�qDf� Dg  Dg� Dh  Dh}qDh�qDi}qDi�qDj}qDk  Dk� Dl�Dl� Dl�qDm� Dn�Dn��Do  Do��Dp�Dp}qDp�qDq}qDr  Dr��Dr�qDsz�Ds�qDt� Du�Du�Dv�Dv��Dw�Dw�Dx�Dx}qDx�qDy��Dz�Dz� D{  D{� D{�qD|� D}�D}��D~  D~� D~�qD}qD�qD�@ D�� D��qD��qD�@ D�� D��HD��D�@ D�}qD���D�HD�AHD��HD�� D�HD�B�D�� D��HD�  D�>�D�~�D�� D�HD�B�D��HD�� D�  D�>�D�� D��HD��D�B�D�� D�� D�  D�@ D�� D�� D�HD�>�D�� D��HD���D�@ D��HD�� D���D�@ D��HD�� D���D�>�D��HD��HD��D�@ D�}qD���D�HD�AHD��HD�� D���D�@ D��HD��HD�HD�@ D�~�D��qD�  D�B�D��HD�� D�  D�>�D��HD�� D�  D�B�D���D��HD�HD�AHD��HD�� D�  D�@ D�� D��HD��D�AHD�� D�� D���D�>�D�~�D�� D�HD�>�D�~�D���D�  D�B�D���D�� D���D�@ D��HD���D��qD�>�D�~�D���D���D�>�D�~�D���D�  D�@ D�� D�� D���D�=qD�~�D�� D�HD�>�D�}qD�� D���D�=qD�� D�� D�HD�@ D�}qD���D�HD�B�D�� D���D�  D�AHD�� D���D�  D�B�D��HD�� D�HD�@ D�~�D��HD�HD�>�D�� D�D�HD�@ D���D��HD�  D�AHD��HD�D�HD�@ D��HD�D�  D�>�D�~�D�� D�HD�AHD���D��HD���D�@ D��HD�� D���D�>�D�}qD���D�  D�AHD�� D���D�  D�AHD�� D�� D�HD�@ D�~�D��HD�  D�>�D��HD��HD���D�@ D�� D��HD�HD�@ D�� D�� D�HD�AHD�~�D�� D�HD�AHD�� D��qD�HD�AHD�� D���D�  D�AHD�~�D��qD�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD��HD�� D���D�AHD���D��HD�  D�>�D�� D��HD�HD�@ D��HD�D��D�AHD�� D�� D�  D�@ D�� D�� D��qD�>�D D��HD�HD�AHD�~�DýqD���D�@ DĀ Dľ�D���D�>�Dŀ Dž�D���D�@ D�~�D�� D��D�@ D�~�DǾ�D�  D�>�D�~�D�� D�  D�@ Dɀ D�� D�  D�AHDʁHD�� D�HD�AHDˀ D˾�D��qD�>�D̀ D�� D�  D�@ D́HD�� D�  D�>�D΀ Dξ�D���D�>�D�~�D�� D��D�AHD�~�Dо�D�  D�AHDсHD��HD���D�>�DҀ D��HD�HD�@ DӁHD��HD�HD�@ DԀ DԾ�D�  D�@ DՀ Dվ�D���D�@ DցHD�� D�  D�@ D�~�D׾�D�  D�@ D؁HD�� D�  D�AHDفHD�D�HD�@ Dڀ Dھ�D��qD�=qD�~�D�� D�  D�AHD܁HD�� D���D�>�D�~�D�� D��D�@ Dހ D��HD�  D�>�D߁HD�D�HD�@ D�� D��HD�HD�>�D�}qD�qD���D�>�D� D�� D�  D�>�D�}qD�qD���D�>�D� D��HD�  D�>�D�~�D徸D���D�@ D�HD�qD�  D�B�D�HD羸D���D�@ D� D�D�  D�@ D�HD�� D���D�>�D�~�D��HD�  D�>�D�~�D�qD�  D�AHD� D��HD�HD�@ D� D�� D���D�>�D� DD��qD�@ D�~�D�� D�  D�=qD�~�D�� D�  D�@ D�HD�� D�  D�@ D�~�D�� D�  D�>�D� D�� D��qD�>�D�HD�� D���D�@ D�� D���D�  D�AHD�� D��HD��D�AHD�� D��qD�  D�AHD�� D���D��qD�@ D�� D���D�HD�/\D�o\?#�
?.{?aG�?�  ?��R?�{?��?�G�@�\@��@(�@.{@8Q�@L��@aG�@h��@}p�@��@�{@�
=@�  @��@��@��H@��@˅@�z�@޸R@�@�\)@���AG�A�
AQ�Ap�AG�A�A=qA{A!�A&ffA*�HA/\)A2�\A7
=A;�A?\)AB�\AG�AK�AN�RAS33AW�AZ=qA^{Ab�\AfffAi��An�RAs33AuAz�HA\)A���A��HA���A�ffA��A�G�A�33A�z�A�{A�  A���A��\A�(�A�{A�\)A���A��HA���A�{A�  A�=qA��
A�A�  A�=qA��
A�ffA���A��HA�z�A�\)A���A�33A�A�  A�=qA�z�A�\)A�G�A�33A�AУ�Aҏ\A���A׮Aٙ�AۅA�{A��A��HA���A�A�=qA�(�A�RA�A��
A�{A���A��A�B   BG�B�\B�B��BffB�B��B
{B\)Bz�B�B33BQ�Bp�B�HB(�B�BffB�B��B�B33Bz�B��B�\B   B!�B"{B#�B$��B%��B'
=B(Q�B)��B*�RB+�
B-G�B.�\B/�B0��B2=qB3�B4��B5��B7
=B8Q�B9G�B:�\B;�
B=�B>=qB?�B@��BBffBC�BD��BF=qBG�BH��BJ=qBK�
BM�BNffBO�
BQG�BR�\BS�
BUG�BV�RBX(�BY�BZ�\B\  B]G�B^�\B_�Ba�Bb�\Bc�
Bd��Bf=qBg�
Bh��Bi�Bk\)Bl��Bm�Bo
=Bpz�Bq�Br�HBt  Bup�Bv�HBw�
Bx��BzffB{�
B|��B~{B\)B�=qB��HB��B�  B��RB�\)B�B�Q�B��HB���B�(�B���B�G�B��B�ffB��HB��B�{B��\B��B�B�Q�B���B�G�B��B�ffB��HB�\)B�  B�z�B��HB�p�B�{B�z�B���B��B�{B�z�B���B�p�B�  B�ffB��HB�p�B��B�ffB���B�G�B��B�ffB���B�G�B��B�ffB���B�\)B��B�z�B��B���B�(�B���B�p�B�{B��\B�33B��
B��\B�G�B��
B�ffB��B��
B���B�33B��B��\B�G�B�(�B��HB��B�=qB���B��B��\B�G�B�  B��RB�\)B�=qB�
=B��
B���B�\)B�{B���B�B��\B�\)B�{B��HB��B�z�B�\)B�(�B��HB���B�z�B�G�B�(�B���BŮB�ffB�33B�  B��HBɮB�z�B�\)B�{B��HBͮBΏ\B�\)B�Q�B��B��Bң�B�p�B�Q�B�33B�  B���BׅB�Q�B�33B�{B���B��
Bܣ�B݅B�Q�B��B��
B��B�B�=qB�
=B�B�ffB���B�p�B�{B�\B��B癚B�  B�ffB�RB���B�G�B�B�  B�ffB�RB�
=B�\)B�B��B�=qB�\B��HB�33B�B��B�(�B�ffB��B���B�33B�B��
B�(�B�ffB��B��HB��B�\)B�B��
B�=qB�\B��HB��B�\)B�B��B�(�B�ffB��RB���B�G�B���B��B�(�B�ffB���B���B�33B��B��
B�(�B�z�B���B�
=B�\)B���B��
B�(�B�ffB��RB���B�G�B���B��B�=qB�z�B���B��B�\)B�B�  B�Q�B���B���B�G�B���B��
C {C =qC ffC ��C C �C�CG�C�C�C�C�C\)C��C��C  C=qCp�C��C�HC{CG�Cz�C�RC�C�CQ�C�CC��C33CffC��C��C
=C=qCp�C�C�C�C\)C��C�
C	
=C	G�C	�C	C
  C
=qC
�C
C  C=qCz�C�RC��C33Cz�CC  CG�C�C�
C
=CQ�C��C�HC�CffC�C�C(�CffC�C��C33Cp�C�RC��C33Cz�C�RC��C33Cp�C�C�C(�Cp�C�C�C(�CffC��C�HC�C\)C��C�HC�C\)C��C�HC�C\)C��C�HC�C\)C��C�HC�C\)C��C�HC�C\)C��C�C(�CffC��C�C(�CffC��C�HC(�CffC��C�HC�CffC��C�HC �C \)C ��C �
C!{C!\)C!��C!�
C"{C"Q�C"��C"�
C#�C#ffC#��C#�C$33C$p�C$�RC$�C%(�C%p�C%�C%�HC&�C&ffC&��C&�HC'(�C'ffC'��C'�HC((�C(p�C(��C(�HC)(�C)ffC)��C)�C*(�C*ffC*�C*�C+(�C+p�C+��C+�C,(�C,ffC,��C,�HC-�C-\)C-��C-�HC.�C.ffC.��C.�C/33C/p�C/�C/�C0(�C0ffC0��C0�
C1{C1\)C1��C1�
C2{C2Q�C2��C2�HC3�C3ffC3�C3�C4(�C4p�C4�C4��C533C5p�C5C6  C6Q�C6��C6�C733C7z�C7C8
=C8Q�C8�\C8�
C9(�C9z�C9C:{C:\)C:�C:�C;33C;z�C;C<  C<G�C<�\C<�
C=�C=p�C=C>
=C>Q�C>��C>�HC?(�C?ffC?�RC@  C@Q�C@��C@��CA=qCA�\CA�HCB33CBz�CBCC{CCffCC�CC��CDG�CD�\CD�CE33CE�\CE�HCF(�CF�CF�
CG(�CGz�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                      ?��?��H@:�H@}p�@�G�@�  @�G�A ��A  A   A+�A@  A_\)A�Q�A�  A��A���A���A�Q�A�  A�  B   B�
B�B�
B (�B(Q�B0Q�B8(�B@  BH  BP(�BXQ�B`(�Bh(�Bp(�Bw�
B�  B��B�  B��B��B��
B�  B�{B��B��B�  B�{B�{B�{B��B��B��B��B�{B�{B�  B��B��
B�  B�{B�(�B�=qB�{B�{B�{B�{B�  B��
C��C��C  C��C	�C�C  C  C  C��C  C
=C  C  C  C 
=C!��C#��C&  C'��C*  C,  C.
=C0  C2  C4
=C6  C7��C9��C;��C=�C?��CB  CD  CF
=CH{CJ{CL  CM�CO��CR
=CT{CV
=CX
=CZ{C\{C^
=C`  Cb
=Cd  Ce��Ch  Cj
=Cl
=Cn
=Cp  Cr
=Ct{Cv
=Cx  Cy��C{��C~
=C�
=C�C�  C�C�  C���C�  C�C�C�C�C�  C�  C�  C�  C�C�  C���C���C�  C���C���C�C�C���C���C�  C�C�C�  C���C�  C�C�C�  C�C�C�  C���C�  C���C���C���C���C���C�C�
=C�C�C�  C�  C�  C�C�  C�  C�  C�C�C�
=C���C�  C�  C���C�  C�  C�  C�  C�  C�C���C�  C�  C�  C�  C�  C�C���C���C���C���C�  C�C�  C���C���C�  C�C���C�  C�C�  C�C�  C���C�  C���C���C���C�C�  C�  C�  C���C���C���C�C�
=C�
=C�
=C�  C�C�
=C�  C�C�
=C�C���C�C�
=C�C�  C���C���C�C�  C���C���C�  D �D � D  D� D�qD}qD�qD}qD��Dz�D��Dz�D  D��D  DxRD�qD��D	�D	� D
  D
� D  D� D�qD}qD�qD}qD��D� DD��D  D}qD��D� D�D��D  D}qD  D�D  D}qD�qD� D��D}qD  D� D  Dz�D��D}qD  D��DD�D�qD� D  D� D  D� D �D ��D!D!��D!�qD"}qD#�D#�D$  D$� D%�D%� D&  D&� D'  D'}qD'�qD(� D)  D)��D*  D*}qD*�qD+� D+�qD,}qD-  D-��D.  D.}qD/  D/� D/�qD0}qD0�qD1}qD1�qD2}qD2�qD3� D4  D4� D5  D5� D6  D6}qD6��D7}qD8  D8}qD8��D9� D:�D:� D;  D;}qD;�qD<}qD=  D=}qD>  D>�D?  D?� D@  D@� DA�DA� DA��DB� DC�DC� DD�DD� DD�qDE��DF  DF� DG�DG��DH  DH� DI�DI}qDJ  DJ��DK  DK��DL�DL��DM  DMz�DN  DN� DO  DO}qDP  DP��DQ�DQ� DR  DR��DSDS��DT  DT� DU  DU}qDU�qDV� DW�DW� DX  DX� DX�qDY}qDZ  DZ��D[  D[� D\  D\}qD\�qD]}qD]�qD^��D_D_��D`  D`� Da�Da��Db�Db}qDc  Dc� Dc�qDd}qDd�qDe� De�qDf� Dg  Dg� Dh  Dh}qDh�qDi}qDi�qDj}qDk  Dk� Dl�Dl� Dl�qDm� Dn�Dn��Do  Do��Dp�Dp}qDp�qDq}qDr  Dr��Dr�qDsz�Ds�qDt� Du�Du�Dv�Dv��Dw�Dw�Dx�Dx}qDx�qDy��Dz�Dz� D{  D{� D{�qD|� D}�D}��D~  D~� D~�qD}qD�qD�@ D�� D��qD��qD�@ D�� D��HD��D�@ D�}qD���D�HD�AHD��HD�� D�HD�B�D�� D��HD�  D�>�D�~�D�� D�HD�B�D��HD�� D�  D�>�D�� D��HD��D�B�D�� D�� D�  D�@ D�� D�� D�HD�>�D�� D��HD���D�@ D��HD�� D���D�@ D��HD�� D���D�>�D��HD��HD��D�@ D�}qD���D�HD�AHD��HD�� D���D�@ D��HD��HD�HD�@ D�~�D��qD�  D�B�D��HD�� D�  D�>�D��HD�� D�  D�B�D���D��HD�HD�AHD��HD�� D�  D�@ D�� D��HD��D�AHD�� D�� D���D�>�D�~�D�� D�HD�>�D�~�D���D�  D�B�D���D�� D���D�@ D��HD���D��qD�>�D�~�D���D���D�>�D�~�D���D�  D�@ D�� D�� D���D�=qD�~�D�� D�HD�>�D�}qD�� D���D�=qD�� D�� D�HD�@ D�}qD���D�HD�B�D�� D���D�  D�AHD�� D���D�  D�B�D��HD�� D�HD�@ D�~�D��HD�HD�>�D�� D�D�HD�@ D���D��HD�  D�AHD��HD�D�HD�@ D��HD�D�  D�>�D�~�D�� D�HD�AHD���D��HD���D�@ D��HD�� D���D�>�D�}qD���D�  D�AHD�� D���D�  D�AHD�� D�� D�HD�@ D�~�D��HD�  D�>�D��HD��HD���D�@ D�� D��HD�HD�@ D�� D�� D�HD�AHD�~�D�� D�HD�AHD�� D��qD�HD�AHD�� D���D�  D�AHD�~�D��qD�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD��HD�� D���D�AHD���D��HD�  D�>�D�� D��HD�HD�@ D��HD�D��D�AHD�� D�� D�  D�@ D�� D�� D��qD�>�D D��HD�HD�AHD�~�DýqD���D�@ DĀ Dľ�D���D�>�Dŀ Dž�D���D�@ D�~�D�� D��D�@ D�~�DǾ�D�  D�>�D�~�D�� D�  D�@ Dɀ D�� D�  D�AHDʁHD�� D�HD�AHDˀ D˾�D��qD�>�D̀ D�� D�  D�@ D́HD�� D�  D�>�D΀ Dξ�D���D�>�D�~�D�� D��D�AHD�~�Dо�D�  D�AHDсHD��HD���D�>�DҀ D��HD�HD�@ DӁHD��HD�HD�@ DԀ DԾ�D�  D�@ DՀ Dվ�D���D�@ DցHD�� D�  D�@ D�~�D׾�D�  D�@ D؁HD�� D�  D�AHDفHD�D�HD�@ Dڀ Dھ�D��qD�=qD�~�D�� D�  D�AHD܁HD�� D���D�>�D�~�D�� D��D�@ Dހ D��HD�  D�>�D߁HD�D�HD�@ D�� D��HD�HD�>�D�}qD�qD���D�>�D� D�� D�  D�>�D�}qD�qD���D�>�D� D��HD�  D�>�D�~�D徸D���D�@ D�HD�qD�  D�B�D�HD羸D���D�@ D� D�D�  D�@ D�HD�� D���D�>�D�~�D��HD�  D�>�D�~�D�qD�  D�AHD� D��HD�HD�@ D� D�� D���D�>�D� DD��qD�@ D�~�D�� D�  D�=qD�~�D�� D�  D�@ D�HD�� D�  D�@ D�~�D�� D�  D�>�D� D�� D��qD�>�D�HD�� D���D�@ D�� D���D�  D�AHD�� D��HD��D�AHD�� D��qD�  D�AHD�� D���D��qD�@ D�� D���D�HD�/\G�O�?#�
?.{?aG�?�  ?��R?�{?��?�G�@�\@��@(�@.{@8Q�@L��@aG�@h��@}p�@��@�{@�
=@�  @��@��@��H@��@˅@�z�@޸R@�@�\)@���AG�A�
AQ�Ap�AG�A�A=qA{A!�A&ffA*�HA/\)A2�\A7
=A;�A?\)AB�\AG�AK�AN�RAS33AW�AZ=qA^{Ab�\AfffAi��An�RAs33AuAz�HA\)A���A��HA���A�ffA��A�G�A�33A�z�A�{A�  A���A��\A�(�A�{A�\)A���A��HA���A�{A�  A�=qA��
A�A�  A�=qA��
A�ffA���A��HA�z�A�\)A���A�33A�A�  A�=qA�z�A�\)A�G�A�33A�AУ�Aҏ\A���A׮Aٙ�AۅA�{A��A��HA���A�A�=qA�(�A�RA�A��
A�{A���A��A�B   BG�B�\B�B��BffB�B��B
{B\)Bz�B�B33BQ�Bp�B�HB(�B�BffB�B��B�B33Bz�B��B�\B   B!�B"{B#�B$��B%��B'
=B(Q�B)��B*�RB+�
B-G�B.�\B/�B0��B2=qB3�B4��B5��B7
=B8Q�B9G�B:�\B;�
B=�B>=qB?�B@��BBffBC�BD��BF=qBG�BH��BJ=qBK�
BM�BNffBO�
BQG�BR�\BS�
BUG�BV�RBX(�BY�BZ�\B\  B]G�B^�\B_�Ba�Bb�\Bc�
Bd��Bf=qBg�
Bh��Bi�Bk\)Bl��Bm�Bo
=Bpz�Bq�Br�HBt  Bup�Bv�HBw�
Bx��BzffB{�
B|��B~{B\)B�=qB��HB��B�  B��RB�\)B�B�Q�B��HB���B�(�B���B�G�B��B�ffB��HB��B�{B��\B��B�B�Q�B���B�G�B��B�ffB��HB�\)B�  B�z�B��HB�p�B�{B�z�B���B��B�{B�z�B���B�p�B�  B�ffB��HB�p�B��B�ffB���B�G�B��B�ffB���B�G�B��B�ffB���B�\)B��B�z�B��B���B�(�B���B�p�B�{B��\B�33B��
B��\B�G�B��
B�ffB��B��
B���B�33B��B��\B�G�B�(�B��HB��B�=qB���B��B��\B�G�B�  B��RB�\)B�=qB�
=B��
B���B�\)B�{B���B�B��\B�\)B�{B��HB��B�z�B�\)B�(�B��HB���B�z�B�G�B�(�B���BŮB�ffB�33B�  B��HBɮB�z�B�\)B�{B��HBͮBΏ\B�\)B�Q�B��B��Bң�B�p�B�Q�B�33B�  B���BׅB�Q�B�33B�{B���B��
Bܣ�B݅B�Q�B��B��
B��B�B�=qB�
=B�B�ffB���B�p�B�{B�\B��B癚B�  B�ffB�RB���B�G�B�B�  B�ffB�RB�
=B�\)B�B��B�=qB�\B��HB�33B�B��B�(�B�ffB��B���B�33B�B��
B�(�B�ffB��B��HB��B�\)B�B��
B�=qB�\B��HB��B�\)B�B��B�(�B�ffB��RB���B�G�B���B��B�(�B�ffB���B���B�33B��B��
B�(�B�z�B���B�
=B�\)B���B��
B�(�B�ffB��RB���B�G�B���B��B�=qB�z�B���B��B�\)B�B�  B�Q�B���B���B�G�B���B��
C {C =qC ffC ��C C �C�CG�C�C�C�C�C\)C��C��C  C=qCp�C��C�HC{CG�Cz�C�RC�C�CQ�C�CC��C33CffC��C��C
=C=qCp�C�C�C�C\)C��C�
C	
=C	G�C	�C	C
  C
=qC
�C
C  C=qCz�C�RC��C33Cz�CC  CG�C�C�
C
=CQ�C��C�HC�CffC�C�C(�CffC�C��C33Cp�C�RC��C33Cz�C�RC��C33Cp�C�C�C(�Cp�C�C�C(�CffC��C�HC�C\)C��C�HC�C\)C��C�HC�C\)C��C�HC�C\)C��C�HC�C\)C��C�HC�C\)C��C�C(�CffC��C�C(�CffC��C�HC(�CffC��C�HC�CffC��C�HC �C \)C ��C �
C!{C!\)C!��C!�
C"{C"Q�C"��C"�
C#�C#ffC#��C#�C$33C$p�C$�RC$�C%(�C%p�C%�C%�HC&�C&ffC&��C&�HC'(�C'ffC'��C'�HC((�C(p�C(��C(�HC)(�C)ffC)��C)�C*(�C*ffC*�C*�C+(�C+p�C+��C+�C,(�C,ffC,��C,�HC-�C-\)C-��C-�HC.�C.ffC.��C.�C/33C/p�C/�C/�C0(�C0ffC0��C0�
C1{C1\)C1��C1�
C2{C2Q�C2��C2�HC3�C3ffC3�C3�C4(�C4p�C4�C4��C533C5p�C5C6  C6Q�C6��C6�C733C7z�C7C8
=C8Q�C8�\C8�
C9(�C9z�C9C:{C:\)C:�C:�C;33C;z�C;C<  C<G�C<�\C<�
C=�C=p�C=C>
=C>Q�C>��C>�HC?(�C?ffC?�RC@  C@Q�C@��C@��CA=qCA�\CA�HCB33CBz�CBCC{CCffCC�CC��CDG�CD�\CD�CE33CE�\CE�HCF(�CF�CF�
CG(�CGz�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                      @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@�tG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�+A�-A�+A�-A�33A�33A�5?A�5?A�33A�5?A�7LA�7LA�5?A�7LA�9XA�;dA�9XA�?}A�A�A�C�A�;dA�+A�-A�/A�;dA�/A�+A�(�A�+A�/A�5?A� �A�bA�1A�  A���A���A���A���A���A��A��A���Aѕ�Aͥ�A��A��A�A��\A�&�A��A��A���A���A�r�A�C�A�|�A�\)A�VA���A�A�A�dZA���A��A�`BA��\A��^A��PA���A��A�G�A�C�A�C�A�+A�jA��7A�A��A��`A���A{"�Au\)AoVAjI�AgAb�A^(�AY"�AWl�AU�AQ�PANM�AO�AQp�AQ�TANz�AF�HAC
=AA�mAA��A@ĜA>-A;��A: �A8��A7�A5VA3�A2��A1+A0-A/��A/��A.Q�A.^5A-��A-�
A-�A-/A,Q�A+�TA+XA*��A*$�A)��A)+A(�A&��A%�wA$��A$��A%%A%�A$��A$ĜA$^5A"$�A!\)A!
=A �A ��A 1'A�hA�A  A�7A�HAv�A��A
=Az�A5?A�A��A��At�A��Av�A1'A�-A��Al�A"�A%A�`AVA�RAbNA�;A�^A�A?}A�`AI�AA&�A
=A�HAjA^5AJA�-A��AhsA33A�HA�jAbNA�A�A��A�A\)A�yA��A�+Av�Ar�AA�A�;AhsA
=A�AA�A9XAJA�
A��A
��A
E�A
1A	�wA	dZA�A��AI�A$�A{A1A�A�A?}A�RA~�A�A&�Az�A�A�PAO�A"�A�+AJA�
A�
A��Al�AoA �9A A�A @��;@�33@���@�7L@���@��9@�9X@�C�@�
=@���@�{@�?}@���@��u@�1'@�n�@�&�@��@���@�bN@�Z@�Q�@�b@��
@�@��y@�5?@���@�-@�?}@���@�Ĝ@�(�@�\)@�R@�^5@�J@��@��T@��#@�&�@�j@�Z@�ƨ@�R@�J@�7@�/@��@�j@���@�dZ@�v�@��@��T@�hs@��/@�Z@�F@�ff@��@�p�@��@���@�9@��@��@���@�S�@��H@�M�@�J@��#@ݙ�@���@���@��@ڇ+@�hs@��@�S�@�-@Ձ@�&�@Լj@�Ĝ@�Ĝ@�Z@���@���@�ƨ@ӶF@ӍP@���@с@�/@���@мj@�Q�@�@�-@��T@�O�@�Ĝ@�bN@��@��m@˅@�"�@���@��@�G�@���@�  @��;@��
@ǶF@�;d@�@�V@�-@�J@��@ũ�@�x�@�7L@�V@Ĭ@�dZ@�5?@��T@��-@��7@�X@��/@�I�@�1@��
@�C�@��R@�ff@�@���@��9@�b@��@���@�|�@�33@���@��@�p�@���@�(�@��@�dZ@��@�
=@���@��@��@�`B@��@��/@�Z@���@��;@��;@���@��@��@�V@��h@�?}@�/@�%@���@�Z@���@�ƨ@���@�
=@�v�@�^5@�n�@�^5@��@�J@�@��@��-@���@�hs@�Q�@� �@��w@�;d@���@�-@�{@�{@�J@�J@���@�%@��F@�\)@�S�@�C�@�o@�@�@���@��+@�M�@��^@�V@��9@���@���@�dZ@�C�@��@���@�ff@��@��-@�x�@�&�@�Q�@�  @��
@��
@�ƨ@���@�;d@�@�v�@�V@�5?@���@���@�7L@�&�@�V@�Ĝ@�z�@�A�@�1@��m@���@��@�l�@�"�@���@�v�@�V@�J@�&�@��/@��9@�j@�1@���@�ƨ@�ƨ@��F@��@���@��R@���@�M�@��T@��h@�O�@���@�r�@���@���@��@�;d@�@�M�@�@�@��@���@�G�@���@�  @��F@�|�@�l�@�dZ@�C�@�"�@��@���@�V@�J@��T@���@�X@���@��/@��j@��u@�bN@�(�@�1@��@���@�33@�@���@���@�E�@��@��7@�G�@�/@��@��`@��j@��@�Q�@�b@��m@��@�|�@�;d@���@��H@���@�ff@�^5@�M�@�$�@��@��-@��@�`B@�`B@�G�@��@�Ĝ@�j@�b@���@�"�@��y@���@�~�@�^5@�5?@�{@��#@��h@�hs@�?}@�&�@���@���@��9@�z�@�b@��@;d@~ȴ@~ff@}�@}�@}O�@}V@|�D@|�@{�
@{t�@z�@y�@y��@yX@xb@v��@vȴ@v�R@v��@v�+@vff@vE�@u��@uV@t�@t(�@st�@r��@r=q@q��@q&�@pr�@pQ�@p �@o�P@o
=@n��@nV@n$�@m�-@m�@l�D@l9X@l1@k�m@kƨ@k��@k33@j�!@jn�@i��@i�7@ix�@iG�@hĜ@h �@g�@f��@fȴ@f$�@e�@d�@d��@d�@d��@d�D@d1@c��@c��@cS�@co@bn�@a�7@aG�@`��@`Q�@_�@_|�@_;d@_
=@^�R@]��@]V@\z�@\1@[t�@[@Z�!@Z~�@Z^5@ZM�@ZM�@Y�7@X��@X��@X��@X��@XbN@W�@W+@V5?@U`B@UV@T��@Tj@T(�@T�@S��@S�F@S��@SC�@S@Rn�@Q��@P�`@P�u@PA�@O�w@N��@N��@Nff@NE�@M@M�@Lj@L1@K�F@Kt�@Ko@J��@J=q@I�@I��@I�^@Ix�@I�@H�@H �@G�P@G
=@F@E`B@E?}@E/@E�@E�@EV@D�@D�D@DI�@D�@C��@C�m@C�F@CdZ@B��@Bn�@A��@A��@AX@A&�@@��@@�@@ �@?�@?��@?�P@?K�@?+@>��@>�@>��@>E�@=�@=V@<�/@<�j@<��@<j@;�m@;�F@;��@;C�@:��@:�\@:-@9�^@9�7@9�7@9x�@9x�@97L@9%@8�9@8A�@8 �@7�@7�w@7K�@7
=@6�R@6v�@6$�@5�-@4�/@4��@4Z@4I�@49X@4�@3�m@3�
@3C�@2�!@2^5@2-@1��@2�@2�@1�@1�#@1�@1�^@1hs@0�9@0�u@0�@0r�@0A�@0  @/�@/l�@.��@.��@.�y@.�@.ȴ@.��@.V@.$�@.$�@.{@-�@-��@-p�@-`B@-�@,��@,j@,�@+ƨ@+��@+dZ@+"�@*�@*n�@*^5@*�@)�^@)x�@)X@)7L@)7L@)&�@)&�@)%@(Ĝ@(�u@(r�@(A�@'�;@'�;@'�;@'�w@'l�@';d@&�y@&�+@&ff@&@%�T@%�h@%�@%�@%`B@$��@$�D@$9X@#��@#�m@#�
@#�F@#�F@#�F@#t�@#C�@#C�@#C�@#@"�!@"-@!��@!�^@!��@!&�@ r�@ A�@ 1'@ A�@  �@��@��@l�@�@�R@��@v�@{@�T@�h@/@�/@��@z�@I�@�@�
@�F@�@t�@33@@�H@�H@�!@��@~�@M�@-@J@�^@hs@%@��@�9@��@Q�@1'@�@�P@K�@;d@+@
=@�@ȴ@�R@��@�+@5?@5?@$�@$�@�@�@?}@��@j@I�@(�@�@��@��@dZ@o@�H@��@^5@-@��@�@G�@%@Ĝ@Q�@1'@ �@  @�;@�@\)@K�@;d@+@
=@��@ȴ@�R@�RA�(�A�(�A�+A�(�A�-A�+A�+A�/A�+A�/A�/A�+A�+A�/A�+A�+A�/A�1'A�/A�1'A�5?A�1'A�1'A�5?A�1'A�33A�5?A�7LA�33A�5?A�9XA�33A�1'A�5?A�5?A�33A�33A�7LA�33A�33A�7LA�;dA�5?A�7LA�;dA�9XA�9XA�;dA�;dA�/A�1'A�5?A�1'A�1'A�7LA�7LA�33A�7LA�7LA�5?A�5?A�;dA�9XA�5?A�;dA�;dA�5?A�5?A�7LA�7LA�5?A�;dA�?}A�;dA�;dA�;dA�5?A�5?A�;dA�9XA�7LA�9XA�?}A�=qA�;dA�9XA�=qA�9XA�7LA�9XA�9XA�7LA�9XA�?}A�=qA�=qA�C�A�A�A�;dA�?}A�A�A�=qA�A�A�C�A�A�A�;dA�G�A�K�A�C�A�C�A�E�A�G�A�?}A�=qA�A�A�=qA�=qA�=qA�=qA�9XA�7LA�33A�+A�(�A�+A�/A�+A�&�A�-A�(�A�(�A�(�A�5?A�33A�-A�/A�-A�&�A�1'A�;dA�7LA�9XA�7LA�7LA�5?A�C�A�E�A�5?A�/A�1'A�1'A�+A�-A�/A�+A�-A�1'A�1'A�(�A�$�A�(�A�+A�$�A� �A�(�A�1'A�+A�+A�-A�+A�&�A�&�A�-A�-A�+A�-A�/A�33A�33A�33A�7LA�7LA�7LA�33A�33A�1'A�-A�/A�(�A��A�bA�VA��A�{A�{A�JA�
=A�
=A�%A�A�1A�
=A�1A�A�A�A�A���A���A�  A�  A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��TA��`A��/A���A���A�ĜA�ĜA���AѲ-AѰ!AѰ!AѮAѧ�Aѝ�A�n�A�7LA��yA�-AϬA���A͉7A�v�A�{A�A�bNA��A�\)A���AŇ+A��Ağ�AăA�~�A�bNA��;A�r�A� �A��;A���A�ƨAº^A¬APA�;dA��jA�C�A�9XA�5?A�5?A�5?A�$�A� �A�$�A��A�JA�  A��A���A��A���A��\A�|�A�p�A�^5A�=qA���A��wA�t�A�C�A��A��HA���A�1'A���A��uA��A�z�A�O�A�;dA��/A�l�A�C�A�$�A��9A��`A�{A��PA���A�9XA�VA��
A�33A���A��A�Q�A�(�A��yA���A���A�z�A�1A��DA�
=A���A�jA�K�A�33A�  A�M�A��A���A�bA��PA�/A��;A���A��PA��A�r�A�VA�I�A�;dA�/A�+A�$�A��A�VA���A��mA��#A�ĜA��RA���A���A���A�A���A�\)A�"�A��A��-A�I�A��
A�~�A�-A���A���A��+A�z�A�l�A�dZA�ZA�5?A��A�{A�bA�VA�  A��TA��FA�M�A��
A�$�A�jA��TA���A�z�A�^5A�C�A� �A�A��/A�ƨA���A�l�A�&�A��`A���A�z�A�K�A�(�A��A���A�`BA�33A�  A��
A��^A���A�v�A�/A���A��jA�S�A�9XA��A���A�ZA�(�A��/A��-A��+A�l�A�A�A��A���A���A��A���A�;dA��A��FA��^A�z�A�ffA�bNA��A���A���A��HA���A�ȴA��9A���A�p�A�O�A��A���A��uA�bNA�+A���A���A���A�hsA�$�A��!A�Q�A�/A��A�A��#A�ƨA�|�A��#A���A�(�A�7LA�
=A�G�A���A�p�A�&�A���A�jA�%A���A�VA���A��9A��A��A���A���A��DA�XA�n�A���A��\A�\)A�G�A�?}A�oA��jA��uA�x�A�ffA�`BA�M�A�1'A� �A�VA��TA��;A���A���A�ȴA��^A��FA��A���A���A���A��\A��A�VA~1A}33A|~�A{�hAzz�Ayp�Ax��Axz�Ax5?Aw��Av=qAuO�At�At~�AtJAs�7ArVAqVAo�;Ao�An�AnAmdZAlM�Ak�Ak�#AjĜAi��Ai�7Ai�AidZAh�AhM�Ag�mAg�7Af�yAf�AfI�Af=qAf$�Ae��Ad�Ac|�AcAbz�AbJAa�AaVA`��A`z�A_�A_G�A^M�A\r�A[�AZ��AZM�AZbAYƨAY|�AX�AX{AW�mAW�;AW��AW��AW�AWx�AWdZAWG�AW&�AV��AVĜAV��AVI�AVJAU�
AU�AT��AT��ATI�AS�;ASC�AR�AQ7LAPv�AN�ANffAN9XAN1'AN9XAN=qANE�ANI�ANn�AN��AN��AO
=AO�AOx�APJAP�/AP�`AQAP��AQ�AQS�AQS�AQ�AQ�-AQ�FAQ�AQ��AQ��AQ��AQ��AQ��AQ�AQ��AQ��AQ+AP��APA�AO�wAN5?AMAK�AJ��AI&�AH  AGO�AF�\AF  AD��AD�+ADJACdZAC/AB�yAB�!ABn�ABE�AB �ABAA��AA�
AAAA�^AA�FAA�AA��AA��AA��AA��AA�7AA`BAAK�AA�A@��A@�jA@�+A@A�A@JA?��A?K�A>�+A=�
A=S�A<��A<�RA<E�A;�A;�^A;�hA;O�A;�A:��A:�+A:^5A:�A9�A9�-A9x�A9XA9;dA9�A8�A8�jA8n�A8�A7�#G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                      A�+A�-A�+A�-A�33A�33A�5?A�5?A�33A�5?A�7LA�7LA�5?A�7LA�9XA�;dA�9XA�?}A�A�A�C�A�;dA�+A�-A�/A�;dA�/A�+A�(�A�+A�/A�5?A� �A�bA�1A�  A���A���A���A���A���A��A��A���Aѕ�Aͥ�A��A��A�A��\A�&�A��A��A���A���A�r�A�C�A�|�A�\)A�VA���A�A�A�dZA���A��A�`BA��\A��^A��PA���A��A�G�A�C�A�C�A�+A�jA��7A�A��A��`A���A{"�Au\)AoVAjI�AgAb�A^(�AY"�AWl�AU�AQ�PANM�AO�AQp�AQ�TANz�AF�HAC
=AA�mAA��A@ĜA>-A;��A: �A8��A7�A5VA3�A2��A1+A0-A/��A/��A.Q�A.^5A-��A-�
A-�A-/A,Q�A+�TA+XA*��A*$�A)��A)+A(�A&��A%�wA$��A$��A%%A%�A$��A$ĜA$^5A"$�A!\)A!
=A �A ��A 1'A�hA�A  A�7A�HAv�A��A
=Az�A5?A�A��A��At�A��Av�A1'A�-A��Al�A"�A%A�`AVA�RAbNA�;A�^A�A?}A�`AI�AA&�A
=A�HAjA^5AJA�-A��AhsA33A�HA�jAbNA�A�A��A�A\)A�yA��A�+Av�Ar�AA�A�;AhsA
=A�AA�A9XAJA�
A��A
��A
E�A
1A	�wA	dZA�A��AI�A$�A{A1A�A�A?}A�RA~�A�A&�Az�A�A�PAO�A"�A�+AJA�
A�
A��Al�AoA �9A A�A @��;@�33@���@�7L@���@��9@�9X@�C�@�
=@���@�{@�?}@���@��u@�1'@�n�@�&�@��@���@�bN@�Z@�Q�@�b@��
@�@��y@�5?@���@�-@�?}@���@�Ĝ@�(�@�\)@�R@�^5@�J@��@��T@��#@�&�@�j@�Z@�ƨ@�R@�J@�7@�/@��@�j@���@�dZ@�v�@��@��T@�hs@��/@�Z@�F@�ff@��@�p�@��@���@�9@��@��@���@�S�@��H@�M�@�J@��#@ݙ�@���@���@��@ڇ+@�hs@��@�S�@�-@Ձ@�&�@Լj@�Ĝ@�Ĝ@�Z@���@���@�ƨ@ӶF@ӍP@���@с@�/@���@мj@�Q�@�@�-@��T@�O�@�Ĝ@�bN@��@��m@˅@�"�@���@��@�G�@���@�  @��;@��
@ǶF@�;d@�@�V@�-@�J@��@ũ�@�x�@�7L@�V@Ĭ@�dZ@�5?@��T@��-@��7@�X@��/@�I�@�1@��
@�C�@��R@�ff@�@���@��9@�b@��@���@�|�@�33@���@��@�p�@���@�(�@��@�dZ@��@�
=@���@��@��@�`B@��@��/@�Z@���@��;@��;@���@��@��@�V@��h@�?}@�/@�%@���@�Z@���@�ƨ@���@�
=@�v�@�^5@�n�@�^5@��@�J@�@��@��-@���@�hs@�Q�@� �@��w@�;d@���@�-@�{@�{@�J@�J@���@�%@��F@�\)@�S�@�C�@�o@�@�@���@��+@�M�@��^@�V@��9@���@���@�dZ@�C�@��@���@�ff@��@��-@�x�@�&�@�Q�@�  @��
@��
@�ƨ@���@�;d@�@�v�@�V@�5?@���@���@�7L@�&�@�V@�Ĝ@�z�@�A�@�1@��m@���@��@�l�@�"�@���@�v�@�V@�J@�&�@��/@��9@�j@�1@���@�ƨ@�ƨ@��F@��@���@��R@���@�M�@��T@��h@�O�@���@�r�@���@���@��@�;d@�@�M�@�@�@��@���@�G�@���@�  @��F@�|�@�l�@�dZ@�C�@�"�@��@���@�V@�J@��T@���@�X@���@��/@��j@��u@�bN@�(�@�1@��@���@�33@�@���@���@�E�@��@��7@�G�@�/@��@��`@��j@��@�Q�@�b@��m@��@�|�@�;d@���@��H@���@�ff@�^5@�M�@�$�@��@��-@��@�`B@�`B@�G�@��@�Ĝ@�j@�b@���@�"�@��y@���@�~�@�^5@�5?@�{@��#@��h@�hs@�?}@�&�@���@���@��9@�z�@�b@��@;d@~ȴ@~ff@}�@}�@}O�@}V@|�D@|�@{�
@{t�@z�@y�@y��@yX@xb@v��@vȴ@v�R@v��@v�+@vff@vE�@u��@uV@t�@t(�@st�@r��@r=q@q��@q&�@pr�@pQ�@p �@o�P@o
=@n��@nV@n$�@m�-@m�@l�D@l9X@l1@k�m@kƨ@k��@k33@j�!@jn�@i��@i�7@ix�@iG�@hĜ@h �@g�@f��@fȴ@f$�@e�@d�@d��@d�@d��@d�D@d1@c��@c��@cS�@co@bn�@a�7@aG�@`��@`Q�@_�@_|�@_;d@_
=@^�R@]��@]V@\z�@\1@[t�@[@Z�!@Z~�@Z^5@ZM�@ZM�@Y�7@X��@X��@X��@X��@XbN@W�@W+@V5?@U`B@UV@T��@Tj@T(�@T�@S��@S�F@S��@SC�@S@Rn�@Q��@P�`@P�u@PA�@O�w@N��@N��@Nff@NE�@M@M�@Lj@L1@K�F@Kt�@Ko@J��@J=q@I�@I��@I�^@Ix�@I�@H�@H �@G�P@G
=@F@E`B@E?}@E/@E�@E�@EV@D�@D�D@DI�@D�@C��@C�m@C�F@CdZ@B��@Bn�@A��@A��@AX@A&�@@��@@�@@ �@?�@?��@?�P@?K�@?+@>��@>�@>��@>E�@=�@=V@<�/@<�j@<��@<j@;�m@;�F@;��@;C�@:��@:�\@:-@9�^@9�7@9�7@9x�@9x�@97L@9%@8�9@8A�@8 �@7�@7�w@7K�@7
=@6�R@6v�@6$�@5�-@4�/@4��@4Z@4I�@49X@4�@3�m@3�
@3C�@2�!@2^5@2-@1��@2�@2�@1�@1�#@1�@1�^@1hs@0�9@0�u@0�@0r�@0A�@0  @/�@/l�@.��@.��@.�y@.�@.ȴ@.��@.V@.$�@.$�@.{@-�@-��@-p�@-`B@-�@,��@,j@,�@+ƨ@+��@+dZ@+"�@*�@*n�@*^5@*�@)�^@)x�@)X@)7L@)7L@)&�@)&�@)%@(Ĝ@(�u@(r�@(A�@'�;@'�;@'�;@'�w@'l�@';d@&�y@&�+@&ff@&@%�T@%�h@%�@%�@%`B@$��@$�D@$9X@#��@#�m@#�
@#�F@#�F@#�F@#t�@#C�@#C�@#C�@#@"�!@"-@!��@!�^@!��@!&�@ r�@ A�@ 1'@ A�@  �@��@��@l�@�@�R@��@v�@{@�T@�h@/@�/@��@z�@I�@�@�
@�F@�@t�@33@@�H@�H@�!@��@~�@M�@-@J@�^@hs@%@��@�9@��@Q�@1'@�@�P@K�@;d@+@
=@�@ȴ@�R@��@�+@5?@5?@$�@$�@�@�@?}@��@j@I�@(�@�@��@��@dZ@o@�H@��@^5@-@��@�@G�@%@Ĝ@Q�@1'@ �@  @�;@�@\)@K�@;d@+@
=@��@ȴ@�RG�O�A�(�A�(�A�+A�(�A�-A�+A�+A�/A�+A�/A�/A�+A�+A�/A�+A�+A�/A�1'A�/A�1'A�5?A�1'A�1'A�5?A�1'A�33A�5?A�7LA�33A�5?A�9XA�33A�1'A�5?A�5?A�33A�33A�7LA�33A�33A�7LA�;dA�5?A�7LA�;dA�9XA�9XA�;dA�;dA�/A�1'A�5?A�1'A�1'A�7LA�7LA�33A�7LA�7LA�5?A�5?A�;dA�9XA�5?A�;dA�;dA�5?A�5?A�7LA�7LA�5?A�;dA�?}A�;dA�;dA�;dA�5?A�5?A�;dA�9XA�7LA�9XA�?}A�=qA�;dA�9XA�=qA�9XA�7LA�9XA�9XA�7LA�9XA�?}A�=qA�=qA�C�A�A�A�;dA�?}A�A�A�=qA�A�A�C�A�A�A�;dA�G�A�K�A�C�A�C�A�E�A�G�A�?}A�=qA�A�A�=qA�=qA�=qA�=qA�9XA�7LA�33A�+A�(�A�+A�/A�+A�&�A�-A�(�A�(�A�(�A�5?A�33A�-A�/A�-A�&�A�1'A�;dA�7LA�9XA�7LA�7LA�5?A�C�A�E�A�5?A�/A�1'A�1'A�+A�-A�/A�+A�-A�1'A�1'A�(�A�$�A�(�A�+A�$�A� �A�(�A�1'A�+A�+A�-A�+A�&�A�&�A�-A�-A�+A�-A�/A�33A�33A�33A�7LA�7LA�7LA�33A�33A�1'A�-A�/A�(�A��A�bA�VA��A�{A�{A�JA�
=A�
=A�%A�A�1A�
=A�1A�A�A�A�A���A���A�  A�  A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��TA��`A��/A���A���A�ĜA�ĜA���AѲ-AѰ!AѰ!AѮAѧ�Aѝ�A�n�A�7LA��yA�-AϬA���A͉7A�v�A�{A�A�bNA��A�\)A���AŇ+A��Ağ�AăA�~�A�bNA��;A�r�A� �A��;A���A�ƨAº^A¬APA�;dA��jA�C�A�9XA�5?A�5?A�5?A�$�A� �A�$�A��A�JA�  A��A���A��A���A��\A�|�A�p�A�^5A�=qA���A��wA�t�A�C�A��A��HA���A�1'A���A��uA��A�z�A�O�A�;dA��/A�l�A�C�A�$�A��9A��`A�{A��PA���A�9XA�VA��
A�33A���A��A�Q�A�(�A��yA���A���A�z�A�1A��DA�
=A���A�jA�K�A�33A�  A�M�A��A���A�bA��PA�/A��;A���A��PA��A�r�A�VA�I�A�;dA�/A�+A�$�A��A�VA���A��mA��#A�ĜA��RA���A���A���A�A���A�\)A�"�A��A��-A�I�A��
A�~�A�-A���A���A��+A�z�A�l�A�dZA�ZA�5?A��A�{A�bA�VA�  A��TA��FA�M�A��
A�$�A�jA��TA���A�z�A�^5A�C�A� �A�A��/A�ƨA���A�l�A�&�A��`A���A�z�A�K�A�(�A��A���A�`BA�33A�  A��
A��^A���A�v�A�/A���A��jA�S�A�9XA��A���A�ZA�(�A��/A��-A��+A�l�A�A�A��A���A���A��A���A�;dA��A��FA��^A�z�A�ffA�bNA��A���A���A��HA���A�ȴA��9A���A�p�A�O�A��A���A��uA�bNA�+A���A���A���A�hsA�$�A��!A�Q�A�/A��A�A��#A�ƨA�|�A��#A���A�(�A�7LA�
=A�G�A���A�p�A�&�A���A�jA�%A���A�VA���A��9A��A��A���A���A��DA�XA�n�A���A��\A�\)A�G�A�?}A�oA��jA��uA�x�A�ffA�`BA�M�A�1'A� �A�VA��TA��;A���A���A�ȴA��^A��FA��A���A���A���A��\A��A�VA~1A}33A|~�A{�hAzz�Ayp�Ax��Axz�Ax5?Aw��Av=qAuO�At�At~�AtJAs�7ArVAqVAo�;Ao�An�AnAmdZAlM�Ak�Ak�#AjĜAi��Ai�7Ai�AidZAh�AhM�Ag�mAg�7Af�yAf�AfI�Af=qAf$�Ae��Ad�Ac|�AcAbz�AbJAa�AaVA`��A`z�A_�A_G�A^M�A\r�A[�AZ��AZM�AZbAYƨAY|�AX�AX{AW�mAW�;AW��AW��AW�AWx�AWdZAWG�AW&�AV��AVĜAV��AVI�AVJAU�
AU�AT��AT��ATI�AS�;ASC�AR�AQ7LAPv�AN�ANffAN9XAN1'AN9XAN=qANE�ANI�ANn�AN��AN��AO
=AO�AOx�APJAP�/AP�`AQAP��AQ�AQS�AQS�AQ�AQ�-AQ�FAQ�AQ��AQ��AQ��AQ��AQ��AQ�AQ��AQ��AQ+AP��APA�AO�wAN5?AMAK�AJ��AI&�AH  AGO�AF�\AF  AD��AD�+ADJACdZAC/AB�yAB�!ABn�ABE�AB �ABAA��AA�
AAAA�^AA�FAA�AA��AA��AA��AA��AA�7AA`BAAK�AA�A@��A@�jA@�+A@A�A@JA?��A?K�A>�+A=�
A=S�A<��A<�RA<E�A;�A;�^A;�hA;O�A;�A:��A:�+A:^5A:�A9�A9�-A9x�A9XA9;dA9�A8�A8�jA8n�A8�A7�#G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B�$B��B�RB��B��B��B��B��B��B��B��B��B��B�RB��B��B��B��B��B�B��B��B��B�RB��B��B��B��B��B�B��B�B�B��B��B�zB�FB��B��B��B�VB�1B��B҉B	(XB	��B	�PB
3hB
n�B
�bB
�UB
�B
�BhB<B2-B�B �BsB��B��Bp�BK)B�B
��B
ޞB
�B
�B
�=B
��B
qB
YB
/�B
	�B	�oB	�B	��B	��B	�gB	��B	��B	}VB	l�B	b�B	T�B	5?B	$�B	 �B	�B	~B	B�B	b�B	tTB	cB	r�B	]dB	S�B	QNB	M�B	D�B	5�B	,B	$@B	�B	uB	�B	B	oB	 �B	�B	�B	CaB	gmB	zDB	{�B	~�B	��B	�_B	�CB	�B	�OB	�:B	�$B	��B	�XB	�[B	�EB	�B	�BB	یB	�TB	�mB	�KB	��B	�B	�B	� B	�AB	��B	�]B	�B	��B
SB

rB
�B
 B
�B
�B
�B
1B
�B
!�B
"�B
-�B
-B
*�B
,�B
.}B
/�B
1�B
4B
6B
:�B
FB
HKB
IRB
J�B
JXB
J�B
J�B
K�B
LdB
M6B
PB
OvB
OvB
P�B
S&B
S�B
S�B
R�B
Q�B
R�B
Q�B
R�B
P�B
PB
S�B
T,B
T,B
T�B
V9B
S�B
S[B
S&B
R�B
S�B
R B
O�B
NpB
N�B
N�B
N�B
OvB
NpB
OB
M�B
K^B
I�B
I�B
HKB
FtB
GzB
FtB
E9B
D�B
D3B
D3B
C-B
C�B
A�B
?}B
?}B
:�B
:*B
6�B
8�B
6B
5tB
5�B
2�B
4B
4�B
5?B
6FB
5B
4�B
33B
1�B
0�B
2�B
.�B
.}B
,�B
,�B
,qB
,B
)�B
*�B
)_B
)�B
($B
'�B
)�B
)_B
"�B
"�B
#B
!�B
!�B
!bB
!bB
 \B
 'B
B
IB
xB
�B
!�B
!�B
!bB
!�B
 �B
�B
 �B
 'B
�B
�B
�B
 �B
B
�B
OB
�B
�B
�B
7B
B
qB
OB
�B
�B
OB
B
�B
OB
�B
�B
IB
�B
~B
~B
IB
B
�B
�B
B
IB
�B
�B
B
�B
CB
�B
OB
�B
	B
�B
�B
�B
�B
B
@B
�B
MB
�B
�B
YB
�B
SB
SB
�B
MB
B
FB
{B
{B
FB
�B
:B
B
FB
�B
{B
{B
�B
B
�B
�B
�B
SB
�B
�B
�B
�B
�B
�B
B
$B
�B
�B
�B
�B
�B
SB
�B
�B
YB
�B
MB
B
�B
FB
{B
FB
B
B
�B
FB
�B
{B
FB
�B
B
�B
@B
@B
@B
�B
�B
�B
FB
�B
�B
�B
�B
�B
�B
�B
�B
+B
+B
�B
1B
�B
�B
_B
_B
eB
eB
�B
�B
kB
7B
B
	B
�B
kB
�B
1B
�B
7B
7B
kB
=B
�B
B
xB
�B
�B
xB
xB
�B
B
�B
qB
�B
	B
�B
�B
�B
kB
kB
�B
IB
CB
CB
�B
~B
OB
�B
�B
VB
�B
�B
OB
 \B
"�B
"�B
"�B
"�B
"4B
#B
"�B
#:B
"�B
#nB
#nB
#nB
$B
%FB
%B
%B
%B
%�B
%B
%zB
$�B
$�B
%B
%�B
&B
&LB
&�B
&�B
&�B
'B
&�B
&�B
'B
&�B
'RB
'B
'�B
'�B
'�B
($B
(�B
)*B
)_B
)_B
)_B
(�B
(�B
)_B
)�B
)�B
*0B
*eB
*0B
*0B
)�B
)�B
)�B
)_B
)�B
*eB
*eB
+�B
+�B
+6B
,=B
,=B
,�B
-wB
-�B
.�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0!B
0UB
1'B
1[B
1[B
1[B
1�B
2-B
2�B
2�B
2�B
2�B
3hB
3�B
3hB
3hB
4nB
4�B
4nB
4nB
5B
5�B
5�B
6�B
6�B
6�B
6�B
7�B
7LB
7�B
8�B
8�B
8�B
8�B
8�B
9XB
9�B
9$B
9�B
9�B
8�B
9$B
9$B
9�B
9�B
:*B
9�B
9�B
:*B
:�B
:�B
:�B
:�B
;0B
;�B
<B
<6B
<6B
<6B
<�B
<�B
=<B
=qB
=�B
=�B
=�B
>wB
>wB
>BB
>�B
?HB
?�B
@OB
@�B
@�B
AUB
@�B
@�B
@�B
@�B
A B
A�B
A�B
B[B
B�B
A�B
B[B
B'B
B�B
B�B
B�B
B�B
CaB
C-B
B�B
C�B
C�B
C-B
C-B
C�B
C�B
D3B
D3B
C�B
EmB
E9B
EmB
E�B
FB
FB
FB
FB
FtB
GB
GEB
GzB
GzB
G�B
G�B
G�B
HKB
H�B
HB
IB
IB
IB
IB
JXB
J#B
J#B
I�B
J#B
J�B
K)B
J�B
JXB
J#B
J#B
I�B
J�B
JXB
J#B
J#B
JXB
K�B
K�B
K�B
L0B
L�B
L�B
MB
L�B
L�B
L�B
M�B
M�B
NpB
N<B
OB
OB
OvB
OvB
OvB
OBB
OB
P}B
P}B
P}B
P�B
P�B
RTB
R�B
S�B
S[B
S&B
TaB
U2B
U2B
UgB
UgB
V9B
VB
V�B
V9B
V9B
V�B
W�B
WsB
W?B
W?B
W
B
W�B
W
B
WsB
W?B
WsB
W�B
X�B
X�B
X�B
YB
YKB
ZB
ZQB
Z�B
Z�B
Z�B
[�B
[�B
\�B
\]B
[�B
[#B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\]B
\�B
^B
^5B
_;B
_;B
`B
_�B
_;B
_;B
_B
^�B
^�B
^�B
_�B
`�B
aB
aHB
a|B
a|B
a�B
bNB
bB
b�B
b�B
b�B
a�B
a�B
bB
bNB
b�B
c�B
c�B
d&B
d�B
e�B
e�B
f2B
ffB
ffB
f�B
f�B
f�B
f�B
gB
gmB
g�B
g�B
g�B
h
B
hsB
hsB
h�B
iB
iDB
i�B
jKB
jKB
jB
jKB
jB
jB
jKB
jB
jKB
jB
jB
j�B
kB
k�B
l"B
l"B
l"B
l"B
lWB
l�B
l�B
m)B
l�B
l�B
l�B
m)B
m�B
m�B
n/B
m�B
n/B
m�B
m�B
n/B
n�B
n�B
o B
o B
o B
o�B
o�B
oiB
o�B
pB
pB
p;B
p�B
p�B
p�B
p�B
p�B
qB
p�B
qB
qAB
qAB
qAB
qvB
qAB
qvB
qAB
qvB
q�B
rB
rGB
r|B
sB
r�B
r�B
r�B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tTB
tTB
t�B
t�B
uZB
u%B
uZB
u%B
u�B
u�B
u�B
v+B
v`B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
wfB
w2B
w2B
wfB
w�B
x8B
x8B
x�B
y	B
y	B
y	B
y	B
y�B
y�B
zDB
z�B
zxB
z�B
z�B
{JB
{JB
{B
{JB
{�B
|B
{�B
|�B
|�B
|B
|�B
|�B
|�B
}VB
}VB
}VB
}�B
}VB
}�B
~]B
~(B
}�B
~]B
~�B
~�B
cB
cB
�4B
�4B
�4B
�B
�iB
�4B
�B
�B
�oB
��B
��B
��B
�;B
�oB
�;B
�B
�B
�B
��B
�;B
�;B
�B
��B
��B
�B
��B
��B
�GB
�{B
��B
��B
�MB
��B
��B
�B
�SB
��B
�SB
��B
�%B
��B
�%B
��B
��B
�YB
�_B
�+B
��B�B�B��B��B��B��B�XB��B�XB�$B�RB�XB�$B�B�$B�XB�LB��B�$B��B�LB�$B��B�LB�RB�$B�B��B�XB�XB��B��B�XB�B��B��B�$B��B��B�XB��B�B��B��B��B�RB�$B�RB�B��B��B��B��B��B��B�RB�*B�XB�RB�XB�XB��B�XB��B��B��B�_B��B��B�B�XB��B��B��B�XB��B��B��B�RB��B�$B�RB��B��B�$B�$B�RB�$B��B�RB��B�$B�XB��B�$B�$B��B��B�_B�RB��B��B��B��B�$B�*B�FB�B�$B�XB��B��B�B��B�B�$B�RB�$B��B�RB��B��B��B��B��B��B��B�$B�B�RB�RB��B�:B��B��B�B�B��B�B�nB��B��B�B��B�B��B��B��B�XB��B��B��B��B��B�B��B��B��B�XB��B�LB�zB��B�_B�nB��B�B��B�RB��B��B�$B�B��B��B�LB��B�zB��B��B��B��B��B�RB��B�B�RB�LB�$B��B�0B��B�B��B�zB��B��B�B�LB��B�B��B�zB��B�B��B��B��B�B�B��B��B��B��B�FB�B��B��B��B�LB��B�tB�zB��B�LB�FB�@B�FB�LB�FB��B�B�B��B�B�@B�B��B�B�tB��B�FB�nB�B�B��B��B�B��B�:B��B�hB�nB�hB�-B��B��B��B��B�bB��B�\B�VB��B��B�qB�B��B�qB�_B��B��B�YB��B�kB��B��B��B��B�XB�B�[B�RB��B��B��B͟B�PB	�B	�B	�B	�B	�B	4�B	G�B	b�B	x8B	|�B	�B	��B	��B	�oB	��B	��B
~B
"4B
"�B
"�B
"hB
+kB
0�B
?HB
\)B
`�B
hsB
p�B
s�B
��B
�kB
�B
�:B
�@B
�B
��B
��B
ĜB
��B
�<B
ҽB
�yB
�HB
�+B
��B
��B
�B
��B
�.B
�VB"BB
�BB�B:^B8�BA�B>wB9�B/�B5�B4�B,�B)*B%�B"4B�B�B�B�B!bB+B)�B^�Bf�Bj�Bs�B��B��B��B�B��B�1B�uB�PB��B}�B|PB|�BzBt�Bu�Bu�Bp�Bo�Bq�Br|Bn/BkQBgmBb�B\�B]dB��B^BR BFB/�B/OB)�B&B+6B$�B=B�B�B1B�BB�BoBB�B;B
��B
��B
��B
��BAB�BkB~B_B�B
��B
��B
�)B
��B
�QB
�B
�B
�sB
�B
�mB
�)B
�B
� B
�BB
�BB
�
B
�/B
�2B
��B
��B
бB
�BB
��B
�EB
�?B
ǮB
��B
�B
�'B
��B
��B
�wB
�?B
��B
��B
��B
�FB
�-B
�B
�CB
�6B
��B
��B
��B
�nB
��B
��B
�3B
��B
�lB
}VB
y	B
��B
�B
�FB
��B
��B
�{B
~]B
�uB
�uB
�4B
�{B
��B
�iB
}�B
|B
v�B
w2B
r�B
qAB
tTB
xB
iyB
bB
]/B
_pB
XEB
R�B
f2B
W
B
F�B
VmB
ZB
aB
7B
4B
4�B
$�B
"4B
 �B
�B
�B
�B
�B
B
�B
�B	�PB	��B	��B	�B
�B	�B	�B	��B	�vB	�;B	��B	��B	ߤB	�pB	�B	�B	�WB	�B	��B	�yB	�EB	�TB	�B	бB	ϫB	�,B	��B	͟B	��B	��B	�RB	�zB	ǮB	��B	��B	��B	��B	�mB	��B	�B	�OB	�kB	�$B	��B	��B	�bB	�hB	�B	�B	��B	��B	��B	�zB	�"B	�B	��B	��B	��B	|�B	|�B	��B	tTB	tB	p;B	s�B	v�B	wfB	r�B	rGB	v�B	kQB	c�B	a�B	c�B	l�B	j�B	r�B	^�B	\�B	]/B	Q�B	W�B	R�B	T�B	V9B	Y�B	GEB	t�B	J�B	>�B	8B	9�B	8�B	6�B	<�B	:*B	&B	($B	&B	%zB	#nB	"�B	$@B	$@B	#�B	&LB	#nB	#B	$B	 �B	�B	!�B	 �B	~B	�B	�B	xB	/�B	�B	#�B	.�B	+B	�B	B	qB	xB	VB	!B	IB	%FB	7�B	8�B	7B	4B	@�B	S�B	X�B	Z�B	_�B	W�B	^B	`BB	aB	jKB	iDB	f�B	poB	rGB	q�B	s�B	s�B	uZB	w�B	y�B	�B	{B	v+B	w2B	�~B	v�B	~]B	}"B	�B	��B	kB	w�B	iDB	rGB	b�B	d�B	ffB	ZQB	Y�B	XyB	UgB	U�B	V�B	T�B	T,B	T�B	R B	S�B	Q�B	QB	QB	P}B	P�B	O�B	PHB	U�B	PB	QNB	O�B	L�B	MB	LdB	IRB	JXB	EB	Q�B	F�B	C�B	:�B	;�B	=<B	7�B	4�B	3�B	3�B	0!B	1�B	,=B	+�B	,B	)�B	-wB	'B	$tB	&B	$�B	#�B	#nB	#B	!�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                      B��B��B�B��B�iB��B��B��B��B��B�	B��B��B��B��B�tB��B��B��B�7B��B�,B��B��B�B�~B�B��B��B��B�B��B�/B�[B�=B��B��B��B�jB��B��B��B�{B�B��B�qB	/�B	��B	��B
5B
qZB
��B
�UB
�OB�B!|BG�B:1B(oB/�B}�B��B�^B�pBa[BvB�B
��B
�&B
�aB
��B
��B
�2B
v�B
B�B
9B	��B	�7B	؋B	�	B	�B	��B	�;B	��B	z�B	r�B	d�B	;[B	+B	.�B	)kB	�B	>B	b�B	��B	��B	|B	ajB	UhB	T�B	V&B	MB	;B	0SB	*�B	%�B	�B	�B	
B	�B	SB	�B	.B	CzB	h�B	z�B	|�B	�xB	��B	��B	�6B	��B	�9B	�9B	��B	�|B	�GB	ƲB	��B	�$B	�B	�PB	��B	�EB	�MB	�=B	��B	��B	�rB	�B	��B
 xB	��B
B
B
�B
�B
�B
�B
�B
�B
�B
+B
":B
$�B
0;B
-�B
+�B
.LB
.�B
0�B
2�B
4wB
6}B
:vB
GB
I�B
KB
K$B
K*B
LB
LQB
M�B
NbB
OFB
P�B
P'B
P�B
QB
TRB
T�B
T
B
SiB
R�B
S�B
RcB
S�B
Q�B
P�B
T�B
T�B
T�B
V�B
WNB
TB
S�B
SKB
S�B
U-B
S�B
Q<B
O�B
P>B
OB
O�B
PCB
O�B
RSB
N�B
LPB
J�B
KB
I�B
G�B
H�B
F�B
E}B
EB
D�B
E,B
D�B
EgB
B�B
A�B
BNB
=B
<#B
8NB
9�B
6�B
7�B
7�B
3�B
4B
5&B
6�B
7�B
6cB
60B
4B
2hB
2.B
5B
0B
.�B
->B
-�B
.!B
,�B
*�B
+�B
*�B
*�B
(XB
(�B
,�B
+�B
##B
"�B
#�B
!�B
!�B
!�B
!�B
!B
!KB
dB
 B
�B
 �B
"FB
"B
"�B
#1B
"B
 zB
!RB
 [B
�B
�B
 AB
!�B
`B
�B
 :B
�B
�B
�B
�B
~B
�B
xB
 2B
 8B
�B
B
 �B
VB
B
!B
;B
�B
.B
�B
�B
>B
B
�B
BB
0B
�B
7B
~B
:B
}B
�B
�B
�B
B
�B
�B
�B
*B
�B
�B
�B
dB
�B
�B
�B
�B
�B
�B
|B
zB
�B
�B
B
oB
�B
bB
�B
%B
FB
�B
B
�B
kB
�B
�B
[B
/B
YB
�B
4B
�B
�B
jB
B
SB
xB
�B
B
9B
�B
B
�B
�B
SB
}B
�B
�B
mB
"B
<B
�B
�B
�B
-B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
cB
+B
�B
�B
�B
�B
~B
	B
B
�B
�B
�B
EB
�B
�B
�B
�B
�B
�B
�B
`B
�B
kB
B
rB
�B
�B
�B
�B
�B
�B
 B
HB
B
jB
&B
�B
�B
 B
*B
�B
MB
B
 B
aB
 B
�B
�B
MB
B
>B
�B
�B
�B
�B
B
B
�B
`B
oB
;B
�B
aB
oB
 eB
�B
 B
 .B
*B
!�B
#<B
#RB
#B
"�B
"�B
#�B
#�B
#�B
#NB
$%B
$�B
$B
$\B
%MB
%;B
%aB
%�B
&-B
&B
%�B
$�B
%UB
%�B
&�B
&BB
&�B
'B
'yB
'+B
'�B
'-B
'$B
'iB
'mB
'�B
'�B
(aB
(B
(hB
)�B
)�B
)�B
)�B
*B
)�B
(�B
(�B
)�B
*9B
+B
*�B
*�B
*�B
+B
*�B
*'B
*�B
*B
*�B
*�B
+B
,-B
,"B
,�B
,�B
,OB
-B
.B
.�B
0B
0�B
0NB
/�B
0B
/�B
/�B
04B
0�B
0�B
1�B
1�B
1�B
1�B
2:B
2�B
2�B
3FB
3TB
3dB
3�B
3�B
3�B
4B
56B
5	B
4�B
4�B
5�B
6aB
6fB
7^B
7B
7B
7TB
8B
7�B
8-B
9B
8�B
8�B
9VB
9pB
9�B
9�B
9�B
:TB
9�B
9B
9yB
9�B
:B
9�B
:hB
: B
:.B
:�B
;mB
;�B
;�B
;�B
<B
<B
<]B
<�B
<B
<�B
<�B
=B
=�B
=�B
=�B
>B
>1B
>�B
>�B
>�B
?�B
?�B
@@B
@�B
@�B
A0B
A�B
@�B
@�B
AB
@�B
AiB
B'B
BB
CMB
CB
B'B
C�B
C5B
C-B
B�B
B�B
B�B
C�B
CYB
CuB
D�B
D B
C�B
C�B
DHB
D�B
D�B
D�B
D�B
E�B
EyB
E�B
F$B
FzB
FSB
FHB
FB
GB
G�B
G�B
G�B
G�B
G�B
G�B
H"B
H�B
H�B
H�B
I_B
I6B
IXB
I�B
KB
KB
JMB
I�B
J�B
K.B
K�B
J�B
J{B
J:B
JBB
J;B
KB
JtB
JiB
JoB
KB
L�B
LDB
LWB
L�B
MB
M@B
MDB
MB
M B
M�B
N�B
NfB
N�B
N�B
O}B
OdB
O�B
O�B
O�B
OZB
O�B
Q!B
P�B
P�B
P�B
QCB
R�B
S�B
T�B
T%B
S�B
T�B
UgB
UtB
U~B
U�B
VvB
V#B
V�B
V�B
V�B
W�B
X�B
W�B
W�B
W�B
W�B
W�B
WOB
W�B
W�B
XB
X�B
YB
Y7B
X�B
YuB
Y�B
Z�B
Z�B
Z�B
Z�B
[B
\)B
\�B
]-B
\�B
\ B
\&B
\�B
[�B
[�B
[�B
[�B
[�B
[�B
\"B
\�B
\�B
^"B
^OB
_oB
_�B
`�B
`BB
_�B
_�B
_XB
_
B
^�B
_FB
`9B
aB
a<B
a�B
a�B
a�B
bB
buB
bbB
b�B
cnB
b�B
bB
bB
b@B
b�B
cjB
c�B
c�B
d�B
eDB
e�B
e�B
f�B
f�B
fmB
f�B
f�B
f�B
gB
g_B
g�B
g�B
h	B
hB
h{B
h�B
h�B
h�B
iiB
i�B
jvB
j�B
j�B
j�B
jaB
j�B
jLB
jlB
j�B
j�B
j�B
j�B
j�B
kB
k�B
lRB
l5B
lB
lYB
l�B
mhB
mB
m=B
mB
m(B
m<B
m�B
m�B
n0B
n5B
nB
nBB
nB
n0B
nqB
n�B
n�B
oB
o(B
oSB
o�B
o�B
o�B
pRB
p?B
p\B
p�B
p�B
p�B
qB
p�B
qTB
q'B
q B
qqB
q�B
qeB
qcB
q{B
qUB
q|B
qhB
q�B
rB
r;B
r�B
r�B
sB
r�B
r�B
s6B
s�B
s�B
tB
s�B
s�B
s�B
tB
s�B
s�B
s�B
t}B
t�B
t�B
u1B
uB
umB
uGB
u^B
u2B
vB
u�B
u�B
v6B
v�B
w!B
wzB
w�B
xB
w�B
x#B
xGB
w�B
wFB
w,B
w�B
w�B
xlB
xtB
y,B
ygB
y"B
yAB
yjB
y�B
zB
z�B
z�B
z�B
{B
z�B
{~B
{�B
{�B
{�B
{�B
|\B
{�B
|�B
|�B
|OB
|�B
|�B
|�B
}yB
}B
}�B
}�B
}�B
~'B
~B
~AB
}�B
~�B
~�B
~�B
�B
zB
�HB
�XB
�hB
�B
�~B
�MB
�)B
�SB
�sB
��B
��B
�B
��B
��B
��B
�iB
�1B
�/B
��B
�eB
��B
�QB
��B
��B
�UB
��B
�B
�zB
��B
�}B
��B
��B
��B
��B
�8B
�xB
��B
��B
�B
�9B
�B
�8B
�B
��B
��B
�qB
�.G�O�B�B�B��B��B��B��B�XB��B�XB�$B�RB�XB�$B�B�$B�XB�LB��B�$B��B�LB�$B��B�LB�RB�$B�B��B�XB�XB��B��B�XB�B��B��B�$B��B��B�XB��B�B��B��B��B�RB�$B�RB�B��B��B��B��B��B��B�RB�*B�XB�RB�XB�XB��B�XB��B��B��B�_B��B��B�B�XB��B��B��B�XB��B��B��B�RB��B�$B�RB��B��B�$B�$B�RB�$B��B�RB��B�$B�XB��B�$B�$B��B��B�_B�RB��B��B��B��B�$B�*B�FB�B�$B�XB��B��B�B��B�B�$B�RB�$B��B�RB��B��B��B��B��B��B��B�$B�B�RB�RB��B�:B��B��B�B�B��B�B�nB��B��B�B��B�B��B��B��B�XB��B��B��B��B��B�B��B��B��B�XB��B�LB�zB��B�_B�nB��B�B��B�RB��B��B�$B�B��B��B�LB��B�zB��B��B��B��B��B�RB��B�B�RB�LB�$B��B�0B��B�B��B�zB��B��B�B�LB��B�B��B�zB��B�B��B��B��B�B�B��B��B��B��B�FB�B��B��B��B�LB��B�tB�zB��B�LB�FB�@B�FB�LB�FB��B�B�B��B�B�@B�B��B�B�tB��B�FB�nB�B�B��B��B�B��B�:B��B�hB�nB�hB�-B��B��B��B��B�bB��B�\B�VB��B��B�qB�B��B�qB�_B��B��B�YB��B�kB��B��B��B��B�XB�B�[B�RB��B��B��B͟B�PB	�B	�B	�B	�B	�B	4�B	G�B	b�B	x8B	|�B	�B	��B	��B	�oB	��B	��B
~B
"4B
"�B
"�B
"hB
+kB
0�B
?HB
\)B
`�B
hsB
p�B
s�B
��B
�kB
�B
�:B
�@B
�B
��B
��B
ĜB
��B
�<B
ҽB
�yB
�HB
�+B
��B
��B
�B
��B
�.B
�VB"BB
�BB�B:^B8�BA�B>wB9�B/�B5�B4�B,�B)*B%�B"4B�B�B�B�B!bB+B)�B^�Bf�Bj�Bs�B��B��B��B�B��B�1B�uB�PB��B}�B|PB|�BzBt�Bu�Bu�Bp�Bo�Bq�Br|Bn/BkQBgmBb�B\�B]dB��B^BR BFB/�B/OB)�B&B+6B$�B=B�B�B1B�BB�BoBB�B;B
��B
��B
��B
��BAB�BkB~B_B�B
��B
��B
�)B
��B
�QB
�B
�B
�sB
�B
�mB
�)B
�B
� B
�BB
�BB
�
B
�/B
�2B
��B
��B
бB
�BB
��B
�EB
�?B
ǮB
��B
�B
�'B
��B
��B
�wB
�?B
��B
��B
��B
�FB
�-B
�B
�CB
�6B
��B
��B
��B
�nB
��B
��B
�3B
��B
�lB
}VB
y	B
��B
�B
�FB
��B
��B
�{B
~]B
�uB
�uB
�4B
�{B
��B
�iB
}�B
|B
v�B
w2B
r�B
qAB
tTB
xB
iyB
bB
]/B
_pB
XEB
R�B
f2B
W
B
F�B
VmB
ZB
aB
7B
4B
4�B
$�B
"4B
 �B
�B
�B
�B
�B
B
�B
�B	�PB	��B	��B	�B
�B	�B	�B	��B	�vB	�;B	��B	��B	ߤB	�pB	�B	�B	�WB	�B	��B	�yB	�EB	�TB	�B	бB	ϫB	�,B	��B	͟B	��B	��B	�RB	�zB	ǮB	��B	��B	��B	��B	�mB	��B	�B	�OB	�kB	�$B	��B	��B	�bB	�hB	�B	�B	��B	��B	��B	�zB	�"B	�B	��B	��B	��B	|�B	|�B	��B	tTB	tB	p;B	s�B	v�B	wfB	r�B	rGB	v�B	kQB	c�B	a�B	c�B	l�B	j�B	r�B	^�B	\�B	]/B	Q�B	W�B	R�B	T�B	V9B	Y�B	GEB	t�B	J�B	>�B	8B	9�B	8�B	6�B	<�B	:*B	&B	($B	&B	%zB	#nB	"�B	$@B	$@B	#�B	&LB	#nB	#B	$B	 �B	�B	!�B	 �B	~B	�B	�B	xB	/�B	�B	#�B	.�B	+B	�B	B	qB	xB	VB	!B	IB	%FB	7�B	8�B	7B	4B	@�B	S�B	X�B	Z�B	_�B	W�B	^B	`BB	aB	jKB	iDB	f�B	poB	rGB	q�B	s�B	s�B	uZB	w�B	y�B	�B	{B	v+B	w2B	�~B	v�B	~]B	}"B	�B	��B	kB	w�B	iDB	rGB	b�B	d�B	ffB	ZQB	Y�B	XyB	UgB	U�B	V�B	T�B	T,B	T�B	R B	S�B	Q�B	QB	QB	P}B	P�B	O�B	PHB	U�B	PB	QNB	O�B	L�B	MB	LdB	IRB	JXB	EB	Q�B	F�B	C�B	:�B	;�B	=<B	7�B	4�B	3�B	3�B	0!B	1�B	,=B	+�B	,B	)�B	-wB	'B	$tB	&B	$�B	#�B	#nB	#B	!�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���=��<��u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�h/<E�<#�
<#�
<}t$<4j�<#�
<#�
<�E�<��4<��i<�<��V<� �=�<���<Y�=<y�&<��%<��<�z�<#�
<#�
<#�
<���<��s<��<~�a<@-�<f�(<�n<�c<#�
<#�
<g�<%�D<#�
<#�
<#�
<ND�<ŧ<U`�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2020061508595520200615085955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020062507003820200625070038QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020062507003820200625070038QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020082411445320200824114453IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                