CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-11-27T23:28:24Z creation; 2023-04-26T19:14:27Z DMQC;      
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
_FillValue        G�O�     (  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \0   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     (  c�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     (  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( (   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 6P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( >   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( e   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181127232824  20230426191427  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                    AA  AOAO7316_008644_032                 7316_008644_032                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @ؓ����'@ؓ����'11  @ؓ�ݗ�+@ؓ�ݗ�+@+����@+�����c�p�Ͽ��c�p�Ͽ�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@B�\@�  @��\@�G�@޸R@�p�A  A!G�A,��A@  A`  A�  A���A�Q�A��A�  A�  A�  A��B Q�B  B  B(�B   B'�
B0  B8(�B@(�BG�
BP  BX  B`  Bh��Bo�
Bw�
B�  B��B�  B�{B�  B�  B�  B�  B��B�{B�{B�  B�  B��B�  B�(�B�{B�{B�{B�  B��B��B��
B��B��B�  B��B�  B�{B�  B��B��C   C  C  C  C  C
  C  C{C
=C��C  C{C{C
=C  C  C   C"
=C$  C%��C(  C)��C+��C.
=C0  C2  C4
=C6{C8  C9�C<  C>{C@
=CB
=CD
=CF{CH  CJ  CL  CN  CP  CR  CT
=CU��CX  CZ
=C\  C]��C`  Cb{Cd
=Cf
=Ch  Ci��Ck��Cm��Co�Cq�Cs�Cu�Cx
=Cz
=C|  C}�C�C���C�  C�C�C�  C�  C���C�  C�C�  C�  C�  C�C�C�C���C���C�  C�  C�  C�C�C���C���C���C���C���C���C���C���C�  C�  C�  C�  C�  C�  C�C�C�  C�  C�  C�C�C�C�C�  C�  C�C�C�  C�  C�  C�  C�C�C�  C���C���C�  C���C���C�  C�  C�  C�  C�C�C�C�
=C�C���C�  C�C�  C�  C�  C�C�C�C�C�
=C�C�  C�C�  C�  C�C���C���C���C�C�C�
=C�  C���C�  C�
=C�  C�  C�
=C�C���C�  C�  C�C�C�  C���C�  C�C�  C���C�  C�C�  C���C���C�  C�
=C�C�C�  C�  C�  C���C�  C�D   D }qD ��D� D�D��D�D�D  D� D  D� D  D� D  D� D�D�D	�D	� D
D
� D
�qD� D�qDz�D��D}qD�qD}qD�qD}qD�qD}qD�qD� D�D� D  D� D�qD� D�D��D�D� D�qD}qD�D�D  D}qD�qD� D  D��D�D� D  D��DD��D�qD� D �D }qD ��D!z�D!�qD"� D#  D#� D#��D$z�D%  D%��D&�D&}qD&�qD'��D(�D(� D(�qD)}qD*  D*�D+�D+� D+�qD,}qD,�qD-}qD.�D.� D/  D/��D0�D0� D1  D1}qD2  D2��D3  D3z�D3�qD4}qD4�qD5� D6  D6� D7  D7� D8�D8� D9  D9��D9�qD:}qD:�qD;}qD<  D<� D=�D=}qD=�qD>� D?  D?� D?�qD@� DA  DA� DB�DB}qDC  DC��DD  DD}qDD��DE}qDF  DF� DG  DG}qDH  DH��DI  DI� DJ  DJ� DK�DK��DL  DL� DM  DM}qDN  DN}qDO  DO� DP  DP� DQ  DQ� DR�DR� DS  DS��DT�DT��DU  DU��DV  DV}qDW  DW��DX�DX��DX�qDYz�DZ  DZ��D[  D[� D\  D\}qD\�qD]� D^  D^� D_  D_}qD_�qD`� D`�qDaz�Da��Db� Dc�Dc��Dd�Dd� Dd��De}qDf  Df� Df�qDg��Dh�Dh}qDh�qDi� Dj�Dj��Dk�Dk� Dk�qDl}qDm  Dm��Dn�Dn��Do  Do� Do�qDp}qDp�qDq}qDq�qDr}qDs�Ds� Ds�qDt� Du�Du� Du��Dv}qDv�qDw� Dx  Dx}qDx�qDy� Dy�qDz}qD{  D{� D|  D|� D}  D}� D~  D~� D  D��D�  D�@ D�� D���D�  D�AHD��HD�� D���D�>�D�}qD���D���D�>�D��HD��HD�HD�AHD��HD��HD�  D�>�D�}qD�� D���D�>�D�� D�� D�HD�AHD��HD��HD�  D�@ D�~�D���D�  D�AHD�� D�� D���D�>�D�~�D�� D���D�@ D�� D���D�  D�@ D�� D���D���D�@ D�� D�� D���D�=qD�}qD�� D��D�AHD�� D�� D�  D�@ D�� D��HD�  D�>�D�� D�� D�HD�AHD�� D�� D�  D�@ D��HD��HD�  D�@ D��HD�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D���D���D�=qD�� D�� D�  D�=qD�}qD�� D���D�>�D�� D�D�HD�AHD�� D�� D���D�AHD���D��HD�HD�AHD�� D�� D�  D�AHD��HD�D�HD�@ D�� D�� D��D�AHD��HD�� D�  D�>�D�~�D��HD��D�@ D�~�D�� D�  D�>�D��HD�� D�  D�AHD��HD��HD�HD�AHD��HD�D��D�B�D�� D���D�  D�AHD�� D�� D�HD�>�D�}qD�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�HD�AHD�� D�� D�HD�>�D�� D��HD���D�>�D�� D�� D�  D�>�D��HD�D��D�B�D�~�D�� D��D�>�D�� D�D�  D�=qD�� D���D�HD�>�D�~�D���D���D�AHD��HD�� D�  D�@ D�� D��HD���D�=qD�~�D�� D�HD�@ D�}qD���D���D�@ D��HD�� D���D�>�D�� D���D�  D�AHD��HD��HD�  D�@ D�� D���D�  D�AHD�� D��HD��D�AHD�� D���D�  D�@ D�� D��HD�  D�>�D�~�D���D�  D�AHD�� D���D���D�>�D�~�D���D�HD�@ D�� D���D���D�>�D D�� D�  D�AHDÁHD��HD���D�AHDāHD�� D���D�>�Dŀ D�� D���D�@ DƂ�D��HD���D�=qDǀ DǾ�D�  D�@ DȀ DȾ�D�  D�@ D�~�Dɾ�D�  D�@ Dʀ D��HD�  D�@ Dˀ D��HD�HD�@ D̀ D��HD�  D�=qD�~�D�� D�HD�@ D�~�DνqD���D�@ Dπ D�D�HD�@ DЁHD�� D�  D�AHDсHD�� D�  D�>�D�}qD�� D�HD�AHDӁHD��HD���D�>�DԁHD�D�HD�>�D�}qDվ�D�HD�AHDցHD��HD�  D�>�D�~�D׾�D�  D�@ D؁HD�� D���D�@ Dق�D��HD���D�@ Dڀ Dھ�D���D�@ D�~�D۾�D�HD�@ D܀ Dܾ�D��qD�@ D݀ D��HD�  D�@ Dހ D��HD�  D�>�D߀ D߾�D�  D�@ D�~�DྸD���D�@ D� D�� D�  D�@ D�~�D��HD�  D�>�D� D��HD�HD�AHD� D侸D�HD�B�D�~�D�� D�  D�>�D� D�qD���D�>�D�}qD羸D�  D�@ D�HD�D���D�<)D�|)D�qD��qD�>�D�~�D�qD�  D�B�D�HD�� D���D�=qD�~�D�� D�HD�@ D� D��HD�HD�@ D�HD��HD���D�>�D� DﾸD�  D�AHD�� D�� D�  D�AHD�HD��HD�  D�@ D� D�� D�  D�@ D� D�qD���D�@ D� D�� D���D�>�D�� D�D�HD�AHD�� D���D�  D�AHD�~�D�Ф>���?\)?W
=?�z�?�33?�@�@��@.{@E�@^�R@s33@��
@���@���@�ff@�\)@��H@��@�33@�  @���@�@��RAA(�A��A
=Ap�A#33A'�A.{A4z�A<(�AA�AG�AN{AU�A[�AaG�Ag
=Amp�Atz�Az=qA\)A��HA��RA��A���A��A�33A��RA��A���A��A��A��RA�=qA�p�A�Q�A��A�
=A�=qA�p�A�Q�A��
A�\)A��HA�A�Q�AӅAָRA�=qA��A�  A�33A�ffA�G�A�(�A�ffA�A�(�A��RA�G�A�33A�p�A��B ��B�B�HB�
B��B{B
=B  B��B	�B
=B(�B�B�B
=B(�B�B{B
=B  B�B{B
=B�
B��B{B
=B�
B��B{B33B (�B ��B!�B#
=B$  B$��B%B&�RB'�
B(��B)B*�\B+\)B,Q�B-p�B.ffB/33B0  B0��B2{B3
=B3�B4��B5p�B6�\B7\)B8Q�B9�B9�B:�\B;�B<z�B=G�B=�B>�RB?\)B@Q�BA�BA�BB�RBC�BD(�BD��BE�BF�HBG�
BH��BI��BJ�\BK�BL��BM�BN�RBO�BP��BQBR�HBT  BT��BU�BV�HBW�
BX��BZ{B[33B\Q�B]p�B^�\B_\)B`z�Bap�Bb�RBc�
Bd��BeBf�RBg�Bh(�Bh��Bi��BjffBk
=Bk\)Bk�
Bl(�Bl��BmG�Bm�BnffBn�HBo\)Bo�
BpQ�Bp��Bq�BqBrffBs
=Bs�Bt(�Bt��Bu�Bu��Bv{Bv�HBw�Bx(�Bx��ByBzffB{
=B{�B|(�B|��B}��B~=qB~�HB�B�=qB���B�
=B�\)B��B�{B�Q�B��RB�
=B�p�B�B�(�B�z�B��HB�\)B��B�(�B�z�B���B��B��B��
B�Q�B��RB��B��B��B�Q�B��RB�33B��B�{B��\B���B�\)B�B�(�B��\B���B�p�B��
B�=qB��RB��B���B�{B�z�B���B�p�B��B�ffB��HB�G�B�B�(�B���B��B���B�{B��\B�
=B�p�B��B�ffB��HB�G�B�B�Q�B���B�33B�B�=qB���B�G�B�B�=qB��RB�G�B��B�(�B��RB��B���B�{B��\B�
=B��B��B�ffB��HB�p�B��B�Q�B��HB�\)B��
B�Q�B��HB�\)B�B�Q�B��RB�33B��B�(�B���B��B��B�{B�z�B���B�p�B��B�Q�B���B�G�B�B�=qB��RB�33B��B�{B���B��B���B�{B��\B�
=B��B�  B�z�B���B�p�B��B�Q�B���B�G�B�B�=qB���B��B���B�{B�z�B��HB�\)B�B�=qB£�B��BÙ�B�{B�z�B��HB�\)B��
B�=qBƸRB�33BǮB�  B�z�B���B�p�B��
B�Q�B���B�G�BˮB�(�B̏\B�
=B�p�B��B�Q�BθRB�33Bϙ�B�{BЏ\B�
=Bљ�B�{Bҏ\B�
=Bә�B�{Bԏ\B�
=BՅB�  B�z�B���B�\)B��
B�Q�B���B�33B�B�=qBڸRB�33BۮB�(�BܸRB��Bݙ�B�{Bޏ\B���B�p�B��B�ffB��HB�\)B��B�z�B���B�p�B��B�ffB���B�G�B��
B�Q�B��HB�p�B��B�ffB���B�p�B�  B�z�B���B�p�B��B�z�B��HB�p�B�  B��B��BB�{B��\B�
=B�B�{B��B��B�B�(�B��RB��B���B�(�B���B�33B�B�Q�B���B�p�B�{B��\B�
=B���B�(�B��RB�G�B��B��\B�33B�C (�C p�C �RC
=CffC�RC  CG�C��C�HC33C�\C�HC33Cp�CC{Cz�C��C�C\)C�C
=C\)C�RC  CG�C��C��C	G�C	��C	��C
=qC
�C
�
C33C�C�
C33Cz�C��C{Cp�C��C(�C�C�HC33Cz�C�
C=qC��C��CG�C��C�CQ�C�C
=CffCC{Cp�C��C(�C�\C�CG�C��C�CQ�C�RC�Cp�C��C�Cz�C�
C=qC��C�C=qC��C  CffCC{CffCC33C�\C�HC=qC��C 
=C ffC �RC!{C!z�C!�HC"G�C"��C"��C#Q�C#C$�C$p�C$��C%(�C%��C%��C&Q�C&��C'
=C'p�C'�
C(33C(��C(�C)G�C)�RC*{C*p�C*��C+(�C+�\C+��C,\)C,�C-
=C-z�C-�HC.33C.�\C.��C/\)C/C0{C0p�C0�HC1G�C1��C2  C2\)C2�RC3(�C3�C3�HC4=qC4��C5  C5\)C5�RC6{C6p�C6�
C7=qC7�\C7�HC8G�C8�C9
=C9\)C9�RC:�C:�C:�
C;(�C;�C;�C<Q�C<��C<��C=\)C=�RC>�C>p�C>C?�C?�C?�HC@=qC@��C@�CAQ�CA�RCB
=CB\)CB�RCC{CCp�CC��CD(�CDp�CD�
CE=qCE�\CE�HCF33CF�\CF��CGQ�CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              ?�  @�\@B�\@�  @��\@�G�@޸R@�p�A  A!G�A,��A@  A`  A�  A���A�Q�A��A�  A�  A�  A��B Q�B  B  B(�B   B'�
B0  B8(�B@(�BG�
BP  BX  B`  Bh��Bo�
Bw�
B�  B��B�  B�{B�  B�  B�  B�  B��B�{B�{B�  B�  B��B�  B�(�B�{B�{B�{B�  B��B��B��
B��B��B�  B��B�  B�{B�  B��B��C   C  C  C  C  C
  C  C{C
=C��C  C{C{C
=C  C  C   C"
=C$  C%��C(  C)��C+��C.
=C0  C2  C4
=C6{C8  C9�C<  C>{C@
=CB
=CD
=CF{CH  CJ  CL  CN  CP  CR  CT
=CU��CX  CZ
=C\  C]��C`  Cb{Cd
=Cf
=Ch  Ci��Ck��Cm��Co�Cq�Cs�Cu�Cx
=Cz
=C|  C}�C�C���C�  C�C�C�  C�  C���C�  C�C�  C�  C�  C�C�C�C���C���C�  C�  C�  C�C�C���C���C���C���C���C���C���C���C�  C�  C�  C�  C�  C�  C�C�C�  C�  C�  C�C�C�C�C�  C�  C�C�C�  C�  C�  C�  C�C�C�  C���C���C�  C���C���C�  C�  C�  C�  C�C�C�C�
=C�C���C�  C�C�  C�  C�  C�C�C�C�C�
=C�C�  C�C�  C�  C�C���C���C���C�C�C�
=C�  C���C�  C�
=C�  C�  C�
=C�C���C�  C�  C�C�C�  C���C�  C�C�  C���C�  C�C�  C���C���C�  C�
=C�C�C�  C�  C�  C���C�  C�D   D }qD ��D� D�D��D�D�D  D� D  D� D  D� D  D� D�D�D	�D	� D
D
� D
�qD� D�qDz�D��D}qD�qD}qD�qD}qD�qD}qD�qD� D�D� D  D� D�qD� D�D��D�D� D�qD}qD�D�D  D}qD�qD� D  D��D�D� D  D��DD��D�qD� D �D }qD ��D!z�D!�qD"� D#  D#� D#��D$z�D%  D%��D&�D&}qD&�qD'��D(�D(� D(�qD)}qD*  D*�D+�D+� D+�qD,}qD,�qD-}qD.�D.� D/  D/��D0�D0� D1  D1}qD2  D2��D3  D3z�D3�qD4}qD4�qD5� D6  D6� D7  D7� D8�D8� D9  D9��D9�qD:}qD:�qD;}qD<  D<� D=�D=}qD=�qD>� D?  D?� D?�qD@� DA  DA� DB�DB}qDC  DC��DD  DD}qDD��DE}qDF  DF� DG  DG}qDH  DH��DI  DI� DJ  DJ� DK�DK��DL  DL� DM  DM}qDN  DN}qDO  DO� DP  DP� DQ  DQ� DR�DR� DS  DS��DT�DT��DU  DU��DV  DV}qDW  DW��DX�DX��DX�qDYz�DZ  DZ��D[  D[� D\  D\}qD\�qD]� D^  D^� D_  D_}qD_�qD`� D`�qDaz�Da��Db� Dc�Dc��Dd�Dd� Dd��De}qDf  Df� Df�qDg��Dh�Dh}qDh�qDi� Dj�Dj��Dk�Dk� Dk�qDl}qDm  Dm��Dn�Dn��Do  Do� Do�qDp}qDp�qDq}qDq�qDr}qDs�Ds� Ds�qDt� Du�Du� Du��Dv}qDv�qDw� Dx  Dx}qDx�qDy� Dy�qDz}qD{  D{� D|  D|� D}  D}� D~  D~� D  D��D�  D�@ D�� D���D�  D�AHD��HD�� D���D�>�D�}qD���D���D�>�D��HD��HD�HD�AHD��HD��HD�  D�>�D�}qD�� D���D�>�D�� D�� D�HD�AHD��HD��HD�  D�@ D�~�D���D�  D�AHD�� D�� D���D�>�D�~�D�� D���D�@ D�� D���D�  D�@ D�� D���D���D�@ D�� D�� D���D�=qD�}qD�� D��D�AHD�� D�� D�  D�@ D�� D��HD�  D�>�D�� D�� D�HD�AHD�� D�� D�  D�@ D��HD��HD�  D�@ D��HD�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D���D���D�=qD�� D�� D�  D�=qD�}qD�� D���D�>�D�� D�D�HD�AHD�� D�� D���D�AHD���D��HD�HD�AHD�� D�� D�  D�AHD��HD�D�HD�@ D�� D�� D��D�AHD��HD�� D�  D�>�D�~�D��HD��D�@ D�~�D�� D�  D�>�D��HD�� D�  D�AHD��HD��HD�HD�AHD��HD�D��D�B�D�� D���D�  D�AHD�� D�� D�HD�>�D�}qD�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�HD�AHD�� D�� D�HD�>�D�� D��HD���D�>�D�� D�� D�  D�>�D��HD�D��D�B�D�~�D�� D��D�>�D�� D�D�  D�=qD�� D���D�HD�>�D�~�D���D���D�AHD��HD�� D�  D�@ D�� D��HD���D�=qD�~�D�� D�HD�@ D�}qD���D���D�@ D��HD�� D���D�>�D�� D���D�  D�AHD��HD��HD�  D�@ D�� D���D�  D�AHD�� D��HD��D�AHD�� D���D�  D�@ D�� D��HD�  D�>�D�~�D���D�  D�AHD�� D���D���D�>�D�~�D���D�HD�@ D�� D���D���D�>�D D�� D�  D�AHDÁHD��HD���D�AHDāHD�� D���D�>�Dŀ D�� D���D�@ DƂ�D��HD���D�=qDǀ DǾ�D�  D�@ DȀ DȾ�D�  D�@ D�~�Dɾ�D�  D�@ Dʀ D��HD�  D�@ Dˀ D��HD�HD�@ D̀ D��HD�  D�=qD�~�D�� D�HD�@ D�~�DνqD���D�@ Dπ D�D�HD�@ DЁHD�� D�  D�AHDсHD�� D�  D�>�D�}qD�� D�HD�AHDӁHD��HD���D�>�DԁHD�D�HD�>�D�}qDվ�D�HD�AHDցHD��HD�  D�>�D�~�D׾�D�  D�@ D؁HD�� D���D�@ Dق�D��HD���D�@ Dڀ Dھ�D���D�@ D�~�D۾�D�HD�@ D܀ Dܾ�D��qD�@ D݀ D��HD�  D�@ Dހ D��HD�  D�>�D߀ D߾�D�  D�@ D�~�DྸD���D�@ D� D�� D�  D�@ D�~�D��HD�  D�>�D� D��HD�HD�AHD� D侸D�HD�B�D�~�D�� D�  D�>�D� D�qD���D�>�D�}qD羸D�  D�@ D�HD�D���D�<)D�|)D�qD��qD�>�D�~�D�qD�  D�B�D�HD�� D���D�=qD�~�D�� D�HD�@ D� D��HD�HD�@ D�HD��HD���D�>�D� DﾸD�  D�AHD�� D�� D�  D�AHD�HD��HD�  D�@ D� D�� D�  D�@ D� D�qD���D�@ D� D�� D���D�>�D�� D�D�HD�AHD�� D���D�  D�AHD�~�G�O�>���?\)?W
=?�z�?�33?�@�@��@.{@E�@^�R@s33@��
@���@���@�ff@�\)@��H@��@�33@�  @���@�@��RAA(�A��A
=Ap�A#33A'�A.{A4z�A<(�AA�AG�AN{AU�A[�AaG�Ag
=Amp�Atz�Az=qA\)A��HA��RA��A���A��A�33A��RA��A���A��A��A��RA�=qA�p�A�Q�A��A�
=A�=qA�p�A�Q�A��
A�\)A��HA�A�Q�AӅAָRA�=qA��A�  A�33A�ffA�G�A�(�A�ffA�A�(�A��RA�G�A�33A�p�A��B ��B�B�HB�
B��B{B
=B  B��B	�B
=B(�B�B�B
=B(�B�B{B
=B  B�B{B
=B�
B��B{B
=B�
B��B{B33B (�B ��B!�B#
=B$  B$��B%B&�RB'�
B(��B)B*�\B+\)B,Q�B-p�B.ffB/33B0  B0��B2{B3
=B3�B4��B5p�B6�\B7\)B8Q�B9�B9�B:�\B;�B<z�B=G�B=�B>�RB?\)B@Q�BA�BA�BB�RBC�BD(�BD��BE�BF�HBG�
BH��BI��BJ�\BK�BL��BM�BN�RBO�BP��BQBR�HBT  BT��BU�BV�HBW�
BX��BZ{B[33B\Q�B]p�B^�\B_\)B`z�Bap�Bb�RBc�
Bd��BeBf�RBg�Bh(�Bh��Bi��BjffBk
=Bk\)Bk�
Bl(�Bl��BmG�Bm�BnffBn�HBo\)Bo�
BpQ�Bp��Bq�BqBrffBs
=Bs�Bt(�Bt��Bu�Bu��Bv{Bv�HBw�Bx(�Bx��ByBzffB{
=B{�B|(�B|��B}��B~=qB~�HB�B�=qB���B�
=B�\)B��B�{B�Q�B��RB�
=B�p�B�B�(�B�z�B��HB�\)B��B�(�B�z�B���B��B��B��
B�Q�B��RB��B��B��B�Q�B��RB�33B��B�{B��\B���B�\)B�B�(�B��\B���B�p�B��
B�=qB��RB��B���B�{B�z�B���B�p�B��B�ffB��HB�G�B�B�(�B���B��B���B�{B��\B�
=B�p�B��B�ffB��HB�G�B�B�Q�B���B�33B�B�=qB���B�G�B�B�=qB��RB�G�B��B�(�B��RB��B���B�{B��\B�
=B��B��B�ffB��HB�p�B��B�Q�B��HB�\)B��
B�Q�B��HB�\)B�B�Q�B��RB�33B��B�(�B���B��B��B�{B�z�B���B�p�B��B�Q�B���B�G�B�B�=qB��RB�33B��B�{B���B��B���B�{B��\B�
=B��B�  B�z�B���B�p�B��B�Q�B���B�G�B�B�=qB���B��B���B�{B�z�B��HB�\)B�B�=qB£�B��BÙ�B�{B�z�B��HB�\)B��
B�=qBƸRB�33BǮB�  B�z�B���B�p�B��
B�Q�B���B�G�BˮB�(�B̏\B�
=B�p�B��B�Q�BθRB�33Bϙ�B�{BЏ\B�
=Bљ�B�{Bҏ\B�
=Bә�B�{Bԏ\B�
=BՅB�  B�z�B���B�\)B��
B�Q�B���B�33B�B�=qBڸRB�33BۮB�(�BܸRB��Bݙ�B�{Bޏ\B���B�p�B��B�ffB��HB�\)B��B�z�B���B�p�B��B�ffB���B�G�B��
B�Q�B��HB�p�B��B�ffB���B�p�B�  B�z�B���B�p�B��B�z�B��HB�p�B�  B��B��BB�{B��\B�
=B�B�{B��B��B�B�(�B��RB��B���B�(�B���B�33B�B�Q�B���B�p�B�{B��\B�
=B���B�(�B��RB�G�B��B��\B�33B�C (�C p�C �RC
=CffC�RC  CG�C��C�HC33C�\C�HC33Cp�CC{Cz�C��C�C\)C�C
=C\)C�RC  CG�C��C��C	G�C	��C	��C
=qC
�C
�
C33C�C�
C33Cz�C��C{Cp�C��C(�C�C�HC33Cz�C�
C=qC��C��CG�C��C�CQ�C�C
=CffCC{Cp�C��C(�C�\C�CG�C��C�CQ�C�RC�Cp�C��C�Cz�C�
C=qC��C�C=qC��C  CffCC{CffCC33C�\C�HC=qC��C 
=C ffC �RC!{C!z�C!�HC"G�C"��C"��C#Q�C#C$�C$p�C$��C%(�C%��C%��C&Q�C&��C'
=C'p�C'�
C(33C(��C(�C)G�C)�RC*{C*p�C*��C+(�C+�\C+��C,\)C,�C-
=C-z�C-�HC.33C.�\C.��C/\)C/C0{C0p�C0�HC1G�C1��C2  C2\)C2�RC3(�C3�C3�HC4=qC4��C5  C5\)C5�RC6{C6p�C6�
C7=qC7�\C7�HC8G�C8�C9
=C9\)C9�RC:�C:�C:�
C;(�C;�C;�C<Q�C<��C<��C=\)C=�RC>�C>p�C>C?�C?�C?�HC@=qC@��C@�CAQ�CA�RCB
=CB\)CB�RCC{CCp�CC��CD(�CDp�CD�
CE=qCE�\CE�HCF33CF�\CF��CGQ�CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�A�A�9XA�7LA�;dA�7LA�5?A�5?A�7LA�5?A�9XA�7LA�33A�5?A�1'A�/A�+A�-A�+A�(�A�-A�&�A�&�A�(�A�(�A�(�A�(�A�$�A� �A�{A�$�A�JA��
A�l�A¸RA�  A�-A�A��DA���A��\A��wA���A��FA�;dA�ĜA��9A��A��RA���A��DA�E�A�x�A�33A��FA��A�x�A���A��A��A�9XA�v�A��!A���A��A���A�bNAw�TAtQ�Ao��Ak"�AjJAip�AiVAh�!Ag��AcƨAZ��AY;dAV��AP�AOhsAO�AM��AK+AH�/AE"�AC�PAA��A?��A?�A?x�A>�RA>v�A>M�A>(�A> �A>A=VA;O�A9��A9%A8(�A7�;A7;dA6r�A5�A4�A4ĜA4�\A3A2��A2 �A0��A01A/�7A.��A.ffA.A-�A,�HA,VA+C�A)VA'�A&��A%��A$�+A$1A#��A#`BA#"�A"��A"jA!�A!l�A ��A ffA��A�yA�A�A�A?}A�AA��A~�AZAA�A5?Al�A�\AffAbAp�A"�A��A��A��A;dA�A^5A�TAdZA?}A�A��A=qA�-A��A��AE�AA�wAC�A%AbNA�#A��Ax�A/AAz�AƨAO�A
��A
�+A
E�A	�;A	`BA��AĜAVA�A��A�A\)A��A�AVA�PA;dAA�jAz�A1'AA�AdZA;dA
=A��AffAM�A(�A��AA��A7LA ��A ~�A -@�dZ@��+@��@���@���@�+@���@�~�@���@���@�dZ@��\@���@�%@�D@�Q�@�1@��;@�@���@�ȴ@�!@�@�\@�ff@�J@�`B@�1'@�33@��@�ff@�-@�@��T@��@�w@�;d@�o@�+@���@��/@�Q�@�@�|�@�C�@���@�n�@�?}@��@�Ĝ@�1'@�l�@��@�n�@�^@��@��u@�(�@߮@�t�@�o@��@޸R@�M�@��T@�G�@ܛ�@۾w@��@�ȴ@��@ٺ^@�G�@��`@؃@�bN@�Q�@�Q�@�Q�@� �@�\)@�@ա�@�O�@���@ԓu@�1'@��m@���@���@У�@�j@�(�@Ϯ@�o@��H@Χ�@��@�x�@��@���@�A�@˾w@�C�@��y@�n�@��@���@ɑh@�p�@�O�@�/@�z�@Ǯ@��@�ff@�`B@���@�Z@þw@�dZ@��y@�~�@���@�x�@�?}@�Ĝ@���@��@�5?@���@�G�@�V@��/@�r�@�b@��@�o@��H@�5?@�/@��@�z�@�Q�@�1'@��w@�l�@���@���@�x�@�`B@�?}@���@�Z@��;@�S�@���@�5?@���@�Ĝ@�Q�@�(�@�1@��;@���@�\)@��@�ff@�E�@��T@�hs@�&�@�r�@� �@���@�ƨ@���@��@�S�@�
=@��@�ȴ@�n�@�@���@�7L@�Z@���@���@�o@���@��\@�M�@�{@��@��-@�`B@�V@��@���@�j@�1@��;@��w@�l�@�
=@���@�v�@�J@��h@��@�z�@�\)@���@�V@�-@�@��#@���@�/@��@�Ĝ@�I�@�b@�1@��m@��@�|�@�33@��\@�-@��@��@�J@���@�hs@���@��9@�bN@�1'@�(�@�1@���@��@�@���@�^5@�M�@�-@��T@�&�@���@��/@��@�Z@�A�@��@��F@�C�@�+@�+@��@�@��\@�=q@��@���@�hs@�?}@���@�Q�@��m@�;d@�@���@�V@�@��@�X@�7L@��@��9@�r�@�r�@�Z@�1'@���@��w@�C�@�"�@�o@�
=@��@��y@���@���@���@�ff@�J@��@�@���@��7@�G�@�/@��@��`@���@��@�j@�1@�\)@��y@���@���@�~�@�^5@�=q@�$�@��@�hs@��@�V@��u@�9X@�1@��P@�K�@�o@��R@�{@��@��#@���@��@�`B@�G�@���@�bN@�1'@�w@|�@K�@K�@�@~�@~V@}�-@}�@|�j@|Z@|9X@|1@{�
@{33@z�!@z^5@y�7@yX@y&�@x�u@w�;@wl�@v�@vE�@t��@s��@s@r�H@r~�@r-@q�@q�#@q�^@qhs@q&�@p�@o��@n��@m��@mV@l�@lz�@l(�@k��@j��@i��@i��@i�@h��@h�u@g��@f�@f5?@e�h@d��@d9X@c�
@cdZ@co@b��@b~�@a��@a�@`��@`Q�@`  @_��@_�P@_K�@^�y@^E�@]�@]�@\��@\1@[ƨ@[�@[S�@["�@Z�@Z��@Zn�@Z-@Y��@Y7L@X��@X�`@X��@X�9@X�u@XQ�@Xb@Wl�@W
=@V��@Vff@VV@V$�@U@U`B@U�@T��@T��@T�@TI�@S�m@St�@S33@R�@R�!@Rn�@Q��@QX@P�9@P�@Pr�@PA�@O�;@O�P@OK�@O
=@N�@N�+@NV@N5?@N{@N@M�T@M��@M�@L�/@LI�@K��@K��@KC�@J�@J��@JM�@I�#@I��@Ix�@I�@H��@Hr�@G�@G
=@F��@F�+@Fv�@FE�@E�@E�h@Ep�@E/@D�@D�/@D��@D�j@D�@C��@CdZ@B��@B^5@B=q@B�@A�#@A�^@AX@@�9@@A�@?��@?+@?
=@>�y@>�R@>V@>{@=�@=O�@=�@<�@<(�@;ƨ@;dZ@;C�@;C�@:�@:~�@9��@9�^@9x�@8��@8�`@8Ĝ@8�9@8�9@8��@8��@8��@8�@8A�@7�@7�P@7\)@7;d@7
=@6�R@6v�@6E�@6{@5�h@5p�@5O�@5?}@5�@4��@4�@41@3�m@3ƨ@3t�@3@2^5@1��@1hs@1&�@0r�@0b@0  @/�@/�@/\)@.��@.��@.V@-��@-p�@,�@,�@+dZ@+@*�!@*n�@)�@)�7@)x�@)hs@)7L@)%@(�`@(Ĝ@(��@(��@(b@'��@'l�@'�@&�@&ff@&E�@&5?@&@%�@%��@%?}@$��@$9X@#t�@#33@"�H@"�\@!�^@!�7@!�@ ��@ A�@��@�@
=@ȴ@v�@V@@�-@��@�h@�@p�@?}@��@�/@�j@z�@Z@Z@(�@��@�F@��@�@�@dZ@o@�@�@33@"�@@��@J@�^@hs@G�@7L@�`@�9@��@�@Q�@b@�@+@�@�R@�+@�+@ff@V@$�@�@�T@��@�@`B@`B@`B@O�@?}@��@�@z�@j@Z@9X@(�@�@�m@��@�@C�@@��@�!@�\@�\@n�@=q@-@J@�@��@�^@��@��@�7@G�@��@�9@�u@A�@  @�@�;@�@|�@K�@��@�@�@�R@��@��@V@$�@{@�@�-@p�@O�@?}@V@�j@j@�@�m@ƨ@ƨ@�F@�F@�F@��@S�@C�@
�@
�H@
��@
��@
�\@
^5@
-@	��@	�#@	��@	��@	��@	�^@	�^@	��@	��@	�7@	�7@	x�@	&�@	%@��@Ĝ@�u@1'@�@�w@|�@\)@\)A�E�A�I�A�K�A�I�A�E�A�G�A�C�A�;dA�7LA�;dA�;dA�33A�5?A�9XA�;dA�=qA�9XA�33A�9XA�9XA�5?A�33A�1'A�5?A�9XA�5?A�5?A�7LA�7LA�5?A�33A�7LA�9XA�9XA�5?A�7LA�;dA�5?A�1'A�/A�33A�;dA�7LA�33A�33A�7LA�;dA�9XA�33A�-A�-A�1'A�+A�+A�-A�33A�1'A�(�A�&�A�&�A�-A�/A�-A�&�A�+A�/A�1'A�-A�+A�(�A�+A�/A�-A�(�A�&�A�(�A�-A�(�A�+A�-A�1'A�+A�(�A�&�A�&�A�&�A�(�A�&�A�$�A�$�A�&�A�(�A�(�A�$�A�$�A�$�A�(�A�+A�&�A�$�A�$�A�(�A�+A�+A�&�A�&�A�+A�+A�&�A�&�A�(�A�+A�(�A�&�A�&�A�+A�+A�(�A�&�A�$�A�(�A�+A�(�A�&�A�&�A�&�A�+A�(�A�$�A�$�A�$�A�&�A�+A�$�A�"�A�$�A�&�A�$�A�"�A� �A� �A�$�A�$�A�$�A�"�A��A��A� �A��A��A��A�{A�oA�{A��A��A�{A�bA�bA�VA�%A�ĜA݅A��A�p�A��A�7LA�A�+A��#A�A��`A�I�AжFA��A��AϋDA�K�A�
=AθRA��A��
Aʴ9A�^5A�33A���AǑhA�E�A��mAƃA��A�z�A�33A�O�AhA�\)A�E�A�1A��A�ȴA�|�A�K�A�9XA�/A�&�A��A�%A��yA��
A�ƨA��wA�A�ȴA��
A�%A�&�A�Q�A�hsA�l�A�C�A�A���A�n�A���A��mA�S�A�n�A��A���A�|�A���A��hA�^5A�;dA�
=A��/A��FA��A�+A���A�ĜA��DA�K�A�&�A��`A���A�ffA�S�A�C�A��A���A��HA��wA���A�^5A��
A��A�t�A���A��A���A�  A���A��A���A��A���A��A�p�A�ffA�\)A�VA�/A�A��HA���A��RA��!A���A��7A�n�A�ZA�?}A�A��#A���A�z�A�A�A��A�JA�A���A���A��A���A�;dA��/A���A�C�A��/A���A�n�A�1'A�%A��
A��-A��^A��jA�A���A�ȴA�ȴA�ȴA���A���A���A�ĜA�ĜA��wA��9A��A���A���A��DA�9XA�VA�oA��A�A�%A�A�A�A�  A���A���A��uA��A�|�A�x�A�n�A�\)A�;dA�1A��#A���A��wA��-A��7A�p�A�5?A�  A��-A�S�A���A��A�ffA�"�A���A��A�^5A�33A���A�ĜA���A���A���A��\A��A��A�t�A�ffA�bNA�^5A�M�A�7LA�"�A���A���A�A�=qA�bA��;A��jA���A�^5A���A��+A�O�A�33A��A�A���A��\A�r�A�M�A��A��A�hsA�JA��A��A���A�K�A��A�$�A���A��A��HA��A���A��uA� �A��A��wA�K�A�ffA���A�=qA��+A�O�A�1'A���A��A�t�A�  A�~�A��A�O�A�
=A�ȴA��uA�\)A�33A��yA��A�%A��jA��+A�/A��A�1'A�A��TA���A��^A���A�l�A��A���A���A�ZA��A��A��#A��-A��7A�p�A�bNA�Q�A�;dA�+A��A��A��A�^5A�$�A���A���A�x�A�K�A�"�A��yA�ĜA���A�n�A�Q�A�5?A�VA��/A���A�n�A�-A��A��!A�dZA�  A�^5Ap�A~Q�A}hsA|�Ay
=Ax1Awl�Aw�Av��Av~�Av-Au��At�AtZAs��As�hAs?}AsoAr��Aq�;Aq
=Ao33Al�HAlJAk��Akp�AkhsAk7LAj�/Aj��Aj��Ajz�AjI�Aj�Ai�AiAi��Ai��Ai�hAix�AihsAi`BAiK�Ai33Ai"�Ai"�Ai%Ai
=Ah��Ah�yAh�HAh�/Ah��Ah�!Ahv�AhA�Ah9XAhbAg�Ag`BAgS�AgG�Ag�Af�AfAe"�Ab�DA]�;A\$�A[�FAZ�uAZJAY�wAY��AY��AYl�AYC�AY�AX�AXZAW�-AW?}AV�9AU�^AU�AT9XAR�jAP��AO��AOx�AOp�AOl�AOhsAOhsAOhsAOdZAO\)AOG�AO+AOoAN��AN�AN�HAN�uAM�PAM/AL��AL5?AK��AKp�AJ��AJ��AJ-AI�
AI`BAIoAH�!AG�^AF �AE�AEO�AD�HAD�\ADQ�AC�AC��ACx�ACC�AC�AB�HAB�DAAAAXA@��A?��A?�-A?��A?�A?�7A?�PA?��A?��A?�-A?A?��A?��A?��A?��A?33A>�yA>�A>��A>ȴA>�!A>��A>�uA>�DA>�DA>r�A>ZA>ZA>ZA>VA>Q�A>I�A>=qA>9XA>$�A>$�A>$�A> �A> �A>$�A>(�A>(�A> �A>�A>JA>JA>A>  A=��A=�A=��A=;dA<ĜA<bNA<$�A;��A;�wA;�7A;"�A:�RA:VA9�A9��A9�mA9K�A9�A9oA9�A9�A9VA8�A8ĜA8~�A8Q�A81A7�mA7��A8  A7�A7�A7�;A7��A7�^A7��A7`BA7K�A7/A7VA6�/A6�jA6��A6n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              A�I�A�A�A�9XA�7LA�;dA�7LA�5?A�5?A�7LA�5?A�9XA�7LA�33A�5?A�1'A�/A�+A�-A�+A�(�A�-A�&�A�&�A�(�A�(�A�(�A�(�A�$�A� �A�{A�$�A�JA��
A�l�A¸RA�  A�-A�A��DA���A��\A��wA���A��FA�;dA�ĜA��9A��A��RA���A��DA�E�A�x�A�33A��FA��A�x�A���A��A��A�9XA�v�A��!A���A��A���A�bNAw�TAtQ�Ao��Ak"�AjJAip�AiVAh�!Ag��AcƨAZ��AY;dAV��AP�AOhsAO�AM��AK+AH�/AE"�AC�PAA��A?��A?�A?x�A>�RA>v�A>M�A>(�A> �A>A=VA;O�A9��A9%A8(�A7�;A7;dA6r�A5�A4�A4ĜA4�\A3A2��A2 �A0��A01A/�7A.��A.ffA.A-�A,�HA,VA+C�A)VA'�A&��A%��A$�+A$1A#��A#`BA#"�A"��A"jA!�A!l�A ��A ffA��A�yA�A�A�A?}A�AA��A~�AZAA�A5?Al�A�\AffAbAp�A"�A��A��A��A;dA�A^5A�TAdZA?}A�A��A=qA�-A��A��AE�AA�wAC�A%AbNA�#A��Ax�A/AAz�AƨAO�A
��A
�+A
E�A	�;A	`BA��AĜAVA�A��A�A\)A��A�AVA�PA;dAA�jAz�A1'AA�AdZA;dA
=A��AffAM�A(�A��AA��A7LA ��A ~�A -@�dZ@��+@��@���@���@�+@���@�~�@���@���@�dZ@��\@���@�%@�D@�Q�@�1@��;@�@���@�ȴ@�!@�@�\@�ff@�J@�`B@�1'@�33@��@�ff@�-@�@��T@��@�w@�;d@�o@�+@���@��/@�Q�@�@�|�@�C�@���@�n�@�?}@��@�Ĝ@�1'@�l�@��@�n�@�^@��@��u@�(�@߮@�t�@�o@��@޸R@�M�@��T@�G�@ܛ�@۾w@��@�ȴ@��@ٺ^@�G�@��`@؃@�bN@�Q�@�Q�@�Q�@� �@�\)@�@ա�@�O�@���@ԓu@�1'@��m@���@���@У�@�j@�(�@Ϯ@�o@��H@Χ�@��@�x�@��@���@�A�@˾w@�C�@��y@�n�@��@���@ɑh@�p�@�O�@�/@�z�@Ǯ@��@�ff@�`B@���@�Z@þw@�dZ@��y@�~�@���@�x�@�?}@�Ĝ@���@��@�5?@���@�G�@�V@��/@�r�@�b@��@�o@��H@�5?@�/@��@�z�@�Q�@�1'@��w@�l�@���@���@�x�@�`B@�?}@���@�Z@��;@�S�@���@�5?@���@�Ĝ@�Q�@�(�@�1@��;@���@�\)@��@�ff@�E�@��T@�hs@�&�@�r�@� �@���@�ƨ@���@��@�S�@�
=@��@�ȴ@�n�@�@���@�7L@�Z@���@���@�o@���@��\@�M�@�{@��@��-@�`B@�V@��@���@�j@�1@��;@��w@�l�@�
=@���@�v�@�J@��h@��@�z�@�\)@���@�V@�-@�@��#@���@�/@��@�Ĝ@�I�@�b@�1@��m@��@�|�@�33@��\@�-@��@��@�J@���@�hs@���@��9@�bN@�1'@�(�@�1@���@��@�@���@�^5@�M�@�-@��T@�&�@���@��/@��@�Z@�A�@��@��F@�C�@�+@�+@��@�@��\@�=q@��@���@�hs@�?}@���@�Q�@��m@�;d@�@���@�V@�@��@�X@�7L@��@��9@�r�@�r�@�Z@�1'@���@��w@�C�@�"�@�o@�
=@��@��y@���@���@���@�ff@�J@��@�@���@��7@�G�@�/@��@��`@���@��@�j@�1@�\)@��y@���@���@�~�@�^5@�=q@�$�@��@�hs@��@�V@��u@�9X@�1@��P@�K�@�o@��R@�{@��@��#@���@��@�`B@�G�@���@�bN@�1'@�w@|�@K�@K�@�@~�@~V@}�-@}�@|�j@|Z@|9X@|1@{�
@{33@z�!@z^5@y�7@yX@y&�@x�u@w�;@wl�@v�@vE�@t��@s��@s@r�H@r~�@r-@q�@q�#@q�^@qhs@q&�@p�@o��@n��@m��@mV@l�@lz�@l(�@k��@j��@i��@i��@i�@h��@h�u@g��@f�@f5?@e�h@d��@d9X@c�
@cdZ@co@b��@b~�@a��@a�@`��@`Q�@`  @_��@_�P@_K�@^�y@^E�@]�@]�@\��@\1@[ƨ@[�@[S�@["�@Z�@Z��@Zn�@Z-@Y��@Y7L@X��@X�`@X��@X�9@X�u@XQ�@Xb@Wl�@W
=@V��@Vff@VV@V$�@U@U`B@U�@T��@T��@T�@TI�@S�m@St�@S33@R�@R�!@Rn�@Q��@QX@P�9@P�@Pr�@PA�@O�;@O�P@OK�@O
=@N�@N�+@NV@N5?@N{@N@M�T@M��@M�@L�/@LI�@K��@K��@KC�@J�@J��@JM�@I�#@I��@Ix�@I�@H��@Hr�@G�@G
=@F��@F�+@Fv�@FE�@E�@E�h@Ep�@E/@D�@D�/@D��@D�j@D�@C��@CdZ@B��@B^5@B=q@B�@A�#@A�^@AX@@�9@@A�@?��@?+@?
=@>�y@>�R@>V@>{@=�@=O�@=�@<�@<(�@;ƨ@;dZ@;C�@;C�@:�@:~�@9��@9�^@9x�@8��@8�`@8Ĝ@8�9@8�9@8��@8��@8��@8�@8A�@7�@7�P@7\)@7;d@7
=@6�R@6v�@6E�@6{@5�h@5p�@5O�@5?}@5�@4��@4�@41@3�m@3ƨ@3t�@3@2^5@1��@1hs@1&�@0r�@0b@0  @/�@/�@/\)@.��@.��@.V@-��@-p�@,�@,�@+dZ@+@*�!@*n�@)�@)�7@)x�@)hs@)7L@)%@(�`@(Ĝ@(��@(��@(b@'��@'l�@'�@&�@&ff@&E�@&5?@&@%�@%��@%?}@$��@$9X@#t�@#33@"�H@"�\@!�^@!�7@!�@ ��@ A�@��@�@
=@ȴ@v�@V@@�-@��@�h@�@p�@?}@��@�/@�j@z�@Z@Z@(�@��@�F@��@�@�@dZ@o@�@�@33@"�@@��@J@�^@hs@G�@7L@�`@�9@��@�@Q�@b@�@+@�@�R@�+@�+@ff@V@$�@�@�T@��@�@`B@`B@`B@O�@?}@��@�@z�@j@Z@9X@(�@�@�m@��@�@C�@@��@�!@�\@�\@n�@=q@-@J@�@��@�^@��@��@�7@G�@��@�9@�u@A�@  @�@�;@�@|�@K�@��@�@�@�R@��@��@V@$�@{@�@�-@p�@O�@?}@V@�j@j@�@�m@ƨ@ƨ@�F@�F@�F@��@S�@C�@
�@
�H@
��@
��@
�\@
^5@
-@	��@	�#@	��@	��@	��@	�^@	�^@	��@	��@	�7@	�7@	x�@	&�@	%@��@Ĝ@�u@1'@�@�w@|�@\)G�O�A�E�A�I�A�K�A�I�A�E�A�G�A�C�A�;dA�7LA�;dA�;dA�33A�5?A�9XA�;dA�=qA�9XA�33A�9XA�9XA�5?A�33A�1'A�5?A�9XA�5?A�5?A�7LA�7LA�5?A�33A�7LA�9XA�9XA�5?A�7LA�;dA�5?A�1'A�/A�33A�;dA�7LA�33A�33A�7LA�;dA�9XA�33A�-A�-A�1'A�+A�+A�-A�33A�1'A�(�A�&�A�&�A�-A�/A�-A�&�A�+A�/A�1'A�-A�+A�(�A�+A�/A�-A�(�A�&�A�(�A�-A�(�A�+A�-A�1'A�+A�(�A�&�A�&�A�&�A�(�A�&�A�$�A�$�A�&�A�(�A�(�A�$�A�$�A�$�A�(�A�+A�&�A�$�A�$�A�(�A�+A�+A�&�A�&�A�+A�+A�&�A�&�A�(�A�+A�(�A�&�A�&�A�+A�+A�(�A�&�A�$�A�(�A�+A�(�A�&�A�&�A�&�A�+A�(�A�$�A�$�A�$�A�&�A�+A�$�A�"�A�$�A�&�A�$�A�"�A� �A� �A�$�A�$�A�$�A�"�A��A��A� �A��A��A��A�{A�oA�{A��A��A�{A�bA�bA�VA�%A�ĜA݅A��A�p�A��A�7LA�A�+A��#A�A��`A�I�AжFA��A��AϋDA�K�A�
=AθRA��A��
Aʴ9A�^5A�33A���AǑhA�E�A��mAƃA��A�z�A�33A�O�AhA�\)A�E�A�1A��A�ȴA�|�A�K�A�9XA�/A�&�A��A�%A��yA��
A�ƨA��wA�A�ȴA��
A�%A�&�A�Q�A�hsA�l�A�C�A�A���A�n�A���A��mA�S�A�n�A��A���A�|�A���A��hA�^5A�;dA�
=A��/A��FA��A�+A���A�ĜA��DA�K�A�&�A��`A���A�ffA�S�A�C�A��A���A��HA��wA���A�^5A��
A��A�t�A���A��A���A�  A���A��A���A��A���A��A�p�A�ffA�\)A�VA�/A�A��HA���A��RA��!A���A��7A�n�A�ZA�?}A�A��#A���A�z�A�A�A��A�JA�A���A���A��A���A�;dA��/A���A�C�A��/A���A�n�A�1'A�%A��
A��-A��^A��jA�A���A�ȴA�ȴA�ȴA���A���A���A�ĜA�ĜA��wA��9A��A���A���A��DA�9XA�VA�oA��A�A�%A�A�A�A�  A���A���A��uA��A�|�A�x�A�n�A�\)A�;dA�1A��#A���A��wA��-A��7A�p�A�5?A�  A��-A�S�A���A��A�ffA�"�A���A��A�^5A�33A���A�ĜA���A���A���A��\A��A��A�t�A�ffA�bNA�^5A�M�A�7LA�"�A���A���A�A�=qA�bA��;A��jA���A�^5A���A��+A�O�A�33A��A�A���A��\A�r�A�M�A��A��A�hsA�JA��A��A���A�K�A��A�$�A���A��A��HA��A���A��uA� �A��A��wA�K�A�ffA���A�=qA��+A�O�A�1'A���A��A�t�A�  A�~�A��A�O�A�
=A�ȴA��uA�\)A�33A��yA��A�%A��jA��+A�/A��A�1'A�A��TA���A��^A���A�l�A��A���A���A�ZA��A��A��#A��-A��7A�p�A�bNA�Q�A�;dA�+A��A��A��A�^5A�$�A���A���A�x�A�K�A�"�A��yA�ĜA���A�n�A�Q�A�5?A�VA��/A���A�n�A�-A��A��!A�dZA�  A�^5Ap�A~Q�A}hsA|�Ay
=Ax1Awl�Aw�Av��Av~�Av-Au��At�AtZAs��As�hAs?}AsoAr��Aq�;Aq
=Ao33Al�HAlJAk��Akp�AkhsAk7LAj�/Aj��Aj��Ajz�AjI�Aj�Ai�AiAi��Ai��Ai�hAix�AihsAi`BAiK�Ai33Ai"�Ai"�Ai%Ai
=Ah��Ah�yAh�HAh�/Ah��Ah�!Ahv�AhA�Ah9XAhbAg�Ag`BAgS�AgG�Ag�Af�AfAe"�Ab�DA]�;A\$�A[�FAZ�uAZJAY�wAY��AY��AYl�AYC�AY�AX�AXZAW�-AW?}AV�9AU�^AU�AT9XAR�jAP��AO��AOx�AOp�AOl�AOhsAOhsAOhsAOdZAO\)AOG�AO+AOoAN��AN�AN�HAN�uAM�PAM/AL��AL5?AK��AKp�AJ��AJ��AJ-AI�
AI`BAIoAH�!AG�^AF �AE�AEO�AD�HAD�\ADQ�AC�AC��ACx�ACC�AC�AB�HAB�DAAAAXA@��A?��A?�-A?��A?�A?�7A?�PA?��A?��A?�-A?A?��A?��A?��A?��A?33A>�yA>�A>��A>ȴA>�!A>��A>�uA>�DA>�DA>r�A>ZA>ZA>ZA>VA>Q�A>I�A>=qA>9XA>$�A>$�A>$�A> �A> �A>$�A>(�A>(�A> �A>�A>JA>JA>A>  A=��A=�A=��A=;dA<ĜA<bNA<$�A;��A;�wA;�7A;"�A:�RA:VA9�A9��A9�mA9K�A9�A9oA9�A9�A9VA8�A8ĜA8~�A8Q�A81A7�mA7��A8  A7�A7�A7�;A7��A7�^A7��A7`BA7K�A7/A7VA6�/A6�jA6��A6n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	ncB	ncB	m�B	m�B	m�B	n/B	m�B	m�B	m�B	m�B	m]B	m�B	m�B	m�B	n/B	m]B	m�B	m]B	m�B	m�B	m)B	m�B	m�B	m]B	m)B	l�B	l"B	j�B	hsB	bNB	�.B
<6B
_B
PB
kQB
�4BP}BS[B;0B;dBbNB�_B��B��B�wB��B�B�!B��B��B�~B��Br�BiyBMjB3�B
��B
��B
f2B
�B	�B	�B	˒B	�EB	�^B	��B	�LB	�@B	x�B	v�B	h�B	k�B	l�B	iDB	e�B	g�B	s�B	�B	B	�B	�B��B�B�2B�B�B��B��B	{B	�B	"�B	/OB	A B	E�B	HKB	QB	_�B	h
B	s�B	zB	~�B	��B	�~B	�uB	�_B	�~B	��B	��B	��B	�'B	�'B	��B	�B	�ZB	�B	�B	��B	�B	��B
�B
VB
FB
�B
�B
$B
�B
B
#:B
$�B
$B
%FB
$�B
&�B
&�B
,B
1'B
2�B
1[B
4B
33B
2aB
1�B
6�B
1�B
1'B
4�B
6zB
7�B
8RB
7�B
8B
=�B
?B
?B
@�B
D3B
EmB
F�B
H�B
IB
H�B
H�B
G�B
H�B
IRB
NB
NpB
N�B
MjB
OB
M�B
MB
MjB
MB
MjB
LdB
LdB
K�B
JXB
H�B
H�B
HKB
GEB
HB
F�B
GB
E�B
D�B
E�B
E9B
C�B
B[B
B�B
CaB
B�B
A�B
AUB
@�B
?}B
>BB
>B
=qB
;dB
;dB
:�B
9�B
:�B
9�B
8�B
8B
7�B
7B
7B
6FB
6B
5�B
6FB
5�B
4�B
4�B
2�B
2�B
2�B
1�B
1�B
1�B
.�B
.�B
.B
,�B
+�B
)_B
&�B
'RB
$tB
#:B
!�B
�B
�B
�B
�B
VB
�B
�B
�B
~B
IB
B
xB
xB
B
�B
�B
eB
1B
�B
_B
B
�B
YB
�B
YB
$B
$B
SB
�B
�B
�B
FB
�B
�B
B
�B
�B
B
@B
@B
�B
@B
B
@B
�B
�B
�B
oB
:B
{B
{B
�B
�B
B
�B
�B
bB
�B
�B
.B
�B
�B
�B
�B
\B
.B
 B
�B
\B
�B
�B
VB
PB
�B
B
B
~B
B
�B
�B
~B
�B
�B
�B
�B
B
xB
DB
xB
DB
xB
�B
xB
DB
DB
B

�B

rB
DB
�B

�B
�B
JB
xB
B
JB
~B
~B
JB
�B
~B
B
~B
�B
PB
PB
�B
�B
�B
�B
(B
�B
(B
�B
�B
�B
�B
�B
.B
�B
�B
�B
�B
B
4B
4B
 B
�B
 B
�B
�B
�B
:B
B
oB
@B
B
B
B
�B
uB
B
FB
�B
B
{B
FB
{B
�B
MB
�B
�B
�B
�B
B
SB
B
B
�B
�B
�B
$B
+B
�B
_B
�B
�B
1B
eB
�B
�B
�B
�B
7B
7B
B
kB
kB
kB
B
kB
kB
kB
�B
	B
=B
qB
B
�B
CB
�B
CB
CB
B
xB
CB
�B
xB
IB
B
B
~B
B
B
�B
�B
VB
VB
VB
VB
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
 �B
 �B
 �B
 �B
 �B
 �B
#nB
#:B
$@B
%�B
&LB
&�B
&�B
'�B
(�B
($B
(XB
(XB
(XB
)_B
)*B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
+kB
+B
+6B
+kB
,B
+�B
+�B
+�B
,=B
,B
,�B
-�B
.IB
.IB
.�B
/B
/OB
/OB
/OB
/�B
/OB
/�B
/�B
/�B
/�B
0!B
0UB
0UB
0�B
0UB
0�B
1'B
1'B
1'B
1[B
1�B
1�B
1[B
1�B
2�B
2�B
3hB
33B
3hB
33B
33B
2�B
3hB
3�B
33B
33B
3hB
2�B
2aB
33B
2�B
2�B
3�B
4�B
4�B
4�B
4�B
5tB
5B
5B
5�B
6�B
7�B
7�B
7�B
8RB
8�B
9XB
9$B
9$B
8�B
9XB
:*B
9�B
9�B
9�B
9�B
:*B
9�B
9�B
:�B
:^B
:^B
:�B
;dB
;dB
;dB
;�B
=qB
>B
=�B
>B
>wB
>wB
>�B
>�B
>�B
>�B
>�B
?HB
?}B
@�B
AUB
A�B
A�B
AUB
A�B
B'B
B[B
B�B
B�B
C-B
B�B
B�B
D3B
D3B
C�B
D3B
D�B
EB
D�B
D�B
D�B
D�B
EmB
E�B
FB
FtB
F?B
F�B
F�B
GB
F�B
G�B
HB
HB
H�B
H�B
H�B
H�B
H�B
H�B
IB
IB
I�B
IRB
IRB
J#B
J#B
J#B
J#B
J#B
J#B
J#B
JXB
J#B
K)B
J�B
K^B
K�B
K^B
K�B
L0B
LdB
M6B
M6B
MjB
M6B
MjB
NpB
N�B
NpB
M�B
M�B
MjB
M�B
N�B
OB
OB
OB
OBB
PB
P�B
QB
QB
QNB
Q�B
QNB
QB
QNB
P�B
QB
P�B
QNB
Q�B
RTB
RTB
R�B
R�B
S[B
S�B
T,B
S�B
S�B
S�B
T�B
T�B
UgB
VmB
W
B
WsB
W?B
WsB
WsB
W�B
W�B
W�B
XEB
XEB
XEB
XEB
XEB
YB
XB
XB
X�B
XB
XB
XB
YB
YB
YKB
X�B
YB
YKB
Y�B
YB
YB
ZB
Z�B
Z�B
[WB
[WB
[�B
\]B
\�B
\�B
\�B
\�B
\�B
]/B
]�B
^5B
^jB
^�B
_pB
_;B
_pB
_pB
_�B
_pB
_pB
_pB
_;B
_�B
_�B
`B
`vB
`�B
`�B
aB
aB
aB
aHB
a�B
a�B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
c B
c�B
d&B
d&B
dZB
e,B
e�B
e`B
e�B
e�B
f2B
f�B
gB
f�B
f�B
f�B
gmB
g�B
h>B
hsB
h�B
h�B
iyB
i�B
jB
jKB
jKB
jKB
jB
j�B
jB
jB
kB
j�B
kB
kQB
k�B
l"B
lWB
l�B
l�B
l�B
m)B
m)B
m]B
m�B
m�B
m�B
m�B
n/B
n�B
n�B
o B
oiB
o�B
p;B
p�B
poB
qB
qB
qAB
qvB
q�B
r|B
r|B
rGB
r|B
r|B
r�B
r�B
sB
sB
sB
sB
sMB
s�B
tTB
tTB
t�B
tTB
t�B
u%B
u%B
u�B
v�B
w�B
wfB
wfB
u�B
v`B
v�B
v+B
v�B
w2B
wfB
xB
xB
w�B
xlB
x�B
y	B
y>B
zDB
y�B
zB
y�B
z�B
zDB
{�B
{�B
{�B
{JB
|B
{�B
{JB
{JB
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}VB
}"B
}�B
}�B
}�B
~�B
~]B
~�B
~�B
~�B
.B
.B
~�B
.B
cB
~�B
cB
.B
cB
cB
�B
�B
�4B
�4B
��B
��B
��B
��B
�;B
�;B
�oB
��B
�uB
��B
��B
��B
��B
�GB
�B
�B
�GB
�B
�{B
�{B
�{B
��B
�MB
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
��B
��B
�YB
�YB
��B
��B
��B
�_B
��B
��B
��B
��B
�1B
�fB
��B
�fB
�fB
��B
��B
��B
��B
��B
�lB
�B
��B
��B
�7B
��B
�B	o5B	m�B	m�B	oiB	o B	m)B	n/B	n�B	q�B	l�B	m�B	m�B	ncB	n/B	m)B	m]B	ncB	oiB	m)B	l�B	ncB	n�B	o B	m�B	l�B	m�B	n�B	m]B	m)B	n/B	n�B	n/B	l�B	m]B	n�B	ncB	m]B	m�B	l�B	ncB	m�B	m)B	m�B	o5B	m�B	m)B	k�B	m�B	o5B	o�B	l�B	l�B	m�B	o B	m)B	m]B	l�B	m�B	ncB	m�B	m]B	l"B	m�B	n�B	l�B	l�B	j�B	l�B	n/B	n�B	m]B	l�B	l�B	n/B	o�B	m]B	kQB	m)B	m�B	m�B	ncB	l"B	l�B	n/B	ncB	ncB	l�B	m]B	ncB	n�B	m)B	l�B	l�B	m�B	ncB	m�B	l�B	l�B	m�B	n�B	n/B	l�B	lWB	l�B	m�B	n/B	lWB	l�B	m�B	m�B	m�B	l"B	lWB	m]B	m�B	l"B	k�B	l"B	m)B	m�B	lWB	kQB	l"B	l�B	l�B	k�B	j�B	k�B	lWB	l�B	lWB	kB	jB	k�B	l"B	kB	i�B	jB	jB	kB	jKB	iDB	h>B	h
B	h>B	h�B	h�B	gmB	f�B	d�B	e�B	f2B	e�B	cTB	a|B	_�B	_�B	_;B	]dB	\)B	[WB	a�B	gB	e`B	��B	�B	�CB
T�B
.IB
.B
@�B
HKB
=�B
R B
OBB
E�B
UgB
OB
NpB
IB
]dB
�XB
qAB
qB
^5B
B�B
@�B
EB
K�B
E�B
R�B
I�B
��B
ffB
f�B
l�B
jKB
o5B
jB
l�B
rGB
n�B
m]B
kQB
k�B
m�B
rGB
y	B
.B
��B
�_B
��B
��B
�:B
ǮB
�2B
�BABOB0�B4B;�BGzBdZBv�Be�B�xBn�B]dB_�BW�BUgBU�BV9BR BR�BPBR�BP}BJ�BL�BG�BEBD�BFtB=<B7�B3hB5B3hB0UB1[B.}B-CB9XB?HBDgB<BIB<jB<�B?}BDgBF?BOvBY�Ba�Bf�BjBk�Bn�BuZB��B�	B��B��B��B��B�+B�xB�7B��B�_B�~B�GB��B{�B�B��B��B�fB��B�eB��B��B��B��B��B��B�$B��B��B�B��B�}B��B��B��B��B��B�$B�B��B�kB�B�wB�}B�B��B�'B�UB�OB��B�'B� B��B�B��B��B�qB�qB�B�B��B�B��B�B��B�=B�kB�qB�CB�6B�$B�nB��B�tB��B�:B�7B��B��B�hB��B�xB��B�MB�FB�B�B�B��B��B��ByrBu�Bu�Bu�Bs�Br�BsBqABncBm]Bn/Bl�BiBiBs�Bt�Bj�BW�BYBR�BT�BRTBYBT,BH�BEmBD�BB�BF�B@�B:�B8RB5tB3�B5�B=BeBB!-B
=B%B
�B
�yB
�>B
�B
ٴB
�B
ɺB
�?B
�'B
��B
��B
��B
��B
��B
�1B
�SB
iDB
[�B
OvB
DgB
8�B
8B
2aB
($B
�B
�B
hB
;B
GB
+B
	�B
uB	��B	�B	�8B	��B	��B	��B	�dB	�/B	ߤB	�|B	�B	��B
$�B	�HB	�&B	�dB	��B	ȴB	�HB	�6B	�RB	��B	ȴB	�zB	��B	�B	��B	�B	�B	ŢB	�9B	��B	��B	��B	�aB	��B	�hB	��B	��B	�hB	�9B	�hB	�hB	�'B	��B	��B	�=B	��B	��B	��B	��B	�B	�\B	��B	��B	��B	�oB	�1B	�B	��B	}�B	�AB	�B	�B	{�B	yrB	m�B	sMB	h
B	l�B	o5B	rB	��B	�B	j�B	o B	e�B	e�B	iyB	i�B	e�B	kQB	jB	n�B	j�B	k�B	n/B	k�B	l�B	ncB	l�B	kB	k�B	m�B	ncB	jB	jB	jB	e�B	f�B	f�B	e�B	dZB	d&B	dZB	gB	iB	e`B	h�B	m]B	jB	e`B	dZB	d&B	d&B	k�B	k�B	�B	�B	%zB	*�B	VB	 �B	uB	�B	�B	JB	�B	JB	�B	xB		�B��B	�B	B��B	B	B	�B��B��B�B��B�B�B�B�|B�B��B��B�B�B�B�B�JB		lB��B��B	�B�|B��B� B�B�mB�B�B�8B��B�.B	 �B�AB��B�MB�%B�AB�>B�(B��B�B��B�.B	  B	�B��B	.B	bB	�B	�B	+B	�B	 �B	�B	!�B	$�B	%FB	%�B	'RB	*eB	.IB	8�B	5?B	9�B	AUB	AUB	A�B	C�B	C�B	C�B	CaB	G�B	GzB	H�B	H�B	HB	G�B	G�B	JXB	IRB	NB	P�B	R�B	S�B	W
B	XEB	Y�B	]dB	f2B	h�B	iDB	g�B	hsB	g�B	h�B	e�B	jB	zB	xlB	|PB	x�B	xlB	y�B	z�B	{B	z�B	|B	sB	n�B	�=B	� B	��B	��B	��B	��B	�YB	�B	��B	�rB	�B	�.B	��B	�JB	��B	�B	��B	��B	�uB	�MB	�SB	�1B	��B	�YB	��B	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              B	n�B	n�B	nB	m�B	m�B	nIB	m�B	m�B	m�B	m�B	mwB	m�B	m�B	m�B	nIB	mwB	m�B	mwB	m�B	m�B	m]B	m�B	m�B	mwB	mCB	l�B	l=B	k6B	jB	q�B	��B
T�B
��B
h�B
v�B
�pBe�BaBLBGEBg�B�<B��B�9B�UB�kB��B��B�nB��B�B�B{dBxRBY�BI�B�B
�/B
��B
(�B
�B
 OB	҉B	уB	ÖB	ðB	��B	��B	��B	��B	l�B	m�B	nB	j�B	j0B	u�B	�pB	$�B	�B	FB	
�B��B��B	 B�	B�B��B	 B	
XB	�B	#�B	1�B	A�B	FtB	H�B	QhB	`�B	k�B	yrB	cB	�oB	��B	��B	��B	�=B	��B	��B	�QB	��B	�B	��B	�B	��B	�B	�B	�3B	�XB	�rB	��B
B
}B
yB
!�B
 �B
�B
�B
"hB
$�B
%zB
%�B
&LB
&fB
'�B
(>B
-�B
3MB
4B
4B
6�B
4B
2�B
4�B
9	B
3hB
4�B
8B
8B
88B
8�B
8RB
:�B
@�B
?�B
@iB
B�B
EmB
G_B
IB
J=B
J�B
J�B
I�B
IlB
J#B
I�B
O(B
O�B
PHB
O�B
Q�B
N�B
N<B
NVB
N"B
OB
MjB
N�B
MjB
K)B
I�B
I�B
IB
IRB
J�B
H�B
IRB
F%B
E�B
G_B
GB
EmB
CGB
D3B
D�B
C-B
BB
B�B
BB
@�B
?�B
@�B
>�B
<PB
<jB
;�B
;B
<B
:�B
9	B
8�B
8lB
88B
8RB
6�B
6�B
6�B
7B
6`B
6FB
6FB
4B
3�B
4TB
3�B
5?B
3�B
/5B
/�B
.�B
-]B
-CB
+B
)_B
(�B
%�B
$�B
"�B
 vB
 B
 B
VB
 \B
!B
B
�B
�B
�B
�B
�B
�B
�B
�B
QB
�B
B
eB
�B
xB
�B
�B
�B
yB
1B
+B
sB
�B
2B
�B
2B
�B
�B
{B
�B
B
�B
FB
�B
�B
FB
�B
B
[B
�B
@B
�B
B
MB
�B
2B
gB
FB
�B
B
4B
�B
}B
�B
B
�B
�B
�B
�B
�B
[B
�B
�B
\B
\B
B
B
�B
B
(B
B
�B
�B
B
�B
dB
�B
B
dB
�B
~B
JB
dB
�B
dB
~B
�B
�B
�B
^B
DB
�B
�B
�B
B
�B
B
�B
<B
B
jB
PB
6B
�B
B
B
"B
�B
�B
pB
�B
�B
�B
�B
�B
�B
�B
vB
�B
�B
bB
�B
}B
B
bB
�B
4B
�B
�B
hB
NB
hB
:B
�B
�B
�B
@B
&B
B
B
[B
[B
[B
uB
�B
B
B
�B
�B
gB
�B
�B
SB
�B
B
B
B
SB
�B
�B
�B
�B
YB
�B
�B
�B
1B
B
KB
B
B
�B
�B
�B
B
kB
kB
�B
kB
kB
#B
�B
�B
�B
#B
�B
#B
�B
�B
)B
�B
B
�B
/B
�B
�B
�B
�B
IB
�B
IB
dB
�B
/B
dB
�B
�B
�B
�B
 BB
�B
VB
�B
 B
 vB
 \B
 �B
 \B
 �B
 B
 B
 vB
 BB
 �B
!HB
!|B
!-B
!B
!bB
"NB
#�B
#�B
$�B
&2B
&�B
'B
'8B
(�B
(�B
($B
(sB
(�B
)*B
)�B
)�B
*0B
*eB
)�B
*�B
+B
+kB
,B
+�B
+�B
,B
,�B
,�B
,"B
+�B
+�B
,�B
,�B
,�B
-�B
.�B
.�B
/5B
0B
/�B
/iB
/iB
/�B
/iB
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
1AB
1[B
1[B
1�B
1�B
2-B
1�B
2-B
3MB
3hB
3�B
3�B
3hB
3�B
3�B
3hB
3MB
4TB
4B
3hB
4B
4B
2�B
3MB
3�B
3MB
3�B
4�B
5%B
4�B
4�B
5?B
5�B
5ZB
5�B
6�B
7LB
8B
88B
7�B
8RB
9$B
9�B
9�B
9�B
9�B
9�B
:�B
:*B
:*B
9�B
:^B
:�B
:^B
:�B
:�B
:�B
:�B
;B
;�B
<B
<B
=B
>�B
>�B
>B
>wB
>�B
>�B
>�B
>�B
>�B
?.B
?HB
@ B
@�B
A�B
A�B
A�B
A�B
A�B
B'B
B�B
C-B
CaB
CB
C{B
CGB
C�B
D�B
D�B
D�B
D�B
E�B
EmB
E9B
EB
EB
E9B
F?B
FYB
F�B
F�B
F�B
GB
G+B
G_B
GEB
HKB
H�B
H�B
IlB
IB
IB
H�B
H�B
H�B
IRB
IlB
I�B
I�B
I�B
J�B
JrB
J=B
J=B
J=B
JXB
JrB
J�B
J�B
K�B
K^B
K�B
K�B
K�B
L0B
L�B
L�B
MPB
MjB
M�B
M�B
M�B
N�B
O(B
N�B
N"B
M�B
M�B
NpB
OvB
OBB
O(B
OBB
O�B
PbB
Q4B
QhB
QNB
Q�B
Q�B
QhB
Q4B
QhB
Q B
Q4B
Q4B
Q�B
R:B
R�B
R�B
S[B
S@B
S�B
S�B
T�B
T,B
T,B
TaB
T�B
UgB
VB
W
B
WsB
W�B
WYB
W�B
W�B
XEB
W�B
X+B
X�B
X_B
X_B
X_B
X�B
Y�B
X_B
X�B
YB
XEB
X+B
X_B
YKB
YB
Y�B
Y1B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[=B
[�B
[�B
\B
\�B
]/B
]/B
]B
\�B
\�B
]�B
^B
^�B
^�B
_!B
_�B
_VB
_�B
_pB
_�B
_pB
_pB
_�B
_�B
`'B
`BB
`BB
`�B
`�B
a-B
abB
aHB
aHB
a�B
a�B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
c:B
c�B
d@B
dtB
dtB
d�B
e�B
e�B
ezB
e�B
fB
f�B
g8B
gRB
gRB
gB
gmB
h
B
hsB
h�B
h�B
h�B
i_B
i�B
i�B
j0B
jB
jB
jeB
j�B
j�B
j�B
kB
kkB
kQB
kkB
k�B
l"B
l=B
lqB
l�B
l�B
mB
m�B
m�B
m�B
nIB
nB
nB
n/B
o B
o B
oOB
o�B
o�B
p;B
p�B
p�B
p�B
q[B
qAB
q�B
q�B
q�B
r�B
r�B
raB
r�B
r�B
r�B
sB
sMB
s3B
sB
sMB
s�B
s�B
tnB
tnB
t�B
t�B
u?B
u?B
u%B
u�B
wB
w�B
w�B
xB
v`B
v�B
v�B
vFB
wLB
wfB
w�B
xB
x8B
xB
x�B
yXB
y$B
y�B
zxB
y�B
z*B
y�B
z�B
zxB
{�B
{�B
{�B
{dB
|B
{�B
{dB
{dB
|B
}B
}"B
|�B
|�B
}B
}�B
}�B
}�B
}qB
~B
}�B
}�B
~�B
~wB
B
~�B
~�B
cB
HB
B
HB
}B
B
}B
HB
}B
�B
�B
�B
�iB
��B
�B
��B
��B
��B
�oB
�oB
��B
��B
�uB
��B
��B
��B
��B
�{B
�-B
�GB
��B
�aB
��B
��B
��B
�3B
��B
��B
��B
��B
��B
��B
��B
�SB
��B
�%B
��B
�?B
�B
�B
�%B
�tB
��B
��B
��B
�B
�zB
��B
��B
��B
��B
�KB
��B
��B
�fB
��B
��B
��B
��B
��B
��B
��B
�RB
�B
��B
�RB
��G�O�B	o5B	m�B	m�B	oiB	o B	m)B	n/B	n�B	q�B	l�B	m�B	m�B	ncB	n/B	m)B	m]B	ncB	oiB	m)B	l�B	ncB	n�B	o B	m�B	l�B	m�B	n�B	m]B	m)B	n/B	n�B	n/B	l�B	m]B	n�B	ncB	m]B	m�B	l�B	ncB	m�B	m)B	m�B	o5B	m�B	m)B	k�B	m�B	o5B	o�B	l�B	l�B	m�B	o B	m)B	m]B	l�B	m�B	ncB	m�B	m]B	l"B	m�B	n�B	l�B	l�B	j�B	l�B	n/B	n�B	m]B	l�B	l�B	n/B	o�B	m]B	kQB	m)B	m�B	m�B	ncB	l"B	l�B	n/B	ncB	ncB	l�B	m]B	ncB	n�B	m)B	l�B	l�B	m�B	ncB	m�B	l�B	l�B	m�B	n�B	n/B	l�B	lWB	l�B	m�B	n/B	lWB	l�B	m�B	m�B	m�B	l"B	lWB	m]B	m�B	l"B	k�B	l"B	m)B	m�B	lWB	kQB	l"B	l�B	l�B	k�B	j�B	k�B	lWB	l�B	lWB	kB	jB	k�B	l"B	kB	i�B	jB	jB	kB	jKB	iDB	h>B	h
B	h>B	h�B	h�B	gmB	f�B	d�B	e�B	f2B	e�B	cTB	a|B	_�B	_�B	_;B	]dB	\)B	[WB	a�B	gB	e`B	��B	�B	�CB
T�B
.IB
.B
@�B
HKB
=�B
R B
OBB
E�B
UgB
OB
NpB
IB
]dB
�XB
qAB
qB
^5B
B�B
@�B
EB
K�B
E�B
R�B
I�B
��B
ffB
f�B
l�B
jKB
o5B
jB
l�B
rGB
n�B
m]B
kQB
k�B
m�B
rGB
y	B
.B
��B
�_B
��B
��B
�:B
ǮB
�2B
�BABOB0�B4B;�BGzBdZBv�Be�B�xBn�B]dB_�BW�BUgBU�BV9BR BR�BPBR�BP}BJ�BL�BG�BEBD�BFtB=<B7�B3hB5B3hB0UB1[B.}B-CB9XB?HBDgB<BIB<jB<�B?}BDgBF?BOvBY�Ba�Bf�BjBk�Bn�BuZB��B�	B��B��B��B��B�+B�xB�7B��B�_B�~B�GB��B{�B�B��B��B�fB��B�eB��B��B��B��B��B��B�$B��B��B�B��B�}B��B��B��B��B��B�$B�B��B�kB�B�wB�}B�B��B�'B�UB�OB��B�'B� B��B�B��B��B�qB�qB�B�B��B�B��B�B��B�=B�kB�qB�CB�6B�$B�nB��B�tB��B�:B�7B��B��B�hB��B�xB��B�MB�FB�B�B�B��B��B��ByrBu�Bu�Bu�Bs�Br�BsBqABncBm]Bn/Bl�BiBiBs�Bt�Bj�BW�BYBR�BT�BRTBYBT,BH�BEmBD�BB�BF�B@�B:�B8RB5tB3�B5�B=BeBB!-B
=B%B
�B
�yB
�>B
�B
ٴB
�B
ɺB
�?B
�'B
��B
��B
��B
��B
��B
�1B
�SB
iDB
[�B
OvB
DgB
8�B
8B
2aB
($B
�B
�B
hB
;B
GB
+B
	�B
uB	��B	�B	�8B	��B	��B	��B	�dB	�/B	ߤB	�|B	�B	��B
$�B	�HB	�&B	�dB	��B	ȴB	�HB	�6B	�RB	��B	ȴB	�zB	��B	�B	��B	�B	�B	ŢB	�9B	��B	��B	��B	�aB	��B	�hB	��B	��B	�hB	�9B	�hB	�hB	�'B	��B	��B	�=B	��B	��B	��B	��B	�B	�\B	��B	��B	��B	�oB	�1B	�B	��B	}�B	�AB	�B	�B	{�B	yrB	m�B	sMB	h
B	l�B	o5B	rB	��B	�B	j�B	o B	e�B	e�B	iyB	i�B	e�B	kQB	jB	n�B	j�B	k�B	n/B	k�B	l�B	ncB	l�B	kB	k�B	m�B	ncB	jB	jB	jB	e�B	f�B	f�B	e�B	dZB	d&B	dZB	gB	iB	e`B	h�B	m]B	jB	e`B	dZB	d&B	d&B	k�B	k�B	�B	�B	%zB	*�B	VB	 �B	uB	�B	�B	JB	�B	JB	�B	xB		�B��B	�B	B��B	B	B	�B��B��B�B��B�B�B�B�|B�B��B��B�B�B�B�B�JB		lB��B��B	�B�|B��B� B�B�mB�B�B�8B��B�.B	 �B�AB��B�MB�%B�AB�>B�(B��B�B��B�.B	  B	�B��B	.B	bB	�B	�B	+B	�B	 �B	�B	!�B	$�B	%FB	%�B	'RB	*eB	.IB	8�B	5?B	9�B	AUB	AUB	A�B	C�B	C�B	C�B	CaB	G�B	GzB	H�B	H�B	HB	G�B	G�B	JXB	IRB	NB	P�B	R�B	S�B	W
B	XEB	Y�B	]dB	f2B	h�B	iDB	g�B	hsB	g�B	h�B	e�B	jB	zB	xlB	|PB	x�B	xlB	y�B	z�B	{B	z�B	|B	sB	n�B	�=B	� B	��B	��B	��B	��B	�YB	�B	��B	�rB	�B	�.B	��B	�JB	��B	�B	��B	��B	�uB	�MB	�SB	�1B	��B	�YB	��B	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��=SSi<�m�=�R<���<A�g<j�<�w&<b��<�V�<FN�<#�
<#�
<G�;<b��<#�
<#�
<#�
<#�
<#�
<#�
<Qj<#�
<#�
<r��<K�<��<̾=�+<կ�<���<�o)<�#}<#�
<-�<#�
<���<�ss<Qj<|��<d��<#�
<#�
<#�
<#�
<#�
<d��<� �<#�
<#�
<�]�<#�
<#�
<#�
<#�
<#�
<G{E<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018112723282420181127232824IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010320190106200103QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010320190106200103QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551220190521075512IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                