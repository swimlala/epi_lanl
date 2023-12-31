CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-04-26T19:36:06Z creation; 2023-05-01T21:35:41Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20210426193606  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               y   yAA  AOAO7314_008642_121                 7314_008642_121                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�ouQ֌i@�ouQ֌i11  @�ou���'@�ou���'@1�V�Ϫ�@1�V�Ϫ��b��t��b��t�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@=p�@z�H@��R@��R@�  A   AG�A ��A,(�A?\)A`��A�Q�A�Q�A�Q�A�  A��AϮA�\)A�B (�B  B  B�B   B(  B0  B7�
B@(�BH  BP(�BX(�B`  Bh(�Bp  Bw�
B�  B��B��
B�  B�  B��B��B�  B�  B�  B��B��B�  B��
B��B�  B��B��
B��
B��
B�  B��B�  B�{B�{B�  B��
B��
B��B�{B�{B�{C {C{C
=C  C  C	��C  C
=C  C  C  C  C  C  C  C  C��C"  C$
=C&  C(
=C*
=C,
=C.  C0  C2
=C4  C5�C8  C:  C<  C>
=C@  CB  CC��CF  CH  CI��CK��CN  CP  CQ��CS��CU��CW��CZ  C\
=C^
=C`
=Cb  Cc��Cf  Ch  Ci��Ck��Cn
=Cp
=Cr
=Ct
=Cv
=Cw��Cy��C{��C}��C�  C�  C���C���C�  C�C�  C�  C�  C�  C�  C�C�  C�  C�  C���C�  C�C�  C�C�  C���C�  C�  C�  C�C�  C�  C�C�C�C�C�C�  C�  C�  C���C�C�C�  C�C�C���C���C�C�C�  C���C���C���C���C���C�C�C�
=C�  C���C���C�  C�C�C�  C�  C�C�  C���C���C���C���C�  C�  C���C���C���C�  C�C�  C�  C���C���C�C�C�  C�  C�  C�  C���C���C�  C�C�C�
=C�  C�  C�  C�C�C�  C���C���C�
=C�
=C�C�C�C�C�  C�  C�  C�C�
=C�
=C�C�C�C�  C���C�  C�C�
=C�  C���C�  C�C�C�C�C�
=D D � D ��D}qD  D� D�D��D�D�D  Dz�D�qD}qD  D��DD�D	�D	�D
  D
z�D
��Dz�D��D� D  D��DD��D  D� D�qD� DD�D�D��D�D��D  D� D  D}qD�qD}qD�D��D  D}qD�qD� D�qD� D�qD}qD  D}qD�qD}qD  D� D  D� D   D }qD!  D!��D"  D"� D#D#��D$  D$��D%  D%z�D%�qD&� D&�qD'}qD'�qD(��D)�D)� D*  D*��D+�D+� D+�qD,}qD-  D-��D.  D.}qD/  D/� D0  D0� D1  D1��D2�D2��D3  D3� D4  D4� D5�D5� D6  D6� D7  D7�D8  D8z�D8�qD9� D:  D:� D;�D;��D<  D<� D=  D=}qD=�qD>}qD>�qD?� D@�D@� DA  DA� DB  DB� DC  DC��DD�DD��DE�DE��DF�DF��DG�DG� DG�qDH� DI  DI� DJ  DJ��DK�DK� DK�qDL}qDM  DM��DN  DN}qDO  DO� DP�DP��DQ  DQ��DR�DR� DS�DS��DS�qDT}qDT�qDU� DV�DV� DW  DW� DX�DX�DY�DY}qDY�qDZ� DZ�qD[}qD[�qD\� D]�D]� D]��D^}qD_  D_� D_�qD`� Da  Da��Db  Db� Db�qDc}qDc�qDd� De�De��Df�Df� Dg  Dg� Dg�qDh}qDh�qDi� Dj  Dj��Dj�qDk� Dl  Dl}qDm  Dm� Dm�qDn}qDo  Do}qDp  Dp��DqDq��Dr  Dr� Dr�qDs� Dt�Dt��Dt��Du}qDv  Dv� Dw  Dw}qDw�qDx}qDx�qDy}qDy�qDz� D{  D{��D|�D|��D|�qD}� D~�D~� D~�qD� D�HD�@ D�~�D��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D�  D�AHD���D��HD�  D�>�D�� D�� D���D�>�D�~�D��qD�  D�B�D���D�� D��qD�=qD�~�D���D�  D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D��HD�� D�  D�@ D��HD�D�HD�@ D��HD�D�HD�AHD��HD��HD��D�@ D�~�D���D�  D�@ D��HD���D��qD�>�D�� D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D��HD��D�>�D�~�D��HD�HD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�}qD���D��D�B�D�� D��qD���D�@ D��HD�D�  D�AHD��HD�� D�HD�AHD�� D�� D���D�=qD�� D�D�  D�@ D�� D���D�HD�AHD��HD��HD�HD�AHD�� D�� D�HD�AHD���D�� D��qD�=qD�}qD��qD���D�@ D��HD�� D���D�@ D��HD�� D���D�AHD��HD���D���D�=qD�}qD�� D�  D�@ D�� D�� D�HD�=qD�~�D�D��D�@ D�}qD���D�  D�@ D��HD��HD��D�@ D�� D��HD���D�>�D�� D�� D�HD�@ D��HD���D�  D�@ D�~�D�� D�HD�>�D�� D��HD�  D�@ D��HD���D�  D�AHD�~�D�� D�HD�>�D�� D��HD�  D�=qD�~�D���D�  D�@ D��HD�D���D�@ D�� D�� D�HD�>�D�� D�� D�  D�AHD�~�D�� D�  D�=qD�~�D���D���D�@ D��HD��HD���D�>�D�~�D���D�HD�B�D�� D��qD���D�@ D�� D���D�HD�B�D�~�D���D�  D�>�D�� D�D�HD�AHD�� D���D���D�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ DÀ D�� D�  D�@ D�~�D�� D�  D�=qD�}qD�� D�  D�@ DƁHD��HD�  D�>�D�~�D�� D���D�>�DȀ D�� D�  D�>�D�~�D�� D�HD�@ Dʀ D�� D�  D�AHDˁHD�� D���D�@ D̀ D�� D�  D�>�D́HD�� D���D�@ D΀ D�� D�  D�@ DρHDϾ�D���D�AHDЁHD�� D�  D�>�Dр D��HD���D�=qDҀ D��HD�  D�AHDӀ D�� D���D�>�D�~�D�� D�HD�AHDՂ�D��HD�  D�>�D�~�D־�D�  D�AHDׁHD��HD�  D�AHD؀ DؽqD���D�@ DفHD�� D��qD�>�DځHD��HD�  D�>�Dۀ D�� D���D�>�D܀ D�� D���D�@ D݀ DݽqD��qD�=qD�}qD�� D�HD�@ D�}qD߾�D���D�@ D�~�DྸD���D�>�D� D��HD�HD�AHD�HD��HD�  D�@ D�HD��HD�HD�@ D� D�� D���D�=qD� D��HD�HD�B�D�HD��HD�  D�@ D�~�D羸D���D�@ D� D�� D�HD�@ D�~�D�� D�HD�B�D�HD�� D��D�AHD� D�� D�  D�@ D� D�� D���D�AHD�HD�� D�  D�=qD�}qD��HD�HD�>�D�~�DﾸD���D�AHD��HD�� D���D�>�D� D��HD�  D�>�D�~�D�qD��qD�@ D�HD��HD�  D�@ D� D�� D�  D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�@ D��HD���D��qD�=qD�~�D��HD�HD�B�D���D��HD��3D�5�?��?aG�?�\)?�33?Ǯ?�ff@�@
=@#�
@5@E�@W
=@fff@s33@�  @���@��@�Q�@�  @���@���@�
=@��R@�ff@�{@�
=@�  @�@��@���AG�AA	��A\)A�
A�A�A ��A%A*=qA.{A333A7�A<��AA�AFffAK�AP��AUAZ�HA_\)Ac�
AhQ�Al��AqG�AvffAz=qA\)A��A�(�A�ffA���A�33A�A�Q�A��\A�p�A��A��A�z�A�
=A���A��
A��RA���A��A�{A���A��A�A���A�33A�A�Q�A�33A�A�Q�A��HA�p�A�  A�=qA���A�\)A��A�(�A޸RA�G�A�(�A�RA�G�A�A�{A�Q�A��HA�p�A��A�=qA�z�A�
=B ��B�B
=B(�Bp�B�RB  B	G�B
�\B�
B��B=qB\)B��BB�HB(�BG�BffB�B��B=qB�B��B{B\)B ��B!�B#
=B$Q�B%p�B&�RB(  B)G�B*�RB,  B-G�B.�\B/�
B0��B2=qB3�B4��B5B7
=B8z�B9B;
=B<z�B=B?
=B@(�BAG�BB�\BC�
BD��BF=qBG\)BH��BIBK33BLz�BMBO
=BP(�BQG�BR�\BS�BT��BV=qBW�BX��BY�B[
=B\Q�B]G�B^ffB_�B`��Bb{Bc\)Bd��BeBf�HBg�
Bh��Bj{Bk33BlQ�BmG�Bn�\Bo�Bp��Bq�Bs
=BtQ�BuG�Bv=qBw\)Bx��By�B{33B|z�B}B~�HB�  B���B�33B��
B�z�B��B�B�ffB�
=B��B�Q�B��HB�p�B�(�B���B��B�(�B��RB�\)B��B��\B�G�B�  B���B�33B��
B�ffB�
=B�B�ffB�
=B���B�=qB���B�p�B�{B���B�G�B��B��\B�G�B��
B�z�B�
=B���B�=qB���B��B�Q�B���B��B�(�B���B�p�B�(�B��HB��B�(�B��HB��B�=qB��HB���B�Q�B�
=B�B�ffB��B�B�ffB��B��
B��\B�\)B�  B��RB�G�B�  B��RB�\)B�(�B��HB��B�ffB���B��B�Q�B��B��
B��\B�\)B��B��\B�\)B�{B���B��B�Q�B�
=B�B�ffB�
=B��
Bģ�B�\)B�{BƸRB�\)B�(�B��HBə�B�Q�B���BˮB�Q�B�
=B��
BΣ�B�\)B�  BиRB�\)B�{B��HBә�B�Q�B��B�B�ffB�
=B��
B�z�B�G�B�  BڸRB�\)B�{BܸRB�\)B�  B���B߅B�=qB���BᙚB�Q�B�
=B��
B�\B�\)B�{B���B�p�B�(�B���B�B�ffB�
=B��
B�\B�\)B�  B�RB�\)B�{B���B�B�Q�B���B�B�ffB�33B��B���B�\)B�  B��RB���B�Q�B�
=B�B�ffB�33B��B���B�p�C {C ffC C�Cp�C�RC��C33Cz�C�RC��C33CffC�C�RC�HC{C=qCffC�\C�C�
C
=C=qCffC�C�C�HC{C=qCffC�\C�C�HC{C=qC\)C�C�RC�C{C=qCffC�C�RC�C	{C	33C	\)C	�\C	C	�C
{C
=qC
ffC
�\C
C
��C{C33CffC��CC�C
=C=qCp�C�\C�RC�HC{CG�CffC�\C�C�C{CG�CffC�\C�RC�C�C=qCffC�\CC��C�C=qCp�C��C��C��C�C=qCp�C��C��C�C�CQ�Cz�C��C��C��C(�CQ�Cp�C��C�
C  C(�CQ�C�C�RC�HC
=C33CffC��C��C�C{CG�Cz�C�C�
C  C=qCp�C��C��C��C(�CffC��CC�C(�CffC�\CC�C(�CffC��C�RC�C(�C\)C�\CC�C(�C\)C�\C�RC�C(�C\)C�\C�RC�C(�C\)C�\C�RC��C33CffC�\C��C {C =qC p�C ��C �C!(�C!\)C!��C!�
C"{C"G�C"�C"C#  C#33C#p�C#�RC#�C$�C$\)C$��C$�
C%
=C%Q�C%�\C%C&  C&=qC&�C&�RC&�C'=qC'p�C'��C'�HC((�C(ffC(��C(�
C)�C)ffC)��C)��C*{C*\)C*��C*��C+
=C+Q�C+��C+C,
=C,G�C,�C,�RC,��C-=qC-p�C-�C-�C.(�C.\)C.��C.�HC/�C/Q�C/��C/�
C0
=C0=qC0�\C0C0��C1(�C1z�C1�C1�HC2�C2ffC2��C2�
C3
=C3Q�C3�\C3C3��C4=qC4z�C4�C4�HC5(�C5ffC5�\C5��C6{C6G�C6z�C6C7  C733C7ffC7��C7�C8{C8Q�C8�\C8C8��C9=qC9z�C9��C9�
C:�C:\)C:�\C:�RC;  C;33C;ffC;��C;�HC<�C<G�C<��C<�
C=  C=33C=z�C=�C=�HC>�C>ffC>�\C>C?
=C?G�C?p�C?�C?�C@�C@G�C@�\C@C@��CA(�CAffCA��CACB  CB=qCB\)CB��CB�
CC  CC33CCz�CC�CC�
CD{CDG�CDz�CD�CD��CE�CEQ�CE��CE�RCE��CF33CFffCF�CF��CF��CG(�CG\)CG��CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                  ?��@�\@=p�@z�H@��R@��R@�  A   AG�A ��A,(�A?\)A`��A�Q�A�Q�A�Q�A�  A��AϮA�\)A�B (�B  B  B�B   B(  B0  B7�
B@(�BH  BP(�BX(�B`  Bh(�Bp  Bw�
B�  B��B��
B�  B�  B��B��B�  B�  B�  B��B��B�  B��
B��B�  B��B��
B��
B��
B�  B��B�  B�{B�{B�  B��
B��
B��B�{B�{B�{C {C{C
=C  C  C	��C  C
=C  C  C  C  C  C  C  C  C��C"  C$
=C&  C(
=C*
=C,
=C.  C0  C2
=C4  C5�C8  C:  C<  C>
=C@  CB  CC��CF  CH  CI��CK��CN  CP  CQ��CS��CU��CW��CZ  C\
=C^
=C`
=Cb  Cc��Cf  Ch  Ci��Ck��Cn
=Cp
=Cr
=Ct
=Cv
=Cw��Cy��C{��C}��C�  C�  C���C���C�  C�C�  C�  C�  C�  C�  C�C�  C�  C�  C���C�  C�C�  C�C�  C���C�  C�  C�  C�C�  C�  C�C�C�C�C�C�  C�  C�  C���C�C�C�  C�C�C���C���C�C�C�  C���C���C���C���C���C�C�C�
=C�  C���C���C�  C�C�C�  C�  C�C�  C���C���C���C���C�  C�  C���C���C���C�  C�C�  C�  C���C���C�C�C�  C�  C�  C�  C���C���C�  C�C�C�
=C�  C�  C�  C�C�C�  C���C���C�
=C�
=C�C�C�C�C�  C�  C�  C�C�
=C�
=C�C�C�C�  C���C�  C�C�
=C�  C���C�  C�C�C�C�C�
=D D � D ��D}qD  D� D�D��D�D�D  Dz�D�qD}qD  D��DD�D	�D	�D
  D
z�D
��Dz�D��D� D  D��DD��D  D� D�qD� DD�D�D��D�D��D  D� D  D}qD�qD}qD�D��D  D}qD�qD� D�qD� D�qD}qD  D}qD�qD}qD  D� D  D� D   D }qD!  D!��D"  D"� D#D#��D$  D$��D%  D%z�D%�qD&� D&�qD'}qD'�qD(��D)�D)� D*  D*��D+�D+� D+�qD,}qD-  D-��D.  D.}qD/  D/� D0  D0� D1  D1��D2�D2��D3  D3� D4  D4� D5�D5� D6  D6� D7  D7�D8  D8z�D8�qD9� D:  D:� D;�D;��D<  D<� D=  D=}qD=�qD>}qD>�qD?� D@�D@� DA  DA� DB  DB� DC  DC��DD�DD��DE�DE��DF�DF��DG�DG� DG�qDH� DI  DI� DJ  DJ��DK�DK� DK�qDL}qDM  DM��DN  DN}qDO  DO� DP�DP��DQ  DQ��DR�DR� DS�DS��DS�qDT}qDT�qDU� DV�DV� DW  DW� DX�DX�DY�DY}qDY�qDZ� DZ�qD[}qD[�qD\� D]�D]� D]��D^}qD_  D_� D_�qD`� Da  Da��Db  Db� Db�qDc}qDc�qDd� De�De��Df�Df� Dg  Dg� Dg�qDh}qDh�qDi� Dj  Dj��Dj�qDk� Dl  Dl}qDm  Dm� Dm�qDn}qDo  Do}qDp  Dp��DqDq��Dr  Dr� Dr�qDs� Dt�Dt��Dt��Du}qDv  Dv� Dw  Dw}qDw�qDx}qDx�qDy}qDy�qDz� D{  D{��D|�D|��D|�qD}� D~�D~� D~�qD� D�HD�@ D�~�D��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D�  D�AHD���D��HD�  D�>�D�� D�� D���D�>�D�~�D��qD�  D�B�D���D�� D��qD�=qD�~�D���D�  D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D��HD�� D�  D�@ D��HD�D�HD�@ D��HD�D�HD�AHD��HD��HD��D�@ D�~�D���D�  D�@ D��HD���D��qD�>�D�� D�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D��HD��D�>�D�~�D��HD�HD�>�D�~�D���D���D�>�D�~�D���D���D�>�D�}qD���D��D�B�D�� D��qD���D�@ D��HD�D�  D�AHD��HD�� D�HD�AHD�� D�� D���D�=qD�� D�D�  D�@ D�� D���D�HD�AHD��HD��HD�HD�AHD�� D�� D�HD�AHD���D�� D��qD�=qD�}qD��qD���D�@ D��HD�� D���D�@ D��HD�� D���D�AHD��HD���D���D�=qD�}qD�� D�  D�@ D�� D�� D�HD�=qD�~�D�D��D�@ D�}qD���D�  D�@ D��HD��HD��D�@ D�� D��HD���D�>�D�� D�� D�HD�@ D��HD���D�  D�@ D�~�D�� D�HD�>�D�� D��HD�  D�@ D��HD���D�  D�AHD�~�D�� D�HD�>�D�� D��HD�  D�=qD�~�D���D�  D�@ D��HD�D���D�@ D�� D�� D�HD�>�D�� D�� D�  D�AHD�~�D�� D�  D�=qD�~�D���D���D�@ D��HD��HD���D�>�D�~�D���D�HD�B�D�� D��qD���D�@ D�� D���D�HD�B�D�~�D���D�  D�>�D�� D�D�HD�AHD�� D���D���D�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ DÀ D�� D�  D�@ D�~�D�� D�  D�=qD�}qD�� D�  D�@ DƁHD��HD�  D�>�D�~�D�� D���D�>�DȀ D�� D�  D�>�D�~�D�� D�HD�@ Dʀ D�� D�  D�AHDˁHD�� D���D�@ D̀ D�� D�  D�>�D́HD�� D���D�@ D΀ D�� D�  D�@ DρHDϾ�D���D�AHDЁHD�� D�  D�>�Dр D��HD���D�=qDҀ D��HD�  D�AHDӀ D�� D���D�>�D�~�D�� D�HD�AHDՂ�D��HD�  D�>�D�~�D־�D�  D�AHDׁHD��HD�  D�AHD؀ DؽqD���D�@ DفHD�� D��qD�>�DځHD��HD�  D�>�Dۀ D�� D���D�>�D܀ D�� D���D�@ D݀ DݽqD��qD�=qD�}qD�� D�HD�@ D�}qD߾�D���D�@ D�~�DྸD���D�>�D� D��HD�HD�AHD�HD��HD�  D�@ D�HD��HD�HD�@ D� D�� D���D�=qD� D��HD�HD�B�D�HD��HD�  D�@ D�~�D羸D���D�@ D� D�� D�HD�@ D�~�D�� D�HD�B�D�HD�� D��D�AHD� D�� D�  D�@ D� D�� D���D�AHD�HD�� D�  D�=qD�}qD��HD�HD�>�D�~�DﾸD���D�AHD��HD�� D���D�>�D� D��HD�  D�>�D�~�D�qD��qD�@ D�HD��HD�  D�@ D� D�� D�  D�>�D�~�D�� D�  D�>�D�~�D�� D�  D�@ D��HD���D��qD�=qD�~�D��HD�HD�B�D���D��HD��3G�O�?��?aG�?�\)?�33?Ǯ?�ff@�@
=@#�
@5@E�@W
=@fff@s33@�  @���@��@�Q�@�  @���@���@�
=@��R@�ff@�{@�
=@�  @�@��@���AG�AA	��A\)A�
A�A�A ��A%A*=qA.{A333A7�A<��AA�AFffAK�AP��AUAZ�HA_\)Ac�
AhQ�Al��AqG�AvffAz=qA\)A��A�(�A�ffA���A�33A�A�Q�A��\A�p�A��A��A�z�A�
=A���A��
A��RA���A��A�{A���A��A�A���A�33A�A�Q�A�33A�A�Q�A��HA�p�A�  A�=qA���A�\)A��A�(�A޸RA�G�A�(�A�RA�G�A�A�{A�Q�A��HA�p�A��A�=qA�z�A�
=B ��B�B
=B(�Bp�B�RB  B	G�B
�\B�
B��B=qB\)B��BB�HB(�BG�BffB�B��B=qB�B��B{B\)B ��B!�B#
=B$Q�B%p�B&�RB(  B)G�B*�RB,  B-G�B.�\B/�
B0��B2=qB3�B4��B5B7
=B8z�B9B;
=B<z�B=B?
=B@(�BAG�BB�\BC�
BD��BF=qBG\)BH��BIBK33BLz�BMBO
=BP(�BQG�BR�\BS�BT��BV=qBW�BX��BY�B[
=B\Q�B]G�B^ffB_�B`��Bb{Bc\)Bd��BeBf�HBg�
Bh��Bj{Bk33BlQ�BmG�Bn�\Bo�Bp��Bq�Bs
=BtQ�BuG�Bv=qBw\)Bx��By�B{33B|z�B}B~�HB�  B���B�33B��
B�z�B��B�B�ffB�
=B��B�Q�B��HB�p�B�(�B���B��B�(�B��RB�\)B��B��\B�G�B�  B���B�33B��
B�ffB�
=B�B�ffB�
=B���B�=qB���B�p�B�{B���B�G�B��B��\B�G�B��
B�z�B�
=B���B�=qB���B��B�Q�B���B��B�(�B���B�p�B�(�B��HB��B�(�B��HB��B�=qB��HB���B�Q�B�
=B�B�ffB��B�B�ffB��B��
B��\B�\)B�  B��RB�G�B�  B��RB�\)B�(�B��HB��B�ffB���B��B�Q�B��B��
B��\B�\)B��B��\B�\)B�{B���B��B�Q�B�
=B�B�ffB�
=B��
Bģ�B�\)B�{BƸRB�\)B�(�B��HBə�B�Q�B���BˮB�Q�B�
=B��
BΣ�B�\)B�  BиRB�\)B�{B��HBә�B�Q�B��B�B�ffB�
=B��
B�z�B�G�B�  BڸRB�\)B�{BܸRB�\)B�  B���B߅B�=qB���BᙚB�Q�B�
=B��
B�\B�\)B�{B���B�p�B�(�B���B�B�ffB�
=B��
B�\B�\)B�  B�RB�\)B�{B���B�B�Q�B���B�B�ffB�33B��B���B�\)B�  B��RB���B�Q�B�
=B�B�ffB�33B��B���B�p�C {C ffC C�Cp�C�RC��C33Cz�C�RC��C33CffC�C�RC�HC{C=qCffC�\C�C�
C
=C=qCffC�C�C�HC{C=qCffC�\C�C�HC{C=qC\)C�C�RC�C{C=qCffC�C�RC�C	{C	33C	\)C	�\C	C	�C
{C
=qC
ffC
�\C
C
��C{C33CffC��CC�C
=C=qCp�C�\C�RC�HC{CG�CffC�\C�C�C{CG�CffC�\C�RC�C�C=qCffC�\CC��C�C=qCp�C��C��C��C�C=qCp�C��C��C�C�CQ�Cz�C��C��C��C(�CQ�Cp�C��C�
C  C(�CQ�C�C�RC�HC
=C33CffC��C��C�C{CG�Cz�C�C�
C  C=qCp�C��C��C��C(�CffC��CC�C(�CffC�\CC�C(�CffC��C�RC�C(�C\)C�\CC�C(�C\)C�\C�RC�C(�C\)C�\C�RC�C(�C\)C�\C�RC��C33CffC�\C��C {C =qC p�C ��C �C!(�C!\)C!��C!�
C"{C"G�C"�C"C#  C#33C#p�C#�RC#�C$�C$\)C$��C$�
C%
=C%Q�C%�\C%C&  C&=qC&�C&�RC&�C'=qC'p�C'��C'�HC((�C(ffC(��C(�
C)�C)ffC)��C)��C*{C*\)C*��C*��C+
=C+Q�C+��C+C,
=C,G�C,�C,�RC,��C-=qC-p�C-�C-�C.(�C.\)C.��C.�HC/�C/Q�C/��C/�
C0
=C0=qC0�\C0C0��C1(�C1z�C1�C1�HC2�C2ffC2��C2�
C3
=C3Q�C3�\C3C3��C4=qC4z�C4�C4�HC5(�C5ffC5�\C5��C6{C6G�C6z�C6C7  C733C7ffC7��C7�C8{C8Q�C8�\C8C8��C9=qC9z�C9��C9�
C:�C:\)C:�\C:�RC;  C;33C;ffC;��C;�HC<�C<G�C<��C<�
C=  C=33C=z�C=�C=�HC>�C>ffC>�\C>C?
=C?G�C?p�C?�C?�C@�C@G�C@�\C@C@��CA(�CAffCA��CACB  CB=qCB\)CB��CB�
CC  CC33CCz�CC�CC�
CD{CDG�CDz�CD�CD��CE�CEQ�CE��CE�RCE��CF33CFffCF�CF��CF��CG(�CG\)CG��CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aě�AđhAčPAď\AċDAĕ�Aĕ�Aĕ�AđhAĉ7AčPAčPAċDAčPAđhAċDA�~�A�x�A�x�A�z�A�r�A�v�A�v�A�t�A�x�A�t�A�x�A�v�A�x�A�v�A�|�A�r�A�jA�hsA�^5A�-A�C�A��DA��/A��mA��mA��RA���A�jA��A��\A���A�l�A��A�I�A�1A���A��;A��!A�^5A��A��9A�ffA�Q�A��A���A�^5A��hA�1'A�S�A��A�^5A���A��A�A�l�A�/A���A��A���A��A��yA�^5A��TA��A�-A�z�A�A���A��DA�A��PA� �A�jA���A�ĜA���A�K�A���A���A�ZA�dZA��
A��-A���A��DA��DA��RA�=qA�`BA��A��+A�ffA~�/Az�Ax��At�!Ao��AlJAhjAd1'A`��A^�A]�#A[hsAXffAT��AO��AM�7AK�TAJr�AH�AE��AC�A@�A?�A=S�A:�+A8�+A6��A4�A0�HA/XA,��A+�#A++A+��A*�jA)�;A(�uA(�\A*=qA+7LA*��A*M�A+7LA-;dA-"�A-�mA.5?A.E�A.E�A.ZA+�mA)��A'�7A&�`A&A�A%ƨA%\)A$�yA$��A#��A"��A"bA!33A �\AG�A�A5?A��AdZA��A�A33A9XA�-Ap�A;dA�AXA�yA�-A��A�RAr�A$�A\)A��A�A��A7LA�A�AA/AI�A|�At�A�^Ax�A&�A
ȴA
�A
��A
n�A	�TA	�Az�A�A�
A�FA��A33AQ�A�mAAl�A�A�A��A5?A��A33A�`A�HA�DA�;A��A&�A �A z�A (�@�K�@�-@���@��
@��@�+@�=q@��@�7L@�bN@���@�F@�@�t�@�K�@�V@��@�~�@�@�$�@���@��@��@�!@�^5@�O�@�M�@�Q�@�K�@�v�@�p�@䛦@�A�@�ƨ@��@�$�@�`B@�j@�x�@�5?@���@��@�V@�Z@���@�33@�-@�hs@ܬ@�"�@�V@��@�1'@׾w@�|�@�j@���@�%@��@�x�@��/@�dZ@���@�/@�/@Լj@ӝ�@�J@�7L@Ѓ@��m@�+@���@ΰ!@���@͡�@̬@˾w@�o@���@�t�@� �@�S�@ɲ-@�r�@ǥ�@�n�@ŉ7@�`B@�/@��/@�bN@þw@�l�@���@§�@�V@�@���@��h@�x�@�p�@��@��
@�@�5?@�@�`B@��@��@�1'@��m@��@�|�@�o@���@��@�?}@�V@���@���@��D@�j@�1@�dZ@�
=@��H@�ȴ@�M�@���@�?}@��`@��@�r�@�Z@� �@��@���@�"�@��@��@��@��^@�hs@�%@�Z@��@�ƨ@�S�@��@��@���@�ȴ@�~�@��@�@�`B@�O�@�O�@�G�@�?}@���@�r�@�  @��w@�C�@���@��H@��R@��+@�V@��@��@��@��@�I�@�9X@��@��
@�ƨ@��F@��@���@�33@���@��!@��+@�^5@��T@�p�@�`B@�X@�&�@�Q�@�K�@�"�@��@�@���@��+@��@�p�@��@���@��/@�Ĝ@��D@�A�@��@��P@�t�@�K�@��@��!@�^5@�M�@�E�@�J@���@��7@�7L@��`@�Ĝ@�bN@�1@��
@���@�\)@�"�@���@�n�@�=q@��@�@�hs@�&�@��`@��9@��D@�r�@� �@��m@�ƨ@�\)@�o@��H@���@�^5@�V@��@�@�hs@�7L@��@��9@�bN@��@��
@��F@���@�l�@�33@��H@�E�@��@��#@�@���@��h@�V@�j@� �@���@�+@��y@��!@�ff@�M�@�5?@�J@��@���@�Ĝ@�r�@�I�@�9X@��@���@��;@��P@�o@��\@�v�@�{@��^@���@��@�/@���@���@�Ĝ@��9@���@�z�@�I�@�1'@��@���@��@�S�@�C�@�33@�@��@��R@��+@�$�@�J@��T@��@���@���@��9@���@�bN@�1'@��m@��@���@���@��P@�t�@�"�@�ȴ@��!@���@��\@�^5@��@���@���@�hs@�O�@�V@��/@��u@�bN@�I�@�1@K�@~V@}��@}/@|�@|j@|(�@{�
@{t�@z��@zn�@z^5@y�@y&�@x��@x�@w�w@w+@w�@w
=@v�y@v��@vff@v{@u��@u�@t�D@tI�@t(�@s�
@s�F@s�F@s��@sdZ@r�H@r�\@r~�@r=q@q�#@q��@q%@p�u@p�@pr�@pbN@p �@p  @o��@o�@o�P@ol�@o\)@o;d@n��@n$�@n{@n@m�T@m��@m�@mO�@l�/@l(�@k�F@kS�@ko@j�H@j��@j��@jn�@i��@i7L@h��@hr�@hb@g�w@g��@gl�@g;d@g
=@f5?@e�-@d�@d�j@d(�@c�@cdZ@c@b�@a��@a�7@a7L@`�u@` �@_K�@^�y@^�+@^$�@]@]��@]�@]O�@]V@\�/@\�j@\��@\j@\(�@\1@[ƨ@[�F@[dZ@[C�@[o@Z�\@Z=q@Z�@Y��@Y��@Y�7@Y7L@X�`@X��@X�9@X�@XA�@Xb@W�@W�@W
=@V�R@V5?@U@U�@T��@T��@T9X@T1@S�F@St�@SdZ@SS�@S"�@R��@R��@RM�@R-@Q�@Q7L@P�u@P �@O�@Ol�@O+@N�R@N$�@M�h@L��@L(�@Kƨ@K��@KdZ@K33@Ko@J�@J�H@J�H@J��@J^5@J=q@JJ@I��@HĜ@Hb@G��@GK�@FV@E��@E`B@D��@D�@DI�@C�m@CC�@Co@B��@B^5@B�@A��@A��@Ahs@@�u@@1'@@ �@?�@?�w@?�P@?;d@>��@>�R@>V@>$�@>@=@=?}@<��@<9X@;�@;S�@;t�@;��@:�H@:�!@:��@:=q@9�@9��@9�^@9��@9&�@8��@8�9@8bN@8b@7\)@6�R@65?@5�@5�h@5/@4�@41@3��@3dZ@3"�@2�@2��@2n�@2�@1�@1��@1x�@1X@1&�@0��@0�u@0  @/�@/l�@/;d@.�R@.ff@.$�@-@-/@,�j@,z�@,�@+ƨ@+S�@+C�@+33@+33@+o@*��@*n�@*^5@*J@)��@)x�@)G�@)%@(�u@(  @'�@'�w@'\)@'�@&�@&�+@&$�@&@%�@%��@%@%��@%p�@%`B@%/@$�@$��@$I�@$9X@$(�@$1@#�
@#�F@#�@#33@#"�@#o@"�!@"^5@"-@"�@!��@!��@!X@!&�@ ��@ �@ A�@ 1'@  �@  �@ b@�;@��@\)@;d@��@ff@E�@E�@{@�T@��@@��@�@O�@V@��@��@z�@I�@�@�m@�@S�@C�@o@��@n�@n�@n�@^5@-@�@��@�7@x�@hs@X@&�@�9@bN@b@  @��@�P@K�@V@5?@@@�@p�@`B@O�@�@�@��@��@I�@��@ƨ@�F@��@dZ@dZ@33@@��@�\@M�@=q@J@��@�^@��@hs@��@��@�9@�u@b@��@|�@\)@K�@K�@K�@K�@;d@;d@+@�@
=@��@�+@v�@V@{@�@��@@�-@�h@�hAĕ�Aę�Aġ�Aġ�Aġ�AđhAčPAĉ7AčPAď\AċDAď\AđhAď\Aď\Aď\Aĉ7AčPAċDAċDAĕ�AčPAĕ�Aė�Aĕ�Aė�Aė�Aĕ�AēuAēuAĕ�Aĕ�Aĕ�AēuAčPAčPAāAċDAċDAčPAčPAēuAď\AčPAċDAċDAčPAď\AčPAċDAąAć+AċDAċDAčPAčPAčPAčPAčPAď\Aď\Aď\AēuAēuAđhAēuAđhAď\AđhAčPAď\AčPAĉ7AăA�|�A�|�A�~�A�~�A�~�A�|�A�z�A�z�A�z�A�x�A�x�A�v�A�v�A�v�A�z�A�z�A�|�A�x�A�z�AāAāAăA�z�A�r�A�r�A�p�A�p�A�p�A�p�A�r�A�r�A�v�A�x�A�x�A�v�A�v�A�v�A�x�A�x�A�x�A�x�A�x�A�v�A�v�A�t�A�r�A�r�A�t�A�t�A�t�A�v�A�x�A�x�A�x�A�x�A�x�A�v�A�t�A�t�A�r�A�t�A�r�A�t�A�t�A�v�A�x�A�z�A�x�A�x�A�x�A�x�A�v�A�v�A�t�A�v�A�x�A�x�A�z�A�z�A�z�A�z�A�v�A�v�A�v�A�t�A�r�A�v�A�x�A�z�A�z�A�z�A�z�A�|�A�|�A�z�A�z�A�z�A�v�A�v�A�r�A�t�A�l�A�n�A�l�A�jA�jA�hsA�hsA�ffA�hsA�jA�jA�jA�hsA�ffA�ffA�bNA�^5A�^5A�^5A�^5A�^5A�^5A�^5A�S�A�E�A�?}A�7LA�bA��HA���AöFAÍPA�VA�5?A���A¥�A�I�A���A��A��DA�&�A���A���A��A��yA��;A��A���A�ȴA���A���A��/A��A���A�1A�JA�
=A�A��A���A��wA��^A��RA��RA��RA��^A��RA��FA��9A��A���A���A��\A��7A��A�z�A�p�A�jA�bNA�ZA�M�A�;dA�33A�33A�$�A�oA�ĜA��!A���A���A��\A��7A�v�A�`BA�-A�
=A��A�ƨA���A���A��\A�p�A�`BA�O�A�5?A��A�{A�A��yA���A��uA��A�XA�C�A�-A��A�JA�A�A�A�
=A��A�oA��A��A��A��yA��A��A��yA��HA��
A���A�ȴA�ĜA��9A��A���A���A��\A��A�dZA�C�A��A�JA���A���A��A��;A��#A��#A��A�ĜA���A��PA�z�A�l�A�dZA�hsA�dZA�bNA�`BA�\)A�XA�O�A�G�A�?}A�9XA�-A��A�bA�  A��A��
A��A���A��hA��hA��DA��A�r�A�^5A�G�A�1'A�VA���A��A�ZA�M�A�C�A�A�A�?}A�=qA�+A��A��\A�n�A�C�A�/A�&�A��A�VA�A��A��/A��wA���A��7A�jA�K�A�5?A��A���A��A��;A���A���A�I�A��A���A��!A�bNA�~�A��jA�bNA��A��jA�1'A��^A��A��-A�Q�A�1'A�VA���A��A�v�A�A�p�A�hsA�\)A�+A��mA�A���A��A�hsA�I�A�1'A�"�A�{A�
=A���A��mA��/A���A���A��
A��HA��;A���A���A���A���A��9A���A���A��hA��7A��A�n�A�Q�A�E�A�"�A��yA���A�`BA�K�A�E�A�9XA�(�A�
=A���A��mA��`A��
A�A��9A��-A��A���A���A��DA�|�A�t�A�n�A�S�A�=qA�+A�bA��A���A���A���A���A��\A�ffA�bNA�r�A��A�x�A�l�A�bNA�^5A�\)A�Q�A�M�A�A�A��A��A��;A��#A��A��
A���A��#A��;A��TA��mA��A��A��A��A��`A��
A��-A�v�A�dZA�E�A�-A� �A���A��jA���A�jA�I�A�1'A�(�A�&�A� �A�
=A���A��A���A��A�K�A��A��A�|�A��A���A���A�|�A�G�A�{A���A���A��DA�+A��A�hsA��A��;A���A�dZA�VA��TA��A��A���A��A�r�A�Q�A�9XA��A�A�  A��A��/A��FA��7A��A�t�A�;dA�33A�-A��A�{A���A��yA��;A��#A��#A��TA��/A���A�n�A�jA�n�A�l�A�hsA�\)A�VA�O�A�;dA�+A��A�%A��A��A��HA���A��RA���A�jA�G�A�VA�A���A���A��A��HA���A���A�ffA�C�A��A���A��#A�ȴA�ȴA���A��-A��A��A��A���A���A���A���A���A��hA��A�x�A�p�A�jA�`BA�K�A�;dA�5?A�-A� �A�{A�oA�bA�%A�  A���A��A��#A���A�ȴA���A��RA���A���A��7A�|�A�x�A�v�A�l�A�hsA�dZA�dZA�XA�Q�A�G�A�VA��!A��\A�z�A�n�A�dZA�S�A�7LA�JA���A�M�A�&�A�VA��`A�ƨA���A�|�A�O�A�&�A��A��HA��;A��
A���A��+A�t�A�dZA�9XA��A��A�9XA��jA�ZA��A���A��A��A��yA��A�ȴA���A�\)A�K�A�G�A�9XA��A�oA�  A��A��RA���A�ffA�7LA��`A�jA�-A��#A�A��FA��-A���A���A��\A�l�A�I�A�JA��!A��PA�r�A�7LA�A��RA�XA�
=A��wA�bNA��A��
A��hA� �A��jA��A�"�A���A���A�ffA��
A�=qA�"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                  Aě�AđhAčPAď\AċDAĕ�Aĕ�Aĕ�AđhAĉ7AčPAčPAċDAčPAđhAċDA�~�A�x�A�x�A�z�A�r�A�v�A�v�A�t�A�x�A�t�A�x�A�v�A�x�A�v�A�|�A�r�A�jA�hsA�^5A�-A�C�A��DA��/A��mA��mA��RA���A�jA��A��\A���A�l�A��A�I�A�1A���A��;A��!A�^5A��A��9A�ffA�Q�A��A���A�^5A��hA�1'A�S�A��A�^5A���A��A�A�l�A�/A���A��A���A��A��yA�^5A��TA��A�-A�z�A�A���A��DA�A��PA� �A�jA���A�ĜA���A�K�A���A���A�ZA�dZA��
A��-A���A��DA��DA��RA�=qA�`BA��A��+A�ffA~�/Az�Ax��At�!Ao��AlJAhjAd1'A`��A^�A]�#A[hsAXffAT��AO��AM�7AK�TAJr�AH�AE��AC�A@�A?�A=S�A:�+A8�+A6��A4�A0�HA/XA,��A+�#A++A+��A*�jA)�;A(�uA(�\A*=qA+7LA*��A*M�A+7LA-;dA-"�A-�mA.5?A.E�A.E�A.ZA+�mA)��A'�7A&�`A&A�A%ƨA%\)A$�yA$��A#��A"��A"bA!33A �\AG�A�A5?A��AdZA��A�A33A9XA�-Ap�A;dA�AXA�yA�-A��A�RAr�A$�A\)A��A�A��A7LA�A�AA/AI�A|�At�A�^Ax�A&�A
ȴA
�A
��A
n�A	�TA	�Az�A�A�
A�FA��A33AQ�A�mAAl�A�A�A��A5?A��A33A�`A�HA�DA�;A��A&�A �A z�A (�@�K�@�-@���@��
@��@�+@�=q@��@�7L@�bN@���@�F@�@�t�@�K�@�V@��@�~�@�@�$�@���@��@��@�!@�^5@�O�@�M�@�Q�@�K�@�v�@�p�@䛦@�A�@�ƨ@��@�$�@�`B@�j@�x�@�5?@���@��@�V@�Z@���@�33@�-@�hs@ܬ@�"�@�V@��@�1'@׾w@�|�@�j@���@�%@��@�x�@��/@�dZ@���@�/@�/@Լj@ӝ�@�J@�7L@Ѓ@��m@�+@���@ΰ!@���@͡�@̬@˾w@�o@���@�t�@� �@�S�@ɲ-@�r�@ǥ�@�n�@ŉ7@�`B@�/@��/@�bN@þw@�l�@���@§�@�V@�@���@��h@�x�@�p�@��@��
@�@�5?@�@�`B@��@��@�1'@��m@��@�|�@�o@���@��@�?}@�V@���@���@��D@�j@�1@�dZ@�
=@��H@�ȴ@�M�@���@�?}@��`@��@�r�@�Z@� �@��@���@�"�@��@��@��@��^@�hs@�%@�Z@��@�ƨ@�S�@��@��@���@�ȴ@�~�@��@�@�`B@�O�@�O�@�G�@�?}@���@�r�@�  @��w@�C�@���@��H@��R@��+@�V@��@��@��@��@�I�@�9X@��@��
@�ƨ@��F@��@���@�33@���@��!@��+@�^5@��T@�p�@�`B@�X@�&�@�Q�@�K�@�"�@��@�@���@��+@��@�p�@��@���@��/@�Ĝ@��D@�A�@��@��P@�t�@�K�@��@��!@�^5@�M�@�E�@�J@���@��7@�7L@��`@�Ĝ@�bN@�1@��
@���@�\)@�"�@���@�n�@�=q@��@�@�hs@�&�@��`@��9@��D@�r�@� �@��m@�ƨ@�\)@�o@��H@���@�^5@�V@��@�@�hs@�7L@��@��9@�bN@��@��
@��F@���@�l�@�33@��H@�E�@��@��#@�@���@��h@�V@�j@� �@���@�+@��y@��!@�ff@�M�@�5?@�J@��@���@�Ĝ@�r�@�I�@�9X@��@���@��;@��P@�o@��\@�v�@�{@��^@���@��@�/@���@���@�Ĝ@��9@���@�z�@�I�@�1'@��@���@��@�S�@�C�@�33@�@��@��R@��+@�$�@�J@��T@��@���@���@��9@���@�bN@�1'@��m@��@���@���@��P@�t�@�"�@�ȴ@��!@���@��\@�^5@��@���@���@�hs@�O�@�V@��/@��u@�bN@�I�@�1@K�@~V@}��@}/@|�@|j@|(�@{�
@{t�@z��@zn�@z^5@y�@y&�@x��@x�@w�w@w+@w�@w
=@v�y@v��@vff@v{@u��@u�@t�D@tI�@t(�@s�
@s�F@s�F@s��@sdZ@r�H@r�\@r~�@r=q@q�#@q��@q%@p�u@p�@pr�@pbN@p �@p  @o��@o�@o�P@ol�@o\)@o;d@n��@n$�@n{@n@m�T@m��@m�@mO�@l�/@l(�@k�F@kS�@ko@j�H@j��@j��@jn�@i��@i7L@h��@hr�@hb@g�w@g��@gl�@g;d@g
=@f5?@e�-@d�@d�j@d(�@c�@cdZ@c@b�@a��@a�7@a7L@`�u@` �@_K�@^�y@^�+@^$�@]@]��@]�@]O�@]V@\�/@\�j@\��@\j@\(�@\1@[ƨ@[�F@[dZ@[C�@[o@Z�\@Z=q@Z�@Y��@Y��@Y�7@Y7L@X�`@X��@X�9@X�@XA�@Xb@W�@W�@W
=@V�R@V5?@U@U�@T��@T��@T9X@T1@S�F@St�@SdZ@SS�@S"�@R��@R��@RM�@R-@Q�@Q7L@P�u@P �@O�@Ol�@O+@N�R@N$�@M�h@L��@L(�@Kƨ@K��@KdZ@K33@Ko@J�@J�H@J�H@J��@J^5@J=q@JJ@I��@HĜ@Hb@G��@GK�@FV@E��@E`B@D��@D�@DI�@C�m@CC�@Co@B��@B^5@B�@A��@A��@Ahs@@�u@@1'@@ �@?�@?�w@?�P@?;d@>��@>�R@>V@>$�@>@=@=?}@<��@<9X@;�@;S�@;t�@;��@:�H@:�!@:��@:=q@9�@9��@9�^@9��@9&�@8��@8�9@8bN@8b@7\)@6�R@65?@5�@5�h@5/@4�@41@3��@3dZ@3"�@2�@2��@2n�@2�@1�@1��@1x�@1X@1&�@0��@0�u@0  @/�@/l�@/;d@.�R@.ff@.$�@-@-/@,�j@,z�@,�@+ƨ@+S�@+C�@+33@+33@+o@*��@*n�@*^5@*J@)��@)x�@)G�@)%@(�u@(  @'�@'�w@'\)@'�@&�@&�+@&$�@&@%�@%��@%@%��@%p�@%`B@%/@$�@$��@$I�@$9X@$(�@$1@#�
@#�F@#�@#33@#"�@#o@"�!@"^5@"-@"�@!��@!��@!X@!&�@ ��@ �@ A�@ 1'@  �@  �@ b@�;@��@\)@;d@��@ff@E�@E�@{@�T@��@@��@�@O�@V@��@��@z�@I�@�@�m@�@S�@C�@o@��@n�@n�@n�@^5@-@�@��@�7@x�@hs@X@&�@�9@bN@b@  @��@�P@K�@V@5?@@@�@p�@`B@O�@�@�@��@��@I�@��@ƨ@�F@��@dZ@dZ@33@@��@�\@M�@=q@J@��@�^@��@hs@��@��@�9@�u@b@��@|�@\)@K�@K�@K�@K�@;d@;d@+@�@
=@��@�+@v�@V@{@�@��@@�-@�hG�O�Aĕ�Aę�Aġ�Aġ�Aġ�AđhAčPAĉ7AčPAď\AċDAď\AđhAď\Aď\Aď\Aĉ7AčPAċDAċDAĕ�AčPAĕ�Aė�Aĕ�Aė�Aė�Aĕ�AēuAēuAĕ�Aĕ�Aĕ�AēuAčPAčPAāAċDAċDAčPAčPAēuAď\AčPAċDAċDAčPAď\AčPAċDAąAć+AċDAċDAčPAčPAčPAčPAčPAď\Aď\Aď\AēuAēuAđhAēuAđhAď\AđhAčPAď\AčPAĉ7AăA�|�A�|�A�~�A�~�A�~�A�|�A�z�A�z�A�z�A�x�A�x�A�v�A�v�A�v�A�z�A�z�A�|�A�x�A�z�AāAāAăA�z�A�r�A�r�A�p�A�p�A�p�A�p�A�r�A�r�A�v�A�x�A�x�A�v�A�v�A�v�A�x�A�x�A�x�A�x�A�x�A�v�A�v�A�t�A�r�A�r�A�t�A�t�A�t�A�v�A�x�A�x�A�x�A�x�A�x�A�v�A�t�A�t�A�r�A�t�A�r�A�t�A�t�A�v�A�x�A�z�A�x�A�x�A�x�A�x�A�v�A�v�A�t�A�v�A�x�A�x�A�z�A�z�A�z�A�z�A�v�A�v�A�v�A�t�A�r�A�v�A�x�A�z�A�z�A�z�A�z�A�|�A�|�A�z�A�z�A�z�A�v�A�v�A�r�A�t�A�l�A�n�A�l�A�jA�jA�hsA�hsA�ffA�hsA�jA�jA�jA�hsA�ffA�ffA�bNA�^5A�^5A�^5A�^5A�^5A�^5A�^5A�S�A�E�A�?}A�7LA�bA��HA���AöFAÍPA�VA�5?A���A¥�A�I�A���A��A��DA�&�A���A���A��A��yA��;A��A���A�ȴA���A���A��/A��A���A�1A�JA�
=A�A��A���A��wA��^A��RA��RA��RA��^A��RA��FA��9A��A���A���A��\A��7A��A�z�A�p�A�jA�bNA�ZA�M�A�;dA�33A�33A�$�A�oA�ĜA��!A���A���A��\A��7A�v�A�`BA�-A�
=A��A�ƨA���A���A��\A�p�A�`BA�O�A�5?A��A�{A�A��yA���A��uA��A�XA�C�A�-A��A�JA�A�A�A�
=A��A�oA��A��A��A��yA��A��A��yA��HA��
A���A�ȴA�ĜA��9A��A���A���A��\A��A�dZA�C�A��A�JA���A���A��A��;A��#A��#A��A�ĜA���A��PA�z�A�l�A�dZA�hsA�dZA�bNA�`BA�\)A�XA�O�A�G�A�?}A�9XA�-A��A�bA�  A��A��
A��A���A��hA��hA��DA��A�r�A�^5A�G�A�1'A�VA���A��A�ZA�M�A�C�A�A�A�?}A�=qA�+A��A��\A�n�A�C�A�/A�&�A��A�VA�A��A��/A��wA���A��7A�jA�K�A�5?A��A���A��A��;A���A���A�I�A��A���A��!A�bNA�~�A��jA�bNA��A��jA�1'A��^A��A��-A�Q�A�1'A�VA���A��A�v�A�A�p�A�hsA�\)A�+A��mA�A���A��A�hsA�I�A�1'A�"�A�{A�
=A���A��mA��/A���A���A��
A��HA��;A���A���A���A���A��9A���A���A��hA��7A��A�n�A�Q�A�E�A�"�A��yA���A�`BA�K�A�E�A�9XA�(�A�
=A���A��mA��`A��
A�A��9A��-A��A���A���A��DA�|�A�t�A�n�A�S�A�=qA�+A�bA��A���A���A���A���A��\A�ffA�bNA�r�A��A�x�A�l�A�bNA�^5A�\)A�Q�A�M�A�A�A��A��A��;A��#A��A��
A���A��#A��;A��TA��mA��A��A��A��A��`A��
A��-A�v�A�dZA�E�A�-A� �A���A��jA���A�jA�I�A�1'A�(�A�&�A� �A�
=A���A��A���A��A�K�A��A��A�|�A��A���A���A�|�A�G�A�{A���A���A��DA�+A��A�hsA��A��;A���A�dZA�VA��TA��A��A���A��A�r�A�Q�A�9XA��A�A�  A��A��/A��FA��7A��A�t�A�;dA�33A�-A��A�{A���A��yA��;A��#A��#A��TA��/A���A�n�A�jA�n�A�l�A�hsA�\)A�VA�O�A�;dA�+A��A�%A��A��A��HA���A��RA���A�jA�G�A�VA�A���A���A��A��HA���A���A�ffA�C�A��A���A��#A�ȴA�ȴA���A��-A��A��A��A���A���A���A���A���A��hA��A�x�A�p�A�jA�`BA�K�A�;dA�5?A�-A� �A�{A�oA�bA�%A�  A���A��A��#A���A�ȴA���A��RA���A���A��7A�|�A�x�A�v�A�l�A�hsA�dZA�dZA�XA�Q�A�G�A�VA��!A��\A�z�A�n�A�dZA�S�A�7LA�JA���A�M�A�&�A�VA��`A�ƨA���A�|�A�O�A�&�A��A��HA��;A��
A���A��+A�t�A�dZA�9XA��A��A�9XA��jA�ZA��A���A��A��A��yA��A�ȴA���A�\)A�K�A�G�A�9XA��A�oA�  A��A��RA���A�ffA�7LA��`A�jA�-A��#A�A��FA��-A���A���A��\A�l�A�I�A�JA��!A��PA�r�A�7LA�A��RA�XA�
=A��wA�bNA��A��
A��hA� �A��jA��A�"�A���A���A�ffA��
A�=qA�"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�_B	�1B	��B	�4B	�$B	�6B	�WB	��B
�B
$�B
B�B
d�B
��B
��B
��B
�B
�KB  BBB�B�BBIB#nB,BB�BRTB��B��BȴB�9BܒB�+BPB�B��BɆB��B�vB��B��B�;B�B�BbB�BB�B�3B�B��B�sB�QB�[B�)B�B�qB��B��B�'B��B��B�Bs�B_;BD�B7B+B
��B
҉B
��B
Q�B
0UB	�>B	�2B	��B	�@B	��B	|�B	h�B	\�B	K)B	@�B	A B	7B	/�B	�B	�B��B��B��BߤB�XBΥB��B��B�LB�hB��B�6B��B��B��B�nB��B�B��B�"B��B�B��B	+B	PHB	bB	b�B	m�B	��B	�wB	�gB	��B	�B	�cB	�B	�B	�KB	�]B	��B	҉B	�B	�gB	�
B	֡B	��B	�;B	�pB	רB	�B	�}B	ҽB	��B	�B	��B	�WB	��B	یB	�sB	�sB	��B	��B	�#B	�KB	��B	��B	��B	�#B	��B	�B	�TB	��B	�vB	�B	��B	ںB	�aB	՛B	ѷB	��B	ÖB	��B	ƨB	ɺB	�KB	ɆB	ȀB	��B	� B	��B	�<B	�B	��B	��B	�XB	��B	�6B	̘B	ʌB	��B	��B	ʌB	�pB	�[B	�NB	��B	�B	��B	��B	��B	�XB	͟B	ΥB	�B	�B	� B	��B	�B	�B	��B	ɺB	��B	�}B	бB	�B	͟B	�#B	��B	�qB	��B	�RB	ȀB	�KB	��B	�vB	��B	҉B	�vB	�B	�6B	��B	ʌB	�zB	�HB	��B	��B	��B	�0B	�dB	�jB	�B	��B	�B	�B	�UB	̘B	�QB	��B	�/B	��B	��B	��B	�gB	ѷB	ҽB	�}B	�B	��B	�jB	��B	�dB	��B	چB	�)B	�/B	�vB	��B	��B	ݘB	��B	��B	��B	��B	��B	�EB	�9B	՛B	�2B	ԕB	��B	��B	ٴB	רB	�sB	�mB	�sB	ںB	��B	�B	��B	ٴB	�sB	�gB	�NB	�}B	ѷB	��B	��B	�[B	��B	��B	�9B	�mB	��B	�EB	�B	�QB	چB	ٴB	֡B	��B	�9B	֡B	��B	��B	�B	��B	�QB	�QB	�QB	یB	��B	ݘB	�5B	ޞB	��B	�;B	��B	�B	�HB	�B	�B	�B	�ZB	�,B	�`B	�`B	��B	�fB	�2B	�2B	�fB	�B	�B	��B	��B	�>B	�>B	�>B	�sB	�B	��B	�B	��B	��B	��B	�WB	�WB	�"B	�)B	�B	��B	�]B	�)B	�)B	�)B	��B	�B	� B	�;B	�B	�GB	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�%B	�%B	�%B	�ZB	�ZB	�%B	��B	��B	�2B	��B	��B	�8B	��B	�B	��B	��B	�rB	��B	�>B	�rB	�>B	�>B	��B	�B	��B	��B	��B	�]B	�]B	�]B	�cB
 �B
;B
;B
oB
�B
�B
�B
�B
�B
{B
B
{B
�B
GB
GB
�B
�B
�B
SB
�B
�B
YB
�B
�B
+B
_B
�B
�B
fB
fB
�B
�B
	lB
	7B
	lB

=B

�B

rB

�B

�B

rB
B
B
�B
xB
�B
JB
~B
B
PB
B
B
�B
�B
�B
�B
�B
(B
�B
�B
�B
�B
4B
hB
B
B
{B
{B
�B
B
MB
MB
MB
B
�B
�B
�B
�B
�B
�B
�B
�B
eB
B
�B
=B
qB
qB
�B
�B
B
B
B
IB
~B
�B
OB
OB
�B
�B
 'B
�B
�B
�B
 'B
 \B
 \B
 �B
!bB
 �B
!�B
"hB
#B
"�B
"hB
"hB
"�B
"4B
#B
$B
$B
#�B
$B
$@B
%zB
%FB
%zB
%B
%B
%FB
%zB
%zB
%zB
%�B
%�B
&LB
&LB
'RB
'�B
'�B
)*B
)�B
+B
+6B
+6B
+�B
+�B
+�B
,=B
,�B
-�B
-B
-CB
.IB
.}B
.�B
.�B
/�B
0!B
0!B
0!B
0�B
0�B
0�B
1[B
1�B
2aB
2�B
3hB
33B
3�B
3�B
3�B
3�B
4B
4nB
4�B
4nB
4�B
5B
4�B
6B
5�B
5�B
5�B
6B
6B
6B
6B
6FB
6�B
6�B
6�B
6�B
7�B
8RB
8B
8B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
:*B
:*B
:*B
:*B
:�B
:�B
;0B
;0B
;dB
;dB
;�B
;�B
;�B
;�B
;�B
<jB
<�B
=qB
=B
>BB
>BB
>B
>�B
>�B
>�B
?HB
?�B
@B
@�B
A�B
B'B
B[B
CaB
CaB
CaB
CaB
CaB
C�B
C�B
CaB
CaB
C�B
C-B
C�B
D3B
C�B
D�B
D�B
EB
D�B
D�B
E9B
E9B
E9B
EmB
F�B
GB
GEB
GzB
G�B
HB
HB
H�B
IB
IB
IRB
I�B
I�B
K)B
J�B
J�B
K^B
K�B
L0B
L0B
L0B
K�B
L�B
MB
L�B
M6B
MB
MjB
N<B
N�B
N�B
OBB
OBB
OBB
O�B
PB
P�B
P�B
P}B
PB
PHB
PHB
PHB
P}B
P}B
P}B
P}B
P�B
Q�B
R�B
R�B
R�B
R�B
RTB
R�B
R�B
S�B
S[B
S[B
S&B
S&B
S[B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
T�B
U2B
U2B
UgB
UgB
U�B
VB
V9B
VB
VmB
VmB
V9B
V�B
V�B
W�B
W�B
W�B
[#B
[#B
[#B
[#B
[�B
[�B
[�B
[�B
[�B
[�B
[#B
[#B
Z�B
Z�B
[�B
[�B
\)B
\)B
\�B
\�B
\�B
^B
^jB
^�B
_;B
_;B
_�B
`BB
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a|B
b�B
bNB
b�B
b�B
cTB
c�B
cTB
c�B
dZB
d&B
d�B
e,B
e�B
e�B
e�B
e�B
e�B
f2B
f�B
f�B
f�B
g8B
gmB
g�B
g�B
h
B
hsB
h�B
h�B
iB
iyB
i�B
i�B
jB
j�B
j�B
j�B
kB
kB
kQB
kQB
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
lWB
l"B
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
ncB
ncB
n�B
n�B
oiB
oiB
oiB
o�B
o�B
o�B
o�B
pB
p;B
p;B
qAB
qAB
qAB
qvB
q�B
q�B
q�B
q�B
rB
rB
rGB
r�B
r�B
sB
sMB
s�B
s�B
tB
tTB
t�B
t�B
t�B
uZB
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w2B
wfB
wfB
w�B
xlB
x�B
y>B
y	B
yrB
yrB
y�B
{B
z�B
{JB
{B
{�B
{�B
{�B
{�B
|B
|B
|B
|PB
|�B
}VB
}"B
|�B
}"B
}VB
}VB
}�B
}�B
}�B
~�B
~�B
~�B
.B
cB
�B
�B
��B
�oB
�oB
�oB
�B
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
�;B
�B
�B
�B
�AB
��B
�B
�GB
�GB
�GB
�{B
��B	��B	�SB	��B	�MB	��B	��B	��B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�1B	�_B	�YB	��B	��B	��B	��B	�fB	�fB	��B	�fB	�1B	�_B	�1B	�B	�JB	��B	��B	��B	��B	��B	�_B	��B	��B	�+B	�YB	�YB	��B	�fB	��B	�1B	��B	�1B	�1B	�fB	�fB	�fB	��B	�_B	��B	��B	��B	��B	�_B	��B	�1B	��B	�_B	�fB	��B	�B	��B	�7B	�_B	��B	��B	��B	��B	��B	�_B	��B	�_B	��B	�1B	�fB	��B	�1B	��B	�SB	��B	��B	��B	��B	�%B	�YB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�+B	��B	��B	�%B	��B	��B	��B	��B	�YB	��B	��B	�+B	��B	�fB	��B	��B	��B	��B	�+B	��B	��B	�YB	�%B	��B	�+B	�_B	��B	�1B	�1B	�fB	��B	��B	�+B	��B	��B	��B	��B	��B	��B	�+B	��B	�1B	��B	��B	�1B	�+B	��B	�%B	��B	�_B	��B	�_B	��B	��B	�1B	��B	��B	�_B	��B	��B	�YB	��B	�YB	��B	��B	�1B	�1B	�_B	��B	��B	��B	��B	��B	��B	��B	�fB	�fB	��B	��B	��B	�%B	��B	�_B	��B	�1B	��B	��B	�+B	�+B	�YB	�YB	��B	�%B	�1B	�B	��B	��B	��B	��B	��B	�=B	�DB	�DB	�xB	�B	��B	��B	��B	�~B	�=B	�.B	��B	��B	�:B	�B	�YB	��B	�nB	��B	�=B	��B	�B	�B	҉B	�QB	�B	��B	�B	�+B	��B	�B	�PB	��B	��B	��B	��B	��B	��B	��B
�B

�B
�B
B
�B
�B
"4B
%�B
'RB
)�B
0�B
7�B
<�B
<B
?�B
D3B
[�B
\�B
bB
d&B
d�B
e�B
jB
p;B
~�B
��B
��B
�SB
�B
�IB
�nB
�B
��B
�UB
�9B
��B
�3B
�B
�LB
��B
��B
�)B
��B
�B
��B
�B
�TB
�B
�
B
�B
�/B
�`BB;B
��B
��B�B�B_BxB\B�B�BbB�B�BB�B@BB�B_B�B�B�B(B�B(BhB B�B BB�B�B�B�B�B=B \B �B �B!�B"�B$B%�B&B&�B(XB*�B-�B0UB2-B9XBA�BGzBFtBF�BH�BJ�BN<BR�BW�B[�Bg�Bt�B�~B��B�YB��B��B��B�SB��B��B��B��B͟B� B�2B�9BרB�2B�,B��B�
B�9B�B�5B�B�ZB�B��B�B�B�B�8B�(B��BDB	lB�B,�B�B�BޞB�B�B�B͟BɆB�HB�B�RBBÖB�6B�B�9B�[BĜBʌBȴBǮB�KB�EB�B��B�jB�B�B�BB�[B�[B�2B�mBچB�WB�pB��B�/B�cB�B�B�B�GB�B�B�B��B�lB�JB��B�PB��B��B��B��B�B�5B�B��B�B��B�
B�B�B�fB�ZB��B�,B��B��B�fB��B�B�cB�"B�B�B�cB�WB�WB�TB�B�+B��B�GB�B�B�B�B�BSBYB
�B�BBB{BB�B�B�B�BB(BBoB�B�B�B�B_B�B�B�BDB�B+B�B
�B	�BMBBBB	�B�B(BBeB�B�BkB�B�BSB�BPBBSB�.BGB�B�`B�xB�GB�B��B��B�?BʌB�^B��B�UB��B�B�wB��B�B�=B�zB�:B��B��B��B�4B�:B�FB�FB��B��B��B��B�eB�xB�~B��B�kB�IB��B��B�zB�B��B��B�dB�)B��B��B��B�)B��B�vB�#BٴB�QB��B֡B՛B��B��B�mB��B��B�aB�6B��B�^B��B�#B�6B�B��BȀBǮB��B��B�HB�B�}B��B�wB�B��B��B�qB��B�jB��B�}B��B��B�B�dB��B�B�dB��B�B��B�$B��B�B��B�FB�B��B�B�9B�3B��B�3B�9B�B�UB�[B�B��B�B��B��B��B��B��B�eB��B��B��B��B��B�IB�CB��B��B��B��B��B�JB��B��B�MB��B��B}�B}�Bu�Br�Bs�B|Bp�Bl�BlWBq�Bm�BjBiDBjBa|BQ�BO�BK�BIRBH�BHKBIBJ�BP�B>BB9�B<B>wB<�B:*B:^B5�B5�B2aB0!B9�B(XB,�B�BeB�B{B�B�BhB�B�B�BYBB
��B
�B
��B
��B
��B
�B
�B
�;B
�B
�mB
�mB
�gB
�B
ĜB
�9B
�aB
�B
�'B
��B
��B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2021042619360620210426193606IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021042700033720210427000337QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021042700033720210427000337QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014620210427140146IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20220504162944              CF      PSAL                            ?��G�O�D�5�G�O�?�  G�O�Sensor Failure                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20220504163452    CF                  PSAL            G�O�?��G�O�CG��G�O�?�                  Sensor Failure  SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                