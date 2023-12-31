CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-12-27T19:11:32Z creation; 2023-04-26T19:14:27Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181227191132  20230426191427  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               #   #AA  AOAO7316_008644_035                 7316_008644_035                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @؛qr�@؛qr�11  @؛qTɅ�@؛qTɅ�@*���8�K@*���8�K�c���^��c���^�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@   @B�\@}p�@��R@�  @�  A   A��A   A,(�A?\)A_\)A�  A�Q�A�Q�A�Q�A�Q�A�Q�A�  A�  B (�B(�BQ�B(�B   B(  B0  B8  B?�
BG�BP  BX(�B`(�Bh  Bp  Bx��B~�HB��B�  B��B��B��B�  B��B�  B�{B�  B�  B�{B�(�B�{B��B�  B��B��B��B�  B�  B�  B�  B��B��
B�  B��B��B��B�{B�(�C �C
=C  C��C�C	��C  C  C��C�C��C��C��C��C  C{C   C!��C$  C&
=C(  C*
=C,{C.  C/��C2  C3�C5�C7��C9��C<
=C>  C@  CB  CC��CF  CH{CJ
=CK��CM�CP  CR
=CT{CV
=CW��CZ  C\
=C^  C`  Cb{Cd{Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs��Cv  Cx
=Cz  C|  C~  C��C���C�  C�C�  C���C�  C�C�C�C�C�  C�  C�  C�C�
=C�C�C�  C���C���C���C�  C�C�  C�C�C���C�  C�C�  C���C���C�  C�  C�C�  C�  C�C�C�  C�  C�  C�  C�  C���C�  C�C�  C���C�  C�  C�  C�C�  C���C���C�  C�  C�  C���C���C�  C�  C�  C�  C���C���C�  C�  C�  C�  C���C�  C�C�C���C���C�  C���C���C���C���C�  C�  C���C���C�  C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C�  C�  C�C�C�C�C�C�  C�C�  C�  C�  C���C���C�  C���C�  C�C�  C�  C�  C�C�C�  C�  C�C�C�  C�D �D }qD ��D}qD�D� D�qD��D�D��D  D}qD�qD��D�D� D  D��D	�D	}qD	��D
z�D  D��D�D}qD��D� D�D��D�D}qD  D��D  D��D�D��D�D��D�D��D  D� D  D��D  D� D�D�D�D��D  D� D�qDz�D�qD� D  D� D�D� D  D� D�qD }qD �qD!}qD!�qD"� D#  D#��D$�D$� D%  D%� D%�qD&� D'�D'� D'�qD(� D)  D)}qD*  D*}qD*�qD+��D,  D,}qD-�D-��D.  D.��D.��D/}qD/�qD0}qD1  D1��D2�D2}qD3  D3� D4  D4� D4�qD5� D6  D6}qD6�qD7��D8�D8}qD9  D9� D:  D:�D;D;}qD;�qD<}qD<�qD=}qD>  D>� D?  D?��D@  D@z�D@��DA��DB�DB��DC�DC}qDD  DD� DD��DE}qDF�DF� DG  DG�DH�DH��DI�DI}qDJ  DJ� DK  DK��DK�qDL}qDM  DM� DN  DN� DO  DO� DP  DP� DQ�DQ��DR  DR� DS  DS� DT  DT� DT�qDU��DV  DV}qDV�qDW}qDX�DX� DY  DY��DZ  DZ� D[�D[� D[�qD\� D]�D]� D]�qD^� D_�D_� D_�qD`��Da  Da}qDb  Db��Dc  Dc��Dd�Dd}qDe  De�Df  Df� DgDg��DhDh�Di  DixRDi��Dj� Dk�Dk� Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn�RDo}qDp�Dp��Dq  Dq}qDq��Dr� Ds  Ds��Dt�Dt��Du�Du��Du�qDvz�Dv�qDw}qDw�qDx� Dx�qDy}qDy�qDz� D{  D{��D|  D|� D}  D}� D~�D~� D  D� D�qD�>�D�� D�� D���D�>�D�� D�� D�  D�>�D�}qD���D�HD�AHD��HD���D���D�@ D�� D���D���D�>�D�� D�� D�  D�@ D�� D��HD�  D�>�D�}qD�� D��D�@ D�}qD���D�  D�>�D�� D�� D�HD�AHD��HD�� D�  D�AHD�� D��HD�  D�=qD�� D�D�HD�>�D�� D��HD�  D�>�D�~�D���D�HD�C�D�� D��qD��qD�>�D�~�D��HD�  D�>�D��HD��HD�HD�AHD��HD�� D�  D�@ D�~�D�� D�  D�B�D���D��HD�  D�=qD�~�D��HD��D�AHD�� D��qD���D�@ D��HD�D��D�AHD��HD�D�  D�@ D��HD�� D�HD�>�D�� D�� D�  D�AHD��HD�� D���D�@ D�� D���D�HD�@ D�� D��HD�HD�B�D�� D���D�  D�>�D�}qD�� D�HD�AHD��HD�� D���D�@ D�~�D�� D��D�@ D�� D���D�  D�AHD���D��HD�  D�@ D�~�D���D���D�@ D�~�D���D��qD�=qD�~�D�� D���D�=qD�� D���D���D�>�D��HD�� D���D�AHD�� D�� D�HD�@ D�~�D�� D�HD�B�D��HD�� D�HD�@ D�~�D�� D��D�AHD�~�D��qD�  D�>�D�~�D��HD�HD�@ D���D�� D���D�AHD�� D���D�HD�@ D�~�D��HD�HD�AHD�� D�� D�  D�>�D��HD��HD�  D�AHD�� D�� D�HD�AHD�~�D���D�  D�@ D�� D�� D���D�=qD�� D��HD���D�>�D�� D��HD�  D�@ D�~�D���D�HD�@ D�� D��HD�HD�AHD��HD��qD��qD�>�D�� D��HD�  D�@ D��HD��HD�HD�AHD��HD���D�  D�AHD�� D���D�  D�AHD�~�D���D�  D�AHD��HD�� D�  D�@ D�~�D¾�D�  D�@ DÀ Dþ�D���D�@ DĂ�D��HD�  D�@ Dŀ D�� D�HD�@ Dƀ D��HD�  D�>�Dǀ D�� D���D�>�D�~�D�� D�  D�@ Dɀ D��HD�  D�@ D�~�Dʾ�D�  D�@ Dˀ D�� D�HD�AHD�~�D�� D�  D�=qD̀ D��HD�  D�@ D΀ Dξ�D�  D�@ Dπ D��HD�  D�>�DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�AHDӁHDӾ�D���D�>�D�~�DԾ�D��qD�>�D�~�D�� D�HD�@ D�~�D־�D���D�>�DׁHD�� D�  D�AHD؁HD�� D�HD�AHDفHD�� D�HD�@ DځHD�� D���D�>�Dۀ D�� D���D�@ D܀ Dܾ�D�  D�@ D�~�Dݾ�D���D�AHDހ D޾�D���D�@ D߁HD�� D���D�@ D��HD�� D�  D�@ D�HD��HD�  D�@ D�HD��HD�  D�AHD�HD��HD�HD�@ D�~�D侸D�  D�@ D� D��HD�  D�>�D� D��HD�HD�AHD�HD�� D�  D�AHD� D�� D���D�@ D�~�D�� D�  D�AHD�HD��HD���D�AHD�HD뾸D�  D�@ D� D�� D�HD�B�D�~�D��qD�  D�B�D�HD�� D���D�>�D�~�D�� D�  D�AHD�� D�D��qD�>�D� D�� D�  D�@ D� D��HD���D�=qD�}qD�D���D�>�D� D��HD�  D�>�D�~�D�� D�  D�AHD��HD�� D��qD�>�D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�D�  D�AHD�s3D��R?.{?8Q�?k�?��?��
?�p�?��?��@
=q@��@#�
@0��@@  @Q�@^�R@h��@z�H@��@�=q@��@��H@�  @�ff@��@�33@��H@�  @��@�=q@��@ٙ�@�  @��@�=q@��@���@��RA�\AA
=qA��A  A�
A�A�A�RA"�\A'
=A+�A.�RA333A7�A=p�AAG�AE�AJ�HAP  ATz�AXQ�A]p�Ac33Ag�Al(�Ap��AuAz�HA~�RA���A�z�A�
=A�G�A��A�{A���A��HA��A��A��\A���A��RA���A��
A��RA���A��HA�p�A�Q�A��HA���A��A�=qA���A�
=A�G�A�(�AƸRA���A��HA�AУ�A�33A��A�\)A�=qA���A޸RA���A��
A�ffA��A�\A���A�A��A�(�A�ffA���A�33A�p�A��B ��B=qB�B��B��B
=BQ�B	p�B
�\B�
BG�BffB�B��B{B�B��BB33B��BB�HB(�B��B�HB   B!G�B"�\B$  B%G�B&ffB'�B(��B*ffB+�B,��B.=qB/�B0��B1B333B4��B5B7
=B8Q�B9B:�HB<  B=�B>�\B@  BA�BB=qBC\)BD��BF{BG33BH(�BIp�BJ�HBL  BM�BNffBO�BP��BR{BS33BTQ�BUp�BV�RBX(�BY�BZ{B[33B\z�B]B^�HB_�
B`��Bb{Bc\)Bdz�Bep�BfffBg�Bh��Bi�Bk33Bl(�Bm�Bn{Bo\)BpQ�BqG�Br{Bs
=Bt  Bt��Bu�Bv�\Bw�Bxz�ByG�By�BzffBz�HB{�B|  B|  B|(�B|(�B|Q�B|Q�B|(�B|  B{�B{�B{�B{�B{\)B{
=Bz�HBz�RBz�RBz�\BzffBz{By�By�By�ByByG�By�By�By�Bx��Bx��Bxz�BxQ�BxQ�Bx(�Bx(�Bw�
Bw�Bw�Bw�Bw�Bw\)Bw33Bv�HBv�HBv�HBw
=Bv�HBv�RBv�RBv�RBv�HBv�HBv�RBv�\Bv�RBw
=Bw
=Bw
=Bw33Bw33Bw�Bw�
Bx  BxQ�Bx��Bx��Bx��ByG�By��Bz{Bz�\Bz�HB{�B{�
B|(�B|z�B|��B}�B}��B~{B~�\B
=B\)B�B�{B�ffB���B���B�33B�p�B��B��B�(�B�z�B���B�33B��B��B�(�B��\B���B�G�B��B�(�B��\B���B�\)B�B�(�B��\B���B�\)B��
B�=qB��RB�33B���B�{B�ffB��HB�G�B�B�Q�B��HB�\)B��
B�Q�B���B�G�B�B�(�B��RB�G�B��
B�ffB��HB�\)B��B�ffB���B�p�B�  B�z�B��B��B�Q�B��HB�\)B��B�z�B���B���B�(�B���B�p�B�  B��\B�33B�B�Q�B���B��B�{B��RB�G�B�B�Q�B���B��B�{B��RB�33B��
B�Q�B���B���B�{B��RB�\)B��
B�z�B�
=B���B�{B��RB�G�B�B�Q�B���B�\)B��
B�ffB���B�p�B�  B��\B�
=B���B�{B���B�33B��B�(�B��RB�33B��B�(�B���B��B���B�{B��\B�
=B��B�  B�ffB��HB�\)B��
B�=qB¸RB�33BîB�(�Bď\B�
=B�p�B��B�Q�B���B�G�BǮB�=qBȣ�B�33BɮB�(�Bʣ�B��BˮB�(�B̏\B�
=BͅB�{BΣ�B��Bϙ�B�=qBиRB�G�B�B�Q�B��HB�p�B�  Bԏ\B��BծB�(�BָRB�G�B�B�=qB���B�\)B��
B�Q�B��HB�\)B��
B�z�B���B݅B�  Bޣ�B�33B�B�Q�B��HB�p�B�  B�\B��B�B�Q�B��HB�p�B�{B��B�G�B��B�\B�G�B��B�\B�33B��
B�ffB�
=B��B�Q�B���B�B�Q�B�
=B�B�z�B��B��
B�z�B�33B��
B�z�B�33B��
B��\B�33B�  B��RB�\)B�  B��RB�\)B�  B��RB�p�C {C p�C �
C(�C�\C�CG�C��C��CG�C��C  C\)C�RC�Cz�C��C(�Cz�C�
C33C�\C�CQ�C�C	  C	\)C	�C
  C
ffC
�RC�Cp�C��C{Cp�CC{Cp�C��C�Cz�CC
=C\)C�RC
=CffCC{C\)C�C  CG�C��C
=C\)C�C��CG�C��C�HC=qC��C�C33Cz�C��C�Cz�C��C(�Cp�CC
=C\)C�RC{CffC�C  CQ�C��C  CQ�C�C��CG�C��C�HC=qC�\C�HC(�Cp�C�RC 
=C ffC �RC!
=C!\)C!��C!�C"33C"�C"�HC#=qC#�\C#�
C$(�C$p�C$C%{C%p�C%C&
=C&Q�C&��C&��C'G�C'��C'��C(G�C(��C(�HC)33C)�C)�
C*33C*�C*�HC+(�C+z�C+��C,(�C,�C,�
C-(�C-p�C-��C.33C.�C.�
C/(�C/�C/�C0=qC0�\C0�C1=qC1��C2  C2\)C2��C3  C3ffC3C4�C4p�C4��C533C5�\C5��C6Q�C6�C7
=C7ffC7C8�C8�C8�C9=qC9��C9��C:Q�C:�C;�C;�C;�
C<(�C<��C=  C=\)C=�RC>�C>p�C>�
C?=qC?��C@  C@\)C@�RCA(�CA��CA�CBG�CB��CC{CCz�CC�HCD=qCD��CE  CEffCE�
CF=qCF��CF��CG\)CGCH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      ?�=q@   @B�\@}p�@��R@�  @�  A   A��A   A,(�A?\)A_\)A�  A�Q�A�Q�A�Q�A�Q�A�Q�A�  A�  B (�B(�BQ�B(�B   B(  B0  B8  B?�
BG�BP  BX(�B`(�Bh  Bp  Bx��B~�HB��B�  B��B��B��B�  B��B�  B�{B�  B�  B�{B�(�B�{B��B�  B��B��B��B�  B�  B�  B�  B��B��
B�  B��B��B��B�{B�(�C �C
=C  C��C�C	��C  C  C��C�C��C��C��C��C  C{C   C!��C$  C&
=C(  C*
=C,{C.  C/��C2  C3�C5�C7��C9��C<
=C>  C@  CB  CC��CF  CH{CJ
=CK��CM�CP  CR
=CT{CV
=CW��CZ  C\
=C^  C`  Cb{Cd{Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs��Cv  Cx
=Cz  C|  C~  C��C���C�  C�C�  C���C�  C�C�C�C�C�  C�  C�  C�C�
=C�C�C�  C���C���C���C�  C�C�  C�C�C���C�  C�C�  C���C���C�  C�  C�C�  C�  C�C�C�  C�  C�  C�  C�  C���C�  C�C�  C���C�  C�  C�  C�C�  C���C���C�  C�  C�  C���C���C�  C�  C�  C�  C���C���C�  C�  C�  C�  C���C�  C�C�C���C���C�  C���C���C���C���C�  C�  C���C���C�  C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C�  C�  C�C�C�C�C�C�  C�C�  C�  C�  C���C���C�  C���C�  C�C�  C�  C�  C�C�C�  C�  C�C�C�  C�D �D }qD ��D}qD�D� D�qD��D�D��D  D}qD�qD��D�D� D  D��D	�D	}qD	��D
z�D  D��D�D}qD��D� D�D��D�D}qD  D��D  D��D�D��D�D��D�D��D  D� D  D��D  D� D�D�D�D��D  D� D�qDz�D�qD� D  D� D�D� D  D� D�qD }qD �qD!}qD!�qD"� D#  D#��D$�D$� D%  D%� D%�qD&� D'�D'� D'�qD(� D)  D)}qD*  D*}qD*�qD+��D,  D,}qD-�D-��D.  D.��D.��D/}qD/�qD0}qD1  D1��D2�D2}qD3  D3� D4  D4� D4�qD5� D6  D6}qD6�qD7��D8�D8}qD9  D9� D:  D:�D;D;}qD;�qD<}qD<�qD=}qD>  D>� D?  D?��D@  D@z�D@��DA��DB�DB��DC�DC}qDD  DD� DD��DE}qDF�DF� DG  DG�DH�DH��DI�DI}qDJ  DJ� DK  DK��DK�qDL}qDM  DM� DN  DN� DO  DO� DP  DP� DQ�DQ��DR  DR� DS  DS� DT  DT� DT�qDU��DV  DV}qDV�qDW}qDX�DX� DY  DY��DZ  DZ� D[�D[� D[�qD\� D]�D]� D]�qD^� D_�D_� D_�qD`��Da  Da}qDb  Db��Dc  Dc��Dd�Dd}qDe  De�Df  Df� DgDg��DhDh�Di  DixRDi��Dj� Dk�Dk� Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn�RDo}qDp�Dp��Dq  Dq}qDq��Dr� Ds  Ds��Dt�Dt��Du�Du��Du�qDvz�Dv�qDw}qDw�qDx� Dx�qDy}qDy�qDz� D{  D{��D|  D|� D}  D}� D~�D~� D  D� D�qD�>�D�� D�� D���D�>�D�� D�� D�  D�>�D�}qD���D�HD�AHD��HD���D���D�@ D�� D���D���D�>�D�� D�� D�  D�@ D�� D��HD�  D�>�D�}qD�� D��D�@ D�}qD���D�  D�>�D�� D�� D�HD�AHD��HD�� D�  D�AHD�� D��HD�  D�=qD�� D�D�HD�>�D�� D��HD�  D�>�D�~�D���D�HD�C�D�� D��qD��qD�>�D�~�D��HD�  D�>�D��HD��HD�HD�AHD��HD�� D�  D�@ D�~�D�� D�  D�B�D���D��HD�  D�=qD�~�D��HD��D�AHD�� D��qD���D�@ D��HD�D��D�AHD��HD�D�  D�@ D��HD�� D�HD�>�D�� D�� D�  D�AHD��HD�� D���D�@ D�� D���D�HD�@ D�� D��HD�HD�B�D�� D���D�  D�>�D�}qD�� D�HD�AHD��HD�� D���D�@ D�~�D�� D��D�@ D�� D���D�  D�AHD���D��HD�  D�@ D�~�D���D���D�@ D�~�D���D��qD�=qD�~�D�� D���D�=qD�� D���D���D�>�D��HD�� D���D�AHD�� D�� D�HD�@ D�~�D�� D�HD�B�D��HD�� D�HD�@ D�~�D�� D��D�AHD�~�D��qD�  D�>�D�~�D��HD�HD�@ D���D�� D���D�AHD�� D���D�HD�@ D�~�D��HD�HD�AHD�� D�� D�  D�>�D��HD��HD�  D�AHD�� D�� D�HD�AHD�~�D���D�  D�@ D�� D�� D���D�=qD�� D��HD���D�>�D�� D��HD�  D�@ D�~�D���D�HD�@ D�� D��HD�HD�AHD��HD��qD��qD�>�D�� D��HD�  D�@ D��HD��HD�HD�AHD��HD���D�  D�AHD�� D���D�  D�AHD�~�D���D�  D�AHD��HD�� D�  D�@ D�~�D¾�D�  D�@ DÀ Dþ�D���D�@ DĂ�D��HD�  D�@ Dŀ D�� D�HD�@ Dƀ D��HD�  D�>�Dǀ D�� D���D�>�D�~�D�� D�  D�@ Dɀ D��HD�  D�@ D�~�Dʾ�D�  D�@ Dˀ D�� D�HD�AHD�~�D�� D�  D�=qD̀ D��HD�  D�@ D΀ Dξ�D�  D�@ Dπ D��HD�  D�>�DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�AHDӁHDӾ�D���D�>�D�~�DԾ�D��qD�>�D�~�D�� D�HD�@ D�~�D־�D���D�>�DׁHD�� D�  D�AHD؁HD�� D�HD�AHDفHD�� D�HD�@ DځHD�� D���D�>�Dۀ D�� D���D�@ D܀ Dܾ�D�  D�@ D�~�Dݾ�D���D�AHDހ D޾�D���D�@ D߁HD�� D���D�@ D��HD�� D�  D�@ D�HD��HD�  D�@ D�HD��HD�  D�AHD�HD��HD�HD�@ D�~�D侸D�  D�@ D� D��HD�  D�>�D� D��HD�HD�AHD�HD�� D�  D�AHD� D�� D���D�@ D�~�D�� D�  D�AHD�HD��HD���D�AHD�HD뾸D�  D�@ D� D�� D�HD�B�D�~�D��qD�  D�B�D�HD�� D���D�>�D�~�D�� D�  D�AHD�� D�D��qD�>�D� D�� D�  D�@ D� D��HD���D�=qD�}qD�D���D�>�D� D��HD�  D�>�D�~�D�� D�  D�AHD��HD�� D��qD�>�D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�D�  D�AHD�s3G�O�?.{?8Q�?k�?��?��
?�p�?��?��@
=q@��@#�
@0��@@  @Q�@^�R@h��@z�H@��@�=q@��@��H@�  @�ff@��@�33@��H@�  @��@�=q@��@ٙ�@�  @��@�=q@��@���@��RA�\AA
=qA��A  A�
A�A�A�RA"�\A'
=A+�A.�RA333A7�A=p�AAG�AE�AJ�HAP  ATz�AXQ�A]p�Ac33Ag�Al(�Ap��AuAz�HA~�RA���A�z�A�
=A�G�A��A�{A���A��HA��A��A��\A���A��RA���A��
A��RA���A��HA�p�A�Q�A��HA���A��A�=qA���A�
=A�G�A�(�AƸRA���A��HA�AУ�A�33A��A�\)A�=qA���A޸RA���A��
A�ffA��A�\A���A�A��A�(�A�ffA���A�33A�p�A��B ��B=qB�B��B��B
=BQ�B	p�B
�\B�
BG�BffB�B��B{B�B��BB33B��BB�HB(�B��B�HB   B!G�B"�\B$  B%G�B&ffB'�B(��B*ffB+�B,��B.=qB/�B0��B1B333B4��B5B7
=B8Q�B9B:�HB<  B=�B>�\B@  BA�BB=qBC\)BD��BF{BG33BH(�BIp�BJ�HBL  BM�BNffBO�BP��BR{BS33BTQ�BUp�BV�RBX(�BY�BZ{B[33B\z�B]B^�HB_�
B`��Bb{Bc\)Bdz�Bep�BfffBg�Bh��Bi�Bk33Bl(�Bm�Bn{Bo\)BpQ�BqG�Br{Bs
=Bt  Bt��Bu�Bv�\Bw�Bxz�ByG�By�BzffBz�HB{�B|  B|  B|(�B|(�B|Q�B|Q�B|(�B|  B{�B{�B{�B{�B{\)B{
=Bz�HBz�RBz�RBz�\BzffBz{By�By�By�ByByG�By�By�By�Bx��Bx��Bxz�BxQ�BxQ�Bx(�Bx(�Bw�
Bw�Bw�Bw�Bw�Bw\)Bw33Bv�HBv�HBv�HBw
=Bv�HBv�RBv�RBv�RBv�HBv�HBv�RBv�\Bv�RBw
=Bw
=Bw
=Bw33Bw33Bw�Bw�
Bx  BxQ�Bx��Bx��Bx��ByG�By��Bz{Bz�\Bz�HB{�B{�
B|(�B|z�B|��B}�B}��B~{B~�\B
=B\)B�B�{B�ffB���B���B�33B�p�B��B��B�(�B�z�B���B�33B��B��B�(�B��\B���B�G�B��B�(�B��\B���B�\)B�B�(�B��\B���B�\)B��
B�=qB��RB�33B���B�{B�ffB��HB�G�B�B�Q�B��HB�\)B��
B�Q�B���B�G�B�B�(�B��RB�G�B��
B�ffB��HB�\)B��B�ffB���B�p�B�  B�z�B��B��B�Q�B��HB�\)B��B�z�B���B���B�(�B���B�p�B�  B��\B�33B�B�Q�B���B��B�{B��RB�G�B�B�Q�B���B��B�{B��RB�33B��
B�Q�B���B���B�{B��RB�\)B��
B�z�B�
=B���B�{B��RB�G�B�B�Q�B���B�\)B��
B�ffB���B�p�B�  B��\B�
=B���B�{B���B�33B��B�(�B��RB�33B��B�(�B���B��B���B�{B��\B�
=B��B�  B�ffB��HB�\)B��
B�=qB¸RB�33BîB�(�Bď\B�
=B�p�B��B�Q�B���B�G�BǮB�=qBȣ�B�33BɮB�(�Bʣ�B��BˮB�(�B̏\B�
=BͅB�{BΣ�B��Bϙ�B�=qBиRB�G�B�B�Q�B��HB�p�B�  Bԏ\B��BծB�(�BָRB�G�B�B�=qB���B�\)B��
B�Q�B��HB�\)B��
B�z�B���B݅B�  Bޣ�B�33B�B�Q�B��HB�p�B�  B�\B��B�B�Q�B��HB�p�B�{B��B�G�B��B�\B�G�B��B�\B�33B��
B�ffB�
=B��B�Q�B���B�B�Q�B�
=B�B�z�B��B��
B�z�B�33B��
B�z�B�33B��
B��\B�33B�  B��RB�\)B�  B��RB�\)B�  B��RB�p�C {C p�C �
C(�C�\C�CG�C��C��CG�C��C  C\)C�RC�Cz�C��C(�Cz�C�
C33C�\C�CQ�C�C	  C	\)C	�C
  C
ffC
�RC�Cp�C��C{Cp�CC{Cp�C��C�Cz�CC
=C\)C�RC
=CffCC{C\)C�C  CG�C��C
=C\)C�C��CG�C��C�HC=qC��C�C33Cz�C��C�Cz�C��C(�Cp�CC
=C\)C�RC{CffC�C  CQ�C��C  CQ�C�C��CG�C��C�HC=qC�\C�HC(�Cp�C�RC 
=C ffC �RC!
=C!\)C!��C!�C"33C"�C"�HC#=qC#�\C#�
C$(�C$p�C$C%{C%p�C%C&
=C&Q�C&��C&��C'G�C'��C'��C(G�C(��C(�HC)33C)�C)�
C*33C*�C*�HC+(�C+z�C+��C,(�C,�C,�
C-(�C-p�C-��C.33C.�C.�
C/(�C/�C/�C0=qC0�\C0�C1=qC1��C2  C2\)C2��C3  C3ffC3C4�C4p�C4��C533C5�\C5��C6Q�C6�C7
=C7ffC7C8�C8�C8�C9=qC9��C9��C:Q�C:�C;�C;�C;�
C<(�C<��C=  C=\)C=�RC>�C>p�C>�
C?=qC?��C@  C@\)C@�RCA(�CA��CA�CBG�CB��CC{CCz�CC�HCD=qCD��CE  CEffCE�
CF=qCF��CF��CG\)CGCH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A��A��A��A��A��A��A���AӮAӓuAә�AӓuAӏ\AӉ7A�|�A�l�A�Q�A�/A�/A�+A�VAω7A�oA�ĜA�|�A�
=A�;dA�(�A���A�(�A�?}A�r�A�7LA���A��A���A��
A���A��A��9A��PA�$�A�^5A���A���A��DA�C�A���A�v�Ap�A~ �Au��Ap��Am�TAj�jAf�HAd�A`��A`M�A_��A_S�A^�A]?}AXAT�9ASdZANA�AIAEXAAXA>M�A;��A:�A8�RA7oA5l�A2��A05?A/��A.��A.VA.�\A-�A-/A,^5A,jA,�A,I�A+oA(I�A&jA%�;A&�A&jA&E�A%�-A$^5A#\)A#G�A!�A �`A!��A"�A$�\A%�A%ƨA%�PA%33A$��A$VA$jA$~�A$=qA#A#hsA"��A"9XA!�^A!�hA!VA �uA 5?A��AK�A�`AffA�A�PA��A��Av�AbA��A"�A�A��A��An�AJA��A��A�#A�AO�A"�A�/Ar�A�;A��A\)A;dA�A��AbNA��A�
A�hAK�A"�A%A��Av�A$�AA��A�7AK�A�A��A~�A  AhsA+A��A��AffA��A\)AA�9Av�AE�A(�A{A  A��A7LA
��A
�\A
ffA
VA	�TA	G�A	
=A��AVA�AƨA�At�A\)AK�A�AĜA�A%A��A^5A�^A
=A�!A�AbNA1A�A
=A Q�A 1'@��F@��y@���@�M�@��T@�hs@��@�r�@�1'@�+@�-@��-@�p�@��u@� �@��w@�\)@��@��@��-@��-@�hs@��@�D@�A�@��@�+@�5?@�G�@�  @�l�@�ȴ@���@�j@�w@���@�7L@��@��;@��@�&�@�@�z�@�1@�;d@���@�\@�E�@�@��@�x�@�&�@��D@�1'@��m@��@�-@�hs@��
@ۅ@�dZ@�o@ڏ\@�@ى7@�/@�I�@ָR@�{@պ^@�O�@���@ԣ�@�A�@��@ӝ�@�
=@ҸR@�E�@�-@�{@ѡ�@��@ЋD@�ƨ@υ@�
=@ͩ�@̛�@�z�@�Z@�A�@�1'@�b@�  @��@��@��m@��;@��
@˕�@�|�@�K�@�@ʇ+@��#@�O�@���@�  @�dZ@��@�v�@�@�V@�Q�@Å@��@¸R@§�@+@�M�@���@��#@���@�G�@���@���@�Q�@��@�l�@�@��R@�^5@�{@��-@���@�x�@�O�@�/@�%@�Ĝ@�Z@��w@���@�=q@��@���@��-@��-@��^@��-@�&�@���@�r�@���@���@�t�@�;d@���@�J@��@�@���@��h@�x�@�O�@���@��j@�Z@��w@���@���@�C�@��H@�n�@�5?@��@��^@��@��@��@�A�@��m@��@�+@��H@��R@���@�M�@��#@��^@�hs@���@�I�@� �@�1@���@�dZ@���@��@��h@�?}@���@��D@�r�@� �@��@�33@�o@��H@��!@�v�@�-@�hs@�&�@�V@��9@�Z@�Q�@� �@��@���@�l�@��@��y@��!@�^5@��@���@�hs@�7L@�%@��D@�b@��P@�@��H@�ȴ@��+@�J@��^@�7L@�Ĝ@�r�@�b@�S�@��y@�V@�J@��@��^@��h@�O�@���@�Q�@���@�K�@�+@��@��+@�^5@�M�@�@�x�@�V@�z�@��@��@���@���@���@�dZ@�"�@��@���@�~�@��@��#@���@�O�@�7L@�V@��`@��@�9X@���@�|�@�S�@��@�V@�$�@�@��-@��@��@�Ĝ@��D@�Z@��@��F@�|�@�S�@�"�@�o@���@��R@���@�V@�J@�x�@��@�%@��`@��9@�j@��@�t�@�\)@�C�@�+@��@���@�v�@�=q@�@���@��-@��h@�?}@�V@���@�z�@��@�Q�@��@��F@�|�@�+@���@��H@��@���@�V@�E�@��@��@�@���@��7@�hs@�G�@���@�r�@�1@�  @�  @�;@��@~�@~V@}�-@|��@|Z@{��@{ƨ@{��@{�@{C�@z��@zM�@y��@y%@xĜ@xbN@w�@w�@w\)@w+@vv�@v@u�-@u`B@t��@t�@s�
@s��@sS�@s@r�\@q��@q�^@qG�@q�@p��@p��@pbN@pQ�@p �@p  @ol�@nff@m�@m/@l�@l9X@kC�@j��@j~�@j^5@i��@i�7@i�@h�`@h�@h  @g��@g��@g
=@fE�@e�T@e��@e�@e�@d�@dZ@d(�@c�
@c�F@c33@b��@bn�@b�@a��@a�@`Ĝ@`��@`bN@_�@_��@_\)@_
=@^�y@^��@^ff@^$�@]�T@]�@]?}@\��@\�D@\Z@[�m@[��@[dZ@[33@[o@Z�@Z��@Z��@Z�@Y�@Y�7@X�`@XbN@X  @W�w@W��@W|�@W\)@V��@V�+@V$�@V@U�h@UV@Tj@TI�@S�m@S�
@S�F@SS�@R�@R�!@R�@Q��@Q�7@Qx�@Qhs@Q�@PĜ@P1'@P  @O�;@O�P@N��@N�+@N{@M��@M/@L�/@L�j@L��@LI�@K�
@K��@KdZ@KS�@K@J�!@J�\@J^5@J�@I��@HA�@G\)@F�y@F�R@F�+@FV@F{@E�@E�@EO�@D��@D(�@C��@C"�@B�H@B�!@B�\@B~�@A�^@AG�@@��@@Ĝ@@��@@bN@@A�@@  @?�@?\)@>�R@>5?@=�T@=�-@=`B@<��@<Z@<9X@<(�@;�m@;dZ@:�H@:�\@:J@9��@97L@8��@8��@8�9@8r�@8bN@8A�@7�@7K�@7+@6��@6ff@6{@5p�@5V@4�j@4�D@4I�@41@3�m@3�F@3��@3dZ@333@2=q@1�@1�^@1x�@1&�@0�`@0��@0Ĝ@0Q�@0b@0  @/��@/\)@/;d@/�@.�R@.{@-�T@-��@-@-��@-/@-/@-V@,�/@,��@,�D@,�@+��@+33@*�@*�!@*~�@*n�@*M�@*=q@*�@)�#@)�7@)7L@(��@(Ĝ@(�u@(�@(�@(Q�@(b@'�;@'�P@&��@&��@&V@&@%�-@%�@%O�@$�@$�j@$�D@$I�@$(�@#��@#��@#dZ@#@"��@"~�@"n�@"n�@"n�@"n�@"M�@"=q@"=q@"-@"�@"�@!�@!�^@!x�@!&�@ �9@ r�@ r�@ Q�@ 1'@  �@  �@   @��@�P@l�@;d@v�@�@��@p�@��@��@�D@I�@��@�m@�m@�F@�@S�@�@�\@=q@J@�^@��@G�@%@%@�`@�`@��@Ĝ@�9@�9@r�@1'@�@�w@�P@l�@K�@K�@;d@��@ȴ@��@ff@V@V@V@E�@{@�-@�@/@V@��@z�@Z@Z@Z@Z@Z@9X@��@�
@��@��@��@33@"�@��@~�@^5@^5@��@�^@x�@G�@&�@%@��@��@bN@Q�@Q�@Q�@A�@1'@b@��@�@|�@l�@K�@�y@ȴ@�+@ff@V@$�@{@��@��@�h@�h@�h@�hA��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A���A���A��A��A��A���A���A��A��A���A���A��A��A��A���A���A��A���A���A���A��A��A���A���A���A��A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A���A���A��A��A��A��A��A��A��A��A���A��A��A��A���A��A��A��A��A��A��;A���A���A���A���A�ȴA�ƨA�AӸRAӸRAӰ!Aө�AӬAӟ�AӉ7AӇ+AӍPAә�Aӛ�Aӛ�Aӝ�Aӛ�Aә�Aӗ�Aӕ�Aӗ�Aә�Aӕ�Aӏ\AӓuAӕ�AӑhAӏ\AӑhAӑhAӏ\AӍPAӑhAӓuAӏ\AӋDAӇ+AӋDAӉ7AӁA�~�AӁAӁA�~�A�z�A�x�A�x�A�v�A�p�A�ffA�hsA�l�A�jA�bNA�ZA�ZA�VA�O�A�G�A�C�A�9XA�1'A�1'A�/A�+A�+A�-A�/A�-A�+A�+A�1'A�33A�33A�1'A�/A�+A�-A�-A�(�A� �A��A�oA�oA�oA�bA�
=A�%A�1A�A�  A���A��;A�VA�C�A��Aѕ�A�I�A�
=A��/Aк^AЍPA�~�A�^5A�=qA�33A�"�A�&�A� �A�(�A��A�{A�
=A�VA�VA�{A�oA�{A��A�VA��A�%A�A���A���A���A���A���A���A�JA�  A���A���A��A��A��A��A��A��A��A��mA��HA��A���A�ȴA�ĜA϶FAϰ!AϮAϥ�Aϙ�A�ffA�;dA�I�A�7LA�&�A��A�1A�A���A��A�ĜA�~�A�$�Aͣ�A�n�A�
=A��`ÃA�A�  A�bNA�n�AȅA��A�ƨA�jA�=qA�+A��A�{A�A���A���A��HA���AƏ\A�E�A�oA��
Aŕ�A�dZA�VA�C�A�33A�{A�ĜA�Q�AÛ�A�-A�bNA��!A�A�l�A���A�?}A�O�A��yA��^A���A�v�A�dZA�XA�M�A�A�A� �A�1A��A��A��A��yA��#A��RA�n�A��
A���A��A�l�A���A��FA��DA��A�z�A�`BA�33A�1A�A���A�?}A���A�"�A��TA�9XA��A��A�VA�
=A�ĜA�n�A���A��A�p�A���A�A�~�A�"�A��;A���A�1A���A�C�A���A�=qA��A�ƨA���A��A�hsA�ffA�E�A�oA���A��
A��wA�G�A�x�A��+A���A�9XA�^5A�ĜA�+A���A��+A��+A��\A��A���A���A�VA�ZA�VA��-A���A���A�/A�\)A���A�\)A�C�A�-A��A��A�bA���A��`A���A�A���A�dZA�oA���A���A��TA�-A�G�A���A��A���A�A�
=A���A��yA��!A� �A��A�x�A�JA�ZA��+A��A���A�bNA���A�(�A�r�A�ȴA�JA��A��9A���A���A��7A��A�VA�C�A�+A��/A�n�A�t�A�7LA�1'A�{A���A�A��A���A�$�A�ffA��A��A�VA��A�=qA�(�A�A��`A���A��wA���A��DA�p�A�XA�M�A�M�A�33A�&�A�(�A�9XA�O�A�`BA��PA��DA�33A��A���A�~�A��A�-A�hsA��DA�S�A���A�1A��A~�A~��A~�A�-A�VA�|�A�v�A��A~�A}ƨA{�AzAxv�AvȴAu��Au��At�HAr�+AqAp��Apz�ApZAo��Ao�wAo��Ao/AnI�AmdZAlI�Ak;dAj�`Aj�HAj��Aj�RAjbNAi|�AhI�Af�yAf5?Ae�FAeVAd�Ad~�AdI�Ad  AcS�Ab  Aa&�A`�yA`��A`ĜA`�A`�\A`v�A`Q�A`5?A`  A_�A_�mA_�wA_��A_��A_O�A_dZA_`BA_l�A_dZA_?}A_33A_C�A_oA^�A^�/A^�9A^v�A^-A]�-A]`BA]33A\��A\�\A[��AZ��AYoAVĜAV1AU��AU�AUAT��AT��AT��ATbNAT�AS�AS�TASdZAS7LAS�ARZAQ��AO��AN5?AM&�ALr�AL �AKAJffAIAH(�AG�;AG�#AGS�AF5?AE��AE+AD��ADz�AD �AC��ABȴAAK�A@��A?�A?��A?p�A?7LA>�jA>�A=�-A=G�A<��A<E�A;�;A;�A;7LA:��A:�RA:�\A:~�A:VA9��A9�FA9��A9p�A9&�A8��A8ȴA8�DA85?A7�A7�^A7S�A7/A6�A6�\A65?A5��A5|�A5l�A5hsA5S�A5�A4^5A3�A37LA3�A2��A1�#A133A0�\A0JA/�A/�#A/��A/��A/A/��A/|�A/�A/�7A/l�A/K�A/
=A.��A.jA.5?A.1A.�A.A�A.ȴA.��A.��A.�\A.�uA.�uA.z�A.E�A.{A-�;A-�TA-��A-A-��A-�hA-O�A,�/A,��A,�+A,�DA,v�A,I�A,1'A,9XA,E�A,ZA,r�A,�DA,�DA,�DA,�+A,~�A,z�A,v�A,v�A,v�A,n�A,1'A,1A+��A+�^A+;dA*�uA*VA*�A)��A(ĜA'&�A'"�A'�A'�A'%A&I�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A��A��A��A��A��A��A���AӮAӓuAә�AӓuAӏ\AӉ7A�|�A�l�A�Q�A�/A�/A�+A�VAω7A�oA�ĜA�|�A�
=A�;dA�(�A���A�(�A�?}A�r�A�7LA���A��A���A��
A���A��A��9A��PA�$�A�^5A���A���A��DA�C�A���A�v�Ap�A~ �Au��Ap��Am�TAj�jAf�HAd�A`��A`M�A_��A_S�A^�A]?}AXAT�9ASdZANA�AIAEXAAXA>M�A;��A:�A8�RA7oA5l�A2��A05?A/��A.��A.VA.�\A-�A-/A,^5A,jA,�A,I�A+oA(I�A&jA%�;A&�A&jA&E�A%�-A$^5A#\)A#G�A!�A �`A!��A"�A$�\A%�A%ƨA%�PA%33A$��A$VA$jA$~�A$=qA#A#hsA"��A"9XA!�^A!�hA!VA �uA 5?A��AK�A�`AffA�A�PA��A��Av�AbA��A"�A�A��A��An�AJA��A��A�#A�AO�A"�A�/Ar�A�;A��A\)A;dA�A��AbNA��A�
A�hAK�A"�A%A��Av�A$�AA��A�7AK�A�A��A~�A  AhsA+A��A��AffA��A\)AA�9Av�AE�A(�A{A  A��A7LA
��A
�\A
ffA
VA	�TA	G�A	
=A��AVA�AƨA�At�A\)AK�A�AĜA�A%A��A^5A�^A
=A�!A�AbNA1A�A
=A Q�A 1'@��F@��y@���@�M�@��T@�hs@��@�r�@�1'@�+@�-@��-@�p�@��u@� �@��w@�\)@��@��@��-@��-@�hs@��@�D@�A�@��@�+@�5?@�G�@�  @�l�@�ȴ@���@�j@�w@���@�7L@��@��;@��@�&�@�@�z�@�1@�;d@���@�\@�E�@�@��@�x�@�&�@��D@�1'@��m@��@�-@�hs@��
@ۅ@�dZ@�o@ڏ\@�@ى7@�/@�I�@ָR@�{@պ^@�O�@���@ԣ�@�A�@��@ӝ�@�
=@ҸR@�E�@�-@�{@ѡ�@��@ЋD@�ƨ@υ@�
=@ͩ�@̛�@�z�@�Z@�A�@�1'@�b@�  @��@��@��m@��;@��
@˕�@�|�@�K�@�@ʇ+@��#@�O�@���@�  @�dZ@��@�v�@�@�V@�Q�@Å@��@¸R@§�@+@�M�@���@��#@���@�G�@���@���@�Q�@��@�l�@�@��R@�^5@�{@��-@���@�x�@�O�@�/@�%@�Ĝ@�Z@��w@���@�=q@��@���@��-@��-@��^@��-@�&�@���@�r�@���@���@�t�@�;d@���@�J@��@�@���@��h@�x�@�O�@���@��j@�Z@��w@���@���@�C�@��H@�n�@�5?@��@��^@��@��@��@�A�@��m@��@�+@��H@��R@���@�M�@��#@��^@�hs@���@�I�@� �@�1@���@�dZ@���@��@��h@�?}@���@��D@�r�@� �@��@�33@�o@��H@��!@�v�@�-@�hs@�&�@�V@��9@�Z@�Q�@� �@��@���@�l�@��@��y@��!@�^5@��@���@�hs@�7L@�%@��D@�b@��P@�@��H@�ȴ@��+@�J@��^@�7L@�Ĝ@�r�@�b@�S�@��y@�V@�J@��@��^@��h@�O�@���@�Q�@���@�K�@�+@��@��+@�^5@�M�@�@�x�@�V@�z�@��@��@���@���@���@�dZ@�"�@��@���@�~�@��@��#@���@�O�@�7L@�V@��`@��@�9X@���@�|�@�S�@��@�V@�$�@�@��-@��@��@�Ĝ@��D@�Z@��@��F@�|�@�S�@�"�@�o@���@��R@���@�V@�J@�x�@��@�%@��`@��9@�j@��@�t�@�\)@�C�@�+@��@���@�v�@�=q@�@���@��-@��h@�?}@�V@���@�z�@��@�Q�@��@��F@�|�@�+@���@��H@��@���@�V@�E�@��@��@�@���@��7@�hs@�G�@���@�r�@�1@�  @�  @�;@��@~�@~V@}�-@|��@|Z@{��@{ƨ@{��@{�@{C�@z��@zM�@y��@y%@xĜ@xbN@w�@w�@w\)@w+@vv�@v@u�-@u`B@t��@t�@s�
@s��@sS�@s@r�\@q��@q�^@qG�@q�@p��@p��@pbN@pQ�@p �@p  @ol�@nff@m�@m/@l�@l9X@kC�@j��@j~�@j^5@i��@i�7@i�@h�`@h�@h  @g��@g��@g
=@fE�@e�T@e��@e�@e�@d�@dZ@d(�@c�
@c�F@c33@b��@bn�@b�@a��@a�@`Ĝ@`��@`bN@_�@_��@_\)@_
=@^�y@^��@^ff@^$�@]�T@]�@]?}@\��@\�D@\Z@[�m@[��@[dZ@[33@[o@Z�@Z��@Z��@Z�@Y�@Y�7@X�`@XbN@X  @W�w@W��@W|�@W\)@V��@V�+@V$�@V@U�h@UV@Tj@TI�@S�m@S�
@S�F@SS�@R�@R�!@R�@Q��@Q�7@Qx�@Qhs@Q�@PĜ@P1'@P  @O�;@O�P@N��@N�+@N{@M��@M/@L�/@L�j@L��@LI�@K�
@K��@KdZ@KS�@K@J�!@J�\@J^5@J�@I��@HA�@G\)@F�y@F�R@F�+@FV@F{@E�@E�@EO�@D��@D(�@C��@C"�@B�H@B�!@B�\@B~�@A�^@AG�@@��@@Ĝ@@��@@bN@@A�@@  @?�@?\)@>�R@>5?@=�T@=�-@=`B@<��@<Z@<9X@<(�@;�m@;dZ@:�H@:�\@:J@9��@97L@8��@8��@8�9@8r�@8bN@8A�@7�@7K�@7+@6��@6ff@6{@5p�@5V@4�j@4�D@4I�@41@3�m@3�F@3��@3dZ@333@2=q@1�@1�^@1x�@1&�@0�`@0��@0Ĝ@0Q�@0b@0  @/��@/\)@/;d@/�@.�R@.{@-�T@-��@-@-��@-/@-/@-V@,�/@,��@,�D@,�@+��@+33@*�@*�!@*~�@*n�@*M�@*=q@*�@)�#@)�7@)7L@(��@(Ĝ@(�u@(�@(�@(Q�@(b@'�;@'�P@&��@&��@&V@&@%�-@%�@%O�@$�@$�j@$�D@$I�@$(�@#��@#��@#dZ@#@"��@"~�@"n�@"n�@"n�@"n�@"M�@"=q@"=q@"-@"�@"�@!�@!�^@!x�@!&�@ �9@ r�@ r�@ Q�@ 1'@  �@  �@   @��@�P@l�@;d@v�@�@��@p�@��@��@�D@I�@��@�m@�m@�F@�@S�@�@�\@=q@J@�^@��@G�@%@%@�`@�`@��@Ĝ@�9@�9@r�@1'@�@�w@�P@l�@K�@K�@;d@��@ȴ@��@ff@V@V@V@E�@{@�-@�@/@V@��@z�@Z@Z@Z@Z@Z@9X@��@�
@��@��@��@33@"�@��@~�@^5@^5@��@�^@x�@G�@&�@%@��@��@bN@Q�@Q�@Q�@A�@1'@b@��@�@|�@l�@K�@�y@ȴ@�+@ff@V@$�@{@��@��@�h@�h@�hG�O�A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A���A���A��A��A��A���A���A��A��A���A���A��A��A��A���A���A��A���A���A���A��A��A���A���A���A��A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A���A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A���A���A��A��A��A��A��A��A��A��A���A��A��A��A���A��A��A��A��A��A��;A���A���A���A���A�ȴA�ƨA�AӸRAӸRAӰ!Aө�AӬAӟ�AӉ7AӇ+AӍPAә�Aӛ�Aӛ�Aӝ�Aӛ�Aә�Aӗ�Aӕ�Aӗ�Aә�Aӕ�Aӏ\AӓuAӕ�AӑhAӏ\AӑhAӑhAӏ\AӍPAӑhAӓuAӏ\AӋDAӇ+AӋDAӉ7AӁA�~�AӁAӁA�~�A�z�A�x�A�x�A�v�A�p�A�ffA�hsA�l�A�jA�bNA�ZA�ZA�VA�O�A�G�A�C�A�9XA�1'A�1'A�/A�+A�+A�-A�/A�-A�+A�+A�1'A�33A�33A�1'A�/A�+A�-A�-A�(�A� �A��A�oA�oA�oA�bA�
=A�%A�1A�A�  A���A��;A�VA�C�A��Aѕ�A�I�A�
=A��/Aк^AЍPA�~�A�^5A�=qA�33A�"�A�&�A� �A�(�A��A�{A�
=A�VA�VA�{A�oA�{A��A�VA��A�%A�A���A���A���A���A���A���A�JA�  A���A���A��A��A��A��A��A��A��A��mA��HA��A���A�ȴA�ĜA϶FAϰ!AϮAϥ�Aϙ�A�ffA�;dA�I�A�7LA�&�A��A�1A�A���A��A�ĜA�~�A�$�Aͣ�A�n�A�
=A��`ÃA�A�  A�bNA�n�AȅA��A�ƨA�jA�=qA�+A��A�{A�A���A���A��HA���AƏ\A�E�A�oA��
Aŕ�A�dZA�VA�C�A�33A�{A�ĜA�Q�AÛ�A�-A�bNA��!A�A�l�A���A�?}A�O�A��yA��^A���A�v�A�dZA�XA�M�A�A�A� �A�1A��A��A��A��yA��#A��RA�n�A��
A���A��A�l�A���A��FA��DA��A�z�A�`BA�33A�1A�A���A�?}A���A�"�A��TA�9XA��A��A�VA�
=A�ĜA�n�A���A��A�p�A���A�A�~�A�"�A��;A���A�1A���A�C�A���A�=qA��A�ƨA���A��A�hsA�ffA�E�A�oA���A��
A��wA�G�A�x�A��+A���A�9XA�^5A�ĜA�+A���A��+A��+A��\A��A���A���A�VA�ZA�VA��-A���A���A�/A�\)A���A�\)A�C�A�-A��A��A�bA���A��`A���A�A���A�dZA�oA���A���A��TA�-A�G�A���A��A���A�A�
=A���A��yA��!A� �A��A�x�A�JA�ZA��+A��A���A�bNA���A�(�A�r�A�ȴA�JA��A��9A���A���A��7A��A�VA�C�A�+A��/A�n�A�t�A�7LA�1'A�{A���A�A��A���A�$�A�ffA��A��A�VA��A�=qA�(�A�A��`A���A��wA���A��DA�p�A�XA�M�A�M�A�33A�&�A�(�A�9XA�O�A�`BA��PA��DA�33A��A���A�~�A��A�-A�hsA��DA�S�A���A�1A��A~�A~��A~�A�-A�VA�|�A�v�A��A~�A}ƨA{�AzAxv�AvȴAu��Au��At�HAr�+AqAp��Apz�ApZAo��Ao�wAo��Ao/AnI�AmdZAlI�Ak;dAj�`Aj�HAj��Aj�RAjbNAi|�AhI�Af�yAf5?Ae�FAeVAd�Ad~�AdI�Ad  AcS�Ab  Aa&�A`�yA`��A`ĜA`�A`�\A`v�A`Q�A`5?A`  A_�A_�mA_�wA_��A_��A_O�A_dZA_`BA_l�A_dZA_?}A_33A_C�A_oA^�A^�/A^�9A^v�A^-A]�-A]`BA]33A\��A\�\A[��AZ��AYoAVĜAV1AU��AU�AUAT��AT��AT��ATbNAT�AS�AS�TASdZAS7LAS�ARZAQ��AO��AN5?AM&�ALr�AL �AKAJffAIAH(�AG�;AG�#AGS�AF5?AE��AE+AD��ADz�AD �AC��ABȴAAK�A@��A?�A?��A?p�A?7LA>�jA>�A=�-A=G�A<��A<E�A;�;A;�A;7LA:��A:�RA:�\A:~�A:VA9��A9�FA9��A9p�A9&�A8��A8ȴA8�DA85?A7�A7�^A7S�A7/A6�A6�\A65?A5��A5|�A5l�A5hsA5S�A5�A4^5A3�A37LA3�A2��A1�#A133A0�\A0JA/�A/�#A/��A/��A/A/��A/|�A/�A/�7A/l�A/K�A/
=A.��A.jA.5?A.1A.�A.A�A.ȴA.��A.��A.�\A.�uA.�uA.z�A.E�A.{A-�;A-�TA-��A-A-��A-�hA-O�A,�/A,��A,�+A,�DA,v�A,I�A,1'A,9XA,E�A,ZA,r�A,�DA,�DA,�DA,�+A,~�A,z�A,v�A,v�A,v�A,n�A,1'A,1A+��A+�^A+;dA*�uA*VA*�A)��A(ĜA'&�A'"�A'�A'�A'%A&I�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�1B�1B�fB��B�1B��B��B�1B��B��B�1B��B��B��B��B�fB�lB��B�	B�	B�	B�JB�MB��B��B�kB��B�UB��B��B�B��B��B��B�[B�-B]�B	xlB
8�B
��B
�IB
��B
`�B
\�B
hsB
��B
�$B
��B,�B�B�	B{JBWsB,�BDB
�B
��B
zxB
m�B
GzB
,�B
=B
e�B
-B	�OB
B	��B	��B	�_B	g�B	k�B	N<B	:*B	1'B	+6B	$@B	 'B	�B	
rB��BܒB� B��B��B��B��B�FB�9B��B�[B��B��B��B�?B�aB�B��B��B	�B	B	FB	B	�B	�B	OB	B	%�B	]/B	t�B	~�B	�B	��B	��B	�B	��B	�fB	�bB	ȀB	�iB
DB
4B
)�B
1�B
.�B
2-B
:^B
B�B
K)B
I�B
J�B
L�B
R B
TaB
U�B
W�B
W
B
U�B
T�B
T�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U2B
T�B
TaB
T�B
S�B
T�B
S�B
R B
R�B
QNB
P�B
P}B
PB
O�B
O�B
OB
N�B
N�B
OvB
OvB
P}B
P}B
PB
PHB
OBB
N�B
N<B
M�B
M�B
LdB
LdB
K�B
K�B
J�B
J�B
J#B
IB
GzB
F?B
FB
E�B
F?B
E9B
F�B
E�B
D�B
EB
D�B
D�B
D�B
D�B
DgB
EB
C�B
CaB
DgB
C�B
C-B
C�B
@�B
@�B
?}B
@�B
?}B
>wB
>�B
>B
=<B
<�B
<B
:�B
;0B
8RB
6�B
5�B
5B
4B
33B
2�B
1�B
0�B
0!B
0�B
-wB
,=B
,B
+kB
+�B
,�B
,�B
,B
*�B
(�B
(�B
)_B
*eB
(�B
(�B
)_B
(�B
(�B
(�B
(XB
'�B
&�B
&�B
&�B
&�B
%�B
%FB
$�B
$B
"�B
"hB
!-B
 �B
�B
!B
�B
�B
B
�B
�B
�B
�B
1B
_B
�B
�B
+B
�B
�B
SB
B
�B
�B
�B
�B
FB
B
{B
B
B
uB
�B
:B
:B
oB
B
�B
 B
:B
oB
�B
�B
�B
.B
.B
�B
�B
�B
�B
�B
�B
(B
�B
\B
�B
\B
�B
�B
VB
�B
"B
�B
�B
�B
PB
�B
PB
�B
PB
PB
B
�B
�B
JB
~B
B
JB
�B
xB
~B
~B
�B
B
B
B
�B
PB
�B
�B
B
�B
�B
�B
B
�B
�B
PB
PB
B
�B
�B
VB
�B
�B
�B
VB
VB
�B
"B
�B
�B
�B
�B
�B
�B
�B
�B
(B
�B
�B
�B
"B
�B
�B
�B
�B
\B
�B
�B
�B
.B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
 B
�B
bB
�B
�B
:B
�B
�B
B
B
�B
�B
�B
FB
B
B
�B
�B
FB
�B
�B
�B
B
�B
�B
�B
�B
SB
MB
�B
�B
YB
YB
�B
�B
YB
_B
�B
�B
�B
�B
�B
�B
1B
�B
B
�B
	B
�B
kB
	B
�B
qB
�B
�B
=B
�B
�B
B
�B
CB
xB
B
�B
IB
�B
�B
B
�B
�B
!B
�B
VB
VB
!B
VB
VB
!B
VB
�B
 �B
"�B
#B
"�B
#�B
$B
#�B
"4B
"�B
#nB
#nB
"�B
"�B
#:B
#:B
#�B
$�B
%zB
&�B
&�B
&�B
&�B
'RB
'�B
'�B
'�B
($B
(XB
)_B
)�B
*eB
*eB
*eB
*eB
*�B
*�B
+B
+�B
+B
,=B
+�B
+�B
+�B
+�B
,�B
,�B
-B
-�B
.B
.�B
.�B
.�B
/B
/B
/B
/�B
/�B
/�B
/�B
/�B
1'B
0�B
0�B
0�B
0�B
1[B
2aB
2�B
2�B
2�B
2�B
33B
33B
33B
3�B
3�B
4B
3�B
4B
3�B
3�B
4B
3�B
4nB
4�B
4�B
3�B
5B
49B
49B
4�B
5?B
6�B
6�B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
8B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
:*B
9�B
:^B
:�B
:�B
;0B
;�B
;dB
;�B
;dB
<B
;�B
;�B
<�B
<�B
<�B
<�B
>B
>wB
>B
>wB
>BB
>�B
?HB
?}B
?}B
?�B
?�B
?�B
@B
?�B
@B
?�B
?�B
@OB
A�B
A�B
A�B
B'B
B�B
C-B
C�B
C�B
C�B
C�B
D3B
C�B
C�B
DgB
DgB
D�B
D�B
EmB
D�B
D�B
D�B
EB
E9B
EmB
E9B
E�B
F?B
FtB
GB
F�B
F�B
F�B
GB
GzB
GzB
GzB
G�B
HKB
HB
HB
H�B
H�B
IRB
I�B
I�B
I�B
J#B
J#B
J�B
J�B
J�B
J�B
K)B
K)B
K^B
K^B
K^B
K�B
K^B
L0B
K�B
LdB
M6B
M6B
M�B
M�B
MjB
M�B
M�B
M�B
NB
NB
M�B
M�B
M�B
NB
M�B
N<B
N<B
OB
N�B
NpB
NpB
OvB
OvB
OvB
OvB
OBB
OBB
O�B
O�B
PB
P}B
P�B
P�B
QNB
RTB
R�B
S[B
S[B
S�B
S�B
TaB
T�B
T�B
T�B
T�B
UgB
U2B
U2B
U2B
U2B
U�B
V�B
W�B
WsB
WsB
WsB
W�B
WsB
WsB
W�B
W�B
XyB
X�B
YB
YKB
YB
Y�B
YKB
YB
Z�B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[WB
[�B
\]B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
^5B
^�B
^�B
_B
_;B
_�B
`B
_�B
_�B
`BB
`B
`B
`�B
`�B
`�B
a|B
aHB
a�B
bNB
b�B
b�B
b�B
c B
cTB
cTB
cTB
c�B
cTB
c�B
d�B
d�B
d�B
d�B
e`B
e`B
e`B
e`B
f2B
ffB
f2B
gB
g�B
gmB
g8B
gB
g�B
h>B
h>B
h>B
h>B
iyB
i�B
jKB
j�B
jB
jB
jKB
i�B
jKB
jB
jB
j�B
jB
j�B
j�B
j�B
kB
kQB
k�B
k�B
l�B
m)B
n/B
ncB
n�B
o B
o5B
pB
poB
p;B
p�B
qvB
q�B
q�B
q�B
rGB
rB
rGB
r|B
r�B
r�B
sB
s�B
s�B
tB
t�B
t�B
tTB
t�B
tB
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u%B
u%B
uZB
u%B
u�B
u�B
v+B
wfB
v�B
v`B
v�B
w�B
w�B
w2B
w2B
x�B
x�B
xlB
xlB
y	B
x�B
x�B
x�B
x�B
x�B
xlB
x�B
y>B
y	B
yrB
y�B
zDB
zxB
zxB
zDB
z�B
{JB
{B
{�B
{B
{�B
{�B
|B
{B
|PB
|�B
}"B
}"B
}�B
}VB
}�B
}�B
}�B
}�B
~]B
~(B
~�B
~�B
~�B
~�B
~�B
cB
.B
cB
�B
cB
�iB
�B
�iB
�4B
� B
�B
�B
�B
� B
��B
��B
��B
�B
�oB
��B
�AB
�AB
�B
�B
��B
��B
��B
�B
�B
�B
�GB
��B
�B
�B
��B
�B
�MB
��B
�MB
��B
��B
��B
��B
�B
�B
�SB
��B
��B
��B
�%B
�%B
�YB
��B
��B
�%B
��B
��B��B��B�lB��B�_B��B�7B�1B��B�fB�B��B�+B�_B�7B��B�_B��B��B��B��B��B�B��B��B�+B��B��B��B��B�_B�fB��B�fB�_B��B��B��B��B�_B��B�1B��B��B�fB�B��B��B�1B��B�fB��B��B�B�1B��B��B��B��B��B��B��B�B��B��B��B��B�fB��B��B��B��B�+B�_B��B��B��B�+B�fB�7B�1B�+B�1B�rB�	B�1B�fB�=B��B��B��B��B��B�	B�B�lB�B��B�lB�lB�rB��B�B�B�=B��B�rB��B�=B��B��B�=B�7B�=B�B��B�7B��B��B�~B�B��B�\B�@B�FB�B�MB�SB�YB�+B�=B�=B�~B�VB�B��B��B��B��B��B�LB��B��B��B�B�B�B�B��B�CB��B�wB�B��B�OB�}B�IB��B��B��B��B��B��B�B��B��B�B�B�B�?B�tB��B��B��B�?B�B��B�B�nB�?B��B�RB��B�RB��B�$B��B�jB��B��B��B��B�B�HB��B��B��B��B�OB��B��B��BÖB��B��B�UB��B�-B�gBÖB��B��B�'B�gB��B��B�[B��B��B�gB�5B��B�B�BeB&�B(�B-B49B3hB8�B@�BDgBG�BEmBG�BD�BH�BI�BL0BJ�BK)BIRBIRBI�BI�BL0BGBMjBL�BNpBO�BO�BOvBN�BN�BIRBN<BN�BN<BM�BN�BN�BN�BN�BN<BM�BNpBOvBP}BP}BR BP�BTaBT�BS&BS�BQ�B^�BjBc BcTBd�Bg8BiDBiBh�Bi�Bo�B�4B��B�eB��B��B��B�yB	B	@B	8�B	m]B	��B	�B	�[B	��B	�zB	��B	�dB	�<B	҉B	�[B	�[B	��B	��B	�B	��B	�fB
�B
VB
oB
�B
�B
SB
kB
�B
0�B
B'B
M�B
j�B
Z�B
qAB
m�B
� B
��B
�OB
��B
�B
�6B
�kB
��B
�B
��B
�4B
�B
��B
��B
��B
��B
�B
��B
�B
�1B
��B
��B
�YB
{�B
v�B
kB
d�B
aB
_pB
^jB
[WB
W
B
]/B
l�B
VmB
RTB
W?B
_pB
b�B
u�B
ZQB
[�B
]�B
X�B
`�B
tTB
tTB
�~B
�B
��B
�~B
�lB
�GB
��B
��B
�oB
�B
��B
��B
��B
�B
�B
�B
��B
�B
��B
��B
�0B
�CB
��B$@B.�B_pB[WB\)B>�B9�B1[B�BBB�B
��B3�BT,Be�B�B��B�B�B��B��B�\B{�Bm�Be�Bc�B_�B^B]�B^BYBW?BR�BN<BP�BH�BF?BK�BM6B:�B&�BYBoB
��B  BB)�B�B�B�B�B{BGB\B
=B
�B
�B
چB
�B
��B
�;B
�gB
�KB
�OB
�'B
�fB
�B
�MB
cB
��B
{�B
zDB
�B
~(B
��B
c B
b�B
l�B
h�B
j�B
n/B
m]B
~�B
uZB
_B
MjB
a|B
L0B
<�B
7�B
9�B
2�B
6B
.�B
/OB
,=B
*0B
)*B
(�B
,qB
8�B
5?B
49B
@OB
M�B
K�B
V�B
l�B
v�B
g8B
a�B
_�B
c B
V�B
IRB
4B
kB	�2B
YB
iB	�qB	�nB	��B	�B	�|B	�.B
GB
eB
�B	�oB
�B	�B	�jB	��B	�UB	�OB	��B	��B	��B	��B	�B	��B	��B	��B	�1B	�B	��B	�B	�SB	s�B	e`B	d�B	d�B	hsB	n�B	�B	qvB	p�B	]�B	X�B	ZB	N�B	J�B	I�B	I�B	P�B	]�B	=qB	49B	.IB	0�B	1�B	/�B	-B	6�B	1'B	.}B	,B	*�B	.}B	,qB	-wB	/OB	$@B	"�B	#nB	$@B	$tB	#B	"hB	!�B	�B	 �B	xB	"�B	 �B	!�B	FB	B	FB	�B	�B	
	B	SB	&LB�ZB��B��B�>B�mB�TB��B�&B�B�BچBߤB��BԕB��B�B�yBںB�}B��B��B�BیB��B�*B��B��B��B��B��B��B�$B��B�FB��B�3B��B��B��B�eB��B��B��B��B��B��B��B��B�?B��B�9B��B��B��B��B��B��B�nB��B�nB��B�-B��B�9B�zB��B��B�FB��B�aB��B��B��B�0B�zB�B�4B�RB��B��B��B�!B�B��B��B�B��B�OB�IB�B��B�B�B�?B��B�BBÖB�BÖB�9B� B� B��B�BɆB�pB�B�`B�B�B�B�B�+B	 4B��B	�B	B	�B	�B	B	B	�B	\B	�B	�B	B	�B	hB	4B	�B	�B	�B	CB	qB	xB	�B	CB	xB	�B		B	=B	!bB	CB	�B	"�B	#:B	!�B	1B	kB	�B	8B	"B	"B	�B	�B	bB	!�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      B�1B�1B�KB��B�1B��B��B�B��B��B�1B��B��B��B��B��B�lB��B�	B�	B�=B�B��B�OB��B��B��B�oB��B�+B��B�>B��B��BĶB�?B�~B	��B
X�B
��B
��B
�B
y�B
tB
cB
��B
�OB
�KB;�B�B�dB�{BkB?�B%FBEB
��B
�tB
�B
NVB
/OB
B�B
HB
I�B	�=B
1B	��B	�~B	��B	t�B	t�B	X+B	=B	3MB	,qB	&fB	&�B	&fB	�B�B��B�TB�_B��B��B�B�B�$B�xB�LB��B��B�-B��B�mB�B��B��B	B	�B	,B	/B	!�B	(�B	$ZB	+B	#:B	^B	u?B	�;B	�vB	�VB	��B	��B	�vB	�?B	��B	��B	�B

�B
 B
+6B
3�B
/�B
1�B
:DB
C�B
L�B
KDB
L~B
O(B
S�B
UB
WsB
YKB
X_B
W?B
V�B
VB
U�B
U�B
VB
V�B
U�B
VB
V�B
WYB
WYB
VB
U2B
U2B
U�B
U�B
X�B
T�B
T�B
T,B
RB
Q�B
Q�B
Q�B
Q�B
P�B
PB
O\B
O�B
PHB
P�B
Q�B
Q B
QB
Q4B
O�B
OBB
OB
N�B
N�B
L�B
M6B
L�B
L�B
K�B
K�B
KxB
J�B
I�B
G+B
F�B
F�B
G+B
F�B
H�B
F�B
E�B
E�B
E�B
E9B
EB
E9B
E9B
GB
E9B
DgB
EB
C�B
D�B
E�B
A�B
A�B
A;B
B'B
@ B
>�B
?}B
>wB
=�B
=�B
=qB
>B
>BB
9rB
8lB
8B
7�B
5?B
3�B
33B
3MB
2aB
2|B
33B
.B
-wB
-wB
+�B
,WB
-�B
-�B
-]B
+6B
)_B
*�B
+B
+QB
)�B
*B
*0B
)yB
)yB
)�B
)�B
(>B
'B
'B
'�B
'8B
&LB
%�B
&LB
%�B
$tB
$�B
"hB
!�B
!HB
!-B
�B
 'B
OB
�B
)B
�B
�B
B
�B
�B
eB
�B
sB
YB
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
,B
�B
�B
@B
uB
�B
oB
�B
�B
�B
�B
hB
NB
�B
�B
�B
�B
 B
bB
�B
�B
vB
�B
bB
�B
�B
\B
�B
�B
�B
pB
�B
�B
�B
�B
�B
jB
�B
jB
jB
6B
6B
B
�B
B
B
�B
�B
�B
B
�B
�B
B
dB
pB
PB
�B
B
"B
PB
6B
6B
PB
jB
B
�B
"B
�B
�B
�B
�B
(B
"B
�B
�B
B
�B
<B
pB
<B
<B
<B
�B
�B
�B
HB
�B
vB
�B
�B
�B
VB
�B
�B
�B
�B
�B
(B
BB
.B
B
HB
B
HB
�B
�B
.B
}B
B
�B
�B
4B
 B
B
TB
�B
�B
TB
B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
SB
2B
gB
�B
$B
B
B
SB
�B
mB
+B
�B
B
+B
yB
�B
B
B
�B
B
1B
eB
KB
B
�B
WB
QB
�B
�B
�B
�B
qB
qB
�B
qB
CB
�B
CB
]B
�B
B
�B
�B
B
�B
OB
�B
B
OB
�B
�B
�B
�B
 'B
 B
�B
 �B
 'B
 BB
�B
�B
 �B
"�B
#�B
#�B
$�B
%`B
$@B
"�B
# B
$&B
#�B
"�B
#nB
$@B
$&B
$�B
%�B
%�B
&�B
&�B
&�B
'RB
'�B
(
B
($B
(
B
(�B
(�B
)�B
*�B
*�B
*�B
*�B
+B
+kB
+QB
+�B
,B
,B
-)B
,B
+�B
,WB
,�B
-CB
-)B
-wB
.B
.�B
/5B
/B
/5B
/�B
/OB
/�B
/�B
/�B
0B
0�B
1B
1�B
1'B
1AB
1[B
1�B
2GB
33B
33B
2�B
2�B
3�B
3�B
3�B
3�B
4B
49B
49B
4B
4�B
49B
4�B
4TB
3�B
4�B
5ZB
5B
4B
5�B
4�B
4nB
4�B
5�B
72B
7B
7�B
8B
8B
8B
88B
88B
7�B
8B
8�B
9�B
88B
7�B
8B
88B
8�B
9	B
9$B
9�B
:*B
:^B
:*B
:B
:DB
:DB
:�B
;B
;0B
<B
;�B
;�B
<B
;�B
<PB
<B
<�B
=<B
="B
=<B
=�B
>�B
>�B
>]B
>�B
>�B
?cB
?�B
?�B
?�B
@B
?�B
@ B
@OB
@ B
@OB
@B
@OB
AUB
B�B
A�B
BuB
B�B
C�B
C�B
DB
C�B
C�B
DMB
D�B
D3B
DgB
D�B
D�B
D�B
E9B
F%B
EB
EB
D�B
EmB
E�B
E�B
EmB
E�B
FtB
F�B
GzB
G+B
G+B
G_B
G�B
G�B
G�B
G�B
HB
H�B
HfB
HfB
IB
I7B
I�B
I�B
I�B
JXB
JrB
J�B
J�B
J�B
K)B
KDB
K^B
K^B
KxB
KxB
K�B
K�B
K�B
LdB
LdB
MB
M�B
M�B
M�B
M�B
M�B
M�B
NB
N<B
NpB
N<B
N"B
NVB
NpB
N<B
N<B
NVB
NpB
OvB
OB
N�B
OB
O�B
O�B
O�B
O�B
O�B
O�B
P.B
PB
PHB
P�B
Q�B
QhB
Q�B
R�B
S@B
S�B
S�B
S�B
TB
T�B
T�B
U2B
T�B
UB
U�B
UgB
UgB
U�B
U�B
W$B
W�B
XB
W�B
W�B
W�B
W�B
W�B
W�B
XB
XEB
YB
YeB
Y�B
Y�B
Y�B
Y�B
YeB
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[=B
[qB
[�B
\xB
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
^OB
^�B
^�B
_!B
_pB
_�B
`B
`B
`B
`'B
`\B
`BB
`�B
aHB
`�B
a-B
a�B
a�B
bNB
b�B
b�B
b�B
c:B
cnB
cnB
c�B
cnB
c�B
c�B
dtB
eB
d�B
eB
eFB
e�B
ezB
ezB
e�B
ffB
f�B
ffB
gmB
g�B
g�B
g�B
g�B
h
B
hXB
hXB
hsB
h�B
iyB
i�B
jB
j�B
j�B
kB
j�B
jKB
j�B
jeB
j�B
j�B
j�B
j�B
j�B
k6B
kkB
k�B
lB
l"B
l�B
mCB
n/B
n�B
oB
o5B
o�B
p�B
p�B
p�B
p�B
q�B
rB
rB
rGB
r|B
rGB
r�B
r�B
r�B
s3B
shB
tTB
tTB
t9B
t�B
t�B
tTB
t�B
t9B
t�B
t�B
uB
t�B
t�B
t�B
uZB
utB
utB
u�B
utB
u�B
vB
vFB
w�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
y$B
y$B
x�B
x�B
yrB
x�B
x�B
y$B
x�B
x�B
x�B
x�B
yrB
yrB
y�B
y�B
zxB
z�B
z�B
z�B
{B
{JB
{�B
{�B
{�B
{�B
{�B
|B
{�B
|�B
|�B
}VB
}VB
}�B
}qB
}�B
}�B
~B
}�B
~wB
~wB
~�B
~�B
~�B
~�B
~�B
�B
cB
�B
�B
�B
��B
� B
�iB
�4B
� B
�B
�B
�B
�B
��B
��B
��B
�oB
��B
��B
��B
�[B
�B
�uB
��B
�-B
�B
�-B
�-B
�GB
�{B
��B
�3B
�B
��B
�3B
�gB
��B
��B
��B
��B
��B
��B
��B
�9B
��B
��B
��B
�%B
�?B
�tB
��B
��B
��B
�%B
��G�O�B��B��B�lB��B�_B��B�7B�1B��B�fB�B��B�+B�_B�7B��B�_B��B��B��B��B��B�B��B��B�+B��B��B��B��B�_B�fB��B�fB�_B��B��B��B��B�_B��B�1B��B��B�fB�B��B��B�1B��B�fB��B��B�B�1B��B��B��B��B��B��B��B�B��B��B��B��B�fB��B��B��B��B�+B�_B��B��B��B�+B�fB�7B�1B�+B�1B�rB�	B�1B�fB�=B��B��B��B��B��B�	B�B�lB�B��B�lB�lB�rB��B�B�B�=B��B�rB��B�=B��B��B�=B�7B�=B�B��B�7B��B��B�~B�B��B�\B�@B�FB�B�MB�SB�YB�+B�=B�=B�~B�VB�B��B��B��B��B��B�LB��B��B��B�B�B�B�B��B�CB��B�wB�B��B�OB�}B�IB��B��B��B��B��B��B�B��B��B�B�B�B�?B�tB��B��B��B�?B�B��B�B�nB�?B��B�RB��B�RB��B�$B��B�jB��B��B��B��B�B�HB��B��B��B��B�OB��B��B��BÖB��B��B�UB��B�-B�gBÖB��B��B�'B�gB��B��B�[B��B��B�gB�5B��B�B�BeB&�B(�B-B49B3hB8�B@�BDgBG�BEmBG�BD�BH�BI�BL0BJ�BK)BIRBIRBI�BI�BL0BGBMjBL�BNpBO�BO�BOvBN�BN�BIRBN<BN�BN<BM�BN�BN�BN�BN�BN<BM�BNpBOvBP}BP}BR BP�BTaBT�BS&BS�BQ�B^�BjBc BcTBd�Bg8BiDBiBh�Bi�Bo�B�4B��B�eB��B��B��B�yB	B	@B	8�B	m]B	��B	�B	�[B	��B	�zB	��B	�dB	�<B	҉B	�[B	�[B	��B	��B	�B	��B	�fB
�B
VB
oB
�B
�B
SB
kB
�B
0�B
B'B
M�B
j�B
Z�B
qAB
m�B
� B
��B
�OB
��B
�B
�6B
�kB
��B
�B
��B
�4B
�B
��B
��B
��B
��B
�B
��B
�B
�1B
��B
��B
�YB
{�B
v�B
kB
d�B
aB
_pB
^jB
[WB
W
B
]/B
l�B
VmB
RTB
W?B
_pB
b�B
u�B
ZQB
[�B
]�B
X�B
`�B
tTB
tTB
�~B
�B
��B
�~B
�lB
�GB
��B
��B
�oB
�B
��B
��B
��B
�B
�B
�B
��B
�B
��B
��B
�0B
�CB
��B$@B.�B_pB[WB\)B>�B9�B1[B�BBB�B
��B3�BT,Be�B�B��B�B�B��B��B�\B{�Bm�Be�Bc�B_�B^B]�B^BYBW?BR�BN<BP�BH�BF?BK�BM6B:�B&�BYBoB
��B  BB)�B�B�B�B�B{BGB\B
=B
�B
�B
چB
�B
��B
�;B
�gB
�KB
�OB
�'B
�fB
�B
�MB
cB
��B
{�B
zDB
�B
~(B
��B
c B
b�B
l�B
h�B
j�B
n/B
m]B
~�B
uZB
_B
MjB
a|B
L0B
<�B
7�B
9�B
2�B
6B
.�B
/OB
,=B
*0B
)*B
(�B
,qB
8�B
5?B
49B
@OB
M�B
K�B
V�B
l�B
v�B
g8B
a�B
_�B
c B
V�B
IRB
4B
kB	�2B
YB
iB	�qB	�nB	��B	�B	�|B	�.B
GB
eB
�B	�oB
�B	�B	�jB	��B	�UB	�OB	��B	��B	��B	��B	�B	��B	��B	��B	�1B	�B	��B	�B	�SB	s�B	e`B	d�B	d�B	hsB	n�B	�B	qvB	p�B	]�B	X�B	ZB	N�B	J�B	I�B	I�B	P�B	]�B	=qB	49B	.IB	0�B	1�B	/�B	-B	6�B	1'B	.}B	,B	*�B	.}B	,qB	-wB	/OB	$@B	"�B	#nB	$@B	$tB	#B	"hB	!�B	�B	 �B	xB	"�B	 �B	!�B	FB	B	FB	�B	�B	
	B	SB	&LB�ZB��B��B�>B�mB�TB��B�&B�B�BچBߤB��BԕB��B�B�yBںB�}B��B��B�BیB��B�*B��B��B��B��B��B��B�$B��B�FB��B�3B��B��B��B�eB��B��B��B��B��B��B��B��B�?B��B�9B��B��B��B��B��B��B�nB��B�nB��B�-B��B�9B�zB��B��B�FB��B�aB��B��B��B�0B�zB�B�4B�RB��B��B��B�!B�B��B��B�B��B�OB�IB�B��B�B�B�?B��B�BBÖB�BÖB�9B� B� B��B�BɆB�pB�B�`B�B�B�B�B�+B	 4B��B	�B	B	�B	�B	B	B	�B	\B	�B	�B	B	�B	hB	4B	�B	�B	�B	CB	qB	xB	�B	CB	xB	�B		B	=B	!bB	CB	�B	"�B	#:B	!�B	1B	kB	�B	8B	"B	"B	�B	�B	bB	!�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�>2=<hT<�t�=�<#�
<�L<���<�c�<�3"<��<�#<#�
<#�
<t�<#�
<���<#�
<�!�<��<<�o<���<M��<G{E<���<#�
<#�
<#�
<�D�<�!�<'�<��I<��{<#�
<,��<Z��<#�
<'�<#�
<#�
<#�
<#�
<#�
<�0�<1WX<#�
<��{<���<T5�<X�c<6�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018122719113220181227191132IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010420190106200104QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010420190106200104QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551320190521075513IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                