CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-06-14T12:07:55Z creation; 2023-04-26T19:14:28Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20190614120755  20230426191428  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               4   4AA  AOAO7316_008644_052                 7316_008644_052                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�ŝ6P&@�ŝ6P&11  @�ŝ���@�ŝ���@*f����@*f�����c�$�6e�c�$�6e11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�\@B�\@�  @�G�@�  @޸RA ��A  A   A,(�A?\)A`  A\)A��A��A��A��A�  A�  A�A��B  B(�B(�B (�B((�B0(�B7�
B?�BG�BP  BX(�B_�
Bg�
Bo�
Bx  B�{B�{B�  B�{B�(�B�{B�  B��
B��B��B�  B�{B�  B��B��B��B��B��B��B�  B�ffB��B��
B��B��B��B�  B��B��B��B��B�{C 
=C  C
=C  C  C
  C
=C
=C  C
=C  C�C��C��C��C��C��C!��C$
=C&
=C(  C)��C,  C-��C0  C2{C4
=C6  C7��C:  C<
=C=��C?��CB
=CD
=CF
=CH  CI��CK��CN  CP
=CR
=CT
=CU��CW�CY��C[��C^  C`
=Cb  Cc��Cf
=Ch
=Ci��Cl  Cn
=Co��Cq��Ct  Cv  Cw�Cy��C{��C~  C��C���C�  C���C���C�  C���C���C�C�  C��C�  C�  C���C�C�  C�  C�  C�  C�  C�C�
=C�C�  C���C���C���C��C���C�C�  C�  C�  C�  C�  C�C�
=C�\C�C���C���C�  C�  C�C�
=C�
=C�  C���C�  C�C�
=C�C���C�C�
=C�C���C�  C�C�  C�  C�
=C�C�  C���C���C���C���C�C���C���C���C���C�  C�  C�C�C�
=C�C�  C���C���C���C�  C�
=C�C�  C���C���C���C�  C�
=C�C�  C���C���C���C���C���C���C�C�C�  C�  C�  C�
=C�C�  C�  C�C�
=C�
=C�  C���C�  C�C�C�C�  C���C�C�  C�  C���C���C�  C�C�C���D � D  D� D�D� D�qD� DD��D  Dz�D  D�D�D��D  D� D�qD	}qD
  D
� D
��Dz�D  D��D�qD� D�D�D  Dz�D�qD� D�qDxRD��D}qD  D��D  Dz�D�qD��DD� D�qD� DD� D�D��DD� D  D� D  D}qD  D��D  D� D�qD}qD�qD � D!�D!� D"�D"� D"�qD#� D$�D$��D%�D%� D&  D&��D'  D'� D(  D(� D)  D)� D*  D*� D+�D+� D+�qD,� D-  D-� D.  D.� D/�D/��D0�D0� D1  D1z�D1�qD2}qD2�qD3}qD3�qD4� D5  D5��D6  D6��D7�D7� D8  D8� D9  D9��D:�D:��D;�D;� D;�qD<� D=  D=}qD>  D>��D?�D?��D@�D@� DA  DA}qDB�DB}qDB�qDC��DC�qDDz�DE  DE��DF  DF��DG  DG� DH  DH� DH�qDI}qDI�qDJ� DJ�qDK}qDK�qDL}qDM  DM��DNDN� DO  DO��DP�DP��DQ  DQ� DQ�qDR}qDS  DS�DTDT� DU  DU��DU�qDVz�DV�qDW}qDX  DX��DY  DYz�DY�qDZ� D[  D[� D[�qD\� D]�D]��D^�D^� D_  D_}qD`  D`� Da  Da��Db�Db��DcDc� Dc�qDd� De�De}qDe��Df}qDg  Dg� Dh  Dh� Di  Di� Dj�Dj��Dk�Dk��Dl�Dl��Dm  Dmz�Dn  Dn��Do�Do��Dp�Dp� Dp�qDq� Dr  Dr}qDr�qDs��Dt�Dt��DuDu��Du�qDv� Dw�Dw}qDw�qDx}qDy�Dy��Dz  Dz��D{�D{��D|  D|}qD}  D}�D~�D~}qD~�qD��D�HD�@ D���D��HD�  D�@ D�}qD��qD���D�>�D�� D�� D�HD�@ D��HD�� D���D�>�D�~�D�� D��D�AHD�� D�� D�  D�@ D�~�D���D�HD�@ D�~�D�� D�  D�AHD���D��HD���D�@ D��HD��HD�  D�@ D�� D��HD��D�@ D�� D�� D�  D�B�D�� D���D�  D�@ D�~�D��HD��D�AHD��HD���D��)D�=qD�}qD���D�  D�AHD��HD���D��qD�@ D��HD�D�HD�>�D�}qD���D�HD�AHD�~�D���D�  D�@ D��HD��HD�HD�@ D�� D��HD�  D�=qD�� D��HD�  D�=qD�~�D��HD�  D�AHD���D��HD�  D�>�D�~�D�� D���D�AHD���D��HD�HD�B�D��HD��HD�HD�@ D��HD��HD�  D�>�D��HD��HD��qD�=qD�~�D���D���D�>�D��HD�� D��qD�AHD�� D��qD��qD�@ D�� D�� D�  D�<)D�~�D��HD��D�AHD��HD�� D���D�>�D�~�D���D���D�=qD�� D�� D�  D�B�D���D�D��D�@ D�~�D��HD�HD�AHD�� D���D�  D�AHD��HD�� D�  D�>�D�}qD���D�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D���D���D�>�D�~�D�� D�HD�>�D�|)D��qD���D�@ D�� D���D��qD�>�D�� D�� D�HD�>�D�}qD��qD���D�AHD���D��HD���D�@ D��HD��HD�  D�>�D�}qD���D�  D�@ D�� D���D��qD�>�D��HD�� D�  D�AHD��HD�� D�  D�@ D��HD��HD�  D�@ D�~�D���D��qD�@ D��HD�� D�  D�@ D��HD�� D���D�>�D�~�D���D�  D�@ D��HD��HD�HD�@ D�� D�� D�  D�@ D�� D���D�HD�B�D D¾�D�  D�@ DÀ Dþ�D���D�@ DĀ D��HD�HD�AHDŀ D�� D�  D�@ D�~�Dƾ�D�  D�AHDǀ D�� D�HD�@ DȀ D�� D�  D�@ Dɀ Dɾ�D���D�>�D�}qDʽqD���D�@ Dˀ D��HD�HD�AHD́HD�� D�HD�AHD�~�D�� D�  D�>�D�~�D�� D�  D�>�Dπ D��HD�  D�@ DЁHD�� D���D�@ DсHD��HD�  D�@ DҀ D�� D���D�@ DӀ D��HD�HD�@ DԁHD�D�HD�@ D�~�DսqD���D�@ Dր D�� D�  D�AHDׁHD��HD�HD�AHD�~�D�� D�HD�>�D�~�D�� D�HD�>�D�}qDھ�D�HD�@ Dۀ D��HD�HD�B�D܁HD�� D�HD�@ D�~�D��HD�  D�>�Dހ D��HD��D�AHD߀ D�� D�  D�@ D�� D�� D�  D�>�D� D�� D���D�@ D� D�� D�  D�>�D�~�D㾸D���D�@ D� D侸D��qD�>�D�~�D�� D�  D�@ D�HD�� D���D�@ D�~�D羸D�  D�>�D�}qD辸D�HD�AHD� D�qD���D�>�D�~�D��HD�HD�>�D�}qD�qD��qD�AHD삏D��HD���D�@ D�HD�� D�HD�@ D�~�D��HD�HD�@ D�HD��HD���D�>�D�� D�� D�HD�@ D�~�D�D�  D�AHD�~�D�D���D�AHD�HD�D�  D�@ D� D��HD�  D�AHD�� D���D���D�>�D�� D��HD�HD�@ D�~�D�� D���D�@ D���D��HD�  D�AHD��HD��HD�HD�'�D�c�>�?.{?aG�?�\)?�33?���?��@
=q@(�@+�@@  @Q�@^�R@s33@��
@��@�z�@�p�@���@�\)@�Q�@��
@˅@�33@�p�@�ff@�{@�
=A ��A�A��Ap�A�\AffA=qA�RA#33A'
=A+�A0  A4z�A8Q�A<��AA�AEAJ=qAN�RAS�
AXQ�A\(�AaG�AfffAj=qAn�RAs33Aw
=Az�HA�  A�=qA��
A�ffA�Q�A�=qA�z�A��RA���A��\A���A��RA���A��HA��A��RA���A�33A��A�
=A���A��
A�p�A��A��A��A�A�Q�A�=qA�(�A�ffA���A\A���AǮA�G�A˅A�A�Q�A��A�(�AָRA���Aڏ\A�p�A߮AᙚA�A�{A�Q�A��A�z�A�RA���A�\A��A��A���A�33A�B   B�B�B�HBQ�BG�B=qB33B��B	B
�RB�
BG�BffB\)B��B{B
=BQ�B��B
=B  BG�B�RB�
B��B{B�B ��B!�B#\)B$��B%B'
=B(z�B)B+
=B,Q�B-B/33B0��B1B3
=B4��B6{B7
=B8z�B9�B;\)B<z�B=�B?\)B@��BA�BC33BD��BF{BG\)BH��BJ{BK\)BL��BM�BO�BP��BQ�BS�BT��BU�BW\)BX��BZ=qB[�B\��B^=qB_�B`��Ba�Bc�Bd��Bf=qBg\)Bh��Bj=qBk�Bl��Bn{Bo�Bp��Br{Bs�Bt��BvffBw�Bx��Bz=qB{�
B}G�B~=qB�
B���B�G�B��B���B�\)B�  B��RB�p�B�(�B��HB�p�B�{B��HB���B�=qB��HB��B�Q�B���B��B�Q�B�
=B���B�=qB���B��B�=qB��HB���B�Q�B��HB��B�=qB���B��B�{B���B�p�B�{B���B�33B��B��\B��B��B�Q�B�
=B��B�=qB���B��B�(�B���B�\)B�  B���B�\)B�(�B���B�p�B�(�B��HB���B�Q�B�
=B�B�ffB�33B��B���B�G�B��B���B�p�B�(�B��HB��B�=qB���B��B�z�B�G�B�  B��RB�p�B�{B���B���B�Q�B��B�B�z�B��B��
B��\B�\)B�{B¸RB�\)B�  BĸRB�p�B�(�B���BǮB�ffB��BɮB�Q�B���B˙�B�=qB��HB�p�B��
B�=qBΏ\B���B�G�Bϙ�B��B�{B�Q�B�ffB�z�BУ�B���B�
=B��B�33B�\)BхBѮB��B�(�B�Q�B�z�Bң�B���B���B��B�G�BӅB�B�  B�=qB�ffBԣ�B���B���B�33B�\)Bՙ�B�B�  B�=qB�z�B���B�
=B�G�B�p�B׮B��B�(�B�ffBأ�B���B�G�BمB��
B�{B�Q�Bڏ\B���B�
=B�\)Bۙ�B��B�=qBܣ�B��HB��B�p�B�B�  B�Q�Bޣ�B�
=B�\)B߮B�{B�z�B���B��B�p�B�B�  B�ffB�RB��B�p�B�B�(�B�\B���B�\)B�B�  B�Q�B��B�
=B�\)B�B�(�B��B���B�\)B�B�(�B�\B���B�G�B�B�(�B�z�B���B�\)B�B�(�B��B�
=B�p�B��B�Q�B���B�33B�B�{B�z�B��HB�\)B�B�{B�\B���B�\)B�B�=qB���B�
=B��B��B�ffB���B�33B���B�{B�z�B���B�\)B�B�=qB���B��B�p�B��B�Q�B��RB�33B���C   C =qC ffC ��C �
C{CG�Cz�C�C�C�CQ�C�\CC  C=qCp�C��C�
C{CG�Cz�C�RC�C(�C\)C�\CC��C33CffC��C�
C{C=qCz�C�C�C�C\)C�\CC��C	(�C	ffC	��C	��C
  C
=qC
p�C
�C
�HC{CG�C�C�RC�C�C\)C�\C��C  C33Cp�C��C�
C{CG�C�C�RC��C33Cp�C��C�C�C\)C��C��C
=C=qC�C�RC  C=qCz�CC  C=qC�CC
=CG�C�CC
=CG�C�CC  CG�C�CC  CG�Cz�CC  C=qC�C��C
=CQ�C�\C�
C�CffC��C�C(�CffC��C�C33Cz�CC
=CQ�C�\C�
C{C\)C��C�C33C�CC 
=C G�C �\C ��C!{C!ffC!�C!�C"33C"z�C"�C"�C#(�C#ffC#�C#�C$33C$p�C$�C$�C%(�C%\)C%��C%��C&{C&\)C&��C&�
C'{C'Q�C'�C'C(
=C(Q�C(�\C(��C)
=C)G�C)�C)��C*{C*\)C*��C*�C+(�C+p�C+�C+��C,=qC,�\C,�
C-�C-\)C-��C-�HC.(�C.p�C.�RC/  C/G�C/z�C/C0{C0\)C0��C0�C133C1p�C1�C2  C2G�C2��C2�HC3�C3ffC3�C3��C4=qC4�\C4�
C5{C5\)C5��C5�C6=qC6�C6C7  C7G�C7�\C7�HC833C8z�C8C9  C9=qC9�C9�
C:�C:p�C:�RC;  C;=qC;�C;��C<�C<p�C<�RC=  C==qC=�C=C>{C>ffC>�RC?  C?=qC?�C?��C@{C@ffC@�RC@��CAG�CA�CA��CB�CBffCB�RCB��CC=qCC�CC�
CD�CD\)CD��CE  CEG�CE�\CE��CF�CFp�CF�CF��CGG�CG��CG�HCH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            ?�=q@�\@B�\@�  @�G�@�  @޸RA ��A  A   A,(�A?\)A`  A\)A��A��A��A��A�  A�  A�A��B  B(�B(�B (�B((�B0(�B7�
B?�BG�BP  BX(�B_�
Bg�
Bo�
Bx  B�{B�{B�  B�{B�(�B�{B�  B��
B��B��B�  B�{B�  B��B��B��B��B��B��B�  B�ffB��B��
B��B��B��B�  B��B��B��B��B�{C 
=C  C
=C  C  C
  C
=C
=C  C
=C  C�C��C��C��C��C��C!��C$
=C&
=C(  C)��C,  C-��C0  C2{C4
=C6  C7��C:  C<
=C=��C?��CB
=CD
=CF
=CH  CI��CK��CN  CP
=CR
=CT
=CU��CW�CY��C[��C^  C`
=Cb  Cc��Cf
=Ch
=Ci��Cl  Cn
=Co��Cq��Ct  Cv  Cw�Cy��C{��C~  C��C���C�  C���C���C�  C���C���C�C�  C��C�  C�  C���C�C�  C�  C�  C�  C�  C�C�
=C�C�  C���C���C���C��C���C�C�  C�  C�  C�  C�  C�C�
=C�\C�C���C���C�  C�  C�C�
=C�
=C�  C���C�  C�C�
=C�C���C�C�
=C�C���C�  C�C�  C�  C�
=C�C�  C���C���C���C���C�C���C���C���C���C�  C�  C�C�C�
=C�C�  C���C���C���C�  C�
=C�C�  C���C���C���C�  C�
=C�C�  C���C���C���C���C���C���C�C�C�  C�  C�  C�
=C�C�  C�  C�C�
=C�
=C�  C���C�  C�C�C�C�  C���C�C�  C�  C���C���C�  C�C�C���D � D  D� D�D� D�qD� DD��D  Dz�D  D�D�D��D  D� D�qD	}qD
  D
� D
��Dz�D  D��D�qD� D�D�D  Dz�D�qD� D�qDxRD��D}qD  D��D  Dz�D�qD��DD� D�qD� DD� D�D��DD� D  D� D  D}qD  D��D  D� D�qD}qD�qD � D!�D!� D"�D"� D"�qD#� D$�D$��D%�D%� D&  D&��D'  D'� D(  D(� D)  D)� D*  D*� D+�D+� D+�qD,� D-  D-� D.  D.� D/�D/��D0�D0� D1  D1z�D1�qD2}qD2�qD3}qD3�qD4� D5  D5��D6  D6��D7�D7� D8  D8� D9  D9��D:�D:��D;�D;� D;�qD<� D=  D=}qD>  D>��D?�D?��D@�D@� DA  DA}qDB�DB}qDB�qDC��DC�qDDz�DE  DE��DF  DF��DG  DG� DH  DH� DH�qDI}qDI�qDJ� DJ�qDK}qDK�qDL}qDM  DM��DNDN� DO  DO��DP�DP��DQ  DQ� DQ�qDR}qDS  DS�DTDT� DU  DU��DU�qDVz�DV�qDW}qDX  DX��DY  DYz�DY�qDZ� D[  D[� D[�qD\� D]�D]��D^�D^� D_  D_}qD`  D`� Da  Da��Db�Db��DcDc� Dc�qDd� De�De}qDe��Df}qDg  Dg� Dh  Dh� Di  Di� Dj�Dj��Dk�Dk��Dl�Dl��Dm  Dmz�Dn  Dn��Do�Do��Dp�Dp� Dp�qDq� Dr  Dr}qDr�qDs��Dt�Dt��DuDu��Du�qDv� Dw�Dw}qDw�qDx}qDy�Dy��Dz  Dz��D{�D{��D|  D|}qD}  D}�D~�D~}qD~�qD��D�HD�@ D���D��HD�  D�@ D�}qD��qD���D�>�D�� D�� D�HD�@ D��HD�� D���D�>�D�~�D�� D��D�AHD�� D�� D�  D�@ D�~�D���D�HD�@ D�~�D�� D�  D�AHD���D��HD���D�@ D��HD��HD�  D�@ D�� D��HD��D�@ D�� D�� D�  D�B�D�� D���D�  D�@ D�~�D��HD��D�AHD��HD���D��)D�=qD�}qD���D�  D�AHD��HD���D��qD�@ D��HD�D�HD�>�D�}qD���D�HD�AHD�~�D���D�  D�@ D��HD��HD�HD�@ D�� D��HD�  D�=qD�� D��HD�  D�=qD�~�D��HD�  D�AHD���D��HD�  D�>�D�~�D�� D���D�AHD���D��HD�HD�B�D��HD��HD�HD�@ D��HD��HD�  D�>�D��HD��HD��qD�=qD�~�D���D���D�>�D��HD�� D��qD�AHD�� D��qD��qD�@ D�� D�� D�  D�<)D�~�D��HD��D�AHD��HD�� D���D�>�D�~�D���D���D�=qD�� D�� D�  D�B�D���D�D��D�@ D�~�D��HD�HD�AHD�� D���D�  D�AHD��HD�� D�  D�>�D�}qD���D�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D���D���D�>�D�~�D�� D�HD�>�D�|)D��qD���D�@ D�� D���D��qD�>�D�� D�� D�HD�>�D�}qD��qD���D�AHD���D��HD���D�@ D��HD��HD�  D�>�D�}qD���D�  D�@ D�� D���D��qD�>�D��HD�� D�  D�AHD��HD�� D�  D�@ D��HD��HD�  D�@ D�~�D���D��qD�@ D��HD�� D�  D�@ D��HD�� D���D�>�D�~�D���D�  D�@ D��HD��HD�HD�@ D�� D�� D�  D�@ D�� D���D�HD�B�D D¾�D�  D�@ DÀ Dþ�D���D�@ DĀ D��HD�HD�AHDŀ D�� D�  D�@ D�~�Dƾ�D�  D�AHDǀ D�� D�HD�@ DȀ D�� D�  D�@ Dɀ Dɾ�D���D�>�D�}qDʽqD���D�@ Dˀ D��HD�HD�AHD́HD�� D�HD�AHD�~�D�� D�  D�>�D�~�D�� D�  D�>�Dπ D��HD�  D�@ DЁHD�� D���D�@ DсHD��HD�  D�@ DҀ D�� D���D�@ DӀ D��HD�HD�@ DԁHD�D�HD�@ D�~�DսqD���D�@ Dր D�� D�  D�AHDׁHD��HD�HD�AHD�~�D�� D�HD�>�D�~�D�� D�HD�>�D�}qDھ�D�HD�@ Dۀ D��HD�HD�B�D܁HD�� D�HD�@ D�~�D��HD�  D�>�Dހ D��HD��D�AHD߀ D�� D�  D�@ D�� D�� D�  D�>�D� D�� D���D�@ D� D�� D�  D�>�D�~�D㾸D���D�@ D� D侸D��qD�>�D�~�D�� D�  D�@ D�HD�� D���D�@ D�~�D羸D�  D�>�D�}qD辸D�HD�AHD� D�qD���D�>�D�~�D��HD�HD�>�D�}qD�qD��qD�AHD삏D��HD���D�@ D�HD�� D�HD�@ D�~�D��HD�HD�@ D�HD��HD���D�>�D�� D�� D�HD�@ D�~�D�D�  D�AHD�~�D�D���D�AHD�HD�D�  D�@ D� D��HD�  D�AHD�� D���D���D�>�D�� D��HD�HD�@ D�~�D�� D���D�@ D���D��HD�  D�AHD��HD��HD�HD�'�G�O�>�?.{?aG�?�\)?�33?���?��@
=q@(�@+�@@  @Q�@^�R@s33@��
@��@�z�@�p�@���@�\)@�Q�@��
@˅@�33@�p�@�ff@�{@�
=A ��A�A��Ap�A�\AffA=qA�RA#33A'
=A+�A0  A4z�A8Q�A<��AA�AEAJ=qAN�RAS�
AXQ�A\(�AaG�AfffAj=qAn�RAs33Aw
=Az�HA�  A�=qA��
A�ffA�Q�A�=qA�z�A��RA���A��\A���A��RA���A��HA��A��RA���A�33A��A�
=A���A��
A�p�A��A��A��A�A�Q�A�=qA�(�A�ffA���A\A���AǮA�G�A˅A�A�Q�A��A�(�AָRA���Aڏ\A�p�A߮AᙚA�A�{A�Q�A��A�z�A�RA���A�\A��A��A���A�33A�B   B�B�B�HBQ�BG�B=qB33B��B	B
�RB�
BG�BffB\)B��B{B
=BQ�B��B
=B  BG�B�RB�
B��B{B�B ��B!�B#\)B$��B%B'
=B(z�B)B+
=B,Q�B-B/33B0��B1B3
=B4��B6{B7
=B8z�B9�B;\)B<z�B=�B?\)B@��BA�BC33BD��BF{BG\)BH��BJ{BK\)BL��BM�BO�BP��BQ�BS�BT��BU�BW\)BX��BZ=qB[�B\��B^=qB_�B`��Ba�Bc�Bd��Bf=qBg\)Bh��Bj=qBk�Bl��Bn{Bo�Bp��Br{Bs�Bt��BvffBw�Bx��Bz=qB{�
B}G�B~=qB�
B���B�G�B��B���B�\)B�  B��RB�p�B�(�B��HB�p�B�{B��HB���B�=qB��HB��B�Q�B���B��B�Q�B�
=B���B�=qB���B��B�=qB��HB���B�Q�B��HB��B�=qB���B��B�{B���B�p�B�{B���B�33B��B��\B��B��B�Q�B�
=B��B�=qB���B��B�(�B���B�\)B�  B���B�\)B�(�B���B�p�B�(�B��HB���B�Q�B�
=B�B�ffB�33B��B���B�G�B��B���B�p�B�(�B��HB��B�=qB���B��B�z�B�G�B�  B��RB�p�B�{B���B���B�Q�B��B�B�z�B��B��
B��\B�\)B�{B¸RB�\)B�  BĸRB�p�B�(�B���BǮB�ffB��BɮB�Q�B���B˙�B�=qB��HB�p�B��
B�=qBΏ\B���B�G�Bϙ�B��B�{B�Q�B�ffB�z�BУ�B���B�
=B��B�33B�\)BхBѮB��B�(�B�Q�B�z�Bң�B���B���B��B�G�BӅB�B�  B�=qB�ffBԣ�B���B���B�33B�\)Bՙ�B�B�  B�=qB�z�B���B�
=B�G�B�p�B׮B��B�(�B�ffBأ�B���B�G�BمB��
B�{B�Q�Bڏ\B���B�
=B�\)Bۙ�B��B�=qBܣ�B��HB��B�p�B�B�  B�Q�Bޣ�B�
=B�\)B߮B�{B�z�B���B��B�p�B�B�  B�ffB�RB��B�p�B�B�(�B�\B���B�\)B�B�  B�Q�B��B�
=B�\)B�B�(�B��B���B�\)B�B�(�B�\B���B�G�B�B�(�B�z�B���B�\)B�B�(�B��B�
=B�p�B��B�Q�B���B�33B�B�{B�z�B��HB�\)B�B�{B�\B���B�\)B�B�=qB���B�
=B��B��B�ffB���B�33B���B�{B�z�B���B�\)B�B�=qB���B��B�p�B��B�Q�B��RB�33B���C   C =qC ffC ��C �
C{CG�Cz�C�C�C�CQ�C�\CC  C=qCp�C��C�
C{CG�Cz�C�RC�C(�C\)C�\CC��C33CffC��C�
C{C=qCz�C�C�C�C\)C�\CC��C	(�C	ffC	��C	��C
  C
=qC
p�C
�C
�HC{CG�C�C�RC�C�C\)C�\C��C  C33Cp�C��C�
C{CG�C�C�RC��C33Cp�C��C�C�C\)C��C��C
=C=qC�C�RC  C=qCz�CC  C=qC�CC
=CG�C�CC
=CG�C�CC  CG�C�CC  CG�Cz�CC  C=qC�C��C
=CQ�C�\C�
C�CffC��C�C(�CffC��C�C33Cz�CC
=CQ�C�\C�
C{C\)C��C�C33C�CC 
=C G�C �\C ��C!{C!ffC!�C!�C"33C"z�C"�C"�C#(�C#ffC#�C#�C$33C$p�C$�C$�C%(�C%\)C%��C%��C&{C&\)C&��C&�
C'{C'Q�C'�C'C(
=C(Q�C(�\C(��C)
=C)G�C)�C)��C*{C*\)C*��C*�C+(�C+p�C+�C+��C,=qC,�\C,�
C-�C-\)C-��C-�HC.(�C.p�C.�RC/  C/G�C/z�C/C0{C0\)C0��C0�C133C1p�C1�C2  C2G�C2��C2�HC3�C3ffC3�C3��C4=qC4�\C4�
C5{C5\)C5��C5�C6=qC6�C6C7  C7G�C7�\C7�HC833C8z�C8C9  C9=qC9�C9�
C:�C:p�C:�RC;  C;=qC;�C;��C<�C<p�C<�RC=  C==qC=�C=C>{C>ffC>�RC?  C?=qC?�C?��C@{C@ffC@�RC@��CAG�CA�CA��CB�CBffCB�RCB��CC=qCC�CC�
CD�CD\)CD��CE  CEG�CE�\CE��CF�CFp�CF�CF��CGG�CG��CG�HCH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A�1A�1A�1A�1A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�JA�JA�VA�VA�VA�bA�bA�bA�bA�bA�bA�M�A܁A���A�bNAݍPAݍPAݍPA�|�A�ZA݁A�~�A�p�A�bNA�^5A�bNA�ffA�Q�A�S�A�9XA�JA��A��A�n�A�VA�/A�z�A���A�AÏ\A�1A�bA�K�A���A���A���A�A�&�A��#A�/A�M�A���A�$�A���A��A���A��A�-A�`BA��hA���A��/A�|�A��A��A��A���A��^A�1Ax�Au��AoS�Ajr�Ai�Ai%Ae�TA`(�AZ��AQ&�ANv�AM�-ALv�AJ�DAIƨAI
=AH��AHZAHjAH�DAIhsAG��AD�AD^5AC�
A@��A9�mA5�A5/A4I�A1�#A/�A-��A,VA)��A)?}A(E�A'%A&��A&r�A&$�A%��A#�;A#+A#&�A$��A&n�A'l�A'�A%��A$�/A#��A%oA$�A#��A#�FA$bA#�wA#C�A"M�A"9XA"{A!�TA!x�A �Ap�A�A9XA5?A��AffA�A�DAA�A��A;dA�A��AM�A��A�Ax�A��A�+A^5A$�A��A��A�/AZA��Ap�A��A=qA�Ap�A�^A�AbA5?A$�A�;A��AXAn�A�A��AXAoA%AE�A�mA��A?}A"�AA��A�!A�A5?AAA`BA�A�+A~�A �A7LA
��A
��A
ZA	�mA	��A	t�A��A�uA�AA��AffA{AhsA�A��Ar�AbNAM�AbAƨA7LA�yAffA1A�FA ��A �`A ��A ��A Q�A �@��
@��P@�V@��@�x�@��@��@��R@��T@�`B@�Q�@�|�@�n�@�@��T@���@��-@�A�@��@�C�@���@�ȴ@�
=@���@�R@�v�@�J@��T@�@��@��@�(�@�@�33@�
=@���@@��@���@�A�@�\)@�33@�M�@陚@蛦@��@�dZ@�
=@��y@�\@��T@�`B@���@�A�@���@�ȴ@��T@��@��D@�33@ޏ\@�^5@ݡ�@�/@�%@ܣ�@܃@�bN@��
@�"�@�n�@�5?@��@�{@���@�X@���@�  @�C�@�"�@��@֧�@�^5@��@Ցh@��@ԓu@�1@ӝ�@�dZ@��H@���@Ұ!@҇+@�5?@��#@��@���@ЋD@�Q�@��
@�K�@Χ�@�V@���@��@�bN@��
@�l�@��H@ɉ7@�Z@Ǿw@ư!@���@��`@ě�@�z�@�1'@�\)@�
=@���@���@�X@��@��/@��@�dZ@�ȴ@�~�@�=q@���@�?}@�Ĝ@�Z@�b@�|�@���@�~�@�E�@���@�O�@��@���@�b@��w@�t�@��@��R@�=q@��7@�j@��m@��P@��@���@��\@�$�@���@�X@��@�b@��@�K�@��H@���@�E�@���@���@�p�@�X@�/@��@�(�@��w@���@�S�@���@���@�E�@�J@���@�&�@��j@�9X@�b@���@�ƨ@���@��y@���@��!@���@�v�@���@��7@�`B@��@��/@���@�9X@��w@�C�@�@���@��\@�n�@�^5@�M�@�-@���@�x�@�V@���@�Z@�I�@�A�@�1'@�(�@� �@��@�1@��@��P@�ȴ@���@�E�@�{@��^@��7@�&�@��u@�I�@�1@���@��@��R@�~�@�M�@��@���@��@���@��@�7L@���@�j@�9X@�  @�dZ@��@��!@��+@�E�@��#@��^@���@�G�@��j@�z�@�I�@�  @��m@��
@��F@�C�@�@��R@�ff@��@��@���@���@�?}@��`@���@���@��D@�Z@� �@���@���@�t�@�dZ@�o@��H@��R@��+@�ff@�-@���@���@��7@�O�@���@���@�Ĝ@���@�Q�@�b@��;@��F@��P@�dZ@��@�ȴ@��+@�^5@���@���@���@��^@���@�x�@�/@���@�z�@�bN@�9X@��w@�\)@��@��R@�5?@��^@��h@��@�x�@�7L@��`@�9X@��P@�33@��!@��\@�n�@�n�@�^5@���@���@�%@��/@��9@�  @�w@�@K�@+@�@~�y@~��@~$�@~@}`B@|z�@|1@{ƨ@{S�@z�H@z�!@zM�@z=q@zJ@y�^@y&�@x��@xr�@x�@xr�@xA�@w�w@w�P@w\)@w�@v��@v�@v�R@u�T@u?}@t��@tz�@tI�@sƨ@sdZ@so@r��@r�!@r^5@rJ@q��@qx�@qX@q7L@pĜ@p  @o|�@n��@n�@n�@n��@n$�@m�T@m��@m?}@l�j@lz�@l9X@k��@kƨ@k33@j�\@jJ@i��@ix�@i7L@h�@g�;@g�;@g�@g+@f��@fȴ@fff@f@e�@d�@b��@b-@bJ@a��@a�#@a��@a��@a%@`�u@`A�@` �@`b@_�@_
=@^��@^@]��@]��@]��@]��@]@]@]�@\j@[�@[t�@Z��@Z�@ZJ@Y�#@Yx�@X��@X1'@W�@W��@W�P@Vȴ@VV@V@U�-@UO�@T��@T�@S��@S�@S33@R��@R�@Q��@Q�#@Qx�@PĜ@P��@PA�@O�@O;d@N��@Nff@NE�@N{@M��@L�/@L��@L��@L��@L9X@K��@K��@KC�@Ko@J��@I��@I��@IG�@H��@H�9@H�@HQ�@G�@G�@G��@G�P@G\)@G;d@F��@F�@F�R@F�+@FV@E�T@E/@EV@EV@D��@D�@D��@C�@C33@Co@B�!@B^5@B�@A�#@A�@@Ĝ@@r�@@1'@@  @?|�@>�y@>�@>�R@>v�@>ff@>E�@>5?@>@=�T@=�h@=�@<�D@<j@<Z@;��@;dZ@;@:~�@:�@9�#@9�^@9hs@9%@8��@8�@8Q�@8  @7�w@7l�@6�R@6E�@5�T@5�-@5�@5p�@5`B@5`B@5O�@5V@4z�@3C�@2��@2�!@2�\@2^5@2�@1��@1��@0�`@0bN@/�;@/�@/|�@/\)@/K�@.�+@-@-�@-�@-V@-V@,��@,�@,I�@,�@+��@+��@*�@*~�@*n�@*^5@*=q@*-@*�@)�@)��@)�7@)�@(�9@(�@(r�@(bN@(A�@(  @'�;@'�@'\)@'
=@&�@&�R@&�+@&E�@&@%��@%�h@%�@%`B@%V@$�/@$j@#�@#@"��@"��@"��@"��@"�!@"^5@!��@!�7@!G�@!�@!%@ �9@ r�@ bN@ bN@ bN@ A�@�@�P@\)@+@�y@ȴ@�+@E�@$�@{@@�@�T@@�-@�-@��@�h@�@p�@O�@�@�@��@��@��@��@�D@z�@z�@I�@�m@ƨ@��@��@�@t�@S�@S�@C�@C�@33@33@@��@�@�^@��@&�@��@��@bN@ �@�@��@��@�@K�@+@�@
=@��@ȴ@��@��@��@v�@V@$�@{@@�T@�T@��@�-@�@p�@p�@p�@?}@/@�@��@�@��@�@�@�@�D@j@�@�
@��@dZ@"�@��@^5@^5@-@��@�@�^@��@��@��@��@��@�7@�`@�u@bN@bNA��yA��A��A��A���A��mA��A�%A�1A�A�
=A�1A�A�
=A�JA�%A�1A�
=A�%A�1A�JA�JA�1A�
=A�JA�1A�%A�
=A�JA�1A�1A�JA�JA�1A�1A�JA�
=A�%A�1A�JA�1A�1A�JA�JA�
=A�1A�JA�JA�1A�1A�JA�JA�1A�JA�VA�1A�JA�VA�
=A�
=A�bA�JA�
=A�bA�JA�
=A�VA�bA�JA�VA�oA�VA�
=A�bA�VA�JA�JA�bA�VA�
=A�bA�VA�JA�bA�bA�JA�JA�bA�bA�JA�bA�bA�VA�bA�{A�bA�VA�oA�{A�VA�bA�{A�oA�VA�bA�oA�VA�VA�oA�oA�VA�VA�oA�oA�VA�bA�oA�oA�VA�VA�oA�oA�bA�VA�bA�oA�VA�VA�bA�oA�VA�JA�oA�bA�VA��A�Q�A�VA�VA�S�A�VA�VA�^5A�hsA�|�A܉7Aܕ�Aܡ�AܮAܬAܼjA�ĜA���A���A��A�/A�ZA�~�A݇+A݋DAݏ\AݍPA݋DAݏ\AݑhA݋DAݍPAݏ\AݑhAݍPA݋DAݍPAݑhAݍPA݋DAݏ\Aݏ\A݋DA݇+A݇+A݇+A�~�A�p�A�jA�`BA�ZA�XA�XA�XA�bNA�hsA݃A݋DA݉7A݉7AݍPA݇+A�~�A�|�A�~�A�|�A�x�A�z�A�v�A�t�A�jA�dZA�ffA�hsA�bNA�`BA�`BA�dZA�bNA�^5A�\)A�`BA�`BA�\)A�^5A�dZA�bNA�`BA�bNA�ffA�ffA�ffA�dZA�ffA�hsA�dZA�ZA�S�A�S�A�Q�A�M�A�M�A�S�A�S�A�Q�A�Q�A�S�A�Q�A�M�A�K�A�A�A�5?A�(�A�&�A��A��A�A���A�  A�oA�;dA�G�A�ZA�bNA�K�A�l�A�;dA��A�(�A�?}A�XA�ĜA��Aԗ�A�9XA�t�A�ZA�G�AξwA��HA�XA�"�A�
=A���A���A�XA�K�A�E�A�/A��A��yAǥ�AǃA�jA�l�A�XA�M�A�;dA�+A�
=Aƥ�A�l�A���AŁA�bNA��/A��#A���A���AöFA�v�A�M�A�33A�-A�&�A��A���A���A®APA�Q�A�9XA�ĜA�A�A��FA���A�(�A�{A��A��uA�VA�&�A�1A��#A���A�^5A���A���A�|�A�x�A�l�A�9XA��A���A�ƨA��FA���A���A��PA�l�A�O�A��A��A��mA��yA�ĜA�t�A�O�A���A��A�O�A��A��A���A�l�A��A���A���A���A�ĜA��!A���A��DA��+A�p�A�l�A�?}A�+A�%A���A���A�~�A�XA�-A�JA���A�dZA��A���A�VA���A�ffA�bA���A��#A��A�K�A�$�A��A�  A��A��`A���A��jA���A��hA��A�x�A�bNA�A�A�&�A�JA��;A��A�v�A�=qA��A�A���A��A��jA��uA�jA�VA�33A��A���A��DA�l�A�&�A�  A��`A��`A�ĜA���A��A��A�A���A�n�A�9XA��A��7A�=qA���A�oA��A���A�\)A�;dA��jA��A�Q�A��A��A��^A���A��A�M�A��A��A��TA��#A���A�ĜA���A�I�A���A���A��7A�^5A�"�A���A�ȴA��+A�7LA���A��A�=qA���A���A��!A���A��\A��A�jA�ZA�I�A�A�A�5?A� �A���A�A��PA�I�A�ƨA�;dA�
=A��A���A�l�A�1'A��A��A�ȴA��A�O�A�|�A���A��jA���A�~�A�M�A�  A��RA�  A��hA�`BA�VA���A�dZA�1'A��A�{A�bA���A�ȴA�z�A�&�A��A��9A�p�A�A���A�dZA�=qA�(�A�"�A� �A�bA�  A�A�ffA��A��yA���A���A���A��DA�x�A�XA�I�A�5?A��A�A��TA���A�l�A�(�A��A�r�A�&�A��^A�A�A��#A��A�ZA�K�A�C�A�9XA�(�A��;A�-A�dZA�p�A���A�=qA�XA��jA�z�A�5?A��\A��A�bNA��A���A��A���A�z�A��A���A��A�hsA�S�A�-A��9A��FA�I�A��A���A��wA�x�A���A�ĜA�z�A��A/A�A~�A~�RA}\)Az�yAwx�AvQ�Au�7At�uAs��Ar�!Ar�Aq/Ap�\Ao�;Ao+AnZAmC�Ak�Ak;dAj�HAj�uAjv�Aj1'AiAi�hAi�PAi�7Ai�7Ai�7Ai�7Ai�7Ai�Ail�AiXAiC�Ai/Ai+Ai"�Ai
=Ah�jAhffAg�AgK�Af�HAfE�Ae�hAe&�Ad�yAdI�AbĜAa�A`��A`z�A`bA_;dA^��A^�+A^E�A]��A\��A["�AY�AYoAWO�AVJAT�/AS;dAQ�;AO��AOp�AO�AOVAO
=AN��AN�\ANn�ANZANA�ANbAM��AM�AM��AM�FAM��AM��AM�7AMt�AMl�AMXAM�AL��ALn�AK�^AK�AJ��AJ��AJ��AJ�\AJ�DAJ�AJbNAJ(�AJJAI�mAIƨAI��AI�hAI�AI`BAI7LAI�AI
=AH�AH�`AH�HAH��AHȴAH�9AH��AH�AHffAHjAHffAHbNAHVAHZAHQ�AHVAHVAHZAHZAHbNAHn�AHn�AHr�AH~�AH~�AH�+AH�AH�DAH�\AH�\AH�uAH��AH�AI+AIS�AI��AI��AI�AI�AI�AH�+AH$�AG��AGXAF�!AF�AE|�AEVAD�/ADȴAD�RAD�AD��AD�\AD~�ADn�ADQ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            A��A���A�1A�1A�1A�1A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�JA�JA�VA�VA�VA�bA�bA�bA�bA�bA�bA�M�A܁A���A�bNAݍPAݍPAݍPA�|�A�ZA݁A�~�A�p�A�bNA�^5A�bNA�ffA�Q�A�S�A�9XA�JA��A��A�n�A�VA�/A�z�A���A�AÏ\A�1A�bA�K�A���A���A���A�A�&�A��#A�/A�M�A���A�$�A���A��A���A��A�-A�`BA��hA���A��/A�|�A��A��A��A���A��^A�1Ax�Au��AoS�Ajr�Ai�Ai%Ae�TA`(�AZ��AQ&�ANv�AM�-ALv�AJ�DAIƨAI
=AH��AHZAHjAH�DAIhsAG��AD�AD^5AC�
A@��A9�mA5�A5/A4I�A1�#A/�A-��A,VA)��A)?}A(E�A'%A&��A&r�A&$�A%��A#�;A#+A#&�A$��A&n�A'l�A'�A%��A$�/A#��A%oA$�A#��A#�FA$bA#�wA#C�A"M�A"9XA"{A!�TA!x�A �Ap�A�A9XA5?A��AffA�A�DAA�A��A;dA�A��AM�A��A�Ax�A��A�+A^5A$�A��A��A�/AZA��Ap�A��A=qA�Ap�A�^A�AbA5?A$�A�;A��AXAn�A�A��AXAoA%AE�A�mA��A?}A"�AA��A�!A�A5?AAA`BA�A�+A~�A �A7LA
��A
��A
ZA	�mA	��A	t�A��A�uA�AA��AffA{AhsA�A��Ar�AbNAM�AbAƨA7LA�yAffA1A�FA ��A �`A ��A ��A Q�A �@��
@��P@�V@��@�x�@��@��@��R@��T@�`B@�Q�@�|�@�n�@�@��T@���@��-@�A�@��@�C�@���@�ȴ@�
=@���@�R@�v�@�J@��T@�@��@��@�(�@�@�33@�
=@���@@��@���@�A�@�\)@�33@�M�@陚@蛦@��@�dZ@�
=@��y@�\@��T@�`B@���@�A�@���@�ȴ@��T@��@��D@�33@ޏ\@�^5@ݡ�@�/@�%@ܣ�@܃@�bN@��
@�"�@�n�@�5?@��@�{@���@�X@���@�  @�C�@�"�@��@֧�@�^5@��@Ցh@��@ԓu@�1@ӝ�@�dZ@��H@���@Ұ!@҇+@�5?@��#@��@���@ЋD@�Q�@��
@�K�@Χ�@�V@���@��@�bN@��
@�l�@��H@ɉ7@�Z@Ǿw@ư!@���@��`@ě�@�z�@�1'@�\)@�
=@���@���@�X@��@��/@��@�dZ@�ȴ@�~�@�=q@���@�?}@�Ĝ@�Z@�b@�|�@���@�~�@�E�@���@�O�@��@���@�b@��w@�t�@��@��R@�=q@��7@�j@��m@��P@��@���@��\@�$�@���@�X@��@�b@��@�K�@��H@���@�E�@���@���@�p�@�X@�/@��@�(�@��w@���@�S�@���@���@�E�@�J@���@�&�@��j@�9X@�b@���@�ƨ@���@��y@���@��!@���@�v�@���@��7@�`B@��@��/@���@�9X@��w@�C�@�@���@��\@�n�@�^5@�M�@�-@���@�x�@�V@���@�Z@�I�@�A�@�1'@�(�@� �@��@�1@��@��P@�ȴ@���@�E�@�{@��^@��7@�&�@��u@�I�@�1@���@��@��R@�~�@�M�@��@���@��@���@��@�7L@���@�j@�9X@�  @�dZ@��@��!@��+@�E�@��#@��^@���@�G�@��j@�z�@�I�@�  @��m@��
@��F@�C�@�@��R@�ff@��@��@���@���@�?}@��`@���@���@��D@�Z@� �@���@���@�t�@�dZ@�o@��H@��R@��+@�ff@�-@���@���@��7@�O�@���@���@�Ĝ@���@�Q�@�b@��;@��F@��P@�dZ@��@�ȴ@��+@�^5@���@���@���@��^@���@�x�@�/@���@�z�@�bN@�9X@��w@�\)@��@��R@�5?@��^@��h@��@�x�@�7L@��`@�9X@��P@�33@��!@��\@�n�@�n�@�^5@���@���@�%@��/@��9@�  @�w@�@K�@+@�@~�y@~��@~$�@~@}`B@|z�@|1@{ƨ@{S�@z�H@z�!@zM�@z=q@zJ@y�^@y&�@x��@xr�@x�@xr�@xA�@w�w@w�P@w\)@w�@v��@v�@v�R@u�T@u?}@t��@tz�@tI�@sƨ@sdZ@so@r��@r�!@r^5@rJ@q��@qx�@qX@q7L@pĜ@p  @o|�@n��@n�@n�@n��@n$�@m�T@m��@m?}@l�j@lz�@l9X@k��@kƨ@k33@j�\@jJ@i��@ix�@i7L@h�@g�;@g�;@g�@g+@f��@fȴ@fff@f@e�@d�@b��@b-@bJ@a��@a�#@a��@a��@a%@`�u@`A�@` �@`b@_�@_
=@^��@^@]��@]��@]��@]��@]@]@]�@\j@[�@[t�@Z��@Z�@ZJ@Y�#@Yx�@X��@X1'@W�@W��@W�P@Vȴ@VV@V@U�-@UO�@T��@T�@S��@S�@S33@R��@R�@Q��@Q�#@Qx�@PĜ@P��@PA�@O�@O;d@N��@Nff@NE�@N{@M��@L�/@L��@L��@L��@L9X@K��@K��@KC�@Ko@J��@I��@I��@IG�@H��@H�9@H�@HQ�@G�@G�@G��@G�P@G\)@G;d@F��@F�@F�R@F�+@FV@E�T@E/@EV@EV@D��@D�@D��@C�@C33@Co@B�!@B^5@B�@A�#@A�@@Ĝ@@r�@@1'@@  @?|�@>�y@>�@>�R@>v�@>ff@>E�@>5?@>@=�T@=�h@=�@<�D@<j@<Z@;��@;dZ@;@:~�@:�@9�#@9�^@9hs@9%@8��@8�@8Q�@8  @7�w@7l�@6�R@6E�@5�T@5�-@5�@5p�@5`B@5`B@5O�@5V@4z�@3C�@2��@2�!@2�\@2^5@2�@1��@1��@0�`@0bN@/�;@/�@/|�@/\)@/K�@.�+@-@-�@-�@-V@-V@,��@,�@,I�@,�@+��@+��@*�@*~�@*n�@*^5@*=q@*-@*�@)�@)��@)�7@)�@(�9@(�@(r�@(bN@(A�@(  @'�;@'�@'\)@'
=@&�@&�R@&�+@&E�@&@%��@%�h@%�@%`B@%V@$�/@$j@#�@#@"��@"��@"��@"��@"�!@"^5@!��@!�7@!G�@!�@!%@ �9@ r�@ bN@ bN@ bN@ A�@�@�P@\)@+@�y@ȴ@�+@E�@$�@{@@�@�T@@�-@�-@��@�h@�@p�@O�@�@�@��@��@��@��@�D@z�@z�@I�@�m@ƨ@��@��@�@t�@S�@S�@C�@C�@33@33@@��@�@�^@��@&�@��@��@bN@ �@�@��@��@�@K�@+@�@
=@��@ȴ@��@��@��@v�@V@$�@{@@�T@�T@��@�-@�@p�@p�@p�@?}@/@�@��@�@��@�@�@�@�D@j@�@�
@��@dZ@"�@��@^5@^5@-@��@�@�^@��@��@��@��@��@�7@�`@�u@bNG�O�A��yA��A��A��A���A��mA��A�%A�1A�A�
=A�1A�A�
=A�JA�%A�1A�
=A�%A�1A�JA�JA�1A�
=A�JA�1A�%A�
=A�JA�1A�1A�JA�JA�1A�1A�JA�
=A�%A�1A�JA�1A�1A�JA�JA�
=A�1A�JA�JA�1A�1A�JA�JA�1A�JA�VA�1A�JA�VA�
=A�
=A�bA�JA�
=A�bA�JA�
=A�VA�bA�JA�VA�oA�VA�
=A�bA�VA�JA�JA�bA�VA�
=A�bA�VA�JA�bA�bA�JA�JA�bA�bA�JA�bA�bA�VA�bA�{A�bA�VA�oA�{A�VA�bA�{A�oA�VA�bA�oA�VA�VA�oA�oA�VA�VA�oA�oA�VA�bA�oA�oA�VA�VA�oA�oA�bA�VA�bA�oA�VA�VA�bA�oA�VA�JA�oA�bA�VA��A�Q�A�VA�VA�S�A�VA�VA�^5A�hsA�|�A܉7Aܕ�Aܡ�AܮAܬAܼjA�ĜA���A���A��A�/A�ZA�~�A݇+A݋DAݏ\AݍPA݋DAݏ\AݑhA݋DAݍPAݏ\AݑhAݍPA݋DAݍPAݑhAݍPA݋DAݏ\Aݏ\A݋DA݇+A݇+A݇+A�~�A�p�A�jA�`BA�ZA�XA�XA�XA�bNA�hsA݃A݋DA݉7A݉7AݍPA݇+A�~�A�|�A�~�A�|�A�x�A�z�A�v�A�t�A�jA�dZA�ffA�hsA�bNA�`BA�`BA�dZA�bNA�^5A�\)A�`BA�`BA�\)A�^5A�dZA�bNA�`BA�bNA�ffA�ffA�ffA�dZA�ffA�hsA�dZA�ZA�S�A�S�A�Q�A�M�A�M�A�S�A�S�A�Q�A�Q�A�S�A�Q�A�M�A�K�A�A�A�5?A�(�A�&�A��A��A�A���A�  A�oA�;dA�G�A�ZA�bNA�K�A�l�A�;dA��A�(�A�?}A�XA�ĜA��Aԗ�A�9XA�t�A�ZA�G�AξwA��HA�XA�"�A�
=A���A���A�XA�K�A�E�A�/A��A��yAǥ�AǃA�jA�l�A�XA�M�A�;dA�+A�
=Aƥ�A�l�A���AŁA�bNA��/A��#A���A���AöFA�v�A�M�A�33A�-A�&�A��A���A���A®APA�Q�A�9XA�ĜA�A�A��FA���A�(�A�{A��A��uA�VA�&�A�1A��#A���A�^5A���A���A�|�A�x�A�l�A�9XA��A���A�ƨA��FA���A���A��PA�l�A�O�A��A��A��mA��yA�ĜA�t�A�O�A���A��A�O�A��A��A���A�l�A��A���A���A���A�ĜA��!A���A��DA��+A�p�A�l�A�?}A�+A�%A���A���A�~�A�XA�-A�JA���A�dZA��A���A�VA���A�ffA�bA���A��#A��A�K�A�$�A��A�  A��A��`A���A��jA���A��hA��A�x�A�bNA�A�A�&�A�JA��;A��A�v�A�=qA��A�A���A��A��jA��uA�jA�VA�33A��A���A��DA�l�A�&�A�  A��`A��`A�ĜA���A��A��A�A���A�n�A�9XA��A��7A�=qA���A�oA��A���A�\)A�;dA��jA��A�Q�A��A��A��^A���A��A�M�A��A��A��TA��#A���A�ĜA���A�I�A���A���A��7A�^5A�"�A���A�ȴA��+A�7LA���A��A�=qA���A���A��!A���A��\A��A�jA�ZA�I�A�A�A�5?A� �A���A�A��PA�I�A�ƨA�;dA�
=A��A���A�l�A�1'A��A��A�ȴA��A�O�A�|�A���A��jA���A�~�A�M�A�  A��RA�  A��hA�`BA�VA���A�dZA�1'A��A�{A�bA���A�ȴA�z�A�&�A��A��9A�p�A�A���A�dZA�=qA�(�A�"�A� �A�bA�  A�A�ffA��A��yA���A���A���A��DA�x�A�XA�I�A�5?A��A�A��TA���A�l�A�(�A��A�r�A�&�A��^A�A�A��#A��A�ZA�K�A�C�A�9XA�(�A��;A�-A�dZA�p�A���A�=qA�XA��jA�z�A�5?A��\A��A�bNA��A���A��A���A�z�A��A���A��A�hsA�S�A�-A��9A��FA�I�A��A���A��wA�x�A���A�ĜA�z�A��A/A�A~�A~�RA}\)Az�yAwx�AvQ�Au�7At�uAs��Ar�!Ar�Aq/Ap�\Ao�;Ao+AnZAmC�Ak�Ak;dAj�HAj�uAjv�Aj1'AiAi�hAi�PAi�7Ai�7Ai�7Ai�7Ai�7Ai�Ail�AiXAiC�Ai/Ai+Ai"�Ai
=Ah�jAhffAg�AgK�Af�HAfE�Ae�hAe&�Ad�yAdI�AbĜAa�A`��A`z�A`bA_;dA^��A^�+A^E�A]��A\��A["�AY�AYoAWO�AVJAT�/AS;dAQ�;AO��AOp�AO�AOVAO
=AN��AN�\ANn�ANZANA�ANbAM��AM�AM��AM�FAM��AM��AM�7AMt�AMl�AMXAM�AL��ALn�AK�^AK�AJ��AJ��AJ��AJ�\AJ�DAJ�AJbNAJ(�AJJAI�mAIƨAI��AI�hAI�AI`BAI7LAI�AI
=AH�AH�`AH�HAH��AHȴAH�9AH��AH�AHffAHjAHffAHbNAHVAHZAHQ�AHVAHVAHZAHZAHbNAHn�AHn�AHr�AH~�AH~�AH�+AH�AH�DAH�\AH�\AH�uAH��AH�AI+AIS�AI��AI��AI�AI�AI�AH�+AH$�AG��AGXAF�!AF�AE|�AEVAD�/ADȴAD�RAD�AD��AD�\AD~�ADn�ADQ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�B	�B	�B	�B	�B	B	�B	�B	�B	�B	$B	YB	$B	�B	�B	�B	+B	�B	�B	_B	�B	�B	1'B	MjB	t�B	�aB	��B	�fB	��B	��B	�B	��B
 �B
SB

�B
~B
�B
B
(B
\B
�B
�B
$tB	�B	:^B	-�B	I�B	g8B	tTB	��B	��B	��B	��B	ںB	��B
1B
7�B
\]B
��B
ɆB
� B
�
B
�B�B�B6zB&B�BJB
�+B
уB
�6B
�kB
�B
o�B
JXB
9�B
%B	��B	רB	�B	��B	c�B	DgB	:�B	7�B	4�B	)�B	 �B	JB��B�fB�B�B�vB�B��B�GB��B��B	bB	#:B	B		B��B�MB�yB�,B��B��B��B�/B�B�B�+B�GB�B��B	B	B	�B	$�B	&B	#�B	,=B	RTB	�AB	��B	��B	�!B	��B	��B	��B	�B	�B	�B	�pB	�B	�;B	�GB	�B	��B	�%B	�	B	��B	��B	�MB	�;B	�TB	�B	��B	� B	�2B	�fB	�yB	��B	�B	�`B
	7B
�B
�B
{B
�B
4B
�B
YB
�B
�B
$�B
 �B
�B
�B
=B
�B
�B
�B
'�B
8�B
:*B
D�B
K�B
K�B
LdB
N�B
JXB
I�B
I�B
JXB
H�B
G�B
L�B
H�B
LdB
K)B
JXB
JXB
JXB
J#B
I�B
I�B
I�B
I�B
H�B
I�B
IB
H�B
K�B
I�B
GzB
GzB
I�B
I�B
F�B
K)B
K�B
H�B
FtB
@�B
>B
>B
>BB
<6B
:�B
:�B
;�B
<jB
@�B
=�B
=qB
:^B
8B
5?B
5�B
5�B
4B
2�B
2aB
1�B
0�B
4�B
5�B
4�B
3hB
.�B
0!B
-�B
2aB
1�B
-�B
.�B
.}B
/OB
,qB
+�B
-wB
.B
-�B
-wB
'�B
(XB
,qB
+kB
/OB
/OB
.�B
/OB
0!B
/OB
/�B
.�B
-�B
-�B
-CB
,�B
,B
+�B
*�B
,�B
+B
+6B
)*B
(XB
(�B
'�B
&LB
%B
"�B
!�B
#�B
$�B
$B
"hB
#:B
"hB
!-B
 �B
B
qB
�B
�B
B
�B
B
�B
B
�B
�B
�B
oB
B
�B
_B
�B
�B
kB
�B
�B
qB
	B
	B
�B
qB
�B
qB
�B
�B
�B
�B
�B
kB
�B
7B
7B
7B
B
1B
�B
_B
�B
�B
�B
+B
+B
�B
�B
�B
�B
YB
�B
�B
�B
�B
�B
FB
FB
�B
SB
SB
B
�B
SB
�B
�B
�B
YB
YB
�B
�B
$B
�B
SB
SB
�B
$B
�B
�B
_B
+B
�B
�B
�B
YB
�B
�B
YB
�B
�B
�B
�B
$B
�B
_B
�B
�B
+B
_B
�B
�B
+B
+B
+B
�B
+B
_B
+B
_B
�B
�B
�B
�B
_B
�B
�B
�B
B
�B
B
eB
eB
1B
1B
�B
B
7B
7B
�B
�B
�B
�B
	B
B
�B
�B
eB
�B
xB
xB
xB
xB
�B
�B
~B
�B
B
�B
VB
�B
�B
 \B
 �B
!�B
!�B
#�B
$tB
$�B
$�B
$�B
$�B
$�B
$�B
$tB
$@B
$B
$�B
%B
$�B
%B
$tB
%B
$@B
%zB
&LB
%�B
%�B
&B
'B
&�B
'B
'B
'RB
'B
'B
&�B
'B
'B
'�B
'RB
'RB
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)_B
)_B
*eB
+kB
+�B
+�B
,B
+�B
+�B
,B
,=B
,qB
,�B
-CB
-CB
-wB
-CB
-�B
.�B
/B
.�B
.�B
/OB
/�B
0�B
0�B
0�B
0�B
0UB
0�B
0�B
1'B
1'B
1[B
1�B
1�B
1�B
2�B
2�B
3hB
33B
33B
33B
3�B
4B
4B
4B
49B
4nB
4�B
5?B
5B
5?B
6B
6B
6B
6B
6B
6FB
7B
7�B
7�B
7LB
7LB
7�B
8B
8B
7�B
7�B
7LB
7�B
7�B
8�B
8�B
7�B
7�B
6FB
8RB
7LB
7�B
7�B
8RB
8�B
8�B
8�B
8B
8B
8B
8RB
8B
8�B
9XB
9XB
9�B
9XB
9�B
9�B
9�B
9�B
:�B
:^B
:�B
;0B
;�B
<�B
<�B
<�B
=B
=�B
=�B
>B
>BB
>wB
>wB
>�B
?B
>�B
?B
?B
?B
?B
>�B
@B
@B
@B
@B
@OB
@�B
A B
A B
AUB
A B
A�B
A�B
A�B
A�B
A�B
A�B
B[B
B�B
CaB
C�B
CaB
CaB
C�B
C�B
C�B
C�B
D�B
EB
E9B
E9B
E9B
E9B
FB
F?B
F�B
F�B
F�B
F�B
HKB
HB
G�B
HKB
HKB
HB
HB
HKB
H�B
H�B
J�B
K)B
K�B
K)B
K^B
K^B
K)B
K)B
LdB
L0B
L�B
L0B
L0B
L�B
MB
MjB
M�B
M�B
M�B
M�B
M�B
MjB
M6B
MjB
OB
N�B
N�B
O�B
OvB
OvB
O�B
OvB
P}B
P�B
P}B
P}B
QB
Q�B
Q�B
Q�B
R B
RTB
R�B
R�B
S�B
S[B
S�B
T,B
TaB
T,B
TaB
T�B
T�B
T�B
U2B
U2B
U�B
VB
VB
VB
VB
V�B
WsB
W?B
W
B
V�B
WsB
W?B
W�B
WsB
WsB
XyB
X�B
YB
YB
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[WB
[�B
[�B
[�B
\]B
]dB
\�B
\�B
]/B
\�B
]/B
^5B
^B
^B
^�B
^�B
_B
_B
_�B
_�B
`BB
`BB
`vB
`�B
aHB
aHB
aHB
a|B
a|B
a|B
a|B
a|B
a|B
a�B
bNB
b�B
b�B
b�B
cTB
c�B
c�B
dZB
dZB
d�B
d�B
d�B
e,B
e,B
e`B
e`B
e�B
e�B
e�B
f�B
f�B
g8B
g8B
g�B
g�B
g�B
g�B
gmB
g�B
h
B
i�B
jB
jB
jB
jB
j�B
j�B
kQB
l�B
l�B
m)B
m)B
m]B
l�B
l�B
m�B
m�B
o B
o5B
o5B
o5B
o B
o�B
oiB
o�B
o�B
o�B
p�B
p;B
pB
p;B
pB
pB
pB
pB
p;B
poB
p�B
p�B
qvB
q�B
q�B
rB
r�B
sB
sMB
s�B
tB
t�B
u%B
uZB
uZB
u�B
v+B
u�B
v+B
u�B
v`B
v`B
v�B
xB
w�B
wfB
wfB
wfB
w2B
wfB
w�B
x�B
x�B
x�B
y	B
y>B
y�B
y�B
y�B
y�B
zxB
{JB
{�B
|�B
|PB
|�B
|�B
|�B
}"B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~(B
}�B
~]B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
�B
�B
�B
�B
�B
� B
� B
� B
� B
� B
� B
� B
�B
� B
� B
�4B
�4B
��B
��B
��B
�;B
�oB
��B
��B
�oB
�oB
��B
�B
��B
�B
��B
��B
��B
�B
��B
�uB
�B
�AB
�uB
��B
��B
�AB
��B
��B
�uB
�GB
�B
�B
��B
�B
��B
��B
��B
�MB
��B
��B
�GB
�GB
�B
�{B
�GB
�GB
�{B
��B
�MB
�MB
��B
�SB
��B
��B
�SB
�SB
�B
��B
��B
�SB
��B
��B
��B
��B
��B	�B	�B	7B	_B	$B	IB	eB	B	�B	�B	�B	�B	�B	SB	�B	YB	�B	�B	�B	�B	�B	�B	YB	�B	�B	�B	$B	�B	�B	�B	YB	�B	�B	YB	�B	�B	SB	�B	�B	�B	$B	$B	SB	�B	$B	�B	B	B	�B	�B	SB	$B	_B	�B	SB	+B	$B	SB	�B	+B	�B	YB	+B	SB	�B	�B	�B	B	_B	�B	�B	YB	�B	�B	�B	+B	�B	�B	�B	�B	�B	$B	�B	$B	�B	_B	�B	YB	�B	�B	YB	YB	�B	�B	�B	+B	eB	�B	$B	�B	_B	�B	YB	7B	�B	�B	�B	�B	�B	YB	_B	�B	�B	YB	�B	+B	�B	+B	1B	�B	�B	�B	�B	B	1B	+B	eB	�B	1B	�B	B	�B	�B	�B		B	�B	-B	5B	:�B	6�B	5tB	4nB	=�B	?}B	JXB	V�B	WsB	^�B	ffB	jB	r|B	p;B	y	B	��B	��B	��B	��B	�B	�%B	��B	�%B	��B	�8B	��B	�`B	�8B	�B	��B	�`B	�B	�lB	��B	�+B	�8B	��B	��B	�`B	�lB	�8B	��B	��B	�2B	�B	��B	�B	�TB	�B	�AB	�B	�+B	��B	�+B
 �B
B
 �B	��B
 �B
;B
oB	�.B
  B
B
�B
MB
MB
YB
�B
�B
�B

�B
�B
B

�B
B
~B
PB
�B
�B
PB
PB
�B
JB
�B
�B
JB
JB
B
�B
B
JB
PB
�B
.B
VB
VB
�B
�B
�B
�B
�B
 B
(B
(B
.B
�B
bB
�B
�B
�B
�B
�B
bB
�B
�B
�B
eB
 �B
$B
#B
$tB
9�B
B

�B	��B	ȴB	�B	�B	�4B	s�B	I�B	;0B	0�B	qB	:�B	�B	8B	#nB	kB	#nB	P�B	OB	C�B	E�B	H�B	H�B	U2B	aB	iDB	iDB	hsB	jB	jB	l"B	m]B	qvB	��B	��B	�$B	�nB	�2B	�tB	��B	��B	��B	��B	�_B	��B	�*B	��B	��B	�LB	�0B	�B	��B	�=B	�IB	�_B	�wB	�EB	�B	��B	��B	�,B	��B	�TB	��B	�B	��B	�B	�cB	�B
 �B
�B
GB	��B	��B
�B
�B
YB
�B
�B
B
{B
�B
%B
	lB
xB
�B
B
B
�B
YB
�B
&�B
%�B
'RB
)�B
-B
0�B
0�B
:^B
C�B
C�B
FtB
D3B
F�B
FB
HB
GEB
F�B
HKB
R B
I�B
P}B
S[B
S&B
R�B
T�B
V�B
T�B
\�B
e,B
k�B
[#B
r�B
m�B
s�B
qB
qB
y�B
�SB
��B
��B
��B
�FB
��B
�YB
�eB
��B
��B
��B
�qB
�IB
�-B
�0B
�qB
� B
�XB
� B
��B
�B
�mB
�NB
�BB
�NB
�pB
��B
��B
��B
�TB
�B
�B
�B
�`B
�sB
�B
�DB
�B
�iB
��B
��B
�B
�B
��B
��B
�,B
��B
�ZB
�8B
�B
�%B
�B
�]B
�B
�B�BB
��B
��B1B�BJB
=BoB.B�BVB�BeB+B�B%�B&�B"4B-B8�B7�B5tB;0B8�B:^B4B5?B2-B-�B)�B%�B&�B$tB"hB"�B!bBBBCB�B�BBBB$tBMB�B@B(B�B�B
=B�B�B_B�B#:B
��B
�MB
�B
�B
�iB
�WB
�]B
�VB
��B
�EB
ݘB
�EB
ӏB
�B
�B
�9B
�gB
�tB
ǮB
˒B
�3B
�qB
�$B
��B
��B
�B
��B
��B
�qB
��B
�_B
��B
�$B
�hB
�YB
��B
��B
�SB
��B
�B
��B
~]B
}"B
{�B
}�B
zDB
z�B
o�B
w2B
r�B
p;B
r|B
^B
c�B
i�B
\�B
ZQB
OB
GB
D3B
@�B
?HB
>BB
C�B
JXB
H�B
T�B
,qB
5B
3hB
"�B
$B
�B
 �B
{B
�B
B	�B	� B	�B	�B	��B	�`B	�B	�B	��B	��B	�B	��B	уB	�B	�B	�2B	��B	�BB	�B	�3B	�B	�[B	��B	��B	��B	�aB	��B	��B	�B	�JB	��B	��B	|B	o�B	p;B	d�B	`�B	[WB	X�B	]/B	a|B	L�B	DgB	C�B	?HB	CaB	D3B	=�B	<jB	<6B	;�B	:�B	9�B	8�B	8�B	;0B	9�B	8�B	7�B	6�B	5tB	7B	8B	7LB	;�B	;0B	6B	6B	:^B	-�B	-CB	3�B	NB	0�B	-�B	#nB	)_B	'�B	�B	eB	�B	�B	$B	=qB	�B	B	!B	&�B	
	B	VB	B	oB	�B�>B�rB��B	B��B��B��B��B��B�B�2B��B�fB��B��B��B��B�TB��B�DB�	B��B	MB	 4B�B�GB�B�vB�oB��B�B�B��B�GB�|B�B�oB�5B�B��B�iB�;B�B� B��B�cB�B�B�iB��B�B�iB�B�AB��B�B�B�B�MB�B�B�|B�B�B�DB�B��B�DB�B��B��B��B��B�"B��B	�B	�B	B		B	VB	 \B	'RB	*�B	�B	 'B	"4B	�B	!�B	�B	�B	�B	�B	�B	B	VB	DB	�B	
	B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            B	�B	�B	�B	B	�B	�B	�B	7B	�B	�B	�B	�B	,B	iB	/B	�B	�B	�B	>B		B	�B	oB	�B	�B	0 B	K�B	riB	ҞB	��B	�wB	��B	�7B	�B	��B
 �B
�B

�B
mB
�B
�B
2B
B
(B
QB
=�B	�B	`�B	9OB	M�B	j�B	��B	�B	�B	��B	�B	�xB	�/B
 4B
KgB
p�B
�EB
�MB
�B
�&B
�cB�B$�BA�B-�B*�BwBTB
�B
��B
�!B
��B
�}B
a6B
S�B
�B	��B	��B	��B	�#B	s9B	H!B	=ZB	B�B	HB	=�B	=�B	/B��B��B	 6B�B��B�.B�B�*B�HB�B	B	+�B	~B	�B	
�B		�B�<B�cBجBݒB�iB��B�aB�B��B��B�7B��B	IB	KB	�B	+9B	(�B	#�B	&�B	MZB	B	��B	�AB	�sB	�&B	�GB	�jB	��B	�yB	�0B	�B	��B	�5B	�B	�OB	��B	��B	��B	��B	�'B	�_B	�uB	��B	�qB	�B	�,B	�B	�B	��B	��B	��B	�#B

�B
�B
sB
-B
[B
�B
�B
�B
bB
GB
&�B
"�B
 �B
�B
�B
�B
 ?B
�B
&hB
8�B
9�B
D�B
L�B
L.B
NUB
Q�B
L0B
K%B
J�B
KBB
IB
J�B
M�B
I�B
M�B
K�B
J�B
KB
J�B
J�B
J�B
J�B
J�B
J�B
J'B
KB
IbB
J B
N�B
J�B
H�B
H�B
K�B
J�B
G�B
L�B
MYB
K=B
I�B
B>B
>�B
?_B
@{B
=QB
<B
<#B
<B
<�B
A�B
?B
?cB
;�B
9�B
6�B
7B
8`B
4_B
3B
2�B
3"B
1�B
5KB
6�B
6�B
44B
0 B
2�B
-�B
4�B
3B
/B
0�B
0B
1.B
-�B
+tB
-�B
.�B
0tB
/ZB
'�B
(�B
,�B
+
B
/xB
/�B
/oB
0B
0qB
/�B
0�B
/QB
.�B
.�B
-�B
,�B
,|B
,LB
,(B
.�B
,pB
,�B
)�B
)�B
*B
)�B
'�B
&B
#vB
"YB
$cB
%�B
%B
#wB
$VB
#hB
#B
".B
�B
�B
B
$B
�B
�B
�B
B
�B
 B
&B
�B
�B
QB
*B
�B
B
B
�B
�B
aB
�B
`B
�B
B
	B
}B
'B
�B
�B
�B
oB
!B
RB
�B
dB
�B
�B
�B
�B
cB
�B
uB
�B
�B
ZB
�B
�B
YB
QB
B
/B
�B
B
�B
�B
�B
�B
�B
KB
�B
B
�B
�B
�B
_B
�B
GB
�B

B
�B
�B
�B
JB
+B
nB
uB
�B
.B
�B
�B
�B
xB
�B
�B
�B
 B
"B
�B
[B
�B
uB
�B
�B
�B
dB
�B
�B
�B
�B
]B
�B
�B
qB
YB
VB
�B
*B
�B
#B
�B
UB
9B
�B
�B
.B
0B
�B
TB
=B
�B
�B
B
�B
B
�B
�B
(B
�B
�B
�B
B
B
YB
/B
B
�B
YB
�B
�B
�B
B
B
oB
�B
gB
9B
�B
MB
�B
�B
 B
 �B
!|B
"{B
"�B
$bB
%BB
%B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
&6B
%tB
%WB
%}B
%"B
%�B
%B
&�B
&�B
&>B
&�B
'$B
'�B
'cB
'�B
'~B
'�B
'NB
'�B
'HB
'�B
'�B
(qB
'�B
'�B
(�B
)�B
)B
)B
)KB
*XB
)�B
)�B
*B
+rB
+�B
,B
,[B
,>B
+�B
,%B
,�B
,�B
-
B
-�B
-�B
-�B
-�B
-�B
.�B
/TB
/AB
.�B
/dB
/�B
0gB
1*B
1fB
15B
0�B
0�B
1TB
1IB
1�B
1sB
1�B
2]B
2!B
2�B
3B
3?B
3�B
3`B
3�B
3�B
4B
4iB
4\B
4[B
4�B
5B
5GB
5�B
5jB
5�B
6`B
6*B
69B
6HB
6kB
6�B
7�B
8*B
7�B
7�B
87B
8�B
8�B
8�B
8�B
8�B
7�B
7�B
8
B
9B
9rB
9/B
8�B
7B
99B
7�B
7�B
7�B
8�B
9DB
9�B
9�B
8~B
8�B
9gB
8�B
89B
9B
9}B
9sB
9�B
9�B
:8B
9�B
:oB
:�B
;6B
:�B
;:B
;�B
<	B
<�B
<�B
<�B
=eB
>9B
>/B
>hB
>>B
>�B
>�B
?XB
?KB
?B
?TB
?;B
?;B
?JB
?�B
@�B
@�B
@lB
@WB
@�B
APB
ArB
AeB
A~B
AvB
A�B
BB
BBB
BB
B B
B7B
CB
CSB
C�B
C�B
CmB
C�B
DB
DBB
DHB
DjB
EGB
ENB
E�B
E~B
EuB
E�B
F�B
F�B
GB
F�B
F�B
G�B
H�B
H&B
G�B
H�B
H�B
HQB
H}B
H�B
IB
J@B
LB
K�B
K�B
KBB
K�B
KuB
K\B
K�B
L�B
L�B
L�B
LPB
L�B
MhB
MjB
NB
NB
M�B
M�B
M�B
M�B
MvB
M�B
NzB
O�B
OB
OvB
P[B
O�B
O�B
PB
O�B
Q3B
P�B
P�B
P�B
Q�B
Q�B
RAB
RDB
R�B
R�B
R�B
SsB
T B
S�B
TSB
T�B
T�B
TZB
T�B
U�B
U*B
U3B
U�B
U�B
V)B
VGB
V+B
V>B
V�B
W`B
W�B
WEB
WB
W;B
W�B
W�B
W�B
W�B
W�B
YB
YB
YnB
Y�B
Y�B
ZB
ZVB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[cB
[JB
[~B
[�B
[�B
\
B
] B
]�B
]B
]B
]JB
]*B
^6B
^�B
^.B
^fB
^�B
^�B
_WB
_�B
`/B
`/B
`�B
`B
`�B
ajB
a_B
aoB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
b*B
b�B
b�B
cB
cUB
c�B
dYB
duB
d�B
d�B
d�B
d�B
e&B
edB
e}B
e�B
e�B
fB
f&B
f�B
gB
g1B
gmB
gjB
g�B
g�B
g�B
g�B
g�B
hCB
i-B
jOB
j;B
j�B
j�B
j�B
kB
k/B
l-B
mDB
mEB
m^B
m]B
m�B
mB
m�B
n�B
nDB
oZB
oHB
o<B
oOB
oVB
o�B
o�B
o�B
o�B
p�B
qB
pPB
pB
p^B
pB
pB
p;B
p.B
p�B
p�B
qB
qB
q�B
q�B
q�B
rWB
s	B
sOB
s�B
t	B
tSB
t�B
uZB
u�B
u�B
u�B
v?B
vB
vSB
vNB
v�B
v�B
w�B
x�B
w�B
wmB
wxB
wlB
wMB
w�B
x_B
yB
x�B
y	B
y#B
y�B
zB
y�B
y�B
y�B
z�B
{�B
|B
|�B
|�B
|�B
|�B
}3B
}dB
}yB
}�B
}�B
}�B
}�B
}�B
~B
}�B
~B
~B
~<B
~=B
~B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
B
~�B
2B
�B
�B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�	B
�6B
�!B
��B
�9B
�_B
��B
��B
��B
�B
�~B
��B
��B
��B
��B
��B
��B
�B
��B
�"B
�B
��B
��B
�%B
��B
��B
�@B
�WB
��B
��B
��B
�WB
��B
�B
��B
�HB
�B
�GB
��B
�0B
��B
�B
��B
�?B
��B
��B
�kB
�sB
�kB
��B
��B
�B
��B
�B
��B
�YB
��B
��B
��B
�B
�eB
�XB
�3B
��B
��B
��B
��B
��B
��B
��G�O�B	�B	�B	7B	_B	$B	IB	eB	B	�B	�B	�B	�B	�B	SB	�B	YB	�B	�B	�B	�B	�B	�B	YB	�B	�B	�B	$B	�B	�B	�B	YB	�B	�B	YB	�B	�B	SB	�B	�B	�B	$B	$B	SB	�B	$B	�B	B	B	�B	�B	SB	$B	_B	�B	SB	+B	$B	SB	�B	+B	�B	YB	+B	SB	�B	�B	�B	B	_B	�B	�B	YB	�B	�B	�B	+B	�B	�B	�B	�B	�B	$B	�B	$B	�B	_B	�B	YB	�B	�B	YB	YB	�B	�B	�B	+B	eB	�B	$B	�B	_B	�B	YB	7B	�B	�B	�B	�B	�B	YB	_B	�B	�B	YB	�B	+B	�B	+B	1B	�B	�B	�B	�B	B	1B	+B	eB	�B	1B	�B	B	�B	�B	�B		B	�B	-B	5B	:�B	6�B	5tB	4nB	=�B	?}B	JXB	V�B	WsB	^�B	ffB	jB	r|B	p;B	y	B	��B	��B	��B	��B	�B	�%B	��B	�%B	��B	�8B	��B	�`B	�8B	�B	��B	�`B	�B	�lB	��B	�+B	�8B	��B	��B	�`B	�lB	�8B	��B	��B	�2B	�B	��B	�B	�TB	�B	�AB	�B	�+B	��B	�+B
 �B
B
 �B	��B
 �B
;B
oB	�.B
  B
B
�B
MB
MB
YB
�B
�B
�B

�B
�B
B

�B
B
~B
PB
�B
�B
PB
PB
�B
JB
�B
�B
JB
JB
B
�B
B
JB
PB
�B
.B
VB
VB
�B
�B
�B
�B
�B
 B
(B
(B
.B
�B
bB
�B
�B
�B
�B
�B
bB
�B
�B
�B
eB
 �B
$B
#B
$tB
9�B
B

�B	��B	ȴB	�B	�B	�4B	s�B	I�B	;0B	0�B	qB	:�B	�B	8B	#nB	kB	#nB	P�B	OB	C�B	E�B	H�B	H�B	U2B	aB	iDB	iDB	hsB	jB	jB	l"B	m]B	qvB	��B	��B	�$B	�nB	�2B	�tB	��B	��B	��B	��B	�_B	��B	�*B	��B	��B	�LB	�0B	�B	��B	�=B	�IB	�_B	�wB	�EB	�B	��B	��B	�,B	��B	�TB	��B	�B	��B	�B	�cB	�B
 �B
�B
GB	��B	��B
�B
�B
YB
�B
�B
B
{B
�B
%B
	lB
xB
�B
B
B
�B
YB
�B
&�B
%�B
'RB
)�B
-B
0�B
0�B
:^B
C�B
C�B
FtB
D3B
F�B
FB
HB
GEB
F�B
HKB
R B
I�B
P}B
S[B
S&B
R�B
T�B
V�B
T�B
\�B
e,B
k�B
[#B
r�B
m�B
s�B
qB
qB
y�B
�SB
��B
��B
��B
�FB
��B
�YB
�eB
��B
��B
��B
�qB
�IB
�-B
�0B
�qB
� B
�XB
� B
��B
�B
�mB
�NB
�BB
�NB
�pB
��B
��B
��B
�TB
�B
�B
�B
�`B
�sB
�B
�DB
�B
�iB
��B
��B
�B
�B
��B
��B
�,B
��B
�ZB
�8B
�B
�%B
�B
�]B
�B
�B�BB
��B
��B1B�BJB
=BoB.B�BVB�BeB+B�B%�B&�B"4B-B8�B7�B5tB;0B8�B:^B4B5?B2-B-�B)�B%�B&�B$tB"hB"�B!bBBBCB�B�BBBB$tBMB�B@B(B�B�B
=B�B�B_B�B#:B
��B
�MB
�B
�B
�iB
�WB
�]B
�VB
��B
�EB
ݘB
�EB
ӏB
�B
�B
�9B
�gB
�tB
ǮB
˒B
�3B
�qB
�$B
��B
��B
�B
��B
��B
�qB
��B
�_B
��B
�$B
�hB
�YB
��B
��B
�SB
��B
�B
��B
~]B
}"B
{�B
}�B
zDB
z�B
o�B
w2B
r�B
p;B
r|B
^B
c�B
i�B
\�B
ZQB
OB
GB
D3B
@�B
?HB
>BB
C�B
JXB
H�B
T�B
,qB
5B
3hB
"�B
$B
�B
 �B
{B
�B
B	�B	� B	�B	�B	��B	�`B	�B	�B	��B	��B	�B	��B	уB	�B	�B	�2B	��B	�BB	�B	�3B	�B	�[B	��B	��B	��B	�aB	��B	��B	�B	�JB	��B	��B	|B	o�B	p;B	d�B	`�B	[WB	X�B	]/B	a|B	L�B	DgB	C�B	?HB	CaB	D3B	=�B	<jB	<6B	;�B	:�B	9�B	8�B	8�B	;0B	9�B	8�B	7�B	6�B	5tB	7B	8B	7LB	;�B	;0B	6B	6B	:^B	-�B	-CB	3�B	NB	0�B	-�B	#nB	)_B	'�B	�B	eB	�B	�B	$B	=qB	�B	B	!B	&�B	
	B	VB	B	oB	�B�>B�rB��B	B��B��B��B��B��B�B�2B��B�fB��B��B��B��B�TB��B�DB�	B��B	MB	 4B�B�GB�B�vB�oB��B�B�B��B�GB�|B�B�oB�5B�B��B�iB�;B�B� B��B�cB�B�B�iB��B�B�iB�B�AB��B�B�B�B�MB�B�B�|B�B�B�DB�B��B�DB�B��B��B��B��B�"B��B	�B	�B	B		B	VB	 \B	'RB	*�B	�B	 'B	"4B	�B	!�B	�B	�B	�B	�B	�B	B	VB	DB	�B	
	B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�G�=V=)�<H��<#�
<#�
<UV�<#�
<#�
<#�
<��$<#�
<T<�<�E<���<���<#�
<-��<E�4<�TS<��F<0�<M�<D+�<#�
<x`�<�ؼ<��<<H<HM<#�
<+��<�H�<��<ӻ�<f!k<��?<��<<���<�;�<���<#�
<#�
<?ɖ<�q�<�ƙ<�9�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<<��<���<Y�Z<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019061412075520190614120755IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019062411002820190624110028QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019062411002820190624110028QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020010906572820200109065728IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                