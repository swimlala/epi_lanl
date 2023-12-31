CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-04-13T16:27:39Z creation; 2023-04-26T19:14:26Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180413162739  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               	   	AA  AOAO7316_008644_009                 7316_008644_009                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�Z�s-�@�Z�s-�11  @�Z�d��@�Z�d��@)�z�]�z@)�z�]�z�d)X�/D��d)X�/D�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @@  @�G�@�  @�  @�G�A ��A��A ��A,(�A@  A`  A�  A�  A��A��A��AϮA�Q�A��B   B�
B�
B  B�
B(  B0  B7�
B?�
BG�BO�
BW�
B`  BhQ�BpQ�Bx  B�  B�  B�{B�  B�  B�  B�{B�{B�  B�  B�  B�  B�  B�  B��B�  B�  B��
B��
B�  B�  B�  B�  B��B��B�  B�  B�{B��B��B�{B�{C   C��C��C��C��C

=C  C��C  C
=C
=C  C  C  C  C
=C 
=C!��C$  C&  C(  C*  C,  C.  C/��C2  C4
=C6
=C8
=C:  C<  C=��C?��CB  CD  CF
=CH
=CJ
=CL
=CN  CP  CR
=CT
=CV  CX  CY��C[��C^  C`
=Cb
=Cd  Cf  Ch
=Cj  Ck��Cn  Cp  Cr
=Ct  Cu��Cx
=Cz
=C|  C~  C�C�
=C�  C�  C�C���C���C���C�C�  C���C���C���C���C�  C���C���C�  C���C���C�  C�C�  C���C���C�  C�  C�  C�C�
=C�C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C�C�  C���C�  C�
=C�C�  C�  C�  C���C���C���C���C���C���C�  C�  C�C�C�
=C�  C���C���C���C�  C�C�C�  C�  C���C���C�C���C���C�  C�C�  C�  C�  C�  C�  C���C���C���C�  C�C���C�  C�C�  C���C���C���C�C�  C�  C�C���C���C���C���C�  C�C�  C���C���C�C�
=C�  C�  C�C�C���C�  C�
=C�C�C�
=C�
=C�C���C���C���C���C���D � D�D� D�D� D�qD� D  D� D  D� D  D��D  D� D  D}qD	  D	z�D	�qD
}qD  D��D  Dz�D�qD}qD  D��D  D}qD�qDz�D�qD��D�D��D�D��D�D� D�qD}qD  D��DD��D�D� D  D��D  D� D  D� D�D� D  D��DD��D�qD}qD�qD }qD!  D!��D"�D"� D"�qD#� D$�D$�D%�D%��D&�D&��D'  D'� D(  D(� D)�D)� D)��D*}qD+  D+}qD+�qD,z�D,�qD-}qD-�qD.}qD.�qD/}qD0  D0� D1�D1}qD2  D2��D3  D3}qD3��D4� D5�D5}qD5�qD6z�D6��D7��D8D8� D9  D9��D9�qD:}qD;  D;z�D<  D<��D=  D=z�D>  D>��D?  D?z�D?�qD@� DA  DA��DB  DB� DC  DC��DDDD��DE  DE}qDF  DF��DF�qDG� DH�DH� DI  DIz�DI�qDJ� DJ�qDK� DL�DL}qDM  DM� DN  DN� DO  DO��DP�DP��DP�qDQ� DR  DRz�DR�qDS��DTDT� DU�DU��DV�DV��DW  DW��DX�DX}qDX�qDY� DZ  DZ��D[�D[��D\  D\� D]�D]��D^  D^� D^�qD_z�D_��D`}qDa  Da��Db�Db��Dc  Dc� Dd  Dd}qDe  De��Df�Df� Dg  Dg� Dh  Dh� Di�Di� Dj  Dj��Dk  Dk}qDk�qDl� Dl�qDm� Dn  Dn� DoDo��Dp�Dp��Dp�qDq}qDq�qDr}qDs�Ds� Ds�qDt� Du  Du}qDu�qDv}qDw  Dw� Dx�Dx��Dy�Dy� Dz  Dz� D{�D{� D{�qD|}qD}  D}}qD}�qD~}qD  D� D�qD�>�D�~�D�� D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�@ D�� D�� D�HD�@ D�~�D���D�  D�>�D�� D�D�  D�>�D�� D��HD���D�@ D��HD�� D�HD�@ D��HD�D��D�AHD��HD��HD�HD�AHD��HD�� D�  D�>�D�}qD�� D���D�>�D�� D���D���D�>�D�~�D���D�  D�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D��HD�� D�  D�AHD�� D�� D�HD�@ D�� D���D���D�@ D�� D��HD�  D�AHD���D�� D���D�>�D�~�D���D�  D�>�D�~�D���D��qD�>�D�~�D���D���D�@ D�� D�� D�HD�AHD��HD��HD�  D�@ D���D��HD���D�@ D�� D��HD��D�B�D��HD���D���D�>�D�� D��HD�  D�>�D�� D��HD��D�@ D�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D�� D���D�@ D���D��HD�HD�@ D�~�D�� D���D�@ D��HD��HD�  D�B�D��HD��HD�HD�AHD�� D�� D�  D�>�D��HD�D�  D�@ D�� D�� D�  D�=qD�}qD���D�HD�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�~�D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D���D�D��D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�~�D��qD�  D�AHD�� D���D��qD�>�D�� D�� D���D�>�D�~�D�� D���D�@ D�� D�� D���D�@ D���D��HD�HD�@ D�~�D�� D�  D�>�D�~�D�� D���D�>�D�}qD�� D�HD�>�D�~�D�� D�HD�@ DÀ Dþ�D���D�@ DĀ D�� D�  D�@ D�~�Dž�D���D�>�Dƀ D�� D�  D�>�Dǀ D��HD�  D�>�DȀ D�� D���D�>�Dɀ D�� D���D�>�Dʀ Dʾ�D���D�>�Dˀ D�� D�HD�AHD̀ D�� D�  D�>�D�~�D��HD�HD�@ D΁HD�D�HD�AHDπ D�� D�  D�AHDЀ Dо�D�HD�@ Dр D��HD��D�AHDҁHDҾ�D�  D�@ DӁHD��HD�  D�@ DԀ D�� D���D�@ DՀ D�� D�  D�AHDցHD��HD�  D�@ D׀ D��HD�HD�AHD؀ D�� D�HD�B�Dـ Dپ�D���D�>�DځHD��HD���D�@ DہHD�� D�  D�AHD܁HD�� D�HD�@ D݀ D�� D�  D�AHDހ D޾�D�  D�>�D�~�D�� D�  D�@ D�~�D�� D�  D�>�D� D�� D�  D�>�D� D�� D�  D�AHD� D㾸D�  D�AHD�HD��HD�HD�>�D�~�D�� D�  D�@ D� D�� D�  D�@ D� D��HD�  D�@ D� D辸D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D낏D��HD��qD�=qD�~�D�� D�HD�@ D�~�D�� D�HD�AHD� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�AHD�HD��HD�HD�@ D�HD��HD�HD�@ D� D�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D���D�B�D��HD���D���D�@ D�}qD���D�HD�@ D�}qD�� D�HD�AHD�� D�� D�HD�1�D�u�?#�
?.{?W
=?�  ?�{?�(�?��@��@&ff@8Q�@E�@Y��@s33@��
@���@�
=@��\@�\)@�Q�@�G�@���@ٙ�@��
@�{@�Q�A�A�A��A�A
=Ap�A!�A&ffA,(�A1�A7
=A<(�AAG�AG
=AL(�AQ�AVffA\(�Aa�Ag
=Ak�AqG�Aw
=A|(�A�Q�A�33A�A���A�33A�p�A�Q�A��A�{A���A��HA�{A���A�33A�p�A���A�33A�{A�Q�A��HA�A���A�33A�A�Q�A�33A�{A�Q�A��HA�AУ�A��HA�Aأ�A�33A��A�  A��HA�p�A�  A�\A�A�Q�A�\A���A��A��\A��A�\)B ��B=qB�B��B�B33B��B	B
�HB(�Bp�B�RB�
B�B�\B�
B��B=qB�B�BffB�B��B=qB�B ��B"{B#\)B$��B&{B'\)B(z�B)B+\)B,��B-B.�HB0(�B1��B2�HB4(�B5G�B6ffB7�
B9G�B:�\B;�B<��B>ffB?�B@��BB=qBC\)BD��BF=qBG�BH��BI�BK33BL��BN{BO33BPz�BQ��BS33BTz�BU��BV�HBX  BYp�BZ�RB\  B]G�B^=qB_�B`��Bb=qBc\)Bdz�Be��Bf�HBh(�BiG�Bj{Bj�HBk�
Bl��Bmp�Bm�BnffBo
=Bo�Bp  Bp(�Bp��Bp��Bq��Br{BrffBr�RBs
=Bs�Bt(�Bt��Bt��BuG�Bu�Bv=qBv�\Bv�HBw\)Bx  BxQ�Bx��By�Byp�Bz{Bz�\Bz�HB{33B{�B|(�B|��B}�B}p�B}B~ffB~�HB33B�B�  B�Q�B��\B���B���B��B�\)B��B��B�{B�=qB�z�B���B��B�G�B�p�B���B��B�=qB�z�B���B��HB�33B�p�B���B��
B�{B�z�B��RB���B��B�p�B�B�  B�=qB�z�B��RB���B�G�B���B�B��B�=qB��\B���B���B�33B�p�B��
B�{B�ffB��\B���B��B�p�B�B��B�(�B�z�B��RB��B�p�B���B��
B�(�B�z�B���B�
=B�\)B��B��
B�=qB��\B��HB��B�\)B��B�  B�Q�B��\B��HB��B��B��
B�(�B�z�B��RB���B�G�B��B�  B�Q�B��\B��HB�33B��B��
B�(�B�z�B���B�
=B�G�B���B�{B�ffB��\B��HB��B�p�B��
B�(�B�z�B���B�
=B�33B���B��B�Q�B���B���B�33B��B��B�Q�B��RB�
=B�\)B���B��B�Q�B���B�
=B�p�B��
B�(�B�z�B��RB�
=B�p�B�B�(�B�z�B��HB�G�B���B��B�=qB��\B���B�G�B��B�{B�z�B��HB�G�B��B��
B�(�B��\B���B�p�B��
B�(�B�z�B��HB��B��B��B�=qB���B��B�p�B��
B�(�B�z�B���B�33B��B�{B�z�B��HB�33B���B��B�Q�B���B�
=B��B��B�ffB���B��B��B��B�=qB���B��B��B�  B�ffB���B�G�B�B�(�B���B��B���B�  B�z�B���B�p�B��
B�Q�BĸRB�33BŮB�{BƏ\B�
=BǅB�  B�z�B�
=BɅB�{Bʏ\B�
=B˅B�  B̏\B���BͅB�  B�z�B���BυB�{BЏ\B��Bљ�B�=qB���B�\)B��Bԏ\B�
=BծB�(�B���B�\)B��B�z�B�
=Bٙ�B�(�Bڣ�B�G�B�B�Q�B��HB�p�B�  Bޏ\B��B߮B�=qB�RB�\)B��B�z�B�
=B㙚B�(�B���B�\)B��B�z�B��B癚B�=qB���B�\)B��B�z�B�
=B뙚B�(�B���B�G�B��B�z�B�
=B�B�=qB��HB�p�B�{B�\B��B�B�(�B��RB�G�B��
B�ffB���B��B�{B��RB�G�B��
B�z�B��B���B�(�B���B�\)B��B��\B��B�C (�C z�C C
=C\)C��C�C33Cz�CC{C\)C��C�C=qC�\C�
C(�Cp�CC�CffC�RC
=CQ�C��C�HC(�Cp�C�RC	
=C	Q�C	��C	��C
=qC
��C
�HC(�Cp�C�RC
=CQ�C�C
=C\)C�C��CG�C��C�C=qC�\C�CG�C��C��CQ�C��C�CG�C��C��CQ�C�C  CG�C��C��CQ�C�C
=C\)C�RC
=C\)C�RC
=CffC��C33C�C�
C33C�C�
C(�C�\C�CG�C��C�C=qC��C�CG�C��C��CG�C�\C�
C {C Q�C �\C ��C!  C!=qC!ffC!�\C!C!�C"�C"\)C"�\C"C#  C#(�C#\)C#�C#�RC#�HC$�C$Q�C$�\C$C%  C%(�C%\)C%�C%�C%�HC&{C&Q�C&�C&�RC&��C'�C'G�C'p�C'��C'�
C(
=C(=qC(p�C(�C(�HC)
=C)33C)\)C)�\C)C)�C*�C*Q�C*�\C*�RC*�C+
=C+33C+ffC+��C+��C,  C,(�C,Q�C,z�C,�C,�HC-{C-=qC-ffC-�\C-��C.  C.33C.\)C.�C.�RC.�C/{C/Q�C/�C/�RC/�HC0
=C0=qC0ffC0��C0�
C1
=C133C1\)C1�C1�RC1��C2�C2Q�C2z�C2��C2��C3  C333C3ffC3�\C3�RC3�HC4{C4G�C4z�C4��C4��C5  C5(�C5\)C5��C5��C5��C6�C6Q�C6z�C6�RC6�C7�C7Q�C7p�C7��C7�
C8{C8G�C8p�C8��C8C8��C9(�C9ffC9�\C9C9�HC:{C:Q�C:z�C:�RC:�HC;
=C;33C;p�C;��C;�
C<
=C<33C<\)C<�\C<C=  C=33C=\)C=�C=�C=�C>�C>Q�C>z�C>�C>�
C?  C?=qC?p�C?��C?�
C@  C@(�C@ffC@��C@�
CA  CA(�CA\)CA��CA��CB
=CB(�CB\)CB�\CB��CC
=CC33CCffCC�\CCCD  CD=qCDp�CD�\CDCE  CE=qCEffCE��CE��CF  CF33CFp�CF��CF��CG  CG33CGp�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        ?��@   @@  @�G�@�  @�  @�G�A ��A��A ��A,(�A@  A`  A�  A�  A��A��A��AϮA�Q�A��B   B�
B�
B  B�
B(  B0  B7�
B?�
BG�BO�
BW�
B`  BhQ�BpQ�Bx  B�  B�  B�{B�  B�  B�  B�{B�{B�  B�  B�  B�  B�  B�  B��B�  B�  B��
B��
B�  B�  B�  B�  B��B��B�  B�  B�{B��B��B�{B�{C   C��C��C��C��C

=C  C��C  C
=C
=C  C  C  C  C
=C 
=C!��C$  C&  C(  C*  C,  C.  C/��C2  C4
=C6
=C8
=C:  C<  C=��C?��CB  CD  CF
=CH
=CJ
=CL
=CN  CP  CR
=CT
=CV  CX  CY��C[��C^  C`
=Cb
=Cd  Cf  Ch
=Cj  Ck��Cn  Cp  Cr
=Ct  Cu��Cx
=Cz
=C|  C~  C�C�
=C�  C�  C�C���C���C���C�C�  C���C���C���C���C�  C���C���C�  C���C���C�  C�C�  C���C���C�  C�  C�  C�C�
=C�C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C�C�  C���C�  C�
=C�C�  C�  C�  C���C���C���C���C���C���C�  C�  C�C�C�
=C�  C���C���C���C�  C�C�C�  C�  C���C���C�C���C���C�  C�C�  C�  C�  C�  C�  C���C���C���C�  C�C���C�  C�C�  C���C���C���C�C�  C�  C�C���C���C���C���C�  C�C�  C���C���C�C�
=C�  C�  C�C�C���C�  C�
=C�C�C�
=C�
=C�C���C���C���C���C���D � D�D� D�D� D�qD� D  D� D  D� D  D��D  D� D  D}qD	  D	z�D	�qD
}qD  D��D  Dz�D�qD}qD  D��D  D}qD�qDz�D�qD��D�D��D�D��D�D� D�qD}qD  D��DD��D�D� D  D��D  D� D  D� D�D� D  D��DD��D�qD}qD�qD }qD!  D!��D"�D"� D"�qD#� D$�D$�D%�D%��D&�D&��D'  D'� D(  D(� D)�D)� D)��D*}qD+  D+}qD+�qD,z�D,�qD-}qD-�qD.}qD.�qD/}qD0  D0� D1�D1}qD2  D2��D3  D3}qD3��D4� D5�D5}qD5�qD6z�D6��D7��D8D8� D9  D9��D9�qD:}qD;  D;z�D<  D<��D=  D=z�D>  D>��D?  D?z�D?�qD@� DA  DA��DB  DB� DC  DC��DDDD��DE  DE}qDF  DF��DF�qDG� DH�DH� DI  DIz�DI�qDJ� DJ�qDK� DL�DL}qDM  DM� DN  DN� DO  DO��DP�DP��DP�qDQ� DR  DRz�DR�qDS��DTDT� DU�DU��DV�DV��DW  DW��DX�DX}qDX�qDY� DZ  DZ��D[�D[��D\  D\� D]�D]��D^  D^� D^�qD_z�D_��D`}qDa  Da��Db�Db��Dc  Dc� Dd  Dd}qDe  De��Df�Df� Dg  Dg� Dh  Dh� Di�Di� Dj  Dj��Dk  Dk}qDk�qDl� Dl�qDm� Dn  Dn� DoDo��Dp�Dp��Dp�qDq}qDq�qDr}qDs�Ds� Ds�qDt� Du  Du}qDu�qDv}qDw  Dw� Dx�Dx��Dy�Dy� Dz  Dz� D{�D{� D{�qD|}qD}  D}}qD}�qD~}qD  D� D�qD�>�D�~�D�� D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�@ D�� D�� D�HD�@ D�~�D���D�  D�>�D�� D�D�  D�>�D�� D��HD���D�@ D��HD�� D�HD�@ D��HD�D��D�AHD��HD��HD�HD�AHD��HD�� D�  D�>�D�}qD�� D���D�>�D�� D���D���D�>�D�~�D���D�  D�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D��HD�� D�  D�AHD�� D�� D�HD�@ D�� D���D���D�@ D�� D��HD�  D�AHD���D�� D���D�>�D�~�D���D�  D�>�D�~�D���D��qD�>�D�~�D���D���D�@ D�� D�� D�HD�AHD��HD��HD�  D�@ D���D��HD���D�@ D�� D��HD��D�B�D��HD���D���D�>�D�� D��HD�  D�>�D�� D��HD��D�@ D�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D�� D���D�@ D���D��HD�HD�@ D�~�D�� D���D�@ D��HD��HD�  D�B�D��HD��HD�HD�AHD�� D�� D�  D�>�D��HD�D�  D�@ D�� D�� D�  D�=qD�}qD���D�HD�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�~�D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D���D�D��D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�~�D��qD�  D�AHD�� D���D��qD�>�D�� D�� D���D�>�D�~�D�� D���D�@ D�� D�� D���D�@ D���D��HD�HD�@ D�~�D�� D�  D�>�D�~�D�� D���D�>�D�}qD�� D�HD�>�D�~�D�� D�HD�@ DÀ Dþ�D���D�@ DĀ D�� D�  D�@ D�~�Dž�D���D�>�Dƀ D�� D�  D�>�Dǀ D��HD�  D�>�DȀ D�� D���D�>�Dɀ D�� D���D�>�Dʀ Dʾ�D���D�>�Dˀ D�� D�HD�AHD̀ D�� D�  D�>�D�~�D��HD�HD�@ D΁HD�D�HD�AHDπ D�� D�  D�AHDЀ Dо�D�HD�@ Dр D��HD��D�AHDҁHDҾ�D�  D�@ DӁHD��HD�  D�@ DԀ D�� D���D�@ DՀ D�� D�  D�AHDցHD��HD�  D�@ D׀ D��HD�HD�AHD؀ D�� D�HD�B�Dـ Dپ�D���D�>�DځHD��HD���D�@ DہHD�� D�  D�AHD܁HD�� D�HD�@ D݀ D�� D�  D�AHDހ D޾�D�  D�>�D�~�D�� D�  D�@ D�~�D�� D�  D�>�D� D�� D�  D�>�D� D�� D�  D�AHD� D㾸D�  D�AHD�HD��HD�HD�>�D�~�D�� D�  D�@ D� D�� D�  D�@ D� D��HD�  D�@ D� D辸D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D낏D��HD��qD�=qD�~�D�� D�HD�@ D�~�D�� D�HD�AHD� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�AHD�HD��HD�HD�@ D�HD��HD�HD�@ D� D�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D���D�B�D��HD���D���D�@ D�}qD���D�HD�@ D�}qD�� D�HD�AHD�� D�� D�HD�1�G�O�?#�
?.{?W
=?�  ?�{?�(�?��@��@&ff@8Q�@E�@Y��@s33@��
@���@�
=@��\@�\)@�Q�@�G�@���@ٙ�@��
@�{@�Q�A�A�A��A�A
=Ap�A!�A&ffA,(�A1�A7
=A<(�AAG�AG
=AL(�AQ�AVffA\(�Aa�Ag
=Ak�AqG�Aw
=A|(�A�Q�A�33A�A���A�33A�p�A�Q�A��A�{A���A��HA�{A���A�33A�p�A���A�33A�{A�Q�A��HA�A���A�33A�A�Q�A�33A�{A�Q�A��HA�AУ�A��HA�Aأ�A�33A��A�  A��HA�p�A�  A�\A�A�Q�A�\A���A��A��\A��A�\)B ��B=qB�B��B�B33B��B	B
�HB(�Bp�B�RB�
B�B�\B�
B��B=qB�B�BffB�B��B=qB�B ��B"{B#\)B$��B&{B'\)B(z�B)B+\)B,��B-B.�HB0(�B1��B2�HB4(�B5G�B6ffB7�
B9G�B:�\B;�B<��B>ffB?�B@��BB=qBC\)BD��BF=qBG�BH��BI�BK33BL��BN{BO33BPz�BQ��BS33BTz�BU��BV�HBX  BYp�BZ�RB\  B]G�B^=qB_�B`��Bb=qBc\)Bdz�Be��Bf�HBh(�BiG�Bj{Bj�HBk�
Bl��Bmp�Bm�BnffBo
=Bo�Bp  Bp(�Bp��Bp��Bq��Br{BrffBr�RBs
=Bs�Bt(�Bt��Bt��BuG�Bu�Bv=qBv�\Bv�HBw\)Bx  BxQ�Bx��By�Byp�Bz{Bz�\Bz�HB{33B{�B|(�B|��B}�B}p�B}B~ffB~�HB33B�B�  B�Q�B��\B���B���B��B�\)B��B��B�{B�=qB�z�B���B��B�G�B�p�B���B��B�=qB�z�B���B��HB�33B�p�B���B��
B�{B�z�B��RB���B��B�p�B�B�  B�=qB�z�B��RB���B�G�B���B�B��B�=qB��\B���B���B�33B�p�B��
B�{B�ffB��\B���B��B�p�B�B��B�(�B�z�B��RB��B�p�B���B��
B�(�B�z�B���B�
=B�\)B��B��
B�=qB��\B��HB��B�\)B��B�  B�Q�B��\B��HB��B��B��
B�(�B�z�B��RB���B�G�B��B�  B�Q�B��\B��HB�33B��B��
B�(�B�z�B���B�
=B�G�B���B�{B�ffB��\B��HB��B�p�B��
B�(�B�z�B���B�
=B�33B���B��B�Q�B���B���B�33B��B��B�Q�B��RB�
=B�\)B���B��B�Q�B���B�
=B�p�B��
B�(�B�z�B��RB�
=B�p�B�B�(�B�z�B��HB�G�B���B��B�=qB��\B���B�G�B��B�{B�z�B��HB�G�B��B��
B�(�B��\B���B�p�B��
B�(�B�z�B��HB��B��B��B�=qB���B��B�p�B��
B�(�B�z�B���B�33B��B�{B�z�B��HB�33B���B��B�Q�B���B�
=B��B��B�ffB���B��B��B��B�=qB���B��B��B�  B�ffB���B�G�B�B�(�B���B��B���B�  B�z�B���B�p�B��
B�Q�BĸRB�33BŮB�{BƏ\B�
=BǅB�  B�z�B�
=BɅB�{Bʏ\B�
=B˅B�  B̏\B���BͅB�  B�z�B���BυB�{BЏ\B��Bљ�B�=qB���B�\)B��Bԏ\B�
=BծB�(�B���B�\)B��B�z�B�
=Bٙ�B�(�Bڣ�B�G�B�B�Q�B��HB�p�B�  Bޏ\B��B߮B�=qB�RB�\)B��B�z�B�
=B㙚B�(�B���B�\)B��B�z�B��B癚B�=qB���B�\)B��B�z�B�
=B뙚B�(�B���B�G�B��B�z�B�
=B�B�=qB��HB�p�B�{B�\B��B�B�(�B��RB�G�B��
B�ffB���B��B�{B��RB�G�B��
B�z�B��B���B�(�B���B�\)B��B��\B��B�C (�C z�C C
=C\)C��C�C33Cz�CC{C\)C��C�C=qC�\C�
C(�Cp�CC�CffC�RC
=CQ�C��C�HC(�Cp�C�RC	
=C	Q�C	��C	��C
=qC
��C
�HC(�Cp�C�RC
=CQ�C�C
=C\)C�C��CG�C��C�C=qC�\C�CG�C��C��CQ�C��C�CG�C��C��CQ�C�C  CG�C��C��CQ�C�C
=C\)C�RC
=C\)C�RC
=CffC��C33C�C�
C33C�C�
C(�C�\C�CG�C��C�C=qC��C�CG�C��C��CG�C�\C�
C {C Q�C �\C ��C!  C!=qC!ffC!�\C!C!�C"�C"\)C"�\C"C#  C#(�C#\)C#�C#�RC#�HC$�C$Q�C$�\C$C%  C%(�C%\)C%�C%�C%�HC&{C&Q�C&�C&�RC&��C'�C'G�C'p�C'��C'�
C(
=C(=qC(p�C(�C(�HC)
=C)33C)\)C)�\C)C)�C*�C*Q�C*�\C*�RC*�C+
=C+33C+ffC+��C+��C,  C,(�C,Q�C,z�C,�C,�HC-{C-=qC-ffC-�\C-��C.  C.33C.\)C.�C.�RC.�C/{C/Q�C/�C/�RC/�HC0
=C0=qC0ffC0��C0�
C1
=C133C1\)C1�C1�RC1��C2�C2Q�C2z�C2��C2��C3  C333C3ffC3�\C3�RC3�HC4{C4G�C4z�C4��C4��C5  C5(�C5\)C5��C5��C5��C6�C6Q�C6z�C6�RC6�C7�C7Q�C7p�C7��C7�
C8{C8G�C8p�C8��C8C8��C9(�C9ffC9�\C9C9�HC:{C:Q�C:z�C:�RC:�HC;
=C;33C;p�C;��C;�
C<
=C<33C<\)C<�\C<C=  C=33C=\)C=�C=�C=�C>�C>Q�C>z�C>�C>�
C?  C?=qC?p�C?��C?�
C@  C@(�C@ffC@��C@�
CA  CA(�CA\)CA��CA��CB
=CB(�CB\)CB�\CB��CC
=CC33CCffCC�\CCCD  CD=qCDp�CD�\CDCE  CE=qCEffCE��CE��CF  CF33CFp�CF��CF��CG  CG33CGp�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�7LA�I�A�Q�A�Q�A�S�A�S�A�VA�XA�XA�VA�S�A�Q�A�O�A�O�A�K�A�ZA�O�A�K�A�M�A�I�A� �A��A׮A�?}AԸRA���A҇+A�XA�"�AѼjA�jA�;dA�{A���A�+A��
AϏ\A�/A΍PA͗�A�E�A�C�A˼jA���A�5?A���A�hsA�hsA��#Aŉ7Aß�A��A��A��-A�v�A�7LA�K�A�;dA�7LA��TA���A�E�A��A�`BA�VA��jA���A�1A�hsA�1A��wA�t�A���A��\A��A���A�5?A�$�A�;dA�A��#A}�hAyXAt~�ArbNAip�AeO�Abn�AaoA`��A_�A]�AZz�AU�AQdZAP=qAL�AG��AGp�AF$�AD�DAB�9A@ZA>n�A<�/A:I�A6�A3�;A0�HA/oA-S�A)ƨA)�A*ffA)��A*{A(ZA&bA$�yA%S�A%x�A%�A%�#A'A(VA(bA't�A&��A&ffA&v�A'%A(E�A(A'�A&ȴA&��A&jA%�TA$�A#��A#dZA"��A!�A!�A!C�A I�AO�AffA��A;dA��A�!AZA�TA/AĜA�A�wAC�A�RAQ�A�A�AhsA?}A�A
=A��A�`A�/A�\A �A�mA�PA�AZAbA��A�A��Ap�A
=A��A��A�\An�AI�A{AA|�A\)A
=Ar�A9XA�#At�A`BAK�A%A�!AbNAbA��A�PAO�A+A%A
�A
��A
��A
r�A
�A	��A	p�A	C�A�yA�\A �A�FA��Ar�A�A�TA�PA�A�!AZA��A�FA�A?}AVA�AȴA��A��Az�AA�A�Al�AG�A&�A �jA �uA r�A ZA =qA 1@�ƨ@��@�5?@���@�X@��`@���@��@��@�K�@��R@�^5@��-@�O�@�Ĝ@�1@���@��!@��-@�bN@�P@��y@��@�Z@�F@�\)@��H@�V@���@�`B@�r�@땁@�"�@ꗍ@��@�hs@�z�@�1@�l�@�
=@���@�-@�O�@���@�bN@��
@�|�@�"�@��H@�R@�E�@��@�7@�&�@�V@��@��;@�33@��@�Q�@۶F@�"�@ڧ�@�E�@���@�&�@��@��@���@�z�@��@�b@��
@���@֧�@��T@�x�@�7L@ԛ�@ӝ�@��@�5?@Ѳ-@�O�@���@Гu@�(�@Ͼw@�l�@��H@�^5@�@�`B@��`@̛�@�Z@��m@˝�@�"�@�V@��@�O�@ȓu@�Z@�b@�l�@���@�ȴ@�-@š�@�O�@���@Ĵ9@ě�@�Z@��@�33@�~�@�J@���@��h@�/@�V@�j@��@��y@�5?@���@��^@���@���@�`B@��@�V@���@�Ĝ@�(�@�dZ@��H@��+@�E�@�J@�x�@�O�@��@���@���@��@��u@�b@���@��@�K�@��y@�E�@���@���@��-@���@�hs@���@���@�1'@��@�;d@�ȴ@��!@���@���@���@���@��9@�1@��w@��@�t�@�33@��@��y@��@���@���@���@���@��\@�=q@��#@�V@���@���@��u@�r�@�1@��P@�o@�ȴ@�~�@���@�X@��/@���@�r�@�(�@���@�ƨ@���@�n�@�@��`@��@���@��@�I�@�ƨ@�33@���@�n�@�-@��^@��@��@�I�@��
@�\)@�
=@��y@�ff@��#@���@�G�@��@���@�I�@�ƨ@�dZ@�o@��H@���@�$�@���@��7@��@�?}@�Ĝ@� �@��@�S�@�33@�33@�+@���@�ȴ@���@��\@�-@���@���@�7L@��/@��u@�r�@�Z@�I�@� �@���@�ȴ@�~�@�v�@�V@�5?@��^@�x�@�X@�X@��@���@�z�@�1'@��@��@�ƨ@�+@�^5@�@���@�/@��/@��9@�z�@�(�@���@�C�@�ȴ@���@�J@���@��-@�p�@�&�@��@���@��u@�Q�@��;@��w@��@�|�@�K�@���@�ȴ@���@�^5@��@�@���@�p�@��@��/@���@���@�bN@�1'@��@�ƨ@���@��P@�\)@�33@���@�v�@�-@�@�hs@��@�%@��/@���@��9@�r�@�9X@��@�@�w@K�@~�@~@}/@|�j@{�m@{t�@{"�@z��@z=q@y�^@xĜ@xb@x  @w��@w;d@v��@v{@u�@t��@tj@t9X@t�@t1@t1@s��@r��@r��@r=q@qX@p��@pĜ@p�@o�@o|�@o
=@nv�@n@m�h@m�@lj@l�@k�F@kt�@k33@j��@j-@i�7@iG�@h��@h�u@g�@gK�@g+@f�@f�+@fv�@f$�@e��@e?}@eV@d�/@d(�@c��@c33@c@b��@b�!@bM�@a�#@a�7@aG�@`Ĝ@`Q�@_��@^�y@^{@]�@]?}@\�/@\Z@[�F@[o@Z�@Z�!@Z��@Z~�@Z^5@Y��@YG�@X��@X��@XA�@W�@W�w@W\)@W;d@V��@V5?@U�T@U��@T�/@T�@S�
@So@R�!@Rn�@R=q@Q�#@Q�7@Q%@P�9@P �@O�@Ol�@OK�@O;d@N�@Nff@N@M@M�-@M`B@L��@Lz�@L�@K�
@Kt�@Ko@J�@J��@J-@I�@I�7@I�@I�@H��@H�u@HQ�@HA�@HA�@G�@Gl�@G+@F�+@F@E`B@EV@D�j@D�j@D�j@D��@D�D@Dz�@DI�@C��@CdZ@C"�@B��@BJ@A�^@A��@Ax�@AX@@��@@ �@?�;@?��@?l�@>��@>V@=��@=p�@=`B@=?}@=�@<�/@<�D@<(�@;�F@;dZ@;33@:�H@:��@:J@9�7@97L@8�9@7��@7��@7��@7�P@7l�@7
=@6��@6ff@6E�@5�@5�-@5�@4��@4�@3�
@3C�@3"�@3@2�!@2n�@2�@1�@1��@1�7@1G�@0Ĝ@0 �@0  @/�;@/�w@/�P@/K�@/K�@/+@.�R@.V@-�@-�@-�@,z�@,j@,Z@,Z@,Z@,Z@,I�@+�m@+��@+�@+t�@+t�@+�@+dZ@+33@+o@*�H@*��@*�!@*~�@*M�@)�@)��@)��@)hs@)&�@)�@)%@(�`@(��@(b@'�@'|�@'
=@&ȴ@&ff@%�h@$��@$��@$(�@#��@#�
@#ƨ@#�F@#�F@#��@#�@#C�@#@"�!@"J@!��@!7L@ ��@ Ĝ@ �@  �@   @�@�@��@��@��@��@��@+@��@�@��@v�@�@��@��@?}@��@�j@�D@�@��@��@�m@1@9X@�m@t�@t�@C�@o@�!@=q@�#@�#@��@hs@G�@&�@�u@r�@Q�@1'@  @�@��@��@�@ff@E�@E�@�T@��@z�@Z@Z@I�@(�@�@ƨ@C�@"�@"�@@�H@�\@^5@J@�#@�7@hs@G�@��@�9@Q�@ �@�;@K�@ȴ@�+@V@V@5?@��@p�@/@V@�/@�@z�@(�@�m@�
@�F@��@t�@33@@
�@
��@
n�@
M�@
-@
-@	�@	�#@	�#@	��@	�^@	��@	��@	�7@	X@	&�@��@Ĝ@�u@�u@�u@�u@�u@r�@A�@  @�w@�@��@�P@|�@K�@;d@+@+A�7LA�9XA�1'A�5?A�?}A�E�A�=qA�S�A�S�A�Q�A�O�A�S�A�VA�S�A�Q�A�Q�A�VA�VA�Q�A�O�A�XA�XA�VA�S�A�VA�ZA�ZA�XA�S�A�XA�ZA�S�A�S�A�VA�VA�Q�A�O�A�VA�VA�K�A�O�A�M�A�S�A�Q�A�O�A�M�A�S�A�O�A�K�A�O�A�K�A�O�A�I�A�I�A�E�A�I�A�O�A�XA�S�A�\)A�^5A�^5A�VA�S�A�XA�VA�Q�A�O�A�O�A�E�A�C�A�=qA�G�A�O�A�VA�S�A�K�A�M�A�O�A�M�A�K�A�O�A�M�A�G�A�E�A�K�A�G�A�E�A�I�A�I�A�7LA��A�A�oA�1A�A��`A��mA׺^A׺^A׸RAײ-Aװ!A׸RA׶FA׮Aס�Aי�Aי�Aׇ+A�~�A�M�A��#A�$�Aհ!A�ffA��A�|�A���A�VA��A���A���Aҟ�Aҥ�AғuAҋDA҉7A҃A҇+AҍPA҉7A�x�A�jA�`BA�VA�M�A�?}A�33A�-A�+A�&�A�"�A��A�A��TA��
A���AѬAыDAуA�~�A�v�A�jA�bNA�\)A�VA�O�A�E�A�;dA�33A�1'A�-A�$�A��A�oA�{A�oA�1A���A��mA��TA��TA��
AиRAЩ�AБhA�t�A�I�A�$�A�1A���A���A���A���A��A��A��A��A��#A���A���A���A���A���A���A�AϾwAϺ^AϸRAϲ-AϬAϥ�Aϣ�Aϡ�Aϙ�AϑhAϏ\AϑhAϏ\AύPAϋDAύPAω7AσA�z�A�r�A�p�A�jA�ffA�bNA�^5A�S�A�G�A�I�A�G�A�C�A�=qA�;dA�"�A��A��A��A��A�
=A���A��HA���A�A�A���AθRAΰ!AήAάAΩ�AΝ�A΃A�r�A�jA�G�A�A�A�9XA�{A���A���Aͺ^AͲ-AͬAͩ�Aͥ�A͟�A͓uA͉7A̓A�z�A�t�A�l�A�ffA�bNA�bNA�^5A�\)A�VA�VA�VA�S�A�M�A�G�A�A�A�5?A�-A�$�A��A�A��`A���A�ƨA̮Ȧ+A�M�A�=qA�/A���A���A�ĜA�ƨA���A���A��#A��HA���A��A���A�ȴA���A˴9A˥�A˝�A˙�Aˏ\A˅A�n�A�9XA�33A�$�A�bA�%A��;AʶFAʍPA�v�A�r�A�l�A�bNA�VA�S�A�M�A�C�A�;dA�;dA�9XA�7LA�/A�&�A��A��A��A�JA�
=A�A���A���A��TA��;A��#A�ȴAɺ^Aɣ�AɅA�bNA�9XA�$�A�%A��
A�x�A�-A�{A���A��A��#AǾwAǗ�AǅA�z�A�l�A�ffA�dZA�bNA�`BA�^5A�ZA�VA�E�A�(�A�{A�JA�  A��A��HA��
A�ĜAƧ�AƃA�O�A�1'A� �A�%A��#AŬA�|�A�XA�7LA�A��
Aĩ�A�M�A�/A�A�ƨAô9AÝ�A�XA�1'A��A�A�A� �A��!A�p�A�1'A���A��;A��A�9XA���A��`A���A��A�n�A�1'A�{A���A�VA�O�A���A��A���A�1'A��yA��TA���A�\)A�VA���A�VA�7LA��PA��#A�?}A��;A�M�A��jA��hA��A�n�A�ffA�`BA�I�A�9XA�1A���A�7LA�bNA�+A��A��A�?}A�I�A�;dA��A��A�n�A���A�/A�G�A��A��wA���A�/A�dZA���A���A�l�A�oA��#A��^A�x�A�%A��^A�dZA��A�hsA�K�A�?}A��A��mA��RA��A�z�A�r�A�^5A�VA�=qA�5?A� �A�A��;A��!A�p�A��yA���A��7A�^5A�33A���A���A�S�A�A��A�bNA�+A��`A�n�A�ĜA�r�A�E�A�A�E�A�A���A��7A�jA�\)A�O�A�C�A�"�A��;A��-A���A�p�A�\)A�I�A��A��^A�33A�x�A�|�A�JA�ƨA��A�M�A�%A��#A���A�~�A�Q�A�  A�ƨA�l�A�C�A�/A�"�A���A��A��
A�ȴA��!A�Q�A�ƨA��DA�v�A�r�A�~�A�Q�A�5?A�/A��A��A��`A���A���A���A���A���A���A��hA��7A�t�A�hsA�l�A�E�A��A��jA���A��\A���A�p�A�A��HA��7A���A�E�A�^5A�VA�-A�&�A�"�A�bNA�ȴA���A�hsA�ĜA��A�(�A���A���A�O�A���A���A�|�A�/A�A��A��9A�n�A�VA��PA���A���A��HA�Q�A�AC�A~ �A}&�A|^5A{�FA{t�A{/AzM�Ax��AwƨAvffAu��Au
=At�AtAs��AsK�Ar��Ar�!Ar��Ar��Ar �Ap�RAnn�Ak/Ai7LAh�+Ag��Ag�AfȴAf��AfM�Ae��Ae��Ael�Ae7LAe
=Ad��Ad�9Ad��AdQ�Ac��Ac|�AcoAb�yAb=qAa�#Aa��Aa�Aa|�Aa`BAaC�Aa?}Aa33Aa+AaoAa
=A`��A`��A`�A`�A`��A`��A`�A`��A`��A`��A`��A`�\A`v�A`M�A`1'A` �A_��A_�TA_��A_�PA_p�A_S�A_%A^��A^�A^�A^�RA^�A^VA^{A]��A]�7A]&�A]
=A\�!A\VA\-A[�A[|�AZ�yAZ��AZ��AZ~�AY�AYp�AY%AX�`AXE�AWXAV��AV�+AV-AU�;AU��AT��ATI�AS��AR�+AQ��AQ��AQ�AQhsAQ\)AQXAQ?}AQ+AQ�AP�AP��AP�jAP~�APz�APv�APbNAP5?AP$�AP1AO�TAO�#AOAO�^AO��AO+AOAN�\AM%AK?}AJM�AI��AIXAHȴAHZAH=qAHbAG�;AG�
AG�
AG�
AGAG�-AG��AG��AG�hAG�hAG�hAGx�AGhsAGXAGO�AGG�AGC�AG;dAG33AG�AGVAF�9AFI�AE�#AEXAEVAD��AD�AD��ADĜAD��AD�RAD��AD�\ADv�AD^5AD9XAD1AC��AC��ACl�ACO�AC+AB�AB�!ABI�AA��AA;dAA
=A@�`A@ĜA@�DA@ffA@I�A@9XA@(�A@JA?�#A?��A?x�A?G�A>�/A>v�A>M�A>bA=��A=��A=�PA=\)A=C�A=33A=�A=A<�A<��A<�9A<��A<~�A<1'A;�mA;��A;�PG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        A�7LA�I�A�Q�A�Q�A�S�A�S�A�VA�XA�XA�VA�S�A�Q�A�O�A�O�A�K�A�ZA�O�A�K�A�M�A�I�A� �A��A׮A�?}AԸRA���A҇+A�XA�"�AѼjA�jA�;dA�{A���A�+A��
AϏ\A�/A΍PA͗�A�E�A�C�A˼jA���A�5?A���A�hsA�hsA��#Aŉ7Aß�A��A��A��-A�v�A�7LA�K�A�;dA�7LA��TA���A�E�A��A�`BA�VA��jA���A�1A�hsA�1A��wA�t�A���A��\A��A���A�5?A�$�A�;dA�A��#A}�hAyXAt~�ArbNAip�AeO�Abn�AaoA`��A_�A]�AZz�AU�AQdZAP=qAL�AG��AGp�AF$�AD�DAB�9A@ZA>n�A<�/A:I�A6�A3�;A0�HA/oA-S�A)ƨA)�A*ffA)��A*{A(ZA&bA$�yA%S�A%x�A%�A%�#A'A(VA(bA't�A&��A&ffA&v�A'%A(E�A(A'�A&ȴA&��A&jA%�TA$�A#��A#dZA"��A!�A!�A!C�A I�AO�AffA��A;dA��A�!AZA�TA/AĜA�A�wAC�A�RAQ�A�A�AhsA?}A�A
=A��A�`A�/A�\A �A�mA�PA�AZAbA��A�A��Ap�A
=A��A��A�\An�AI�A{AA|�A\)A
=Ar�A9XA�#At�A`BAK�A%A�!AbNAbA��A�PAO�A+A%A
�A
��A
��A
r�A
�A	��A	p�A	C�A�yA�\A �A�FA��Ar�A�A�TA�PA�A�!AZA��A�FA�A?}AVA�AȴA��A��Az�AA�A�Al�AG�A&�A �jA �uA r�A ZA =qA 1@�ƨ@��@�5?@���@�X@��`@���@��@��@�K�@��R@�^5@��-@�O�@�Ĝ@�1@���@��!@��-@�bN@�P@��y@��@�Z@�F@�\)@��H@�V@���@�`B@�r�@땁@�"�@ꗍ@��@�hs@�z�@�1@�l�@�
=@���@�-@�O�@���@�bN@��
@�|�@�"�@��H@�R@�E�@��@�7@�&�@�V@��@��;@�33@��@�Q�@۶F@�"�@ڧ�@�E�@���@�&�@��@��@���@�z�@��@�b@��
@���@֧�@��T@�x�@�7L@ԛ�@ӝ�@��@�5?@Ѳ-@�O�@���@Гu@�(�@Ͼw@�l�@��H@�^5@�@�`B@��`@̛�@�Z@��m@˝�@�"�@�V@��@�O�@ȓu@�Z@�b@�l�@���@�ȴ@�-@š�@�O�@���@Ĵ9@ě�@�Z@��@�33@�~�@�J@���@��h@�/@�V@�j@��@��y@�5?@���@��^@���@���@�`B@��@�V@���@�Ĝ@�(�@�dZ@��H@��+@�E�@�J@�x�@�O�@��@���@���@��@��u@�b@���@��@�K�@��y@�E�@���@���@��-@���@�hs@���@���@�1'@��@�;d@�ȴ@��!@���@���@���@���@��9@�1@��w@��@�t�@�33@��@��y@��@���@���@���@���@��\@�=q@��#@�V@���@���@��u@�r�@�1@��P@�o@�ȴ@�~�@���@�X@��/@���@�r�@�(�@���@�ƨ@���@�n�@�@��`@��@���@��@�I�@�ƨ@�33@���@�n�@�-@��^@��@��@�I�@��
@�\)@�
=@��y@�ff@��#@���@�G�@��@���@�I�@�ƨ@�dZ@�o@��H@���@�$�@���@��7@��@�?}@�Ĝ@� �@��@�S�@�33@�33@�+@���@�ȴ@���@��\@�-@���@���@�7L@��/@��u@�r�@�Z@�I�@� �@���@�ȴ@�~�@�v�@�V@�5?@��^@�x�@�X@�X@��@���@�z�@�1'@��@��@�ƨ@�+@�^5@�@���@�/@��/@��9@�z�@�(�@���@�C�@�ȴ@���@�J@���@��-@�p�@�&�@��@���@��u@�Q�@��;@��w@��@�|�@�K�@���@�ȴ@���@�^5@��@�@���@�p�@��@��/@���@���@�bN@�1'@��@�ƨ@���@��P@�\)@�33@���@�v�@�-@�@�hs@��@�%@��/@���@��9@�r�@�9X@��@�@�w@K�@~�@~@}/@|�j@{�m@{t�@{"�@z��@z=q@y�^@xĜ@xb@x  @w��@w;d@v��@v{@u�@t��@tj@t9X@t�@t1@t1@s��@r��@r��@r=q@qX@p��@pĜ@p�@o�@o|�@o
=@nv�@n@m�h@m�@lj@l�@k�F@kt�@k33@j��@j-@i�7@iG�@h��@h�u@g�@gK�@g+@f�@f�+@fv�@f$�@e��@e?}@eV@d�/@d(�@c��@c33@c@b��@b�!@bM�@a�#@a�7@aG�@`Ĝ@`Q�@_��@^�y@^{@]�@]?}@\�/@\Z@[�F@[o@Z�@Z�!@Z��@Z~�@Z^5@Y��@YG�@X��@X��@XA�@W�@W�w@W\)@W;d@V��@V5?@U�T@U��@T�/@T�@S�
@So@R�!@Rn�@R=q@Q�#@Q�7@Q%@P�9@P �@O�@Ol�@OK�@O;d@N�@Nff@N@M@M�-@M`B@L��@Lz�@L�@K�
@Kt�@Ko@J�@J��@J-@I�@I�7@I�@I�@H��@H�u@HQ�@HA�@HA�@G�@Gl�@G+@F�+@F@E`B@EV@D�j@D�j@D�j@D��@D�D@Dz�@DI�@C��@CdZ@C"�@B��@BJ@A�^@A��@Ax�@AX@@��@@ �@?�;@?��@?l�@>��@>V@=��@=p�@=`B@=?}@=�@<�/@<�D@<(�@;�F@;dZ@;33@:�H@:��@:J@9�7@97L@8�9@7��@7��@7��@7�P@7l�@7
=@6��@6ff@6E�@5�@5�-@5�@4��@4�@3�
@3C�@3"�@3@2�!@2n�@2�@1�@1��@1�7@1G�@0Ĝ@0 �@0  @/�;@/�w@/�P@/K�@/K�@/+@.�R@.V@-�@-�@-�@,z�@,j@,Z@,Z@,Z@,Z@,I�@+�m@+��@+�@+t�@+t�@+�@+dZ@+33@+o@*�H@*��@*�!@*~�@*M�@)�@)��@)��@)hs@)&�@)�@)%@(�`@(��@(b@'�@'|�@'
=@&ȴ@&ff@%�h@$��@$��@$(�@#��@#�
@#ƨ@#�F@#�F@#��@#�@#C�@#@"�!@"J@!��@!7L@ ��@ Ĝ@ �@  �@   @�@�@��@��@��@��@��@+@��@�@��@v�@�@��@��@?}@��@�j@�D@�@��@��@�m@1@9X@�m@t�@t�@C�@o@�!@=q@�#@�#@��@hs@G�@&�@�u@r�@Q�@1'@  @�@��@��@�@ff@E�@E�@�T@��@z�@Z@Z@I�@(�@�@ƨ@C�@"�@"�@@�H@�\@^5@J@�#@�7@hs@G�@��@�9@Q�@ �@�;@K�@ȴ@�+@V@V@5?@��@p�@/@V@�/@�@z�@(�@�m@�
@�F@��@t�@33@@
�@
��@
n�@
M�@
-@
-@	�@	�#@	�#@	��@	�^@	��@	��@	�7@	X@	&�@��@Ĝ@�u@�u@�u@�u@�u@r�@A�@  @�w@�@��@�P@|�@K�@;d@+G�O�A�7LA�9XA�1'A�5?A�?}A�E�A�=qA�S�A�S�A�Q�A�O�A�S�A�VA�S�A�Q�A�Q�A�VA�VA�Q�A�O�A�XA�XA�VA�S�A�VA�ZA�ZA�XA�S�A�XA�ZA�S�A�S�A�VA�VA�Q�A�O�A�VA�VA�K�A�O�A�M�A�S�A�Q�A�O�A�M�A�S�A�O�A�K�A�O�A�K�A�O�A�I�A�I�A�E�A�I�A�O�A�XA�S�A�\)A�^5A�^5A�VA�S�A�XA�VA�Q�A�O�A�O�A�E�A�C�A�=qA�G�A�O�A�VA�S�A�K�A�M�A�O�A�M�A�K�A�O�A�M�A�G�A�E�A�K�A�G�A�E�A�I�A�I�A�7LA��A�A�oA�1A�A��`A��mA׺^A׺^A׸RAײ-Aװ!A׸RA׶FA׮Aס�Aי�Aי�Aׇ+A�~�A�M�A��#A�$�Aհ!A�ffA��A�|�A���A�VA��A���A���Aҟ�Aҥ�AғuAҋDA҉7A҃A҇+AҍPA҉7A�x�A�jA�`BA�VA�M�A�?}A�33A�-A�+A�&�A�"�A��A�A��TA��
A���AѬAыDAуA�~�A�v�A�jA�bNA�\)A�VA�O�A�E�A�;dA�33A�1'A�-A�$�A��A�oA�{A�oA�1A���A��mA��TA��TA��
AиRAЩ�AБhA�t�A�I�A�$�A�1A���A���A���A���A��A��A��A��A��#A���A���A���A���A���A���A�AϾwAϺ^AϸRAϲ-AϬAϥ�Aϣ�Aϡ�Aϙ�AϑhAϏ\AϑhAϏ\AύPAϋDAύPAω7AσA�z�A�r�A�p�A�jA�ffA�bNA�^5A�S�A�G�A�I�A�G�A�C�A�=qA�;dA�"�A��A��A��A��A�
=A���A��HA���A�A�A���AθRAΰ!AήAάAΩ�AΝ�A΃A�r�A�jA�G�A�A�A�9XA�{A���A���Aͺ^AͲ-AͬAͩ�Aͥ�A͟�A͓uA͉7A̓A�z�A�t�A�l�A�ffA�bNA�bNA�^5A�\)A�VA�VA�VA�S�A�M�A�G�A�A�A�5?A�-A�$�A��A�A��`A���A�ƨA̮Ȧ+A�M�A�=qA�/A���A���A�ĜA�ƨA���A���A��#A��HA���A��A���A�ȴA���A˴9A˥�A˝�A˙�Aˏ\A˅A�n�A�9XA�33A�$�A�bA�%A��;AʶFAʍPA�v�A�r�A�l�A�bNA�VA�S�A�M�A�C�A�;dA�;dA�9XA�7LA�/A�&�A��A��A��A�JA�
=A�A���A���A��TA��;A��#A�ȴAɺ^Aɣ�AɅA�bNA�9XA�$�A�%A��
A�x�A�-A�{A���A��A��#AǾwAǗ�AǅA�z�A�l�A�ffA�dZA�bNA�`BA�^5A�ZA�VA�E�A�(�A�{A�JA�  A��A��HA��
A�ĜAƧ�AƃA�O�A�1'A� �A�%A��#AŬA�|�A�XA�7LA�A��
Aĩ�A�M�A�/A�A�ƨAô9AÝ�A�XA�1'A��A�A�A� �A��!A�p�A�1'A���A��;A��A�9XA���A��`A���A��A�n�A�1'A�{A���A�VA�O�A���A��A���A�1'A��yA��TA���A�\)A�VA���A�VA�7LA��PA��#A�?}A��;A�M�A��jA��hA��A�n�A�ffA�`BA�I�A�9XA�1A���A�7LA�bNA�+A��A��A�?}A�I�A�;dA��A��A�n�A���A�/A�G�A��A��wA���A�/A�dZA���A���A�l�A�oA��#A��^A�x�A�%A��^A�dZA��A�hsA�K�A�?}A��A��mA��RA��A�z�A�r�A�^5A�VA�=qA�5?A� �A�A��;A��!A�p�A��yA���A��7A�^5A�33A���A���A�S�A�A��A�bNA�+A��`A�n�A�ĜA�r�A�E�A�A�E�A�A���A��7A�jA�\)A�O�A�C�A�"�A��;A��-A���A�p�A�\)A�I�A��A��^A�33A�x�A�|�A�JA�ƨA��A�M�A�%A��#A���A�~�A�Q�A�  A�ƨA�l�A�C�A�/A�"�A���A��A��
A�ȴA��!A�Q�A�ƨA��DA�v�A�r�A�~�A�Q�A�5?A�/A��A��A��`A���A���A���A���A���A���A��hA��7A�t�A�hsA�l�A�E�A��A��jA���A��\A���A�p�A�A��HA��7A���A�E�A�^5A�VA�-A�&�A�"�A�bNA�ȴA���A�hsA�ĜA��A�(�A���A���A�O�A���A���A�|�A�/A�A��A��9A�n�A�VA��PA���A���A��HA�Q�A�AC�A~ �A}&�A|^5A{�FA{t�A{/AzM�Ax��AwƨAvffAu��Au
=At�AtAs��AsK�Ar��Ar�!Ar��Ar��Ar �Ap�RAnn�Ak/Ai7LAh�+Ag��Ag�AfȴAf��AfM�Ae��Ae��Ael�Ae7LAe
=Ad��Ad�9Ad��AdQ�Ac��Ac|�AcoAb�yAb=qAa�#Aa��Aa�Aa|�Aa`BAaC�Aa?}Aa33Aa+AaoAa
=A`��A`��A`�A`�A`��A`��A`�A`��A`��A`��A`��A`�\A`v�A`M�A`1'A` �A_��A_�TA_��A_�PA_p�A_S�A_%A^��A^�A^�A^�RA^�A^VA^{A]��A]�7A]&�A]
=A\�!A\VA\-A[�A[|�AZ�yAZ��AZ��AZ~�AY�AYp�AY%AX�`AXE�AWXAV��AV�+AV-AU�;AU��AT��ATI�AS��AR�+AQ��AQ��AQ�AQhsAQ\)AQXAQ?}AQ+AQ�AP�AP��AP�jAP~�APz�APv�APbNAP5?AP$�AP1AO�TAO�#AOAO�^AO��AO+AOAN�\AM%AK?}AJM�AI��AIXAHȴAHZAH=qAHbAG�;AG�
AG�
AG�
AGAG�-AG��AG��AG�hAG�hAG�hAGx�AGhsAGXAGO�AGG�AGC�AG;dAG33AG�AGVAF�9AFI�AE�#AEXAEVAD��AD�AD��ADĜAD��AD�RAD��AD�\ADv�AD^5AD9XAD1AC��AC��ACl�ACO�AC+AB�AB�!ABI�AA��AA;dAA
=A@�`A@ĜA@�DA@ffA@I�A@9XA@(�A@JA?�#A?��A?x�A?G�A>�/A>v�A>M�A>bA=��A=��A=�PA=\)A=C�A=33A=�A=A<�A<��A<�9A<��A<~�A<1'A;�mA;��A;�PG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�XB��B�*B��B�XB��B��B�_B��B��B�XB��B��B��B��B��B�zB�B��B��B�	B��B�PBc B?HBFBK�BGBA�B:�B<B?BF�B]/BjBs�B�+B��B��B	B	�XB
2-B
�AB
�!B
�9B
��B�B!B*0B+6BxB
��B
��B
�B
�dB
�B&BS�B?HB"�B�B
��B
��B
�2B
�XB
�B
�zB
�DB
�@B
�XB
��B
��B
~�B
p;B
S�B
xB	�B	��B	�OB	��B	~�B	kB	P}B	_;B	r|B	U2B	NB	F?B	CaB	?B	:�B	7�B	?�B	=<B	2�B	,�B	*eB	)*B	1�B	3hB	;dB	=<B	C�B	FB	Q�B	R B	@�B	�B	%B	{B	�B	�B	@�B	`vB	r�B	v�B	rB	{B	�B	�bB	��B	�vB	��B
�B
=B
B
�B
1B
VB
+�B
EB
U�B
[�B
]�B
]/B
\]B
^5B
bB
b�B
_;B
b�B
c�B
b�B
dZB
`�B
`B
aB
]�B
^B
\�B
\]B
\�B
\]B
]/B
]�B
`BB
`�B
a�B
_;B
_�B
^jB
\]B
[�B
\)B
[�B
[�B
[WB
Z�B
ZB
\�B
\�B
\�B
\�B
[WB
ZQB
XEB
W�B
VmB
U2B
U�B
RTB
Q�B
R B
Q�B
P�B
QB
P}B
QB
PB
OBB
O�B
O�B
N�B
N�B
M�B
MB
L�B
MjB
MB
K�B
L0B
K)B
J�B
JXB
I�B
I�B
IB
H�B
HKB
G�B
GzB
F�B
E�B
D�B
EB
C-B
B�B
A B
B�B
B'B
A�B
@�B
AUB
@�B
?HB
>wB
>B
=B
=B
<�B
;�B
;�B
;dB
<B
;�B
;dB
;�B
;�B
:*B
:*B
9�B
:*B
8�B
8RB
8B
7�B
6�B
6B
5?B
4�B
2�B
2�B
2-B
1[B
1�B
/�B
/�B
.IB
.IB
,�B
,qB
+kB
*eB
(�B
(�B
'B
&LB
%B
$B
$@B
!�B
!�B
 �B
 �B
 \B
�B
!B
 �B
 'B
�B
!B
�B
�B
�B
�B
�B
B
CB
CB
�B
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
�B
SB
B
B
B
�B
�B
�B
�B
�B
�B
B
B

�B

�B

rB

�B
	�B
	7B
DB
�B

=B
	�B

rB

rB
�B
DB
	�B
	�B
1B
	�B
	B
	B
	B
	B
�B
	�B
	�B

=B
B

�B

	B
JB
�B

=B
DB

�B
�B
	B
�B
�B
	lB

�B

=B

�B

�B
B

�B

=B

	B
	�B
	lB

rB
�B
PB
JB
�B
B
�B
�B
PB
PB
�B
.B
�B
�B
�B
�B
�B
�B
�B
�B
�B
4B
hB
�B
�B
�B
B
B
�B
B
�B
B
B
�B
{B
@B
uB
@B
uB
�B
uB
B
�B
�B
B
�B
B
�B
FB
MB
�B
{B
FB
�B
�B
MB
�B
�B
B
�B
�B
B
B
�B
SB
B
�B
�B
�B
$B
�B
_B
�B
B
�B
�B
�B
�B
�B
�B
	B
kB
=B
�B
B
�B
�B
B
B
�B
B
xB
CB
�B
CB
B
B
�B
qB
qB
�B
�B
�B
�B
�B
�B
�B
�B
CB
�B
�B
B
xB
�B
IB
B
�B
�B
B
B
OB
OB
B
�B
�B
�B
�B
 'B
!bB
"4B
!�B
!�B
!�B
!�B
!�B
"4B
"4B
"4B
!�B
"4B
"hB
"�B
#B
#:B
#nB
#�B
#�B
#nB
#nB
#�B
$�B
$@B
$tB
$�B
$tB
%FB
%B
$�B
$tB
%B
$�B
%�B
%�B
&B
&�B
&�B
(XB
)*B
)*B
)�B
)_B
)�B
)�B
*eB
*�B
+6B
,B
,�B
,=B
.B
-�B
.B
.�B
/�B
/�B
0�B
0!B
0�B
1�B
1'B
1�B
1�B
1�B
2�B
1�B
2�B
2-B
33B
2�B
2�B
2�B
3hB
2�B
2�B
33B
3�B
3�B
4B
33B
3hB
3�B
4�B
4nB
4�B
4B
3�B
49B
5B
4�B
4�B
5B
5B
5B
5B
5B
5B
4�B
4�B
5?B
5tB
5�B
5�B
6B
6�B
6zB
6zB
6�B
6�B
6�B
8RB
7�B
7�B
7�B
8RB
9$B
9$B
9�B
:*B
:*B
:*B
:*B
9�B
9�B
:�B
:�B
:�B
;�B
<jB
<jB
=�B
>wB
>BB
>B
=�B
>B
=qB
=�B
>BB
>wB
>wB
>�B
>�B
?}B
?�B
@�B
AUB
AUB
B'B
A�B
C-B
C�B
C�B
C�B
C�B
C�B
C�B
DgB
E9B
E9B
E9B
F?B
FtB
F�B
GB
GEB
GzB
H�B
IB
IB
IB
IRB
IB
IB
I�B
J�B
J�B
J�B
J�B
K)B
K�B
K�B
K�B
K�B
K�B
K�B
L0B
LdB
M6B
M6B
M6B
M�B
M�B
M�B
N<B
NB
N<B
N�B
N�B
N�B
O�B
PB
PB
P�B
QB
P�B
QB
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
R�B
R�B
S&B
S[B
S�B
S�B
S�B
S�B
T,B
TaB
T�B
T�B
U2B
UgB
UgB
U�B
VB
U�B
VmB
VmB
VmB
VmB
W
B
W
B
W
B
V�B
W
B
W�B
W�B
XB
XEB
YB
YB
YKB
YKB
YB
YKB
YKB
YB
YB
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
\]B
\�B
]/B
]dB
]/B
]dB
]dB
]�B
]�B
^B
^5B
^jB
^jB
^�B
^jB
_;B
_pB
_�B
`B
aHB
aB
`�B
`�B
`�B
aHB
a�B
a�B
a�B
a�B
bB
bB
b�B
cTB
c�B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
e,B
e,B
e`B
ffB
f�B
f�B
f�B
f�B
gB
gB
g8B
g8B
gmB
g�B
h
B
h>B
iDB
i�B
i�B
jKB
jB
jB
jB
jB
jB
jB
jB
jB
jB
j�B
j�B
kB
kB
k�B
lWB
l�B
m]B
m]B
m�B
m�B
m�B
o5B
o�B
pB
pB
poB
p�B
p;B
p;B
p;B
oiB
n�B
n/B
m�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
ncB
n�B
n�B
pB
p;B
poB
p�B
p�B
p�B
p�B
qvB
q�B
q�B
q�B
q�B
rB
sB
sB
r�B
sMB
t�B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
v�B
v�B
wfB
xB
x8B
x�B
x�B
x8B
x�B
zB
zDB
zDB
y�B
zxB
zxB
|B
|PB
|PB
|PB
}"B
|�B
|�B
|�B
}VB
}VB
}�B
}�B
~(B
~]B
~(B
~�B
~�B
.B
~�B
~]B
~]B
~�B
~�B
~�B
cB
cB
cB
cB
�B
�B
�B
�B
�B
� B
�4B
�4B
��B
��B
�;B
�oB
�;B
�B
��B
�;B
�oB
�oB
�B
�oB
�;B
�B
��B
��B
�;B
��B
��B
�{B
�B
��B
�B
��B
�B
��B
��B
��B
��B
��B
�SB
�%B
��B
��B
�%B
��B
�YB
�%B
�YB
�YB
��B
��B
��B
�+B
�_B
��B
��B
��B
��B
��B
�1B
��B
�fB
��B
�fB
�7B
�7B
�B
�7B
��B
�	B
�lB��B��B��B��B�$B��B��B�B��B��B�_B��B��B�_B�0B��B��B��B��B�$B��B��B�_B�B��B��B�XB��B�eB�*B�*B�_B��B�$B��B�XB�*B�$B�B�XB��B�eB��B��B�XB��B��B��B�*B��B��B�RB�LB�@B��B��B�tB�B��B��B�$B��B��B��B�B��B�XB��B�RB�0B��B��B��B�B�@B�B��B�LB�FB��B�LB�B�tB�LB��B��B��B�FB��B�nB��B��B��B��B��B��B��B��B�=B��B��B��B��B��B��B��B��B�DB��B�PB�B��B�$B�Bm)Bm�BpBXEBk�BB�BJ�B;dB;�B<B9$BF�BA�BB'BC�BEBIBK�BM6BOBMBJ�BJ�BH�BH�BH�BGzBF?BE�BFtBF�BK�B?}B?�BDgB=�B=qB:�B:*B:�B;dB;dB;dB:�B:�B<�B<�B;�B;0B;�B?B?HB?B?B?�BAUBE9BE9BD3BFtBJ�BJ�BMjBPBT�B^Bc�Bg8Bf2Bd�Be,BffBg�Bf�BffBjBkQBk�BkBjKBi�Bl�Bl�Bm�Bm�Bm�Bn�Bo�Bq�BqBp�Br�Bt�Bt�Bs�BsMBs�Bt�Bs�BtBt�Bv�BxBw�By	By	B{B}VB� B�AB��B��B�MB��B��B�	B�B�JB��B�JB��B�oB�B��B��B��B�_B��B�7B��B�kB�1B��B�VB�!B��B��B�B��B�B��B�qB��BбB��B��B�]B�jB��B��B�B��B	GB	�B		lB	JB	xB	PB	"B	 B	�B	oB	�B	�B	�B	#nB	&�B	)_B	+�B	7LB	GzB	ZB	e�B	j�B	{B	�1B	��B	�!B	�B	ɆB	��B	�9B	�sB	�B	��B	�B	�GB	��B
"4B
8RB
?�B
D�B
HB
K�B
NB
PB
R B
T,B
X�B
k�B
m�B
poB
s�B
v�B
�;B
��B
��B
��B
�MB
�B
��B
��B
�B
�=B
��B
��B
��B
��B
��B
�\B
��B
�@B
��B
��B
�B
�B
��B
��B
�B
�aB
�'B
��B
��B
�*B
��B
��B
��B
�?B
��B
�5B
�B�B4BBB�BB�B�B@BoB�B�B\BVB�B�B"B.B�BOB�B�BIBIBB�B�B!�B#�B(�B%�B$�B&LB+6B-�B+kB+6B+�B.B,qB/�B1[B-�B+6B0�B(�B+�B,B(�B!�B$B)�B,�B&�B!-B�B�B�BB�B�B�B
��B
��B
��B
�%B
��B
�cBoB1B
��B
�yB
�B
� B
�HB
�)B
�^B
��B
��B
�
B
�B
��B
�B
��B
�)B
��B
�yB
ҽB
��B
ȴB
��B
��B
�zB
ɆB
�KB
бB
ݘB
��B
רB
�B
҉B
��B
��B
�B�B�B�B5tBH�B@B+BeB;dBOBBV�Bc�BOBM6BP}BK�B>BB?BB�BA�B7�B=�BFB(�B&�B"hB)*B \B#�B�B:B4B BB�B	B�B�BGB�B
�>BuB
�B
�2B
��B
�B
�DB
�B
� B
�BB
�B
�dB
��B
�EB
�|B
��B
�B
��B
��B
��B
�B
ÖB
��B
��B
�}B
�wB
��B
�IB
�[B
�$B
�FB
��B
�hB
��B
��B
��B
��B
�hB
�_B
��B
��B
��B
�B
�{B
��B
�SB
~]B
��B
��B
�B
��B
��B
�B
��B
�B
�tB
�nB
�B
�bB
�aB
��B
�_B
��B
��B
��B
��B
�{B
�=B
��B
�~B
��B
��B
��B
� B
}�B
��B
}�B
|�B
xB
{B
� B
yrB
|�B
}�B
i�B
`vB
d�B
X�B
]�B
v+B
B'B
`B
�B
$@B
I�B
 \B
�B
B

�B	�TB	��B	��B	��B	�B	��B	��B	ɆB	�B	�XB	�[B	�B	�wB	��B	�'B	��B	�0B	�IB	��B	��B	�9B	�-B	�=B	��B	�B	�B	��B	�iB	v�B	s�B	d�B	g8B	y>B	c�B	ffB	gmB	U2B	RTB	R�B	K�B	E9B	W
B	W?B	X�B	W
B	Y�B	h�B	|�B	�oB	�tB	{JB	_pB	qvB	b�B	_B	W?B	^�B	ZB	U2B	S&B	U�B	S�B	T,B	P�B	P�B	RTB	J�B	U�B	I�B	J�B	M�B	H�B	J#B	GEB	GB	K�B	GzB	FB	F�B	DgB	HB	EmB	GB	EmB	E�B	CaB	B�B	C�B	F?B	C�B	CaB	B�B	CaB	CaB	C-B	C�B	A�B	>�B	?�B	A�B	>�B	>�B	?�B	=�B	A�B	;dB	:*B	;dB	=B	<�B	:�B	:�B	;dB	8�B	;�B	3hB	6zB	EB	9�B	6�B	<6B	8�B	6FB	4�B	5�B	9�B	3hB	4nB	0!B	GzB	T�B	>wB	8�B	>�B	7�B	;�B	DgB	E9B	A�B	XB	>�B	@OB	=<B	<B	:�B	9$B	9�B	7B	8�B	?}B	:�B	5B	3hB	2�B	+�B	4nB	6FB	0UB	4B	2aB	+kB	/�B	)�B	/�B	&�B	"hB	)�B	>�B	-�B	-CB	($B	'B	5�B	.}B	)�B	,�B	*eB	($B	'�B	'�B	&�B	(�B	($B	)*B	'�B	'RB	&�B	)�B	*0B	*�B	*�B	*0B	)*B	)�B	*�B	,�B	,=B	4nB	0�B	5�B	6zB	6�B	49B	4nB	5B	1�B	1�B	2�B	6B	2-B	2�B	4�B	1'B	5tB	8�B	6B	7�B	4�B	7�B	7�B	<B	A�B	K�B	@B	=�B	?}B	=B	>�B	>B	;dB	:^B	:�B	=<B	?}B	>�B	:^B	@�B	JXB	C�B	DgB	FB	A�B	EB	GEB	F�B	DgB	DgB	F�B	DgB	F?B	G�B	EB	D�B	GzB	I�B	L�B	I�B	F�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        B�*B�>B��B�*B�B�sB��B�B��B�*B�B�sB�
B�
B��B�B��B��B�LB��B��B�]B�$B�Bm]BAoBGBL�BI7BCGB<B<�B@�BJ#B^�Bl"BvFB��B��B�>B	#�B	��B
6+B
�mB
��B
��B�BFB&�B5�B;B2�B2B
�B
�B
��B
�fB2GB`vBJrB'�B�B�B
��B
�:B
� B
��B
�B
�QB
�qB
��B
��B
��B
��B
y�B
p;B
4�B	�B	�HB	B	��B	�B	zDB	Z7B	{B	�4B	^�B	R�B	HKB	GEB	E�B	E�B	GB	N"B	BB	>wB	<jB	,�B	-�B	7�B	9�B	C-B	C�B	IRB	O\B	^5B	[�B	KB	!|B	B	�B	DB	B	BB	`'B	x�B	~wB	u�B	z^B	�1B	�-B	�KB	�^B	��B
mB
IB
5B
B
�B
IB
'�B
E�B
X�B
\�B
^�B
^B
^jB
a�B
ezB
d�B
a�B
d�B
d�B
d@B
g�B
dZB
c:B
c�B
_pB
_pB
]�B
]�B
^OB
^�B
^�B
`'B
a�B
b�B
c�B
`�B
`�B
_�B
\�B
\]B
\�B
[�B
[�B
[�B
[#B
[WB
^�B
]�B
]�B
^OB
]�B
[qB
Y�B
Y�B
X�B
YB
W$B
R�B
R�B
SB
RB
QNB
Q�B
Q�B
RB
P�B
P}B
Q�B
P�B
P.B
P.B
N"B
MjB
M�B
N�B
N"B
L�B
MB
LB
K�B
J�B
J�B
JXB
I�B
IlB
IB
H�B
IB
GzB
FYB
F%B
FYB
D�B
D�B
C�B
D�B
CaB
B�B
BAB
B�B
BB
@�B
?�B
?B
=�B
=�B
=VB
<�B
<B
;�B
<jB
<�B
<jB
=�B
<�B
:�B
:�B
:�B
:�B
9>B
8�B
8�B
88B
7�B
7�B
6zB
5�B
3�B
3�B
2�B
2�B
2aB
0;B
0�B
/ B
/�B
-�B
-wB
,�B
+QB
*�B
*�B
)�B
'�B
&fB
%�B
'B
#B
"NB
!|B
!�B
!�B
 'B
 �B
"NB
!B
�B
 \B
�B
 'B
�B
B
dB
�B
dB
�B
�B
�B
�B
�B
�B
sB
$B
�B
B
yB
�B
�B
YB
YB
�B
�B
aB
 B
�B
pB
�B
�B
6B
DB

�B
)B
^B
^B
	�B
	�B
�B
�B
�B

�B
B
�B
jB
�B

�B

�B
	B

rB
	�B
	�B
	�B
	�B
	�B

�B

XB
xB
�B
DB

�B
B
JB
)B
�B
�B

	B

XB
	B
�B

�B
xB

�B
�B
�B
�B
DB

�B

=B

#B

XB
�B
�B
"B
�B
0B
�B
B
�B
�B
�B
BB
�B
.B
B
�B
HB
B
�B
�B
B
4B
�B
oB
�B
[B
&B
B
uB
@B
[B
&B
[B
[B
�B
�B
�B
�B
B
�B
FB
�B
@B
�B
&B
�B
gB
�B
�B
�B
B
B
�B
{B
�B
�B
B
�B
mB
MB
B
9B
SB
�B
�B
mB
B
�B
B
�B
�B
�B
�B
WB
B
�B
B
�B
�B
�B
qB
�B
qB
xB
�B
�B
CB
]B
xB
xB
)B
5B
�B
�B
B
xB
CB
�B
�B
�B
xB
]B
)B
�B
B
�B
xB
�B
�B
�B
)B
�B
B
�B
~B
�B
~B
B
�B
�B
�B
�B
�B
VB
 �B
 'B
�B
 B
!B
"�B
#TB
!�B
"4B
!�B
!�B
"4B
"�B
"�B
"hB
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$B
#�B
#�B
$tB
%,B
%,B
$ZB
$�B
$�B
%`B
%�B
%FB
$�B
$�B
%�B
%�B
&2B
&B
&fB
'B
($B
)�B
)�B
)�B
*B
)�B
*B
*B
+B
+�B
,=B
,�B
-B
-]B
.}B
-�B
.�B
/OB
/�B
0�B
0�B
0�B
1�B
1�B
1[B
1�B
2aB
2�B
2�B
2aB
3B
2�B
3�B
3B
2�B
33B
3�B
33B
33B
3�B
4B
4TB
4TB
3�B
3�B
49B
4�B
4�B
5�B
4�B
4�B
4�B
5�B
5B
5%B
5?B
5?B
5�B
5tB
5ZB
5ZB
5B
5ZB
5�B
6FB
6�B
6`B
6�B
7B
6�B
6�B
7LB
7�B
7�B
8�B
8B
7�B
8�B
8�B
9�B
9�B
:xB
:�B
:^B
:DB
:DB
:B
:DB
;B
;0B
;dB
<�B
<�B
<�B
>(B
?B
>�B
>�B
>wB
>wB
=�B
>]B
>�B
>�B
>�B
?.B
?.B
?�B
@OB
AUB
A�B
A�B
BuB
B�B
C�B
C�B
C�B
DB
DB
DMB
DMB
D�B
EmB
EmB
E�B
F�B
F�B
GB
GEB
GzB
G�B
H�B
IlB
IlB
I�B
I�B
I�B
I�B
J�B
KB
J�B
J�B
KB
K�B
L�B
K�B
L0B
LB
LB
L0B
L�B
MB
M�B
MjB
M�B
M�B
NB
N<B
NpB
NVB
N�B
O(B
O(B
O�B
PbB
PbB
P�B
QB
QhB
QB
Q�B
Q�B
RB
RTB
R�B
R�B
SB
R�B
R�B
S&B
S�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
UB
UgB
U�B
U�B
U�B
VB
VSB
V9B
V�B
VmB
V�B
V�B
W?B
W$B
W
B
W$B
W�B
W�B
XEB
X�B
X�B
YeB
YeB
YKB
YKB
Y1B
YeB
YeB
YKB
ZB
ZB
ZB
ZQB
Z�B
[	B
Z�B
Z�B
[#B
[#B
[�B
\B
\B
[�B
\CB
\�B
]dB
]dB
]~B
]IB
]�B
]�B
]�B
^5B
^jB
^�B
^�B
^�B
^�B
_B
_�B
_�B
`'B
`�B
a|B
aB
`�B
`�B
aHB
a�B
b4B
a�B
a�B
b4B
bNB
b�B
c�B
c�B
dB
c�B
d&B
dtB
d�B
eB
e,B
e,B
ezB
ezB
e�B
gB
f�B
f�B
f�B
gB
g8B
gB
gmB
g�B
g�B
h>B
h�B
h�B
i�B
i�B
i�B
jKB
jB
jB
j0B
jB
j�B
j�B
j�B
jB
jB
j�B
kB
k6B
kQB
lB
lqB
m)B
m�B
m�B
m�B
m�B
ncB
oiB
o�B
p!B
p;B
p�B
q[B
poB
p�B
p�B
o�B
o B
o B
n/B
m)B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nB
nIB
n}B
o B
n�B
o�B
poB
pUB
p�B
qAB
p�B
p�B
qB
q�B
q�B
q�B
q�B
q�B
r|B
sMB
s3B
s3B
s�B
u?B
t�B
t�B
u�B
vFB
vFB
v+B
v+B
v�B
v�B
w�B
w�B
xB
y$B
y	B
xRB
y	B
zDB
z�B
z�B
zDB
zxB
z�B
|PB
|jB
|�B
|�B
}<B
}B
}B
}"B
}qB
}�B
~(B
~wB
~�B
~wB
~BB
.B
}B
�B
~�B
~]B
~wB
~�B
~�B
HB
�B
}B
cB
}B
�B
�B
�B
�B
�B
�OB
�OB
�iB
� B
� B
��B
��B
��B
��B
�'B
��B
��B
�oB
�;B
��B
��B
�AB
��B
��B
�oB
��B
�-B
��B
�-B
��B
�-B
��B
�gB
��B
��B
�B
�9B
��B
�mB
�%B
��B
�B
�%B
�B
�tB
�?B
�YB
�tB
��B
�+B
�+B
�_B
��B
��B
��B
��B
��B
��B
�fB
�KB
��B
��B
��B
�RB
�RB
�7B
�RB
��B
�	G�O�B��B��B��B��B�$B��B��B�B��B��B�_B��B��B�_B�0B��B��B��B��B�$B��B��B�_B�B��B��B�XB��B�eB�*B�*B�_B��B�$B��B�XB�*B�$B�B�XB��B�eB��B��B�XB��B��B��B�*B��B��B�RB�LB�@B��B��B�tB�B��B��B�$B��B��B��B�B��B�XB��B�RB�0B��B��B��B�B�@B�B��B�LB�FB��B�LB�B�tB�LB��B��B��B�FB��B�nB��B��B��B��B��B��B��B��B�=B��B��B��B��B��B��B��B��B�DB��B�PB�B��B�$B�Bm)Bm�BpBXEBk�BB�BJ�B;dB;�B<B9$BF�BA�BB'BC�BEBIBK�BM6BOBMBJ�BJ�BH�BH�BH�BGzBF?BE�BFtBF�BK�B?}B?�BDgB=�B=qB:�B:*B:�B;dB;dB;dB:�B:�B<�B<�B;�B;0B;�B?B?HB?B?B?�BAUBE9BE9BD3BFtBJ�BJ�BMjBPBT�B^Bc�Bg8Bf2Bd�Be,BffBg�Bf�BffBjBkQBk�BkBjKBi�Bl�Bl�Bm�Bm�Bm�Bn�Bo�Bq�BqBp�Br�Bt�Bt�Bs�BsMBs�Bt�Bs�BtBt�Bv�BxBw�By	By	B{B}VB� B�AB��B��B�MB��B��B�	B�B�JB��B�JB��B�oB�B��B��B��B�_B��B�7B��B�kB�1B��B�VB�!B��B��B�B��B�B��B�qB��BбB��B��B�]B�jB��B��B�B��B	GB	�B		lB	JB	xB	PB	"B	 B	�B	oB	�B	�B	�B	#nB	&�B	)_B	+�B	7LB	GzB	ZB	e�B	j�B	{B	�1B	��B	�!B	�B	ɆB	��B	�9B	�sB	�B	��B	�B	�GB	��B
"4B
8RB
?�B
D�B
HB
K�B
NB
PB
R B
T,B
X�B
k�B
m�B
poB
s�B
v�B
�;B
��B
��B
��B
�MB
�B
��B
��B
�B
�=B
��B
��B
��B
��B
��B
�\B
��B
�@B
��B
��B
�B
�B
��B
��B
�B
�aB
�'B
��B
��B
�*B
��B
��B
��B
�?B
��B
�5B
�B�B4BBB�BB�B�B@BoB�B�B\BVB�B�B"B.B�BOB�B�BIBIBB�B�B!�B#�B(�B%�B$�B&LB+6B-�B+kB+6B+�B.B,qB/�B1[B-�B+6B0�B(�B+�B,B(�B!�B$B)�B,�B&�B!-B�B�B�BB�B�B�B
��B
��B
��B
�%B
��B
�cBoB1B
��B
�yB
�B
� B
�HB
�)B
�^B
��B
��B
�
B
�B
��B
�B
��B
�)B
��B
�yB
ҽB
��B
ȴB
��B
��B
�zB
ɆB
�KB
бB
ݘB
��B
רB
�B
҉B
��B
��B
�B�B�B�B5tBH�B@B+BeB;dBOBBV�Bc�BOBM6BP}BK�B>BB?BB�BA�B7�B=�BFB(�B&�B"hB)*B \B#�B�B:B4B BB�B	B�B�BGB�B
�>BuB
�B
�2B
��B
�B
�DB
�B
� B
�BB
�B
�dB
��B
�EB
�|B
��B
�B
��B
��B
��B
�B
ÖB
��B
��B
�}B
�wB
��B
�IB
�[B
�$B
�FB
��B
�hB
��B
��B
��B
��B
�hB
�_B
��B
��B
��B
�B
�{B
��B
�SB
~]B
��B
��B
�B
��B
��B
�B
��B
�B
�tB
�nB
�B
�bB
�aB
��B
�_B
��B
��B
��B
��B
�{B
�=B
��B
�~B
��B
��B
��B
� B
}�B
��B
}�B
|�B
xB
{B
� B
yrB
|�B
}�B
i�B
`vB
d�B
X�B
]�B
v+B
B'B
`B
�B
$@B
I�B
 \B
�B
B

�B	�TB	��B	��B	��B	�B	��B	��B	ɆB	�B	�XB	�[B	�B	�wB	��B	�'B	��B	�0B	�IB	��B	��B	�9B	�-B	�=B	��B	�B	�B	��B	�iB	v�B	s�B	d�B	g8B	y>B	c�B	ffB	gmB	U2B	RTB	R�B	K�B	E9B	W
B	W?B	X�B	W
B	Y�B	h�B	|�B	�oB	�tB	{JB	_pB	qvB	b�B	_B	W?B	^�B	ZB	U2B	S&B	U�B	S�B	T,B	P�B	P�B	RTB	J�B	U�B	I�B	J�B	M�B	H�B	J#B	GEB	GB	K�B	GzB	FB	F�B	DgB	HB	EmB	GB	EmB	E�B	CaB	B�B	C�B	F?B	C�B	CaB	B�B	CaB	CaB	C-B	C�B	A�B	>�B	?�B	A�B	>�B	>�B	?�B	=�B	A�B	;dB	:*B	;dB	=B	<�B	:�B	:�B	;dB	8�B	;�B	3hB	6zB	EB	9�B	6�B	<6B	8�B	6FB	4�B	5�B	9�B	3hB	4nB	0!B	GzB	T�B	>wB	8�B	>�B	7�B	;�B	DgB	E9B	A�B	XB	>�B	@OB	=<B	<B	:�B	9$B	9�B	7B	8�B	?}B	:�B	5B	3hB	2�B	+�B	4nB	6FB	0UB	4B	2aB	+kB	/�B	)�B	/�B	&�B	"hB	)�B	>�B	-�B	-CB	($B	'B	5�B	.}B	)�B	,�B	*eB	($B	'�B	'�B	&�B	(�B	($B	)*B	'�B	'RB	&�B	)�B	*0B	*�B	*�B	*0B	)*B	)�B	*�B	,�B	,=B	4nB	0�B	5�B	6zB	6�B	49B	4nB	5B	1�B	1�B	2�B	6B	2-B	2�B	4�B	1'B	5tB	8�B	6B	7�B	4�B	7�B	7�B	<B	A�B	K�B	@B	=�B	?}B	=B	>�B	>B	;dB	:^B	:�B	=<B	?}B	>�B	:^B	@�B	JXB	C�B	DgB	FB	A�B	EB	GEB	F�B	DgB	DgB	F�B	DgB	F?B	G�B	EB	D�B	GzB	I�B	L�B	I�B	F�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<a��<>Җ<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<)�b<#�
<#�
<#�
<Og�<��<���<�J�<�ђ<kd�<PD<eo<<[,<eo<<L��<#�
<#�
<H�r<uA�<uA�<#�
<S�<�y<�8$<#�
<#�
<#�
<#�
<#�
<3�.<�	�<�D�<f=�<A��<��<�c�<|�=<��|<6]<�<p��<3�.<#�
<#�
<#�
<#�
<J��<���<w��<#�
<RJX<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<1Q�<[$I<8U/<<�><#�
<#�
<M��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018041316273920180413162739IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018042317043720180423170437QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018042317043720180423170437QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550720190521075507IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                