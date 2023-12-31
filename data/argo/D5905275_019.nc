CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-07-21T22:29:34Z creation; 2023-04-26T19:14:26Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20180721222934  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_019                 7316_008644_019                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�s�-w1�@�s�-w1�11  @�s�a@O@�s�a@O@)�FI�m@)�FI�m�d�ե�c�d�ե�c11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@@  @�  @�  @��R@�  A ��A��A ��A,(�A@  A_\)A\)A�  A�  A��A��A�  A�Q�A�  A��B  B  B  B (�B(  B0  B8(�B@(�BG�
BP  BX(�B`  Bh  BpQ�Bx  B�  B�{B�  B�  B�  B�  B��B��B��
B��B�  B�  B�  B�{B�  B�{B�  B��B��B�  B�{B��B��B��B��
B��
B��B�{B�  B��
B�  B�(�C   C��C�C��C��C
  C{C{C
=C
=C
=C
=C  C
=C{C{C 
=C!��C#��C&  C(  C)��C,  C.
=C0  C1��C4  C6{C8
=C:  C<
=C>  C?�CA��CC��CE��CH  CJ
=CL  CN
=CP  CR  CT{CV
=CX  CZ  C\
=C^
=C`
=Cb  Cc��Ce��Ch  Cj
=Ck��Cm��Cp  Cq��Cs��Cv
=Cx  Cz  C|
=C~  C��C�  C�  C�  C�C�C�C�C�  C�  C�  C���C�  C�  C���C�  C�C�  C�  C�C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�  C���C���C���C�  C�  C�C�  C���C�  C�C�
=C�C�C�  C�  C���C�  C�  C�C�  C�  C�  C�  C�C�  C���C�  C�  C�  C�  C�  C���C���C���C���C�  C�  C�  C�C�  C���C���C���C�C�  C���C���C���C�C�  C���C�  C�  C���C�  C�C�  C���C�  C���C���C�  C�  C���C�C�C�  C�  C���C���C�  C�  C���C�  C�  C���C�C�
=C�C�C�  C���C�C�C���C�  C�  C�  C�C�C�C�  C�  C�C�  C���C�  D   D � D�D��D  D}qD�qD� D�qDz�D  D� D�qD� D�qD� DD� D�qD	� D	�qD
� D  D� D�qD� D  DxRD  D��D�D��D�D��D  D}qD  D��D�D��D  D� D  D� D��Dz�D  D� D�qDz�D��D� D�D��D�qDz�D�qD� D�D�D�D� D  D� D   D � D!  D!}qD!��D"z�D"��D#� D$D$��D$�qD%}qD&  D&��D&�qD'z�D'�qD(��D)  D)}qD)��D*z�D+  D+� D+�qD,}qD-  D-� D.  D.�D/  D/��D0D0��D1  D1� D2�D2�D3D3� D3�qD4� D5D5�D6D6��D7�D7��D8  D8� D9  D9� D:  D:}qD;  D;�D<  D<��D<�qD=}qD>  D>��D?�D?��D@  D@}qDA  DA� DA�qDB}qDC  DC��DD  DD� DE�DE��DF�DF}qDF�qDG��DH�DH��DH�qDI}qDJ�DJ��DK�DK��DL  DL}qDM  DM� DN�DN��DO  DO� DP�DP� DP�qDQ}qDR�DR��DS  DS��DT�DT��DU�DU��DV�DV� DV�qDW� DX  DX��DY  DY��DZ�DZ��D[�D[� D\  D\� D]�D]��D^  D^� D_�D_��D`  D`}qD`�qDa� Db  Db}qDb�qDc� Dc�qDd}qDd�qDe}qDe�qDf� Dg  Dg��Dh  Dh}qDi  Di� Dj�Dj� Dk  Dk� Dl  Dl� Dl�qDm}qDm�qDn}qDn�qDo� Dp  Dp}qDp��Dq}qDr  Dr}qDr��Ds� Dt  Dt}qDu  Du��Dv  Dv}qDw  Dw��Dx  Dx}qDy  Dy��Dz�Dz� Dz�qD{z�D{��D|z�D|�qD}��D~�D~��D~�qD}qD�  D�>�D�~�D�� D�  D�@ D�~�D���D�  D�AHD�� D���D�  D�@ D��HD���D���D�AHD��HD�� D�  D�@ D�� D��HD�HD�>�D�� D�� D�  D�@ D��HD��HD�HD�AHD��HD��HD�HD�AHD�� D�� D���D�>�D�~�D�� D�  D�B�D��HD�� D���D�>�D�� D��HD��D�@ D�� D�� D���D�@ D��HD��HD�  D�@ D�� D�� D���D�@ D�� D��HD�HD�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D�� D���D���D�>�D�� D�� D���D�>�D��HD���D�  D�AHD��HD�� D���D�>�D�� D�D��D�B�D��HD��HD�HD�AHD�� D��HD�HD�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�� D���D���D�AHD�� D���D���D�>�D�� D�D�HD�AHD�~�D�� D�HD�AHD�� D��qD�  D�@ D�� D���D��qD�>�D�� D��HD�HD�@ D�� D��HD�  D�AHD��HD�� D�HD�B�D�� D��HD�HD�AHD��HD�D�  D�>�D�~�D�� D�HD�@ D�� D�� D�HD�AHD�� D�� D�HD�>�D��HD�� D�  D�@ D�� D��HD�HD�@ D�~�D��qD��qD�@ D��HD�� D���D�>�D�� D���D�  D�B�D�� D��qD�  D�B�D���D��HD�HD�@ D�� D���D�  D�AHD�~�D���D���D�@ D�� D�� D�HD�@ D�~�D�� D�HD�B�D�� D��qD���D�@ D�� D�� D�  D�>�D�~�D�� D�HD�AHD��HD��HD�HD�>�D�� D��HD�HD�@ D�~�D�� D�  D�AHD��HD��HD�  D�@ D�~�D�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D�  D�@ D�� D�� D�  D�AHD�� D���D���D�>�D D¾�D���D�@ D�~�D�� D�  D�>�D�}qDľ�D�  D�>�D�~�Dž�D���D�@ Dƀ D��HD�  D�AHD�~�D�� D�  D�@ D�~�DȾ�D���D�@ D�~�D�� D�HD�@ DʁHD�� D�  D�>�D�~�D�� D��qD�>�D̀ D̾�D�  D�AHD̀ D;�D���D�>�D�~�D�� D�  D�AHDρHDϾ�D�  D�@ DЀ D��HD�HD�@ D�~�D�� D�  D�>�D�~�DҾ�D���D�@ DӀ D�� D�HD�@ DԁHD�� D��qD�>�DՀ Dվ�D���D�@ Dր D�� D�HD�@ D׀ D�� D�  D�AHD؁HD��HD�HD�AHDق�D��HD�  D�@ D�~�D��HD�  D�>�DہHD�� D���D�@ D܀ Dܾ�D�HD�>�D�~�D�D�  D�=qD�~�D�� D�HD�AHD߁HD�� D�  D�AHD�� D�� D���D�@ D� DᾸD�HD�@ D�~�D⾸D�  D�>�D� D�� D�  D�>�D�~�D�� D�  D�@ D�HD��HD���D�@ D� D�� D�  D�AHD�HD羸D�  D�AHD�HD�D�  D�>�D� D�� D�HD�B�D�HD��HD�HD�AHD�HD�� D�  D�AHD�~�D쾸D�  D�=qD�~�D�� D���D�@ D�HDD���D�AHD�~�DﾸD�HD�@ D�� D�D���D�>�D�~�D��HD���D�=qD�~�D�D��qD�@ D�HD�� D���D�>�D�HD�D��D�@ D�~�D��qD�  D�B�D��HD�� D�  D�@ D�~�D��HD��D�B�D���D�� D�  D�AHD�� D�� D��\D�0�?#�
?8Q�?u?��R?�p�?�(�?��H@��@(�@.{@B�\@Q�@aG�@s33@�  @�=q@�33@�(�@��@���@�@��R@���@У�@�
=@�G�@�@�z�@��HA�A
=A(�A  A�
A�A��A!�A&ffA*�HA/\)A333A8Q�A=p�AA�AE�AJ=qAO\)AS�
AX��A\��AaG�AfffAk�Ap  As�
Ax��A~{A���A��
A�{A�Q�A��\A���A�\)A��A�(�A�ffA���A��A�{A�Q�A�=qA�z�A�
=A���A�(�A�ffA���A�33A�p�A�Q�A��HA��A�\)A�=qA���A�
=A�G�A˅A�{AУ�A�33A�p�A�\)A��A�z�A�
=A�G�A�33A�p�A�A�\A���A�\)A�G�A�A�p�A�  A��\A��A�\)B ��BB
=BQ�Bp�B�\B�B��B
=qB\)Bz�B��B�HB(�BG�B�\B�B��BB�HB(�B��B�RB�
B��B{B\)B z�B!B"�HB#�
B%�B&ffB'�B(��B)B*�HB,(�B-G�B.�\B/�
B0��B1�B3
=B4(�B5G�B6�\B7�B8��B9B:�HB<  B=�B>=qB?33B@(�BA�BBffBC\)BDQ�BEG�BF{BG33BHQ�BIp�BJ�\BK�BL��BM�BO
=BP(�BQG�BR�\BS�
BUG�BVffBW�BX��BY�B[33B\Q�B]��B^�HB`Q�Ba��Bc
=BdQ�Be��Bf�RBh  BiG�BjffBk�Bl��Bn=qBo\)Bpz�Bq��Br�\Bs�Btz�Bu�Bu�Bv�\Bw33Bw�
Bxz�By�ByBz�\B{\)B|  B|��B}p�B~=qB
=B�B�=qB��\B���B�G�B���B�  B�Q�B��\B���B�\)B�B�{B�z�B���B�33B���B�  B�Q�B��RB�
=B�\)B�B�{B�ffB��RB��B�\)B�B�{B�Q�B��RB�
=B�G�B���B��B�Q�B���B���B�G�B��B��
B�=qB��\B��HB�33B��B��
B�(�B�ffB��RB�
=B�\)B��B�  B�=qB��\B��HB��B�p�B�B�{B�Q�B���B��HB�33B�p�B��B�{B�Q�B��\B��HB�33B��B��
B�(�B�z�B��RB��B�p�B��B�{B�ffB��RB�
=B�\)B��B�  B�ffB��RB�
=B�p�B��B�{B�ffB��RB��B�p�B��
B�(�B�z�B��HB�33B���B�  B�Q�B��RB�
=B�p�B��
B�=qB��\B���B�\)B�B�(�B��\B��HB�G�B��B�{B�ffB���B��B��B��
B�=qB���B���B�p�B��
B�=qB��\B���B�\)B�B�{B�z�B��HB�G�B�B�(�B��\B���B�p�B��
B�Q�B��RB��B��B��B�Q�B���B�33B���B�{B��\B�
=B�\)B��B�Q�B��RB��B��B��B�Q�B��RB�33B���B�{B�z�B���B�p�B��B�Q�B��RB�33B���B�  B�z�B��HB�G�B�B�(�B£�B��BÙ�B�{Bď\B�
=BŅB��B�ffB���B�33BǙ�B�  B�z�B��HB�\)B�B�=qBʸRB�33BˮB�(�Ḅ�B��BͅB��B�ffB���B�33BϮB�(�BЏ\B�
=BхB�  B�z�B��HB�G�BӮB�(�Bԏ\B���BՅB�  B�ffB���B�\)B��
B�=qBظRB�
=BمB�  B�z�B���B�\)B��
B�(�Bܣ�B�
=B݅B�  B�z�B��HB�G�B߮B�{B��B��BᙚB�  B�ffB��HB�G�B��
B�Q�B���B�33B�B�(�B�RB�33B�B�(�B��B��B�B�Q�B���B�G�B�B�Q�B���B�p�B��B�ffB�
=B�B�(�B��B��B�B�ffB��HB�\)B��B�\B��B��B�(�B��HB�p�B�  B��\B��B��
B�ffB��HB��B�(�B���B�G�B��B���B��B��C 33C z�C C
=C\)C�C��C=qC��C�HC(�Cp�C��C�C\)C�C
=CQ�C��C�CG�C�\C�
C(�C�CC{Cz�CC	
=C	\)C	�RC
  C
Q�C
��C  CG�C�\C�C=qC�C�HC33Cz�C�
C�Cp�C�
C(�Cp�C�
C�Cp�C��C(�Cp�C��C�CffC��C�Cp�C��C{Cz�CC�Cz�CC(�C�C��C33Cz�C�
C33C�C�
C33C�C�HC=qCz�C�HC33Cz�C�HC33Cz�C�
C�Cp�C��C
=CffC�RC��C\)C��C��C =qC �\C �C!33C!�\C!�HC"33C"�\C"��C#(�C#z�C#C$�C$ffC$C%{C%\)C%�RC&  C&G�C&�C&�C'G�C'��C'�C(=qC(�\C(�C)=qC)�C)�HC*(�C*�\C*�
C+33C+�C+��C,33C,z�C,C-�C-ffC-C.{C.\)C.�C.��C/Q�C/��C/�C0=qC0�C0�HC1(�C1�C1��C2�C2p�C2�RC3{C3\)C3�RC4  C4Q�C4�C4�C5G�C5�\C5�C6=qC6z�C6�HC7�C7z�C7��C8�C8p�C8�RC9{C9Q�C9�RC9��C:Q�C:��C:�C;G�C;�C;�C<(�C<�\C<��C=(�C=p�C=��C>{C>\)C>C?  C?ffC?�C@  C@Q�C@��CA  CAG�CA��CA��CBG�CB��CB�HCCG�CC�\CC�HCD=qCD�CD�CE33CE�CE�CF(�CF�\CF�
CG(�CG�\CG��CH33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      ?��@�\@@  @�  @�  @��R@�  A ��A��A ��A,(�A@  A_\)A\)A�  A�  A��A��A�  A�Q�A�  A��B  B  B  B (�B(  B0  B8(�B@(�BG�
BP  BX(�B`  Bh  BpQ�Bx  B�  B�{B�  B�  B�  B�  B��B��B��
B��B�  B�  B�  B�{B�  B�{B�  B��B��B�  B�{B��B��B��B��
B��
B��B�{B�  B��
B�  B�(�C   C��C�C��C��C
  C{C{C
=C
=C
=C
=C  C
=C{C{C 
=C!��C#��C&  C(  C)��C,  C.
=C0  C1��C4  C6{C8
=C:  C<
=C>  C?�CA��CC��CE��CH  CJ
=CL  CN
=CP  CR  CT{CV
=CX  CZ  C\
=C^
=C`
=Cb  Cc��Ce��Ch  Cj
=Ck��Cm��Cp  Cq��Cs��Cv
=Cx  Cz  C|
=C~  C��C�  C�  C�  C�C�C�C�C�  C�  C�  C���C�  C�  C���C�  C�C�  C�  C�C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�  C���C���C���C�  C�  C�C�  C���C�  C�C�
=C�C�C�  C�  C���C�  C�  C�C�  C�  C�  C�  C�C�  C���C�  C�  C�  C�  C�  C���C���C���C���C�  C�  C�  C�C�  C���C���C���C�C�  C���C���C���C�C�  C���C�  C�  C���C�  C�C�  C���C�  C���C���C�  C�  C���C�C�C�  C�  C���C���C�  C�  C���C�  C�  C���C�C�
=C�C�C�  C���C�C�C���C�  C�  C�  C�C�C�C�  C�  C�C�  C���C�  D   D � D�D��D  D}qD�qD� D�qDz�D  D� D�qD� D�qD� DD� D�qD	� D	�qD
� D  D� D�qD� D  DxRD  D��D�D��D�D��D  D}qD  D��D�D��D  D� D  D� D��Dz�D  D� D�qDz�D��D� D�D��D�qDz�D�qD� D�D�D�D� D  D� D   D � D!  D!}qD!��D"z�D"��D#� D$D$��D$�qD%}qD&  D&��D&�qD'z�D'�qD(��D)  D)}qD)��D*z�D+  D+� D+�qD,}qD-  D-� D.  D.�D/  D/��D0D0��D1  D1� D2�D2�D3D3� D3�qD4� D5D5�D6D6��D7�D7��D8  D8� D9  D9� D:  D:}qD;  D;�D<  D<��D<�qD=}qD>  D>��D?�D?��D@  D@}qDA  DA� DA�qDB}qDC  DC��DD  DD� DE�DE��DF�DF}qDF�qDG��DH�DH��DH�qDI}qDJ�DJ��DK�DK��DL  DL}qDM  DM� DN�DN��DO  DO� DP�DP� DP�qDQ}qDR�DR��DS  DS��DT�DT��DU�DU��DV�DV� DV�qDW� DX  DX��DY  DY��DZ�DZ��D[�D[� D\  D\� D]�D]��D^  D^� D_�D_��D`  D`}qD`�qDa� Db  Db}qDb�qDc� Dc�qDd}qDd�qDe}qDe�qDf� Dg  Dg��Dh  Dh}qDi  Di� Dj�Dj� Dk  Dk� Dl  Dl� Dl�qDm}qDm�qDn}qDn�qDo� Dp  Dp}qDp��Dq}qDr  Dr}qDr��Ds� Dt  Dt}qDu  Du��Dv  Dv}qDw  Dw��Dx  Dx}qDy  Dy��Dz�Dz� Dz�qD{z�D{��D|z�D|�qD}��D~�D~��D~�qD}qD�  D�>�D�~�D�� D�  D�@ D�~�D���D�  D�AHD�� D���D�  D�@ D��HD���D���D�AHD��HD�� D�  D�@ D�� D��HD�HD�>�D�� D�� D�  D�@ D��HD��HD�HD�AHD��HD��HD�HD�AHD�� D�� D���D�>�D�~�D�� D�  D�B�D��HD�� D���D�>�D�� D��HD��D�@ D�� D�� D���D�@ D��HD��HD�  D�@ D�� D�� D���D�@ D�� D��HD�HD�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D�� D���D���D�>�D�� D�� D���D�>�D��HD���D�  D�AHD��HD�� D���D�>�D�� D�D��D�B�D��HD��HD�HD�AHD�� D��HD�HD�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�� D���D���D�AHD�� D���D���D�>�D�� D�D�HD�AHD�~�D�� D�HD�AHD�� D��qD�  D�@ D�� D���D��qD�>�D�� D��HD�HD�@ D�� D��HD�  D�AHD��HD�� D�HD�B�D�� D��HD�HD�AHD��HD�D�  D�>�D�~�D�� D�HD�@ D�� D�� D�HD�AHD�� D�� D�HD�>�D��HD�� D�  D�@ D�� D��HD�HD�@ D�~�D��qD��qD�@ D��HD�� D���D�>�D�� D���D�  D�B�D�� D��qD�  D�B�D���D��HD�HD�@ D�� D���D�  D�AHD�~�D���D���D�@ D�� D�� D�HD�@ D�~�D�� D�HD�B�D�� D��qD���D�@ D�� D�� D�  D�>�D�~�D�� D�HD�AHD��HD��HD�HD�>�D�� D��HD�HD�@ D�~�D�� D�  D�AHD��HD��HD�  D�@ D�~�D�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D�  D�@ D�� D�� D�  D�AHD�� D���D���D�>�D D¾�D���D�@ D�~�D�� D�  D�>�D�}qDľ�D�  D�>�D�~�Dž�D���D�@ Dƀ D��HD�  D�AHD�~�D�� D�  D�@ D�~�DȾ�D���D�@ D�~�D�� D�HD�@ DʁHD�� D�  D�>�D�~�D�� D��qD�>�D̀ D̾�D�  D�AHD̀ D;�D���D�>�D�~�D�� D�  D�AHDρHDϾ�D�  D�@ DЀ D��HD�HD�@ D�~�D�� D�  D�>�D�~�DҾ�D���D�@ DӀ D�� D�HD�@ DԁHD�� D��qD�>�DՀ Dվ�D���D�@ Dր D�� D�HD�@ D׀ D�� D�  D�AHD؁HD��HD�HD�AHDق�D��HD�  D�@ D�~�D��HD�  D�>�DہHD�� D���D�@ D܀ Dܾ�D�HD�>�D�~�D�D�  D�=qD�~�D�� D�HD�AHD߁HD�� D�  D�AHD�� D�� D���D�@ D� DᾸD�HD�@ D�~�D⾸D�  D�>�D� D�� D�  D�>�D�~�D�� D�  D�@ D�HD��HD���D�@ D� D�� D�  D�AHD�HD羸D�  D�AHD�HD�D�  D�>�D� D�� D�HD�B�D�HD��HD�HD�AHD�HD�� D�  D�AHD�~�D쾸D�  D�=qD�~�D�� D���D�@ D�HDD���D�AHD�~�DﾸD�HD�@ D�� D�D���D�>�D�~�D��HD���D�=qD�~�D�D��qD�@ D�HD�� D���D�>�D�HD�D��D�@ D�~�D��qD�  D�B�D��HD�� D�  D�@ D�~�D��HD��D�B�D���D�� D�  D�AHD�� D�� D��\G�O�?#�
?8Q�?u?��R?�p�?�(�?��H@��@(�@.{@B�\@Q�@aG�@s33@�  @�=q@�33@�(�@��@���@�@��R@���@У�@�
=@�G�@�@�z�@��HA�A
=A(�A  A�
A�A��A!�A&ffA*�HA/\)A333A8Q�A=p�AA�AE�AJ=qAO\)AS�
AX��A\��AaG�AfffAk�Ap  As�
Ax��A~{A���A��
A�{A�Q�A��\A���A�\)A��A�(�A�ffA���A��A�{A�Q�A�=qA�z�A�
=A���A�(�A�ffA���A�33A�p�A�Q�A��HA��A�\)A�=qA���A�
=A�G�A˅A�{AУ�A�33A�p�A�\)A��A�z�A�
=A�G�A�33A�p�A�A�\A���A�\)A�G�A�A�p�A�  A��\A��A�\)B ��BB
=BQ�Bp�B�\B�B��B
=qB\)Bz�B��B�HB(�BG�B�\B�B��BB�HB(�B��B�RB�
B��B{B\)B z�B!B"�HB#�
B%�B&ffB'�B(��B)B*�HB,(�B-G�B.�\B/�
B0��B1�B3
=B4(�B5G�B6�\B7�B8��B9B:�HB<  B=�B>=qB?33B@(�BA�BBffBC\)BDQ�BEG�BF{BG33BHQ�BIp�BJ�\BK�BL��BM�BO
=BP(�BQG�BR�\BS�
BUG�BVffBW�BX��BY�B[33B\Q�B]��B^�HB`Q�Ba��Bc
=BdQ�Be��Bf�RBh  BiG�BjffBk�Bl��Bn=qBo\)Bpz�Bq��Br�\Bs�Btz�Bu�Bu�Bv�\Bw33Bw�
Bxz�By�ByBz�\B{\)B|  B|��B}p�B~=qB
=B�B�=qB��\B���B�G�B���B�  B�Q�B��\B���B�\)B�B�{B�z�B���B�33B���B�  B�Q�B��RB�
=B�\)B�B�{B�ffB��RB��B�\)B�B�{B�Q�B��RB�
=B�G�B���B��B�Q�B���B���B�G�B��B��
B�=qB��\B��HB�33B��B��
B�(�B�ffB��RB�
=B�\)B��B�  B�=qB��\B��HB��B�p�B�B�{B�Q�B���B��HB�33B�p�B��B�{B�Q�B��\B��HB�33B��B��
B�(�B�z�B��RB��B�p�B��B�{B�ffB��RB�
=B�\)B��B�  B�ffB��RB�
=B�p�B��B�{B�ffB��RB��B�p�B��
B�(�B�z�B��HB�33B���B�  B�Q�B��RB�
=B�p�B��
B�=qB��\B���B�\)B�B�(�B��\B��HB�G�B��B�{B�ffB���B��B��B��
B�=qB���B���B�p�B��
B�=qB��\B���B�\)B�B�{B�z�B��HB�G�B�B�(�B��\B���B�p�B��
B�Q�B��RB��B��B��B�Q�B���B�33B���B�{B��\B�
=B�\)B��B�Q�B��RB��B��B��B�Q�B��RB�33B���B�{B�z�B���B�p�B��B�Q�B��RB�33B���B�  B�z�B��HB�G�B�B�(�B£�B��BÙ�B�{Bď\B�
=BŅB��B�ffB���B�33BǙ�B�  B�z�B��HB�\)B�B�=qBʸRB�33BˮB�(�Ḅ�B��BͅB��B�ffB���B�33BϮB�(�BЏ\B�
=BхB�  B�z�B��HB�G�BӮB�(�Bԏ\B���BՅB�  B�ffB���B�\)B��
B�=qBظRB�
=BمB�  B�z�B���B�\)B��
B�(�Bܣ�B�
=B݅B�  B�z�B��HB�G�B߮B�{B��B��BᙚB�  B�ffB��HB�G�B��
B�Q�B���B�33B�B�(�B�RB�33B�B�(�B��B��B�B�Q�B���B�G�B�B�Q�B���B�p�B��B�ffB�
=B�B�(�B��B��B�B�ffB��HB�\)B��B�\B��B��B�(�B��HB�p�B�  B��\B��B��
B�ffB��HB��B�(�B���B�G�B��B���B��B��C 33C z�C C
=C\)C�C��C=qC��C�HC(�Cp�C��C�C\)C�C
=CQ�C��C�CG�C�\C�
C(�C�CC{Cz�CC	
=C	\)C	�RC
  C
Q�C
��C  CG�C�\C�C=qC�C�HC33Cz�C�
C�Cp�C�
C(�Cp�C�
C�Cp�C��C(�Cp�C��C�CffC��C�Cp�C��C{Cz�CC�Cz�CC(�C�C��C33Cz�C�
C33C�C�
C33C�C�HC=qCz�C�HC33Cz�C�HC33Cz�C�
C�Cp�C��C
=CffC�RC��C\)C��C��C =qC �\C �C!33C!�\C!�HC"33C"�\C"��C#(�C#z�C#C$�C$ffC$C%{C%\)C%�RC&  C&G�C&�C&�C'G�C'��C'�C(=qC(�\C(�C)=qC)�C)�HC*(�C*�\C*�
C+33C+�C+��C,33C,z�C,C-�C-ffC-C.{C.\)C.�C.��C/Q�C/��C/�C0=qC0�C0�HC1(�C1�C1��C2�C2p�C2�RC3{C3\)C3�RC4  C4Q�C4�C4�C5G�C5�\C5�C6=qC6z�C6�HC7�C7z�C7��C8�C8p�C8�RC9{C9Q�C9�RC9��C:Q�C:��C:�C;G�C;�C;�C<(�C<�\C<��C=(�C=p�C=��C>{C>\)C>C?  C?ffC?�C@  C@Q�C@��CA  CAG�CA��CA��CBG�CB��CB�HCCG�CC�\CC�HCD=qCD�CD�CE33CE�CE�CF(�CF�\CF�
CG(�CG�\CG��CH33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�oA��A��A��A� �A�"�A�$�A�1'A�33A�7LA�A�A�M�A�C�A�/A��A�VA���A��A��A��A��`AݶFAݥ�Aݙ�Aݏ\A݇+A݃A�|�A�p�A���AҼjA�bNA�v�A�  A�O�A�ƨA�dZA��A�bNA�(�A��jA�S�A���A���A�^5A�$�A��+A��A��A��A��mA�r�A���A�  A��HA�9XA�1A��A�Q�A��A��^A��A�;dA�x�A���A�7LA|z�AzjAx��Aw�-AvA�AsAo�PAj��Ah-Ael�A`�/A^~�AU�hAQO�AN�AL�!AJ��AH1'AD��A@��A>�yA<��A9t�A7�A6�9A6  A5�^A5hsA5K�A5�A4��A4�!A4VA3��A3p�A2~�A133A0�yA0�RA0��A0r�A0M�A/��A/��A/oA-��A-7LA,�RA,5?A+��A+��A*�A*�uA*^5A*1A)��A)XA(�RA(Q�A(9XA(5?A(-A($�A(JA'�wA'�A&�`A&�jA& �A%��A%�A$�uA$�uA$r�A$ �A"��A ĜA ffA VAƨA�^AA�TA�RA�jA~�A��AVA�`A9XA�mA��AK�A/A�9AffA �At�A�A�jA  A�A`BA�A�A5?A�PA�^AȴAffA(�A�TA��AC�A+A�A
=AoA�A��A^5AJAO�Av�A�;AK�A�A
ĜA
~�A
1'A	�A	��A	;dA	A�9A�FA�AC�A�/A=qAbA1A  A�
A��AK�A%A�jA^5A�
A�7A�Ar�A-A��Ax�A ��A �jA �DA b@��
@�K�@��@��R@�@��h@�x�@���@�Q�@���@�+@���@���@���@�v�@�$�@��/@��@��\@�@�`B@��/@��j@�bN@�ƨ@��H@��@��@�`B@�(�@��@�x�@���@���@�@�@�@�@���@��;@�"�@柾@��@�x�@�@���@�@�+@�E�@�@��@�j@�9X@ߍP@�o@އ+@�5?@��#@�/@ܴ9@۝�@��y@ڟ�@�n�@�5?@ٲ-@�X@�7L@�V@�Q�@��
@�S�@�
=@�~�@պ^@�bN@ӥ�@�C�@���@�n�@ѩ�@с@�X@���@�r�@��@ϝ�@Ώ\@�7L@�%@��@���@̴9@̛�@�r�@� �@�;d@ʧ�@ɩ�@�X@��`@�j@�  @��;@Ǯ@�;d@��y@�ȴ@Ƨ�@�v�@�5?@�p�@Ĭ@ċD@�1@Õ�@�dZ@�;d@��@�V@���@���@�%@��@��@��P@�K�@�+@��!@���@���@�(�@�b@��m@���@���@�K�@��!@��@��^@�p�@�X@���@��j@�r�@� �@�1@��w@�+@�ȴ@��+@��@���@�hs@��@���@��@��
@���@�+@�o@���@�^5@��@�@�hs@�&�@�/@�7L@��@��u@���@�S�@�33@�+@�
=@���@��+@�J@�x�@���@��@���@���@�33@���@���@��@�X@��@�I�@��w@���@��P@�;d@��@�o@�o@���@���@���@�E�@��@�x�@�?}@���@���@��u@��@�bN@�9X@�1@��w@�l�@�;d@��@���@�`B@���@��@�j@��w@�"�@���@�ȴ@���@�5?@�hs@��9@�j@�(�@��;@��P@�dZ@�C�@�33@��@��y@���@�n�@�M�@��T@�p�@�/@��/@��D@�bN@�A�@�9X@�1'@�9X@�(�@�1@��@��@�l�@�33@�ȴ@��@��^@��7@�?}@��@��@�9X@��;@���@�K�@�
=@���@���@���@�E�@��@��h@�/@�Ĝ@�z�@�1'@�\)@�o@��y@���@�ff@��@�hs@�/@���@�Q�@�(�@��m@�dZ@�o@�ff@��@���@��#@�p�@�O�@��@��@��/@���@��j@��D@�9X@��@�1@��@���@�+@��H@�ȴ@��!@�=q@��@���@���@�/@���@��j@���@��u@�r�@�I�@�(�@��F@�o@���@�V@�E�@�{@��@��h@�O�@�V@���@���@�Z@�1'@�w@~��@~�R@}��@}p�@|�@|I�@{�@{o@z��@z�\@z=q@y��@y��@y��@x��@x1'@w�@w|�@w�@v��@v��@vff@u�@t�D@tj@tZ@sƨ@s�@rn�@q��@r-@q��@q%@p��@q�@q�@q�@q&�@p�9@pA�@p  @o��@o��@o+@nv�@n5?@m�-@m?}@m/@l�j@l9X@k�
@k�@kS�@k@jn�@jJ@i7L@hĜ@hbN@h  @g��@g�@g��@g|�@gl�@g+@g
=@f��@e�h@e`B@e�@d��@dz�@dZ@d9X@cdZ@b�!@bM�@a��@a&�@`��@`�9@`A�@`b@_�@_�w@_��@_\)@^�y@^$�@]�-@]/@\j@[ƨ@[S�@Z�@Zn�@Y��@Y&�@X��@X�u@XA�@X �@Wl�@Vȴ@V@U�@T��@T��@Tz�@TZ@T�@S��@S33@R��@R^5@Q�^@Q�@PĜ@PQ�@O��@OK�@N��@N$�@M`B@M?}@L��@L�@K�@K33@K@J�H@J-@I�#@I��@IX@H��@H  @G
=@Fv�@F{@E�T@E@Ep�@E/@D��@D��@D�@D�@Dj@C��@C�F@Ct�@B��@B-@B-@B�@A��@Ax�@AG�@A7L@A&�@@��@@��@@b@?��@?�@>�+@>{@=�@=�T@=��@=?}@<�@<9X@;ƨ@;�@;33@;"�@:��@:^5@9�@9hs@8��@8r�@81'@7��@7|�@7K�@7�@6��@6V@5��@5��@5`B@5�@4��@4�/@4�@4I�@2�H@2�\@2^5@2-@2�@2J@1��@1hs@1hs@0�9@/��@/��@/l�@/
=@.�R@.V@-�-@-`B@-?}@-�@,��@,�D@,�@,1@+��@+��@+t�@*��@*=q@)�@)G�@(��@(�u@(Q�@(  @'�w@'��@'l�@'�@&�y@&�@&��@&@%p�@%V@$z�@$j@$Z@$Z@$Z@$9X@$1@#�
@#��@#dZ@#33@"�H@"�\@"�\@"~�@"=q@"�@"J@!��@!��@!x�@ ��@ Ĝ@ ��@ �u@ �@ bN@ b@ b@   @�;@�@�@�w@;d@+@�@
=@
=@�@��@ff@V@5?@{@{@��@�@O�@/@V@j@1@�
@ƨ@�F@�F@��@S�@o@�@��@��@�!@~�@^5@M�@=q@-@^5@^5@-@��@X@X@G�@&�@��@Ĝ@�u@r�@A�@�@��@��@�w@�@K�@+@��@ȴ@��@��@��@��@ff@5?@E�@5?@�T@�@�@p�@O�@�@�/@�j@�@��@�D@�D@�D@z�@j@I�@�@�
@��@"�@^5@M�@=q@-@�@J@��@J@J@�@��@��@��@��@��@��@X@7L@�@%@��@��@Ĝ@�u@r�@1'@��@K�@+@
=@�y@ȴ@�+@v�@E�@$�@{@{@��@��@�@`B@?}@/@�@9X@�m@�F@�@t�@33@
�@
��@
��@
��@
��@
��@
��@
n�@
=q@	��@	��@	x�@	G�@	&�@	�@	%@Ĝ@��@�u@�@�@r�@�A�%A�A�%A�{A�oA�VA�VA�oA�oA��A��A��A��A��A��A�"�A�"�A� �A��A��A�$�A�"�A� �A� �A�"�A�$�A�$�A�"�A� �A�"�A�(�A�5?A�33A�/A�+A�1'A�5?A�7LA�7LA�33A�5?A�9XA�A�A�E�A�E�A�G�A�K�A�K�A�M�A�I�A�M�A�Q�A�O�A�M�A�M�A�K�A�E�A�A�A�;dA�5?A�/A�1'A�33A�5?A�1'A�+A�(�A�$�A� �A��A��A��A��A��A��A�bA�
=A�1A�1A�
=A�1A�A���A���A���A���A��A��A��A��A���A��A��A��A��A���A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��mA��`A��#A���A�ȴA�Aݺ^Aݴ9Aݲ-Aݲ-AݬAݩ�Aݥ�Aݧ�Aݧ�Aݧ�Aݡ�Aݝ�Aݛ�Aݛ�Aݛ�Aݝ�Aݛ�Aݗ�AݓuAݓuAݕ�AݓuAݓuAݍPA݉7A݉7A݉7A݋DA݉7A݅A݃A݃A݅A݅A݅A݅A݁A�~�A�|�A�|�A݁A݃A݁A�|�A�t�A�t�A�x�A�v�A�r�A�p�A�p�A�p�A�n�A�jA�ffA�ZA�S�A�C�A�7LA���A܁A�r�A؝�A�^5Aӝ�A�/Aϝ�A�x�A�ZA�oA�\)Aͧ�A�G�A̅A�ƨA�ffA��AǁA�K�A��yAƮA�v�A�^5A�"�A���A��;AŮA�n�A�1'A���A�/A�%A���A��A��A��A��A��yA��yA��#A��/A��A���Aã�AËDA�l�A�I�A��A��A�ĜA�A�hsA�G�A� �A���A��#A�ĜA��A��7A�p�A�ZA�C�A�-A�oA���A��/A���A��jA��-A���A���A��hA�~�A�ZA�K�A�C�A�?}A�=qA�;dA�;dA�9XA�7LA�7LA�5?A�5?A�1'A�-A�$�A��A��A��A��A��A��A�VA�
=A�JA���A��
A��RA���A���A��DA�t�A�O�A��A��/A��-A���A��A�t�A�`BA�Q�A�?}A�33A�&�A�oA��A���A���A���A�ffA�5?A��
A��A�ZA�A�A�5?A�+A�{A��A�ƨA���A�VA���A��A�$�A�|�A���A��!A���A���A���A���A��\A�/A���A�1'A��A���A���A�p�A�JA��HA��9A�dZA���A�z�A�E�A���A��hA�M�A�&�A�
=A���A��;A���A�ƨA��9A���A��uA��+A��A�p�A�bNA�I�A�=qA�-A��A�
=A�A���A��A��yA���A��FA���A�ffA�7LA���A��A�;dA��DA���A�G�A��uA�=qA���A��A�
=A��A��9A��A�hsA� �A�JA�1A�%A�A�A�A�A�A��A��A�(�A���A���A�9XA�^5A�VA��`A�ƨA��A�r�A�33A�%A��mA���A���A���A�x�A�7LA��/A�n�A�5?A���A�M�A���A���A�JA�|�A�7LA�+A�&�A�$�A��A�  A��
A���A�l�A�&�A���A� �A�A��mA�ZA�VA���A��A��RA�Q�A��A��`A���A���A�Q�A���A��jA��PA�;dA��mA�G�A���A� �A��TA���A��A�E�A��mA���A�v�A�dZA�7LA�oA��wA���A���A��\A��A�E�A�33A�JA��A��-A���A��hA�z�A�I�A�{A��/A��-A�x�A�&�A��A�&�A�|�A��\A��yA��9A��hA��A��-A��\A�p�A�&�A��A��uA�1'A�A�C�A��RA��mA�
=A�bNA�VA�O�A�1'A�VA���A�l�A�"�A�VA�VA�JA��A��A�JA���A��7A�$�A��A~�`A~�A}�^A}O�A|�9A|VA|bA{�A{t�A{&�Az�`Az�\Azv�AzQ�Ay�#Ay��Ay|�Ay/Ax��Ax��AxffAxVAx(�Aw��AwAw�wAw�hAw\)AwS�Aw?}Aw�Av�/Av~�Au�PAt��As�-AsXAsO�As?}As
=Arv�Aq�AqXApZAo��Ao;dAn�\Am�Al�`AlM�Ak�-Ak?}AjjAidZAh�Ah�9Ah~�AhjAhbAgAg��Ag\)AfȴAf�DAe�Adr�Ac�Ab(�AahsA`�yA`��A`n�A`=qA`�A_�#A_�hA_VA^ �A[�^AYp�AV��AU�PAU?}AT1'AS�wAS\)ARVAQXAP�AP�API�AN��ANM�ANbAN  AM�mAM��AM��AMC�AL�`ALv�AL �AKAK�AK7LAKAJn�AJ�AI�AIƨAI`BAH��AH1'AG�hAF�AF~�AF  AE��AE/AD�9AC�AB�AB(�AA\)A@Q�A?�mA?��A?|�A?&�A?�A>��A>ĜA>�RA>�!A>��A>�DA>JA=7LA<bA:��A:M�A9A9��A9�-A9�A8��A8�9A8�uA8=qA7�^A7��A7�hA7x�A7t�A7C�A6��A6M�A6=qA6-A6$�A6{A61A61A5��A5�mA5�#A5��A5��A5ƨA5A5��A5�PA5|�A5hsA5dZA5dZA5hsA5\)A5\)A5S�A5O�A5O�A5C�A5?}A57LA533A533A5+A5+A5%A4��A4�`A4�A4�A4ĜA4ĜA4��A4��A4��A4��A4�RA4�A4��A4z�A4jA4^5A4ZA4VA4Q�A4E�A49XA4(�A41A3�A3�;A3ƨA3��A3��A3�PA3l�A3S�A37LA3/A3"�A2��A2��A25?A1�#A1�hA1dZA1G�A1&�A1"�A1oA1
=A1%A0��A0�A0�`A0ȴA0ȴA0��A0ĜA0�jA0�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      A�
=A�oA��A��A��A� �A�"�A�$�A�1'A�33A�7LA�A�A�M�A�C�A�/A��A�VA���A��A��A��A��`AݶFAݥ�Aݙ�Aݏ\A݇+A݃A�|�A�p�A���AҼjA�bNA�v�A�  A�O�A�ƨA�dZA��A�bNA�(�A��jA�S�A���A���A�^5A�$�A��+A��A��A��A��mA�r�A���A�  A��HA�9XA�1A��A�Q�A��A��^A��A�;dA�x�A���A�7LA|z�AzjAx��Aw�-AvA�AsAo�PAj��Ah-Ael�A`�/A^~�AU�hAQO�AN�AL�!AJ��AH1'AD��A@��A>�yA<��A9t�A7�A6�9A6  A5�^A5hsA5K�A5�A4��A4�!A4VA3��A3p�A2~�A133A0�yA0�RA0��A0r�A0M�A/��A/��A/oA-��A-7LA,�RA,5?A+��A+��A*�A*�uA*^5A*1A)��A)XA(�RA(Q�A(9XA(5?A(-A($�A(JA'�wA'�A&�`A&�jA& �A%��A%�A$�uA$�uA$r�A$ �A"��A ĜA ffA VAƨA�^AA�TA�RA�jA~�A��AVA�`A9XA�mA��AK�A/A�9AffA �At�A�A�jA  A�A`BA�A�A5?A�PA�^AȴAffA(�A�TA��AC�A+A�A
=AoA�A��A^5AJAO�Av�A�;AK�A�A
ĜA
~�A
1'A	�A	��A	;dA	A�9A�FA�AC�A�/A=qAbA1A  A�
A��AK�A%A�jA^5A�
A�7A�Ar�A-A��Ax�A ��A �jA �DA b@��
@�K�@��@��R@�@��h@�x�@���@�Q�@���@�+@���@���@���@�v�@�$�@��/@��@��\@�@�`B@��/@��j@�bN@�ƨ@��H@��@��@�`B@�(�@��@�x�@���@���@�@�@�@�@���@��;@�"�@柾@��@�x�@�@���@�@�+@�E�@�@��@�j@�9X@ߍP@�o@އ+@�5?@��#@�/@ܴ9@۝�@��y@ڟ�@�n�@�5?@ٲ-@�X@�7L@�V@�Q�@��
@�S�@�
=@�~�@պ^@�bN@ӥ�@�C�@���@�n�@ѩ�@с@�X@���@�r�@��@ϝ�@Ώ\@�7L@�%@��@���@̴9@̛�@�r�@� �@�;d@ʧ�@ɩ�@�X@��`@�j@�  @��;@Ǯ@�;d@��y@�ȴ@Ƨ�@�v�@�5?@�p�@Ĭ@ċD@�1@Õ�@�dZ@�;d@��@�V@���@���@�%@��@��@��P@�K�@�+@��!@���@���@�(�@�b@��m@���@���@�K�@��!@��@��^@�p�@�X@���@��j@�r�@� �@�1@��w@�+@�ȴ@��+@��@���@�hs@��@���@��@��
@���@�+@�o@���@�^5@��@�@�hs@�&�@�/@�7L@��@��u@���@�S�@�33@�+@�
=@���@��+@�J@�x�@���@��@���@���@�33@���@���@��@�X@��@�I�@��w@���@��P@�;d@��@�o@�o@���@���@���@�E�@��@�x�@�?}@���@���@��u@��@�bN@�9X@�1@��w@�l�@�;d@��@���@�`B@���@��@�j@��w@�"�@���@�ȴ@���@�5?@�hs@��9@�j@�(�@��;@��P@�dZ@�C�@�33@��@��y@���@�n�@�M�@��T@�p�@�/@��/@��D@�bN@�A�@�9X@�1'@�9X@�(�@�1@��@��@�l�@�33@�ȴ@��@��^@��7@�?}@��@��@�9X@��;@���@�K�@�
=@���@���@���@�E�@��@��h@�/@�Ĝ@�z�@�1'@�\)@�o@��y@���@�ff@��@�hs@�/@���@�Q�@�(�@��m@�dZ@�o@�ff@��@���@��#@�p�@�O�@��@��@��/@���@��j@��D@�9X@��@�1@��@���@�+@��H@�ȴ@��!@�=q@��@���@���@�/@���@��j@���@��u@�r�@�I�@�(�@��F@�o@���@�V@�E�@�{@��@��h@�O�@�V@���@���@�Z@�1'@�w@~��@~�R@}��@}p�@|�@|I�@{�@{o@z��@z�\@z=q@y��@y��@y��@x��@x1'@w�@w|�@w�@v��@v��@vff@u�@t�D@tj@tZ@sƨ@s�@rn�@q��@r-@q��@q%@p��@q�@q�@q�@q&�@p�9@pA�@p  @o��@o��@o+@nv�@n5?@m�-@m?}@m/@l�j@l9X@k�
@k�@kS�@k@jn�@jJ@i7L@hĜ@hbN@h  @g��@g�@g��@g|�@gl�@g+@g
=@f��@e�h@e`B@e�@d��@dz�@dZ@d9X@cdZ@b�!@bM�@a��@a&�@`��@`�9@`A�@`b@_�@_�w@_��@_\)@^�y@^$�@]�-@]/@\j@[ƨ@[S�@Z�@Zn�@Y��@Y&�@X��@X�u@XA�@X �@Wl�@Vȴ@V@U�@T��@T��@Tz�@TZ@T�@S��@S33@R��@R^5@Q�^@Q�@PĜ@PQ�@O��@OK�@N��@N$�@M`B@M?}@L��@L�@K�@K33@K@J�H@J-@I�#@I��@IX@H��@H  @G
=@Fv�@F{@E�T@E@Ep�@E/@D��@D��@D�@D�@Dj@C��@C�F@Ct�@B��@B-@B-@B�@A��@Ax�@AG�@A7L@A&�@@��@@��@@b@?��@?�@>�+@>{@=�@=�T@=��@=?}@<�@<9X@;ƨ@;�@;33@;"�@:��@:^5@9�@9hs@8��@8r�@81'@7��@7|�@7K�@7�@6��@6V@5��@5��@5`B@5�@4��@4�/@4�@4I�@2�H@2�\@2^5@2-@2�@2J@1��@1hs@1hs@0�9@/��@/��@/l�@/
=@.�R@.V@-�-@-`B@-?}@-�@,��@,�D@,�@,1@+��@+��@+t�@*��@*=q@)�@)G�@(��@(�u@(Q�@(  @'�w@'��@'l�@'�@&�y@&�@&��@&@%p�@%V@$z�@$j@$Z@$Z@$Z@$9X@$1@#�
@#��@#dZ@#33@"�H@"�\@"�\@"~�@"=q@"�@"J@!��@!��@!x�@ ��@ Ĝ@ ��@ �u@ �@ bN@ b@ b@   @�;@�@�@�w@;d@+@�@
=@
=@�@��@ff@V@5?@{@{@��@�@O�@/@V@j@1@�
@ƨ@�F@�F@��@S�@o@�@��@��@�!@~�@^5@M�@=q@-@^5@^5@-@��@X@X@G�@&�@��@Ĝ@�u@r�@A�@�@��@��@�w@�@K�@+@��@ȴ@��@��@��@��@ff@5?@E�@5?@�T@�@�@p�@O�@�@�/@�j@�@��@�D@�D@�D@z�@j@I�@�@�
@��@"�@^5@M�@=q@-@�@J@��@J@J@�@��@��@��@��@��@��@X@7L@�@%@��@��@Ĝ@�u@r�@1'@��@K�@+@
=@�y@ȴ@�+@v�@E�@$�@{@{@��@��@�@`B@?}@/@�@9X@�m@�F@�@t�@33@
�@
��@
��@
��@
��@
��@
��@
n�@
=q@	��@	��@	x�@	G�@	&�@	�@	%@Ĝ@��@�u@�@�@r�G�O�A�%A�A�%A�{A�oA�VA�VA�oA�oA��A��A��A��A��A��A�"�A�"�A� �A��A��A�$�A�"�A� �A� �A�"�A�$�A�$�A�"�A� �A�"�A�(�A�5?A�33A�/A�+A�1'A�5?A�7LA�7LA�33A�5?A�9XA�A�A�E�A�E�A�G�A�K�A�K�A�M�A�I�A�M�A�Q�A�O�A�M�A�M�A�K�A�E�A�A�A�;dA�5?A�/A�1'A�33A�5?A�1'A�+A�(�A�$�A� �A��A��A��A��A��A��A�bA�
=A�1A�1A�
=A�1A�A���A���A���A���A��A��A��A��A���A��A��A��A��A���A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��mA��`A��#A���A�ȴA�Aݺ^Aݴ9Aݲ-Aݲ-AݬAݩ�Aݥ�Aݧ�Aݧ�Aݧ�Aݡ�Aݝ�Aݛ�Aݛ�Aݛ�Aݝ�Aݛ�Aݗ�AݓuAݓuAݕ�AݓuAݓuAݍPA݉7A݉7A݉7A݋DA݉7A݅A݃A݃A݅A݅A݅A݅A݁A�~�A�|�A�|�A݁A݃A݁A�|�A�t�A�t�A�x�A�v�A�r�A�p�A�p�A�p�A�n�A�jA�ffA�ZA�S�A�C�A�7LA���A܁A�r�A؝�A�^5Aӝ�A�/Aϝ�A�x�A�ZA�oA�\)Aͧ�A�G�A̅A�ƨA�ffA��AǁA�K�A��yAƮA�v�A�^5A�"�A���A��;AŮA�n�A�1'A���A�/A�%A���A��A��A��A��A��yA��yA��#A��/A��A���Aã�AËDA�l�A�I�A��A��A�ĜA�A�hsA�G�A� �A���A��#A�ĜA��A��7A�p�A�ZA�C�A�-A�oA���A��/A���A��jA��-A���A���A��hA�~�A�ZA�K�A�C�A�?}A�=qA�;dA�;dA�9XA�7LA�7LA�5?A�5?A�1'A�-A�$�A��A��A��A��A��A��A�VA�
=A�JA���A��
A��RA���A���A��DA�t�A�O�A��A��/A��-A���A��A�t�A�`BA�Q�A�?}A�33A�&�A�oA��A���A���A���A�ffA�5?A��
A��A�ZA�A�A�5?A�+A�{A��A�ƨA���A�VA���A��A�$�A�|�A���A��!A���A���A���A���A��\A�/A���A�1'A��A���A���A�p�A�JA��HA��9A�dZA���A�z�A�E�A���A��hA�M�A�&�A�
=A���A��;A���A�ƨA��9A���A��uA��+A��A�p�A�bNA�I�A�=qA�-A��A�
=A�A���A��A��yA���A��FA���A�ffA�7LA���A��A�;dA��DA���A�G�A��uA�=qA���A��A�
=A��A��9A��A�hsA� �A�JA�1A�%A�A�A�A�A�A��A��A�(�A���A���A�9XA�^5A�VA��`A�ƨA��A�r�A�33A�%A��mA���A���A���A�x�A�7LA��/A�n�A�5?A���A�M�A���A���A�JA�|�A�7LA�+A�&�A�$�A��A�  A��
A���A�l�A�&�A���A� �A�A��mA�ZA�VA���A��A��RA�Q�A��A��`A���A���A�Q�A���A��jA��PA�;dA��mA�G�A���A� �A��TA���A��A�E�A��mA���A�v�A�dZA�7LA�oA��wA���A���A��\A��A�E�A�33A�JA��A��-A���A��hA�z�A�I�A�{A��/A��-A�x�A�&�A��A�&�A�|�A��\A��yA��9A��hA��A��-A��\A�p�A�&�A��A��uA�1'A�A�C�A��RA��mA�
=A�bNA�VA�O�A�1'A�VA���A�l�A�"�A�VA�VA�JA��A��A�JA���A��7A�$�A��A~�`A~�A}�^A}O�A|�9A|VA|bA{�A{t�A{&�Az�`Az�\Azv�AzQ�Ay�#Ay��Ay|�Ay/Ax��Ax��AxffAxVAx(�Aw��AwAw�wAw�hAw\)AwS�Aw?}Aw�Av�/Av~�Au�PAt��As�-AsXAsO�As?}As
=Arv�Aq�AqXApZAo��Ao;dAn�\Am�Al�`AlM�Ak�-Ak?}AjjAidZAh�Ah�9Ah~�AhjAhbAgAg��Ag\)AfȴAf�DAe�Adr�Ac�Ab(�AahsA`�yA`��A`n�A`=qA`�A_�#A_�hA_VA^ �A[�^AYp�AV��AU�PAU?}AT1'AS�wAS\)ARVAQXAP�AP�API�AN��ANM�ANbAN  AM�mAM��AM��AMC�AL�`ALv�AL �AKAK�AK7LAKAJn�AJ�AI�AIƨAI`BAH��AH1'AG�hAF�AF~�AF  AE��AE/AD�9AC�AB�AB(�AA\)A@Q�A?�mA?��A?|�A?&�A?�A>��A>ĜA>�RA>�!A>��A>�DA>JA=7LA<bA:��A:M�A9A9��A9�-A9�A8��A8�9A8�uA8=qA7�^A7��A7�hA7x�A7t�A7C�A6��A6M�A6=qA6-A6$�A6{A61A61A5��A5�mA5�#A5��A5��A5ƨA5A5��A5�PA5|�A5hsA5dZA5dZA5hsA5\)A5\)A5S�A5O�A5O�A5C�A5?}A57LA533A533A5+A5+A5%A4��A4�`A4�A4�A4ĜA4ĜA4��A4��A4��A4��A4�RA4�A4��A4z�A4jA4^5A4ZA4VA4Q�A4E�A49XA4(�A41A3�A3�;A3ƨA3��A3��A3�PA3l�A3S�A37LA3/A3"�A2��A2��A25?A1�#A1�hA1dZA1G�A1&�A1"�A1oA1
=A1%A0��A0�A0�`A0ȴA0ȴA0��A0ĜA0�jA0�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�-B	��B	��B	�FB	�FB	�RB	�$B	��B	�HB	��B	ŢB	ɺB	�sB	�|B	� B	�B	�B	�B	� B	� B	�B	��B	�)B	�B	�5B	�iB	� B	��B	�QB	��B	چB	�B	��B
!-B
P�B
�PB
�{B
��B
�?B
��B
�?B
՛B
��B
�"B!bB49B-CB�B�BBB1B
�B
��B
�B
�,B
��B
�{B
YB
�B	�PB	�BB	��B	�oB	p�B	iyB	o5B	_�B	YB	YB	\)B	bB	[�B	S�B	=qB	%B	 �B	+B	MB	�B	"�B	$�B	#:B	~B	�B	kB	/�B	0�B	FtB	\�B	n/B	y�B	��B	�%B	��B	�\B	�uB	��B	�zB	�*B	��B	�[B	��B	�B	�]B
	lB

�B
B
�B
�B
bB
�B
�B
!�B
$�B
*�B
.�B
3�B
>wB
@OB
B[B
CaB
B�B
F?B
H�B
I�B
J�B
J�B
K)B
K^B
K^B
M�B
P}B
Q�B
R�B
U�B
T,B
TaB
U2B
V�B
V9B
VB
XyB
NpB
IRB
K�B
H�B
NpB
PHB
J�B
@OB
D�B
GEB
GB
?HB
B�B
>�B
@�B
DgB
IB
N�B
T�B
U�B
W
B
U�B
S�B
Q�B
QB
N�B
M�B
LdB
I�B
E�B
@B
9$B
3hB
5?B
5tB
5B
5tB
7B
;�B
@�B
B'B
HKB
J#B
J�B
I�B
I�B
IB
G�B
D�B
@B
@�B
@�B
A�B
A B
@�B
@B
>�B
=�B
=�B
=qB
;�B
<B
<�B
<B
;�B
;dB
<B
=<B
>BB
=�B
=<B
=qB
;�B
=<B
:*B
:*B
7�B
7�B
7B
6�B
5�B
7B
9�B
9�B
8�B
7LB
6�B
7LB
5�B
4B
3�B
4nB
3�B
2�B
2�B
1[B
0�B
0UB
/�B
.�B
/B
,=B
+kB
)_B
+B
*0B
)�B
)*B
(�B
&�B
&LB
$�B
$B
#B
 \B
!B
OB
~B
~B
�B
qB
�B
7B
�B
�B
�B
�B
�B
�B
{B
B
@B
�B
�B
oB
�B
:B
�B
oB
�B
�B
�B
�B
B
B
�B
:B
�B
oB
�B
�B
 B
�B
�B
�B
�B
�B
(B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
~B
"B
~B
xB
�B
xB
B

�B

rB

�B
DB

rB
DB

�B
B
�B
DB
DB
xB
~B
~B
B
�B
�B
�B
JB
xB

�B

�B

	B
	�B
	�B
	�B
	lB
�B
fB
	B
	�B

	B

	B
	�B
	B
�B
JB
~B
�B
DB
xB
DB

�B
B
B
�B

�B

�B

	B
B

rB

�B
B
�B
DB
�B
B
�B
�B
�B
~B
~B
�B
PB
�B
�B
"B
�B
�B
PB
B
�B
�B
�B
"B
�B
\B
�B
�B
�B
4B
hB
B
�B
B
uB
�B
FB
�B
{B
�B
{B
�B
FB
�B
uB
B
�B
{B
�B
�B
B
B
B
�B
MB
�B
B
�B
B
SB
�B
$B
YB
�B
$B
�B
�B
�B
�B
YB
YB
�B
�B
�B
�B
�B
�B
�B
IB
�B
�B
�B
�B
�B
~B
B
B
�B
�B
�B
�B
 �B
 �B
�B
�B
 \B
 \B
�B
!-B
�B
�B
 'B
 'B
�B
 \B
 \B
 'B
 'B
 'B
�B
 'B
�B
 'B
!�B
!�B
!�B
!�B
!�B
#:B
"�B
#nB
#nB
$B
$@B
$tB
$�B
$tB
%B
%�B
&B
&B
&�B
'RB
'B
'�B
(�B
(�B
(XB
(�B
(�B
)_B
*0B
)�B
*eB
*�B
*eB
*�B
*eB
+B
,qB
,=B
,B
,�B
-wB
-CB
-�B
-�B
-�B
-�B
-wB
-�B
.IB
.B
.B
-�B
.�B
/�B
/�B
/OB
/OB
0�B
0UB
0�B
0UB
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2-B
2�B
33B
3�B
3hB
3�B
3�B
4nB
4�B
4�B
5B
5?B
5?B
5B
5�B
5�B
5�B
6zB
6FB
6zB
6�B
7�B
7�B
7�B
7�B
7�B
8B
7�B
7�B
9$B
8�B
8�B
9�B
9XB
:*B
:�B
<B
<�B
<�B
<�B
<�B
=B
>B
?B
>B
>�B
?�B
>�B
?HB
A�B
AUB
A�B
A�B
B'B
C-B
B�B
B�B
B[B
B�B
B�B
C-B
C�B
CaB
CaB
D3B
EB
E9B
EmB
EB
FB
E�B
FB
F?B
GB
G�B
G�B
GEB
G�B
G�B
HB
GzB
GzB
F�B
F�B
I�B
H�B
H�B
H�B
H�B
IB
H�B
JXB
J#B
J#B
I�B
I�B
H�B
H�B
IRB
IB
IRB
I�B
IRB
IRB
I�B
J�B
JXB
J�B
K�B
K�B
L0B
L�B
MjB
M6B
NpB
N�B
O�B
OB
O�B
PHB
P}B
QB
QNB
QNB
QB
P�B
P�B
P�B
Q�B
Q�B
R B
RTB
S&B
S&B
S[B
T�B
S�B
S�B
T�B
UgB
U2B
U2B
U�B
W
B
V�B
V�B
V9B
W?B
W�B
XB
WsB
W?B
W
B
XB
X�B
XEB
X�B
X�B
YB
YKB
ZQB
Z�B
ZB
ZB
Z�B
[�B
\]B
[�B
\�B
[�B
[�B
[�B
\�B
\]B
[�B
[�B
\)B
[WB
[�B
[�B
\]B
]dB
^5B
^�B
^5B
^5B
^jB
^�B
_;B
_pB
`B
`�B
`�B
`�B
aHB
a�B
a�B
bNB
b�B
b�B
cTB
c�B
d&B
d�B
d&B
d�B
dZB
e,B
e`B
d�B
e`B
e,B
e,B
e,B
d�B
e�B
f�B
e�B
f�B
gB
gB
gmB
hsB
iyB
iyB
jB
j�B
j�B
jB
jB
jB
j�B
kQB
jB
j�B
jKB
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
lWB
l�B
m]B
m�B
m�B
n/B
ncB
ncB
o B
n�B
n�B
o B
n�B
n�B
oiB
o�B
o�B
o�B
o�B
o�B
o5B
o5B
o5B
o�B
pB
o�B
p;B
o�B
p�B
qB
poB
p;B
qvB
q�B
qvB
p�B
q�B
rGB
r�B
r�B
rGB
rB
r|B
rGB
sMB
r�B
r�B
s�B
sMB
sMB
s�B
t�B
t�B
t�B
u%B
t�B
t�B
uZB
u�B
t�B
u%B
u�B
u�B
u�B
u�B
v+B
v+B
v+B
v`B
v�B
wfB
v�B
w2B
w2B
v�B
wfB
w�B
w2B
w�B
w�B
w�B
w�B
w�B
xB
x�B
x�B
x�B
y�B
y�B
y>B
y>B
y�B
y>B
yrB
y>B
y�B
y�B
zxB
zDB
z�B
{B
{B
z�B
z�B
{JB
zxB
{B
{B
{�B
{B
{B
{�B
{�B
|�B
|B
|B
|�B
|�B
|PB
|�B
|�B
|B
|PB
|PB
|�B
|�B
|PB
|�B
|PB
|PB
|�B
|�B
}"B
}�B
}"B
~(B
~]B
~�B
cB
.B
~�B
~�B
.B
�B
�B
��B
�iB
��B
��B
��B
��B
��B
�uB
��B
�B
�GB
�{B
��B
��B
�B
�B
��B
��B
�%B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�+B
��B
�_B
��B
��B
��B
��B
�fB
�fB
�7B
�7B
��B
��B
��B
�	B
��B
��B
��B
��B
��B
�	B
�=B
�=B
��B
��B
��B
��B
�B
�B
�B
��B
��B
�xB
��B
�B
��B
��B	��B	��B	��B	��B	��B	�nB	�B	��B	�nB	��B	�9B	��B	��B	�zB	�tB	�zB	�zB	��B	�?B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�^B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	��B	�OB	��B	�HB	ϫB	�dB	�TB	՛B	خB	�B	خB	�/B	��B	�B	�B	�vB	�B	�B	�B	�B	�TB	�NB	�NB	�TB	��B	�ZB	��B	��B	�B	�NB	��B	� B	�B	��B	�B	�B	�B	�B	�|B	�B	�B	�TB	��B	�|B	�NB	�B	�&B	��B	�B	�NB	�B	�B	��B	�B	�B	�NB	��B	�&B	��B	� B	�B	�TB	�&B	��B	��B	��B	�B	�B	��B	�B	�sB	��B	��B	�QB	�B	�/B	�]B	��B	�B	��B	�B	�B	�/B	�]B	��B	�B	� B	��B	�5B	��B	�cB	�cB	�B	�B	�oB	�5B	��B	�/B	��B	�B	��B	��B	��B	�B	�B	��B	�iB	�/B	�B	��B	��B	�B	�/B	��B	�B	�B	��B	��B	�B	�QB	��B	��B	��B	�B	�B	�fB	�B	��B	�B	�5B	��B	�)B	�B	��B	�?B	�EB
B	�"B
bB	ȀB
)�B	�B	��B	��B	�B	�B	��B	ޞB	�>B	�B
B[B
	7B
�B
�B
'RB
1[B
;�B
@OB
L�B
R B
WsB
^B
kB
q�B
�_B
�FB
�uB
��B
��B
�:B
��B
�B
�{B
��B
�B
��B
��B
��B
�{B
�B
�1B
�1B
�xB
�=B
�B
�OB
��B
��B
��B
�*B
��B
�6B
��B
�UB
�UB
��B
��B
�B
�zB
��B
��B
��B
��B
��B
�6B
��B
��B
� B
��B
��B
��B
��B
�gB
�9B
�mB
�mB
�9B
ŢB
ŢB
�9B
�tB
�?B
�B
�EB
�tB
�tB
�B
��B
ʌB
�B
�6B
�BB
�[B
�aB
�[B
��B
רB
ںB
�/B
��B
��B
�ZB
��B
�B
�yB
��B
�B
�B
��B
�B
�B
�B
�B
�B
��B
��B
��B
��BuBB �B
��B
�"B
��B
�]B iB iB
�.B�BDBB �B,�B6�B-CB,�B+B*eB)*B*�B;�B?}B:�B1�B5B<6B4B'RB)�B*0B2�B/OB'�B%�B=qB6zB,B&LB$�B$@B!�B �B!�B"hB 'B"�B�BOBOBIB�BB�B�B	BeB�B�B�B_B�B�B�B�B�B1B�B�B�B$�B!-B$B"�B�B�BBB�B�BDB�BMB�B�BYB�B�B�B	B�B�B+B�B�B�B
�B
��B
�B
�B
�KB
�B
�`B
�pB
یB
��B
�KB
ںB
�pB
�TB
��B
�B
�B
�>B
� B
��B�B
�"B
�B
�>B
��B
�B
�B
�B
�TB
��B
��B
�&B
��B
�|B
��B
�B
ɆB
��B
�wB
��B
�-B
��B
��B
�=B
�1B
�LB
��B
�4B
x�B
o�B
l�B
k�B
j�B
��B
N�B
?�B
;dB
0UB
4�B
,B
OB
�B
YB
�B
B
!-B
"B
	B
�B
AB
�B	��B
�B	��B	�GB	� B	�B	�B	�B	�fB	�TB	�B	��B	�WB	�B	��B	�dB	�HB	�6B	�NB	��B	��B	��B	�{B	��B	�1B	�B	�%B	{�B	{B	~(B	|�B	��B	�DB	h�B	e`B	e�B	jKB	d�B	r�B	oiB	g�B	iB	[�B	`�B	�B	i�B	g8B	h
B	�DB	m�B	qvB	u�B	bB	d�B	c B	i�B	\�B	\�B	`�B	Y�B	YKB	\)B	YB	U�B	XEB	]�B	W?B	XB	Z�B	ZQB	ZQB	Z�B	V9B	\�B	\�B	[�B	YB	^�B	\]B	Z�B	[�B	[�B	]/B	_�B	kQB	gB	jKB	W�B	WsB	X�B	ZB	`vB	[#B	_B	[�B	J�B	T�B	P�B	C�B	M�B	:�B	=qB	9�B	H�B	5�B	$�B	,�B	%�B	$B	&�B	!�B	�B	!bB	�B	kB	/B	�B	!-B	{B	�B	
�B	�B	 iB�(B�B��B�	B�>B	DB	1B	"hB	B	  B�B	1B	�B	B	(�B	($B	B	�B	!�B	5�B	#:B	%zB	 �B	�B	 �B	"�B	#�B	&�B	$B	!bB	#:B	�B	!B	~B	'RB	7B	�B	�B	�B	�B	'B	SB	eB	�B	�B	�B	B	�B	2-B	�B	'�B	*eB	?�B	/OB	,=B	0�B	0�B	,=B	0�B	4�B	1'B	2�B	49B	8�B	@�B	OB	T�B	R�B	B'B	OB	Z�B	^B	bB	m�B	g8B	h
B	o�B	q�B	m�B	o B	n�B	m�B	s�B	�+B	{�B	{�B	~(B	~]B	~�B	�4B	�B	�oB	�AB	��B	�B	��B	�B	��B	��B	�=B	��B	��B	�~B	�PB	�B	��B	��B	��B	��B	�"B	�.B	�.B	��B	��B	�4B	��B	��B	��B	�B	��B	��B	�1B	�~B	��B	�!B	�OB	��B	�'B	�zB	��B	�eB	��B	��B	�XB	�XB	��B	�jB	��B	��B	��B	ǮB	��B	�B	�B	�NB	�B	бB	��B	՛B	רB	�9B	��B	��B	��B	�B	��B	�cB	�iB	�vB	��B	�B	�fB	��B	�8B	��B	�PB	�.B
SB
SB
fB
1B
	B

=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      B	�B	��B	��B	�+B	�FB	�8B	�	B	�>B	�B	��B	�SB	ɠB	רB	��B	�B	�B	��B	��B	� B	�:B	�&B	��B	�B	��B	�iB	�B	�B	��B	�=B	�B
�B
�B
YB
*B
Y�B
�}B
�xB
�B
�XB
�aB
�RB
�B
��B�B5%BKDB6�B$�B'�B/B:BmB�B
�B
��B
��B
�B
�CB
jB
$tB
_B	�qB	�B	�EB	{dB	utB	z�B	f�B	^�B	]~B	a�B	lqB	g�B	a�B	F�B	/B	/�B	hB	 �B	OB	-]B	)�B	*KB	&2B	%�B	(�B	5�B	8�B	Q�B	b4B	r-B	|B	��B	�EB	�B	�B	��B	�CB	��B	��B	��B	ּB	�2B	��B	�B
	�B
xB
�B
�B
 B
�B
EB
 B
#TB
&�B
,=B
/�B
6+B
?cB
A B
C{B
DMB
D�B
HfB
J=B
J#B
J�B
J�B
K^B
K�B
L�B
O�B
Q4B
RoB
UB
W?B
V9B
VB
UMB
WYB
W�B
[qB
^�B
O�B
I�B
MjB
I7B
Q B
T,B
N�B
@�B
E�B
IRB
J	B
@4B
D�B
@B
AoB
E�B
I�B
P}B
U�B
V�B
YeB
W$B
UMB
TaB
RTB
O�B
OvB
MjB
KxB
H�B
F?B
<�B
4�B
6+B
6zB
5�B
6�B
7�B
<B
A B
B'B
H�B
KDB
K�B
KB
LJB
LB
I�B
F�B
@�B
B'B
A�B
CB
BB
A�B
A�B
?�B
>�B
@�B
>BB
<�B
=�B
>�B
<�B
;�B
;�B
<�B
>B
?cB
>�B
>]B
>�B
=�B
>wB
<PB
;�B
8�B
8�B
8�B
8�B
6`B
7�B
;�B
:*B
9�B
7�B
7�B
8�B
6zB
4TB
4�B
5�B
4nB
4TB
33B
1�B
1B
0�B
0�B
1'B
0�B
/ B
,�B
*0B
+�B
*B
*KB
*eB
*0B
($B
'B
%`B
&LB
%�B
"�B
 B
�B
B
B
5B
IB
�B
CB
1B
�B
B
sB
_B
YB
B
�B
�B
&B
�B
B
�B
uB
�B
uB
@B
TB
�B
�B
B
gB
[B
�B
 B
[B
oB
�B
hB
 B
�B
�B
�B
B
�B
B
B
�B
�B
vB
�B
BB
"B
�B
�B
pB
jB
�B
�B
B
�B
�B
�B
DB
DB
)B
~B
~B
JB
�B
�B
�B
dB
�B
�B
JB
B
�B
dB
JB
JB
B
�B
�B
�B
�B

rB

=B

=B

�B

#B
�B
	�B

	B

�B
B

�B
	�B

#B
�B
B
�B
�B
�B
�B
^B
DB
JB
6B
�B
)B

�B

�B
�B
B
xB
^B
JB
dB
dB
�B
B
6B
0B
B
B
"B
�B
6B
�B
pB
(B
�B
�B
�B
<B
pB
�B
"B
BB
}B
4B
vB
B
NB
�B
�B
@B
�B
�B
B
�B
�B
B
�B
2B
2B
�B
�B
aB
MB
�B
�B
�B
�B
gB
2B
B
B
gB
gB
B
mB
�B
�B
$B
�B
�B
$B
sB
�B
�B
+B
+B
�B
+B
�B
�B
�B
kB
=B
)B
�B
�B
�B
B
�B
 BB
;B
B
�B
�B
 'B
 B
�B
�B
 �B
 �B
 vB
 'B
 �B
!-B
 �B
!�B
 �B
 �B
 vB
 vB
 B
 vB
 \B
 \B
 vB
 \B
 vB
 �B
 vB
!B
# B
"�B
!�B
"hB
#B
#�B
#nB
$B
$B
$�B
$�B
$�B
$�B
$�B
%�B
&LB
&�B
&�B
'�B
'�B
'�B
)B
)_B
(�B
(�B
)DB
)�B
*KB
*�B
*KB
+kB
+6B
*�B
+�B
+B
,=B
,�B
,�B
,WB
-�B
-�B
-�B
.B
-�B
-�B
-�B
-�B
.}B
.�B
.IB
.IB
.}B
/�B
0B
/�B
/�B
0!B
1'B
0�B
0�B
1'B
2GB
1�B
1�B
1�B
1�B
2B
1�B
2|B
3hB
3�B
3�B
3�B
3�B
4B
4TB
4�B
5%B
5%B
5�B
5�B
5�B
5�B
6�B
5�B
6�B
6�B
6�B
72B
7�B
8B
8B
8B
8B
88B
8RB
88B
8�B
9�B
9>B
9>B
9�B
9�B
:*B
:�B
<�B
=�B
<�B
<�B
="B
=VB
?B
?}B
=�B
?cB
@OB
>�B
?.B
A�B
AUB
A�B
A�B
B�B
C{B
B�B
B�B
B�B
CaB
B�B
C�B
C�B
C{B
C�B
D�B
EmB
E�B
E�B
EmB
F�B
F?B
F�B
F�B
GzB
HB
HB
G_B
G�B
G�B
H1B
G�B
G�B
GEB
G�B
I�B
IB
I7B
I7B
IB
IRB
I�B
J�B
J�B
J�B
J�B
I�B
H�B
IRB
I�B
IRB
I�B
I�B
I�B
I�B
J=B
KB
J�B
K�B
LdB
LdB
L�B
MB
N"B
M�B
N�B
OBB
P.B
OBB
P�B
Q B
QNB
Q�B
Q�B
Q�B
Q4B
QB
Q4B
Q4B
R B
RTB
R�B
R�B
S�B
SuB
S�B
U2B
TaB
T{B
UB
VB
UgB
U�B
V�B
W�B
V�B
W
B
VmB
W�B
X+B
X_B
W�B
W�B
W�B
X�B
YB
X�B
X�B
YB
YeB
Y�B
Z�B
Z�B
Z7B
ZkB
[=B
\)B
\�B
\CB
]�B
\CB
[�B
[�B
]IB
\�B
\)B
[�B
\CB
[�B
[�B
\]B
\�B
]�B
^�B
_B
^OB
^OB
^�B
_B
_�B
`'B
`vB
`�B
a-B
`�B
a�B
bNB
b4B
b�B
c B
cnB
c�B
dB
dZB
d�B
dZB
d�B
d�B
e�B
e�B
eB
e�B
eFB
eFB
e`B
ezB
f�B
f�B
f2B
f�B
gB
gB
g�B
h�B
i�B
j0B
kQB
j�B
j�B
j�B
jeB
jB
kQB
k�B
j�B
kB
j�B
k6B
k�B
k�B
k�B
k�B
k�B
l�B
m)B
l�B
m]B
m�B
nIB
nB
n}B
n�B
n�B
o5B
n�B
o B
oB
n�B
o5B
o�B
pB
p!B
o�B
o�B
o�B
o5B
oOB
oiB
o�B
p;B
p!B
poB
p!B
q'B
qB
p�B
p�B
q�B
q�B
q�B
qB
r-B
r�B
r�B
r�B
raB
r-B
r�B
r�B
sMB
r�B
r�B
s�B
sMB
s�B
tnB
t�B
t�B
t�B
u%B
t�B
u%B
u�B
u�B
uB
u?B
u�B
u�B
vB
u�B
vFB
v`B
v�B
v�B
w2B
w�B
wB
w2B
wLB
wB
w�B
w�B
wLB
w�B
w�B
xB
w�B
w�B
xB
x�B
x�B
x�B
zB
z^B
y�B
y>B
y�B
yrB
y�B
yrB
y�B
zB
z�B
z�B
z�B
{B
{0B
z�B
{JB
{dB
z�B
{�B
{�B
{�B
{B
{B
|B
{�B
|�B
|6B
|jB
|�B
|�B
|jB
|�B
|�B
|jB
|jB
|jB
|�B
|�B
|PB
|�B
|jB
|jB
|�B
}"B
}qB
}�B
}�B
~�B
~wB
~�B
}B
HB
B
B
.B
�B
�B
��B
�iB
��B
��B
�B
�B
��B
��B
��B
�-B
�aB
��B
��B
�B
�MB
�gB
��B
�mB
�?B
�B
��B
��B
��B
��B
��B
�B
��B
��B
�zB
��B
�zB
��B
��B
�B
�KB
�B
��B
�lB
�lB
��B
��B
��B
�#B
��B
��B
��B
��B
��B
�=B
�rB
��B
�)B
��B
�B
��B
�)B
�)B
�^B
��B
��B
��B
��B
�0B
��G�O�B	��B	��B	��B	��B	��B	�nB	�B	��B	�nB	��B	�9B	��B	��B	�zB	�tB	�zB	�zB	��B	�?B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�^B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	��B	�OB	��B	�HB	ϫB	�dB	�TB	՛B	خB	�B	خB	�/B	��B	�B	�B	�vB	�B	�B	�B	�B	�TB	�NB	�NB	�TB	��B	�ZB	��B	��B	�B	�NB	��B	� B	�B	��B	�B	�B	�B	�B	�|B	�B	�B	�TB	��B	�|B	�NB	�B	�&B	��B	�B	�NB	�B	�B	��B	�B	�B	�NB	��B	�&B	��B	� B	�B	�TB	�&B	��B	��B	��B	�B	�B	��B	�B	�sB	��B	��B	�QB	�B	�/B	�]B	��B	�B	��B	�B	�B	�/B	�]B	��B	�B	� B	��B	�5B	��B	�cB	�cB	�B	�B	�oB	�5B	��B	�/B	��B	�B	��B	��B	��B	�B	�B	��B	�iB	�/B	�B	��B	��B	�B	�/B	��B	�B	�B	��B	��B	�B	�QB	��B	��B	��B	�B	�B	�fB	�B	��B	�B	�5B	��B	�)B	�B	��B	�?B	�EB
B	�"B
bB	ȀB
)�B	�B	��B	��B	�B	�B	��B	ޞB	�>B	�B
B[B
	7B
�B
�B
'RB
1[B
;�B
@OB
L�B
R B
WsB
^B
kB
q�B
�_B
�FB
�uB
��B
��B
�:B
��B
�B
�{B
��B
�B
��B
��B
��B
�{B
�B
�1B
�1B
�xB
�=B
�B
�OB
��B
��B
��B
�*B
��B
�6B
��B
�UB
�UB
��B
��B
�B
�zB
��B
��B
��B
��B
��B
�6B
��B
��B
� B
��B
��B
��B
��B
�gB
�9B
�mB
�mB
�9B
ŢB
ŢB
�9B
�tB
�?B
�B
�EB
�tB
�tB
�B
��B
ʌB
�B
�6B
�BB
�[B
�aB
�[B
��B
רB
ںB
�/B
��B
��B
�ZB
��B
�B
�yB
��B
�B
�B
��B
�B
�B
�B
�B
�B
��B
��B
��B
��BuBB �B
��B
�"B
��B
�]B iB iB
�.B�BDBB �B,�B6�B-CB,�B+B*eB)*B*�B;�B?}B:�B1�B5B<6B4B'RB)�B*0B2�B/OB'�B%�B=qB6zB,B&LB$�B$@B!�B �B!�B"hB 'B"�B�BOBOBIB�BB�B�B	BeB�B�B�B_B�B�B�B�B�B1B�B�B�B$�B!-B$B"�B�B�BBB�B�BDB�BMB�B�BYB�B�B�B	B�B�B+B�B�B�B
�B
��B
�B
�B
�KB
�B
�`B
�pB
یB
��B
�KB
ںB
�pB
�TB
��B
�B
�B
�>B
� B
��B�B
�"B
�B
�>B
��B
�B
�B
�B
�TB
��B
��B
�&B
��B
�|B
��B
�B
ɆB
��B
�wB
��B
�-B
��B
��B
�=B
�1B
�LB
��B
�4B
x�B
o�B
l�B
k�B
j�B
��B
N�B
?�B
;dB
0UB
4�B
,B
OB
�B
YB
�B
B
!-B
"B
	B
�B
AB
�B	��B
�B	��B	�GB	� B	�B	�B	�B	�fB	�TB	�B	��B	�WB	�B	��B	�dB	�HB	�6B	�NB	��B	��B	��B	�{B	��B	�1B	�B	�%B	{�B	{B	~(B	|�B	��B	�DB	h�B	e`B	e�B	jKB	d�B	r�B	oiB	g�B	iB	[�B	`�B	�B	i�B	g8B	h
B	�DB	m�B	qvB	u�B	bB	d�B	c B	i�B	\�B	\�B	`�B	Y�B	YKB	\)B	YB	U�B	XEB	]�B	W?B	XB	Z�B	ZQB	ZQB	Z�B	V9B	\�B	\�B	[�B	YB	^�B	\]B	Z�B	[�B	[�B	]/B	_�B	kQB	gB	jKB	W�B	WsB	X�B	ZB	`vB	[#B	_B	[�B	J�B	T�B	P�B	C�B	M�B	:�B	=qB	9�B	H�B	5�B	$�B	,�B	%�B	$B	&�B	!�B	�B	!bB	�B	kB	/B	�B	!-B	{B	�B	
�B	�B	 iB�(B�B��B�	B�>B	DB	1B	"hB	B	  B�B	1B	�B	B	(�B	($B	B	�B	!�B	5�B	#:B	%zB	 �B	�B	 �B	"�B	#�B	&�B	$B	!bB	#:B	�B	!B	~B	'RB	7B	�B	�B	�B	�B	'B	SB	eB	�B	�B	�B	B	�B	2-B	�B	'�B	*eB	?�B	/OB	,=B	0�B	0�B	,=B	0�B	4�B	1'B	2�B	49B	8�B	@�B	OB	T�B	R�B	B'B	OB	Z�B	^B	bB	m�B	g8B	h
B	o�B	q�B	m�B	o B	n�B	m�B	s�B	�+B	{�B	{�B	~(B	~]B	~�B	�4B	�B	�oB	�AB	��B	�B	��B	�B	��B	��B	�=B	��B	��B	�~B	�PB	�B	��B	��B	��B	��B	�"B	�.B	�.B	��B	��B	�4B	��B	��B	��B	�B	��B	��B	�1B	�~B	��B	�!B	�OB	��B	�'B	�zB	��B	�eB	��B	��B	�XB	�XB	��B	�jB	��B	��B	��B	ǮB	��B	�B	�B	�NB	�B	бB	��B	՛B	רB	�9B	��B	��B	��B	�B	��B	�cB	�iB	�vB	��B	�B	�fB	��B	�8B	��B	�PB	�.B
SB
SB
fB
1B
	B

=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
=R�<�h>=�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?��<���<��(<�u�<*ِ<#�
<~�<�1�<#�
<o�<J�'<���<f��<�4�<���<�(�<�D�<#�
<?��<��<��8<���<7Ñ<M6d<J �<#�
<#�
<#�
<#�
<6>�<H��<lV�<$5O<01�<t0<37><�Rm<t0<7Ñ<#�
<#�
<#�
<N��<oy�<#�
<#�
<@�	<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018072122293420180721222934IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018073122005720180731220057QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018073122005720180731220057QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550920190521075509IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                