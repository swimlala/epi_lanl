CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-09-07T17:50:44Z creation; 2023-05-01T21:35:40Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20200907175044  20230501213540  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               b   bAA  AOAO7314_008642_098                 7314_008642_098                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�6j{���@�6j{���11  @�6j�s�@�6j�s�@1uy(��@1uy(���b�C���b�C��11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @@  @�  @��R@��R@�  @��RA\)A   A,(�A@  A`  A�Q�A�Q�A��A�  A��AϮA�Q�A�Q�B   B�B�
B  B   B((�B0(�B8  B@(�BHQ�BP(�BX  B`(�Bh(�Bp��Bx(�B�  B��
B�  B�  B�  B�  B�{B�  B�  B��
B��B�  B�  B�  B�  B�  B��B�  B�  B�  B�{B�{B�  B�  B�  B�  B�  B�  B��B�  B�{B�  C 
=C  C  C
=C
=C

=C��C��C  C��C�C��C  C��C�C��C 
=C"{C$
=C&
=C({C)��C,  C.  C/��C2  C4  C6  C8  C9��C<  C>
=C@
=CB  CC��CF  CH
=CJ  CK��CM��CP  CQ��CS��CU��CX  CZ  C\  C^  C`
=Cb  Cd  Ce��Cg��Cj  Cl  Cn  Co��Cr  Ct  Cu��Cw��Cz  C|  C~  C�  C�  C�  C�  C�C�C�  C�  C�  C�  C�  C�C�  C�  C�  C�  C�C�  C�  C�C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�C�C�  C�  C�C�  C�  C�  C���C���C���C���C���C�C�  C�  C�  C�C�C�  C���C���C�  C�  C�C�
=C�C���C�  C�  C�C���C���C�  C�  C���C���C�  C�  C���C�  C�C�C�C�C�  C�  C�C�
=C�  C�  C�  C�  C�  C�
=C�C�  C���C�  C�C�  C���C���C�  C�
=C�C�  C�C�  C�  C�  C���C���C�  C�  C�  C�  C���C�  C�C�C�
=C���C���C�  C�  C�  C�  C�  C�  D   D }qD  D� D  D��D  D� D�D��D  D� D�D��D�qD}qD��D� D	�D	��D
�D
��D�D� D  D��DD��D�qD� D�qD� D�D� D  D}qD��D}qD  D� D�qD}qD�qD}qD�qD}qD�qD� D  D��D�qD}qD  D� D�D� D�qD� D�D��D�D�DD��D�qD }qD!  D!}qD!�qD"��D#�D#� D#�qD$� D%  D%� D%�qD&z�D'  D'��D(  D(� D(�qD)}qD*  D*��D+�D+}qD,  D,��D-  D-}qD.  D.�D/  D/� D0  D0}qD0�qD1� D2  D2� D3  D3}qD4  D4}qD4�qD5}qD6  D6� D7  D7� D8�D8��D9  D9� D:  D:}qD:�qD;� D<  D<� D<�qD=z�D=�qD>� D>�qD?� D@�D@��DA  DA� DA�qDB� DC�DC��DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI��DJ�DJ� DK  DK� DK�qDL� DMDM��DN  DN��DO  DO}qDP�DP� DQ  DQ� DR�DR�DS�DS� DT�DT��DU�DU� DV  DV� DW  DW� DX  DX��DYDY� DY�qDZ� D[�D[� D[�qD\� D\�qD]}qD^  D^}qD^�qD_� D`  D`}qD`�qDa� Db�Db� Dc  Dc��Dd  Ddz�Dd��De��Df�Df}qDf�qDg}qDh  Dh� Di�Di��Dj  Djz�Dj�qDk� Dl  Dl��DmDm��Dn  Dn��Do  Do� Dp  Dp}qDq�Dq��Dr�Dr� Ds  Ds� Dt  Dt� Du�Du��Dv  Dvz�Dv�qDw� Dx  Dx��DyDy��Dz  Dz� D{  D{}qD{�qD|}qD}  D}� D~  D~��D  D}qD�qD�@ D�� D�D�HD�>�D�� D���D���D�@ D�~�D���D�  D�>�D�~�D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�>�D�� D�� D���D�>�D���D�D�HD�@ D�~�D�� D�HD�@ D�~�D���D��qD�@ D��HD�� D�  D�>�D�~�D��HD�HD�AHD�� D���D��D�AHD�� D��qD�  D�B�D��HD��HD�  D�>�D�}qD�� D��D�AHD��HD��HD�  D�>�D�� D��HD�  D�@ D��HD��HD�HD�@ D�� D��HD�HD�AHD�� D���D���D�>�D�� D��HD���D�@ D�� D�� D�  D�@ D��HD���D���D�AHD��HD�� D���D�>�D�� D�� D�  D�>�D�~�D��HD�  D�@ D�� D�� D�HD�AHD��HD��HD��D�@ D�� D�� D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�>�D��HD�� D�  D�B�D�� D�� D�  D�@ D��HD�� D���D�>�D�~�D���D���D�@ D�� D��HD�HD�@ D�� D�� D�HD�>�D�� D�� D���D�>�D�� D��HD�HD�>�D�}qD��qD���D�@ D�� D�� D�  D�AHD��HD�� D���D�@ D��HD���D���D�AHD���D�� D���D�>�D�� D��HD�  D�>�D�� D��HD��D�B�D�~�D���D��qD�=qD�� D�� D���D�@ D�� D��qD�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D��HD�� D���D�>�D�� D�� D���D�=qD�� D��HD�HD�AHD�~�D���D���D�>�D��HD��HD�HD�AHD�� D�� D�HD�AHD�~�D��qD���D�@ D�~�D��HD�  D�@ D��HD�� D�  D�@ D�~�D���D���D�>�D�~�D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��HD�HD�AHDÁHD��HD�  D�@ DĀ D�� D�  D�AHDŁHD�D��D�@ D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�}qD�� D�HD�AHDɁHD��HD�HD�AHDʀ DʽqD���D�@ DˁHD˾�D���D�@ D́HD�� D���D�@ D́HD�� D��qD�>�D΀ D��HD�HD�@ Dπ D�� D���D�@ DЁHD�� D�HD�B�DсHDѾ�D���D�>�DҁHD��HD�  D�AHDӀ D�� D�HD�AHD�~�DԾ�D�  D�AHDՀ D�� D���D�>�Dր D־�D�  D�@ D�~�D�� D�HD�AHD؀ D�� D�  D�@ Dـ D��HD�HD�@ Dڀ D��HD�  D�>�Dۀ D��HD�HD�@ D܀ Dܾ�D���D�@ D݀ Dݾ�D���D�@ DށHD�� D�  D�@ D߀ D�� D���D�>�D�� D�� D�HD�AHD�~�D�� D�  D�>�D�~�D�� D�HD�@ D�~�D㾸D�  D�@ D� D��HD�HD�AHD�HD��HD�HD�AHD� D澸D���D�>�D� D��HD�  D�@ D�~�D�qD��qD�>�D� D��HD�HD�AHD�~�D꾸D�  D�AHD�HD�� D��qD�@ D�HD�� D���D�@ D�HD�� D���D�@ DD�D�HD�AHD�HD�� D��qD�>�D�� D��HD�HD�AHD� D��HD�  D�>�D� D��HD�HD�>�D�~�D�� D�  D�AHD� D���D���D�@ D��HD��HD�HD�AHD��HD�� D�  D�>�D�~�D�� D���D�>�D�� D�� D�  D�>�D�~�D�� D�  D�/\D�p�?�?.{?k�?�z�?�p�?�(�@   @\)@!G�@333@E�@W
=@fff@z�H@��@���@�@�(�@��@���@�z�@�(�@\@�=q@��@ٙ�@�\@���@��@�Q�A   A�
AQ�A��A��Az�A��A��A!G�A%A(��A-p�A1G�A4z�A8��A<��A@��ADz�AH��AMp�AP��AU�AY��A]p�Ab�\AfffAj=qAn�RAs�
Aw�A{�A�  A�=qA���A��RA���A�33A�p�A��A��A�(�A�{A�Q�A�=qA�(�A�ffA���A�33A�p�A�\)A���A��
A�{A�  A��A��
A�{A�Q�A��\A�(�A�ffA���A\A���A�
=A�G�A�33A�p�AϮA��A�(�A�{A�Q�A��HA��A�
=A�G�A�A�A�  A��A�z�A�RA���A�33A�A��A�=qA�z�A��RB ��BB�HB  BG�B=qB�B��B	��B
�HB  B�B=qB\)Bz�B��B�RB�
B��B�B
=BQ�Bp�B�\B�B��B{B33B Q�B!��B"�RB#�
B%�B&=qB'\)B(z�B)��B*�RB,  B,��B.{B/\)B0z�B1��B2�RB3�
B4��B6{B7
=B8(�B9G�B:�\B;�B<��B>{B?
=B@(�BA�BBffBC�BDz�BE��BF�RBG�
BH��BIBJ�HBK�
BL��BN{BO33BP(�BQG�BR=qBS33BTQ�BUG�BV=qBW\)BXz�BYp�BZ�\B[�B\��B^{B_33B`Q�BaG�Bb=qBc33BdQ�Bep�Bf�\Bg�Bh��BiBj�HBk�
Bl��Bm��BnffBo33Bo�
Bp��Bq�Bq��Br{BrffBr�HBs33Bs�Bt  Btz�Bt��BuG�Bu��Bv{BvffBv�HBw\)Bw�Bx(�Bxz�Bx��ByG�ByBz{BzffBz�HB{\)B{�B|Q�B|��B}G�B}B~=qB~�\B~�HB\)B�
B�{B�Q�B��\B���B�
=B�G�B�p�B�B�  B�=qB�z�B���B��HB��B�G�B�p�B��B��B�(�B�z�B��RB���B�33B��B�B�  B�Q�B��\B���B���B�33B�p�B�B�{B�ffB��RB���B�G�B��B��B�  B�Q�B���B���B�G�B���B��
B�{B�z�B���B��B�p�B��B�{B�Q�B��\B��HB��B�p�B��
B�(�B�z�B���B��B�p�B��B�  B�=qB���B���B�\)B��B�{B�ffB��RB�
=B�\)B�B�(�B��\B���B�\)B�B�{B�ffB���B��B��B��B�Q�B���B�33B��B��B�=qB���B���B�p�B��B�Q�B��RB��B�p�B��
B�(�B���B�
=B��B��B�Q�B���B�
=B�p�B��
B�(�B���B�
=B��B�  B�ffB���B�33B���B��B�Q�B��RB�33B��B�  B�Q�B��RB��B���B�  B�ffB���B��B��B��B�Q�B���B�G�B���B�  B�ffB���B�33B��B�(�B�ffB���B�33B��B�(�B��\B��HB�G�B��B�{B�z�B���B�p�B��
B�(�B�z�B��HB�G�B��B�{B�z�B���B�G�B���B�  B�ffB���B�33B���B��B�Q�B���B�
=B�p�B��B�Q�B��\B���B�\)B��
B�=qB£�B�
=B�\)B��
B�=qBĸRB�
=B�\)B�B�(�Bƣ�B�
=B�p�B��
B�=qBȏ\B���BɅB��
B�=qBʏ\B�
=B˅B��B�ffB̸RB�33BͅB��B�Q�B���B�G�B�B�{B�ffB���B�G�B�B�(�Bҏ\B��HB�\)B��
B�Q�BԸRB��BՅB�  B�z�B���B�G�B�B�{Bأ�B��Bٙ�B�{B�z�B��HB�\)B��
B�Q�B��HB�G�BݮB�(�B޸RB�33Bߙ�B�{B��\B��B�B�{B�\B�
=B㙚B�(�B��B�
=B噚B�{B�RB�G�B�B�=qB�RB�33B��
B�ffB��HB�\)B�  B�\B�
=B홚B�{B��B�G�B��
B�Q�B���B�p�B�{B�\B��B�B�ffB���B�p�B��B���B�33B�B�Q�B��HB�p�B�{B��RB�G�B��
B�z�B�
=B��B�=qB���B�p�C 
=C \)C ��C �CG�C�\C�
C�Cp�C��C
=CQ�C��C�CG�C�C��C(�Cp�C�RC  C\)C�C��C=qC��C�C33Cz�C�
C	(�C	p�C	�RC

=C
ffC
�C
��C=qC��C�C33Cz�C�
C33C�C��C{Cp�CC
=CQ�C�C  CQ�C��C�HC=qC�C��C{CffC�C  CG�C�\C�
C(�Cp�C�C  CQ�C��C�
C33Cz�CC  CQ�C��C�HC33C�C��C
=C\)C�RC��C=qC�\C�HC�Cp�CC  C=qC�\C�HC33Cz�C�RC{CQ�C�\C�C33Cz�C�RC 
=C \)C ��C �HC!(�C!z�C!�RC"  C"Q�C"�\C"�
C#(�C#p�C#��C$  C$G�C$�C$C%{C%ffC%��C%�
C&�C&ffC&�\C&��C'{C'33C'\)C'�\C'�RC'�
C'�C({C((�C(=qC(Q�C(z�C(��C(��C(��C(��C)  C){C)33C)Q�C)p�C)z�C)�C)��C)�HC)��C*{C*=qC*G�C*\)C*�C*��C*�RC*��C*��C+{C+�C+G�C+p�C+�C+�\C+�RC+�HC+�C,
=C,33C,G�C,\)C,�C,��C,�RC,�
C-  C-�C-33C-G�C-p�C-�C-��C-��C-�HC-��C.{C.=qC.Q�C.ffC.�\C.�C.C.�HC/{C/�C/=qC/ffC/�C/��C/C/�C/��C0�C0G�C0\)C0z�C0��C0C0�HC1
=C1(�C1=qC1p�C1�\C1��C1��C2  C2{C233C2ffC2z�C2�\C2��C2�C3
=C3=qC3Q�C3z�C3��C3��C3�C4
=C4G�C4\)C4�C4�RC4�
C4��C5(�C5G�C5ffC5��C5C5�HC6  C633C6Q�C6z�C6��C6��C6��C7�C7Q�C7p�C7��C7��C7��C8{C8G�C8p�C8�\C8C8��C9{C9=qC9p�C9��C9�RC9�C:�C:=qC:p�C:��C:C:��C;(�C;G�C;p�C;�C;��C;��C<(�C<\)C<p�C<��C<�
C<��C=(�C=Q�C=p�C=�C=�
C=��C>(�C>Q�C>p�C>��C>�
C?  C?(�C?\)C?�C?�C?�HC@  C@33C@ffC@�C@�RC@�CA
=CAG�CAz�CA��CA�
CB  CB(�CB\)CB�CB�CB�HCC{CC33CCp�CC��CCCD  CD(�CDQ�CD�\CD�RCD�HCE�CEG�CEffCE��CE�
CE��CF33CFQ�CF�CF�RCF�HCG{CGQ�CGp�CG��CG��CG��CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                       ?�  @   @@  @�  @��R@��R@�  @��RA\)A   A,(�A@  A`  A�Q�A�Q�A��A�  A��AϮA�Q�A�Q�B   B�B�
B  B   B((�B0(�B8  B@(�BHQ�BP(�BX  B`(�Bh(�Bp��Bx(�B�  B��
B�  B�  B�  B�  B�{B�  B�  B��
B��B�  B�  B�  B�  B�  B��B�  B�  B�  B�{B�{B�  B�  B�  B�  B�  B�  B��B�  B�{B�  C 
=C  C  C
=C
=C

=C��C��C  C��C�C��C  C��C�C��C 
=C"{C$
=C&
=C({C)��C,  C.  C/��C2  C4  C6  C8  C9��C<  C>
=C@
=CB  CC��CF  CH
=CJ  CK��CM��CP  CQ��CS��CU��CX  CZ  C\  C^  C`
=Cb  Cd  Ce��Cg��Cj  Cl  Cn  Co��Cr  Ct  Cu��Cw��Cz  C|  C~  C�  C�  C�  C�  C�C�C�  C�  C�  C�  C�  C�C�  C�  C�  C�  C�C�  C�  C�C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�C�C�  C�  C�C�  C�  C�  C���C���C���C���C���C�C�  C�  C�  C�C�C�  C���C���C�  C�  C�C�
=C�C���C�  C�  C�C���C���C�  C�  C���C���C�  C�  C���C�  C�C�C�C�C�  C�  C�C�
=C�  C�  C�  C�  C�  C�
=C�C�  C���C�  C�C�  C���C���C�  C�
=C�C�  C�C�  C�  C�  C���C���C�  C�  C�  C�  C���C�  C�C�C�
=C���C���C�  C�  C�  C�  C�  C�  D   D }qD  D� D  D��D  D� D�D��D  D� D�D��D�qD}qD��D� D	�D	��D
�D
��D�D� D  D��DD��D�qD� D�qD� D�D� D  D}qD��D}qD  D� D�qD}qD�qD}qD�qD}qD�qD� D  D��D�qD}qD  D� D�D� D�qD� D�D��D�D�DD��D�qD }qD!  D!}qD!�qD"��D#�D#� D#�qD$� D%  D%� D%�qD&z�D'  D'��D(  D(� D(�qD)}qD*  D*��D+�D+}qD,  D,��D-  D-}qD.  D.�D/  D/� D0  D0}qD0�qD1� D2  D2� D3  D3}qD4  D4}qD4�qD5}qD6  D6� D7  D7� D8�D8��D9  D9� D:  D:}qD:�qD;� D<  D<� D<�qD=z�D=�qD>� D>�qD?� D@�D@��DA  DA� DA�qDB� DC�DC��DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI��DJ�DJ� DK  DK� DK�qDL� DMDM��DN  DN��DO  DO}qDP�DP� DQ  DQ� DR�DR�DS�DS� DT�DT��DU�DU� DV  DV� DW  DW� DX  DX��DYDY� DY�qDZ� D[�D[� D[�qD\� D\�qD]}qD^  D^}qD^�qD_� D`  D`}qD`�qDa� Db�Db� Dc  Dc��Dd  Ddz�Dd��De��Df�Df}qDf�qDg}qDh  Dh� Di�Di��Dj  Djz�Dj�qDk� Dl  Dl��DmDm��Dn  Dn��Do  Do� Dp  Dp}qDq�Dq��Dr�Dr� Ds  Ds� Dt  Dt� Du�Du��Dv  Dvz�Dv�qDw� Dx  Dx��DyDy��Dz  Dz� D{  D{}qD{�qD|}qD}  D}� D~  D~��D  D}qD�qD�@ D�� D�D�HD�>�D�� D���D���D�@ D�~�D���D�  D�>�D�~�D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�>�D�� D�� D���D�>�D���D�D�HD�@ D�~�D�� D�HD�@ D�~�D���D��qD�@ D��HD�� D�  D�>�D�~�D��HD�HD�AHD�� D���D��D�AHD�� D��qD�  D�B�D��HD��HD�  D�>�D�}qD�� D��D�AHD��HD��HD�  D�>�D�� D��HD�  D�@ D��HD��HD�HD�@ D�� D��HD�HD�AHD�� D���D���D�>�D�� D��HD���D�@ D�� D�� D�  D�@ D��HD���D���D�AHD��HD�� D���D�>�D�� D�� D�  D�>�D�~�D��HD�  D�@ D�� D�� D�HD�AHD��HD��HD��D�@ D�� D�� D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�>�D��HD�� D�  D�B�D�� D�� D�  D�@ D��HD�� D���D�>�D�~�D���D���D�@ D�� D��HD�HD�@ D�� D�� D�HD�>�D�� D�� D���D�>�D�� D��HD�HD�>�D�}qD��qD���D�@ D�� D�� D�  D�AHD��HD�� D���D�@ D��HD���D���D�AHD���D�� D���D�>�D�� D��HD�  D�>�D�� D��HD��D�B�D�~�D���D��qD�=qD�� D�� D���D�@ D�� D��qD�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D��HD�� D���D�>�D�� D�� D���D�=qD�� D��HD�HD�AHD�~�D���D���D�>�D��HD��HD�HD�AHD�� D�� D�HD�AHD�~�D��qD���D�@ D�~�D��HD�  D�@ D��HD�� D�  D�@ D�~�D���D���D�>�D�~�D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D��HD�HD�AHDÁHD��HD�  D�@ DĀ D�� D�  D�AHDŁHD�D��D�@ D�~�Dƾ�D���D�>�D�~�DǾ�D���D�>�D�}qD�� D�HD�AHDɁHD��HD�HD�AHDʀ DʽqD���D�@ DˁHD˾�D���D�@ D́HD�� D���D�@ D́HD�� D��qD�>�D΀ D��HD�HD�@ Dπ D�� D���D�@ DЁHD�� D�HD�B�DсHDѾ�D���D�>�DҁHD��HD�  D�AHDӀ D�� D�HD�AHD�~�DԾ�D�  D�AHDՀ D�� D���D�>�Dր D־�D�  D�@ D�~�D�� D�HD�AHD؀ D�� D�  D�@ Dـ D��HD�HD�@ Dڀ D��HD�  D�>�Dۀ D��HD�HD�@ D܀ Dܾ�D���D�@ D݀ Dݾ�D���D�@ DށHD�� D�  D�@ D߀ D�� D���D�>�D�� D�� D�HD�AHD�~�D�� D�  D�>�D�~�D�� D�HD�@ D�~�D㾸D�  D�@ D� D��HD�HD�AHD�HD��HD�HD�AHD� D澸D���D�>�D� D��HD�  D�@ D�~�D�qD��qD�>�D� D��HD�HD�AHD�~�D꾸D�  D�AHD�HD�� D��qD�@ D�HD�� D���D�@ D�HD�� D���D�@ DD�D�HD�AHD�HD�� D��qD�>�D�� D��HD�HD�AHD� D��HD�  D�>�D� D��HD�HD�>�D�~�D�� D�  D�AHD� D���D���D�@ D��HD��HD�HD�AHD��HD�� D�  D�>�D�~�D�� D���D�>�D�� D�� D�  D�>�D�~�D�� D�  D�/\G�O�?�?.{?k�?�z�?�p�?�(�@   @\)@!G�@333@E�@W
=@fff@z�H@��@���@�@�(�@��@���@�z�@�(�@\@�=q@��@ٙ�@�\@���@��@�Q�A   A�
AQ�A��A��Az�A��A��A!G�A%A(��A-p�A1G�A4z�A8��A<��A@��ADz�AH��AMp�AP��AU�AY��A]p�Ab�\AfffAj=qAn�RAs�
Aw�A{�A�  A�=qA���A��RA���A�33A�p�A��A��A�(�A�{A�Q�A�=qA�(�A�ffA���A�33A�p�A�\)A���A��
A�{A�  A��A��
A�{A�Q�A��\A�(�A�ffA���A\A���A�
=A�G�A�33A�p�AϮA��A�(�A�{A�Q�A��HA��A�
=A�G�A�A�A�  A��A�z�A�RA���A�33A�A��A�=qA�z�A��RB ��BB�HB  BG�B=qB�B��B	��B
�HB  B�B=qB\)Bz�B��B�RB�
B��B�B
=BQ�Bp�B�\B�B��B{B33B Q�B!��B"�RB#�
B%�B&=qB'\)B(z�B)��B*�RB,  B,��B.{B/\)B0z�B1��B2�RB3�
B4��B6{B7
=B8(�B9G�B:�\B;�B<��B>{B?
=B@(�BA�BBffBC�BDz�BE��BF�RBG�
BH��BIBJ�HBK�
BL��BN{BO33BP(�BQG�BR=qBS33BTQ�BUG�BV=qBW\)BXz�BYp�BZ�\B[�B\��B^{B_33B`Q�BaG�Bb=qBc33BdQ�Bep�Bf�\Bg�Bh��BiBj�HBk�
Bl��Bm��BnffBo33Bo�
Bp��Bq�Bq��Br{BrffBr�HBs33Bs�Bt  Btz�Bt��BuG�Bu��Bv{BvffBv�HBw\)Bw�Bx(�Bxz�Bx��ByG�ByBz{BzffBz�HB{\)B{�B|Q�B|��B}G�B}B~=qB~�\B~�HB\)B�
B�{B�Q�B��\B���B�
=B�G�B�p�B�B�  B�=qB�z�B���B��HB��B�G�B�p�B��B��B�(�B�z�B��RB���B�33B��B�B�  B�Q�B��\B���B���B�33B�p�B�B�{B�ffB��RB���B�G�B��B��B�  B�Q�B���B���B�G�B���B��
B�{B�z�B���B��B�p�B��B�{B�Q�B��\B��HB��B�p�B��
B�(�B�z�B���B��B�p�B��B�  B�=qB���B���B�\)B��B�{B�ffB��RB�
=B�\)B�B�(�B��\B���B�\)B�B�{B�ffB���B��B��B��B�Q�B���B�33B��B��B�=qB���B���B�p�B��B�Q�B��RB��B�p�B��
B�(�B���B�
=B��B��B�Q�B���B�
=B�p�B��
B�(�B���B�
=B��B�  B�ffB���B�33B���B��B�Q�B��RB�33B��B�  B�Q�B��RB��B���B�  B�ffB���B��B��B��B�Q�B���B�G�B���B�  B�ffB���B�33B��B�(�B�ffB���B�33B��B�(�B��\B��HB�G�B��B�{B�z�B���B�p�B��
B�(�B�z�B��HB�G�B��B�{B�z�B���B�G�B���B�  B�ffB���B�33B���B��B�Q�B���B�
=B�p�B��B�Q�B��\B���B�\)B��
B�=qB£�B�
=B�\)B��
B�=qBĸRB�
=B�\)B�B�(�Bƣ�B�
=B�p�B��
B�=qBȏ\B���BɅB��
B�=qBʏ\B�
=B˅B��B�ffB̸RB�33BͅB��B�Q�B���B�G�B�B�{B�ffB���B�G�B�B�(�Bҏ\B��HB�\)B��
B�Q�BԸRB��BՅB�  B�z�B���B�G�B�B�{Bأ�B��Bٙ�B�{B�z�B��HB�\)B��
B�Q�B��HB�G�BݮB�(�B޸RB�33Bߙ�B�{B��\B��B�B�{B�\B�
=B㙚B�(�B��B�
=B噚B�{B�RB�G�B�B�=qB�RB�33B��
B�ffB��HB�\)B�  B�\B�
=B홚B�{B��B�G�B��
B�Q�B���B�p�B�{B�\B��B�B�ffB���B�p�B��B���B�33B�B�Q�B��HB�p�B�{B��RB�G�B��
B�z�B�
=B��B�=qB���B�p�C 
=C \)C ��C �CG�C�\C�
C�Cp�C��C
=CQ�C��C�CG�C�C��C(�Cp�C�RC  C\)C�C��C=qC��C�C33Cz�C�
C	(�C	p�C	�RC

=C
ffC
�C
��C=qC��C�C33Cz�C�
C33C�C��C{Cp�CC
=CQ�C�C  CQ�C��C�HC=qC�C��C{CffC�C  CG�C�\C�
C(�Cp�C�C  CQ�C��C�
C33Cz�CC  CQ�C��C�HC33C�C��C
=C\)C�RC��C=qC�\C�HC�Cp�CC  C=qC�\C�HC33Cz�C�RC{CQ�C�\C�C33Cz�C�RC 
=C \)C ��C �HC!(�C!z�C!�RC"  C"Q�C"�\C"�
C#(�C#p�C#��C$  C$G�C$�C$C%{C%ffC%��C%�
C&�C&ffC&�\C&��C'{C'33C'\)C'�\C'�RC'�
C'�C({C((�C(=qC(Q�C(z�C(��C(��C(��C(��C)  C){C)33C)Q�C)p�C)z�C)�C)��C)�HC)��C*{C*=qC*G�C*\)C*�C*��C*�RC*��C*��C+{C+�C+G�C+p�C+�C+�\C+�RC+�HC+�C,
=C,33C,G�C,\)C,�C,��C,�RC,�
C-  C-�C-33C-G�C-p�C-�C-��C-��C-�HC-��C.{C.=qC.Q�C.ffC.�\C.�C.C.�HC/{C/�C/=qC/ffC/�C/��C/C/�C/��C0�C0G�C0\)C0z�C0��C0C0�HC1
=C1(�C1=qC1p�C1�\C1��C1��C2  C2{C233C2ffC2z�C2�\C2��C2�C3
=C3=qC3Q�C3z�C3��C3��C3�C4
=C4G�C4\)C4�C4�RC4�
C4��C5(�C5G�C5ffC5��C5C5�HC6  C633C6Q�C6z�C6��C6��C6��C7�C7Q�C7p�C7��C7��C7��C8{C8G�C8p�C8�\C8C8��C9{C9=qC9p�C9��C9�RC9�C:�C:=qC:p�C:��C:C:��C;(�C;G�C;p�C;�C;��C;��C<(�C<\)C<p�C<��C<�
C<��C=(�C=Q�C=p�C=�C=�
C=��C>(�C>Q�C>p�C>��C>�
C?  C?(�C?\)C?�C?�C?�HC@  C@33C@ffC@�C@�RC@�CA
=CAG�CAz�CA��CA�
CB  CB(�CB\)CB�CB�CB�HCC{CC33CCp�CC��CCCD  CD(�CDQ�CD�\CD�RCD�HCE�CEG�CEffCE��CE�
CE��CF33CFQ�CF�CF�RCF�HCG{CGQ�CGp�CG��CG��CG��CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                       @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�tG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aײ-Aװ!Aײ-A״9Aװ!Aװ!A׸RA׶FA׸RAװ!A׮Aװ!A׸RA׶FA׸RA׼jA׸RA״9Aי�A�r�A�5?A�oA�1A�%A�A���A���A��A��A��HA���A�v�AնFA�-A��A�Q�A��yA�%A�z�A���A��AƋDAş�A�ƨA�7LA�$�A��A���A�$�A�C�A�Q�A���A�O�A��A�A���A�ZA��7A��A�7LA���A�XA��yA�%A�A�A��A�\)A�-A�VA�p�A�`BA��PA���A�ȴA��A��A��TA��A���A��7A�oA�$�A�1A���A�z�A��jA�VA�1A�I�A�JA���A�9XA��A�x�A�v�A�v�A�=qA���A�/A}�FA{�Axr�At�Ap��Am��Ak�wAe��AbE�A`1'A\z�AYt�AU
=AP��AL�RAK��AJ�jAIXAF��AEl�AC��AA�^A?�
A=�PA;;dA9��A85?A7?}A6ffA5dZA4A2�A2ĜA3�-A3?}A2��A1S�A1\)A0�9A.��A.$�A-`BA,{A+�
A*�A)��A)�A(��A(�yA(��A(�jA(^5A'��A'&�A&(�A%l�A$E�A#;dA!��A"ZA"r�A"Q�A!��A �jA��A��A�\A9XA�mAG�A �AG�A�uA5?AJA�A�A�jA(�A�;A��At�A�9A��An�A��A`BAffAA�A7LA�mAS�A%A��A��A;dA�HA�A1'A7LA
=A
��A
I�A	hsA��AZA�FAC�AȴA��A�\A��AS�A�A��AjA��A�-A|�AS�A"�A��A�!AVA�FAVA �9A r�A J@�dZ@���@���@���@�b@��
@�|�@��!@���@�x�@���@��m@�t�@��@��@�X@��u@�1@�|�@�ȴ@�E�@��/@�Q�@�dZ@��H@�-@�@�Z@��@�n�@�{@�@���@���@�Z@畁@���@���@�hs@�`B@�hs@��@�9X@���@�33@�+@�@�^5@ᙚ@�7L@���@�z�@��@�+@ޟ�@ݡ�@���@۶F@��@�$�@�{@�-@���@�hs@ؓu@�(�@�dZ@�^5@�hs@�&�@�bN@��
@ӝ�@�dZ@�
=@�v�@�@�x�@�X@�%@�A�@��;@ϝ�@�dZ@��@�v�@�J@�hs@��@̃@˶F@�C�@���@�ȴ@ʟ�@��#@�7L@���@�z�@ǶF@�l�@�o@�ff@�@��@Ĵ9@�Q�@þw@Ý�@��H@�ff@�$�@��#@�/@���@� �@��
@�;d@�^5@��^@�hs@�X@��`@��D@�I�@��
@���@�v�@��@��@���@�X@��/@��9@��u@�Q�@� �@��w@��@���@�-@�O�@��`@�  @��@�C�@��@��\@�{@���@�p�@��@�9X@��;@�l�@��H@���@�v�@���@��h@�7L@���@�A�@��m@���@�dZ@�@�@���@�9X@��@��H@�n�@�V@�{@�@�O�@�Ĝ@���@�9X@��@���@�S�@��y@�v�@�V@�-@��@�&�@�Ĝ@�z�@�I�@�1@�\)@��R@�M�@�J@�@��@��#@��#@�@�@��7@�`B@�O�@�V@��`@��@�A�@�1@��F@��@�K�@��y@���@�ff@��@��@�$�@�@���@���@��@�G�@���@�(�@��
@���@�C�@��@��R@��!@��+@�^5@��@�J@��-@�`B@��@���@���@�A�@�1@��;@��@�K�@�;d@�o@��!@�$�@��T@��@���@��-@�`B@�?}@��/@��u@��@�j@�bN@��@��@��@�S�@��@��@���@��-@�`B@���@�j@� �@�  @��w@�|�@�|�@�\)@�o@��H@��R@�~�@�5?@�{@��@��-@��7@�?}@�V@��j@��@�bN@�Q�@� �@��w@���@���@�
=@���@��R@��@���@�X@�&�@��@��`@���@�r�@�(�@��
@���@�C�@���@��H@��R@���@��+@�E�@�-@�-@�-@�J@��@���@���@���@�@��@�X@�?}@�7L@�&�@���@�Q�@�;@\)@�@~�@~V@}�-@|�j@|9X@{�@z�@z��@z��@z^5@yx�@xQ�@xb@w�P@v{@u�h@u�@u`B@u�@t�j@tZ@s�
@s�@sC�@r�@r�\@r-@q��@q�7@qX@qG�@q7L@p�9@pb@o�;@o�w@n�y@n��@n$�@m��@m@m�-@m`B@l�@lz�@l�@k��@kƨ@k�@kdZ@k"�@j�\@jM�@j=q@j-@i�@i��@i%@hA�@g�@gK�@f�y@fV@f5?@e�T@e`B@d�@d�D@d�@cƨ@ct�@c"�@b�H@b��@b�\@b-@a�^@ahs@a%@`�@`A�@_�@_|�@_;d@^�@^V@]�@]�h@]�@\z�@[��@[��@[C�@[@Z��@Z~�@Z-@Y�#@Y��@YX@XĜ@XA�@W\)@V��@V�@Vv�@V{@U�@U�h@U?}@T��@Tz�@TI�@T�@S�
@S��@SS�@R��@R�\@R~�@Rn�@R^5@R�@Qhs@P�9@P �@O�@Nȴ@N@M��@MO�@M/@MV@L�/@LZ@L1@K�
@K�F@K��@K��@Kt�@KC�@J�@J-@I��@I��@Ix�@IX@IG�@I%@HĜ@H1'@G�;@G�P@GK�@F�@F�+@F{@E�-@E�@D�/@D�j@D�D@DZ@D(�@C�m@CS�@B��@Bn�@BM�@B=q@A��@A&�@@A�@?|�@?+@?
=@>�y@>ȴ@>�R@>��@>V@>$�@>{@>@=�T@=�h@<��@<j@;�m@;C�@:�\@:=q@9��@9�^@9x�@9�@8��@8��@8bN@8A�@81'@8 �@8  @7�P@7�@6ȴ@6ff@6{@5��@5�-@5�h@5p�@5O�@4��@4�/@4��@4�@4Z@49X@3�m@3��@3"�@3@2��@2��@2=q@1��@1�#@1��@1��@1��@1G�@1%@0Ĝ@0��@0�@0Q�@0b@0  @/�@/�w@/�P@/+@.�+@.5?@-��@-��@-�@-`B@-`B@-/@-�@-�@-V@,��@,�/@,�j@,�@,j@,I�@,(�@,1@+�m@+S�@+"�@*�@*�!@*��@*~�@*^5@*^5@*-@)��@)hs@)&�@(�`@(��@(Ĝ@(r�@(b@'��@';d@&�@&�@&��@&ff@&5?@&@%�-@%p�@$�@$�j@$z�@$(�@$1@#��@#��@#C�@#o@#o@"�@"�@"n�@"J@!��@!��@!x�@!G�@ ��@ Ĝ@ bN@�;@�P@;d@��@�y@�R@ff@$�@{@@�T@@�-@�h@?}@�/@�@z�@I�@1@�@�@�!@�\@^5@-@�@�^@�7@X@&�@�`@�u@1'@�;@��@�P@K�@;d@�@��@�@�R@ff@@��@@�h@�h@�@p�@`B@`B@�@��@z�@j@I�@(�@��@33@@�@�@�H@��@M�@-@�@��@hs@�@%@��@�9@��@r�@A�@ �@�;@�P@\)@+@�y@��@�+@ff@{@��@O�@��@�@��@�@�D@j@I�@(�@�@��@��@C�@
�H@
�!@
��@
n�@
n�@
n�@
M�@
=q@
-@
=q@
=q@
-@
-@
-@
�@	�@	�7@	X@	&�@	�@��@��@A�@1'@ �@ �@b@bA׮Aײ-Aײ-Aײ-Aײ-Aװ!Aװ!Aװ!Aװ!Aײ-Aײ-Aװ!Aײ-A׸RAײ-Aװ!Aװ!A׮Aװ!A׮A׮Aװ!Aײ-Aײ-A״9A׸RA׺^A׸RA׶FA״9A׶FA׶FA׶FA׺^A׸RA׶FAײ-A״9Aײ-A׮A׬A׮A׬A׬A׬A׬A׬A״9A׶FA׺^A׸RA׺^A׸RA׺^A׶FA׸RA׸RA׸RA׶FA׸RA׸RA׸RA׶FA׶FA׶FA׸RA׸RA׺^A׺^A׸RA׸RA׺^A׼jA׼jA׼jA׼jA׼jA׼jA׸RA׸RA׸RA׸RA׸RA׸RA׸RA׸RA׸RA׸RA׸RA׺^A׺^A׺^Aװ!Aש�Aק�Aץ�Aק�Aץ�Aכ�Aכ�Aח�A�|�A�z�A�x�A�z�A�x�A�v�A�p�A�jA�\)A�O�A�M�A�=qA�/A��A��A��A��A�{A�{A�bA�bA�VA�JA�JA�
=A�1A�%A�1A�%A�A�A�%A�%A�%A�%A�%A�%A�1A�%A�%A�A�A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��mA��`A��;A��;A��;A��#A��#A��A��A��
A���A���A���A���A�ƨA���Aִ9A֟�AփA�VA�A�A��A�A��HA�ƨAպ^AլAգ�AՋDA�v�A�O�A��#A��yA�ZAҗ�A�r�A��HA�x�A�5?A��A�A��
A���Aϟ�A�|�A�ffA�A�A�1AΩ�A�l�A�=qA� �A���A��A��yA��#A�ȴA;wAͧ�A͉7A�p�A�dZA�XA�I�A�=qA�-A��A�1A���A��A��#A���A̼jA̛�A�r�A�Q�A�"�A���A��#A���AˬA˃A�VA�-A��A��A���A���A���A�Aʺ^AʶFAʲ-AʮAʩ�Aʣ�Aʝ�Aʗ�Aʕ�AʑhAʏ\AʍPAʋDAʉ7AʃA�z�A�t�A�`BA�bNA�Q�A�K�A�E�A�C�A�9XA��A�ȴA�l�A�+A���AȰ!AȬAȥ�A�XA�/A���A���Aǲ-AǛ�A�z�A�`BA�K�A�7LA�$�A�oA�1A���A��A��;A���A�ĜAƾwAƸRAƲ-AƮAƬAƧ�Aƣ�Aƕ�A�~�A�n�A�dZA�ZA�K�A�=qA�/A� �A��A�VA��A��#AŬA�l�A�{A��HAĮA�ZA��
A�ZA�1A�ƨA�ĜAhA�S�A��A��HA��RA��A���A���A��A�dZA�A�A��A��FA���A��A�v�A�t�A�p�A�Q�A�/A���A��A��HA��
A���A�ĜA��wA��-A���A��DA��A�hsA�G�A�{A��A�z�A�^5A�v�A�x�A���A���A�ƨA�
=A���A��mA��FA���A���A��7A�O�A�(�A��`A���A�5?A��
A���A��PA�p�A�S�A�A�A�(�A��A�JA��#A��!A��uA���A��wA��^A�l�A�33A���A���A��-A��^A��FA��9A��9A��RA��^A��FA���A��DA�p�A�dZA�\)A�Q�A�Q�A�Q�A�S�A�Q�A�K�A�A�A�=qA�?}A�E�A�I�A�K�A�E�A�=qA�/A�(�A���A�ĜA��\A� �A�oA�A���A���A���A���A���A��A��yA��HA��;A��HA��HA��HA��;A��A���A��FA���A���A���A���A��7A��A�jA�7LA�{A��A���A��jA���A��hA�~�A�|�A�~�A��A�x�A�l�A�M�A�E�A�9XA��A�%A��#A���A���A���A��PA�~�A�n�A�VA�9XA� �A��A�oA��A��wA�hsA�
=A��wA��uA�jA�;dA�%A��TA��+A��hA��PA�^5A�"�A�VA�1'A�^5A�XA�(�A��A���A��`A�ƨA��A���A��hA�x�A�K�A�"�A�%A��
A���A�+A���A�~�A�XA�C�A�(�A�%A��-A�bNA�5?A�+A�"�A� �A��A��mA���A�p�A�XA�O�A�I�A�O�A�VA�O�A�=qA�9XA�33A�9XA�1'A��A��\A�?}A�oA�JA��A��`A���A�ĜA���A��A�r�A�Q�A�A�A�$�A��
A�~�A�I�A�?}A�5?A��A�  A��#A��jA��A�ZA�S�A�M�A�1'A��mA�JA���A�x�A�l�A�dZA�(�A��
A�ĜA��FA���A��7A�r�A�bNA�Q�A��A���A�dZA�`BA�XA�1'A�A��
A���A�r�A�A�A�$�A�bA��HA���A�t�A�VA�7LA� �A��A�VA�1A���A��A��
A��^A���A�hsA�7LA�JA��TA���A��\A�jA�I�A��A��`A��!A�C�A��A���A��9A��PA�~�A�dZA�I�A�5?A��A��;A���A���A�C�A���A��`A�O�A�  A���A�E�A��A��wA�v�A�S�A��A���A��A��HA���A�`BA�$�A�{A�JA�  A��`A��#A���A��-A��PA�dZA�K�A�;dA�-A��A�A��A���A���A�M�A�  A���A��;A��;A��
A�ȴA���A��A�`BA�?}A� �A���A��A��^A���A��+A�hsA�XA�K�A�C�A�-A�{A���A��HA�ĜA���A��uA�~�A�bNA�E�A�+A��A���A��`A�ƨA���A�^5A�
=A��RA�E�A��A���A�n�A�1'A�VA���A��A��!A��A�ZA�+A���A���A��A��A�hsA�G�A�$�A�A��A�ĜA���A�hsA�1'A��;A���A�t�A�hsA�S�A�?}A�9XA��A�%A��A���A���A���A��FA��!A���A���A��hA��DA��A�p�A�\)A�&�A���A���A���A��A�v�A�jA�I�A�$�A�  A�ȴA�|�A�5?A���A���A��A�^5A�;dA�A���A���A�`BA�(�A��A���A�hsA�"�A��
A��+A�dZA�A�A� �A���A��9A���A�t�A�$�A��yA��/A��RA��uA�ZA��A���A���A�l�A�/A�oA�A��A��mA��mA��/A���A��jA���A�S�A�G�A�5?A��A���A��wA�|�A�M�A�-A�bA���A��A��#A�ȴA��RA��+A�VA�{A���A�ȴA��DA�9XA�A��AA~ffA}��A}��A}p�A}?}A}+A}VA|��A|��A|�uA|ffA|A{�FA{��A{�A{p�A{dZA{\)A{XA{;dAz��Az��Az�9Azn�AzbAyAyC�AxJAwt�Aw`BAv��Av��Av=qAu��At��At�AtĜAt�\AtbNAt=qAs�As/Arn�Ar=qAr�Ar  Aq�Aq��Aq��AqVApQ�Ao��An��AnI�An�Am�
Am�
Am��AmAm�FAm�Am��Am�hAm�7Amp�Am`BAmS�Am;dAl��Al�Ak�Ak�-AkdZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                       Aײ-Aװ!Aײ-A״9Aװ!Aװ!A׸RA׶FA׸RAװ!A׮Aװ!A׸RA׶FA׸RA׼jA׸RA״9Aי�A�r�A�5?A�oA�1A�%A�A���A���A��A��A��HA���A�v�AնFA�-A��A�Q�A��yA�%A�z�A���A��AƋDAş�A�ƨA�7LA�$�A��A���A�$�A�C�A�Q�A���A�O�A��A�A���A�ZA��7A��A�7LA���A�XA��yA�%A�A�A��A�\)A�-A�VA�p�A�`BA��PA���A�ȴA��A��A��TA��A���A��7A�oA�$�A�1A���A�z�A��jA�VA�1A�I�A�JA���A�9XA��A�x�A�v�A�v�A�=qA���A�/A}�FA{�Axr�At�Ap��Am��Ak�wAe��AbE�A`1'A\z�AYt�AU
=AP��AL�RAK��AJ�jAIXAF��AEl�AC��AA�^A?�
A=�PA;;dA9��A85?A7?}A6ffA5dZA4A2�A2ĜA3�-A3?}A2��A1S�A1\)A0�9A.��A.$�A-`BA,{A+�
A*�A)��A)�A(��A(�yA(��A(�jA(^5A'��A'&�A&(�A%l�A$E�A#;dA!��A"ZA"r�A"Q�A!��A �jA��A��A�\A9XA�mAG�A �AG�A�uA5?AJA�A�A�jA(�A�;A��At�A�9A��An�A��A`BAffAA�A7LA�mAS�A%A��A��A;dA�HA�A1'A7LA
=A
��A
I�A	hsA��AZA�FAC�AȴA��A�\A��AS�A�A��AjA��A�-A|�AS�A"�A��A�!AVA�FAVA �9A r�A J@�dZ@���@���@���@�b@��
@�|�@��!@���@�x�@���@��m@�t�@��@��@�X@��u@�1@�|�@�ȴ@�E�@��/@�Q�@�dZ@��H@�-@�@�Z@��@�n�@�{@�@���@���@�Z@畁@���@���@�hs@�`B@�hs@��@�9X@���@�33@�+@�@�^5@ᙚ@�7L@���@�z�@��@�+@ޟ�@ݡ�@���@۶F@��@�$�@�{@�-@���@�hs@ؓu@�(�@�dZ@�^5@�hs@�&�@�bN@��
@ӝ�@�dZ@�
=@�v�@�@�x�@�X@�%@�A�@��;@ϝ�@�dZ@��@�v�@�J@�hs@��@̃@˶F@�C�@���@�ȴ@ʟ�@��#@�7L@���@�z�@ǶF@�l�@�o@�ff@�@��@Ĵ9@�Q�@þw@Ý�@��H@�ff@�$�@��#@�/@���@� �@��
@�;d@�^5@��^@�hs@�X@��`@��D@�I�@��
@���@�v�@��@��@���@�X@��/@��9@��u@�Q�@� �@��w@��@���@�-@�O�@��`@�  @��@�C�@��@��\@�{@���@�p�@��@�9X@��;@�l�@��H@���@�v�@���@��h@�7L@���@�A�@��m@���@�dZ@�@�@���@�9X@��@��H@�n�@�V@�{@�@�O�@�Ĝ@���@�9X@��@���@�S�@��y@�v�@�V@�-@��@�&�@�Ĝ@�z�@�I�@�1@�\)@��R@�M�@�J@�@��@��#@��#@�@�@��7@�`B@�O�@�V@��`@��@�A�@�1@��F@��@�K�@��y@���@�ff@��@��@�$�@�@���@���@��@�G�@���@�(�@��
@���@�C�@��@��R@��!@��+@�^5@��@�J@��-@�`B@��@���@���@�A�@�1@��;@��@�K�@�;d@�o@��!@�$�@��T@��@���@��-@�`B@�?}@��/@��u@��@�j@�bN@��@��@��@�S�@��@��@���@��-@�`B@���@�j@� �@�  @��w@�|�@�|�@�\)@�o@��H@��R@�~�@�5?@�{@��@��-@��7@�?}@�V@��j@��@�bN@�Q�@� �@��w@���@���@�
=@���@��R@��@���@�X@�&�@��@��`@���@�r�@�(�@��
@���@�C�@���@��H@��R@���@��+@�E�@�-@�-@�-@�J@��@���@���@���@�@��@�X@�?}@�7L@�&�@���@�Q�@�;@\)@�@~�@~V@}�-@|�j@|9X@{�@z�@z��@z��@z^5@yx�@xQ�@xb@w�P@v{@u�h@u�@u`B@u�@t�j@tZ@s�
@s�@sC�@r�@r�\@r-@q��@q�7@qX@qG�@q7L@p�9@pb@o�;@o�w@n�y@n��@n$�@m��@m@m�-@m`B@l�@lz�@l�@k��@kƨ@k�@kdZ@k"�@j�\@jM�@j=q@j-@i�@i��@i%@hA�@g�@gK�@f�y@fV@f5?@e�T@e`B@d�@d�D@d�@cƨ@ct�@c"�@b�H@b��@b�\@b-@a�^@ahs@a%@`�@`A�@_�@_|�@_;d@^�@^V@]�@]�h@]�@\z�@[��@[��@[C�@[@Z��@Z~�@Z-@Y�#@Y��@YX@XĜ@XA�@W\)@V��@V�@Vv�@V{@U�@U�h@U?}@T��@Tz�@TI�@T�@S�
@S��@SS�@R��@R�\@R~�@Rn�@R^5@R�@Qhs@P�9@P �@O�@Nȴ@N@M��@MO�@M/@MV@L�/@LZ@L1@K�
@K�F@K��@K��@Kt�@KC�@J�@J-@I��@I��@Ix�@IX@IG�@I%@HĜ@H1'@G�;@G�P@GK�@F�@F�+@F{@E�-@E�@D�/@D�j@D�D@DZ@D(�@C�m@CS�@B��@Bn�@BM�@B=q@A��@A&�@@A�@?|�@?+@?
=@>�y@>ȴ@>�R@>��@>V@>$�@>{@>@=�T@=�h@<��@<j@;�m@;C�@:�\@:=q@9��@9�^@9x�@9�@8��@8��@8bN@8A�@81'@8 �@8  @7�P@7�@6ȴ@6ff@6{@5��@5�-@5�h@5p�@5O�@4��@4�/@4��@4�@4Z@49X@3�m@3��@3"�@3@2��@2��@2=q@1��@1�#@1��@1��@1��@1G�@1%@0Ĝ@0��@0�@0Q�@0b@0  @/�@/�w@/�P@/+@.�+@.5?@-��@-��@-�@-`B@-`B@-/@-�@-�@-V@,��@,�/@,�j@,�@,j@,I�@,(�@,1@+�m@+S�@+"�@*�@*�!@*��@*~�@*^5@*^5@*-@)��@)hs@)&�@(�`@(��@(Ĝ@(r�@(b@'��@';d@&�@&�@&��@&ff@&5?@&@%�-@%p�@$�@$�j@$z�@$(�@$1@#��@#��@#C�@#o@#o@"�@"�@"n�@"J@!��@!��@!x�@!G�@ ��@ Ĝ@ bN@�;@�P@;d@��@�y@�R@ff@$�@{@@�T@@�-@�h@?}@�/@�@z�@I�@1@�@�@�!@�\@^5@-@�@�^@�7@X@&�@�`@�u@1'@�;@��@�P@K�@;d@�@��@�@�R@ff@@��@@�h@�h@�@p�@`B@`B@�@��@z�@j@I�@(�@��@33@@�@�@�H@��@M�@-@�@��@hs@�@%@��@�9@��@r�@A�@ �@�;@�P@\)@+@�y@��@�+@ff@{@��@O�@��@�@��@�@�D@j@I�@(�@�@��@��@C�@
�H@
�!@
��@
n�@
n�@
n�@
M�@
=q@
-@
=q@
=q@
-@
-@
-@
�@	�@	�7@	X@	&�@	�@��@��@A�@1'@ �@ �@bG�O�A׮Aײ-Aײ-Aײ-Aײ-Aװ!Aװ!Aװ!Aװ!Aײ-Aײ-Aװ!Aײ-A׸RAײ-Aװ!Aװ!A׮Aװ!A׮A׮Aװ!Aײ-Aײ-A״9A׸RA׺^A׸RA׶FA״9A׶FA׶FA׶FA׺^A׸RA׶FAײ-A״9Aײ-A׮A׬A׮A׬A׬A׬A׬A׬A״9A׶FA׺^A׸RA׺^A׸RA׺^A׶FA׸RA׸RA׸RA׶FA׸RA׸RA׸RA׶FA׶FA׶FA׸RA׸RA׺^A׺^A׸RA׸RA׺^A׼jA׼jA׼jA׼jA׼jA׼jA׸RA׸RA׸RA׸RA׸RA׸RA׸RA׸RA׸RA׸RA׸RA׺^A׺^A׺^Aװ!Aש�Aק�Aץ�Aק�Aץ�Aכ�Aכ�Aח�A�|�A�z�A�x�A�z�A�x�A�v�A�p�A�jA�\)A�O�A�M�A�=qA�/A��A��A��A��A�{A�{A�bA�bA�VA�JA�JA�
=A�1A�%A�1A�%A�A�A�%A�%A�%A�%A�%A�%A�1A�%A�%A�A�A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��mA��`A��;A��;A��;A��#A��#A��A��A��
A���A���A���A���A�ƨA���Aִ9A֟�AփA�VA�A�A��A�A��HA�ƨAպ^AլAգ�AՋDA�v�A�O�A��#A��yA�ZAҗ�A�r�A��HA�x�A�5?A��A�A��
A���Aϟ�A�|�A�ffA�A�A�1AΩ�A�l�A�=qA� �A���A��A��yA��#A�ȴA;wAͧ�A͉7A�p�A�dZA�XA�I�A�=qA�-A��A�1A���A��A��#A���A̼jA̛�A�r�A�Q�A�"�A���A��#A���AˬA˃A�VA�-A��A��A���A���A���A�Aʺ^AʶFAʲ-AʮAʩ�Aʣ�Aʝ�Aʗ�Aʕ�AʑhAʏ\AʍPAʋDAʉ7AʃA�z�A�t�A�`BA�bNA�Q�A�K�A�E�A�C�A�9XA��A�ȴA�l�A�+A���AȰ!AȬAȥ�A�XA�/A���A���Aǲ-AǛ�A�z�A�`BA�K�A�7LA�$�A�oA�1A���A��A��;A���A�ĜAƾwAƸRAƲ-AƮAƬAƧ�Aƣ�Aƕ�A�~�A�n�A�dZA�ZA�K�A�=qA�/A� �A��A�VA��A��#AŬA�l�A�{A��HAĮA�ZA��
A�ZA�1A�ƨA�ĜAhA�S�A��A��HA��RA��A���A���A��A�dZA�A�A��A��FA���A��A�v�A�t�A�p�A�Q�A�/A���A��A��HA��
A���A�ĜA��wA��-A���A��DA��A�hsA�G�A�{A��A�z�A�^5A�v�A�x�A���A���A�ƨA�
=A���A��mA��FA���A���A��7A�O�A�(�A��`A���A�5?A��
A���A��PA�p�A�S�A�A�A�(�A��A�JA��#A��!A��uA���A��wA��^A�l�A�33A���A���A��-A��^A��FA��9A��9A��RA��^A��FA���A��DA�p�A�dZA�\)A�Q�A�Q�A�Q�A�S�A�Q�A�K�A�A�A�=qA�?}A�E�A�I�A�K�A�E�A�=qA�/A�(�A���A�ĜA��\A� �A�oA�A���A���A���A���A���A��A��yA��HA��;A��HA��HA��HA��;A��A���A��FA���A���A���A���A��7A��A�jA�7LA�{A��A���A��jA���A��hA�~�A�|�A�~�A��A�x�A�l�A�M�A�E�A�9XA��A�%A��#A���A���A���A��PA�~�A�n�A�VA�9XA� �A��A�oA��A��wA�hsA�
=A��wA��uA�jA�;dA�%A��TA��+A��hA��PA�^5A�"�A�VA�1'A�^5A�XA�(�A��A���A��`A�ƨA��A���A��hA�x�A�K�A�"�A�%A��
A���A�+A���A�~�A�XA�C�A�(�A�%A��-A�bNA�5?A�+A�"�A� �A��A��mA���A�p�A�XA�O�A�I�A�O�A�VA�O�A�=qA�9XA�33A�9XA�1'A��A��\A�?}A�oA�JA��A��`A���A�ĜA���A��A�r�A�Q�A�A�A�$�A��
A�~�A�I�A�?}A�5?A��A�  A��#A��jA��A�ZA�S�A�M�A�1'A��mA�JA���A�x�A�l�A�dZA�(�A��
A�ĜA��FA���A��7A�r�A�bNA�Q�A��A���A�dZA�`BA�XA�1'A�A��
A���A�r�A�A�A�$�A�bA��HA���A�t�A�VA�7LA� �A��A�VA�1A���A��A��
A��^A���A�hsA�7LA�JA��TA���A��\A�jA�I�A��A��`A��!A�C�A��A���A��9A��PA�~�A�dZA�I�A�5?A��A��;A���A���A�C�A���A��`A�O�A�  A���A�E�A��A��wA�v�A�S�A��A���A��A��HA���A�`BA�$�A�{A�JA�  A��`A��#A���A��-A��PA�dZA�K�A�;dA�-A��A�A��A���A���A�M�A�  A���A��;A��;A��
A�ȴA���A��A�`BA�?}A� �A���A��A��^A���A��+A�hsA�XA�K�A�C�A�-A�{A���A��HA�ĜA���A��uA�~�A�bNA�E�A�+A��A���A��`A�ƨA���A�^5A�
=A��RA�E�A��A���A�n�A�1'A�VA���A��A��!A��A�ZA�+A���A���A��A��A�hsA�G�A�$�A�A��A�ĜA���A�hsA�1'A��;A���A�t�A�hsA�S�A�?}A�9XA��A�%A��A���A���A���A��FA��!A���A���A��hA��DA��A�p�A�\)A�&�A���A���A���A��A�v�A�jA�I�A�$�A�  A�ȴA�|�A�5?A���A���A��A�^5A�;dA�A���A���A�`BA�(�A��A���A�hsA�"�A��
A��+A�dZA�A�A� �A���A��9A���A�t�A�$�A��yA��/A��RA��uA�ZA��A���A���A�l�A�/A�oA�A��A��mA��mA��/A���A��jA���A�S�A�G�A�5?A��A���A��wA�|�A�M�A�-A�bA���A��A��#A�ȴA��RA��+A�VA�{A���A�ȴA��DA�9XA�A��AA~ffA}��A}��A}p�A}?}A}+A}VA|��A|��A|�uA|ffA|A{�FA{��A{�A{p�A{dZA{\)A{XA{;dAz��Az��Az�9Azn�AzbAyAyC�AxJAwt�Aw`BAv��Av��Av=qAu��At��At�AtĜAt�\AtbNAt=qAs�As/Arn�Ar=qAr�Ar  Aq�Aq��Aq��AqVApQ�Ao��An��AnI�An�Am�
Am�
Am��AmAm�FAm�Am��Am�hAm�7Amp�Am`BAmS�Am;dAl��Al�Ak�Ak�-AkdZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
qB
�B
xB
�B
�B
xB
qB
=B
=B
xB
�B
�B
	B
�B
xB
CB
IB
�B
)�B
5?B
EmB
K�B
LdB
LdB
K^B
K�B
L�B
LdB
K^B
K^B
H�B
C�B
0�B
!�B	��B	�yB	�B	ݘB	��B	�B	�[B	�2B	�B
~B
&B
0�B
7�B
ZB
�%B
p;B
�SB
��B
��B
�yB
�fB�BFB(�BC-BXB]�BjB��B�B��B��B��B��B�mB�,B��B��B�NB�/B��B��B�B��B�B�B��B�B��B��B��B�BzDBs�BpoBh�BB�B
�B
�B
�]B
�BB
�PB
i�B
PHB
49B
�B	��B	�,B	��B	�wB	��B	�B	kB	G�B	9�B	&�B	eB	B��B�`B�BٴB��B�<BȴB��B�0B�tB��B��B�B�LB�$B�*B�OB�RB�B��B	,=B	0�B	0�B	0UB	?B	F?B	P�B	TaB	_�B	iyB	h�B	|�B	��B	��B	��B	��B	�CB	�IB	�3B	��B	��B	�B	�'B	�kB	�B	�$B	��B	�B	�UB	ŢB	ȀB	��B	��B	��B	��B	�pB	ΥB	�EB	��B	�B	��B	ŢB	��B	ÖB	� B	�HB	��B	�EB	��B	ƨB	��B	��B	�$B	��B	��B	��B	�^B	�nB	��B	�nB	�nB	��B	��B	��B	�tB	�^B	��B	��B	�6B	B	�OB	��B	�UB	�OB	��B	�OB	��B	�}B	��B	B	�OB	�OB	�}B	��B	��B	ƨB	�B	��B	�tB	�B	�EB	��B	�KB	ȴB	ȀB	��B	�XB	ɆB	�XB	��B	�)B	��B	�)B	˒B	��B	ʌB	͟B	��B	�^B	�^B	��B	�dB	�6B	�B	�B	�0B	��B	͟B	��B	��B	�B	ΥB	��B	��B	�B	ϫB	�}B	�B	�B	�TB	ӏB	ԕB	�9B	��B	�2B	��B	�sB	��B	�dB	��B	��B	ܒB	ݘB	��B	�]B	��B	�dB	��B	ݘB	�dB	�/B	ݘB	�WB	��B	�B	�EB	�yB	�B	�;B	�B	�;B	�B	�B	�B	�;B	��B	�B	�;B	�;B	�B	�pB	�HB	�HB	�B	�BB	�B	��B	�|B	�HB	�HB	��B	�|B	��B	�B	�vB	��B	�B	�B	� B	�B	�&B	�ZB	�B	��B	�B	��B	��B	�fB	�B	�B	�B	�`B	��B	��B	��B	�B	�2B	�B	�mB	�B	�B	�B	�B	�QB	�WB	��B	�WB	�WB	�)B	��B	��B	��B	��B	�;B	�B	��B	��B	�GB	�B	�MB	�B	�B	��B	�TB	�%B	�B	��B	�TB	�ZB	��B	��B	��B	��B	��B	�8B	�>B	�>B	��B	�B	�B	��B	�PB	�B	��B	�"B	�"B	��B	��B	�.B	��B	�cB	�.B	��B	��B	��B	�.B
  B
 �B
 iB
 iB
 �B
B
B
uB
�B
�B
�B
�B
�B
{B
MB
�B
B
SB
%B
�B
YB
%B
�B
�B
�B
	7B

	B
�B
�B
�B
�B
�B
�B
VB
�B
"B
�B
�B
�B
"B
�B
�B
�B
\B
�B
bB
 B
B
uB
B
MB
MB
�B
�B
�B
�B
{B
�B
�B
�B
MB
�B
�B
�B
kB
�B
kB
=B
�B
=B
�B
xB
CB
�B
~B
B
B
�B
OB
�B
�B
�B
�B
�B
 �B
 'B
�B
 �B
 'B
 �B
 �B
 �B
"4B
"4B
"4B
"hB
"hB
#B
"�B
#nB
$tB
#�B
#:B
#nB
#nB
$�B
%FB
%B
%�B
'�B
'�B
($B
(�B
(�B
)*B
)�B
)�B
*�B
*�B
+6B
+�B
+6B
,B
,B
-CB
-B
,�B
-�B
-�B
,�B
.�B
.B
-B
-wB
-wB
-CB
-CB
-CB
-wB
.IB
.�B
/OB
/�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
2aB
2�B
2�B
33B
49B
4B
4B
49B
49B
4�B
6�B
5�B
5tB
5?B
5tB
5tB
5�B
5�B
6zB
8RB
8�B
7�B
7�B
8B
9XB
9�B
9XB
:�B
;�B
;�B
;�B
;�B
<6B
<�B
=B
=qB
=qB
=�B
>B
>BB
>�B
>�B
?B
?HB
?B
?B
?�B
@OB
@OB
@�B
A�B
A�B
B[B
B[B
B[B
B[B
B�B
C-B
CaB
C�B
C�B
C�B
C�B
D3B
DgB
D�B
EB
EB
EB
EB
E9B
F?B
F�B
GEB
GzB
G�B
HB
HB
HB
H�B
H�B
IRB
I�B
IRB
I�B
I�B
I�B
JXB
J�B
J�B
K)B
K)B
K^B
K�B
K�B
K�B
L�B
MB
M6B
M�B
NB
NpB
N�B
OvB
O�B
PB
PHB
P}B
P�B
P�B
QNB
QNB
QNB
Q�B
Q�B
R B
R�B
R�B
R�B
S&B
S[B
S[B
S�B
S�B
TaB
T�B
U2B
U�B
U�B
VB
VmB
V�B
W
B
W
B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
XyB
XyB
XEB
XEB
XyB
X�B
YKB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
YKB
YKB
YKB
YKB
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[#B
[�B
\)B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]/B
]dB
^5B
^jB
^jB
^5B
^5B
^jB
_pB
`BB
`�B
aB
aB
aHB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bNB
c B
cTB
c�B
dZB
d�B
e,B
e`B
e`B
e�B
e�B
e`B
e,B
e`B
e`B
e`B
e`B
e�B
f2B
f�B
f�B
f�B
gB
gmB
g8B
gmB
g�B
h
B
iB
h�B
h�B
iB
iDB
iDB
jB
jKB
jB
jB
j�B
k�B
k�B
k�B
k�B
k�B
l"B
l"B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
ncB
n�B
o B
oiB
oiB
o�B
o�B
o�B
p;B
pB
p;B
pB
poB
poB
pB
p;B
poB
p;B
pB
p;B
p�B
qAB
qvB
rGB
rGB
rB
rGB
rB
rB
rGB
r|B
sB
s�B
s�B
s�B
s�B
tTB
t�B
t�B
u%B
t�B
t�B
u%B
t�B
u%B
u�B
uZB
u�B
v`B
v`B
v�B
w2B
wfB
w�B
w�B
x8B
x8B
x8B
x8B
x8B
x�B
y	B
y>B
yrB
yrB
y�B
y�B
zB
z�B
z�B
{JB
{�B
{�B
{�B
|B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}�B
}�B
}�B
~(B
~(B
~]B
~�B
.B
�B
�B
� B
�4B
�4B
��B
��B
�B
�B
�oB
��B
�AB
��B
��B
��B
�B
�GB
�{B
�{B
��B
��B
�B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�SB
��B
�%B
�%B
��B
�%B
��B
��B
��B
��B
��B
��B
��B
�fB
��B
��B
��B
�B
��B
�7B
��B
��B
��B
��B
��B
�	B
�rB
�rB
��B
�B
�DB
�xB
�xB
�xB
�B
�~B
�B
�PB
�PB
��B
��B
��B
��B
��B
��B
��B
��B
�"B
��B
�(B
��B
��B
�.B
�.B
�bB
��B
��B
��B
��B
��B
� B
� B
� B
� B
��B
��B
�:B
�:B
�:B
��B
��B
��B
��B
��B
��B
�B
��B
kB
=B
qB
�B
qB
xB
B
CB
B
�B
�B
�B
xB
	B
�B
�B
�B
�B
xB
�B
B
B
xB
CB
�B
qB
=B
�B
=B
�B
�B
	B
�B
�B
qB
xB
CB
CB
�B
�B
�B
B
�B
�B
CB
xB
CB
kB
=B
=B
	B
�B
=B
7B
�B
�B
	B
=B
qB
=B
�B
=B
CB
xB
xB
�B
CB
CB
�B
�B
�B
CB
B
B
qB
�B
�B
CB
�B
IB
B
~B
B
IB
~B
~B
B
�B
�B
CB
CB
�B
 \B
#B
$tB
%�B
%�B
&LB
)*B
)_B
*�B
2�B
2�B
4nB
3hB
49B
49B
5tB
7�B
;dB
?B
>�B
C�B
GzB
J�B
J�B
J�B
K^B
K^B
K^B
K�B
K�B
L0B
K�B
LdB
LdB
L�B
L�B
LdB
LdB
L�B
L�B
L�B
L�B
L�B
L0B
LdB
K�B
K�B
K�B
J�B
J�B
J�B
K^B
K�B
K�B
K�B
K�B
K�B
K�B
LdB
L�B
MB
MB
MB
L�B
MB
K�B
L0B
L0B
L0B
L0B
L�B
LdB
L0B
L�B
K�B
K�B
K^B
J�B
K)B
K^B
J�B
L0B
LdB
L�B
J�B
J#B
K)B
JXB
JXB
JXB
JXB
IRB
H�B
GEB
G�B
F�B
F�B
FtB
G�B
GB
CaB
>wB
@B
8�B
9�B
2aB
1�B
.�B
-wB
*eB
'B
,qB
4nB
,�B
%B
�B
�B
1B
�B	�B	�TB	�ZB	�B	��B	�GB	�/B	�)B	�B	�B	�ZB	�yB	�B	�
B	��B	��B	�B	�B	��B	��B	�sB	�mB	��B	��B	�B	�B	�B	�&B	�TB	�NB	��B	��B	�B	�B	�B	�B	�|B	�B	�B	�jB	�B	��B	�B	��B	�;B	�TB	�B	�jB	ܒB	�WB	�#B	�WB	��B	چB	چB	چB	��B	یB	�)B	��B	�WB	�WB	�QB	�B	�B	��B	خB	�B	��B	�dB	֡B	��B	�yB	خB	�sB	��B	�B	یB	�)B	�2B	�B	�vB	��B	ϫB	�)B	�2B	�
B	ԕB	ҽB	��B	ҽB	ҽB	��B	��B	��B	�&B	ҽB	ӏB	�[B	��B	ԕB	�,B	�aB	��B	�gB	�gB	�,B	�2B	�,B	רB	רB	��B	�gB	�gB	��B	��B	ҽB	��B	�,B	�KB	��B	�/B	�BB	�
B	�fB	�
B	�mB	��B
oB
+B
{B
 �B
�B
B
{B
�B
!�B
OB
�B
!bB
#B
&B
$�B
&�B
*�B
-�B
(�B
*eB
+kB
+B
,=B
5?B
4�B
7LB
0�B
1'B
1�B
1[B
1�B
3hB
5�B
8B
5�B
7�B
<�B
;�B
;�B
?HB
@�B
C�B
@�B
NpB
B�B
[WB
O�B
�B
�oB
�:B
��B
�1B
�_B
��B
��B
�B
�oB
��B
}"B
v�B
m�B
m]B
rB
p;B
qvB
o�B
k�B
n�B
u�B
t�B
x�B
wfB
��B
��B
��B
�_B
�LB
��B
��B
�xB
�B
�B
�3B
��B
�dB
�gB
�tB
ŢB
�B
��B
�9B
�zB
��B
ɺB
�B
ΥB
ҽB
�[B
��B
�?B
چB
ܒB
�|B
�B
�"B
�5B
�;B
��B
��B
�%B
�sB
�)B
ܒB
�;B
�B
�B
�yB
�B
��B
�]B
��B
�rB
��B{B�BBBfB
�BBB	�B	lB�B~BoBB �B&B%�B#�B$�B#nB"hB \B%FB)�B2aB6FB>BB=�B=�BB�B?BD�BEBH�BI�BJ�BK�BN�BRTBU�BV�BYB`BkQBlWBm)Bk�BW?BR�BP�BT�BZ�BVB\]B_�BpoBs�Bo�B]�BT�B��B�DB��B��B��B��B�OB��B��B��B�CB��B�B�tB�*B��B�B�_B��B�'B��B��B��B��B��B�=B�=B�CB��B��B��B��B�0B�<B��B�B��B��B�3BÖBƨB�tB�EB�BB��B��B�]B�?BѷB��B��BҽBѷB�gB� B�[B�9B�&B�?B�B�fBچB�sB��BچB��BیBیB�BیBרBخB�dB��B�DB�BںB֡B֡B�BרB�?B��B��B�B�B��B�B��B��BیB��B�QB��B�/BܒBیBخB��B�B�2BӏB՛B�mB��B�HB�dB��B�^B�B�B��B��BɺB��B��B�BB��B�B��B��B��B��B�6B�-B�LB�6B�eB��B�B��B��B��B��B��B�RB��B��B��B��B��B�xB�(B��B��B�SB��B~�B�B��B�{B�{B�SB��B��B��B�B�B�+B��B�B��B�YB��B�uB}VB{Bz�BzBy>Bx�Bv�BzB~�Bv�Bo�BqBm�Bm�Bl�BpBq�BoiBn�Bp�Bq�Br|Br�BpoBpBo5Bm]Bk�BiDBl"BiyBg�Bd�BcTBd&BaHB`BB^5B\]B[WBV9BU�BQ�BS�BR�BM�BR BLdBLdB>B:*B1�B-B$�B#�BB�BOBqB�B�B@BVB�B	lB�B�B�B�B �B;B iB
��B
��B
�B
�B
�B
�iB
�]B
�KB
��B
�yB
��B
�,B
� B
�B
�B
��B
ޞB
�|B
�B
��B
�B
ޞB
�;B
�B
�dB
ںB
�EB
��B
��B
ݘB
�BB
�B
ܒB
�B
�vB
֡B
��B
��B
�jB
�0B
�KB
�tB
��B
�B
�jB
��B
��B
��B
�!B
��B
�IB
�nB
�	B
��B
��B
�=B
�(B
��B
��B
��B
�{B
}�B
}VB
~�B
}�B
w�B
w�B
o B
|�B
l�B
e�B
b�B
c B
_�B
^jB
`BB
aB
_�B
_B
cTB
S�B
Q�B
RTB
R B
R�B
IB
J#B
C�B
A�B
?�B
=qB
:*B
<6B
8RB
:�B
7B
4�B
+�B
-�B
/�B
)�B
#�B
%B
%FB
CB
�B
B
�B
VB
B

�B
�B
�B
�B
�B
xB
�B	�cB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�|B	�]B	�B	�]B	�`B	֡B	خB	�#B	��B	ӏB	�,B	��B	��B	�3B	�B	�[B	�UB	��B	�}B	�B	�nB	��B	�[B	��B	��B	��B	�aB	�nB	�B	�B	�IB	�~B	�eB	��B	��B	�_B	�SB	�B	��B	��B	��B	�FB	�oB	��B	��B	��B	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                       B
0B
~B
B
jB
�B
�B
0B
�B
0B
6B
6B
dB
�B
~B
B
B
"B
.B
#B
'8B
6�B
<jB
=B
=B
<B
<�B
=�B
="B
<PB
<�B
;�B
9�B
/OB
#�B	�cB	� B	ݲB	�TB	ӏB	�.B	ǮB	�0B	�OB
B
�B
%,B
-]B
N�B
��B
f�B
��B
�B
�B
�VB
ؓB
�FB	�B�B8�BQ�BU�B]�B��B��B��B��B��B��B��B�)BѝBԕB��B�BٚBңB�B��B�HB�OB��B�B��B~�B|�By�BqABi*Bi_Bi_BG_B
	B
�B
�VB
��B
�0B
d&B
LB
2�B
B	�$B	�&B	�UB	��B	�<B	��B	g�B	@�B	6`B	"�B	B	DB��B�QB�TBϫB̈́B��B�}B�B��B�IB��B��B��B��B�CB� B�+B�B�RB�>B	OB	#TB	&LB	!�B	2|B	=VB	DgB	H1B	T�B	[�B	^B	p�B	utB	x�B	~]B	��B	��B	��B	��B	�WB	��B	��B	�B	�'B	�xB	��B	�B	��B	�B	�DB	�B	�"B	�JB	�B	��B	��B	�GB	�0B	��B	�$B	�LB	�B	�zB	�2B	�B	�AB	�zB	�	B	�B	�dB	��B	��B	�WB	��B	��B	�QB	��B	��B	��B	�B	�
B	��B	��B	�B	��B	��B	��B	�B	�}B	��B	��B	��B	�nB	��B	�;B	��B	�AB	��B	��B	�B	��B	�B	��B	��B	��B	�B	��B	�RB	��B	�XB	�xB	�B	�xB	��B	��B	�B	��B	�B	�VB	��B	��B	�PB	�qB	�B	��B	��B	�B	��B	�B	�B	�BB	��B	�B	��B	�.B	�BB	�B	��B	�cB	��B	�OB	��B	��B	�aB	��B	�;B	�uB	��B	�uB	�B	��B	��B	�B	�YB	�%B	��B	�rB	ȴB	�B	ϫB	��B	��B	��B	�B	�B	ΊB	��B	��B	��B	�\B	��B	��B	ΊB	��B	˒B	�RB	�RB	�vB	� B	�TB	� B	�hB	��B	ңB	��B	�B	��B	ЗB	ЗB	ЗB	�hB	�[B	ҽB	�:B	��B	�[B	�uB	��B	ңB	�B	өB	�@B	��B	�FB	�:B	�,B	ՁB	ՁB	�B	��B	�mB	�mB	�{B	�{B	ּB	�sB	�sB	�yB	ٴB	�1B	�EB	�
B	��B	�B	�?B	��B	׍B	�yB	ٚB	ڠB	یB	ۦB	�)B	��B	�jB	�jB	�~B	�B	޸B	�5B	ބB	�\B	�B	��B	��B	�B	�B	�B	�@B	�tB	��B	�`B	�B	�fB	�B	�B	��B	�B	��B	�B	�RB	�RB	�>B	��B	��B	�B	��B	�"B	�B	��B	��B	��B	�]B	�B	�B	�B	�B	�OB	�B	�B	�B	��B	��B	�vB	�[B	�vB	�B	�GB	�B	��B	�-B	��B	��B	�B	�tB	�`B	��B	�?B	�ZB	�%B	�tB	��B	�2B	��B	��B	�fB	��B	��B	��B	��B	�DB	��B	��B	��B	��B	��B	��B	�wB	�.B	�}B	��B	�}B
  B
 �B	��B	�}B
  B
 4B
 OB
 �B
oB
�B
[B
�B
MB
9B
�B
�B
�B
B
�B
tB
�B
9B
mB
YB
tB
�B

#B
	�B
�B
�B
�B
�B
B
~B
6B
B
�B
�B
�B
�B
(B
(B
�B
�B
�B
�B
�B
B
B
NB
NB
TB
4B
�B
�B
�B
�B
uB
�B
FB
�B
,B
�B
�B
SB
gB
�B
�B
�B
9B
9B
9B
YB
�B
1B
B
�B
B
QB
�B
=B
CB
�B
�B
�B
]B
B
IB
�B
5B
�B
�B
B
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
 BB
 �B
!B
!�B
!�B
!�B
!�B
"NB
"�B
"hB
"�B
"�B
# B
"�B
#:B
#�B
#�B
$�B
%`B
%B
$�B
%FB
%zB
&�B
(>B
'8B
&�B
&fB
&�B
'B
'�B
'RB
(
B
)�B
)yB
(�B
)*B
)�B
+QB
+B
*�B
,�B
-)B
,�B
,�B
,�B
-wB
-�B
.cB
.�B
.�B
/ B
/OB
/�B
/�B
0B
0!B
0;B
0B
0oB
1[B
1[B
1[B
2-B
2�B
2�B
3�B
3MB
3MB
3�B
4B
4nB
4�B
4�B
4�B
5B
5B
5ZB
5�B
5�B
5�B
5�B
6+B
6FB
6�B
7�B
8B
8�B
8�B
9	B
9$B
9>B
9rB
9�B
9�B
:�B
:�B
:xB
:�B
;B
:�B
;dB
;�B
<6B
<PB
<jB
<�B
="B
="B
=<B
=�B
>BB
>�B
?B
?HB
?�B
@OB
@�B
A B
A;B
AoB
A�B
A�B
BB
BuB
B[B
B�B
B�B
CGB
C�B
C�B
C�B
C�B
DgB
DgB
D�B
D�B
EB
EmB
E�B
F?B
F�B
F�B
G+B
G�B
G�B
G�B
G�B
G�B
G�B
HfB
I7B
IRB
I7B
I�B
J	B
I�B
IlB
IRB
I�B
I�B
J�B
KB
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J#B
J�B
KDB
KxB
K�B
LB
LJB
LB
L~B
LJB
L�B
MPB
M�B
NB
N"B
N"B
M�B
M�B
NB
N<B
NVB
N�B
O�B
OvB
O\B
O(B
O\B
PB
QB
Q�B
Q�B
RB
RB
R:B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
SB
S�B
T�B
T�B
UMB
U�B
VB
VSB
V�B
V�B
V�B
V�B
VmB
VSB
VSB
VSB
VSB
VmB
V�B
WsB
W�B
XB
W�B
X+B
X_B
X+B
X_B
X�B
Y1B
ZB
Y�B
Y�B
Z7B
ZQB
ZkB
[=B
[�B
[�B
[�B
[�B
\�B
]B
\�B
\xB
\�B
]B
]IB
]�B
]�B
]�B
^B
^jB
^�B
^�B
^�B
^�B
^�B
_B
_�B
_�B
`BB
`vB
`\B
`�B
`vB
`�B
a-B
`�B
a-B
`�B
abB
abB
`�B
aHB
abB
a-B
`�B
aHB
a�B
bNB
b�B
cTB
c:B
cB
c:B
b�B
c B
c�B
c�B
d@B
d�B
d�B
d�B
eB
e�B
e�B
e�B
ffB
e�B
fB
f2B
e�B
f2B
f�B
f�B
f�B
gmB
g�B
g�B
h>B
h�B
h�B
h�B
iDB
iB
i*B
i*B
i�B
jB
j0B
j0B
jB
jB
j�B
j�B
kQB
lB
lB
lqB
l�B
l�B
l�B
mCB
m]B
mwB
m�B
m�B
m�B
m�B
m�B
nIB
n�B
o B
o B
o5B
oOB
o�B
pUB
pUB
p�B
p�B
qB
q'B
qvB
q�B
q�B
rB
r-B
r�B
r�B
shB
s�B
s�B
s�B
tB
t9B
tnB
tnB
t�B
t�B
uZB
u�B
u�B
u�B
u�B
vB
vB
vB
u�B
vFB
vzB
v�B
wB
wB
v�B
w�B
w�B
w�B
x�B
x�B
x�B
y$B
y$B
yXB
y�B
y�B
y�B
z*B
y�B
zDB
z�B
z�B
z�B
z�B
z�B
{0B
{�B
{B
{�B
|6B
|PB
|jB
|�B
|�B
}<B
}�B
~BB
~BB
~BB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
HB
�B
�4B
��B
��B
�B
�B
�UB
��B
��B
�UB
�oB
��B
��B
��B
��B
�B
��B
��B
�GB
�-B
�GB
��B
�B
��B
��B
��B
��B
��G�O�B
B
�B
B
JB
B
B
�B
�B
�B
PB
�B
PB
B
�B
�B
PB
VB
�B
B
PB
�B
�B
B
�B
~B
B
�B
JB
�B
JB
DB
�B
DB
xB
B
B
�B
�B
�B
PB
PB
�B
PB
�B
�B
B
�B
B
�B
�B
�B
DB
�B

�B
xB
xB
�B
�B
B
�B
JB
�B
�B
B
B
JB
�B
�B
PB
�B
�B
�B
�B
�B
B
JB
~B
�B
�B
�B
�B
"B
�B
�B
"B
"B
�B
�B
�B
�B
�B
PB
 B
�B
B
SB
SB
�B
�B
B
qB
#nB
#�B
%B
$B
$�B
$�B
&B
($B
,B
/�B
/�B
4nB
8B
;dB
;�B
;0B
<B
<B
<B
<jB
<�B
<�B
<�B
=B
=B
=<B
=qB
=B
=B
=qB
=<B
=<B
=<B
=qB
<�B
=B
<�B
<6B
<6B
;dB
;�B
;�B
<B
<jB
<jB
<6B
<jB
<6B
<�B
=B
=<B
=�B
=�B
=�B
=<B
=�B
<�B
<�B
<�B
<�B
<�B
=qB
=B
<�B
=qB
<�B
<6B
<B
;0B
;�B
<B
;�B
<�B
=B
=qB
;�B
:�B
;�B
:�B
:�B
:�B
:�B
9�B
9$B
7�B
8�B
7�B
7�B
7B
8RB
7�B
4B
/B
0�B
)_B
*0B
#B
"hB
VB
B
	B
�B
B
%B
IB
�B
�B
	�B	��B	�B	�B	��B	��B	�HB	�vB	��B	��B	��B	�HB	�;B	��B	�B	�WB	خB	�gB	�mB	�2B	�,B	֡B	՛B	�B	�B	�gB	՛B	�2B	�2B	�aB	��B	��B	��B	҉B	҉B	ҽB	ѷB	�NB	�[B	� B	�NB	ѷB	�B	ϫB	�pB	ΥB	�}B	��B	��B	�#B	�(B	�PB	�B	��B	�B	ˬB	�DB	�DB	�DB	ˬB	�JB	��B	�~B	�B	�B	�B	��B	��B	ɠB	�lB	��B	ȚB	�"B	�_B	ˬB	�7B	�lB	�1B	ƎB	�=B	�JB	��B	��B	��B	�4B	��B	�iB	��B	��B	��B	�SB	�{B	ðB	�{B	�{B	ðB	ðB	āB	��B	�{B	�MB	�B	āB	�SB	��B	�B	ƎB	�%B	�%B	��B	��B	��B	�fB	�fB	ƎB	�%B	�%B	żB	ðB	�{B	ňB	��B	�	B	ʦB	��B	� B	خB	�
B	خB	�+B	�B	�B	��B	�B	�AB	�`B
�B
B

=B
�B
�B
bB
B
�B
�B
�B
YB
qB
�B
eB
	B
B
�B
�B
%�B
%FB
'�B
!bB
!�B
"�B
!�B
"hB
$B
&LB
(�B
&�B
(XB
-CB
,=B
,=B
/�B
1[B
4nB
1�B
?B
33B
K�B
@�B
t�B
�B
��B
~(B
x�B
xB
}�B
w�B
u�B
rB
qAB
m�B
g�B
^�B
^B
b�B
`�B
bB
`vB
\�B
_pB
f2B
e�B
iyB
h
B
.B
�]B
�B
�B
��B
�(B
��B
�B
��B
��B
��B
�sB
��B
��B
��B
�+B
��B
��B
��B
�B
�lB
�DB
��B
�.B
�GB
��B
āB
��B
�B
�B
�B
׍B
ܬB
߾B
��B
�zB
�\B
�B
��B
̳B
�B
��B
�4B
�B
�B
�CB
�OB
��B
�zB
��B
�}B
�B
�B
��B
��B
��B
�0B
��B
��B
�*B
��B
�6B
�B�B
�BNB�B9B,BgB�B�B�B�BQB"�B&�B.�B.cB./B3B/�B5%B5�B9>B:xB;B<�B?cBB�BF%BG_BJ	BP�B[�B\�B]�B\BG�BC{BAoBE�BKBF�BL�BP.B`�Bd@B`'BNVBEBu%B{�B�B��B�B�DB��B�PB�lB�=B��B�B�{B��B��B��B��B��B�B��B�hB�-B�nB�$B�B��B��B��B�=B�	B�B�'B��B��B�CB�}B��B��B��B�B�B��B��B��B�9B�TB��BǮB�'B�gB�gB�-B�'B��BB��BƨBÖBǮB�vB��B��B��B�KB��B�0B��B��B�pB��B�B�B��B�5B�B�vB�)B�B�B��B�BǮB�dB�NB�}B�,B�NBуB�gB�ZB��B�XB��B�HB͟B�B��B�B�RB�tBŢB��B�B��B�3B��B��B�6B��B��B��B�XB�dB�*B�XB�RB�zB��B�B��B�B��B�'B�'B��B��B��B��B��B�\B�~B�YB�	B�yB�aB�aB��B�TB�	B�mB�-B�aB�B�B� Bz^Bu�B~BBoOBu�BwBtBtBu�B}qB�B~BBt�Bs�Bw�BxBu�Bu?Bv�BwBr�Bm�BlBk6Bj�Bi�Bi_Bg�Bj�BoBgRB`\Ba�B^OB^OB]~B`�BbhB_�B_VBa-BbhBcBcnB`�B`�B_�B]�B\BY�B\�BZBX+BUMBS�BT�BQ�BP�BN�BL�BK�BF�BFYBBuBDMBC{B>(BB�B<�B<�B.�B*�B"NB�B2B,B�BB�B�B	B
#B�B
��B
�B
��B
�$B
�$B
�?B
�hB
�'B
��B
�B
�5B
�OB
�B
�&B
�BB
�B
�B
��B
ݘB
�B
�B
��B
��B
רB
ҽB
҉B
�BB
� B
бB
͟B
ϫB
�BB
��B
�,B
�B
�^B
��B
�tB
ÖB
�<B
��B
ϫB
�6B
ϫB
�B
�EB
�zB
ŢB
�B
��B
��B
�B
�UB
��B
�B
�FB
�nB
�FB
��B
�'B
��B
�B
��B
�1B
��B
��B
�B
~]B
~]B
~�B
tB
n�B
m�B
oiB
n�B
hsB
hsB
_�B
m)B
]�B
V�B
S[B
S�B
PHB
OB
P�B
Q�B
P}B
O�B
S�B
D�B
B�B
CB
B�B
CGB
9�B
:�B
4�B
2�B
0oB
./B
*�B
,�B
)B
+�B
'�B
%�B
]B
jB
 �B
QB
aB
�B
B
B
tB
�B	�HB	�B	��B	��B	�jB	�LB	�zB	��B	�6B	�B	�!B	�B	�B	�B	�kB	�_B	�B	�B	�@B	��B	�:B	�B	�CB	�B	�B	�_B	�lB	��B	ƎB	�MB	��B	��B	��B	��B	��B	�B	�B	�|B	�;B	��B	�FB	�zB	�4B	��B	��B	��B	�:B	�FB	��B	��B	�"B	�VB	�=B	�fB	�_B	�7B	�+B	��B	��B	��B	��B	�B	�GB	�oB	��B	��B	�{B	}�B	~�B	zxG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<g�y<�8k<#�
<#�
<*�V<#�
<#�
<#�
<#�
<#�
<~�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<)�3<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<\�<iA�<#�
<#�
<#�
<#�
<#�
<#�
<�b<�~�<m��<2�<�=�<�9<h�<#�
<6��<fz�<#�
<,��<g�<A��<#�
<#�
<���<B-�<#�
<Cכ<4�f<p�|<i7�<P��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OWC V3.0: r =0.9996(+/-0.0001), vertically averaged dS =-0.0148(+/-0.0023)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     NO correction for Conductivity Thermal Mass (CTM) is applied;    OWC V3.0: r =0.9996(+/-0.0001), vertically averaged dS =-0.0148(+/-0.0023)                                                                                                                     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0];                                                                                         PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0];                                                                                         PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2020090717504420200907175044IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020091717004920200917170049QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020091717004920200917170049QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014120210427140141IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                