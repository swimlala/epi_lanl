CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-01-08T19:28:55Z creation; 2021-04-29T20:27:09Z DMQC;      
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
_FillValue        G�O�     p  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     p  dT   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     p  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʀ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  �\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p @d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p g�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210108192855  20210429202818  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_157                 6810_008521_157                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�U0�"�@�U0�"�11  @�U0�4֡@�U0�4֡@2�T�w[�@2�T�w[��e ��a��e ��a�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              
   
?��@�\@E�@�  @�  @�  @�G�A ��A  A   A,(�AAG�A`��A�Q�A��A��A��A��AϮA�\)A�  B (�B�
B�
B  B   B'�
B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�{B�(�B�  B�  B��B�{B�{B�  B�  B�  B�{B�  B��B�  B�(�B�(�B�{B�  B�{B�  B�  B�  B�  B�  B�{B�(�B�(�B�{B�{B�{B�{B�(�C {C{C{C  C
=C

=C��C�C��C  C  C
=C  C{C  C��C��C!��C#�C%�C'�C)��C,  C.
=C0
=C2{C4  C5��C7��C:
=C;��C=��C?�CB  CD  CF
=CH
=CI��CK��CN
=CP  CQ��CT  CV{CX{CY��C\  C]��C_�Cb
=Cd{Cf{Ch{Cj{Cl
=Cn
=Cp  Cr  Ct{Cv
=Cx
=Cy��C|  C}��C�  C�C�  C�C���C�C�
=C�  C���C�  C���C�  C���C���C���C���C�  C�  C�C�  C�  C�C�  C�C�  C�  C�C�C�C�
=C���C�  C�C�C���C���C�C���C���C���C�
=C�
=C���C�\C�C���C�C���C�C�
=C�C�
=C�C���C�  C�
=C�C�  C���C���C�  C�  C���C�  C���C���C���C���C���C���C���C���C�C�C�
=C�  C�\C�C���C���C�
=C�
=C�  C���C���C�  C���C�  C�C���C�  C�  C���C���C��C���C�  C�  C�C���C���C�
=C�C���C�  C�  C�C���C���C���C�C�
=C�  C���C�C�  C���C���C�  C�C���C�  C�C���C���C�  C�  C�  D   D ��D �qD}qD�D��D  D� D�D�DD��D  D�DD� D�qD� D	�D	��D
  D
� D  D� D  D� D  D� D�D� D  D� D�qD}qD�qD� D  D� D�qD}qD�qD}qD  D��D  D� D  D� D  Dz�D�RD}qD�D��D�D��D  D� D  D� D  Dz�D�qD� D   D � D!  D!��D"  D"��D#�D#��D$D$�D%  D%}qD%�qD&}qD'D'� D'�qD(}qD(��D)}qD*�D*��D+  D+}qD,�D,� D,�qD-� D.�D.��D/  D/}qD/�qD0� D1  D1� D1�qD2}qD3  D3��D4  D4� D5  D5}qD6  D6}qD6�qD7��D8  D8}qD8�qD9}qD9��D:}qD;�D;��D;�qD<� D=D=��D=�qD>� D?  D?� D@  D@� DA  DAz�DB  DB��DC�DC� DD  DD��DE�DE��DF  DF�DG�DG� DH  DH� DI�DI�DJ  DJ}qDJ�qDKz�DK�qDL�DMDM��DN�DN}qDN�qDO}qDO��DP� DP�qDQz�DR  DR}qDR��DS}qDS�qDT}qDU  DU��DV�DV� DW�DW��DX  DX� DY�DY��DZ�DZ��D[�D[��D\  D\}qD\��D]}qD^  D^}qD^�qD_� D_��D`z�D`�qDa� Db  Db� Dc�Dc�Dd�Dd��De�De� Df�Df� Df�qDg��Dh�Dh� Di�Di��Di�qDjz�Dj��Dk}qDk�qDl� Dl��Dmz�Dn  Dn��Do  Do}qDp  Dp��Dq�Dq� Dr  Drz�Dr��Ds}qDs�qDt� Du  Du��Dv�Dv}qDw  Dw��Dx  Dx}qDy  Dy� Dy��Dz}qD{  D{}qD|  D|��D}  D}� D~  D~}qD~�qD� D�  D�@ D�� D���D���D�@ D�~�D�� D�  D�@ D�� D�� D���D�@ D��HD���D���D�@ D�� D��HD�  D�@ D��HD�� D�  D�@ D�� D���D��qD�@ D��HD���D�  D�@ D�� D���D�  D�AHD�� D�� D�  D�@ D�~�D��qD���D�AHD�� D��qD��qD�@ D��HD���D���D�AHD��HD��HD�  D�@ D�~�D���D�HD�AHD��HD��HD���D�>�D��HD��HD���D�>�D�� D�� D�HD�AHD�}qD��qD�HD�B�D��HD�D�HD�@ D��HD��HD�HD�B�D��HD���D���D�@ D�~�D�� D���D�@ D��HD��HD��D�B�D��HD���D�  D�@ D�~�D��qD���D�@ D�� D��HD�  D�AHD�~�D���D���D�>�D�~�D��qD���D�AHD�� D��HD�HD�>�D�~�D��HD�  D�>�D�� D���D�  D�AHD�� D��HD��D�AHD�� D���D���D�>�D�~�D���D�  D�@ D��HD��HD��D�@ D�~�D��qD���D�>�D�� D�D�HD�@ D�~�D���D�HD�B�D�~�D��qD�  D�AHD�� D���D���D�@ D��HD��HD���D�>�D�~�D�� D���D�@ D��HD�� D�HD�@ D�� D�� D���D�@ D�� D���D�  D�@ D�� D��HD�  D�@ D��HD��HD�HD�AHD�� D���D��qD�@ D��HD�D�HD�>�D�~�D��HD��D�C�D�� D��qD��qD�>�D�� D��HD�HD�AHD�~�D���D�HD�B�D�� D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�>�D��HD�� D�  D�@ D��HD�D��D�@ D�� D��HD�HD�AHD�� D�� D�  D�>�D�� D�D�  D�=qD�~�D��HD�  D�@ D��HD��HD��D�AHD�� D�� D�  D�AHD�� D�� D�  D�AHD D��HD�  D�@ DÁHD��HD�HD�AHDĀ D�� D��D�B�Dŀ D��HD�  D�@ DƁHD��HD�  D�AHDǁHD�� D���D�>�D�~�D�� D�HD�AHDɁHD��HD�  D�AHDʁHD�� D�  D�@ Dˀ D�� D�  D�>�D̀ D��HD�  D�>�D̀ D��HD�  D�@ D�~�D�� D�  D�@ D�~�D�� D���D�@ D�~�Dо�D�  D�@ Dр D�� D�HD�AHDҁHD��HD���D�=qDӀ D��HD�HD�@ DԀ D�� D�  D�@ DՀ Dվ�D���D�@ Dր D�� D�HD�@ D�~�D׽qD���D�@ D؀ Dؾ�D�  D�@ D�~�D��HD�  D�@ DځHD��HD��D�AHDۀ D۽qD�  D�@ D܁HD��HD�  D�AHD݀ Dݾ�D���D�>�Dހ D��HD�  D�>�D߀ D��HD���D�@ D��HD�� D�  D�@ D�~�D��HD�HD�>�D�}qD⾸D���D�@ D�HD㾸D���D�@ D� D侸D���D�AHD�HD�� D�  D�>�D� D��HD�  D�@ D�HD��HD�  D�@ D�HD�D�HD�>�D�~�D龸D�HD�B�D� D�� D�  D�@ D�HD��HD�HD�>�D�~�D쾸D�  D�>�D�~�D��HD�HD�>�D�~�D�� D�  D�AHD�HD�� D�  D�>�D��HD���D�HD�>�D� D�� D���D�@ D�HD��HD�  D�@ D� D�� D�HD�AHD�HD��HD��D�AHD�~�D��qD���D�@ D�� D���D���D�@ D�� D��qD���D�@ D��HD��HD���D�=qD�}qD�� D��
?8Q�?k�?�\)?�Q�?��?��@��@��@+�@=p�@J=q@^�R@p��@z�H@�=q@���@�
=@��\@�=q@��@�(�@��@˅@�@�G�@�@�\)@��H@��RA�A	��A(�A�A�A��A�RA!G�A%A*�HA-p�A1�A6ffA8��A>�RAC33AE�AI��AN�RAP��AUAY��A\(�Aa�AeAh��An{Ap��AuAy��A|��A���A��\A�(�A�
=A�Q�A��HA�z�A�ffA���A��HA�z�A�
=A���A��A�A�
=A�G�A�33A��A��A�G�A��\A�p�A��RA�G�A��HA���A�
=A���A��\A��A��RA�Q�A��HA�z�A�ffA���A�=qA�(�AθRA�  A�=qA�(�A�A�Q�A�G�A��
A�A�
=A�G�A�A�z�A�RA��A��A�(�A�{A�\)A�A�33A���A�
=A�  A���A�(�A��A�\)B ��BG�BffB\)B  B��B=qB�RB�B��B	��B
ffB�Bz�B�B�\B�B(�BG�B=qB
=Bz�B�B{B\)BQ�B�B�\B\)BQ�B��B=qB\)B ��B!G�B"�RB#�B$z�B%��B&�HB'�B(��B*=qB+
=B,(�B-��B.�\B/�B1�B1�B3
=B4��B5G�B6�\B7�
B8��B9B;33B<Q�B=G�B>�\B?�
B@��BA�BC\)BD(�BE�BF�\BG\)BHz�BI�BJ�RBK�BM�BN{BN�HBPQ�BQp�BR=qBS\)BT��BUp�BVffBW�
BX��BY��B[33B\  B\��B^ffB_\)B`(�Ba��Bb�\Bc\)Bd��Be��BfffBg�Bhz�BiG�Bj�RBk�BlQ�Bm��Bn�RBo33Bpz�Bq�Br�\Bs�
Bt��Bu��Bw33Bxz�ByG�BzffB{�
B|��B}��B
=B�{B�z�B�
=B���B��B��RB�33B���B�{B��RB�33B��B�=qB���B���B���B�{B�Q�B���B��B��
B�ffB��HB�33B��B�=qB���B�33B��B�  B�ffB��B���B��B�z�B���B�G�B��B�ffB���B�33B�B�(�B�z�B��B���B��B�ffB���B�\)B�B�ffB��HB��B��B�=qB�z�B��HB�p�B��B�(�B��\B��B��B��B�Q�B��HB�\)B���B�(�B��RB�
=B�\)B�  B�z�B���B�\)B�B�{B���B��B��B�(�B��\B��HB�p�B�  B�Q�B��RB�\)B��
B�{B��\B��B��B��B�z�B��RB��B��B�{B�ffB��HB�p�B��B�  B���B�
=B�\)B�B�Q�B��RB�
=B��B�(�B�ffB���B�p�B�B�(�B���B�33B��B�  B�Q�B�
=B��B�B�(�B��RB�G�B���B�  B��\B�
=B�\)B�B�Q�B��HB�33B���B�(�B��RB��B�p�B�(�B�z�B��HBÅB��B�=qB��HB�\)B�B�=qB���B�\)BǮB�=qB���B��BɅB�(�Bʣ�B���B�p�B�{B�ffB���B�p�B��B�Q�BθRB�\)B�B�(�B���B�G�Bљ�B�(�B���B�33BӅB�(�BԸRB�
=BՅB�(�BָRB�
=Bי�B�(�B؏\B�
=Bٙ�B�(�Bڏ\B�
=Bۙ�B�=qB܏\B�
=B�B�=qBޏ\B�
=B߮B�{B�z�B��BᙚB�  B�z�B��B㙚B��B�z�B�33B�B��B��B��B�p�B�  B��B���B�p�B�  B��B���B�\)B�  B��B���B�\)B��B�z�B���B�\)B��B��\B���B�\)B��B�\B�
=B�\)B��
B�ffB�
=B���B��B�ffB�
=B�p�B��B�ffB�
=B�p�B�B�ffB��HB�\)B��B�(�B���B�G�B��B�  B��\B��B�p�B�C (�C ffC ��C C  CG�C�C�C�HC�CffC�\CC��CG�C�\C��C��C(�C�C��C��C(�Cp�C�RC��C(�C\)C�C  C33Cp�C��C�HC33Cp�C��C��C	{C	\)C	��C	��C
  C
=qC
�C
��C
��C�C\)C��C�HC
=C33CffC�C�C�CG�C�C�
C{C33Cp�C�RC  C=qCp�C�RC
=C\)C�\C��C{Cp�C�C�HC(�Cz�C��C
=CG�Cz�C��C�CffC��C��C{CffC�C�C(�Cp�C�RC�C(�Cz�C�RC��C33CffC�RC��C(�CffC�RC�HC{CffC��C�
C  C=qC�\C�C�C=qCffC��C�HC(�C\)C�C��C{CG�Cz�C��C�C33C\)C�C��C 
=C G�C ffC ��C!  C!33C!ffC!��C!��C"�C"Q�C"��C"�C#�C#Q�C#��C#�C$�C$Q�C$��C$�HC%�C%Q�C%��C%�C&
=C&G�C&��C&�C'�C'G�C'��C'��C(  C(Q�C(�\C(�RC)
=C)Q�C)z�C)�RC*
=C*Q�C*z�C*C+{C+=qC+z�C+�
C,
=C,=qC,�\C,�
C-
=C-=qC-��C-��C.  C.G�C.��C.C/  C/Q�C/�\C/C0  C0\)C0��C0��C1(�C1z�C1�RC1��C2G�C2��C2�
C3
=C3\)C3�RC4  C433C4z�C4�
C5{C5G�C5��C5�C6(�C6ffC6�C7
=C7G�C7z�C7�RC8
=C8\)C8�\C8C9�C9ffC9��C9�
C:�C:p�C:C;  C;33C;p�C;C<{C<Q�C<�\C<�
C=33C=�C=�RC>  C>\)C>��C>�HC?�C?ffC?C@{C@G�C@�\C@�HCA33CAffCA��CA��CBQ�CB�\CB�
CC{CCp�CCCD
=CDG�CD�\CD�
CE=qCEz�CE�RCF  CF\)CF��CF�
CG(�CGz�CG�RCG�CH=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                       ?��@�\@E�@�  @�  @�  @�G�A ��A  A   A,(�AAG�A`��A�Q�A��A��A��A��AϮA�\)A�  B (�B�
B�
B  B   B'�
B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�{B�(�B�  B�  B��B�{B�{B�  B�  B�  B�{B�  B��B�  B�(�B�(�B�{B�  B�{B�  B�  B�  B�  B�  B�{B�(�B�(�B�{B�{B�{B�{B�(�C {C{C{C  C
=C

=C��C�C��C  C  C
=C  C{C  C��C��C!��C#�C%�C'�C)��C,  C.
=C0
=C2{C4  C5��C7��C:
=C;��C=��C?�CB  CD  CF
=CH
=CI��CK��CN
=CP  CQ��CT  CV{CX{CY��C\  C]��C_�Cb
=Cd{Cf{Ch{Cj{Cl
=Cn
=Cp  Cr  Ct{Cv
=Cx
=Cy��C|  C}��C�  C�C�  C�C���C�C�
=C�  C���C�  C���C�  C���C���C���C���C�  C�  C�C�  C�  C�C�  C�C�  C�  C�C�C�C�
=C���C�  C�C�C���C���C�C���C���C���C�
=C�
=C���C�\C�C���C�C���C�C�
=C�C�
=C�C���C�  C�
=C�C�  C���C���C�  C�  C���C�  C���C���C���C���C���C���C���C���C�C�C�
=C�  C�\C�C���C���C�
=C�
=C�  C���C���C�  C���C�  C�C���C�  C�  C���C���C��C���C�  C�  C�C���C���C�
=C�C���C�  C�  C�C���C���C���C�C�
=C�  C���C�C�  C���C���C�  C�C���C�  C�C���C���C�  C�  C�  D   D ��D �qD}qD�D��D  D� D�D�DD��D  D�DD� D�qD� D	�D	��D
  D
� D  D� D  D� D  D� D�D� D  D� D�qD}qD�qD� D  D� D�qD}qD�qD}qD  D��D  D� D  D� D  Dz�D�RD}qD�D��D�D��D  D� D  D� D  Dz�D�qD� D   D � D!  D!��D"  D"��D#�D#��D$D$�D%  D%}qD%�qD&}qD'D'� D'�qD(}qD(��D)}qD*�D*��D+  D+}qD,�D,� D,�qD-� D.�D.��D/  D/}qD/�qD0� D1  D1� D1�qD2}qD3  D3��D4  D4� D5  D5}qD6  D6}qD6�qD7��D8  D8}qD8�qD9}qD9��D:}qD;�D;��D;�qD<� D=D=��D=�qD>� D?  D?� D@  D@� DA  DAz�DB  DB��DC�DC� DD  DD��DE�DE��DF  DF�DG�DG� DH  DH� DI�DI�DJ  DJ}qDJ�qDKz�DK�qDL�DMDM��DN�DN}qDN�qDO}qDO��DP� DP�qDQz�DR  DR}qDR��DS}qDS�qDT}qDU  DU��DV�DV� DW�DW��DX  DX� DY�DY��DZ�DZ��D[�D[��D\  D\}qD\��D]}qD^  D^}qD^�qD_� D_��D`z�D`�qDa� Db  Db� Dc�Dc�Dd�Dd��De�De� Df�Df� Df�qDg��Dh�Dh� Di�Di��Di�qDjz�Dj��Dk}qDk�qDl� Dl��Dmz�Dn  Dn��Do  Do}qDp  Dp��Dq�Dq� Dr  Drz�Dr��Ds}qDs�qDt� Du  Du��Dv�Dv}qDw  Dw��Dx  Dx}qDy  Dy� Dy��Dz}qD{  D{}qD|  D|��D}  D}� D~  D~}qD~�qD� D�  D�@ D�� D���D���D�@ D�~�D�� D�  D�@ D�� D�� D���D�@ D��HD���D���D�@ D�� D��HD�  D�@ D��HD�� D�  D�@ D�� D���D��qD�@ D��HD���D�  D�@ D�� D���D�  D�AHD�� D�� D�  D�@ D�~�D��qD���D�AHD�� D��qD��qD�@ D��HD���D���D�AHD��HD��HD�  D�@ D�~�D���D�HD�AHD��HD��HD���D�>�D��HD��HD���D�>�D�� D�� D�HD�AHD�}qD��qD�HD�B�D��HD�D�HD�@ D��HD��HD�HD�B�D��HD���D���D�@ D�~�D�� D���D�@ D��HD��HD��D�B�D��HD���D�  D�@ D�~�D��qD���D�@ D�� D��HD�  D�AHD�~�D���D���D�>�D�~�D��qD���D�AHD�� D��HD�HD�>�D�~�D��HD�  D�>�D�� D���D�  D�AHD�� D��HD��D�AHD�� D���D���D�>�D�~�D���D�  D�@ D��HD��HD��D�@ D�~�D��qD���D�>�D�� D�D�HD�@ D�~�D���D�HD�B�D�~�D��qD�  D�AHD�� D���D���D�@ D��HD��HD���D�>�D�~�D�� D���D�@ D��HD�� D�HD�@ D�� D�� D���D�@ D�� D���D�  D�@ D�� D��HD�  D�@ D��HD��HD�HD�AHD�� D���D��qD�@ D��HD�D�HD�>�D�~�D��HD��D�C�D�� D��qD��qD�>�D�� D��HD�HD�AHD�~�D���D�HD�B�D�� D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�>�D��HD�� D�  D�@ D��HD�D��D�@ D�� D��HD�HD�AHD�� D�� D�  D�>�D�� D�D�  D�=qD�~�D��HD�  D�@ D��HD��HD��D�AHD�� D�� D�  D�AHD�� D�� D�  D�AHD D��HD�  D�@ DÁHD��HD�HD�AHDĀ D�� D��D�B�Dŀ D��HD�  D�@ DƁHD��HD�  D�AHDǁHD�� D���D�>�D�~�D�� D�HD�AHDɁHD��HD�  D�AHDʁHD�� D�  D�@ Dˀ D�� D�  D�>�D̀ D��HD�  D�>�D̀ D��HD�  D�@ D�~�D�� D�  D�@ D�~�D�� D���D�@ D�~�Dо�D�  D�@ Dр D�� D�HD�AHDҁHD��HD���D�=qDӀ D��HD�HD�@ DԀ D�� D�  D�@ DՀ Dվ�D���D�@ Dր D�� D�HD�@ D�~�D׽qD���D�@ D؀ Dؾ�D�  D�@ D�~�D��HD�  D�@ DځHD��HD��D�AHDۀ D۽qD�  D�@ D܁HD��HD�  D�AHD݀ Dݾ�D���D�>�Dހ D��HD�  D�>�D߀ D��HD���D�@ D��HD�� D�  D�@ D�~�D��HD�HD�>�D�}qD⾸D���D�@ D�HD㾸D���D�@ D� D侸D���D�AHD�HD�� D�  D�>�D� D��HD�  D�@ D�HD��HD�  D�@ D�HD�D�HD�>�D�~�D龸D�HD�B�D� D�� D�  D�@ D�HD��HD�HD�>�D�~�D쾸D�  D�>�D�~�D��HD�HD�>�D�~�D�� D�  D�AHD�HD�� D�  D�>�D��HD���D�HD�>�D� D�� D���D�@ D�HD��HD�  D�@ D� D�� D�HD�AHD�HD��HD��D�AHD�~�D��qD���D�@ D�� D���D���D�@ D�� D��qD���D�@ D��HD��HD���D�=qD�}qD�� G�O�?8Q�?k�?�\)?�Q�?��?��@��@��@+�@=p�@J=q@^�R@p��@z�H@�=q@���@�
=@��\@�=q@��@�(�@��@˅@�@�G�@�@�\)@��H@��RA�A	��A(�A�A�A��A�RA!G�A%A*�HA-p�A1�A6ffA8��A>�RAC33AE�AI��AN�RAP��AUAY��A\(�Aa�AeAh��An{Ap��AuAy��A|��A���A��\A�(�A�
=A�Q�A��HA�z�A�ffA���A��HA�z�A�
=A���A��A�A�
=A�G�A�33A��A��A�G�A��\A�p�A��RA�G�A��HA���A�
=A���A��\A��A��RA�Q�A��HA�z�A�ffA���A�=qA�(�AθRA�  A�=qA�(�A�A�Q�A�G�A��
A�A�
=A�G�A�A�z�A�RA��A��A�(�A�{A�\)A�A�33A���A�
=A�  A���A�(�A��A�\)B ��BG�BffB\)B  B��B=qB�RB�B��B	��B
ffB�Bz�B�B�\B�B(�BG�B=qB
=Bz�B�B{B\)BQ�B�B�\B\)BQ�B��B=qB\)B ��B!G�B"�RB#�B$z�B%��B&�HB'�B(��B*=qB+
=B,(�B-��B.�\B/�B1�B1�B3
=B4��B5G�B6�\B7�
B8��B9B;33B<Q�B=G�B>�\B?�
B@��BA�BC\)BD(�BE�BF�\BG\)BHz�BI�BJ�RBK�BM�BN{BN�HBPQ�BQp�BR=qBS\)BT��BUp�BVffBW�
BX��BY��B[33B\  B\��B^ffB_\)B`(�Ba��Bb�\Bc\)Bd��Be��BfffBg�Bhz�BiG�Bj�RBk�BlQ�Bm��Bn�RBo33Bpz�Bq�Br�\Bs�
Bt��Bu��Bw33Bxz�ByG�BzffB{�
B|��B}��B
=B�{B�z�B�
=B���B��B��RB�33B���B�{B��RB�33B��B�=qB���B���B���B�{B�Q�B���B��B��
B�ffB��HB�33B��B�=qB���B�33B��B�  B�ffB��B���B��B�z�B���B�G�B��B�ffB���B�33B�B�(�B�z�B��B���B��B�ffB���B�\)B�B�ffB��HB��B��B�=qB�z�B��HB�p�B��B�(�B��\B��B��B��B�Q�B��HB�\)B���B�(�B��RB�
=B�\)B�  B�z�B���B�\)B�B�{B���B��B��B�(�B��\B��HB�p�B�  B�Q�B��RB�\)B��
B�{B��\B��B��B��B�z�B��RB��B��B�{B�ffB��HB�p�B��B�  B���B�
=B�\)B�B�Q�B��RB�
=B��B�(�B�ffB���B�p�B�B�(�B���B�33B��B�  B�Q�B�
=B��B�B�(�B��RB�G�B���B�  B��\B�
=B�\)B�B�Q�B��HB�33B���B�(�B��RB��B�p�B�(�B�z�B��HBÅB��B�=qB��HB�\)B�B�=qB���B�\)BǮB�=qB���B��BɅB�(�Bʣ�B���B�p�B�{B�ffB���B�p�B��B�Q�BθRB�\)B�B�(�B���B�G�Bљ�B�(�B���B�33BӅB�(�BԸRB�
=BՅB�(�BָRB�
=Bי�B�(�B؏\B�
=Bٙ�B�(�Bڏ\B�
=Bۙ�B�=qB܏\B�
=B�B�=qBޏ\B�
=B߮B�{B�z�B��BᙚB�  B�z�B��B㙚B��B�z�B�33B�B��B��B��B�p�B�  B��B���B�p�B�  B��B���B�\)B�  B��B���B�\)B��B�z�B���B�\)B��B��\B���B�\)B��B�\B�
=B�\)B��
B�ffB�
=B���B��B�ffB�
=B�p�B��B�ffB�
=B�p�B�B�ffB��HB�\)B��B�(�B���B�G�B��B�  B��\B��B�p�B�C (�C ffC ��C C  CG�C�C�C�HC�CffC�\CC��CG�C�\C��C��C(�C�C��C��C(�Cp�C�RC��C(�C\)C�C  C33Cp�C��C�HC33Cp�C��C��C	{C	\)C	��C	��C
  C
=qC
�C
��C
��C�C\)C��C�HC
=C33CffC�C�C�CG�C�C�
C{C33Cp�C�RC  C=qCp�C�RC
=C\)C�\C��C{Cp�C�C�HC(�Cz�C��C
=CG�Cz�C��C�CffC��C��C{CffC�C�C(�Cp�C�RC�C(�Cz�C�RC��C33CffC�RC��C(�CffC�RC�HC{CffC��C�
C  C=qC�\C�C�C=qCffC��C�HC(�C\)C�C��C{CG�Cz�C��C�C33C\)C�C��C 
=C G�C ffC ��C!  C!33C!ffC!��C!��C"�C"Q�C"��C"�C#�C#Q�C#��C#�C$�C$Q�C$��C$�HC%�C%Q�C%��C%�C&
=C&G�C&��C&�C'�C'G�C'��C'��C(  C(Q�C(�\C(�RC)
=C)Q�C)z�C)�RC*
=C*Q�C*z�C*C+{C+=qC+z�C+�
C,
=C,=qC,�\C,�
C-
=C-=qC-��C-��C.  C.G�C.��C.C/  C/Q�C/�\C/C0  C0\)C0��C0��C1(�C1z�C1�RC1��C2G�C2��C2�
C3
=C3\)C3�RC4  C433C4z�C4�
C5{C5G�C5��C5�C6(�C6ffC6�C7
=C7G�C7z�C7�RC8
=C8\)C8�\C8C9�C9ffC9��C9�
C:�C:p�C:C;  C;33C;p�C;C<{C<Q�C<�\C<�
C=33C=�C=�RC>  C>\)C>��C>�HC?�C?ffC?C@{C@G�C@�\C@�HCA33CAffCA��CA��CBQ�CB�\CB�
CC{CCp�CCCD
=CDG�CD�\CD�
CE=qCEz�CE�RCF  CF\)CF��CF�
CG(�CGz�CG�RCG�CH=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                       @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�G�A�E�A�E�A�G�A�C�A�E�A�G�A�I�A�G�A�E�A�E�A�E�A�M�A�O�A�VA�XA�S�A�S�A�VA�XA�ZA�ZA�ZA�\)A�ZA�\)A�^5A�`BA�`BA�`BA�bNA�bNA�dZA�`BA�bNA�bNA�jA�dZA�l�A�x�Aմ9Aպ^A��A��A�1'A�hsA֬A��A�1A�bA���A֬A֑hAցA�Q�A� �A�A���A���A���A��A��A��A��;A��
A�ȴAհ!AՑhA��A�ȴA�M�A���Aơ�A���A�33A���A�9XA�I�A�jA���A��RA���A��DA���A��mA�E�A���A��A�ZA��/A��A�E�A��A�ȴA��/A��RA��A��A�
=A�  A�ȴA��A�l�A��RA�t�A�VA�n�A�(�A�O�A��;A��TA�ĜA��;A�l�A��;A�  A�`BA���A�9XA��A�jA���A��HA��A��/A��A�A��7A���A��-A��yA��A��7A}dZA{O�Av=qAuC�As��ArffAp~�An �AlAi�Ag��Ag&�Ae�Ab�A`�!A_A^��A^(�A]�FA\�A[l�AZ��AV�HAT�!AS�
ASp�AR��AQ�AO�mAMx�AL�DAJ-AH~�AF��AE��ACVA@��A>�+A<�A9ƨA8��A7G�A6A49XA2��A1dZA/�A-?}A+C�A*I�A)�PA(bNA&��A%��A%\)A$9XA#t�A ��AA�A��A��AS�A��A�!AA��A?}A5?A|�AbA+A�\A�A�AS�A�A��A�jAM�A|�AK�A��Av�A�;AoAffA��AC�A
�A
��A
��A
jA�`A��A�A�^Al�AȴA5?A�A;dA(�A�wA�A�PA�A��A�7A ��A n�A bNA 5?@���@��@�b@���@�v�@�J@�`B@���@�^5@��@��/@�@��@�&�@�Q�@@���@���@���@�j@�u@�@�(�@�P@�K�@��@��@�j@��;@��y@�x�@�z�@�;d@���@��@۾w@���@��@�Ĝ@ؓu@�Q�@���@�l�@��y@ָR@֏\@�v�@�M�@�{@���@���@Դ9@�V@���@���@��@�5?@���@�O�@Ѓ@θR@�x�@�%@�/@�%@���@�%@��@̓u@�Z@�b@˥�@�"�@��y@ʗ�@��#@�hs@�/@��@�%@���@�(�@���@�5?@���@ũ�@ř�@�hs@��@��@ģ�@�bN@���@�;d@�
=@�@�M�@��@��^@�X@���@�r�@��;@�l�@�ȴ@�ff@�ff@�n�@�n�@�v�@��+@�~�@�n�@�M�@��@��7@���@�z�@�9X@���@��y@���@�ȴ@���@��!@���@�-@�7L@��D@�j@�j@�t�@��@���@���@���@���@�M�@��@�hs@�/@�/@�&�@�bN@�l�@���@�M�@��@�p�@�?}@��@��D@�A�@�(�@��@��@�dZ@�K�@���@��!@�=q@��-@���@��7@�/@���@�j@��;@��@�K�@���@��+@�V@��@��7@���@���@��@��@�33@�"�@���@�^5@�^5@�V@��@���@�?}@��@���@��/@��@�I�@���@�\)@�"�@�o@��+@��@��h@�/@��/@���@�b@��m@���@��
@�l�@��@���@��\@�v�@���@�%@�I�@�  @���@��@�1@��
@�ƨ@��w@��@�\)@�"�@���@��H@��@���@���@��@�p�@�`B@�O�@�V@���@��D@�1'@��;@��F@�t�@�S�@��+@���@�hs@��@��j@�r�@�A�@� �@��@��@�dZ@��@��y@��R@�v�@�=q@���@��h@�O�@�&�@�V@��`@��9@��
@�|�@��@���@��@���@�V@��T@�@��-@��h@���@�hs@��@�%@��j@�j@�Q�@�A�@�(�@� �@� �@��@���@���@�t�@�dZ@�o@�
=@���@�v�@�$�@�@���@�@�@�`B@��@�Z@�1@��m@���@���@�\)@�33@��@��R@��!@���@�^5@�@���@��7@�?}@��@�Ĝ@���@���@��u@�j@�1'@� �@� �@� �@� �@��@���@��F@���@�t�@��@��y@���@�~�@�M�@��@��#@��h@�7L@�V@���@��`@�Ĝ@���@���@�j@� �@��@��@|�@~�y@~$�@}�h@}�@}?}@|Z@|I�@{�F@z��@z�\@z-@y�@y�7@y&�@xbN@xA�@w��@w�@vv�@v{@u@up�@t��@t�j@t(�@sƨ@sC�@r�!@rM�@q��@q�7@qhs@q7L@p �@nȴ@nff@n5?@m�@m��@m`B@m/@l�@l�D@k�m@kdZ@k33@j�H@jn�@i��@hbN@g�@g|�@g\)@g
=@f��@fff@e�@e@e�@e�h@e�@e?}@eV@c�m@co@b�H@bM�@a�#@a��@a�7@a�7@a�@`Ĝ@`�9@`��@`�@`  @^ȴ@^�+@^E�@^@]�h@]/@]V@\�@[��@Z�!@Zn�@Z=q@Z-@Y��@Y��@Y��@Y��@Y�7@YG�@Y%@XQ�@Xb@X  @W�P@WK�@V�y@V�+@Vv�@Vff@V@U�h@U�@U�@T�j@T1@SS�@S"�@R�H@R�\@Rn�@R=q@Q��@Q�@P��@P��@P�9@P�@Pb@O\)@N�y@Nv�@M�T@MO�@L��@Lz�@L�@K��@K"�@J�\@JM�@I�#@IG�@H�`@H��@H1'@G�@G�;@G�@G�@F�y@F�+@E�-@E?}@D�@D�@C�
@C��@C�@CdZ@CS�@C"�@B^5@BJ@A��@A��@Ax�@A%@?�@>�@>�@>ff@=@=@=��@=?}@<�@<(�@;�
@;dZ@;33@;33@:�!@:�\@:-@9�#@9x�@9G�@9�@8r�@8b@7�;@7l�@7K�@7+@6ȴ@6V@6V@65?@6{@5p�@4��@4j@49X@4�@41@3��@3�
@3�F@3�F@3��@3��@3S�@3@2�!@2=q@1��@1�@1G�@0�`@0��@0r�@0A�@0 �@0  @/��@.�R@.��@.��@.��@.�+@.�+@.v�@.v�@.ff@-@-��@-`B@,�@,�@,z�@+��@+��@+S�@+C�@+@*~�@*-@)�@)�^@)��@)�7@)x�@)G�@)�@(�9@(A�@(  @'�;@'l�@'�@&��@&E�@%�@%��@%`B@$�@$1@#ƨ@#ƨ@#�F@#��@#C�@"�H@"n�@"M�@"=q@"-@"-@"�@"�@"J@!��@!��@!��@!x�@!hs@!7L@ ��@ �@�w@�P@|�@|�@l�@\)@;d@�@��@�+@v�@v�@5?@�@��@O�@�@��@Z@(�@�@��@�
@��@dZ@33@o@@�!@=q@��@�@��@��@��@�7@7L@�@��@��@Ĝ@�u@�u@�@bN@A�@A�@1'@ �@�@�@�;@��@��@l�@K�@
=@��@V@5?@$�@$�@{@@�T@�h@p�@`B@/@��@�/@�j@z�@I�@�@1@�
@�F@��@��@��@��@�@dZ@S�@o@@�@��@��@M�@�@��@��@�7@&�@��@�`@�`@��@�@1'@  @�@�;@��@�P@|�@\)@
=@�@�R@�R@��@ff@E�@@��@��A�E�A�G�A�E�A�G�A�C�A�E�A�K�A�E�A�C�A�G�A�C�A�I�A�I�A�A�A�G�A�G�A�A�A�E�A�E�A�C�A�I�A�G�A�C�A�I�A�I�A�E�A�E�A�K�A�G�A�I�A�G�A�G�A�I�A�E�A�E�A�G�A�A�A�E�A�G�A�C�A�E�A�G�A�A�A�G�A�G�A�A�A�E�A�G�A�C�A�K�A�K�A�M�A�S�A�S�A�M�A�Q�A�I�A�O�A�M�A�K�A�Q�A�S�A�Q�A�XA�Q�A�XA�S�A�Q�A�ZA�VA�VA�ZA�VA�ZA�\)A�S�A�XA�ZA�VA�XA�S�A�S�A�ZA�Q�A�VA�S�A�S�A�VA�S�A�Q�A�VA�Q�A�Q�A�VA�S�A�S�A�XA�Q�A�VA�XA�S�A�ZA�VA�XA�ZA�VA�ZA�XA�VA�XA�ZA�VA�ZA�ZA�XA�\)A�^5A�XA�\)A�ZA�XA�\)A�VA�XA�\)A�XA�ZA�\)A�ZA�`BA�\)A�S�A�ZA�ZA�VA�XA�^5A�\)A�^5A�bNA�^5A�ZA�XA�VA�VA�`BA�`BA�\)A�dZA�^5A�S�A�^5A�XA�VA�\)A�XA�ZA�\)A�ZA�ZA�\)A�ZA�^5A�`BA�\)A�`BA�bNA�\)A�^5A�bNA�\)A�^5A�`BA�^5A�^5A�dZA�`BA�`BA�bNA�^5A�`BA�dZA�^5A�^5A�bNA�bNA�^5A�`BA�`BA�\)A�`BA�bNA�^5A�^5A�dZA�`BA�`BA�dZA�bNA�bNA�ffA�bNA�`BA�dZA�ffA�`BA�dZA�ffA�bNA�bNA�ffA�bNA�bNA�ffA�bNA�bNA�dZA�bNA�\)A�bNA�`BA�`BA�bNA�`BA�^5A�ffA�dZA�`BA�ffA�bNA�`BA�dZA�dZA�`BA�bNA�dZA�`BA�dZA�ffA�bNA�l�A�r�A�n�A�l�A�n�A�dZA�bNA�ffA�ffA�bNA�dZA�dZA�bNA�ffA�dZA�`BA�ffA�x�A�r�A�|�A�t�A�~�A�p�A�n�A�x�A�|�A�|�AՁAՃAծAնFAմ9Aպ^Aղ-Aղ-AնFAղ-AոRA���AնFAմ9Aղ-Aմ9AոRAմ9A���A���A�ƨA���A���A�  A�
=A�VA�oA��A��A�$�A�"�A�"�A�$�A��A�JA�+A��A�VA��A�1'A�/A�9XA�?}A�K�A�C�A�XA�I�A�bNA�^5A�dZA�jA�v�A�r�A�n�A�|�A֍PA֏\A֗�A֧�Aֲ-A�AָRA���A��yA��A��A��A���A��A���A���A�A�A�A�1A�%A�A�
=A�1A�1A�VA�VA�JA�bA�oA�VA�VA�{A�{A�oA�oA�oA�VA�1A�
=A�1A�A��A��HA���A�ĜA���Aֺ^Aֲ-A֧�A֡�A֡�A֝�A֕�A֏\A֑hA֕�A֑hA֏\A֑hA֓uA֏\A֍PA֍PA֏\A։7AօAօA�|�A�t�A�n�A�n�A�hsA�bNA�`BA�VA�K�A�E�A�=qA�33A�1'A�5?A�1'A�"�A��A��A�oA�JA�VA�
=A�A�A�A�A�  A�A�A�  A���A�A�  A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A��A���A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��mA��`A��HA��/A��;A��HA��/A��/A��/A��HA��/A��#A��A��
A���A���A���A���A���A�ȴA���A���A�ȴAռjAպ^AվwAպ^Aղ-AծAծAլAթ�Aա�A՝�A՝�A՗�AՏ\AՍPAՍPAՇ+A�z�A�t�A�XA�-A��A��/A���Aԩ�Aԗ�A�bNA�+A��A��AӴ9Aӝ�Aӝ�Aӝ�Aӗ�AӁA���A�(�A�ƨA�E�A��A���A��A�A�A���AͶFA̶FA˝�AɾwA�XA�^5A�n�AɑhA�x�A�bNA�v�A�33AŇ+A� �A��AĲ-A�hsA��A���A���Aé�AÕ�A�|�A�v�A�bNA�\)A�S�A�7LA�1'A� �A� �A��A�VA�  A��A��/A��
A���A�ȴA¶FA´9A¶FA��A�ƨA�~�A��A�bNA��A���A�v�A��#A���A��\A�|�A��
A���A�hsA���A�A��A�x�A�hsA�^5A�K�A�;dA�&�A�{A�A���A��A��A��`A��/A���A���A�A��jA��9A��A���A���A���A���A���A���A���A���A���A���A��PA��\A��hA��PA��7A��PA��PA��A�x�A�ZA�C�A��A���A���A�hsA�VA�9XA�"�A�{A���A��yA��HA��
A�ƨA��FA��-A���A���A��7A�|�A�`BA�-A�A���A��A�C�A��A��
A���A�hsA�I�A��A��A���A��RA��A�n�A�`BA�^5A�^5A�`BA�^5A�ZA�\)A�^5A�XA�XA�XA�ZA�VA�K�A�33A��A��`A��jA���A���A��A�A��+A�5?A�ĜA�n�A�G�A�oA��/A��^A��!A���A�n�A�&�A��HA�I�A��A�&�A�5?A�9XA�9XA�(�A��TA�E�A��A��A���A��A���A��A��DA�G�A�(�A�A��/A���A�ȴA�ƨA�A��jA��jA��jA��jA��9A��!A��9A��-A��!A��9A��-A��-A��A���A���A���A��hA��+A��A��A�x�A�S�A�ZA�VA�7LA�(�A��A��A��FA�v�A�^5A�5?A�JA��`A��
A��wA��-A���A�jA�G�A���A�Q�A�"�A�l�A�bA��A���A�l�A�-A��A�A���A��!A��A�r�A�n�A�I�A��mA�ffA���A��A���A���A��hA��DA��A��A�x�A�p�A�p�A�jA�dZA�jA�dZA�`BA�S�A�VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                       A�G�A�E�A�E�A�G�A�C�A�E�A�G�A�I�A�G�A�E�A�E�A�E�A�M�A�O�A�VA�XA�S�A�S�A�VA�XA�ZA�ZA�ZA�\)A�ZA�\)A�^5A�`BA�`BA�`BA�bNA�bNA�dZA�`BA�bNA�bNA�jA�dZA�l�A�x�Aմ9Aպ^A��A��A�1'A�hsA֬A��A�1A�bA���A֬A֑hAցA�Q�A� �A�A���A���A���A��A��A��A��;A��
A�ȴAհ!AՑhA��A�ȴA�M�A���Aơ�A���A�33A���A�9XA�I�A�jA���A��RA���A��DA���A��mA�E�A���A��A�ZA��/A��A�E�A��A�ȴA��/A��RA��A��A�
=A�  A�ȴA��A�l�A��RA�t�A�VA�n�A�(�A�O�A��;A��TA�ĜA��;A�l�A��;A�  A�`BA���A�9XA��A�jA���A��HA��A��/A��A�A��7A���A��-A��yA��A��7A}dZA{O�Av=qAuC�As��ArffAp~�An �AlAi�Ag��Ag&�Ae�Ab�A`�!A_A^��A^(�A]�FA\�A[l�AZ��AV�HAT�!AS�
ASp�AR��AQ�AO�mAMx�AL�DAJ-AH~�AF��AE��ACVA@��A>�+A<�A9ƨA8��A7G�A6A49XA2��A1dZA/�A-?}A+C�A*I�A)�PA(bNA&��A%��A%\)A$9XA#t�A ��AA�A��A��AS�A��A�!AA��A?}A5?A|�AbA+A�\A�A�AS�A�A��A�jAM�A|�AK�A��Av�A�;AoAffA��AC�A
�A
��A
��A
jA�`A��A�A�^Al�AȴA5?A�A;dA(�A�wA�A�PA�A��A�7A ��A n�A bNA 5?@���@��@�b@���@�v�@�J@�`B@���@�^5@��@��/@�@��@�&�@�Q�@@���@���@���@�j@�u@�@�(�@�P@�K�@��@��@�j@��;@��y@�x�@�z�@�;d@���@��@۾w@���@��@�Ĝ@ؓu@�Q�@���@�l�@��y@ָR@֏\@�v�@�M�@�{@���@���@Դ9@�V@���@���@��@�5?@���@�O�@Ѓ@θR@�x�@�%@�/@�%@���@�%@��@̓u@�Z@�b@˥�@�"�@��y@ʗ�@��#@�hs@�/@��@�%@���@�(�@���@�5?@���@ũ�@ř�@�hs@��@��@ģ�@�bN@���@�;d@�
=@�@�M�@��@��^@�X@���@�r�@��;@�l�@�ȴ@�ff@�ff@�n�@�n�@�v�@��+@�~�@�n�@�M�@��@��7@���@�z�@�9X@���@��y@���@�ȴ@���@��!@���@�-@�7L@��D@�j@�j@�t�@��@���@���@���@���@�M�@��@�hs@�/@�/@�&�@�bN@�l�@���@�M�@��@�p�@�?}@��@��D@�A�@�(�@��@��@�dZ@�K�@���@��!@�=q@��-@���@��7@�/@���@�j@��;@��@�K�@���@��+@�V@��@��7@���@���@��@��@�33@�"�@���@�^5@�^5@�V@��@���@�?}@��@���@��/@��@�I�@���@�\)@�"�@�o@��+@��@��h@�/@��/@���@�b@��m@���@��
@�l�@��@���@��\@�v�@���@�%@�I�@�  @���@��@�1@��
@�ƨ@��w@��@�\)@�"�@���@��H@��@���@���@��@�p�@�`B@�O�@�V@���@��D@�1'@��;@��F@�t�@�S�@��+@���@�hs@��@��j@�r�@�A�@� �@��@��@�dZ@��@��y@��R@�v�@�=q@���@��h@�O�@�&�@�V@��`@��9@��
@�|�@��@���@��@���@�V@��T@�@��-@��h@���@�hs@��@�%@��j@�j@�Q�@�A�@�(�@� �@� �@��@���@���@�t�@�dZ@�o@�
=@���@�v�@�$�@�@���@�@�@�`B@��@�Z@�1@��m@���@���@�\)@�33@��@��R@��!@���@�^5@�@���@��7@�?}@��@�Ĝ@���@���@��u@�j@�1'@� �@� �@� �@� �@��@���@��F@���@�t�@��@��y@���@�~�@�M�@��@��#@��h@�7L@�V@���@��`@�Ĝ@���@���@�j@� �@��@��@|�@~�y@~$�@}�h@}�@}?}@|Z@|I�@{�F@z��@z�\@z-@y�@y�7@y&�@xbN@xA�@w��@w�@vv�@v{@u@up�@t��@t�j@t(�@sƨ@sC�@r�!@rM�@q��@q�7@qhs@q7L@p �@nȴ@nff@n5?@m�@m��@m`B@m/@l�@l�D@k�m@kdZ@k33@j�H@jn�@i��@hbN@g�@g|�@g\)@g
=@f��@fff@e�@e@e�@e�h@e�@e?}@eV@c�m@co@b�H@bM�@a�#@a��@a�7@a�7@a�@`Ĝ@`�9@`��@`�@`  @^ȴ@^�+@^E�@^@]�h@]/@]V@\�@[��@Z�!@Zn�@Z=q@Z-@Y��@Y��@Y��@Y��@Y�7@YG�@Y%@XQ�@Xb@X  @W�P@WK�@V�y@V�+@Vv�@Vff@V@U�h@U�@U�@T�j@T1@SS�@S"�@R�H@R�\@Rn�@R=q@Q��@Q�@P��@P��@P�9@P�@Pb@O\)@N�y@Nv�@M�T@MO�@L��@Lz�@L�@K��@K"�@J�\@JM�@I�#@IG�@H�`@H��@H1'@G�@G�;@G�@G�@F�y@F�+@E�-@E?}@D�@D�@C�
@C��@C�@CdZ@CS�@C"�@B^5@BJ@A��@A��@Ax�@A%@?�@>�@>�@>ff@=@=@=��@=?}@<�@<(�@;�
@;dZ@;33@;33@:�!@:�\@:-@9�#@9x�@9G�@9�@8r�@8b@7�;@7l�@7K�@7+@6ȴ@6V@6V@65?@6{@5p�@4��@4j@49X@4�@41@3��@3�
@3�F@3�F@3��@3��@3S�@3@2�!@2=q@1��@1�@1G�@0�`@0��@0r�@0A�@0 �@0  @/��@.�R@.��@.��@.��@.�+@.�+@.v�@.v�@.ff@-@-��@-`B@,�@,�@,z�@+��@+��@+S�@+C�@+@*~�@*-@)�@)�^@)��@)�7@)x�@)G�@)�@(�9@(A�@(  @'�;@'l�@'�@&��@&E�@%�@%��@%`B@$�@$1@#ƨ@#ƨ@#�F@#��@#C�@"�H@"n�@"M�@"=q@"-@"-@"�@"�@"J@!��@!��@!��@!x�@!hs@!7L@ ��@ �@�w@�P@|�@|�@l�@\)@;d@�@��@�+@v�@v�@5?@�@��@O�@�@��@Z@(�@�@��@�
@��@dZ@33@o@@�!@=q@��@�@��@��@��@�7@7L@�@��@��@Ĝ@�u@�u@�@bN@A�@A�@1'@ �@�@�@�;@��@��@l�@K�@
=@��@V@5?@$�@$�@{@@�T@�h@p�@`B@/@��@�/@�j@z�@I�@�@1@�
@�F@��@��@��@��@�@dZ@S�@o@@�@��@��@M�@�@��@��@�7@&�@��@�`@�`@��@�@1'@  @�@�;@��@�P@|�@\)@
=@�@�R@�R@��@ff@E�@@��G�O�A�E�A�G�A�E�A�G�A�C�A�E�A�K�A�E�A�C�A�G�A�C�A�I�A�I�A�A�A�G�A�G�A�A�A�E�A�E�A�C�A�I�A�G�A�C�A�I�A�I�A�E�A�E�A�K�A�G�A�I�A�G�A�G�A�I�A�E�A�E�A�G�A�A�A�E�A�G�A�C�A�E�A�G�A�A�A�G�A�G�A�A�A�E�A�G�A�C�A�K�A�K�A�M�A�S�A�S�A�M�A�Q�A�I�A�O�A�M�A�K�A�Q�A�S�A�Q�A�XA�Q�A�XA�S�A�Q�A�ZA�VA�VA�ZA�VA�ZA�\)A�S�A�XA�ZA�VA�XA�S�A�S�A�ZA�Q�A�VA�S�A�S�A�VA�S�A�Q�A�VA�Q�A�Q�A�VA�S�A�S�A�XA�Q�A�VA�XA�S�A�ZA�VA�XA�ZA�VA�ZA�XA�VA�XA�ZA�VA�ZA�ZA�XA�\)A�^5A�XA�\)A�ZA�XA�\)A�VA�XA�\)A�XA�ZA�\)A�ZA�`BA�\)A�S�A�ZA�ZA�VA�XA�^5A�\)A�^5A�bNA�^5A�ZA�XA�VA�VA�`BA�`BA�\)A�dZA�^5A�S�A�^5A�XA�VA�\)A�XA�ZA�\)A�ZA�ZA�\)A�ZA�^5A�`BA�\)A�`BA�bNA�\)A�^5A�bNA�\)A�^5A�`BA�^5A�^5A�dZA�`BA�`BA�bNA�^5A�`BA�dZA�^5A�^5A�bNA�bNA�^5A�`BA�`BA�\)A�`BA�bNA�^5A�^5A�dZA�`BA�`BA�dZA�bNA�bNA�ffA�bNA�`BA�dZA�ffA�`BA�dZA�ffA�bNA�bNA�ffA�bNA�bNA�ffA�bNA�bNA�dZA�bNA�\)A�bNA�`BA�`BA�bNA�`BA�^5A�ffA�dZA�`BA�ffA�bNA�`BA�dZA�dZA�`BA�bNA�dZA�`BA�dZA�ffA�bNA�l�A�r�A�n�A�l�A�n�A�dZA�bNA�ffA�ffA�bNA�dZA�dZA�bNA�ffA�dZA�`BA�ffA�x�A�r�A�|�A�t�A�~�A�p�A�n�A�x�A�|�A�|�AՁAՃAծAնFAմ9Aպ^Aղ-Aղ-AնFAղ-AոRA���AնFAմ9Aղ-Aմ9AոRAմ9A���A���A�ƨA���A���A�  A�
=A�VA�oA��A��A�$�A�"�A�"�A�$�A��A�JA�+A��A�VA��A�1'A�/A�9XA�?}A�K�A�C�A�XA�I�A�bNA�^5A�dZA�jA�v�A�r�A�n�A�|�A֍PA֏\A֗�A֧�Aֲ-A�AָRA���A��yA��A��A��A���A��A���A���A�A�A�A�1A�%A�A�
=A�1A�1A�VA�VA�JA�bA�oA�VA�VA�{A�{A�oA�oA�oA�VA�1A�
=A�1A�A��A��HA���A�ĜA���Aֺ^Aֲ-A֧�A֡�A֡�A֝�A֕�A֏\A֑hA֕�A֑hA֏\A֑hA֓uA֏\A֍PA֍PA֏\A։7AօAօA�|�A�t�A�n�A�n�A�hsA�bNA�`BA�VA�K�A�E�A�=qA�33A�1'A�5?A�1'A�"�A��A��A�oA�JA�VA�
=A�A�A�A�A�  A�A�A�  A���A�A�  A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A��A���A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��mA��`A��HA��/A��;A��HA��/A��/A��/A��HA��/A��#A��A��
A���A���A���A���A���A�ȴA���A���A�ȴAռjAպ^AվwAպ^Aղ-AծAծAլAթ�Aա�A՝�A՝�A՗�AՏ\AՍPAՍPAՇ+A�z�A�t�A�XA�-A��A��/A���Aԩ�Aԗ�A�bNA�+A��A��AӴ9Aӝ�Aӝ�Aӝ�Aӗ�AӁA���A�(�A�ƨA�E�A��A���A��A�A�A���AͶFA̶FA˝�AɾwA�XA�^5A�n�AɑhA�x�A�bNA�v�A�33AŇ+A� �A��AĲ-A�hsA��A���A���Aé�AÕ�A�|�A�v�A�bNA�\)A�S�A�7LA�1'A� �A� �A��A�VA�  A��A��/A��
A���A�ȴA¶FA´9A¶FA��A�ƨA�~�A��A�bNA��A���A�v�A��#A���A��\A�|�A��
A���A�hsA���A�A��A�x�A�hsA�^5A�K�A�;dA�&�A�{A�A���A��A��A��`A��/A���A���A�A��jA��9A��A���A���A���A���A���A���A���A���A���A���A��PA��\A��hA��PA��7A��PA��PA��A�x�A�ZA�C�A��A���A���A�hsA�VA�9XA�"�A�{A���A��yA��HA��
A�ƨA��FA��-A���A���A��7A�|�A�`BA�-A�A���A��A�C�A��A��
A���A�hsA�I�A��A��A���A��RA��A�n�A�`BA�^5A�^5A�`BA�^5A�ZA�\)A�^5A�XA�XA�XA�ZA�VA�K�A�33A��A��`A��jA���A���A��A�A��+A�5?A�ĜA�n�A�G�A�oA��/A��^A��!A���A�n�A�&�A��HA�I�A��A�&�A�5?A�9XA�9XA�(�A��TA�E�A��A��A���A��A���A��A��DA�G�A�(�A�A��/A���A�ȴA�ƨA�A��jA��jA��jA��jA��9A��!A��9A��-A��!A��9A��-A��-A��A���A���A���A��hA��+A��A��A�x�A�S�A�ZA�VA�7LA�(�A��A��A��FA�v�A�^5A�5?A�JA��`A��
A��wA��-A���A�jA�G�A���A�Q�A�"�A�l�A�bA��A���A�l�A�-A��A�A���A��!A��A�r�A�n�A�I�A��mA�ffA���A��A���A���A��hA��DA��A��A�x�A�p�A�p�A�jA�dZA�jA�dZA�`BA�S�A�VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BLdBMBL�BM6BM6BL�BLdBL�BL�BL�BLdBLdBLdBL�BMBM6BL�BK�BK�BLdBL�BL�BL0BLdBLdBK�BMjBN<BN�BM�BN<BM�BM�BM�BM�BM�BOBBM�BN�BT�Bg�Bk�B}VB�.B��B�UB͟B�B�fBMB(�BHBL0BNpBT�B]�BaHBa�Bb�BcTBd&Bf2Bh�Bm)Bm�BlWBjKBh
Bg�BP�B,B�B��B��B�B�bB��B��B��B��B��B�wB��B��B��B˒B�B�]B�B�|B,�BJ�Bz�B�SB��B��B��B��B�B�B��B�nB��B��B��B�AB�BsBiBR BHKB?}B)_B�B
�B�DB�B��B�B��B��B�B~�BzxBm�BWsB49B(�B�B�B
��B
�sB
�&B
�B
��B
}VB
rB
f�B
XEB
RTB
E�B
=B
&�B
 �B
�B
B
B	��B	�oB	�KB	��B	�&B	�pB	�[B	��B	��B	�-B	��B	��B	�B	��B	�YB	�xB	��B	|B	p�B	kB	dZB	]dB	WsB	OBB	J#B	=�B	7LB	6B	/�B	+kB	#nB	%B	eB	1B	FB	bB	�B	�B	
�B	�B	�B	�B	{B	B	�B	uB	MB	�B	�B	�B	B	�B	�B	�B	�B	�B	1B	B	DB	.B	hB	�B	@B	�B	�B	@B	hB	�B	~B	#nB	)_B	*�B	+�B	,qB	,B	3�B	2�B	2-B	2-B	;�B	9�B	>B	=B	>BB	A�B	A B	@�B	F?B	E�B	F�B	HB	K)B	O�B	V�B	YB	\]B	[�B	Z�B	[#B	b�B	\�B	^jB	[�B	ZQB	[WB	[�B	Z�B	YKB	W�B	[�B	Y�B	Y�B	c�B	c B	a�B	_�B	_pB	^5B	^�B	bNB	a�B	b�B	e�B	g8B	jB	h>B	g�B	l�B	n/B	qAB	r�B	s�B	r|B	v�B	y�B	~�B	�AB	�{B	��B	�MB	�GB	�GB	��B	�B	��B	��B	��B	��B	�+B	�PB	�4B	��B	�1B	��B	�B	��B	��B	�!B	�B	��B	��B	��B	�XB	�B	�IB	�[B	�?B	��B	�*B	�}B	B	�B	ǮB	�KB	��B	�NB	�aB	�?B	רB	�?B	רB	�?B	ݘB	�BB	�|B	�B	�B	��B	�8B	�DB	�B	�)B	�B	��B	�B	�MB	�B	��B	��B	�2B	�8B	�rB	�B	�B	�B	��B	��B	��B
B
AB
GB
GB
{B
�B
�B
�B
�B
�B
	B

	B
	�B

	B

=B

�B

�B
�B
�B
�B
�B
 B
B
�B
$B
�B
�B
7B
xB
B
�B
�B
!-B
!-B
$@B
$@B
"�B
#:B
#B
#:B
#:B
#:B
#�B
#�B
#�B
$�B
%FB
$�B
$tB
%�B
%�B
&LB
&�B
*�B
,qB
,qB
,�B
-wB
,�B
.}B
.�B
/�B
0!B
0UB
1'B
1�B
1�B
1�B
2�B
6B
6B
5tB
6�B
7LB
7LB
7LB
8�B
9�B
;0B
;0B
;0B
;0B
;�B
<6B
=<B
=�B
=�B
=<B
>�B
>�B
>BB
>�B
?HB
?}B
AUB
AUB
B�B
D3B
D3B
D3B
D3B
C�B
B[B
B[B
@�B
>BB
A�B
DgB
FB
HKB
HKB
HKB
IB
J#B
K�B
LdB
K�B
K�B
LdB
M�B
M�B
MjB
MjB
MjB
MB
MjB
M6B
M6B
NB
NpB
M�B
NpB
N�B
N�B
N<B
NB
NpB
N�B
OB
N�B
O�B
PHB
QNB
Q�B
Q�B
R B
R B
R�B
R�B
S&B
T,B
TaB
T�B
T�B
U2B
VB
W�B
WsB
XyB
XB
XyB
YKB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
ZQB
Z�B
[WB
[�B
[�B
[�B
[�B
\]B
\�B
\]B
\�B
\�B
\)B
\)B
]/B
^B
_;B
^�B
^�B
^�B
_pB
_pB
`B
`BB
aB
aHB
aHB
a|B
a�B
bB
bNB
a�B
bB
bB
a�B
a�B
bNB
c�B
dZB
e,B
e�B
f�B
f�B
gB
g8B
g8B
g�B
g�B
g�B
gmB
gmB
g8B
gB
gmB
g�B
g�B
h>B
h>B
h>B
h�B
hsB
h�B
h�B
h�B
iyB
i�B
i�B
i�B
i�B
i�B
jB
i�B
jB
jB
kB
j�B
j�B
l"B
l�B
l�B
l�B
l�B
l�B
l�B
m]B
m)B
m�B
m�B
m�B
n/B
m�B
o B
oiB
o�B
o�B
o5B
oiB
oiB
o�B
o�B
pB
p�B
p�B
p�B
qvB
qAB
q�B
q�B
rB
s�B
uZB
t�B
tB
s�B
tB
t�B
t�B
tTB
t�B
t�B
t�B
u%B
u%B
u�B
u�B
v�B
w�B
w�B
wfB
wfB
w�B
x�B
x�B
x�B
y	B
yrB
y�B
zB
zB
zDB
|�B
|�B
|�B
}"B
}VB
}�B
}�B
~(B
~�B
~�B
~�B
~�B
~�B
�B
�iB
�4B
��B
��B
��B
��B
��B
��B
�{B
��B
��B
�AB
�B
�AB
��B
��B
��B
�;B
��B
��B
�uB
�AB
�B
�B
�B
�{B
��B
��B
�{B
�MB
�B
�MB
�MB
��B
��B
��B
��B
�%B
�%B
�%B
�YB
�+B
��B
��B
��B
��B
�1B
��B
�7B
�7B
��B
��B
�rB
��B
��B
�DB
�xB
��B
�~B
�JB
��B
��B
�PB
��B
�"B
�"B
�VB
�VB
��B
��B
��B
��B
��B
��B
�4B
�4B
�B
��B
��B
��B
�B
�{B
��B
��B
�B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�B
��B
��B
�SB
�$B
��B
��B
��B
�YB
�+B
�+B
�1B
��B
��B
�B
�kB
��B
�qB
�=B
�	B
�	B
��B
�qB
��B
�qB
��B
�~B
�~B
�~B
�~B
�~B
�~B
��B
�~B
�~B
�~B
�IB
�~B
�~B
��B
�B
�B
��B
�!B
��B
�!B
�VB
��B
��B
��B
�\B
�-B
�-B
�-B
�-B
�-B
��B
�-B
��B
�-B
��B
��B
��B
�4B
�4B
�hB
��B
�B
�nB
�:B
��B
�B
�@B
�tB
��B
��B
��B
��B
��B
�B
�zB
��B
�B
��B
��B
��B
�RB
�RB
��B
��B
��B
��B
�_B
��B
�_B
�_B
��B
��B
�0B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�6B
�kB
��B
�kB
��B
��B
�qB
�B
�B
�B
�B
�B
�CB
�B
��B
��B
��B
��B
��B
�B
�IB
��B
��B
�OB
�OB
��B
��B
��B
��B
��B
�!B
�!B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�-B
��B
��B
�3B
�3B
�3B
�hB
��B
��B
��B
�B
�nB
�nB
��B
�B
�?B
�?B
�tB
��B
��B
�FB
�FB
��B
��B
�B
�LB
�LB
�LB
�LB
�LB
�LB
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�$B
�$B
�$B
�$B
�$B
�XB
��B
��B
��B
��B
��B
��B
��B
�*B
�*B
�*B
��B
��B
��B
�0B
�0B
�dB
�dB
��B
�6B
�6B
�jB
�jB
�jB
��B
��B
��B
�<B
�qB
�qB
�qB
��B
��B
�B
�BB
�wB
�wBK�BL0BM6BL0BMjBM6BK�BNBM�BK�BNpBL0BMBN�BLdBLdBOBL0BLdBMjBK^BK�BNBK�BK^BMjBM6BK�BLdBL�BMBMBK�BM6BLdBK�BM�BK�BK�BMjBL�BK�BMjBK)BK�BM6BL0BK)BN�BK�BLdBL�BK^BL0BM�BJ�BM6BK�BL�BMjBLdBK�BM6BL�BN�BL0BMjBM�BK�BMjBN<BLdBN�BM�BMBL�BMjBM�BM6BLdBL�BN<BLdBLdBK^BLdBL�BK^BK�BMBK)BL�BMBJ�BL0BL�BJ�BL�BK�BJ�BL�BK�BL�BLdBK)BMjBK�BK�BM�BL0BK^BM6BL0BK�BMBK�BL�BM�BK^BM6BMBK�BMjBL�BK�BMjBLdBK�BMBM�BMBM�BK�BI�BM6BL�BJ#BLdBNpBL0BM6BN�BK^BJ�BK�BK^BL�BN�BLdBOBLdBI�BM6BL�BJ�BL0BK�BJ�BLdBL0BK)BLdBL0BK^BM�BK�BM�BM�BN<BMBN�BN<BM6BM�BM�BL�BOvBPBN�BO�BOBMjBOBBO�BM�BM�BOBBM�BMBN�BMjBLdBN<BN�BL�BOBN�BL�BN�BN�BM6BM�BN�BMBM6BN�BM�BL�BN<BNpBM6BNpBN�BM6BN�BN<BLdBN<BN�BK�BMBM�BM6BNBN�BL0BM�BOBBMBM�BOBL�BL�BN�BM�BMBN�BMjBM6BN�BOBMjBR�BO�BNBQBM�BL�BL�BMBMjBK�BMjBK�BL0BNBL�BX�BNBMjBa|BOvBX�BQNBPBM�BXBT�BV�BaHBjKBn/Bj�BkBjKBjBg8Bm)Bm�Bk�Bh
Bj�Bo5Bh�Bk�BqvBu%Bk�Bt�BgB�+B�uB��B�=B��B�:B�"B�4B�B��B�B��B��B��B��B��B��B��B��B�YB�B��B��B��B�B�CB�qB��B��B��B�B��B�BB�'B�B�HB̘B�B�EB��B��B�sB��B��B�KB��B��B�iB�GB�+B��B�B��B��B��B��B�JB�DB�B 4B��B��B��B�B1B
rB�BbBB	BxB �B'RB+6B33B:�BA�BD3BDgBF?BFtBIBI�BIRBI�BL0BMBLdBJ�BL0BMBK^BJ�BL�BMjBL�BL0BM6BNpBNBN�BPBQNBO�BQ�BR�BRTBT�BV9BV�BXBZQBZ�BX�BYKB]�B^�B_;B`�Bb�B`�B`�BbBa�B`�Ba|Bb�B`�B`�BbBbNB`�Ba|Bb�B`�BaHBcTBb�BaHBb�Bc�BbBa�BcTBd�Bb�Bb�Bd�Bc�Bb�Bc�Bd&Bb�Bb�BdZBdZBc�Bc�Be,Be`Bc�Bc�Be�Bf�Bd�BffBg�Bf2Bf2Bg�Bf�Be,Bf�BgmBkQBiyBj�Bl�Bk�BlWBn/Bl�BlWBn/Bm�Bm�Bl"Bn/Bo5Bo5BlWBncBm�Bm)Bk�Bk�Bl�Bl"Bk�Bl"Bn�BlWBjBkQBk�Bk�BjBiyBhsBi�Bi�Bg�Bg�BhsBg�BffBg�Bh�BiDBl�Bm�Bn�BffBe,B_�B`Bg�B`�BY�BW
BS&BIRBGzBG�BG�BK�BGEBi�BQ�B�B�B$�B�B�B�.B�BSB�.BSB��B�]B��B��B��B"hB�B�B�NB�B�B��B�3B��B�B��B�hB�B��B�7B��B�SB�MB��B��B��B�B�4B��B��B��B�B��B��B��B��B��B�xBʌB��B��Bz�B�B�hB�B�qB��B��B�CB�\B��B�<B��B�OB��B��B��B�@B�B��B�zB��B�*B��B��B��B�0B��B�B��B�kB��B�B��B�CB�B��B��B��B��B�CB�B�B��B�wB��B�wB��B�B�CB��B��B�qB�wB��B�}B�zB��B��B��B�BB�B�HB�}B�3BÖB��B�[B�3B�9B��B��B��BǮB��B��B��B��B�9B��BޞB�B�B��B�B��B�KB�B�B��B�|B��B�/B��B�)B�"B�B�)B�B�B��B�B�KB�sB�B��B�B�B�B��B�B�B�B!B"4B1'B8B1'B2�B8�B7�B9XB>�BD�BIBZ�BU2B]/B`�BiBo BqBq�BzDB��B�:B�B��B��B�@B��B��B��B�=B�1B��B��B�IB��B��B��B�	B�7B��B�eB�xB�B�7B�=B�CB�7B��B��B�CB�B��B�!B�IB��B�B��B��B��B��B�IB�LB�VB�'B�@B��B��B��B�RB��B��B�kB�B�_B��B��B�UB��B�qB��B�zB��B�RB�tB�B��B��B��B��B�OB�kB�B�B�kB��B�CB��B�B��B��B�+B�fB��B��B��B�+B��B�B��B�B�B�{B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202104292026552021042920265520210429202655202104292026552021042920265520210429202655SI  SI  ARFMARFM                                                                                                                                                2021010819285520210108192855IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401320210224164013QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401320210224164013QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021042910194120210429101941IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                