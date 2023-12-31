CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-01-15T18:21:34Z creation; 2023-04-26T19:24:27Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190115182134  20230426192427  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               %   %AA  AOAO7315_008643_037                 7315_008643_037                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @ؠ.����@ؠ.����11  @ؠ.���@ؠ.���@/m�a(9@/m�a(9�d�����d����11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @=p�@z�H@��R@�  @�  @��RA  A ��A,��A@��A`��A�  A�  A�  A�  A��A�  A�Q�A�  A�\)B�
B(�B(�B (�B((�B0  B7�
B@  BH(�BO�BW�
B_�Bg�Bo�
Bx  B�{B�  B��
B�  B�(�B�  B��B�  B��B��
B�  B�Q�B�  B�{B��B�  B�{B�  B��B��B��
B�  B�{B�{B�{B�(�B�{B�{B�(�B�  B�{B�{B��C
=C
=C  C
=C
  C�C��C  C��C��C  C
=C
=C  C  C��C"  C$
=C&  C'�C)�C+��C.
=C0{C2{C4
=C6
=C8
=C:  C;��C=�C?��CB
=CD�CF
=CH  CJ
=CL{CN{CP{CR{CS��CV  CX
=CY��C[�C^  C_�Cb  Cd  Ce��Ch  Cj  Ck�Cm��Cp  Cr  Cs�Cu��Cx  Cz
=C|
=C}��C��C�C�  C�  C�C���C�C�
=C�  C���C�  C�C���C���C�C�C�C�
=C�
=C�C�  C�C���C�  C�  C�  C�  C�C�C�  C���C�  C�C�  C���C���C���C�C�
=C�C�
=C�C���C���C�  C�  C���C��C��C���C�C�C�C���C�  C�C���C�  C�C���C�C�
=C�  C�  C�C�  C���C�  C�C�  C���C���C�  C�  C�  C�  C���C���C���C���C���C���C�  C�C���C���C���C�  C�
=C�  C���C���C���C���C���C���C���C���C�C�C���C���C�C���C���C�C���C�  C�  C�  C�  C�  C�C�C�C�\C�
=C���C���C�  C���C���C�  C�  C���C���C���C�D �D �D  D}qD�qD� D  D}qD�D� D��Dz�D��Dz�D�qD}qD  D� D	�D	��D
D
��D  Dz�D��D}qD  D� D�qD}qD�qD��D  Dz�D�qD� D  D��D�qD� D  Dz�D�qD�D  D}qD�qD}qD�D�D  Dz�D�qD��D  Dz�D  D�D�qD��DD��D  D}qD�qD }qD!  D!}qD"  D"� D#  D#� D$�D$��D%�D%�D&  D&� D'�D'��D(  D(z�D(��D)��D*�D*z�D+  D+�D+�qD,}qD-  D-� D-�qD.��D/  D/��D0D0� D0��D1z�D1��D2z�D2�qD3z�D3��D4��D5�D5� D5��D6z�D7  D7� D7�qD8}qD9  D9� D:�D:}qD:�qD;��D<D<� D=�D=�D>D>� D?  D?}qD?��D@� DA�DA��DB�DB��DCDC�DD  DD� DEDE��DF  DF�DG�DG�DHDH��DH�qDI}qDI��DJz�DK  DK�DLDL�DMDM��DN�DN� DN��DOxRDO�qDP��DQDQ� DR  DR}qDS  DS�DTDT� DU  DU� DV  DV}qDV�qDW� DW�qDXz�DY  DY��DZ  DZ��D[D[��D\�D\��D\�qD]z�D]�qD^��D_D_� D_��D`}qD`�qDa� DbDb� Dc  Dc��Dc�qDd� DeDe� Df  Df��Dg  Dg��Dh�Dh� Dh�qDiz�Dj  Dj� Dj�qDk�DlDl� Dl�qDm� Dn�Dn��Do�Do��Dp�Dp� Dp�qDq� DrDr� Dr�qDs}qDt  Dt� Du�Du�Dv�Dv}qDv��Dw��Dx�Dx}qDy  Dy��Dz�Dz�D{D{�D|D|�D}D}�D~�D~�D  Dz�D�  D�C�D���D�� D�  D�=qD�~�D�� D�HD�AHD��HD��qD���D�=qD�� D�� D�  D�B�D�� D�� D�HD�@ D��HD�D�HD�>�D���D�D�HD�B�D�� D�� D�HD�=qD�|)D���D�HD�AHD��HD��HD�  D�AHD�~�D�� D�  D�>�D��HD�� D�  D�>�D�� D��HD���D�>�D�~�D��qD���D�>�D�~�D�� D�  D�AHD�~�D���D���D�AHD��HD�� D��qD�>�D��HD�� D�  D�AHD��HD���D�  D�@ D�� D��HD�HD�>�D�~�D��HD��D�@ D�}qD��HD�HD�>�D��HD��HD�  D�@ D�~�D�� D�HD�@ D��HD�� D�  D�>�D�� D��HD�HD�B�D�� D��qD���D�AHD��HD�� D���D�>�D�~�D��qD���D�AHD��HD��HD��D�>�D�}qD��HD��D�AHD��HD�� D�HD�@ D�~�D�� D�  D�@ D�~�D���D�  D�B�D���D�D�HD�AHD�� D�� D�  D�AHD�~�D��)D��qD�>�D��HD��HD���D�=qD�}qD���D�  D�AHD��HD�D�HD�@ D�~�D��qD���D�>�D�� D��HD�HD�B�D���D�� D�  D�@ D�~�D���D�  D�@ D��HD��HD��D�AHD�~�D���D��qD�>�D��HD��HD�HD�B�D��HD��HD��D�AHD�� D���D�  D�B�D��HD�� D�HD�@ D�|)D���D��qD�>�D�~�D���D��qD�=qD��HD��HD���D�>�D��HD��HD��qD�<)D�~�D�D��D�@ D��HD�D�HD�AHD�� D�� D��D�AHD���D��HD�  D�>�D�~�D���D��qD�>�D�� D���D���D�@ D��HD�� D���D�AHD���D�� D���D�@ D�� D��HD�HD�AHD��HD�D�  D�>�D�� D���D���D�@ D�� D��HD��D�>�D�|)D½qD���D�@ D�~�D��HD�HD�@ DĀ D��HD��qD�=qD�~�D�� D���D�>�D�~�Dƾ�D�HD�B�DǁHD�� D���D�>�DȀ D�D�  D�AHDɂ�DɽqD�HD�@ D�}qDʼ)D��)D�@ D�~�D˾�D�  D�>�D́HD̾�D���D�@ D�}qD;�D�  D�>�D΁HDξ�D���D�AHDρHD��HD�  D�AHDЁHD�� D�  D�AHDсHD�� D���D�>�DҁHD�� D���D�B�Dӂ�D�� D�  D�@ D�~�D�� D���D�=qD�}qD��HD�  D�@ Dր D�� D���D�@ D׀ D�� D�  D�AHD؀ Dؾ�D���D�=qDـ D�� D�  D�@ D�~�D�� D�  D�>�DہHD��HD���D�@ D�~�D�� D�HD�>�D�}qD�� D�  D�>�DށHD�� D���D�@ D߁HD�� D�  D�@ D�� D�� D��D�AHD�HDᾸD�  D�@ D�~�D�� D��D�B�D� D�� D���D�>�D� D�� D�HD�>�D� D�� D��D�>�D�HD��HD�HD�>�D�~�D�� D��qD�@ D肏D�� D�  D�@ D� D�� D��D�AHD�HD�� D��D�@ D� D�� D��D�@ D�HD�D���D�AHD킏D���D�  D�AHD�}qD��HD�HD�AHD� D�� D��D�@ D�~�D�� D���D�B�D�}qD�D��)D�@ D�~�D��HD�  D�@ D�HD��HD��D�>�D�~�D��)D��qD�AHD���D���D���D�C�D��HD��qD���D�=qD�� D��HD�  D�=qD�|)D�� D��D�AHD�~�D�� D�HD�AHD���D���>�?�?W
=?�z�?�Q�?�
=?�@�@z�@+�@=p�@O\)@W
=@h��@xQ�@��@���@�Q�@��R@���@�z�@�(�@\@˅@�@�G�@�@��@��HA�\A��A{A�\A
=A(�A"�\A'�A-p�A1G�A6ffA;�AAG�AHQ�AN{AS33AW�A[�AaG�AfffAk�Ar�\Aw�A|(�A�  A��\A�{A���A��
A�ffA���A��HA��A�  A��\A�{A���A��HA�z�A�\)A�=qA�p�A��A���A�(�A��RA���A�z�A��A��A���A�ffA���A��
A�
=A��A�z�AָRA���A�33A�{AᙚA��
A�
=A��A�33A�A�Q�A��
A�ffA�Q�A�=qA��A��B�BffB(�Bp�B�RB  B	p�B
ffB�B��B�RB(�Bp�B�\B�B��BffB�Bp�B�\B�
B��B�B33B z�B"{B#�B$��B&{B'�B((�B)p�B*�HB,Q�B-B.�HB0(�B0��B2=qB3�B4��B6�\B7�
B9�B:=qB;33B<Q�B=��B?
=B@��BA�BC33BD  BEG�BF�RBH  BIp�BK
=BLQ�BMG�BNffBO�
BQ�BR�\BT(�BUp�BV�RBX(�BY�BZ{B[\)B\��B^{B_�B`��Ba�Bb�HBd  Bep�Bf�HBhz�Bi��Bj�HBl(�Bm�Bn=qBo�Bp��Br{Bs�
BuG�Bv�RBx  ByG�Bz�\B{\)B|��B~{B\)B�z�B�G�B��
B�z�B��B�B�ffB���B�p�B�{B���B�p�B�{B��RB��B�=qB��HB��B�{B��\B�33B��
B�z�B�
=B��
B���B�33B��
B�z�B��B���B�(�B��HB��B�=qB�
=B�B�Q�B���B���B�  B���B�p�B�{B���B��B�(�B��HB��B�(�B���B�p�B�{B��RB�\)B��B�z�B�
=B���B�=qB��HB�p�B�Q�B��HB�p�B��B��\B���B�\)B��
B�{B�z�B���B�
=B�\)B��B�  B�Q�B�ffB��RB���B��HB�
=B�G�B��B��B��
B�(�B��\B��RB��HB�
=B�G�B��B���B��B��
B�{B�ffB��RB��HB��B�G�B�33B�p�B���B��
B�  B�ffB���B���B��HB���B��B�33B�p�B��B��
B�=qB�z�B���B���B�
=B��B�\)B��B���B��
B�{B�=qB��RB��HB��B�G�B��B��B��
B��B�{B�Q�B�z�B��RB���B��B�G�B��B��
B�(�B�Q�B��\B��RB���B��B��B�G�B�p�B��B��B�(�B�ffB��\B���B���B�33B��B�B�  B�=qB�ffB��\B���B��HB���B��B�G�B��B��B��
B�{B�ffB£�B��HB��B�\)B�p�BîB��B�  B�=qB�ffB�z�Bģ�B��HB��B�G�B�p�B�B��
B�(�B�Q�Bƣ�B���B��B�G�BǅB�B�  B�=qB�ffBȣ�B���B��B�33BɅB��B�{B�=qB�z�B���B�33B�p�BˮB�  B�ffḄ�B���B�G�B��B�(�BΏ\B���B�33BυB��
B�=qBУ�B���B�\)BѮB�{B�ffB���B�33BӅB��B�Q�BԸRB�33BՅB��B�ffBָRB��Bי�B�{B؏\B�
=BمB�  B�z�B��HB�p�B��B�ffB��HB�p�B��
B�ffB��HB�p�B��B�z�B���B�B�  B�\B��B㙚B�{B��B���B�B�  B�\B�
=B癚B�(�B�RB�33B�B�=qB���B�\)B��B�\B���B�\)B��B�ffB�
=B�B�{B�RB�G�B��
B�ffB���B�p�B�  B�\B��B��B�=qB���B�33B��B�=qB���B�\)B��
B�z�B�
=B�B�Q�B��HB�p�B�  B��\B��B��C 
=C G�C �C ��C{CQ�C��C��C{Cp�C�RC��C33Cz�CC  C33C\)C��C�C(�CffC�C�C�C\)C�C  C=qC�C�RC�HC�CQ�C�\C�
C	33C	p�C	�RC	�
C
{C
Q�C
�\C
��C
=CG�C�C�C(�CQ�C�C��C  C\)C��C�
C�C\)C�\C�RC�C(�CffC��C��C33Cp�C�RC�
C{CG�C�C�RC  CG�C�C��C�C{CG�C��C�
C{CG�Cz�C��C�HC{C\)C��C�HC�C\)Cz�C�C�HC�CffC�RC�C(�Cp�C�CC  C=qC�C��C{CG�CffC�C�HC=qCz�C�RC�HC{CQ�C�\C�HC�CffC��CC  C33C�C�
C{CG�Cp�C��C��C G�C �C �C �HC!�C!\)C!�C!�C"�C"G�C"�C"�HC#�C#Q�C#�C#�RC$
=C$Q�C$��C$�RC$�C%G�C%�\C%�RC%��C&33C&z�C&��C'
=C'33C'z�C'C({C(\)C(�\C(��C)
=C)Q�C)��C)��C*�C*ffC*C+
=C+\)C+z�C+��C,{C,p�C,�RC,��C-(�C-p�C-�
C.(�C.z�C.�C.��C/Q�C/�C0  C033C0p�C0�
C133C1�C1�RC2
=C2p�C2��C2��C3G�C3�C4  C4\)C4�C4�HC5=qC5��C5��C6(�C6z�C6�C7=qC7z�C7�RC8�C8z�C8�C9  C9Q�C9�RC:{C:=qC:�\C:��C;Q�C;�C;��C<(�C<�\C<C={C=\)C=��C>�C>G�C>��C?  C?Q�C?��C?��C@�C@�C@�
CA
=CAQ�CA�RCB
=CB=qCB�CB��CC=qCCz�CC��CD(�CDz�CD��CE  CE\)CE��CE�
CF=qCF�CFCG  CG\)CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               ?��@   @=p�@z�H@��R@�  @�  @��RA  A ��A,��A@��A`��A�  A�  A�  A�  A��A�  A�Q�A�  A�\)B�
B(�B(�B (�B((�B0  B7�
B@  BH(�BO�BW�
B_�Bg�Bo�
Bx  B�{B�  B��
B�  B�(�B�  B��B�  B��B��
B�  B�Q�B�  B�{B��B�  B�{B�  B��B��B��
B�  B�{B�{B�{B�(�B�{B�{B�(�B�  B�{B�{B��C
=C
=C  C
=C
  C�C��C  C��C��C  C
=C
=C  C  C��C"  C$
=C&  C'�C)�C+��C.
=C0{C2{C4
=C6
=C8
=C:  C;��C=�C?��CB
=CD�CF
=CH  CJ
=CL{CN{CP{CR{CS��CV  CX
=CY��C[�C^  C_�Cb  Cd  Ce��Ch  Cj  Ck�Cm��Cp  Cr  Cs�Cu��Cx  Cz
=C|
=C}��C��C�C�  C�  C�C���C�C�
=C�  C���C�  C�C���C���C�C�C�C�
=C�
=C�C�  C�C���C�  C�  C�  C�  C�C�C�  C���C�  C�C�  C���C���C���C�C�
=C�C�
=C�C���C���C�  C�  C���C��C��C���C�C�C�C���C�  C�C���C�  C�C���C�C�
=C�  C�  C�C�  C���C�  C�C�  C���C���C�  C�  C�  C�  C���C���C���C���C���C���C�  C�C���C���C���C�  C�
=C�  C���C���C���C���C���C���C���C���C�C�C���C���C�C���C���C�C���C�  C�  C�  C�  C�  C�C�C�C�\C�
=C���C���C�  C���C���C�  C�  C���C���C���C�D �D �D  D}qD�qD� D  D}qD�D� D��Dz�D��Dz�D�qD}qD  D� D	�D	��D
D
��D  Dz�D��D}qD  D� D�qD}qD�qD��D  Dz�D�qD� D  D��D�qD� D  Dz�D�qD�D  D}qD�qD}qD�D�D  Dz�D�qD��D  Dz�D  D�D�qD��DD��D  D}qD�qD }qD!  D!}qD"  D"� D#  D#� D$�D$��D%�D%�D&  D&� D'�D'��D(  D(z�D(��D)��D*�D*z�D+  D+�D+�qD,}qD-  D-� D-�qD.��D/  D/��D0D0� D0��D1z�D1��D2z�D2�qD3z�D3��D4��D5�D5� D5��D6z�D7  D7� D7�qD8}qD9  D9� D:�D:}qD:�qD;��D<D<� D=�D=�D>D>� D?  D?}qD?��D@� DA�DA��DB�DB��DCDC�DD  DD� DEDE��DF  DF�DG�DG�DHDH��DH�qDI}qDI��DJz�DK  DK�DLDL�DMDM��DN�DN� DN��DOxRDO�qDP��DQDQ� DR  DR}qDS  DS�DTDT� DU  DU� DV  DV}qDV�qDW� DW�qDXz�DY  DY��DZ  DZ��D[D[��D\�D\��D\�qD]z�D]�qD^��D_D_� D_��D`}qD`�qDa� DbDb� Dc  Dc��Dc�qDd� DeDe� Df  Df��Dg  Dg��Dh�Dh� Dh�qDiz�Dj  Dj� Dj�qDk�DlDl� Dl�qDm� Dn�Dn��Do�Do��Dp�Dp� Dp�qDq� DrDr� Dr�qDs}qDt  Dt� Du�Du�Dv�Dv}qDv��Dw��Dx�Dx}qDy  Dy��Dz�Dz�D{D{�D|D|�D}D}�D~�D~�D  Dz�D�  D�C�D���D�� D�  D�=qD�~�D�� D�HD�AHD��HD��qD���D�=qD�� D�� D�  D�B�D�� D�� D�HD�@ D��HD�D�HD�>�D���D�D�HD�B�D�� D�� D�HD�=qD�|)D���D�HD�AHD��HD��HD�  D�AHD�~�D�� D�  D�>�D��HD�� D�  D�>�D�� D��HD���D�>�D�~�D��qD���D�>�D�~�D�� D�  D�AHD�~�D���D���D�AHD��HD�� D��qD�>�D��HD�� D�  D�AHD��HD���D�  D�@ D�� D��HD�HD�>�D�~�D��HD��D�@ D�}qD��HD�HD�>�D��HD��HD�  D�@ D�~�D�� D�HD�@ D��HD�� D�  D�>�D�� D��HD�HD�B�D�� D��qD���D�AHD��HD�� D���D�>�D�~�D��qD���D�AHD��HD��HD��D�>�D�}qD��HD��D�AHD��HD�� D�HD�@ D�~�D�� D�  D�@ D�~�D���D�  D�B�D���D�D�HD�AHD�� D�� D�  D�AHD�~�D��)D��qD�>�D��HD��HD���D�=qD�}qD���D�  D�AHD��HD�D�HD�@ D�~�D��qD���D�>�D�� D��HD�HD�B�D���D�� D�  D�@ D�~�D���D�  D�@ D��HD��HD��D�AHD�~�D���D��qD�>�D��HD��HD�HD�B�D��HD��HD��D�AHD�� D���D�  D�B�D��HD�� D�HD�@ D�|)D���D��qD�>�D�~�D���D��qD�=qD��HD��HD���D�>�D��HD��HD��qD�<)D�~�D�D��D�@ D��HD�D�HD�AHD�� D�� D��D�AHD���D��HD�  D�>�D�~�D���D��qD�>�D�� D���D���D�@ D��HD�� D���D�AHD���D�� D���D�@ D�� D��HD�HD�AHD��HD�D�  D�>�D�� D���D���D�@ D�� D��HD��D�>�D�|)D½qD���D�@ D�~�D��HD�HD�@ DĀ D��HD��qD�=qD�~�D�� D���D�>�D�~�Dƾ�D�HD�B�DǁHD�� D���D�>�DȀ D�D�  D�AHDɂ�DɽqD�HD�@ D�}qDʼ)D��)D�@ D�~�D˾�D�  D�>�D́HD̾�D���D�@ D�}qD;�D�  D�>�D΁HDξ�D���D�AHDρHD��HD�  D�AHDЁHD�� D�  D�AHDсHD�� D���D�>�DҁHD�� D���D�B�Dӂ�D�� D�  D�@ D�~�D�� D���D�=qD�}qD��HD�  D�@ Dր D�� D���D�@ D׀ D�� D�  D�AHD؀ Dؾ�D���D�=qDـ D�� D�  D�@ D�~�D�� D�  D�>�DہHD��HD���D�@ D�~�D�� D�HD�>�D�}qD�� D�  D�>�DށHD�� D���D�@ D߁HD�� D�  D�@ D�� D�� D��D�AHD�HDᾸD�  D�@ D�~�D�� D��D�B�D� D�� D���D�>�D� D�� D�HD�>�D� D�� D��D�>�D�HD��HD�HD�>�D�~�D�� D��qD�@ D肏D�� D�  D�@ D� D�� D��D�AHD�HD�� D��D�@ D� D�� D��D�@ D�HD�D���D�AHD킏D���D�  D�AHD�}qD��HD�HD�AHD� D�� D��D�@ D�~�D�� D���D�B�D�}qD�D��)D�@ D�~�D��HD�  D�@ D�HD��HD��D�>�D�~�D��)D��qD�AHD���D���D���D�C�D��HD��qD���D�=qD�� D��HD�  D�=qD�|)D�� D��D�AHD�~�D�� D�HD�AHD���G�O�>�?�?W
=?�z�?�Q�?�
=?�@�@z�@+�@=p�@O\)@W
=@h��@xQ�@��@���@�Q�@��R@���@�z�@�(�@\@˅@�@�G�@�@��@��HA�\A��A{A�\A
=A(�A"�\A'�A-p�A1G�A6ffA;�AAG�AHQ�AN{AS33AW�A[�AaG�AfffAk�Ar�\Aw�A|(�A�  A��\A�{A���A��
A�ffA���A��HA��A�  A��\A�{A���A��HA�z�A�\)A�=qA�p�A��A���A�(�A��RA���A�z�A��A��A���A�ffA���A��
A�
=A��A�z�AָRA���A�33A�{AᙚA��
A�
=A��A�33A�A�Q�A��
A�ffA�Q�A�=qA��A��B�BffB(�Bp�B�RB  B	p�B
ffB�B��B�RB(�Bp�B�\B�B��BffB�Bp�B�\B�
B��B�B33B z�B"{B#�B$��B&{B'�B((�B)p�B*�HB,Q�B-B.�HB0(�B0��B2=qB3�B4��B6�\B7�
B9�B:=qB;33B<Q�B=��B?
=B@��BA�BC33BD  BEG�BF�RBH  BIp�BK
=BLQ�BMG�BNffBO�
BQ�BR�\BT(�BUp�BV�RBX(�BY�BZ{B[\)B\��B^{B_�B`��Ba�Bb�HBd  Bep�Bf�HBhz�Bi��Bj�HBl(�Bm�Bn=qBo�Bp��Br{Bs�
BuG�Bv�RBx  ByG�Bz�\B{\)B|��B~{B\)B�z�B�G�B��
B�z�B��B�B�ffB���B�p�B�{B���B�p�B�{B��RB��B�=qB��HB��B�{B��\B�33B��
B�z�B�
=B��
B���B�33B��
B�z�B��B���B�(�B��HB��B�=qB�
=B�B�Q�B���B���B�  B���B�p�B�{B���B��B�(�B��HB��B�(�B���B�p�B�{B��RB�\)B��B�z�B�
=B���B�=qB��HB�p�B�Q�B��HB�p�B��B��\B���B�\)B��
B�{B�z�B���B�
=B�\)B��B�  B�Q�B�ffB��RB���B��HB�
=B�G�B��B��B��
B�(�B��\B��RB��HB�
=B�G�B��B���B��B��
B�{B�ffB��RB��HB��B�G�B�33B�p�B���B��
B�  B�ffB���B���B��HB���B��B�33B�p�B��B��
B�=qB�z�B���B���B�
=B��B�\)B��B���B��
B�{B�=qB��RB��HB��B�G�B��B��B��
B��B�{B�Q�B�z�B��RB���B��B�G�B��B��
B�(�B�Q�B��\B��RB���B��B��B�G�B�p�B��B��B�(�B�ffB��\B���B���B�33B��B�B�  B�=qB�ffB��\B���B��HB���B��B�G�B��B��B��
B�{B�ffB£�B��HB��B�\)B�p�BîB��B�  B�=qB�ffB�z�Bģ�B��HB��B�G�B�p�B�B��
B�(�B�Q�Bƣ�B���B��B�G�BǅB�B�  B�=qB�ffBȣ�B���B��B�33BɅB��B�{B�=qB�z�B���B�33B�p�BˮB�  B�ffḄ�B���B�G�B��B�(�BΏ\B���B�33BυB��
B�=qBУ�B���B�\)BѮB�{B�ffB���B�33BӅB��B�Q�BԸRB�33BՅB��B�ffBָRB��Bי�B�{B؏\B�
=BمB�  B�z�B��HB�p�B��B�ffB��HB�p�B��
B�ffB��HB�p�B��B�z�B���B�B�  B�\B��B㙚B�{B��B���B�B�  B�\B�
=B癚B�(�B�RB�33B�B�=qB���B�\)B��B�\B���B�\)B��B�ffB�
=B�B�{B�RB�G�B��
B�ffB���B�p�B�  B�\B��B��B�=qB���B�33B��B�=qB���B�\)B��
B�z�B�
=B�B�Q�B��HB�p�B�  B��\B��B��C 
=C G�C �C ��C{CQ�C��C��C{Cp�C�RC��C33Cz�CC  C33C\)C��C�C(�CffC�C�C�C\)C�C  C=qC�C�RC�HC�CQ�C�\C�
C	33C	p�C	�RC	�
C
{C
Q�C
�\C
��C
=CG�C�C�C(�CQ�C�C��C  C\)C��C�
C�C\)C�\C�RC�C(�CffC��C��C33Cp�C�RC�
C{CG�C�C�RC  CG�C�C��C�C{CG�C��C�
C{CG�Cz�C��C�HC{C\)C��C�HC�C\)Cz�C�C�HC�CffC�RC�C(�Cp�C�CC  C=qC�C��C{CG�CffC�C�HC=qCz�C�RC�HC{CQ�C�\C�HC�CffC��CC  C33C�C�
C{CG�Cp�C��C��C G�C �C �C �HC!�C!\)C!�C!�C"�C"G�C"�C"�HC#�C#Q�C#�C#�RC$
=C$Q�C$��C$�RC$�C%G�C%�\C%�RC%��C&33C&z�C&��C'
=C'33C'z�C'C({C(\)C(�\C(��C)
=C)Q�C)��C)��C*�C*ffC*C+
=C+\)C+z�C+��C,{C,p�C,�RC,��C-(�C-p�C-�
C.(�C.z�C.�C.��C/Q�C/�C0  C033C0p�C0�
C133C1�C1�RC2
=C2p�C2��C2��C3G�C3�C4  C4\)C4�C4�HC5=qC5��C5��C6(�C6z�C6�C7=qC7z�C7�RC8�C8z�C8�C9  C9Q�C9�RC:{C:=qC:�\C:��C;Q�C;�C;��C<(�C<�\C<C={C=\)C=��C>�C>G�C>��C?  C?Q�C?��C?��C@�C@�C@�
CA
=CAQ�CA�RCB
=CB=qCB�CB��CC=qCCz�CC��CD(�CDz�CD��CE  CE\)CE��CE�
CF=qCF�CFCG  CG\)CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@��@�\G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�hsA�l�A�n�A�hsA�jA�x�A�z�A�z�A�x�A�~�A�|�A�|�A�z�AρAω7Aω7AϋDAϋDAϋDAύPAύPAύPAϋDAϋDAϋDAω7AρA�|�A�l�A�Q�A�C�A�1'A�+A�-A�(�A��A���A��A��A��yA��mA��`A��A���A���A���A��A��A��yA��TA��#AΛ�Aκ^AάAͰ!Ȁ\A��AȼjA�
=A�A�p�A�Q�A�ZA�bNA���A���A���A��7A�^5A�JA�S�A��-A�jA�ffA�M�A��FA�/A���A��#A�$�A���A�E�A�t�A�?}A�v�A���A�Q�A�z�A��A�JA�XA��RA�oA���A��A�M�A��jA�z�A��\A�p�A�VA���A�`BA��A�1A�C�A��A��A�ȴA�9XAy�As�-Ap$�Am�7Ak;dAg�TAgK�Ag�Af��A_dZAZE�AY?}AX1'AUl�AP �AL�AL�!AK�mAG�#AEO�ACG�A@bA<M�A;�7A;K�A;?}A;&�A:�jA9l�A8�yA8z�A7�-A7�A2=qA.5?A,�A+�7A+��A+�A,1'A,=qA.-A.ĜA-��A-+A,=qA+A+��A+\)A+/A+�A)C�A&A�A$z�A#�^A#K�A"��A"�RA"^5A!�7A�FA&�A5?AdZAC�A�A��A��A?}AoAbNA=qA�AbNAC�A�+A �A�RA�AbNA��A��A��A��A�A�/A�A33A�AZA
~�A��AoA�A�A&�AĜA�A�A�yA�HA��Az�AbA(�A9XAI�AZA;dA��A�A7LA �RA {@���@��@�J@��@�&�@�C�@��`@�dZ@�
=@��@��m@�@���@�J@���@�9X@��
@�^5@�V@�^5@�{@��@��@�1@�|�@�33@�o@�^@�ƨ@�E�@�x�@�G�@�u@�t�@�~�@�@�;d@���@�^5@��@��@��`@��@޸R@�J@���@�(�@۝�@�l�@�33@�o@���@��T@��@�V@���@���@���@��`@ؓu@�+@�^5@�E�@�5?@�5?@�$�@��@���@Ցh@�p�@�G�@��@���@�Ĝ@ԛ�@�z�@�Z@�(�@���@�ƨ@ӍP@�C�@��@�&�@�V@��`@д9@Гu@�j@�A�@�b@���@Ϯ@�t�@�K�@��@Ώ\@�E�@�$�@�{@��T@ͩ�@�`B@�7L@��@̬@�(�@�+@���@ʧ�@�ff@���@�x�@�r�@���@Ǖ�@ǝ�@ǶF@���@ļj@�ƨ@�|�@�33@�ȴ@\@�M�@���@��`@�j@�dZ@�-@�G�@�+@��\@�{@��@���@�9X@�o@���@�$�@���@��h@�G�@��u@��F@�K�@��F@���@�Z@���@���@���@��@�A�@��@���@�@�O�@�Ĝ@�Q�@� �@��@�33@��@��@���@��h@�&�@��D@�Q�@�(�@��@�o@��!@�^5@�hs@� �@��w@�|�@��@���@�M�@�-@�J@��#@�X@���@�Q�@�Q�@�1'@�9X@�r�@�  @�|�@�S�@���@�-@�5?@�J@�@���@�O�@��9@�Z@��@��9@��@��9@��@���@��u@�r�@�9X@�1@���@�t�@�K�@���@���@��^@���@�hs@�Q�@�b@��m@��
@��
@��@���@��P@�S�@�+@��!@��@�&�@���@�r�@��@�  @���@��;@���@���@�l�@��@�v�@�^5@�$�@��@���@���@�X@�%@���@���@�Z@�b@���@�|�@�S�@�"�@��y@��H@��R@�-@��@��#@��7@�G�@��@��j@�z�@�1'@��m@�ƨ@�K�@��@���@�~�@�ff@��@���@���@�x�@�p�@�G�@�V@���@���@��w@�|�@�|�@�|�@�t�@�l�@�S�@�;d@�
=@��!@���@�ff@��@��@��^@���@��7@�p�@�?}@���@��j@���@�r�@�9X@�  @��F@�l�@�;d@��R@�E�@�$�@�{@���@�@��^@���@�`B@�/@��@��/@��9@�j@�Z@�Q�@�I�@�  @��@�dZ@�33@���@�v�@�^5@��@���@�p�@�V@��9@�j@�w@l�@
=@~�+@}p�@}/@|�@{�@{@z^5@zJ@yhs@y&�@xbN@wl�@vv�@vv�@vV@vE�@v@u�-@uO�@t�@t��@tz�@t9X@s�m@s��@s��@s��@s��@s�F@s�F@st�@s"�@r=q@q��@q�^@q��@q�@p1'@o\)@n�y@n��@nE�@m�h@l�j@k�m@k�@k33@j�\@i�#@i��@ix�@iX@i&�@i�@i%@h��@h�u@g��@gl�@f��@f�@f�R@fff@f@e��@d�/@dj@dI�@c��@c33@c33@co@b�@b��@b�\@b^5@bM�@b-@b�@a�@a�^@ahs@a�@`�u@_�@^�R@^ff@^V@^@]�@]?}@\��@\��@\��@\(�@[��@[dZ@Z�\@Y�7@XĜ@X1'@X  @W��@Vv�@U`B@S��@S@Q�#@P��@PQ�@O��@OK�@Nȴ@N�+@M�T@L��@L�@L�D@Lj@LZ@L9X@L�@L�@L1@K�m@Kƨ@K�F@K�@Ko@JM�@IX@HQ�@G�;@G�w@G��@GK�@F�+@F{@E�T@EO�@EV@D�/@D�@D��@Dz�@D1@C�
@C��@Ct�@CC�@B��@B~�@B=q@Ax�@A�@@�9@?�@?+@>�+@>5?@>$�@=�T@=`B@<�@<�D@<z�@<z�@<9X@<(�@<1@<1@<1@;��@;ƨ@;33@:�@:�H@:��@:~�@:J@9��@97L@8��@8r�@7�@7�@7
=@6ȴ@6�R@6��@6V@6{@5�h@5V@4��@4z�@4Z@3�F@2�@2��@2M�@2J@1��@2J@1��@1��@1�#@1�7@0��@/�;@/��@/|�@/K�@/
=@.�y@.��@.v�@.5?@.@-�T@-��@-`B@-O�@-?}@-/@-V@,�@+dZ@+@+@*�@*�@*�H@*��@*-@)��@)��@)�7@)7L@(bN@(1'@( �@(b@(  @'�@'��@'�w@'�@'�P@'�P@';d@'+@';d@'+@'
=@&��@&ȴ@&��@&ff@&E�@&$�@&@%�T@%��@%?}@$�/@$��@$�j@$��@$�j@$�j@$z�@$(�@$�@#�m@#�F@#��@#��@#dZ@"��@"M�@"�@"J@!�#@!�#@!�^@!��@!X@!7L@!&�@!&�@!�@ �`@ �u@�@�w@�w@�w@�P@
=@�R@��@{@�T@�T@��@��@O�@`B@p�@?}@�@��@��@��@��@�/@�@�/@�@z�@z�@I�@�@�F@dZ@o@~�@n�@^5@M�@M�@M�@=q@-@J@�@�^@�7@7L@��@��@��@�9@�u@r�@r�@Q�@A�@A�@1'@b@  @  @b@  @  @��@�@�P@K�@�@�R@�+@5?@{@@�h@O�@/@�@�@��@Z@I�@I�@��@�F@t�@�H@~�@^5@=q@-@-@�@��@��@�^@7L@�`@��@�@bN@Q�@1'@1'@ �@ �@  @  @�@��@K�@+@��@$�@@@��@�@O�@��@z�@�@��@�m@�F@dZ@S�@C�@33@"�@o@
�@
�H@
��@
~�@
M�@
-@
�@
J@	��@	��A�hsA�jA�dZA�dZA�hsA�hsA�n�A�n�A�t�A�r�A�l�A�dZA�jA�jA�jA�ffA�hsA�ffA�jA�n�A�jA�z�A�|�A�|�A�x�A�z�A�x�A�|�A�|�A�x�A�t�A�v�A�x�A�|�A�~�A�z�A�|�A�~�A�~�A�~�A�|�A�z�A�z�A�z�A�x�A�|�A�~�A�~�A�|�A�z�A�z�A�z�A�~�AρAσAχ+Aχ+Aχ+Aχ+Aχ+AϋDAϋDAϋDAυAχ+Aχ+AϋDAύPAω7Aω7Aω7Aω7AύPAύPAύPAϋDAω7Aω7AϋDAύPAύPAϋDAω7Aω7AύPAϋDAύPAϏ\AϑhAύPAω7Aω7AϋDAϏ\AϏ\AϏ\AύPAω7AύPAϏ\AϏ\AϏ\AύPAύPAϋDAω7Aω7Aω7Aω7AϋDAύPAϏ\AύPAω7Aω7AϋDAϋDAύPAύPAύPAϋDAω7Aω7AϋDAύPAύPAύPAϋDAχ+AυAσAρAρAυAσAρA�|�A�z�A�|�A�~�AρA�|�A�r�A�n�A�n�A�n�A�l�A�ffA�dZA�bNA�XA�Q�A�K�A�K�A�K�A�K�A�K�A�G�A�E�A�=qA�;dA�9XA�9XA�7LA�33A�/A�+A�&�A�&�A�(�A�+A�-A�/A�/A�-A�+A�(�A�+A�-A�-A�-A�+A�(�A�&�A�&�A�$�A�$�A�$�A�$�A�"�A�"�A��A�bA���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��`A��`A��`A��mA��mA��mA��mA��mA��`A��`A��`A��TA��TA��`A��mA��yA��A��A���A���A���A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��mA��`A��`A��TA��`A��`A��mA��mA��mA��mA��mA��`A��TA��HA��HA��HA��;A��HA��HA��HA��TA��`A��`A��HA��;A��/A��/A��/A��#A��;A��;A��/A��#A��#A��
A��
A��
A��A��A��A��A���A���A���A�ȴA�ȴA�ƨA���AξwAξwAθRAΰ!AΣ�AΓuA΍PA�z�A�ffA�dZA�n�A�v�A΁A·+A΋DAΏ\AΑhAΑhAΣ�AΣ�AΣ�AΣ�AΣ�AΩ�AΧ�AΧ�A�ĜA���A���A���A��#A��;A��HA��yA��A��A��A���A���A�%A���A��;A�ĜAξwAξwAΰ!AΛ�AΏ\A΅A�z�A�r�A�n�A�hsA�bNA�ZA�VA�O�A�I�A�A�A�A���AͲ-Aͩ�A͝�A͝�A͝�A͝�A͝�A͟�A͛�A͋DA�z�A�`BA�O�A�;dA�+A�"�A� �A� �A��A�oA�1A���A���A̮A̍PA�~�A�p�A�XA�&�A���A�z�A�VA�1'A��A��
Aʗ�A�jA�ZA�/A�%A���Aɰ!Aɩ�A�JA���A���A��A��yA��/A���A���A�ĜAȮAȅA�x�A�hsA�\)A�G�A�1'A��A�JA�
=A�1A�A���A��yA�A�A��;A���AƮA�`BAŬA�VA���Aĝ�A�bNA��A��AÇ+A��A���A���A���A¥�AA�v�A�M�A�+A�5?A� �A�oA�  A��HA��hA�`BA�oA��yA��#A�ĜA��-A��!A���A��+A�XA��A���A�bNA�/A��DA�l�A�bNA�`BA�`BA�`BA�^5A�S�A�E�A��A��yA���A�r�A��HA�v�A��`A�z�A�7LA��A��!A�~�A�=qA��;A�hsA�-A��A��A���A�ĜA���A�v�A�XA�9XA�
=A��#A��^A�O�A���A��A�ƨA��RA���A���A��7A�n�A�S�A�?}A�33A�+A��A�oA�VA�A��A�ƨA��7A�"�A��/A��^A��uA��A��A�l�A�O�A�"�A��yA�ȴA���A��A�x�A�jA�`BA�1'A�A��-A��A�C�A�=qA�=qA�;dA�/A�"�A�oA��A��wA���A���A���A��A�r�A�Q�A�C�A�9XA�33A�-A�(�A��A�bA�
=A�%A���A��;A���A�x�A�ffA�O�A�"�A��\A�5?A�  A��HA���A��jA���A�bNA�  A��9A��+A�O�A�/A�
=A���A��HA���A��hA�n�A�/A� �A�bA�
=A��HA��9A���A�v�A�XA�+A�VA���A���A���A�1A���A���A�dZA��A���A�`BA�"�A��A���A���A��\A�x�A�l�A�XA�5?A��A���A��HA��wA���A��uA��A�n�A�^5A�S�A�A�A�&�A�VA���A���A��hA�l�A�S�A�7LA�+A��A��A��A�G�A��A�A��A��A�(�A��TA���A��A�jA�VA�;dA�$�A� �A�VA���A���A���A�Q�A�1A���A���A�t�A�G�A�{A��TA���A���A���A��+A�|�A�ffA�ZA�I�A�7LA� �A�bA���A��TA���A��-A���A�v�A�C�A�A��;A��wA�v�A��;A��PA�VA���A���A���A�ȴA���A��^A��!A���A�ffA�A��/A��A��PA�~�A�r�A�\)A�;dA�1'A�VA���A�ƨA��jA��A��A���A���A���A��\A��A�bNA�I�A�/A�{A��A��wA�x�A��A�(�A��mA���A�p�A�A�A���A���A���A��!A��hA�Q�A�bA���A��A�ZA�7LA��A�%A���A�I�A��A�A���A�dZA� �A��/A�~�A�A�JA���A��uA�bNA�;dA��;A�v�A�G�A�JA��yA���A��^A�1A��wA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               A�hsA�l�A�n�A�hsA�jA�x�A�z�A�z�A�x�A�~�A�|�A�|�A�z�AρAω7Aω7AϋDAϋDAϋDAύPAύPAύPAϋDAϋDAϋDAω7AρA�|�A�l�A�Q�A�C�A�1'A�+A�-A�(�A��A���A��A��A��yA��mA��`A��A���A���A���A��A��A��yA��TA��#AΛ�Aκ^AάAͰ!Ȁ\A��AȼjA�
=A�A�p�A�Q�A�ZA�bNA���A���A���A��7A�^5A�JA�S�A��-A�jA�ffA�M�A��FA�/A���A��#A�$�A���A�E�A�t�A�?}A�v�A���A�Q�A�z�A��A�JA�XA��RA�oA���A��A�M�A��jA�z�A��\A�p�A�VA���A�`BA��A�1A�C�A��A��A�ȴA�9XAy�As�-Ap$�Am�7Ak;dAg�TAgK�Ag�Af��A_dZAZE�AY?}AX1'AUl�AP �AL�AL�!AK�mAG�#AEO�ACG�A@bA<M�A;�7A;K�A;?}A;&�A:�jA9l�A8�yA8z�A7�-A7�A2=qA.5?A,�A+�7A+��A+�A,1'A,=qA.-A.ĜA-��A-+A,=qA+A+��A+\)A+/A+�A)C�A&A�A$z�A#�^A#K�A"��A"�RA"^5A!�7A�FA&�A5?AdZAC�A�A��A��A?}AoAbNA=qA�AbNAC�A�+A �A�RA�AbNA��A��A��A��A�A�/A�A33A�AZA
~�A��AoA�A�A&�AĜA�A�A�yA�HA��Az�AbA(�A9XAI�AZA;dA��A�A7LA �RA {@���@��@�J@��@�&�@�C�@��`@�dZ@�
=@��@��m@�@���@�J@���@�9X@��
@�^5@�V@�^5@�{@��@��@�1@�|�@�33@�o@�^@�ƨ@�E�@�x�@�G�@�u@�t�@�~�@�@�;d@���@�^5@��@��@��`@��@޸R@�J@���@�(�@۝�@�l�@�33@�o@���@��T@��@�V@���@���@���@��`@ؓu@�+@�^5@�E�@�5?@�5?@�$�@��@���@Ցh@�p�@�G�@��@���@�Ĝ@ԛ�@�z�@�Z@�(�@���@�ƨ@ӍP@�C�@��@�&�@�V@��`@д9@Гu@�j@�A�@�b@���@Ϯ@�t�@�K�@��@Ώ\@�E�@�$�@�{@��T@ͩ�@�`B@�7L@��@̬@�(�@�+@���@ʧ�@�ff@���@�x�@�r�@���@Ǖ�@ǝ�@ǶF@���@ļj@�ƨ@�|�@�33@�ȴ@\@�M�@���@��`@�j@�dZ@�-@�G�@�+@��\@�{@��@���@�9X@�o@���@�$�@���@��h@�G�@��u@��F@�K�@��F@���@�Z@���@���@���@��@�A�@��@���@�@�O�@�Ĝ@�Q�@� �@��@�33@��@��@���@��h@�&�@��D@�Q�@�(�@��@�o@��!@�^5@�hs@� �@��w@�|�@��@���@�M�@�-@�J@��#@�X@���@�Q�@�Q�@�1'@�9X@�r�@�  @�|�@�S�@���@�-@�5?@�J@�@���@�O�@��9@�Z@��@��9@��@��9@��@���@��u@�r�@�9X@�1@���@�t�@�K�@���@���@��^@���@�hs@�Q�@�b@��m@��
@��
@��@���@��P@�S�@�+@��!@��@�&�@���@�r�@��@�  @���@��;@���@���@�l�@��@�v�@�^5@�$�@��@���@���@�X@�%@���@���@�Z@�b@���@�|�@�S�@�"�@��y@��H@��R@�-@��@��#@��7@�G�@��@��j@�z�@�1'@��m@�ƨ@�K�@��@���@�~�@�ff@��@���@���@�x�@�p�@�G�@�V@���@���@��w@�|�@�|�@�|�@�t�@�l�@�S�@�;d@�
=@��!@���@�ff@��@��@��^@���@��7@�p�@�?}@���@��j@���@�r�@�9X@�  @��F@�l�@�;d@��R@�E�@�$�@�{@���@�@��^@���@�`B@�/@��@��/@��9@�j@�Z@�Q�@�I�@�  @��@�dZ@�33@���@�v�@�^5@��@���@�p�@�V@��9@�j@�w@l�@
=@~�+@}p�@}/@|�@{�@{@z^5@zJ@yhs@y&�@xbN@wl�@vv�@vv�@vV@vE�@v@u�-@uO�@t�@t��@tz�@t9X@s�m@s��@s��@s��@s��@s�F@s�F@st�@s"�@r=q@q��@q�^@q��@q�@p1'@o\)@n�y@n��@nE�@m�h@l�j@k�m@k�@k33@j�\@i�#@i��@ix�@iX@i&�@i�@i%@h��@h�u@g��@gl�@f��@f�@f�R@fff@f@e��@d�/@dj@dI�@c��@c33@c33@co@b�@b��@b�\@b^5@bM�@b-@b�@a�@a�^@ahs@a�@`�u@_�@^�R@^ff@^V@^@]�@]?}@\��@\��@\��@\(�@[��@[dZ@Z�\@Y�7@XĜ@X1'@X  @W��@Vv�@U`B@S��@S@Q�#@P��@PQ�@O��@OK�@Nȴ@N�+@M�T@L��@L�@L�D@Lj@LZ@L9X@L�@L�@L1@K�m@Kƨ@K�F@K�@Ko@JM�@IX@HQ�@G�;@G�w@G��@GK�@F�+@F{@E�T@EO�@EV@D�/@D�@D��@Dz�@D1@C�
@C��@Ct�@CC�@B��@B~�@B=q@Ax�@A�@@�9@?�@?+@>�+@>5?@>$�@=�T@=`B@<�@<�D@<z�@<z�@<9X@<(�@<1@<1@<1@;��@;ƨ@;33@:�@:�H@:��@:~�@:J@9��@97L@8��@8r�@7�@7�@7
=@6ȴ@6�R@6��@6V@6{@5�h@5V@4��@4z�@4Z@3�F@2�@2��@2M�@2J@1��@2J@1��@1��@1�#@1�7@0��@/�;@/��@/|�@/K�@/
=@.�y@.��@.v�@.5?@.@-�T@-��@-`B@-O�@-?}@-/@-V@,�@+dZ@+@+@*�@*�@*�H@*��@*-@)��@)��@)�7@)7L@(bN@(1'@( �@(b@(  @'�@'��@'�w@'�@'�P@'�P@';d@'+@';d@'+@'
=@&��@&ȴ@&��@&ff@&E�@&$�@&@%�T@%��@%?}@$�/@$��@$�j@$��@$�j@$�j@$z�@$(�@$�@#�m@#�F@#��@#��@#dZ@"��@"M�@"�@"J@!�#@!�#@!�^@!��@!X@!7L@!&�@!&�@!�@ �`@ �u@�@�w@�w@�w@�P@
=@�R@��@{@�T@�T@��@��@O�@`B@p�@?}@�@��@��@��@��@�/@�@�/@�@z�@z�@I�@�@�F@dZ@o@~�@n�@^5@M�@M�@M�@=q@-@J@�@�^@�7@7L@��@��@��@�9@�u@r�@r�@Q�@A�@A�@1'@b@  @  @b@  @  @��@�@�P@K�@�@�R@�+@5?@{@@�h@O�@/@�@�@��@Z@I�@I�@��@�F@t�@�H@~�@^5@=q@-@-@�@��@��@�^@7L@�`@��@�@bN@Q�@1'@1'@ �@ �@  @  @�@��@K�@+@��@$�@@@��@�@O�@��@z�@�@��@�m@�F@dZ@S�@C�@33@"�@o@
�@
�H@
��@
~�@
M�@
-@
�@
J@	��G�O�A�hsA�jA�dZA�dZA�hsA�hsA�n�A�n�A�t�A�r�A�l�A�dZA�jA�jA�jA�ffA�hsA�ffA�jA�n�A�jA�z�A�|�A�|�A�x�A�z�A�x�A�|�A�|�A�x�A�t�A�v�A�x�A�|�A�~�A�z�A�|�A�~�A�~�A�~�A�|�A�z�A�z�A�z�A�x�A�|�A�~�A�~�A�|�A�z�A�z�A�z�A�~�AρAσAχ+Aχ+Aχ+Aχ+Aχ+AϋDAϋDAϋDAυAχ+Aχ+AϋDAύPAω7Aω7Aω7Aω7AύPAύPAύPAϋDAω7Aω7AϋDAύPAύPAϋDAω7Aω7AύPAϋDAύPAϏ\AϑhAύPAω7Aω7AϋDAϏ\AϏ\AϏ\AύPAω7AύPAϏ\AϏ\AϏ\AύPAύPAϋDAω7Aω7Aω7Aω7AϋDAύPAϏ\AύPAω7Aω7AϋDAϋDAύPAύPAύPAϋDAω7Aω7AϋDAύPAύPAύPAϋDAχ+AυAσAρAρAυAσAρA�|�A�z�A�|�A�~�AρA�|�A�r�A�n�A�n�A�n�A�l�A�ffA�dZA�bNA�XA�Q�A�K�A�K�A�K�A�K�A�K�A�G�A�E�A�=qA�;dA�9XA�9XA�7LA�33A�/A�+A�&�A�&�A�(�A�+A�-A�/A�/A�-A�+A�(�A�+A�-A�-A�-A�+A�(�A�&�A�&�A�$�A�$�A�$�A�$�A�"�A�"�A��A�bA���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��`A��`A��`A��mA��mA��mA��mA��mA��`A��`A��`A��TA��TA��`A��mA��yA��A��A���A���A���A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��mA��`A��`A��TA��`A��`A��mA��mA��mA��mA��mA��`A��TA��HA��HA��HA��;A��HA��HA��HA��TA��`A��`A��HA��;A��/A��/A��/A��#A��;A��;A��/A��#A��#A��
A��
A��
A��A��A��A��A���A���A���A�ȴA�ȴA�ƨA���AξwAξwAθRAΰ!AΣ�AΓuA΍PA�z�A�ffA�dZA�n�A�v�A΁A·+A΋DAΏ\AΑhAΑhAΣ�AΣ�AΣ�AΣ�AΣ�AΩ�AΧ�AΧ�A�ĜA���A���A���A��#A��;A��HA��yA��A��A��A���A���A�%A���A��;A�ĜAξwAξwAΰ!AΛ�AΏ\A΅A�z�A�r�A�n�A�hsA�bNA�ZA�VA�O�A�I�A�A�A�A���AͲ-Aͩ�A͝�A͝�A͝�A͝�A͝�A͟�A͛�A͋DA�z�A�`BA�O�A�;dA�+A�"�A� �A� �A��A�oA�1A���A���A̮A̍PA�~�A�p�A�XA�&�A���A�z�A�VA�1'A��A��
Aʗ�A�jA�ZA�/A�%A���Aɰ!Aɩ�A�JA���A���A��A��yA��/A���A���A�ĜAȮAȅA�x�A�hsA�\)A�G�A�1'A��A�JA�
=A�1A�A���A��yA�A�A��;A���AƮA�`BAŬA�VA���Aĝ�A�bNA��A��AÇ+A��A���A���A���A¥�AA�v�A�M�A�+A�5?A� �A�oA�  A��HA��hA�`BA�oA��yA��#A�ĜA��-A��!A���A��+A�XA��A���A�bNA�/A��DA�l�A�bNA�`BA�`BA�`BA�^5A�S�A�E�A��A��yA���A�r�A��HA�v�A��`A�z�A�7LA��A��!A�~�A�=qA��;A�hsA�-A��A��A���A�ĜA���A�v�A�XA�9XA�
=A��#A��^A�O�A���A��A�ƨA��RA���A���A��7A�n�A�S�A�?}A�33A�+A��A�oA�VA�A��A�ƨA��7A�"�A��/A��^A��uA��A��A�l�A�O�A�"�A��yA�ȴA���A��A�x�A�jA�`BA�1'A�A��-A��A�C�A�=qA�=qA�;dA�/A�"�A�oA��A��wA���A���A���A��A�r�A�Q�A�C�A�9XA�33A�-A�(�A��A�bA�
=A�%A���A��;A���A�x�A�ffA�O�A�"�A��\A�5?A�  A��HA���A��jA���A�bNA�  A��9A��+A�O�A�/A�
=A���A��HA���A��hA�n�A�/A� �A�bA�
=A��HA��9A���A�v�A�XA�+A�VA���A���A���A�1A���A���A�dZA��A���A�`BA�"�A��A���A���A��\A�x�A�l�A�XA�5?A��A���A��HA��wA���A��uA��A�n�A�^5A�S�A�A�A�&�A�VA���A���A��hA�l�A�S�A�7LA�+A��A��A��A�G�A��A�A��A��A�(�A��TA���A��A�jA�VA�;dA�$�A� �A�VA���A���A���A�Q�A�1A���A���A�t�A�G�A�{A��TA���A���A���A��+A�|�A�ffA�ZA�I�A�7LA� �A�bA���A��TA���A��-A���A�v�A�C�A�A��;A��wA�v�A��;A��PA�VA���A���A���A�ȴA���A��^A��!A���A�ffA�A��/A��A��PA�~�A�r�A�\)A�;dA�1'A�VA���A�ƨA��jA��A��A���A���A���A��\A��A�bNA�I�A�/A�{A��A��wA�x�A��A�(�A��mA���A�p�A�A�A���A���A���A��!A��hA�Q�A�bA���A��A�ZA�7LA��A�%A���A�I�A��A�A���A�dZA� �A��/A�~�A�A�JA���A��uA�bNA�;dA��;A�v�A�G�A�JA��yA���A��^A�1A��wA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	�YB	�%B	�%B	�%B	�%B	�%B	�YB	�_B	��B	�YB	��B	��B	�lB	�7B	��B	�=B	��B	��B	��B	��B	�B	�FB	�B	�B	�eB	��B	��B	ܒB	�"B	��B

�B
B
(B
YB
4nB
:^B
?B
@�B
A�B
GEB
XyB
h�B
l"B
v+B
~�B
~�B
�{B
��B
��B
��B
��B%B=�B]dB��B��B��B��B˒B��B�aB��B��B�B�B \B'�B0�B/�B3�B7�B:^B9�B9�B7LB6�B9�B)_B#B2�B5tB)*B"4B�BB�B�PB��B�vB��BΥB��B�	B��Bv`Bh�BYKB�B
�/B
�NB
��B
l�B
FtB
)�B
PB	�yB	�2B	��B	��B	v�B	j�B	h�B	dZB	T�B	S&B	Q�B	MB	?�B	_B	(B	�B	 �B��B�B�B�B�>BںB�jB�]B�?B� B� BѷB��B��B� B��B��B�+B�B	AB�B�"B��B�B��B��B	�B	IRB	{B	�CB	��B	��B	��B	�zB	��B	�B	�B	�nB	��B	�B	�4B	��B	��B	��B	��B	��B	�aB	�B	�XB	�^B	�<B	ȴB	��B	�pB	�TB	� B	�`B	��B	ݘB	��B	��B	�dB	�*B	�0B	�[B	�B	�B	�B	�B	�kB	�'B	��B	�tB	��B	�tB	��B	�B	�JB	|PB	��B	��B	o�B	r�B	{�B	��B	�rB	��B	�(B	�(B	�"B	��B	�_B	��B	�4B	�bB	��B	�9B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�[B	�!B	��B	��B	�CB	�=B	�qB	�IB	�}B	��B	��B	��B	�IB	�}B	�nB	��B	�tB	��B	��B	�wB	��B	��B	��B	��B	�LB	��B	�B	�9B	�-B	�0B	�B	�gB	�B	�zB	�zB	�B	ȴB	�zB	�RB	ɺB	�XB	�#B	�XB	��B	ʌB	�)B	�<B	��B	��B	�B	��B	�pB	��B	ΥB	�[B	�&B	��B	��B	��B	�&B	ӏB	��B	��B	�,B	�aB	�aB	ԕB	��B	��B	�2B	�gB	՛B	՛B	՛B	՛B	�gB	�B	��B	�EB	�yB	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	ںB	ںB	ںB	ںB	��B	�#B	�WB	�WB	�WB	��B	�)B	�/B	��B	��B	یB	��B	�yB	רB	�[B	��B	��B	�aB	�sB	�2B	�,B	��B	ԕB	��B	�[B	ҽB	�&B	�B	�9B	�KB	�9B	��B	�HB	�jB	˒B	�#B	��B	��B	ɆB	�B	�RB	�RB	ɆB	ʌB	�dB	ΥB	�B	ԕB	�B	�]B	��B	�B	��B	�|B	�NB	�B	��B	��B	�TB	�B	�B	�B	��B	�B	�TB	�,B	�,B	�B	�,B	��B	� B	��B	�&B	� B	�B	�2B	�
B	��B	��B	�B	��B	�8B	�2B	�B	�B	�ZB	��B	�mB	�B	�B	�B	�B	��B	�QB	��B	�B	�)B	�B	�ZB	��B	��B	��B	�%B	�ZB	��B	�fB	�	B	�>B	��B	�DB	�xB	�xB	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�PB	�"B	��B	��B	�PB	��B	��B	��B	�]B	�.B	��B	�.B
 �B
�B
uB
�B
�B
�B
�B
�B
B
B
�B
�B
_B
�B
fB
fB
	B
	7B
	�B

rB

rB

�B
~B
�B
PB
"B
VB
\B
.B
.B
.B
�B
�B
�B
�B
B
@B
B
FB
�B
B
�B
B
�B
$B
�B
$B
_B
�B
+B
_B
�B
+B
+B
�B
�B
�B
�B
kB
kB
kB
kB
kB
�B
	B
=B
qB
�B
CB
�B
�B
�B
B
B
~B
�B
B
B
B
�B
�B
!B
�B
!B
 �B
 �B
 �B
 �B
 �B
!�B
!bB
!�B
"hB
#:B
$B
$B
$tB
%B
%B
$�B
$�B
%zB
%�B
&B
&LB
'RB
'RB
'RB
'�B
(XB
($B
(�B
(�B
)�B
*0B
*eB
*eB
*�B
+�B
+6B
,B
-CB
.IB
.�B
.�B
/�B
/�B
0!B
0�B
/�B
/�B
/�B
/�B
/�B
/�B
0!B
0�B
0�B
0�B
0�B
1'B
1[B
1'B
1[B
1'B
1'B
0�B
1'B
1[B
2�B
2-B
2-B
2-B
2�B
3hB
4nB
49B
4�B
4�B
5?B
5tB
5�B
5tB
5�B
6zB
7B
7LB
7B
7LB
7�B
7�B
7�B
7LB
7�B
8�B
8�B
9$B
8�B
8�B
9XB
9�B
9�B
:�B
:�B
:�B
<6B
<6B
<jB
<�B
<�B
<�B
=B
=<B
=B
=<B
=<B
=qB
=qB
=�B
=�B
>�B
@B
@B
@�B
@OB
@�B
@�B
A B
A B
A B
AUB
A�B
A�B
A�B
B�B
C-B
C�B
C�B
CaB
C�B
D�B
F?B
FtB
GB
GzB
H�B
H�B
I�B
I�B
I�B
I�B
JXB
K�B
K^B
K�B
K�B
K�B
K�B
L0B
K�B
L0B
LdB
L�B
LdB
L�B
MjB
M�B
NpB
OBB
O�B
O�B
PB
PB
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S[B
S�B
S�B
S�B
S[B
S�B
T,B
T�B
UgB
U�B
W
B
V�B
VmB
V�B
WsB
XB
X�B
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
YB
Y�B
Y�B
Y�B
ZQB
Z�B
Z�B
ZQB
Z�B
Z�B
ZQB
Z�B
[#B
\)B
\�B
\�B
^B
]dB
\�B
]�B
]�B
]�B
^jB
_pB
_pB
_�B
_pB
aHB
a�B
a�B
a�B
bNB
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
d&B
c�B
c�B
c�B
c�B
d&B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
ffB
ffB
f�B
ffB
gB
g�B
h>B
g�B
g�B
hsB
g�B
h>B
hsB
hsB
h�B
h>B
h�B
iDB
hsB
h�B
iDB
h�B
iyB
h�B
iDB
i�B
i�B
iyB
iDB
i�B
jKB
jB
jB
kB
jB
jKB
j�B
k�B
lWB
k�B
l"B
l�B
l�B
k�B
l"B
lWB
k�B
lWB
l"B
lWB
l"B
l�B
l"B
l�B
l�B
m)B
m]B
l�B
l�B
l�B
m�B
m)B
m]B
m�B
n/B
o B
n/B
ncB
o5B
oiB
o�B
o�B
oiB
oiB
o5B
pB
p�B
p�B
qAB
qvB
q�B
r�B
r|B
rGB
rGB
r|B
r�B
rGB
rB
q�B
qAB
p�B
qAB
q�B
rB
rB
q�B
q�B
q�B
rGB
rGB
rB
r�B
sB
r�B
sB
s�B
s�B
s�B
sB
s�B
u%B
u%B
u%B
u�B
u%B
u�B
v`B
v�B
v�B
w2B
w�B
w�B
x8B
xB
x�B
x�B
x�B
y	B
x�B
y�B
y>B
y�B
y>B
yrB
yrB
y�B
zB
{B
{�B
{�B
{JB
|B
|PB
|PB
}VB
}�B
}�B
}�B
}�B
}�B
~(B
}�B
~(B
}�B
~�B
.B
cB
cB
�B
cB
�4B
�B
�4B
�B
�4B
�B
� B
�4B
�4B
�4B
�B
�oB
�B
��B
�oB
��B
��B
��B
�AB
�AB
�uB
�AB
��B
�uB
��B
��B
��B
��B
��B
��B
��B
��B
�{B
�{B
��B
��B
�B
��B
��B	��B	��B	��B	�YB	��B	��B	�B	��B	�+B	�_B	��B	�_B	�B	�B	�SB	��B	��B	�_B	��B	��B	�1B	�YB	��B	�B	�YB	��B	��B	�B	�B	��B	��B	��B	�%B	�B	��B	�B	�1B	�+B	��B	��B	�%B	��B	��B	�+B	�+B	��B	��B	��B	��B	�%B	��B	��B	�SB	�_B	�YB	��B	�lB	�lB	��B	��B	�B	�fB	�7B	��B	�	B	�	B	�7B	��B	��B	�DB	�	B	�xB	��B	�lB	��B	�lB	�DB	�=B	�	B	�rB	�=B	�B	�7B	�B	�~B	��B	��B	��B	�B	��B	�hB	��B	�.B	�(B	�(B	��B	��B	�4B	�bB	��B	��B	�\B	�:B	�B	��B	�uB	�B	�B	��B	�uB	��B	�:B	�4B	��B	�B	��B	��B	�{B	�B	��B	��B	�$B	�_B	��B	��B	��B	��B	��B	��B	�B	�RB	�eB	��B	��B	��B	�qB	��B	��B	��B	��B	�wB	��B	��B	��B	��B	��B	��B	ǮB	�^B	�pB	רB	��B	�BB	�|B	�B	�,B	��B	�>B	�B	�B	��B	�+B	�B	�B	�cB
B
�B
	�B

�B

�B

=B
	�B
�B
B
�B
�B
"B
�B
�B
JB
B
�B
\B
�B
�B
hB
uB
{B
�B
�B
�B
+B
 �B
,�B
5�B
8�B
8�B
8�B
8B
7�B
9XB
9�B
;0B
;�B
<B
<�B
=�B
@OB
@B
?�B
?}B
?HB
?B
?}B
@OB
A B
B'B
B'B
A�B
A�B
A B
A B
@�B
A�B
CaB
C�B
FB
I�B
J�B
K^B
M6B
N�B
QB
X�B
e,B
ffB
g�B
h
B
h�B
iyB
j�B
h�B
iDB
i�B
jKB
kQB
m�B
m�B
qvB
rGB
t�B
v�B
zB
{�B
}�B
~�B
~�B
~�B
~�B
~(B
}�B
}�B
~(B
~�B
.B
�B
cB
��B
�B
�4B
��B
��B
�AB
�B
�MB
�B
�YB
��B
��B
��B
�7B
�B
��B
��B
��B
��B
�	B
�=B
�xB
�B
��B
�"B
�VB
�VB
��B
��B
��B
�B
�B
��B
��B
�bB
��B
��B
�@B
�B
��B
�B
�{B
�B
��B
�+B
�+B
�+B
��B
��B
�1B
��B
�CB
��B
�VB
��B
��B
��B
��B
�~B
�OB
��B
�@B
�kB
��B
�9B
�0B
�3B
��B
רB
�vB
�,B
�`B
�fB
�mB
�sB
�B
�iB
�5B
��B
�B
�B
��B
�B
�B�BoBB
��B  BBuB�B�B�B�B BhB~B"hB($B+�B,B1'B0!B0!B0�B0�B0!B.B.�B-�B.}B.B.�B.IB.BB�B@B?HBA B>�B?HB=�B=B<�B;�B<6B?}BB�BFBG�BJ�BK�BMBL�BLdBMjBOBBO�BQ�BY�B^5Bb�B`B`vBd�BiDBt�BuZBw�ByrB}"B~�B�_B�1B�=B��B�oB��B�YB��B�B�:B�:B��B�tB��B�FB�B��B��B�qB�_B��B��B�}B�OB��B��B�}B�wB�wB��B��B�B�)B�B��B��BŢBŢB��BĜB�gB�BB��B�3B�BB�vB�XB�KB�#B�BбB��BбB�jB��B��B˒B�)B�,B�}B��B�/BרB��B��B��B�mB�B�B�pB��B�B�B�B 4B�;B�B�/B��B��B�B�]B�B�GB�ZB��B�B�lB+B;�B�B�B�B�B�B7B!-B$tB*eB�B�B�B"�B"�B'B)_B'�B.B*�B,qB;�B4B/�B/B.�B/B-�B/�B0UB1'B/�B/�B/OB0UB/�B.}B/�B/�B5�B6�B<jB:�B9�B5�B5�B2�B5�B9$B;�B<jB:^B<jB9�B7�B7LB5B<jBPHB7B;�BE�B9$B7B7�B4nB6FB9�B=�B<B9�B8�B8B8�B8�B8�B8RB7�B7�B7�B7B7�B7LB4�B3�B2�B8�B9�B9XB5�B5�B:�BFtB@�B5?B5B4nB4B7B7�B=B.�B,�B+�B(�B)*B&�B%B&B%�B$tB!�B �B 'BOB*eB#B$B&LB%FB)�B#nB#B*0BPBH�B<6B7�BD�B7�BC-B8B2�B/�B,qB+�B-CB(�B(�B(�B,�B(�B&B%B&�B$�B#:B"4B"4B�BVB�B 'B�BIB �B�B�BB�B�BSBB~B!B�B�B�B�B�BDB	BB iB�BoB�(B�PB�(B�cB�B��B�B�	B�lB�B�%B��B�cB�WB�DB�`B��B��B�|B� B�B�B�/B�B�]B��B�KB�yB�?B��B�&B�?B�TB�RBȀB�#B� B	lB��B�_B��B�4B��B��B��B��B��B��B�'B�FB��B��B��B�1B}�B��B��ByrB�BsBv�Bu�Bo�Bp�Bm�BpoBi�Bh
BsBgmB]/BXB[WBU�BR�BQ�Bc�B�B�BCBSBMB
�DB
�B
�B
�]B
�cB
�`B
��B
��B
یB
�[B
�B
�XB
֡B
ŢB
��B
�FB
�bB
�	B
�:B
�:B
�B
��B
�iB
e�B
e,B
[WB
U�B
aHB
V�B
K^B
GzB
=B
<jB
8�B
A B
-B
,�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               B	�mB	��B	��B	�B	��B	�%B	�?B	�?B	�?B	�zB	��B	�tB	��B	��B	�lB	�7B	��B	�=B	��B	��B	��B	��B	�&B	�aB	�9B	�]B	�B	�'B	�uB	��B	�B	�B

�B
6B
�B
_B
49B
:xB
?.B
@�B
A�B
G+B
XEB
hsB
lWB
vFB
~�B
~�B
��B
��B
��B
��B
�LB*�BE9Bj�B�+B�B�0B�B� B҉BڠB�B�BtB"hB'�B/iB4�B4B;JB=�B@�B=VB<�B;JBAoBC�B0�B.BCGB<�B.IB'�B$�B �B�BB�vB��B��BۦB��B��B��Bx�BpBkB�B
��B
��B
��B
x8B
RoB
8�B
�B	�B	�\B	��B	��B	��B	s�B	p�B	n�B	V�B	T,B	T�B	c�B	O�B	�B	uB	B	�B	�B�`B�B��B�B�NB�B��B�B�B�oB�:BөBچB�B�`B��B��B	'B	�B�B�B�B�B��B�"B	�B	G�B	~]B	�OB	��B	�?B	��B	�LB	�LB	��B	��B	��B	�$B	��B	��B	�B	��B	�8B	��B	��B	��B	�fB	�B	��B	�jB	��B	�CB	��B	�&B	�B	�LB	��B	�LB	�B	�UB	�VB	�}B	��B	�fB	��B	�LB	�B	�2B	��B	�%B	�ZB	��B	�:B	��B	�mB	�1B	�:B	}qB	��B	��B	q�B	r�B	{B	��B	��B	�~B	�.B	��B	��B	�uB	�+B	�B	��B	�\B	�oB	�ZB	��B	�B	�%B	�9B	�IB	�B	��B	�B	�	B	�B	�'B	�TB	��B	��B	��B	��B	�oB	��B	��B	�!B	��B	�cB	�5B	�B	��B	��B	��B	�8B	��B	�iB	�[B	��B	��B	��B	�B	�B	��B	�vB	��B	��B	�9B	��B	�B	�B	�0B	ɠB	��B	�xB	�B	�^B	ʌB	��B	�B	�)B	��B	ϑB	�B	��B	�B	��B	οB	ϫB	�NB	��B	�[B	�B	�B	�&B	ӏB	��B	�,B	�FB	�{B	ԯB	ԯB	��B	�B	�MB	ՁB	��B	�B	�B	�B	�SB	׍B	ٴB	�B	ؓB	��B	�1B	�1B	�eB	��B	ٴB	�B	�QB	�QB	��B	�qB	�=B	�	B	��B	�#B	�WB	ۦB	ۦB	��B	��B	��B	��B	�B	�CB	�CB	ܬB	��B	�kB	��B	��B	��B	��B	�mB	�	B	��B	��B	ԕB	�MB	�FB	��B	��B	��B	�
B	�EB	�qB	�+B	�eB	уB	�VB	�B	��B	�xB	�B	�rB	�	B	�	B	ɺB	�#B	��B	�B	�\B	�\B	�B	�YB	��B	��B	�B	�-B	�B	�B	�FB	�&B	�ZB	�ZB	�TB	�B	�B	��B	�B	��B	��B	�`B	�`B	�LB	�tB	�B	��B	�FB	��B	�tB	�$B	�eB	��B	�FB	�fB	�B	��B	�B	��B	�B	�`B	�8B	�
B	�B	�QB	�B	�0B	��B	�WB	�eB	�B	�B	�B	��B	��B	�B	��B	�FB	��B	�nB	�fB	�	B	�>B	��B	�^B	��B	��B	�0B	�B	�6B	�jB	��B	��B	�(B	�(B	��B	�"B	�]B	��B	�B	��B	�jB	��B	��B	��B	��B	��B	�}B
 iB
�B
GB
GB
�B
�B
�B
�B
B
mB
�B
�B
zB
�B
B
�B
�B
	�B
	�B

rB

�B

�B
�B
B
jB
�B
pB
�B
�B
HB
�B
4B
:B
 B
oB
&B
uB
�B
�B
�B
MB
gB
�B
�B
�B
�B
?B
B
�B
EB
�B
yB
EB
�B
yB
�B
QB
WB
�B
kB
�B
�B
�B
�B
=B
�B
qB
�B
xB
�B
B
B
B
IB
~B
B
OB
jB
jB
�B
B
VB
�B
�B
 'B
!bB
!HB
 �B
 �B
!bB
!�B
!�B
"NB
"�B
#�B
$@B
$ZB
$�B
%,B
%,B
$�B
%FB
&B
&LB
&�B
'RB
'�B
'�B
'�B
(�B
(�B
(�B
)DB
)�B
*�B
*B
*�B
+B
+�B
+�B
+�B
-)B
-�B
.�B
/5B
/�B
/�B
0UB
1B
1[B
/�B
/�B
/�B
/�B
0;B
0!B
0�B
0�B
0�B
0�B
1AB
1[B
1[B
1'B
1[B
1'B
1'B
1AB
1�B
2-B
2�B
2GB
2aB
2�B
3�B
49B
4�B
4�B
5B
5ZB
6B
6FB
6B
5�B
6FB
72B
7fB
7fB
72B
7�B
7�B
7�B
7�B
7�B
8lB
9$B
8�B
9>B
9$B
9>B
9�B
:*B
:�B
;0B
;0B
;�B
<�B
<6B
<�B
<�B
<�B
=B
=<B
=VB
="B
=VB
=qB
=�B
=�B
>BB
>wB
@B
@�B
@iB
@�B
@�B
A;B
A;B
AoB
AUB
AUB
A�B
B'B
BB
B[B
C�B
C�B
D�B
C�B
C�B
EB
E�B
G�B
GB
H1B
H�B
I7B
IlB
J	B
J	B
J	B
J=B
K^B
K�B
KxB
K�B
K�B
K�B
K�B
L0B
LB
LJB
L~B
L�B
L�B
MB
N<B
N�B
O\B
O�B
O�B
PB
P}B
P�B
QNB
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R:B
R:B
R�B
R�B
R�B
S@B
TB
TaB
TFB
S�B
S�B
TFB
T�B
UgB
U�B
U�B
W?B
V�B
V�B
V�B
WsB
X+B
X�B
Y�B
ZB
Y�B
Y�B
ZB
ZQB
ZB
ZB
Y�B
Z7B
ZkB
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
Z�B
[	B
[�B
\CB
\�B
]�B
^�B
]�B
]dB
]�B
]�B
]�B
^�B
_pB
_�B
`BB
`\B
a�B
b4B
a�B
bB
b�B
b�B
cB
c B
c:B
c�B
c�B
c�B
d&B
d@B
dB
dB
d&B
dZB
e`B
ffB
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
g�B
h
B
hXB
g�B
g�B
h�B
g�B
hXB
h�B
h�B
h�B
h�B
h�B
i*B
h�B
h�B
i_B
h�B
i�B
h�B
i_B
i�B
i�B
i�B
i_B
jB
j�B
j�B
j�B
kB
j�B
jeB
kB
l=B
lqB
k�B
lWB
l�B
l�B
lB
l�B
l�B
k�B
lqB
lWB
lWB
l=B
l�B
lqB
l�B
l�B
m)B
mwB
m)B
l�B
m]B
m�B
m)B
m]B
m�B
n�B
oOB
nIB
n�B
oiB
oiB
o�B
o�B
o�B
oiB
o5B
p;B
p�B
p�B
qAB
qvB
q�B
r�B
r|B
raB
r|B
r�B
r�B
r|B
rGB
rB
q�B
qAB
q�B
q�B
r-B
r-B
q�B
q�B
q�B
raB
raB
rGB
r�B
sMB
s3B
shB
s�B
s�B
s�B
s3B
s�B
u%B
u?B
u?B
u�B
u?B
vB
vzB
v�B
v�B
wLB
w�B
xB
xRB
xB
x�B
x�B
y>B
y>B
y$B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z^B
{B
{�B
{�B
{�B
|jB
|�B
|�B
}�B
~B
}�B
~B
}�B
}�B
~BB
~(B
~BB
~wB
B
}B
}B
}B
�B
}B
�4B
�B
�4B
�B
�4B
�B
�OB
��B
�iB
��B
��B
��B
�UB
��B
��B
��B
��B
�[B
��B
�[B
��B
�uB
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
��B
��B
�3B
��G�O�B	��B	��B	��B	�YB	��B	��B	�B	��B	�+B	�_B	��B	�_B	�B	�B	�SB	��B	��B	�_B	��B	��B	�1B	�YB	��B	�B	�YB	��B	��B	�B	�B	��B	��B	��B	�%B	�B	��B	�B	�1B	�+B	��B	��B	�%B	��B	��B	�+B	�+B	��B	��B	��B	��B	�%B	��B	��B	�SB	�_B	�YB	��B	�lB	�lB	��B	��B	�B	�fB	�7B	��B	�	B	�	B	�7B	��B	��B	�DB	�	B	�xB	��B	�lB	��B	�lB	�DB	�=B	�	B	�rB	�=B	�B	�7B	�B	�~B	��B	��B	��B	�B	��B	�hB	��B	�.B	�(B	�(B	��B	��B	�4B	�bB	��B	��B	�\B	�:B	�B	��B	�uB	�B	�B	��B	�uB	��B	�:B	�4B	��B	�B	��B	��B	�{B	�B	��B	��B	�$B	�_B	��B	��B	��B	��B	��B	��B	�B	�RB	�eB	��B	��B	��B	�qB	��B	��B	��B	��B	�wB	��B	��B	��B	��B	��B	��B	ǮB	�^B	�pB	רB	��B	�BB	�|B	�B	�,B	��B	�>B	�B	�B	��B	�+B	�B	�B	�cB
B
�B
	�B

�B

�B

=B
	�B
�B
B
�B
�B
"B
�B
�B
JB
B
�B
\B
�B
�B
hB
uB
{B
�B
�B
�B
+B
 �B
,�B
5�B
8�B
8�B
8�B
8B
7�B
9XB
9�B
;0B
;�B
<B
<�B
=�B
@OB
@B
?�B
?}B
?HB
?B
?}B
@OB
A B
B'B
B'B
A�B
A�B
A B
A B
@�B
A�B
CaB
C�B
FB
I�B
J�B
K^B
M6B
N�B
QB
X�B
e,B
ffB
g�B
h
B
h�B
iyB
j�B
h�B
iDB
i�B
jKB
kQB
m�B
m�B
qvB
rGB
t�B
v�B
zB
{�B
}�B
~�B
~�B
~�B
~�B
~(B
}�B
}�B
~(B
~�B
.B
�B
cB
��B
�B
�4B
��B
��B
�AB
�B
�MB
�B
�YB
��B
��B
��B
�7B
�B
��B
��B
��B
��B
�	B
�=B
�xB
�B
��B
�"B
�VB
�VB
��B
��B
��B
�B
�B
��B
��B
�bB
��B
��B
�@B
�B
��B
�B
�{B
�B
��B
�+B
�+B
�+B
��B
��B
�1B
��B
�CB
��B
�VB
��B
��B
��B
��B
�~B
�OB
��B
�@B
�kB
��B
�9B
�0B
�3B
��B
רB
�vB
�,B
�`B
�fB
�mB
�sB
�B
�iB
�5B
��B
�B
�B
��B
�B
�B�BoBB
��B  BBuB�B�B�B�B BhB~B"hB($B+�B,B1'B0!B0!B0�B0�B0!B.B.�B-�B.}B.B.�B.IB.BB�B@B?HBA B>�B?HB=�B=B<�B;�B<6B?}BB�BFBG�BJ�BK�BMBL�BLdBMjBOBBO�BQ�BY�B^5Bb�B`B`vBd�BiDBt�BuZBw�ByrB}"B~�B�_B�1B�=B��B�oB��B�YB��B�B�:B�:B��B�tB��B�FB�B��B��B�qB�_B��B��B�}B�OB��B��B�}B�wB�wB��B��B�B�)B�B��B��BŢBŢB��BĜB�gB�BB��B�3B�BB�vB�XB�KB�#B�BбB��BбB�jB��B��B˒B�)B�,B�}B��B�/BרB��B��B��B�mB�B�B�pB��B�B�B�B 4B�;B�B�/B��B��B�B�]B�B�GB�ZB��B�B�lB+B;�B�B�B�B�B�B7B!-B$tB*eB�B�B�B"�B"�B'B)_B'�B.B*�B,qB;�B4B/�B/B.�B/B-�B/�B0UB1'B/�B/�B/OB0UB/�B.}B/�B/�B5�B6�B<jB:�B9�B5�B5�B2�B5�B9$B;�B<jB:^B<jB9�B7�B7LB5B<jBPHB7B;�BE�B9$B7B7�B4nB6FB9�B=�B<B9�B8�B8B8�B8�B8�B8RB7�B7�B7�B7B7�B7LB4�B3�B2�B8�B9�B9XB5�B5�B:�BFtB@�B5?B5B4nB4B7B7�B=B.�B,�B+�B(�B)*B&�B%B&B%�B$tB!�B �B 'BOB*eB#B$B&LB%FB)�B#nB#B*0BPBH�B<6B7�BD�B7�BC-B8B2�B/�B,qB+�B-CB(�B(�B(�B,�B(�B&B%B&�B$�B#:B"4B"4B�BVB�B 'B�BIB �B�B�BB�B�BSBB~B!B�B�B�B�B�BDB	BB iB�BoB�(B�PB�(B�cB�B��B�B�	B�lB�B�%B��B�cB�WB�DB�`B��B��B�|B� B�B�B�/B�B�]B��B�KB�yB�?B��B�&B�?B�TB�RBȀB�#B� B	lB��B�_B��B�4B��B��B��B��B��B��B�'B�FB��B��B��B�1B}�B��B��ByrB�BsBv�Bu�Bo�Bp�Bm�BpoBi�Bh
BsBgmB]/BXB[WBU�BR�BQ�Bc�B�B�BCBSBMB
�DB
�B
�B
�]B
�cB
�`B
��B
��B
یB
�[B
�B
�XB
֡B
ŢB
��B
�FB
�bB
�	B
�:B
�:B
�B
��B
�iB
e�B
e,B
[WB
U�B
aHB
V�B
K^B
GzB
=B
<jB
8�B
A B
-B
,�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<b.�<#�
<#�
<E�<k��<#�
<#�
<#�
<(�q<#�
<��<U�b<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<7�I<-(�<#�
<<�<���<#�
<#�
<#�
<#�
<4�<#�
<#�
<#�
<#�
<.��<ZM<#�
<#�
<#�
<#�
<#�
<��S<#�
<#�
<��U<���<Bϫ<J��<r�<P�<#�
<>%l<�t�<�;@<J��<#�
<#�
<0;�<#�
<#�
<#�
<�<�<��7<#�
<#�
</�<�%�<.",<#�
<#�
<[m�<#�
<#�
<=�U<Pg�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��7<S��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
</�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2019011518213420190115182134IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019012518004720190125180047QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019012518004720190125180047QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107544920190521075449IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                