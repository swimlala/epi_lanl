CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-11-17T23:38:22Z creation; 2023-04-26T19:14:27Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181117233822  20230426191427  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_031                 7316_008644_031                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @ؑv    @ؑv    11  @ؑv8�YK@ؑv8�YK@*gֶZ��@*gֶZ���c����@�c����@11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��?��H@@  @��\@�G�@�p�@�  A   A  A ��A,(�A?\)A_\)A�Q�A�  A�Q�A�Q�A�  A�  A߮A�\)B   B  B  B  B�
B(  B0  B7�B?�
BG�
BP  BX  B_�
Bh  Bp(�Bx��B�
B��B��B�  B�(�B�  B��B�  B�{B�  B��B��
B��
B��B�{B�{B�{B�{B��B��B��B�  B�  B��B�  B��B��B��B�  B�  B��B��C   C{C
=C
=C
=C
  C  C  C  C
=C
=C  C  C��C��C  C 
=C"{C$
=C&  C(
=C)��C,  C.  C/��C2  C3�C5�C7��C:  C<{C>{C@  CB  CD{CF
=CG��CI�CK��CN  CP
=CR
=CT
=CV  CX
=CZ{C\  C^  C`  Cb
=Cd
=Cf
=Ch{Cj  Ck�Cm�Co�Cq�Cs�Cu��Cw��Cy�C{�C~  C�  C���C���C���C���C�  C�  C���C�C�
=C�C�  C�C�\C�C�  C���C�C�C�C�  C�  C���C���C�  C�C�  C�  C�  C�  C�C���C���C���C���C���C�  C�  C�C�  C���C�  C�  C���C�  C�C���C�C�C���C�  C�  C�  C�C�  C���C���C�  C�  C���C�  C�C�  C�  C�C�C�  C�  C�C�  C���C�  C�C�C�C�
=C�  C���C�C�C�  C�  C�C�C�C�C�  C���C�  C�C�  C�  C�  C�C�  C�  C�  C���C���C�  C�C�C�  C�  C�  C�  C�  C�C�C�  C���C�  C�C�C�  C�C�  C�  C�C�C���C���C�C�  C���C�  C�  C���D   D � D  D� D  D}qD�qD��D�D��DD��D  D��D�qD}qD�qD� D	  D	� D	�qD
}qD
�qD}qD  D��D  D� D�D��D�D� D  D� D�D��DD� D�qD� D�D� D�qD��D�D��D�qDz�D�qD}qD�qD��D  D� D  D}qD  D� D�D��DD� D�qD� D �D ��D!�D!z�D!�qD"� D#�D#�D$�D$� D%  D%}qD&  D&��D'  D'}qD(  D(� D)�D)�D*�D*��D+�D+��D,�D,� D-�D-�D.�D.��D/  D/� D/�qD0}qD0�qD1}qD1�qD2}qD3  D3� D4�D4� D4�qD5� D6�D6� D6�qD7��D8�D8��D9�D9��D:�D:}qD;  D;� D<�D<�D=  D=}qD=�qD>� D>�qD?� D@D@��DA  DA}qDB  DB��DC  DC}qDC��DD� DE  DE}qDF�DF� DG  DG� DH  DH� DI  DI� DI�qDJ� DK�DK}qDK��DL}qDM  DM��DNDN� DN�qDO� DP�DP��DQ  DQ� DR  DR� DR��DS}qDT�DT��DU�DU� DU�qDV� DW�DW�DX�DX� DY  DY��DZ  DZ� D[  D[��D\  D\� D]  D]� D^  D^��D_  D_� D`  D`�Da�Da� Db  Db��Dc  Dc}qDd�Dd��De  De��Df  Df}qDf�qDg� Dh�Dh��Di  Di� Dj�Dj��Dk�Dk� Dk�qDl}qDl�qDm� Dn  Dn}qDn�qDo}qDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�qDu� Dv  Dv}qDw  Dw��Dx�Dx��Dy  Dy��DzDz� D{  D{� D|�D|��D}  D}}qD}��D~}qD  D� D�HD�@ D�� D���D�  D�AHD�� D�� D���D�=qD�~�D�� D�  D�>�D�� D��HD�  D�@ D�~�D���D�  D�AHD��HD���D���D�@ D�~�D���D�HD�AHD��HD�� D��qD�@ D��HD�� D�  D�>�D�� D��HD�HD�@ D�� D���D���D�@ D�� D��HD�  D�>�D�� D�� D���D�AHD��HD���D���D�>�D�� D��HD��D�AHD�� D�� D���D�@ D�~�D��qD���D�AHD��HD��HD�HD�>�D�� D��HD�HD�@ D�~�D��HD��D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�@ D�~�D���D�  D�@ D��HD���D��qD�>�D��HD��HD�  D�@ D�~�D���D�HD�@ D�~�D��HD�  D�@ D�~�D�� D�  D�>�D�~�D���D�  D�>�D�� D�� D�  D�AHD�~�D���D���D�@ D�� D�� D�  D�=qD�~�D�� D���D�@ D��HD�� D�HD�B�D�� D��HD�HD�@ D�~�D�� D�HD�@ D�~�D��HD��D�AHD�� D���D�  D�AHD��HD��HD�HD�@ D�~�D��qD���D�@ D��HD�� D���D�AHD�� D�� D���D�@ D���D��HD�  D�>�D�� D�D�  D�>�D�~�D��HD�  D�>�D�}qD���D�HD�>�D�� D�� D���D�@ D�� D���D���D�>�D�~�D�� D�HD�>�D�� D�D�  D�=qD�}qD�� D��D�AHD�~�D�� D�  D�@ D��HD��HD�HD�B�D���D�D�HD�>�D�� D��HD�  D�>�D�� D���D���D�@ D��HD��HD�  D�@ D�� D��HD�  D�>�D�~�D���D���D�@ D�� D�� D�  D�@ D�~�D��HD�HD�>�D�� D�� D�HD�@ D�� D���D���D�@ D�� D��HD�HD�AHD��HD�� D�  D�AHD D¾�D���D�>�D�~�Dþ�D���D�@ D�~�D�� D�  D�@ D�~�Dž�D���D�@ Dƀ D�� D��D�B�DǁHD�� D�  D�@ DȁHD�� D���D�@ Dɀ Dɾ�D���D�>�Dʀ D�� D�  D�AHDˀ D˾�D�HD�B�D́HD�� D�HD�>�D̀ D�� D���D�>�D΀ DνqD���D�@ D�~�D��HD�  D�=qDЀ D�D��D�@ D�~�DѾ�D���D�@ DҀ DҾ�D���D�>�D�~�D�� D�HD�AHD�~�D�� D�  D�@ D�~�Dվ�D�  D�@ D�~�D�� D�  D�AHDׁHD�� D�  D�AHD؀ Dؾ�D���D�=qD�~�D��HD�  D�>�Dڀ Dھ�D�  D�@ D�~�D��HD�  D�@ D܀ D�� D�  D�>�D݁HD�D�  D�>�D�~�D�� D�  D�>�D߀ D��HD�  D�>�D�� D�D�  D�=qD� D��HD�  D�@ D�~�D�qD�  D�B�DわD�� D�  D�B�D� D侸D���D�@ D�~�D�qD��qD�>�D� D�� D���D�AHD� D�� D���D�>�D� D辸D�  D�@ D� D�� D�  D�B�D�HD��HD�HD�@ D� D뾸D���D�>�D�~�D�� D�  D�>�D�}qD��qD���D�@ D�~�DD�  D�AHD�HD��HD�  D�>�D�� D��HD�  D�>�D� D��HD�  D�>�D�~�D�� D�  D�>�D�HD�� D�  D�@ D� D�� D���D�=qD�}qD��qD�  D�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�~�D�� D���D�>�D�~�D�� D�>�?B�\?��?�{?���?��@��@#�
@8Q�@E�@\(�@u@�ff@�{@��H@���@�33@�p�@���@�@�\@�{@���A�\AQ�A�RA�A�HA   A'
=A,��A2�\A7�A=p�AC33AI��AP  AU�AZ=qA`��Ag
=Amp�Ar�\Aw�A}p�A��A���A��A�=qA���A�  A��HA�A���A��A��RA�=qA���A��A��HA�{A�G�A�(�A�
=A��A��A�Q�A�33A�{Aȣ�A˅A�ffA��A�z�A�
=A�=qA�p�A��A�A�ffA�G�A�z�A�A�=qA��A�  A�33A�B   BG�B�\B�B��B��B�RB�Bz�B	G�B
=qB33BQ�B�B�B
=B  B��B��B�\B�B��Bp�B{B
=B  B��B�B�HB�Bz�Bp�B=qB
=B�
B!�B!�B"�RB#�B$z�B%p�B&=qB'33B(  B(��B)�B*�RB+\)B,(�B-�B.{B.�HB/�B0(�B1�B2{B2�HB3�B4Q�B5G�B6=qB7
=B7�
B8��B9�B:�HB;�B<z�B=��B>�RB?�
B@��BABB�HBD(�BEG�BF=qBG33BHQ�BIp�BJ�\BK�BLz�BMp�BN�\BO�
BP��BR{BS
=BT  BT��BV{BW\)BXz�BY��BZffB[�B\z�B]B^�HB`  Ba�Ba�Bc
=Bd(�Bep�BfffBg�Bh��Bi��Bj�\Bk�Bl��Bn{Bo33Bp(�Bq�Br=qBs33BtQ�Bt��Bu�Bv�\Bw
=Bw�Bx  Bxz�By�Byp�Bz{BzffBz�HB{33B{�B|  B|Q�B|��B}p�B}�B~�\B~�RB33B�B�{B�Q�B���B��HB�33B�p�B�B�  B�=qB�z�B��RB���B�G�B���B��B�=qB�z�B���B�
=B�\)B���B��
B�(�B�z�B���B�33B��B��
B�=qB��\B��HB�33B�p�B��
B�(�B�z�B��HB�G�B���B�  B�ffB���B�33B���B��B�Q�B��RB�
=B�p�B��
B�(�B�z�B���B�33B���B�  B�ffB��RB�33B��B�  B�ffB��HB�G�B��B�(�B��\B�
=B��B��B�ffB���B�G�B�B�(�B��RB��B��B�{B���B��B���B�{B��\B�
=B���B�  B�z�B�
=B��B�  B��\B�
=B��B�  B��\B���B��B�{B�z�B���B���B�  B��\B�
=B��B�{B��\B�
=B��B�{B��\B�
=B���B�  B��\B�
=B��B�  B��\B�
=B��B�  B�z�B�
=B�p�B��B�z�B���B��B�  B�z�B���B�p�B�  B�z�B���B��B�  B�z�B���B��B��B�z�B���B��B�  B��\B���B��B�  B�z�B���B�p�B�  B�z�B���BÅB�  Bď\B�
=BŅB�{Bƣ�B��BǮB�(�BȸRB�G�B�B�Q�B���B�\)B��
B�Q�B���B�\)B��B�ffB���B�p�B��B�ffB��HB�\)B��
B�Q�B���B�G�B��
B�Q�B���B�G�B�B�=qB֣�B��Bי�B�(�Bأ�B�33B�B�=qB���B�\)B��
B�ffB��HB�\)B��
B�Q�B��HB�p�B��B�z�B�
=BᙚB�(�B��B�33B�B�=qB�RB�G�B�B�=qB���B�\)B��B�z�B�
=B�B�(�B��B��B뙚B�{B��B�33B�B�Q�B��HB�p�B�  B��B�33B�B�=qB���B�G�B��
B�ffB���B���B�(�B��RB�G�B��
B�Q�B��HB�p�B�  B���B�G�B��
B�z�B���B��B�{B���B�33B��
C =qC �\C �
C�CffC�C��C=qC�\C�HC33C�CC
=C\)C��C�C33C�C�
C(�Cp�C�RC  CG�C��C�C33Cz�CC	
=C	Q�C	��C	��C
=qC
�\C
��C{CQ�C��C�HC33C�C�
C(�Cp�C�RC  CG�C�\C�HC33C�C��C{CQ�C��C�C33C�\C�
C{CffC��C�C=qC��C�C33Cz�C�RC
=C\)C�C  CQ�C�\C�HC33C�\C�HC(�Cp�C�RC
=C\)C�C��CG�C�\C�
C�Cz�C�
C�Cp�C�RC  CQ�C�C
=C\)C��C��CG�C��C��C Q�C ��C ��C!=qC!�\C!�HC"33C"�\C"�HC#33C#p�C#��C$�C$z�C$��C%{C%\)C%��C&  C&Q�C&�C'  C'G�C'��C'�C(G�C(��C(�C)33C)�\C)�C*=qC*�C*�
C+33C+�C+��C,�C,z�C,�
C-�C-ffC-C.�C.p�C.�RC/
=C/p�C/C0{C0\)C0�RC1{C1p�C1�RC2
=C2ffC2C3�C3ffC3�RC4�C4p�C4C5�C5z�C5�
C633C6�C6�
C733C7�\C7�C833C8�\C8�C9G�C9��C9�C:=qC:��C;  C;G�C;��C<  C<\)C<�C=  C=\)C=C>�C>ffC>�RC?{C?p�C?C@
=C@p�C@CA{CAffCA��CB�CBp�CBCC�CCz�CC�
CD�CDz�CD�
CE33CE�CE�
CF=qCF��CF�HCG=qCG��CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                    ?��?��H@@  @��\@�G�@�p�@�  A   A  A ��A,(�A?\)A_\)A�Q�A�  A�Q�A�Q�A�  A�  A߮A�\)B   B  B  B  B�
B(  B0  B7�B?�
BG�
BP  BX  B_�
Bh  Bp(�Bx��B�
B��B��B�  B�(�B�  B��B�  B�{B�  B��B��
B��
B��B�{B�{B�{B�{B��B��B��B�  B�  B��B�  B��B��B��B�  B�  B��B��C   C{C
=C
=C
=C
  C  C  C  C
=C
=C  C  C��C��C  C 
=C"{C$
=C&  C(
=C)��C,  C.  C/��C2  C3�C5�C7��C:  C<{C>{C@  CB  CD{CF
=CG��CI�CK��CN  CP
=CR
=CT
=CV  CX
=CZ{C\  C^  C`  Cb
=Cd
=Cf
=Ch{Cj  Ck�Cm�Co�Cq�Cs�Cu��Cw��Cy�C{�C~  C�  C���C���C���C���C�  C�  C���C�C�
=C�C�  C�C�\C�C�  C���C�C�C�C�  C�  C���C���C�  C�C�  C�  C�  C�  C�C���C���C���C���C���C�  C�  C�C�  C���C�  C�  C���C�  C�C���C�C�C���C�  C�  C�  C�C�  C���C���C�  C�  C���C�  C�C�  C�  C�C�C�  C�  C�C�  C���C�  C�C�C�C�
=C�  C���C�C�C�  C�  C�C�C�C�C�  C���C�  C�C�  C�  C�  C�C�  C�  C�  C���C���C�  C�C�C�  C�  C�  C�  C�  C�C�C�  C���C�  C�C�C�  C�C�  C�  C�C�C���C���C�C�  C���C�  C�  C���D   D � D  D� D  D}qD�qD��D�D��DD��D  D��D�qD}qD�qD� D	  D	� D	�qD
}qD
�qD}qD  D��D  D� D�D��D�D� D  D� D�D��DD� D�qD� D�D� D�qD��D�D��D�qDz�D�qD}qD�qD��D  D� D  D}qD  D� D�D��DD� D�qD� D �D ��D!�D!z�D!�qD"� D#�D#�D$�D$� D%  D%}qD&  D&��D'  D'}qD(  D(� D)�D)�D*�D*��D+�D+��D,�D,� D-�D-�D.�D.��D/  D/� D/�qD0}qD0�qD1}qD1�qD2}qD3  D3� D4�D4� D4�qD5� D6�D6� D6�qD7��D8�D8��D9�D9��D:�D:}qD;  D;� D<�D<�D=  D=}qD=�qD>� D>�qD?� D@D@��DA  DA}qDB  DB��DC  DC}qDC��DD� DE  DE}qDF�DF� DG  DG� DH  DH� DI  DI� DI�qDJ� DK�DK}qDK��DL}qDM  DM��DNDN� DN�qDO� DP�DP��DQ  DQ� DR  DR� DR��DS}qDT�DT��DU�DU� DU�qDV� DW�DW�DX�DX� DY  DY��DZ  DZ� D[  D[��D\  D\� D]  D]� D^  D^��D_  D_� D`  D`�Da�Da� Db  Db��Dc  Dc}qDd�Dd��De  De��Df  Df}qDf�qDg� Dh�Dh��Di  Di� Dj�Dj��Dk�Dk� Dk�qDl}qDl�qDm� Dn  Dn}qDn�qDo}qDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�qDu� Dv  Dv}qDw  Dw��Dx�Dx��Dy  Dy��DzDz� D{  D{� D|�D|��D}  D}}qD}��D~}qD  D� D�HD�@ D�� D���D�  D�AHD�� D�� D���D�=qD�~�D�� D�  D�>�D�� D��HD�  D�@ D�~�D���D�  D�AHD��HD���D���D�@ D�~�D���D�HD�AHD��HD�� D��qD�@ D��HD�� D�  D�>�D�� D��HD�HD�@ D�� D���D���D�@ D�� D��HD�  D�>�D�� D�� D���D�AHD��HD���D���D�>�D�� D��HD��D�AHD�� D�� D���D�@ D�~�D��qD���D�AHD��HD��HD�HD�>�D�� D��HD�HD�@ D�~�D��HD��D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�@ D�~�D���D�  D�@ D��HD���D��qD�>�D��HD��HD�  D�@ D�~�D���D�HD�@ D�~�D��HD�  D�@ D�~�D�� D�  D�>�D�~�D���D�  D�>�D�� D�� D�  D�AHD�~�D���D���D�@ D�� D�� D�  D�=qD�~�D�� D���D�@ D��HD�� D�HD�B�D�� D��HD�HD�@ D�~�D�� D�HD�@ D�~�D��HD��D�AHD�� D���D�  D�AHD��HD��HD�HD�@ D�~�D��qD���D�@ D��HD�� D���D�AHD�� D�� D���D�@ D���D��HD�  D�>�D�� D�D�  D�>�D�~�D��HD�  D�>�D�}qD���D�HD�>�D�� D�� D���D�@ D�� D���D���D�>�D�~�D�� D�HD�>�D�� D�D�  D�=qD�}qD�� D��D�AHD�~�D�� D�  D�@ D��HD��HD�HD�B�D���D�D�HD�>�D�� D��HD�  D�>�D�� D���D���D�@ D��HD��HD�  D�@ D�� D��HD�  D�>�D�~�D���D���D�@ D�� D�� D�  D�@ D�~�D��HD�HD�>�D�� D�� D�HD�@ D�� D���D���D�@ D�� D��HD�HD�AHD��HD�� D�  D�AHD D¾�D���D�>�D�~�Dþ�D���D�@ D�~�D�� D�  D�@ D�~�Dž�D���D�@ Dƀ D�� D��D�B�DǁHD�� D�  D�@ DȁHD�� D���D�@ Dɀ Dɾ�D���D�>�Dʀ D�� D�  D�AHDˀ D˾�D�HD�B�D́HD�� D�HD�>�D̀ D�� D���D�>�D΀ DνqD���D�@ D�~�D��HD�  D�=qDЀ D�D��D�@ D�~�DѾ�D���D�@ DҀ DҾ�D���D�>�D�~�D�� D�HD�AHD�~�D�� D�  D�@ D�~�Dվ�D�  D�@ D�~�D�� D�  D�AHDׁHD�� D�  D�AHD؀ Dؾ�D���D�=qD�~�D��HD�  D�>�Dڀ Dھ�D�  D�@ D�~�D��HD�  D�@ D܀ D�� D�  D�>�D݁HD�D�  D�>�D�~�D�� D�  D�>�D߀ D��HD�  D�>�D�� D�D�  D�=qD� D��HD�  D�@ D�~�D�qD�  D�B�DわD�� D�  D�B�D� D侸D���D�@ D�~�D�qD��qD�>�D� D�� D���D�AHD� D�� D���D�>�D� D辸D�  D�@ D� D�� D�  D�B�D�HD��HD�HD�@ D� D뾸D���D�>�D�~�D�� D�  D�>�D�}qD��qD���D�@ D�~�DD�  D�AHD�HD��HD�  D�>�D�� D��HD�  D�>�D� D��HD�  D�>�D�~�D�� D�  D�>�D�HD�� D�  D�@ D� D�� D���D�=qD�}qD��qD�  D�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�~�D�� D���D�>�D�~�D�� G�O�>�?B�\?��?�{?���?��@��@#�
@8Q�@E�@\(�@u@�ff@�{@��H@���@�33@�p�@���@�@�\@�{@���A�\AQ�A�RA�A�HA   A'
=A,��A2�\A7�A=p�AC33AI��AP  AU�AZ=qA`��Ag
=Amp�Ar�\Aw�A}p�A��A���A��A�=qA���A�  A��HA�A���A��A��RA�=qA���A��A��HA�{A�G�A�(�A�
=A��A��A�Q�A�33A�{Aȣ�A˅A�ffA��A�z�A�
=A�=qA�p�A��A�A�ffA�G�A�z�A�A�=qA��A�  A�33A�B   BG�B�\B�B��B��B�RB�Bz�B	G�B
=qB33BQ�B�B�B
=B  B��B��B�\B�B��Bp�B{B
=B  B��B�B�HB�Bz�Bp�B=qB
=B�
B!�B!�B"�RB#�B$z�B%p�B&=qB'33B(  B(��B)�B*�RB+\)B,(�B-�B.{B.�HB/�B0(�B1�B2{B2�HB3�B4Q�B5G�B6=qB7
=B7�
B8��B9�B:�HB;�B<z�B=��B>�RB?�
B@��BABB�HBD(�BEG�BF=qBG33BHQ�BIp�BJ�\BK�BLz�BMp�BN�\BO�
BP��BR{BS
=BT  BT��BV{BW\)BXz�BY��BZffB[�B\z�B]B^�HB`  Ba�Ba�Bc
=Bd(�Bep�BfffBg�Bh��Bi��Bj�\Bk�Bl��Bn{Bo33Bp(�Bq�Br=qBs33BtQ�Bt��Bu�Bv�\Bw
=Bw�Bx  Bxz�By�Byp�Bz{BzffBz�HB{33B{�B|  B|Q�B|��B}p�B}�B~�\B~�RB33B�B�{B�Q�B���B��HB�33B�p�B�B�  B�=qB�z�B��RB���B�G�B���B��B�=qB�z�B���B�
=B�\)B���B��
B�(�B�z�B���B�33B��B��
B�=qB��\B��HB�33B�p�B��
B�(�B�z�B��HB�G�B���B�  B�ffB���B�33B���B��B�Q�B��RB�
=B�p�B��
B�(�B�z�B���B�33B���B�  B�ffB��RB�33B��B�  B�ffB��HB�G�B��B�(�B��\B�
=B��B��B�ffB���B�G�B�B�(�B��RB��B��B�{B���B��B���B�{B��\B�
=B���B�  B�z�B�
=B��B�  B��\B�
=B��B�  B��\B���B��B�{B�z�B���B���B�  B��\B�
=B��B�{B��\B�
=B��B�{B��\B�
=B���B�  B��\B�
=B��B�  B��\B�
=B��B�  B�z�B�
=B�p�B��B�z�B���B��B�  B�z�B���B�p�B�  B�z�B���B��B�  B�z�B���B��B��B�z�B���B��B�  B��\B���B��B�  B�z�B���B�p�B�  B�z�B���BÅB�  Bď\B�
=BŅB�{Bƣ�B��BǮB�(�BȸRB�G�B�B�Q�B���B�\)B��
B�Q�B���B�\)B��B�ffB���B�p�B��B�ffB��HB�\)B��
B�Q�B���B�G�B��
B�Q�B���B�G�B�B�=qB֣�B��Bי�B�(�Bأ�B�33B�B�=qB���B�\)B��
B�ffB��HB�\)B��
B�Q�B��HB�p�B��B�z�B�
=BᙚB�(�B��B�33B�B�=qB�RB�G�B�B�=qB���B�\)B��B�z�B�
=B�B�(�B��B��B뙚B�{B��B�33B�B�Q�B��HB�p�B�  B��B�33B�B�=qB���B�G�B��
B�ffB���B���B�(�B��RB�G�B��
B�Q�B��HB�p�B�  B���B�G�B��
B�z�B���B��B�{B���B�33B��
C =qC �\C �
C�CffC�C��C=qC�\C�HC33C�CC
=C\)C��C�C33C�C�
C(�Cp�C�RC  CG�C��C�C33Cz�CC	
=C	Q�C	��C	��C
=qC
�\C
��C{CQ�C��C�HC33C�C�
C(�Cp�C�RC  CG�C�\C�HC33C�C��C{CQ�C��C�C33C�\C�
C{CffC��C�C=qC��C�C33Cz�C�RC
=C\)C�C  CQ�C�\C�HC33C�\C�HC(�Cp�C�RC
=C\)C�C��CG�C�\C�
C�Cz�C�
C�Cp�C�RC  CQ�C�C
=C\)C��C��CG�C��C��C Q�C ��C ��C!=qC!�\C!�HC"33C"�\C"�HC#33C#p�C#��C$�C$z�C$��C%{C%\)C%��C&  C&Q�C&�C'  C'G�C'��C'�C(G�C(��C(�C)33C)�\C)�C*=qC*�C*�
C+33C+�C+��C,�C,z�C,�
C-�C-ffC-C.�C.p�C.�RC/
=C/p�C/C0{C0\)C0�RC1{C1p�C1�RC2
=C2ffC2C3�C3ffC3�RC4�C4p�C4C5�C5z�C5�
C633C6�C6�
C733C7�\C7�C833C8�\C8�C9G�C9��C9�C:=qC:��C;  C;G�C;��C<  C<\)C<�C=  C=\)C=C>�C>ffC>�RC?{C?p�C?C@
=C@p�C@CA{CAffCA��CB�CBp�CBCC�CCz�CC�
CD�CDz�CD�
CE33CE�CE�
CF=qCF��CF�HCG=qCG��CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                    @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�A�A�hA�uA�uAᕁA�uA�uA�hA�uA�uA�uA�uAᕁAᙚAᙚAᙚAᙚAᝲAᝲAᝲAᝲA�uA�uA�hA�\A�v�A�ȴAփAԾwAҁAϰ!A�A�AŶFA�XA��7A�C�A�v�A�r�A�ffA�33A��+A�%A��A��A�$�A�33A���A�v�A��-A��\A�(�A�I�A��A�%A��!A���A�ZA�dZA�
=A��!A�z�A��!A��A��A��7A���A��uA}�-AyK�As�Aq"�Al�!Afn�Ad �Ab�A_hsA[��AY��AV �AP�AM�AK��AIoAFȴAC�PA@�9A>jA<�9A:�A:ffA:�A:jA:ZA9�hA8�jA7;dA6r�A6$�A5�^A4�`A3�
A3��A3��A2�/A2bNA2(�A133A/��A.��A,��A,=qA+�TA+XA+"�A*�A*v�A)oA(JA'��A'hsA'/A'VA&�HA&��A&I�A%�7A%�A$��A$bA#��A#�7A#x�A"�RA"A!?}A �RA (�A\)A�RAA�A�A�FAdZA�A��A�A��A`BA
=A�AVA�TA�PA;dA��A��A^5A �Ax�A%A�A�9AbNA{A��AG�A�A�AVA�A�A �AƨA�AVA��AffA{A��Ax�A�A~�AJA�A?}A�`AI�A�TA��A&�A�DA��A��A
��A
bNA
 �A
bA	��A��A�+A(�AƨAhsAXAS�AO�A�Ar�A{A�#A|�A�AA�A��A&�AĜA1'A�mAA�-A��At�A�A ��A -@�+@�ȴ@��@��-@�7L@�I�@�  @��
@��@��!@���@�bN@�+@�=q@��@���@���@�@�
=@���@� �@@�C�@��@���@���@���@�!@�~�@�V@�h@�A�@�t�@�33@��@�!@�V@���@�?}@�D@睲@��@�^@�@�o@�ȴ@�n�@�&�@�I�@�  @��@��m@��
@�ƨ@ߝ�@�t�@�dZ@�33@�~�@��@�Ĝ@ۥ�@�+@�^5@ٺ^@�`B@ج@��@�dZ@�@�5?@Ձ@� �@�K�@�E�@ёh@��@�j@϶F@���@�^5@��@�@͉7@��@̣�@�1'@�ƨ@��@ɲ-@�%@���@ț�@�z�@�Z@�1'@Ǿw@�dZ@�"�@��y@�V@��T@�x�@��@���@���@�bN@���@�;d@°!@�E�@��T@�/@�r�@��w@�S�@��H@�ff@�{@��7@��@���@���@��@�(�@��F@���@�S�@�ȴ@�V@��T@��@���@��9@��@��@�K�@�
=@��R@�~�@��@���@�7L@���@��@��u@�r�@�  @�\)@�33@�@���@�^5@��@��@���@��9@��m@���@�;d@���@���@��\@�v�@�V@��@�x�@��@���@�r�@�(�@�\)@��@��!@�M�@��@�7L@�Ĝ@�Z@�  @��@�S�@�o@��@��+@�-@�@��@��h@�?}@��@�Ĝ@���@�I�@�1@��F@��F@���@��@�t�@�+@��y@��!@�V@��T@���@�p�@�&�@���@�r�@�1'@��;@�S�@��@���@��@�~�@�$�@��7@�7L@��/@��@�Z@� �@�1@��m@��@�
=@���@��+@�$�@���@�?}@��/@���@��u@�Q�@�1@���@��w@�t�@�S�@��@�@��@���@�n�@�-@�{@��@���@�G�@��@�%@�Ĝ@�Z@�1@��
@��F@��@�S�@�o@���@��#@��7@�7L@��D@�Q�@�b@���@�ƨ@�t�@�o@���@�-@��^@�/@��@��@��@��@�V@��/@��@���@��u@�j@� �@�  @�ƨ@��@�
=@��H@���@�5?@���@�7L@��@��@���@�1'@�1@�ƨ@�K�@��@���@�=q@���@�O�@��@��@���@��j@�z�@�1'@�b@��@�dZ@��H@���@�ff@�=q@��@��-@�G�@��@��@��9@��@���@�A�@�@��@K�@�@~�R@~ff@}��@}`B@|��@|��@|Z@|�@{��@z��@zJ@yhs@x��@x�9@xbN@x  @w�P@w;d@v�@vv�@u@t��@t9X@s��@sC�@rJ@q�^@qX@p��@pA�@p �@o�@o+@o
=@n�y@n�R@m�@m�-@m��@m`B@l�@l��@l9X@k�F@kS�@j�@j��@ihs@h��@h�9@h�u@hr�@hA�@g�@g
=@fȴ@f��@fv�@fff@f5?@e@e`B@d��@d�D@dZ@c��@cdZ@co@b��@bM�@a�^@aG�@`��@`Ĝ@`�u@`r�@`A�@`b@_�;@_�@_l�@_
=@^v�@^ff@^@]�@\j@[ƨ@[C�@Z�H@Zn�@Yhs@X��@X�u@XbN@XQ�@XA�@XA�@Xb@W�w@W
=@Vff@V5?@V{@U?}@T��@TZ@T�@S��@R�H@R�\@R=q@RJ@Q��@Qhs@P��@Pr�@O�@O�@O\)@N�y@N�R@Nv�@N@MO�@L9X@K�@J��@JJ@I�^@I7L@H��@H�u@H1'@G�@G��@G��@GK�@G�@F��@Fv�@F$�@F{@E�@Ep�@D�/@D��@DI�@C��@C�
@Ct�@CC�@C33@C33@C"�@B�H@B^5@A��@A��@Ax�@@��@@�u@@�@@A�@?l�@?�@>��@=�h@=�@<��@<I�@<1@;��@;S�@;o@:��@:M�@9�#@9��@9G�@9�@8��@8Q�@81'@7�;@7�P@6��@6�+@6@5�-@5�@5?}@4�@4�D@4I�@3�m@3�@3dZ@3S�@333@2�@2�!@2=q@2�@2�@2�@2�@2J@1�@1G�@0�9@0r�@0Q�@0  @/��@/��@.�R@.ff@.5?@.@-�T@-�-@-�@-p�@-`B@-?}@-�@,��@,��@,(�@+ƨ@+C�@+@*�@*�H@*��@*�!@*�\@*-@*J@)�@)��@(�`@(�@(  @'�w@'\)@&�+@&V@&{@%@%`B@%/@%V@$�@$�@$z�@$I�@#�m@#t�@#o@"��@"��@"~�@"=q@"J@!��@!G�@ �9@ bN@ b@   @  �@�@�@�@��@�P@�P@l�@;d@�@ȴ@�R@�+@5?@��@�@`B@/@��@�j@z�@j@Z@9X@9X@9X@�@1@�
@��@��@t�@dZ@33@�H@��@�\@��@�\@~�@M�@=q@=q@J@�@��@x�@��@�9@b@��@��@�@��@�@�@K�@��@�R@v�@�@�-@�-@�h@`B@/@V@V@�@�@��@��@��@�/@�j@z�@9X@�@�
@��@�@dZ@"�@�@�H@��@��@�!@�\@~�@��@�^@x�@�@%@��@��@��@��@�9@��@r�@ �@�@��@��@��@�@K�@+@
=@��@ȴ@ff@V@{@�@��@`B@��@�@�j@�j@�j@�@��@��@��@�D@z�@z�@�D@�D@j@�@��@t�@dZ@S�@o@
�@
��@
��@
~�@
-@	�^@	�7@	x�@	hs@	hs@	G�@	&�@�`@�u@bN@Q�@Q�@Q�@Q�@Q�@Q�@Q�@A�@A�@ �@��@�@�P@|�@|�A�A�A�A�A�~�A�A�A�~�A�|�A�A�A�+A�~�A�\A�uAᕁA�hA�\A�hAᕁAᕁA�hA�hA�uAᕁAᗍAᕁA�hA�uAᗍAᕁA�uA�hA�hAᕁAᕁA�uA�\A�\A�uAᕁAᕁA�\A�\A�uAᕁAᕁA�uA�\A�hAᕁAᕁA�uA�hA�uAᗍAᕁA�uA�uAᗍAᛦAᛦAᙚAᗍAᗍAᛦAᝲAᙚAᗍAᗍAᛦAᝲAᛦAᗍAᙚAᛦAᝲAᙚAᗍAᙚAᛦA៾AᝲAᙚAᛦA៾A៾AᛦAᛦA៾AᝲAᛦAᙚAᛦA��A៾AᝲAᝲA៾A��A៾AᝲA��A��A៾AᝲAᙚAᙚAᙚA�uA�hA�hA�uAᕁAᕁAᕁA�hA�uAᗍAᕁA�uA�hA�uAᗍA�uA�\A�hAᕁA�uA�\A�\A�hAᕁA�uA�\A�\A�uAᕁA�hA�hA�DA�\A�uA�\A�+A�+A�DA�DA�~�A�A�AߍPA�(�A݅Aܝ�A�1Aۡ�A�oA�ZA��A�K�A�E�A��HA׃A�\)A���Aִ9A�;dA��A�ƨAհ!A՗�A�dZA�&�A��/Aԙ�A�/A��AӺ^A�t�A�=qA��#A�?}A��HAѬAч+A�M�A�%A�hsAυA��A�t�A�
=A͕�A�7LA̶FA�O�A���A�+A���A�t�A�C�A��A� �A��TAȣ�A�M�A�A��A��#A�M�A�{A���A�^5A�oA���A�^5A��A��/AēuA�ZA�&�A�A��`Aú^AÉ7A�-A��A���A�A¼jA�A�l�A�5?A��A���A�"�A��/A��wA�ƨA���A���A��hA�S�A�VA��FA�ffA�7LA�VA��TA��FA��hA�C�A��HA�S�A��A��9A��7A�jA�XA�M�A�7LA���A��9A�?}A���A�K�A�=qA�=qA�?}A�K�A��DA��FA��
A��TA��wA�?}A���A�K�A�JA�A�t�A�7LA�bA�  A��A��
A��wA���A�VA��A�"�A��A���A�1'A���A�/A�  A��#A��#A��A��mA��RA�t�A���A�oA�;dA�+A�(�A�dZA��9A�ffA���A�ĜA��A��FA��wA��TA��mA�;dA�VA�ZA�ZA�I�A���A��A�z�A�r�A�n�A�dZA�C�A�A��RA�Q�A��A���A�Q�A� �A�A��A��A���A��A�9XA�C�A���A��A�t�A�dZA�K�A�+A�&�A��A��A�t�A�5?A��A���A�|�A�ffA�E�A� �A�A��`A���A��-A�dZA�A�A�1'A�+A�&�A��A�JA���A�ƨA��uA�ffA�Q�A�7LA�oA���A��;A��!A�v�A�^5A�VA���A�ZA�=qA�5?A�1'A�(�A�VA��`A��^A���A�z�A�ZA�5?A�JA��A��TA��A���A���A��RA���A��PA�|�A�jA�S�A�=qA�+A��A���A��;A��jA��+A�`BA�5?A��;A�VA�;dA���A�%A��A��-A�hsA�\)A�1'A��`A��^A�;dA��A���A���A�VA��A��A�7LA��A�{A�K�A�^5A�Q�A��`A���A�t�A�\)A�C�A�1'A� �A��A�oA���A��;A�ȴA���A�I�A���A��7A�=qA��A���A���A���A���A�ffA�I�A�$�A�VA���A��;A�ĜA���A�=qA�I�A�bNA��
A�jA�A�A��#A�K�A���A��#A�$�A���A�r�A�ZA�A�A�
=A�ĜA���A�E�A�  A���A�hsA�M�A�?}A�/A�$�A���A���A���A��A�I�A��A��A���A�dZA��`A�XA���A�A~��A~M�A}��A}�A}`BA};dA|��A|A�A{��Az�Ay�Aw��Avz�AudZAt��Atz�AtA�As�#Ast�Ar�/ArffArbAq�hAqVAp��Ap��Ap�Ao�^Ao7LAnĜAn^5Am�Ai�PAh^5Ag|�Ag/Af�Af~�AfE�Ae�Ae"�Ad�Ad~�AdJAc�mAc��AcAc�FAc��Ac�PAc?}Ab��Ab�DAbAa��Aal�Aa�A`I�A^�A^�A\ȴA\-A\bA\  A[��A[��A[�A[+AZ��AY��AY�wAY��AY�7AYXAX�/AW�;AV�AVQ�AU�PAT�ASARbNAQ�AQl�AP��AP  AO?}AN��AN-AM�-AMdZAM"�AL��AL�uAL5?AL�AL1AKƨAKG�AJ��AJbAIXAIG�AH�HAHn�AH-AH{AH1AGt�AFv�AE�#AE�7AE+ADz�AC�AC�ACVAB~�ABZABI�AB$�A@�RA?�;A?G�A?A>��A>��A>ffA>E�A>(�A=��A=�
A=G�A<ĜA<�uA<A;�^A;XA:��A:�A:�9A9��A9��A9�TA:M�A:n�A:�A:�+A:�DA:�uA:��A:�\A:�A:r�A:ffA:jA:jA:r�A:jA:jA:ffA:^5A:VA:ZA:^5A:^5A:5?A9��A9�-A9�A9\)A9XA97LA9&�A9oA8��A8�9A8A�A7��A7�wA7hsA7C�A7�A6�`A6�jA6�A6r�A6Q�A6jA6v�A6~�A6bNA65?A6{A5��A5�TA5�
A5A5�wA5�wA5�-A5��A5��A5x�A5
=A4��A4�DA4E�A4�A3�A3�#A3ƨA3�FA3�-A3��A3��A3��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                    A�A�A�A�A�hA�uA�uAᕁA�uA�uA�hA�uA�uA�uA�uAᕁAᙚAᙚAᙚAᙚAᝲAᝲAᝲAᝲA�uA�uA�hA�\A�v�A�ȴAփAԾwAҁAϰ!A�A�AŶFA�XA��7A�C�A�v�A�r�A�ffA�33A��+A�%A��A��A�$�A�33A���A�v�A��-A��\A�(�A�I�A��A�%A��!A���A�ZA�dZA�
=A��!A�z�A��!A��A��A��7A���A��uA}�-AyK�As�Aq"�Al�!Afn�Ad �Ab�A_hsA[��AY��AV �AP�AM�AK��AIoAFȴAC�PA@�9A>jA<�9A:�A:ffA:�A:jA:ZA9�hA8�jA7;dA6r�A6$�A5�^A4�`A3�
A3��A3��A2�/A2bNA2(�A133A/��A.��A,��A,=qA+�TA+XA+"�A*�A*v�A)oA(JA'��A'hsA'/A'VA&�HA&��A&I�A%�7A%�A$��A$bA#��A#�7A#x�A"�RA"A!?}A �RA (�A\)A�RAA�A�A�FAdZA�A��A�A��A`BA
=A�AVA�TA�PA;dA��A��A^5A �Ax�A%A�A�9AbNA{A��AG�A�A�AVA�A�A �AƨA�AVA��AffA{A��Ax�A�A~�AJA�A?}A�`AI�A�TA��A&�A�DA��A��A
��A
bNA
 �A
bA	��A��A�+A(�AƨAhsAXAS�AO�A�Ar�A{A�#A|�A�AA�A��A&�AĜA1'A�mAA�-A��At�A�A ��A -@�+@�ȴ@��@��-@�7L@�I�@�  @��
@��@��!@���@�bN@�+@�=q@��@���@���@�@�
=@���@� �@@�C�@��@���@���@���@�!@�~�@�V@�h@�A�@�t�@�33@��@�!@�V@���@�?}@�D@睲@��@�^@�@�o@�ȴ@�n�@�&�@�I�@�  @��@��m@��
@�ƨ@ߝ�@�t�@�dZ@�33@�~�@��@�Ĝ@ۥ�@�+@�^5@ٺ^@�`B@ج@��@�dZ@�@�5?@Ձ@� �@�K�@�E�@ёh@��@�j@϶F@���@�^5@��@�@͉7@��@̣�@�1'@�ƨ@��@ɲ-@�%@���@ț�@�z�@�Z@�1'@Ǿw@�dZ@�"�@��y@�V@��T@�x�@��@���@���@�bN@���@�;d@°!@�E�@��T@�/@�r�@��w@�S�@��H@�ff@�{@��7@��@���@���@��@�(�@��F@���@�S�@�ȴ@�V@��T@��@���@��9@��@��@�K�@�
=@��R@�~�@��@���@�7L@���@��@��u@�r�@�  @�\)@�33@�@���@�^5@��@��@���@��9@��m@���@�;d@���@���@��\@�v�@�V@��@�x�@��@���@�r�@�(�@�\)@��@��!@�M�@��@�7L@�Ĝ@�Z@�  @��@�S�@�o@��@��+@�-@�@��@��h@�?}@��@�Ĝ@���@�I�@�1@��F@��F@���@��@�t�@�+@��y@��!@�V@��T@���@�p�@�&�@���@�r�@�1'@��;@�S�@��@���@��@�~�@�$�@��7@�7L@��/@��@�Z@� �@�1@��m@��@�
=@���@��+@�$�@���@�?}@��/@���@��u@�Q�@�1@���@��w@�t�@�S�@��@�@��@���@�n�@�-@�{@��@���@�G�@��@�%@�Ĝ@�Z@�1@��
@��F@��@�S�@�o@���@��#@��7@�7L@��D@�Q�@�b@���@�ƨ@�t�@�o@���@�-@��^@�/@��@��@��@��@�V@��/@��@���@��u@�j@� �@�  @�ƨ@��@�
=@��H@���@�5?@���@�7L@��@��@���@�1'@�1@�ƨ@�K�@��@���@�=q@���@�O�@��@��@���@��j@�z�@�1'@�b@��@�dZ@��H@���@�ff@�=q@��@��-@�G�@��@��@��9@��@���@�A�@�@��@K�@�@~�R@~ff@}��@}`B@|��@|��@|Z@|�@{��@z��@zJ@yhs@x��@x�9@xbN@x  @w�P@w;d@v�@vv�@u@t��@t9X@s��@sC�@rJ@q�^@qX@p��@pA�@p �@o�@o+@o
=@n�y@n�R@m�@m�-@m��@m`B@l�@l��@l9X@k�F@kS�@j�@j��@ihs@h��@h�9@h�u@hr�@hA�@g�@g
=@fȴ@f��@fv�@fff@f5?@e@e`B@d��@d�D@dZ@c��@cdZ@co@b��@bM�@a�^@aG�@`��@`Ĝ@`�u@`r�@`A�@`b@_�;@_�@_l�@_
=@^v�@^ff@^@]�@\j@[ƨ@[C�@Z�H@Zn�@Yhs@X��@X�u@XbN@XQ�@XA�@XA�@Xb@W�w@W
=@Vff@V5?@V{@U?}@T��@TZ@T�@S��@R�H@R�\@R=q@RJ@Q��@Qhs@P��@Pr�@O�@O�@O\)@N�y@N�R@Nv�@N@MO�@L9X@K�@J��@JJ@I�^@I7L@H��@H�u@H1'@G�@G��@G��@GK�@G�@F��@Fv�@F$�@F{@E�@Ep�@D�/@D��@DI�@C��@C�
@Ct�@CC�@C33@C33@C"�@B�H@B^5@A��@A��@Ax�@@��@@�u@@�@@A�@?l�@?�@>��@=�h@=�@<��@<I�@<1@;��@;S�@;o@:��@:M�@9�#@9��@9G�@9�@8��@8Q�@81'@7�;@7�P@6��@6�+@6@5�-@5�@5?}@4�@4�D@4I�@3�m@3�@3dZ@3S�@333@2�@2�!@2=q@2�@2�@2�@2�@2J@1�@1G�@0�9@0r�@0Q�@0  @/��@/��@.�R@.ff@.5?@.@-�T@-�-@-�@-p�@-`B@-?}@-�@,��@,��@,(�@+ƨ@+C�@+@*�@*�H@*��@*�!@*�\@*-@*J@)�@)��@(�`@(�@(  @'�w@'\)@&�+@&V@&{@%@%`B@%/@%V@$�@$�@$z�@$I�@#�m@#t�@#o@"��@"��@"~�@"=q@"J@!��@!G�@ �9@ bN@ b@   @  �@�@�@�@��@�P@�P@l�@;d@�@ȴ@�R@�+@5?@��@�@`B@/@��@�j@z�@j@Z@9X@9X@9X@�@1@�
@��@��@t�@dZ@33@�H@��@�\@��@�\@~�@M�@=q@=q@J@�@��@x�@��@�9@b@��@��@�@��@�@�@K�@��@�R@v�@�@�-@�-@�h@`B@/@V@V@�@�@��@��@��@�/@�j@z�@9X@�@�
@��@�@dZ@"�@�@�H@��@��@�!@�\@~�@��@�^@x�@�@%@��@��@��@��@�9@��@r�@ �@�@��@��@��@�@K�@+@
=@��@ȴ@ff@V@{@�@��@`B@��@�@�j@�j@�j@�@��@��@��@�D@z�@z�@�D@�D@j@�@��@t�@dZ@S�@o@
�@
��@
��@
~�@
-@	�^@	�7@	x�@	hs@	hs@	G�@	&�@�`@�u@bN@Q�@Q�@Q�@Q�@Q�@Q�@Q�@A�@A�@ �@��@�@�P@|�G�O�A�A�A�A�A�~�A�A�A�~�A�|�A�A�A�+A�~�A�\A�uAᕁA�hA�\A�hAᕁAᕁA�hA�hA�uAᕁAᗍAᕁA�hA�uAᗍAᕁA�uA�hA�hAᕁAᕁA�uA�\A�\A�uAᕁAᕁA�\A�\A�uAᕁAᕁA�uA�\A�hAᕁAᕁA�uA�hA�uAᗍAᕁA�uA�uAᗍAᛦAᛦAᙚAᗍAᗍAᛦAᝲAᙚAᗍAᗍAᛦAᝲAᛦAᗍAᙚAᛦAᝲAᙚAᗍAᙚAᛦA៾AᝲAᙚAᛦA៾A៾AᛦAᛦA៾AᝲAᛦAᙚAᛦA��A៾AᝲAᝲA៾A��A៾AᝲA��A��A៾AᝲAᙚAᙚAᙚA�uA�hA�hA�uAᕁAᕁAᕁA�hA�uAᗍAᕁA�uA�hA�uAᗍA�uA�\A�hAᕁA�uA�\A�\A�hAᕁA�uA�\A�\A�uAᕁA�hA�hA�DA�\A�uA�\A�+A�+A�DA�DA�~�A�A�AߍPA�(�A݅Aܝ�A�1Aۡ�A�oA�ZA��A�K�A�E�A��HA׃A�\)A���Aִ9A�;dA��A�ƨAհ!A՗�A�dZA�&�A��/Aԙ�A�/A��AӺ^A�t�A�=qA��#A�?}A��HAѬAч+A�M�A�%A�hsAυA��A�t�A�
=A͕�A�7LA̶FA�O�A���A�+A���A�t�A�C�A��A� �A��TAȣ�A�M�A�A��A��#A�M�A�{A���A�^5A�oA���A�^5A��A��/AēuA�ZA�&�A�A��`Aú^AÉ7A�-A��A���A�A¼jA�A�l�A�5?A��A���A�"�A��/A��wA�ƨA���A���A��hA�S�A�VA��FA�ffA�7LA�VA��TA��FA��hA�C�A��HA�S�A��A��9A��7A�jA�XA�M�A�7LA���A��9A�?}A���A�K�A�=qA�=qA�?}A�K�A��DA��FA��
A��TA��wA�?}A���A�K�A�JA�A�t�A�7LA�bA�  A��A��
A��wA���A�VA��A�"�A��A���A�1'A���A�/A�  A��#A��#A��A��mA��RA�t�A���A�oA�;dA�+A�(�A�dZA��9A�ffA���A�ĜA��A��FA��wA��TA��mA�;dA�VA�ZA�ZA�I�A���A��A�z�A�r�A�n�A�dZA�C�A�A��RA�Q�A��A���A�Q�A� �A�A��A��A���A��A�9XA�C�A���A��A�t�A�dZA�K�A�+A�&�A��A��A�t�A�5?A��A���A�|�A�ffA�E�A� �A�A��`A���A��-A�dZA�A�A�1'A�+A�&�A��A�JA���A�ƨA��uA�ffA�Q�A�7LA�oA���A��;A��!A�v�A�^5A�VA���A�ZA�=qA�5?A�1'A�(�A�VA��`A��^A���A�z�A�ZA�5?A�JA��A��TA��A���A���A��RA���A��PA�|�A�jA�S�A�=qA�+A��A���A��;A��jA��+A�`BA�5?A��;A�VA�;dA���A�%A��A��-A�hsA�\)A�1'A��`A��^A�;dA��A���A���A�VA��A��A�7LA��A�{A�K�A�^5A�Q�A��`A���A�t�A�\)A�C�A�1'A� �A��A�oA���A��;A�ȴA���A�I�A���A��7A�=qA��A���A���A���A���A�ffA�I�A�$�A�VA���A��;A�ĜA���A�=qA�I�A�bNA��
A�jA�A�A��#A�K�A���A��#A�$�A���A�r�A�ZA�A�A�
=A�ĜA���A�E�A�  A���A�hsA�M�A�?}A�/A�$�A���A���A���A��A�I�A��A��A���A�dZA��`A�XA���A�A~��A~M�A}��A}�A}`BA};dA|��A|A�A{��Az�Ay�Aw��Avz�AudZAt��Atz�AtA�As�#Ast�Ar�/ArffArbAq�hAqVAp��Ap��Ap�Ao�^Ao7LAnĜAn^5Am�Ai�PAh^5Ag|�Ag/Af�Af~�AfE�Ae�Ae"�Ad�Ad~�AdJAc�mAc��AcAc�FAc��Ac�PAc?}Ab��Ab�DAbAa��Aal�Aa�A`I�A^�A^�A\ȴA\-A\bA\  A[��A[��A[�A[+AZ��AY��AY�wAY��AY�7AYXAX�/AW�;AV�AVQ�AU�PAT�ASARbNAQ�AQl�AP��AP  AO?}AN��AN-AM�-AMdZAM"�AL��AL�uAL5?AL�AL1AKƨAKG�AJ��AJbAIXAIG�AH�HAHn�AH-AH{AH1AGt�AFv�AE�#AE�7AE+ADz�AC�AC�ACVAB~�ABZABI�AB$�A@�RA?�;A?G�A?A>��A>��A>ffA>E�A>(�A=��A=�
A=G�A<ĜA<�uA<A;�^A;XA:��A:�A:�9A9��A9��A9�TA:M�A:n�A:�A:�+A:�DA:�uA:��A:�\A:�A:r�A:ffA:jA:jA:r�A:jA:jA:ffA:^5A:VA:ZA:^5A:^5A:5?A9��A9�-A9�A9\)A9XA97LA9&�A9oA8��A8�9A8A�A7��A7�wA7hsA7C�A7�A6�`A6�jA6�A6r�A6Q�A6jA6v�A6~�A6bNA65?A6{A5��A5�TA5�
A5A5�wA5�wA5�-A5��A5��A5x�A5
=A4��A4�DA4E�A4�A3�A3�#A3ƨA3�FA3�-A3��A3��A3��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                    ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B		B		B	qB	�B	7B	kB	kB	7B	kB	kB	�B	�B	kB	kB	kB	�B	kB	kB	�B	kB	kB	7B	7B	B	�B	�B	�B	FB	\)B
�B
9�B
MB
^�B
dZB
p;B
|�B
��B
��B
�TB
�B�BZQB;dB*0B�B!-BpB�"B�(B��B�QB��B�RB��B��B��B��B�%Bm�B2�BJB
��B
�B
��B
��B
s�B
H�B
-B
'B
1B
	�B	�B	�EB	�oB	v�B	gB	5�B	$@B	�B	"�B	"�B	0�B	-�B	\B��B�iB�sB��B�EB�,B˒B��B�HB�fB	�B	'B	K�B	_;B	j�B	rGB	s�B	z�B	yrB	�uB	�lB	�	B	�B	�eB	�	B	��B	�B	�9B	�B	ݘB	��B	�JB
	7B
(B
VB
B
VB
fB
�B

=B
�B
�B
�B
�B
%FB
)�B
(�B
,=B
+�B
)�B
'�B
(�B
.B
+B
0UB
-CB
/B
+6B
.}B
.B
-wB
/OB
/�B
/�B
0�B
0!B
0�B
2�B
3�B
5B
6�B
8�B
9�B
:�B
;dB
<�B
>B
?B
A�B
B�B
C-B
F?B
E�B
FB
GB
G�B
H�B
JXB
K^B
L�B
PHB
PHB
P}B
PB
O�B
O�B
N�B
OvB
O�B
OvB
OB
L�B
K�B
MB
J�B
JXB
J#B
I�B
IB
H�B
GEB
GB
F�B
F?B
EB
D3B
C�B
EmB
C�B
B'B
A�B
B'B
A B
@B
?�B
?HB
?�B
?B
<�B
=B
<6B
:�B
8�B
8RB
7LB
6zB
7B
6zB
6B
5�B
5tB
4�B
5?B
2�B
3�B
1'B
0�B
0!B
/B
.�B
.�B
-B
,�B
+�B
)�B
(�B
&�B
'RB
%�B
%FB
%zB
$�B
$@B
#:B
$@B
!-B
!bB
 �B
 \B
�B
�B
�B
VB
�B
OB
!B
VB
OB
�B
IB
B
�B
CB
	B
�B
B
�B
�B
�B
�B
�B
�B
�B
SB
SB
SB
B
B
B
B
�B
�B
�B
B
{B
�B
�B
:B
B
�B
hB
�B
hB
bB
bB
�B
\B
VB
JB
PB
�B
B
B
JB
�B
JB
B
�B
xB
DB
DB
DB
B
DB
�B
B
JB
B
B
B
B
~B
~B
�B
~B
B
�B
B
PB
~B
JB
B
�B
PB
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
"B
"B
�B
"B
VB
VB
"B
�B
�B
�B
�B
�B
.B
�B
hB
4B
 B
 B
4B
�B
4B
4B
�B
�B
�B
hB
hB
oB
oB
�B
�B
�B
B
:B
B
oB
�B
B
�B
�B
�B
�B
�B
@B
uB
�B
�B
FB
�B
�B
B
�B
�B
�B
B
�B
$B
YB
YB
�B
�B
+B
�B
�B
1B
1B
�B
�B
1B
1B
eB
1B
eB
�B
�B
B
7B
7B
7B
kB
CB
B
xB
~B
�B
B
�B
�B
�B
�B
�B
�B
OB
B
�B
�B
!B
�B
B
�B
�B
�B
!B
VB
VB
�B
!B
�B
�B
�B
 'B
!bB
!�B
!�B
!�B
"�B
"�B
#�B
#�B
$B
$�B
$�B
%B
%B
$�B
%zB
%zB
%�B
%zB
%�B
%�B
&�B
&�B
&LB
'B
'�B
'�B
'�B
'�B
($B
($B
(XB
(�B
)�B
)*B
)�B
*�B
*eB
*�B
*0B
*�B
*�B
+6B
*�B
*�B
+6B
+kB
+kB
+�B
+�B
+�B
+kB
+�B
+kB
+6B
*�B
+B
+B
*�B
+6B
+kB
+�B
+�B
-B
,�B
-�B
.B
-�B
-�B
-�B
-�B
.IB
.�B
0�B
0!B
0UB
0�B
0�B
1�B
1�B
1�B
1'B
0�B
1�B
1�B
1�B
2aB
2�B
33B
3hB
3�B
3�B
3hB
4�B
5?B
5?B
5�B
5�B
5tB
5tB
6zB
6�B
7B
7�B
7�B
7�B
7�B
7�B
8B
8RB
8�B
8�B
8�B
8�B
9XB
9�B
9�B
:*B
:�B
:�B
;0B
;dB
;�B
<B
;�B
<�B
<�B
=<B
=B
>B
?B
>�B
>�B
?}B
?�B
?}B
?�B
@B
@B
@B
@OB
@�B
@�B
@�B
@�B
@�B
A B
A B
A�B
A�B
A�B
A�B
C�B
C-B
B�B
B�B
C-B
B�B
CaB
D3B
C�B
C�B
C�B
D3B
DgB
D�B
D�B
E9B
EB
E9B
E�B
FB
FB
FtB
F�B
GzB
G�B
HB
HB
HB
HKB
HKB
H�B
HKB
H�B
H�B
H�B
IRB
H�B
H�B
I�B
J�B
K)B
K^B
K�B
K�B
M6B
L�B
M6B
M6B
MB
M6B
L�B
M6B
M6B
N<B
N<B
NB
M�B
OBB
OB
OvB
OBB
PB
PHB
PHB
PHB
P}B
P}B
P�B
QB
Q�B
Q�B
Q�B
R B
R B
RTB
Q�B
R�B
R�B
S�B
T,B
TaB
T�B
T�B
T�B
U2B
T�B
UgB
U2B
VB
VB
V9B
VmB
VmB
W�B
W
B
W?B
W
B
WsB
WsB
W?B
W�B
W�B
XB
XB
X�B
YKB
YB
YB
YKB
Y�B
YB
ZB
Y�B
Z�B
ZB
Y�B
ZQB
Z�B
Z�B
[WB
\)B
\)B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
^�B
^�B
_B
^�B
_B
_pB
_pB
_�B
`vB
`B
`�B
`�B
`�B
aB
`�B
a|B
a|B
a�B
bB
b�B
b�B
b�B
b�B
b�B
c B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
e,B
e,B
e�B
e`B
e�B
f�B
f�B
f�B
f�B
gB
gB
gmB
gB
gB
gB
g8B
gmB
g8B
h
B
h>B
h�B
h�B
iB
hsB
iB
iB
iB
iDB
iDB
iDB
iyB
jB
jB
jKB
jB
j�B
kQB
kQB
k�B
k�B
l"B
l"B
l"B
lWB
lWB
l�B
l�B
l�B
m]B
m]B
m�B
ncB
n�B
o5B
o B
o�B
poB
poB
p�B
qB
qvB
q�B
rB
r|B
rGB
r|B
r|B
r|B
r�B
r�B
s�B
sMB
sB
s�B
tTB
u%B
uZB
uZB
uZB
t�B
u�B
u�B
u�B
uZB
u�B
u�B
u%B
u�B
v+B
u�B
u�B
v`B
v`B
v�B
w2B
wfB
w�B
wfB
v�B
v�B
w2B
xB
xB
wfB
w�B
x8B
wfB
xlB
w�B
xB
w�B
w�B
w�B
xB
x�B
y�B
z�B
z�B
zxB
zDB
zDB
z�B
zDB
z�B
z�B
{JB
{B
{�B
{�B
{JB
{JB
{B
{B
|PB
|�B
|�B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~�B
~�B
~�B
~�B
.B
~�B
.B
.B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
~�B
�B
�B
�4B
�iB
�iB
��B
�;B
�oB
�oB
��B
��B
��B
��B
��B
�uB
�uB
��B
�uB
�AB
�uB
�uB
�uB
��B
�uB
�uB
�uB
��B
�uB
��B
�GB
�GB
�{B
��B
��B
��B
��B
��B
�%B
�YB
��B
��B
��B
�+B
�1B
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
��B
�B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�	B
�	B
�lB	B	CB	kB	7B	�B		B	�B	B	�B	kB	=B	�B	B	�B	B	eB	�B	=B	�B	�B	�B	=B	qB	�B	B	eB	B	�B	�B	�B	�B	kB	�B	�B	B	�B	B	qB	�B	kB	�B	�B	=B	qB	7B	�B	�B	7B	�B		B	�B	eB	�B	�B	qB	7B	B	�B	=B	kB	�B	eB	7B	qB		B	�B	1B	kB	=B	qB	kB	eB	B	qB	�B	�B	�B	7B	=B	=B	kB	eB	7B	�B		B	�B	7B	qB	=B	�B	�B	�B	=B	�B	�B	B	=B		B	B	1B	7B	=B	7B	1B	B		B	�B	�B	eB	7B	�B	�B	�B	�B	�B	�B	7B	eB	_B	�B	�B	7B	1B	�B	_B	�B	�B	YB	�B	�B	eB	�B	�B	YB	_B	+B	B	�B	�B	B	�B	FB	�B	B	uB	:B	�B	JB	�B	�B	wfB	�B	�+B	�aB	ŢB	�RB	��B	��B
B
�B
6�B
,=B
5?B
2-B
>B
CaB
;dB
6�B
6�B
7�B
9�B
B�B
I�B
F?B
N�B
_�B
YB
_B
a�B
b�B
qAB
qAB
YB
R�B
UgB
S[B
W�B
W?B
|B
poB
h
B
r|B
]�B
d&B
qB
m]B
|PB
yrB
qvB
o�B
n�B
x8B
��B
uZB
.B
�AB
�B
�B
��B
��B
��B
�FB
��B
��B
�1B
�MB
�iB
}�B
�4B
{�B
}�B
}"B
}�B
� B
��B
�~B
�	B
��B
�B
�oB
��B
��B
�	B
�rB
�:B
�IB
�LB
��B
�'B
� B
�B
�>B
��B
�QB
�yB
�8B
�B
�B
�B
�B
�WB
�BB
�B
�,B
�WB
��B
�TB
� B
уB
�B
�vB
ܒB
چB
�mB
�B
�B
�B
�KB
�B
�mB�BVB6zBU2Bk�Bz�Bw2Bg�BdZBd&B^�BXyBS�BO�BM�BJ�BL�BPHBM6BP}BP}BS�BVB1�B�B{BDBMB
��B�B"hBK^Be,BAUB>wB@�B>�B-CB�B�B
�DB
�.B
�B
�B
��B
�xBBB=<BW�BV�BU�Be�Bl"Bi�Bo Bp�Bt�B|PB�xB�hB�bB��B�(B��B��B��B� BcB{By�B��B��B��B�6BуB��B�aB�B�|B��B�`B��B��B�ZB�B�B՛BѷB҉BϫB�jB�0B�KB�<B�RB��B�}B��B��B��B�6B�^B��B�*B��B�B�B�wB�0B��B�*B�XB��B�zB�nB�~B�IB��B�=B��B�OB��B�IB��B�B��B�CB��B�_B��B��B�B��B��B�bB�"B��B�~B�xB�fB�+B�%B��B��B�GB�BzDB{�B{JB�GB�4BYBXBF?B?}BNpB49B4�B+B0�B/�B�B7B�B(BhB�BoB
�>B
��B
�B
�ZB
�KB
��B
��B
��B
�wB
��B
��B
��B
�nB
�B
�FB
��B
��B
�4B
��B
�kB
�FB
��B
��B
�GB
�VB
��B
��B
�AB
~�B
� B
{JB
x�B
x�B
u�B
q�B
|�B
�B
kB
_�B
OvB
FB
P�B
FtB
J�B
I�B
?�B
5tB
(�B
&LB
)�B
0�B
.�B
.B
1�B
,B
2aB
'�B
!�B
B
IB
~B
 �B
B
�B
�B
MB
B
VB

�B
+B
AB
PB
�B	�(B	��B	�B	�fB	�pB	�gB	�sB	ٴB	�BB	�0B	��B	�HB	�RB	��B	��B	��B	��B	��B	��B	�PB	�B	~�B	��B	{B	}�B	p;B	p;B	j�B	i�B	c�B	bNB	W�B	��B	n�B	HKB	E9B	<�B	:�B	-�B	1[B	-�B	2-B	'RB	($B	.B	!-B	!�B	�B	!B	OB	~B	 'B	!�B	CB	�B	�B	7B	�B	!�B	.�B	#�B	.�B	!-B	OB	�B	~B	#nB	)�B	.�B	9�B	7�B	0�B	+B	)�B	+kB	1[B	4�B	/OB	(�B	*�B	3�B	$�B	�B	�B	�B	�B	�B	�B	 �B	�B��B��B��B�B�B��B�WB�WB��B�8B��B	�B��B�B�
B�KB�ZBںB�B��B��B�BچB�QB�B�B�9B��BӏB��BɺB��B��BںB˒B��B�0B̘B��B�XB��B��BʌB�B��B҉BیB�?B��B�)B�EB��B��BںB�B�"B��B�>B�(B	 iB	uB	�B	
	B	PB	oB	�B	~B	�B	!bB	&�B	/OB	4nB	9XB	@�B	H�B	T�B	U�B	]�B	b�B	bNB	^5B	\]B	_B	a�B	aB	c�B	g8B	s�B	u%B	rB	t�B	v+B	p;B	ncB	tB	o5B	t�B	r�B	q�B	rB	t�B	u�B	~�B	y�B	|�B	{B	y�B	x�B	y�B	y>B	xlB	y�B	zB	|�B	��B	��B	��B	�B	�YB	�SB	��B	��B	��B	��B	��B	��B	�	B	�lG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                    B	#B		B	=B	�B	7B	�B	kB	QB	�B	�B	�B	�B	�B	�B	kB	�B	�B	�B	�B	kB	�B	QB	QB	QB	�B	1B	KB	"�B	zB
SB
C{B
Y�B
o5B
x�B
�UB
��B
��B
��B
��B
��B�Bl�BPHBIRBVB$�BxlB�[B�}B�XB��B��B�cB�[B��B��B��B�aB~wBD�B(�B
��B
�OB
��B
�NB
��B
[#B
7LB
-�B
&B
=B	�B	��B	��B	�?B	y�B	>(B	)yB	+6B	.cB	*eB	=<B	>]B	�B	UB��B��B��B��B�CBѷB��B�B�2B	<B	'�B	NVB	b4B	o�B	t�B	t�B	|6B	|jB	��B	�#B	��B	��B	��B	�)B	�nB	��B	�B	�,B	�'B	�-B	�"B
	�B
�B
HB
�B
�B

	B
	�B
B
jB
<B
�B
B
'�B
+B
*�B
./B
-CB
*B
(sB
+kB
0�B
-�B
2-B
/�B
1�B
-�B
0!B
/5B
.cB
0oB
0�B
1[B
2aB
1AB
2GB
4B
4�B
6`B
8�B
9�B
:�B
;�B
<PB
>(B
?B
AUB
CGB
C-B
DB
G_B
GB
G�B
HB
HKB
IB
J�B
K�B
N"B
Q�B
Q�B
Q�B
Q�B
Q�B
P}B
O�B
P}B
QB
P�B
Q4B
N�B
M�B
NB
LdB
L~B
K�B
J�B
J�B
J�B
IRB
H�B
IB
HKB
E�B
D�B
E9B
G�B
E�B
C{B
B�B
CaB
AoB
@4B
@B
@OB
A�B
@iB
=�B
>wB
>BB
=B
:�B
:*B
8�B
8�B
8B
6�B
6`B
5�B
6+B
6+B
6�B
4�B
5�B
2B
1�B
0�B
0B
0UB
/5B
-wB
-]B
-)B
+�B
*�B
)*B
)*B
'�B
&LB
&�B
%�B
%FB
%FB
'�B
"NB
"B
!HB
 �B
�B
�B
�B
�B
VB
�B
!|B
 �B
�B
5B
�B
�B
�B
dB
]B
]B
�B
�B
�B
QB
+B
�B
B
EB
�B
�B
mB
9B
SB
mB
mB
B
B
�B
SB
�B
�B
�B
�B
,B
�B
�B
�B
oB
4B
�B
4B
�B
�B
<B
�B
�B
jB
�B
B
jB
�B
�B
0B
dB
0B
0B
0B
dB
�B
"B
�B
�B
dB
dB
~B
�B
6B
B
6B
�B
�B
�B
B
�B
�B
B
<B
�B
VB
�B
�B
<B
�B
BB
�B
pB
�B
<B
�B
BB
pB
pB
VB
�B
(B
�B
�B
�B
vB
vB
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
 B
B
�B
 B
�B
�B
TB
�B
�B
:B
TB
�B
�B
&B
B
B
B
�B
[B
�B
B
�B
�B
�B
,B
�B
gB
�B
gB
�B
�B

B
SB
?B
�B
B
B
+B
B
�B
�B
�B
B
1B
�B
B
1B
�B
�B
�B
�B
B
B
B
kB
B
QB
�B
kB
�B
�B
�B
/B
OB
B
�B
~B
dB
�B
5B
�B
�B
�B
jB
B
�B
�B
 B
�B
�B
�B
VB
�B
�B
�B
 B
 \B
 �B
 B
 \B
!-B
"B
"NB
!�B
"B
#TB
#nB
$B
#�B
$�B
$�B
%FB
%FB
%FB
%zB
%�B
%�B
%�B
%�B
&2B
&�B
'B
&�B
&�B
'�B
(XB
(XB
(>B
(XB
(�B
(�B
)DB
)�B
*eB
)�B
+B
+B
*�B
*�B
*�B
+6B
+kB
,"B
+�B
+�B
,=B
+�B
+kB
+�B
+�B
+�B
+�B
,=B
+�B
+kB
+B
+�B
+QB
+QB
+�B
,WB
,"B
,"B
-�B
-�B
.cB
.cB
./B
.IB
.}B
.IB
.�B
/�B
1AB
0�B
1'B
1�B
1�B
2-B
1�B
1�B
1[B
1vB
2-B
2GB
2|B
2�B
3�B
3�B
3�B
3�B
3�B
49B
5tB
5�B
5�B
6FB
5�B
5�B
6+B
6�B
6�B
7fB
7�B
7�B
7�B
8RB
8RB
8�B
8�B
9	B
9	B
9$B
9�B
:B
:�B
:^B
:xB
:�B
;dB
;�B
;�B
<6B
<jB
<PB
=�B
="B
=�B
=�B
?.B
?}B
?B
?cB
@ B
?�B
@ B
@iB
@4B
@OB
@iB
AB
A;B
@�B
@�B
AUB
A;B
A�B
A�B
B'B
B'B
B[B
CB
DB
CaB
CB
CB
CaB
CaB
D3B
D�B
D3B
DB
DB
DgB
D�B
E9B
EmB
E�B
E9B
E�B
F%B
FYB
FYB
F�B
GzB
G�B
H1B
HKB
HKB
H1B
H�B
H�B
H�B
H�B
H�B
H�B
IlB
IlB
IRB
IRB
J�B
K^B
K�B
K�B
LB
L�B
M�B
MB
MjB
MPB
MB
M6B
MB
M�B
M�B
N�B
NpB
N<B
N�B
O�B
OvB
O�B
O�B
P�B
P�B
P�B
P}B
P�B
P�B
QhB
Q�B
RB
R:B
RB
R�B
RTB
R�B
RoB
S@B
S�B
T�B
T�B
T�B
T�B
UB
UMB
U�B
UgB
U�B
UgB
V9B
VSB
VmB
V�B
V�B
X+B
W$B
WsB
W�B
W�B
W�B
W�B
W�B
W�B
XyB
XEB
X�B
YKB
Y1B
YeB
Y�B
ZQB
Y�B
ZQB
Z�B
Z�B
Z7B
Z7B
[#B
[#B
[=B
\]B
\�B
\xB
]B
]IB
]dB
^B
^B
^B
^OB
_;B
_B
_pB
_B
_pB
_�B
_�B
_�B
`�B
`�B
aB
a-B
a-B
aHB
a-B
a�B
a�B
b4B
b�B
b�B
b�B
cB
b�B
c:B
cnB
c�B
dB
c�B
c�B
c�B
c�B
c�B
dZB
eB
eFB
e`B
ezB
e�B
e�B
ffB
f�B
gB
gB
f�B
g8B
g8B
g�B
gB
gB
gB
gmB
g�B
g�B
hsB
h�B
iB
h�B
i*B
h�B
i*B
iDB
iyB
i_B
iyB
i�B
j0B
jB
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l"B
lWB
l=B
lWB
l�B
l�B
l�B
l�B
mwB
m�B
m�B
m�B
n}B
oB
oiB
oOB
pUB
qB
p�B
q'B
q'B
q[B
rB
raB
r|B
raB
r�B
r|B
r�B
r�B
sMB
s�B
shB
sMB
s�B
t�B
utB
utB
u�B
u�B
u?B
u�B
vB
u�B
utB
u�B
u�B
u?B
u�B
v`B
v+B
vB
vzB
vzB
v�B
w�B
w�B
w�B
wfB
v�B
v�B
wfB
xB
xB
w�B
w�B
x8B
w�B
x�B
xB
x�B
xB
w�B
w�B
xB
x�B
y�B
{B
{0B
z�B
z�B
z�B
z�B
zDB
z�B
{B
{B
{0B
{�B
{�B
{JB
{dB
{B
{B
|jB
|�B
}<B
}�B
}�B
}�B
}�B
~B
~B
~BB
~�B
~�B
~�B
~�B
~�B
HB
B
�B
}B
B
cB
~�B
~�B
~�B
~�B
~�B
~�B
B
cB
HB
�B
�B
�4B
�iB
��B
�;B
�UB
��B
��B
��B
�B
��B
�'B
��B
��B
��B
�B
��B
�uB
�uB
�uB
��B
��B
�uB
�uB
��B
��B
�uB
��B
�GB
�{B
��B
��B
��B
��B
��B
�B
�?B
��B
��B
��B
�B
��B
�fB
�B
�B
��B
�B
�B
�KB
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�B
�RB
��B
��B
�#B
�	G�O�B	B	CB	kB	7B	�B		B	�B	B	�B	kB	=B	�B	B	�B	B	eB	�B	=B	�B	�B	�B	=B	qB	�B	B	eB	B	�B	�B	�B	�B	kB	�B	�B	B	�B	B	qB	�B	kB	�B	�B	=B	qB	7B	�B	�B	7B	�B		B	�B	eB	�B	�B	qB	7B	B	�B	=B	kB	�B	eB	7B	qB		B	�B	1B	kB	=B	qB	kB	eB	B	qB	�B	�B	�B	7B	=B	=B	kB	eB	7B	�B		B	�B	7B	qB	=B	�B	�B	�B	=B	�B	�B	B	=B		B	B	1B	7B	=B	7B	1B	B		B	�B	�B	eB	7B	�B	�B	�B	�B	�B	�B	7B	eB	_B	�B	�B	7B	1B	�B	_B	�B	�B	YB	�B	�B	eB	�B	�B	YB	_B	+B	B	�B	�B	B	�B	FB	�B	B	uB	:B	�B	JB	�B	�B	wfB	�B	�+B	�aB	ŢB	�RB	��B	��B
B
�B
6�B
,=B
5?B
2-B
>B
CaB
;dB
6�B
6�B
7�B
9�B
B�B
I�B
F?B
N�B
_�B
YB
_B
a�B
b�B
qAB
qAB
YB
R�B
UgB
S[B
W�B
W?B
|B
poB
h
B
r|B
]�B
d&B
qB
m]B
|PB
yrB
qvB
o�B
n�B
x8B
��B
uZB
.B
�AB
�B
�B
��B
��B
��B
�FB
��B
��B
�1B
�MB
�iB
}�B
�4B
{�B
}�B
}"B
}�B
� B
��B
�~B
�	B
��B
�B
�oB
��B
��B
�	B
�rB
�:B
�IB
�LB
��B
�'B
� B
�B
�>B
��B
�QB
�yB
�8B
�B
�B
�B
�B
�WB
�BB
�B
�,B
�WB
��B
�TB
� B
уB
�B
�vB
ܒB
چB
�mB
�B
�B
�B
�KB
�B
�mB�BVB6zBU2Bk�Bz�Bw2Bg�BdZBd&B^�BXyBS�BO�BM�BJ�BL�BPHBM6BP}BP}BS�BVB1�B�B{BDBMB
��B�B"hBK^Be,BAUB>wB@�B>�B-CB�B�B
�DB
�.B
�B
�B
��B
�xBBB=<BW�BV�BU�Be�Bl"Bi�Bo Bp�Bt�B|PB�xB�hB�bB��B�(B��B��B��B� BcB{By�B��B��B��B�6BуB��B�aB�B�|B��B�`B��B��B�ZB�B�B՛BѷB҉BϫB�jB�0B�KB�<B�RB��B�}B��B��B��B�6B�^B��B�*B��B�B�B�wB�0B��B�*B�XB��B�zB�nB�~B�IB��B�=B��B�OB��B�IB��B�B��B�CB��B�_B��B��B�B��B��B�bB�"B��B�~B�xB�fB�+B�%B��B��B�GB�BzDB{�B{JB�GB�4BYBXBF?B?}BNpB49B4�B+B0�B/�B�B7B�B(BhB�BoB
�>B
��B
�B
�ZB
�KB
��B
��B
��B
�wB
��B
��B
��B
�nB
�B
�FB
��B
��B
�4B
��B
�kB
�FB
��B
��B
�GB
�VB
��B
��B
�AB
~�B
� B
{JB
x�B
x�B
u�B
q�B
|�B
�B
kB
_�B
OvB
FB
P�B
FtB
J�B
I�B
?�B
5tB
(�B
&LB
)�B
0�B
.�B
.B
1�B
,B
2aB
'�B
!�B
B
IB
~B
 �B
B
�B
�B
MB
B
VB

�B
+B
AB
PB
�B	�(B	��B	�B	�fB	�pB	�gB	�sB	ٴB	�BB	�0B	��B	�HB	�RB	��B	��B	��B	��B	��B	��B	�PB	�B	~�B	��B	{B	}�B	p;B	p;B	j�B	i�B	c�B	bNB	W�B	��B	n�B	HKB	E9B	<�B	:�B	-�B	1[B	-�B	2-B	'RB	($B	.B	!-B	!�B	�B	!B	OB	~B	 'B	!�B	CB	�B	�B	7B	�B	!�B	.�B	#�B	.�B	!-B	OB	�B	~B	#nB	)�B	.�B	9�B	7�B	0�B	+B	)�B	+kB	1[B	4�B	/OB	(�B	*�B	3�B	$�B	�B	�B	�B	�B	�B	�B	 �B	�B��B��B��B�B�B��B�WB�WB��B�8B��B	�B��B�B�
B�KB�ZBںB�B��B��B�BچB�QB�B�B�9B��BӏB��BɺB��B��BںB˒B��B�0B̘B��B�XB��B��BʌB�B��B҉BیB�?B��B�)B�EB��B��BںB�B�"B��B�>B�(B	 iB	uB	�B	
	B	PB	oB	�B	~B	�B	!bB	&�B	/OB	4nB	9XB	@�B	H�B	T�B	U�B	]�B	b�B	bNB	^5B	\]B	_B	a�B	aB	c�B	g8B	s�B	u%B	rB	t�B	v+B	p;B	ncB	tB	o5B	t�B	r�B	q�B	rB	t�B	u�B	~�B	y�B	|�B	{B	y�B	x�B	y�B	y>B	xlB	y�B	zB	|�B	��B	��B	��B	�B	�YB	�SB	��B	��B	��B	��B	��B	��B	�	B	�lG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<o��<�]�<��m<(g�<V/�<�PW<�-u<���<�0<�w&<��~<� ,<>��<M�<�Ğ<��><�:8<'Y|<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<kJ�<�FS<�Ğ<�=�<)��<#�
<f"�<N<�<�p4<���<-�<#�
<V/�<���<v$Z<��L<1�a<z�<�y<#�
<#�
<?��<FN�<#�
<S	<��W<<�T<#�
<#�
<#�
<;>~<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018111723382220181117233822IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010320190106200103QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010320190106200103QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551220190521075512IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                