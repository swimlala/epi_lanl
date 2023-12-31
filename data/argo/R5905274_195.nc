CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-11T19:01:12Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɠ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  р   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � `   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � g    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230511190112  20230511190112  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�'����@�'����11  @�'��l. @�'��l. @0=�^N@0=�^N�d;�e+��d;�e+�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?�  @�\@B�\@�  @��\@�G�@�G�@��RA  A   A,��A@��A`  A�Q�A�  A��A�  A�Q�A�  A�  A�Q�B (�B�
B  B  B�
B'�
B/�
B7�B?�BG�BO�
BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�{B�{B��B��B��B�  B��B��B�  B�  B��B�{B�=qB�(�B�{B��B��
B�  B�{B�  B�{B�  B��
B��
B�  B�{B�{B��C   C
=C  C  C
=C

=C��C
=C  C
=C(�C��C��C  C
=C  C��C!��C$
=C&
=C'��C*  C,  C.  C/��C1��C3��C5��C8  C:  C;��C>  C@
=CA��CC��CE��CH  CJ  CK��CM��CP  CR
=CT  CV  CW�CY��C\  C^  C`
=Cb  Cc��Cf  Ch
=Ci��Ck��Cn
=Cp  Cq��Ct  Cv
=Cx
=Cz  C|  C~  C�  C�  C���C�  C�C�C�  C�C�  C�  C���C���C�  C�
=C�
=C�C�  C�C�C�C�  C�
=C�C���C�  C�C�  C�C���C�  C�C�  C�C�  C���C���C�  C�C�  C���C���C���C���C���C���C���C���C�  C�\C�
=C�C���C���C���C���C���C���C���C���C���C�C���C���C���C�  C�C���C���C���C�C�  C���C�C�  C�  C�C�  C���C���C��C�  C�  C���C���C�  C�  C��C�C�  C���C�  C���C�C�C�  C���C���C�C�  C���C�C�C�  C���C���C���C���C���C���C�C�C�  C�C���C���C�C�C�  C�  C�  C�C�  C���C�  C�  C���C�C�  C���D }qD�D� D  D��D�qD}qD�D� D�qD��DD��D  D� D��Dz�D�qD	��D
  D
� D
�qD}qD��D� D  D}qD�qD� D�qD� D�qD� DD��D�D�D  DxRD��D� DD��D�qD� D  D��D�D�D  D� D�D��DD��D  D��D  Dz�D�qD��D�D��D �D � D!  D!��D"�D"�D#  D#z�D#�qD$}qD$�qD%��D&D&��D'  D'z�D(  D(�D)�D)}qD)�qD*xRD*�RD+}qD+��D,z�D,��D-}qD-�qD.u�D.�qD/�D/�qD0xRD0�RD1z�D1�qD2}qD2�qD3� D4�D4��D5D5��D6�D6�D7  D7��D8�D8��D9D9� D9��D:z�D:�qD;}qD<  D<z�D=  D=�D>D>� D?�D?z�D?��D@�DA�DA� DA��DB�DCDC}qDD  DD� DE�DEz�DF  DF��DG
=DG��DG�qDH� DI�DI}qDI��DJ}qDK  DK��DL  DL� DM  DM}qDNDN��DO  DO}qDP  DP� DP�qDQ� DR  DR� DS�DS��DT  DT��DU  DU}qDV  DV}qDV��DW� DX  DXz�DX�qDY��DZ  DZ}qD[  D[}qD\  D\�D\�qD]� D^  D^}qD_  D_� D`  D`� D`�qDa� Db�Db��Dc�Dc� Dd  Dd}qDd�qDe}qDe�qDf��Dg�Dgz�Dh  Dh��Dh�qDi��DjDj� Dj�qDk}qDl�Dl� Dl�qDm� Dn  Dn��Do  Do� Dp�Dp� Dp��Dqz�Dr  Dr��Ds  Dsz�Ds�qDt��Du�Du��Dv  Dvz�Dw  Dw��Dx�Dx�Dy�Dy� Dy�qDz� D{�D{��D{��D|� D}D}�D~  D~}qD~��D}qD�HD�@ D���D��HD���D�>�D�~�D�� D�  D�AHD�� D��)D�  D�C�D��HD�� D�HD�AHD�~�D��qD�  D�@ D��HD��HD���D�B�D���D���D���D�AHD���D�D���D�>�D��HD�� D�  D�@ D�|)D�� D�HD�=qD�~�D��HD��D�>�D�� D�D�  D�AHD�}qD���D�HD�B�D��HD�D�HD�@ D�� D��HD�HD�>�D��HD��HD��)D�>�D�� D���D�  D�B�D�}qD���D���D�=qD��HD���D���D�>�D�~�D�D���D�@ D��HD��qD��D�AHD�� D��qD��D�@ D�}qD�� D�  D�@ D�� D�D�HD�@ D�}qD���D�  D�@ D��HD�� D���D�AHD��HD�D��qD�AHD�� D�� D��qD�@ D�� D�D��qD�AHD���D�� D�  D�@ D�� D��qD���D�>�D���D��HD�  D�>�D�}qD���D�  D�>�D�~�D���D���D�AHD��HD��HD��D�AHD��HD�� D�HD�@ D�~�D���D���D�>�D�}qD�� D���D�@ D�� D���D�  D�@ D�� D��HD�  D�@ D�~�D��HD�HD�>�D�� D�� D�  D�@ D�~�D���D���D�>�D�� D��qD���D�@ D�� D�� D�HD�>�D�� D���D�HD�@ D���D�� D���D�AHD�~�D���D�  D�>�D�~�D�� D��D�AHD�� D���D�HD�@ D�� D���D�  D�=qD���D�� D�  D�AHD�}qD�D�HD�>�D��HD���D�  D�AHD�~�D�� D�  D�AHD�~�D��HD���D�>�D�� D���D�  D�@ D�~�D��HD��D�AHD�~�D��HD��qD�@ D�~�D�� D�HD�=qD�� D�� D��D�>�D�� D�D�HD�B�D���D��HD���D�=qD�� D��HD���D�@ D�~�D���D���D�@ D�� D���D�  D�AHD�~�D��HD���D�@ DÀ D�D�  D�>�D�~�DĽqD�HD�>�DŁHD�� D�HD�AHD�}qD�� D�  D�B�D�~�DǾ�D��D�=qDȀ D�� D�  D�>�Dɀ Dɾ�D���D�>�D�~�D�� D�  D�@ D�~�D˽qD�  D�AHD̀ D�� D�HD�@ D�~�D�� D�  D�AHD΁HD�D�  D�>�Dπ D��HD���D�@ DЀ Dо�D���D�@ Dр D�� D���D�@ DҀ D�D��D�AHDӁHDӾ�D�  D�AHDԁHD��HD�  D�AHDՁHD��HD���D�>�Dր D�� D�  D�>�D׀ D�� D�  D�@ D؀ D�� D�HD�@ Dـ D�� D�  D�@ DځHD�D�  D�>�Dۀ D�� D���D�@ D܀ Dܾ�D���D�@ D�~�D�� D��qD�@ Dހ D�� D�  D�@ D߁HD�� D�  D�>�D�~�D�� D�HD�@ D� DᾸD���D�@ D� D�� D�HD�AHD�~�D㾸D�  D�>�D�~�D�� D�HD�>�D�~�D��HD�HD�AHD� D�D�HD�@ D� D�� D�  D�>�D�HD�� D�  D�@ D�~�D龸D���D�>�D�~�D�� D�HD�>�D낏D�� D���D�>�D� D��HD�HD�@ D�}qD�� D�HD�AHD�HD�� D�  D�>�D�HD�� D���D�@ D��HD��HD�  D�@ D�HD��HD�HD�@ D�}qD�D���D�>�D�HD�� D���D�>�D�HD��HD���D�=qD�� D���D�  D�>�D��HD��HD�HD�@ D�~�D��qD��D�>�D�~�D�� D��qD�@ D��HD��HD�HD�B�D�n>�G�?��?u?�33?��@
=q@(�@0��@J=q@aG�@�  @�=q@�Q�@��
@�\)@���@��@�\)@ٙ�@�ff@��@��RAz�A
=qAG�A
=A{A#�
A(��A.�RA3�
A:=qA>�RAB�\AG
=AL(�AQ�AVffA[�A`��Ae�Aj=qAn�RAr�\Aw�A{�A~�RA���A��\A�(�A�A��A���A��A�33A��A��RA�\)A���A��\A�(�A�p�A�
=A�Q�A�=qA��A��A��RA�Q�A��A��A�p�A��RA�Q�A�=qA��A�{A�Q�A�=qA��
A�p�A�\)A�G�A�33A��A�\)A���A\A�z�A�ffA�  A��A��
A�p�A�{A�Q�A��A��
A�p�A�\)A���A��HA�z�A޸RA��A�33A��A�
=A���A��HA��A�
=A���A��HA��A�ffA�  A���A��
A�A��B ��B�B
=B(�BG�BffB\)BQ�B	G�B
ffB\)B(�B��B��BffB\)BQ�B��B�B�RB�Bz�B��B�\B\)B(�B��BB�RB�Bz�Bp�B{B33B Q�B!G�B"ffB#\)B$z�B%��B&�RB(  B)�B*=qB+\)B,Q�B-p�B.ffB/\)B0(�B1G�B2=qB3\)B4(�B5p�B6�\B7�B8��B9�B;
=B<(�B=G�B>=qB?\)B@Q�BAp�BB�\BC�BD(�BEp�BF=qBG�BH��BI�BK33BLQ�BMp�BNffBO�BP��BQ��BRffBS�BT��BUBV�HBX  BY�BZ=qB[�B\��B^=qB_33B`z�Bap�BbffBc\)Bd��BeBf�HBg�
BiG�Bj�\Bk�
Bm�Bn=qBo\)Bp(�BqG�BrffBs�Bt��Bv=qBw�Bx��Bz{B{33B|z�B}p�B~=qB�B�Q�B���B�B�Q�B��HB��B�  B�z�B�
=B��B�(�B��HB�p�B�(�B���B�\)B�  B��\B�33B��B�{B���B�G�B�B�ffB�33B�B�ffB��RB�\)B��B���B�\)B�  B���B�G�B��
B�=qB��HB��B�(�B��RB��B�=qB���B�p�B��
B�z�B���B���B�(�B���B���B�(�B���B�p�B��
B�Q�B�
=B��B�=qB�
=B��B�ffB���B�\)B��B���B�p�B�{B���B�\)B��
B�z�B��B��B���B�G�B�B�ffB��B��B���B�33B��B�ffB�
=B��
B��\B�G�B��B�ffB���B�B��\B�G�B��B�ffB�
=B��
B�z�B�33B�B�=qB��HB���B�z�B��B�B�=qB��HB�B�ffB��BǙ�B�=qBȸRBɅB�Q�B���B�\)B�  B̸RBͅB�(�BΏ\B�33B��
BиRB�\)B�  B�z�B��BӮBԏ\B�33B��
B�(�B��HB�p�B�=qB���Bٙ�B�{BڸRBۙ�B�=qBܸRB�\)B�  B���B߅B�=qB��B�\)B�  B���B�B��B��B�\)B�(�B���B�33B��B��B�\)B�{B�z�B��B��B�\B�
=B��B�=qB��B��
B�=qB��HB�B�Q�B��HB�\)B��B���B��B��B��\B�p�B�  B�z�B�
=B��B��\B���B���B��\B�33B���B�(�B��B��
C {C p�C ��C(�C�C�RC
=C�C��C
=CQ�C��C�CQ�C��C�CffC��C�C\)C�C�HC33C�C�C(�Cp�C��C	33C	�\C	C

=C
z�C
��C
=C\)CC{CQ�C��C{CQ�C��C{Cp�C��C  Cp�CC��CG�C�C
=C=qC��C�C(�C\)C�C�C
=C33Cz�C�C�RC�
C
=C
=C�CQ�Cp�CffCz�C�C��CC�
C
=C(�C33C=qC\)C�\C��C��C�RC��C{C
=C(�CffCz�C�C��C��C�C��C�CQ�Cp�Cz�C��C��C�HC  C(�C\)Cp�C�C�RC�HC�C
=CG�Cp�Cp�C��C��C�HC  C33C\)CffC�CC��C�C(�CQ�CQ�Cz�C�RC�
C�HC��C=qCffCffC��C��C��C�C(�CQ�C\)C�C�CC�
C�C=qC=qCp�C��C�C��C
=C{C(�Cp�C�\C��C��C��C 
=C 33C ffC p�C �C �
C �
C!
=C!=qC!G�C!ffC!��C!�C!��C"{C"33C"=qC"�C"�C"�RC"��C#�C#�C#Q�C#�C#�\C#�C#�C$
=C$(�C$ffC$�C$�\C$��C$�C$��C%�C%Q�C%p�C%z�C%C%�HC%�HC&(�C&G�C&G�C&z�C&�C&�C&�HC'{C'{C'G�C'z�C'z�C'��C'�
C'�
C(  C(=qC(=qC(p�C(��C(��C(�
C)  C){C)G�C)p�C)p�C)�C)�
C)�HC*�C*G�C*G�C*�C*�RC*�RC*��C+�C+(�C+\)C+�\C+��C+�
C+��C,  C,G�C,p�C,z�C,�C,�HC,�C-(�C-Q�C-\)C-��C-�C-��C.�C.�C.\)C.�\C.��C.�
C.�C/  C/Q�C/\)C/��C/��C/�
C0{C0G�C0Q�C0��C0��C0�
C1{C1Q�C1\)C1�\C1��C1�HC2{C2Q�C2ffC2�C2�
C2�C333C3G�C3p�C3�RC3��C4  C4G�C4\)C4��C4C4�HC5(�C5G�C5ffC5�C5�RC5��C6=qC6G�C6�\C6��C6��C7{C7(�C7p�C7��C7�C8  C8�C8=qC8�C8��C8�
C9{C9(�C9ffC9��C9�C9��C:{C:Q�C:�\C:��C:�C;�C;33C;�C;�\C;��C<�C<(�C<p�C<��C<�RC=
=C=�C=p�C=��C=�RC>  C>�C>Q�C>��C>�C?  C?�C?Q�C?��C?�RC@  C@{C@G�C@��C@�RCA  CA�CA\)CA��CA�RCB
=CB(�CBQ�CB��CBCC
=CC(�CCQ�CC��CCCD{CD(�CDz�CD�\CD��CE{CE(�CEp�CE�\CE��CF{CF(�CFp�CF��CF�HCG
=CG33CGz�CG�\CG�HCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411141114111111114111411111111111111111111111111411111111111114111111141141111111111411111111111111111141111111111111114111111111111111111141141141141141141141141111141111141141111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                               ?�  @�\@B�\@�  @��\@�G�@�G�@��RA  A   A,��A@��A`  A�Q�A�  A��A�  A�Q�A�  A�  A�Q�B (�B�
B  B  B�
B'�
B/�
B7�B?�BG�BO�
BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�{B�{B��B��B��B�  B��B��B�  B�  B��B�{B�=qB�(�B�{B��B��
B�  B�{B�  B�{B�  B��
B��
B�  B�{B�{B��C   C
=C  C  C
=C

=C��C
=C  C
=C(�C��C��C  C
=C  C��C!��C$
=C&
=C'��C*  C,  C.  C/��C1��C3��C5��C8  C:  C;��C>  C@
=CA��CC��CE��CH  CJ  CK��CM��CP  CR
=CT  CV  CW�CY��C\  C^  C`
=Cb  Cc��Cf  Ch
=Ci��Ck��Cn
=Cp  Cq��Ct  Cv
=Cx
=Cz  C|  C~  C�  C�  C���C�  C�C�C�  C�C�  C�  C���C���C�  C�
=C�
=C�C�  C�C�C�C�  C�
=C�C���C�  C�C�  C�C���C�  C�C�  C�C�  C���C���C�  C�C�  C���C���C���C���C���C���C���C���C�  C�\C�
=C�C���C���C���C���C���C���C���C���C���C�C���C���C���C�  C�C���C���C���C�C�  C���C�C�  C�  C�C�  C���C���C��C�  C�  C���C���C�  C�  C��C�C�  C���C�  C���C�C�C�  C���C���C�C�  C���C�C�C�  C���C���C���C���C���C���C�C�C�  C�C���C���C�C�C�  C�  C�  C�C�  C���C�  C�  C���C�C�  C���D }qD�D� D  D��D�qD}qD�D� D�qD��DD��D  D� D��Dz�D�qD	��D
  D
� D
�qD}qD��D� D  D}qD�qD� D�qD� D�qD� DD��D�D�D  DxRD��D� DD��D�qD� D  D��D�D�D  D� D�D��DD��D  D��D  Dz�D�qD��D�D��D �D � D!  D!��D"�D"�D#  D#z�D#�qD$}qD$�qD%��D&D&��D'  D'z�D(  D(�D)�D)}qD)�qD*xRD*�RD+}qD+��D,z�D,��D-}qD-�qD.u�D.�qD/�D/�qD0xRD0�RD1z�D1�qD2}qD2�qD3� D4�D4��D5D5��D6�D6�D7  D7��D8�D8��D9D9� D9��D:z�D:�qD;}qD<  D<z�D=  D=�D>D>� D?�D?z�D?��D@�DA�DA� DA��DB�DCDC}qDD  DD� DE�DEz�DF  DF��DG
=DG��DG�qDH� DI�DI}qDI��DJ}qDK  DK��DL  DL� DM  DM}qDNDN��DO  DO}qDP  DP� DP�qDQ� DR  DR� DS�DS��DT  DT��DU  DU}qDV  DV}qDV��DW� DX  DXz�DX�qDY��DZ  DZ}qD[  D[}qD\  D\�D\�qD]� D^  D^}qD_  D_� D`  D`� D`�qDa� Db�Db��Dc�Dc� Dd  Dd}qDd�qDe}qDe�qDf��Dg�Dgz�Dh  Dh��Dh�qDi��DjDj� Dj�qDk}qDl�Dl� Dl�qDm� Dn  Dn��Do  Do� Dp�Dp� Dp��Dqz�Dr  Dr��Ds  Dsz�Ds�qDt��Du�Du��Dv  Dvz�Dw  Dw��Dx�Dx�Dy�Dy� Dy�qDz� D{�D{��D{��D|� D}D}�D~  D~}qD~��D}qD�HD�@ D���D��HD���D�>�D�~�D�� D�  D�AHD�� D��)D�  D�C�D��HD�� D�HD�AHD�~�D��qD�  D�@ D��HD��HD���D�B�D���D���D���D�AHD���D�D���D�>�D��HD�� D�  D�@ D�|)D�� D�HD�=qD�~�D��HD��D�>�D�� D�D�  D�AHD�}qD���D�HD�B�D��HD�D�HD�@ D�� D��HD�HD�>�D��HD��HD��)D�>�D�� D���D�  D�B�D�}qD���D���D�=qD��HD���D���D�>�D�~�D�D���D�@ D��HD��qD��D�AHD�� D��qD��D�@ D�}qD�� D�  D�@ D�� D�D�HD�@ D�}qD���D�  D�@ D��HD�� D���D�AHD��HD�D��qD�AHD�� D�� D��qD�@ D�� D�D��qD�AHD���D�� D�  D�@ D�� D��qD���D�>�D���D��HD�  D�>�D�}qD���D�  D�>�D�~�D���D���D�AHD��HD��HD��D�AHD��HD�� D�HD�@ D�~�D���D���D�>�D�}qD�� D���D�@ D�� D���D�  D�@ D�� D��HD�  D�@ D�~�D��HD�HD�>�D�� D�� D�  D�@ D�~�D���D���D�>�D�� D��qD���D�@ D�� D�� D�HD�>�D�� D���D�HD�@ D���D�� D���D�AHD�~�D���D�  D�>�D�~�D�� D��D�AHD�� D���D�HD�@ D�� D���D�  D�=qD���D�� D�  D�AHD�}qD�D�HD�>�D��HD���D�  D�AHD�~�D�� D�  D�AHD�~�D��HD���D�>�D�� D���D�  D�@ D�~�D��HD��D�AHD�~�D��HD��qD�@ D�~�D�� D�HD�=qD�� D�� D��D�>�D�� D�D�HD�B�D���D��HD���D�=qD�� D��HD���D�@ D�~�D���D���D�@ D�� D���D�  D�AHD�~�D��HD���D�@ DÀ D�D�  D�>�D�~�DĽqD�HD�>�DŁHD�� D�HD�AHD�}qD�� D�  D�B�D�~�DǾ�D��D�=qDȀ D�� D�  D�>�Dɀ Dɾ�D���D�>�D�~�D�� D�  D�@ D�~�D˽qD�  D�AHD̀ D�� D�HD�@ D�~�D�� D�  D�AHD΁HD�D�  D�>�Dπ D��HD���D�@ DЀ Dо�D���D�@ Dр D�� D���D�@ DҀ D�D��D�AHDӁHDӾ�D�  D�AHDԁHD��HD�  D�AHDՁHD��HD���D�>�Dր D�� D�  D�>�D׀ D�� D�  D�@ D؀ D�� D�HD�@ Dـ D�� D�  D�@ DځHD�D�  D�>�Dۀ D�� D���D�@ D܀ Dܾ�D���D�@ D�~�D�� D��qD�@ Dހ D�� D�  D�@ D߁HD�� D�  D�>�D�~�D�� D�HD�@ D� DᾸD���D�@ D� D�� D�HD�AHD�~�D㾸D�  D�>�D�~�D�� D�HD�>�D�~�D��HD�HD�AHD� D�D�HD�@ D� D�� D�  D�>�D�HD�� D�  D�@ D�~�D龸D���D�>�D�~�D�� D�HD�>�D낏D�� D���D�>�D� D��HD�HD�@ D�}qD�� D�HD�AHD�HD�� D�  D�>�D�HD�� D���D�@ D��HD��HD�  D�@ D�HD��HD�HD�@ D�}qD�D���D�>�D�HD�� D���D�>�D�HD��HD���D�=qD�� D���D�  D�>�D��HD��HD�HD�@ D�~�D��qD��D�>�D�~�D�� D��qD�@ D��HD��HD�HD�B�D�n>�G�?��?u?�33?��@
=q@(�@0��@J=q@aG�@�  @�=q@�Q�@��
@�\)@���@��@�\)@ٙ�@�ff@��@��RAz�A
=qAG�A
=A{A#�
A(��A.�RA3�
A:=qA>�RAB�\AG
=AL(�AQ�AVffA[�A`��Ae�Aj=qAn�RAr�\Aw�A{�A~�RA���A��\A�(�A�A��A���A��A�33A��A��RA�\)A���A��\A�(�A�p�A�
=A�Q�A�=qA��A��A��RA�Q�A��A��A�p�A��RA�Q�A�=qA��A�{A�Q�A�=qA��
A�p�A�\)A�G�A�33A��A�\)A���A\A�z�A�ffA�  A��A��
A�p�A�{A�Q�A��A��
A�p�A�\)A���A��HA�z�A޸RA��A�33A��A�
=A���A��HA��A�
=A���A��HA��A�ffA�  A���A��
A�A��B ��B�B
=B(�BG�BffB\)BQ�B	G�B
ffB\)B(�B��B��BffB\)BQ�B��B�B�RB�Bz�B��B�\B\)B(�B��BB�RB�Bz�Bp�B{B33B Q�B!G�B"ffB#\)B$z�B%��B&�RB(  B)�B*=qB+\)B,Q�B-p�B.ffB/\)B0(�B1G�B2=qB3\)B4(�B5p�B6�\B7�B8��B9�B;
=B<(�B=G�B>=qB?\)B@Q�BAp�BB�\BC�BD(�BEp�BF=qBG�BH��BI�BK33BLQ�BMp�BNffBO�BP��BQ��BRffBS�BT��BUBV�HBX  BY�BZ=qB[�B\��B^=qB_33B`z�Bap�BbffBc\)Bd��BeBf�HBg�
BiG�Bj�\Bk�
Bm�Bn=qBo\)Bp(�BqG�BrffBs�Bt��Bv=qBw�Bx��Bz{B{33B|z�B}p�B~=qB�B�Q�B���B�B�Q�B��HB��B�  B�z�B�
=B��B�(�B��HB�p�B�(�B���B�\)B�  B��\B�33B��B�{B���B�G�B�B�ffB�33B�B�ffB��RB�\)B��B���B�\)B�  B���B�G�B��
B�=qB��HB��B�(�B��RB��B�=qB���B�p�B��
B�z�B���B���B�(�B���B���B�(�B���B�p�B��
B�Q�B�
=B��B�=qB�
=B��B�ffB���B�\)B��B���B�p�B�{B���B�\)B��
B�z�B��B��B���B�G�B�B�ffB��B��B���B�33B��B�ffB�
=B��
B��\B�G�B��B�ffB���B�B��\B�G�B��B�ffB�
=B��
B�z�B�33B�B�=qB��HB���B�z�B��B�B�=qB��HB�B�ffB��BǙ�B�=qBȸRBɅB�Q�B���B�\)B�  B̸RBͅB�(�BΏ\B�33B��
BиRB�\)B�  B�z�B��BӮBԏ\B�33B��
B�(�B��HB�p�B�=qB���Bٙ�B�{BڸRBۙ�B�=qBܸRB�\)B�  B���B߅B�=qB��B�\)B�  B���B�B��B��B�\)B�(�B���B�33B��B��B�\)B�{B�z�B��B��B�\B�
=B��B�=qB��B��
B�=qB��HB�B�Q�B��HB�\)B��B���B��B��B��\B�p�B�  B�z�B�
=B��B��\B���B���B��\B�33B���B�(�B��B��
C {C p�C ��C(�C�C�RC
=C�C��C
=CQ�C��C�CQ�C��C�CffC��C�C\)C�C�HC33C�C�C(�Cp�C��C	33C	�\C	C

=C
z�C
��C
=C\)CC{CQ�C��C{CQ�C��C{Cp�C��C  Cp�CC��CG�C�C
=C=qC��C�C(�C\)C�C�C
=C33Cz�C�C�RC�
C
=C
=C�CQ�Cp�CffCz�C�C��CC�
C
=C(�C33C=qC\)C�\C��C��C�RC��C{C
=C(�CffCz�C�C��C��C�C��C�CQ�Cp�Cz�C��C��C�HC  C(�C\)Cp�C�C�RC�HC�C
=CG�Cp�Cp�C��C��C�HC  C33C\)CffC�CC��C�C(�CQ�CQ�Cz�C�RC�
C�HC��C=qCffCffC��C��C��C�C(�CQ�C\)C�C�CC�
C�C=qC=qCp�C��C�C��C
=C{C(�Cp�C�\C��C��C��C 
=C 33C ffC p�C �C �
C �
C!
=C!=qC!G�C!ffC!��C!�C!��C"{C"33C"=qC"�C"�C"�RC"��C#�C#�C#Q�C#�C#�\C#�C#�C$
=C$(�C$ffC$�C$�\C$��C$�C$��C%�C%Q�C%p�C%z�C%C%�HC%�HC&(�C&G�C&G�C&z�C&�C&�C&�HC'{C'{C'G�C'z�C'z�C'��C'�
C'�
C(  C(=qC(=qC(p�C(��C(��C(�
C)  C){C)G�C)p�C)p�C)�C)�
C)�HC*�C*G�C*G�C*�C*�RC*�RC*��C+�C+(�C+\)C+�\C+��C+�
C+��C,  C,G�C,p�C,z�C,�C,�HC,�C-(�C-Q�C-\)C-��C-�C-��C.�C.�C.\)C.�\C.��C.�
C.�C/  C/Q�C/\)C/��C/��C/�
C0{C0G�C0Q�C0��C0��C0�
C1{C1Q�C1\)C1�\C1��C1�HC2{C2Q�C2ffC2�C2�
C2�C333C3G�C3p�C3�RC3��C4  C4G�C4\)C4��C4C4�HC5(�C5G�C5ffC5�C5�RC5��C6=qC6G�C6�\C6��C6��C7{C7(�C7p�C7��C7�C8  C8�C8=qC8�C8��C8�
C9{C9(�C9ffC9��C9�C9��C:{C:Q�C:�\C:��C:�C;�C;33C;�C;�\C;��C<�C<(�C<p�C<��C<�RC=
=C=�C=p�C=��C=�RC>  C>�C>Q�C>��C>�C?  C?�C?Q�C?��C?�RC@  C@{C@G�C@��C@�RCA  CA�CA\)CA��CA�RCB
=CB(�CBQ�CB��CBCC
=CC(�CCQ�CC��CCCD{CD(�CDz�CD�\CD��CE{CE(�CEp�CE�\CE��CF{CF(�CFp�CF��CF�HCG
=CG33CGz�CG�\CG�HCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411141114111111114111411111111111111111111111111411111111111114111111141141111111111411111111111111111141111111111111114111111111111111111141141141141141141141141111141111141141111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aӣ�Aӣ�Aӡ�Aӣ�Aӥ�Aӣ�Aӣ�AӬAӲ-AӬAӮAӮAӣ�Aӥ�AӬAӲ-AӶFA�AӼjAӺ^AӾwAӼjAӶFAӣ�AӍPA�`BAҬAљ�A�XA��A�ƨAГuA�9XAϺ^AσA�^5A�O�A�9XAΩ�A�bA��mA���AͮA�Q�A�ƨA�hsA��#A�VA�z�AɾwAȬA�1'A��A�Aǉ7A�ȴA�^5Aş�A�S�A��;A�G�A��TA�hsA�oA���AA��wA���A�bA�-A��`A�%A�|�A�O�A��A�5?A�p�A�?}A�bNA�1A�I�A�"�A��A�E�A��`A��!A�hsA���A�`BA���A�z�A��A�ffA��jA��^A�  A�"�A��DA�VA�+A��wA���A�~�A���A��HA�jA�JA��A}��A{7LAy��AwhsAu�Ar��Ap�\An��An��AnZAmx�Ak��AhQ�Ae�7Ab��A_�7A]��A]S�A\ZA[�^A[33AY�ARv�AP�jANr�AJn�AF�AD�uAC�AB��A@=qA<�9A:z�A9/A6�A6=qA4M�A1�TA0��A/�;A.�jA.=qA,�HA-S�A,I�A)%A'�A'�A&v�A%��A$��A#��A"�+A"�A!�#A �+A��A�A��Ap�A;dAVA�yAA�A��AS�A��At�A��A�AO�A�yA�\A^5AdZA1'AA��Ax�A�AI�A�/A�A��A
ĜA
��A
��A�AVA
5?A	+A��A�yA��A$�Al�@���@�?}@��j@�1@�"�@��@�O�@�Z@��@�;d@�o@���@���@�5?@���@��@�M�@�^@��D@���@�x�@�Z@��@�33@�@�V@�9@�I�@���@�;d@柾@�{@�@�@�`B@�O�@�%@�r�@��@� �@��@�ƨ@���@�=q@�^@�7L@�(�@�t�@�ȴ@���@��@ܬ@�b@���@�x�@�%@ج@�1@�l�@�;d@�C�@�+@���@և+@���@��@�ȴ@ҧ�@�n�@�$�@�5?@�E�@�J@���@�%@�l�@�@͉7@���@̓u@�bN@��m@�"�@ʰ!@�-@���@�x�@ɲ-@�hs@�7L@��/@���@� �@Ǖ�@�"�@�S�@�@���@Ɵ�@ź^@��@��/@�j@��;@�S�@�@¸R@�=q@�$�@�$�@�E�@�E�@�Z@�(�@��
@�dZ@���@�I�@�(�@��;@��m@� �@�1'@���@�dZ@��@��!@��#@���@���@��7@�x�@��@��u@���@���@���@��u@���@�o@���@��@���@��-@�O�@��@���@���@�Q�@��@�|�@�"�@��@�o@�@���@��+@�5?@���@�p�@�G�@���@���@���@�Ĝ@�bN@��;@��@��P@�C�@���@���@���@��+@�~�@�n�@�-@���@��^@�p�@���@�z�@�K�@��y@�ȴ@��!@��+@�$�@��#@�p�@�?}@��@���@�z�@�A�@��;@��P@�l�@�"�@�~�@�M�@�E�@�5?@��@��#@��7@��@���@��j@��u@�j@�Z@��
@���@�t�@�S�@��@��@���@���@�5?@�x�@�%@���@�Z@��@��
@���@�dZ@�\)@�33@��@�n�@�ff@�V@��@�x�@��@�Ĝ@��@�Z@�Z@�j@��u@�b@��
@��@�l�@�dZ@�K�@��@���@�V@�{@��^@�X@�&�@��@�b@�"�@�ff@�M�@�5?@��@��@�@��^@��7@�7L@�%@�z�@� �@�1@��;@���@�S�@�ȴ@�M�@���@��#@��^@���@��h@�x�@�%@��@��@�\)@�S�@�S�@�K�@�o@�ȴ@���@���@�~�@�$�@�@��T@���@�G�@��/@��D@�I�@�b@��P@�o@��@��y@��@���@�5?@��@��T@���@��@�`B@�&�@��@���@��/@�r�@�1'@�1@���@���@�K�@�"�@�o@��@�ȴ@��\@�n�@�V@�{@�@��h@�X@�V@���@��@���@�9X@�(�@�1@�t�@�K�@�
=@��y@���@�v�@�E�@�{@���@��#@��7@�X@�O�@��@���@���@� �@��F@�t�@�;d@�@��R@�=q@�{@��@�@��7@�G�@�V@�%@���@���@��@��`@���@�r�@�@
=@~�R@~V@|��@{�F@{�@{�@{�@{S�@z=q@y7L@w
=@v�R@v��@v�+@v�+@vff@u�@u�@u?}@u/@t�@t�@tI�@s��@s�@sC�@r��@r�\@r^5@r-@q��@qX@q&�@p�9@p �@o�w@o+@o�@nv�@n$�@n@m�@m�h@mV@l�/@lj@l1@kƨ@kƨ@k��@k�@kS�@kS�@kC�@j��@j-@i��@i�^@i��@i�@h1'@hb@hb@g�;@g+@f�@f��@f5?@e/@dI�@d�@c��@c"�@b=q@a��@a�@`b@_��@_K�@_
=@^�R@^$�@]@]/@\�j@\�@[�
@[��@[o@Z�\@Zn�@Z=q@Y�@Y7L@Xr�@W
=@VE�@U�@U�-@U�@Tj@Sƨ@R�H@R�\@Rn�@R-@RJ@Q��@Q�#@Q��@Q&�@P  @O��@O�@O�@Nv�@N{@M��@M�h@Mp�@M?}@L�@L��@L�D@L9X@L1@Kt�@Ko@J�\@Jn�@I�#@I�@H�u@HQ�@Hr�@HA�@G�P@Fȴ@F�R@Fȴ@Fȴ@E�@E��@E�@Ep�@E`B@E�@EV@D�@D��@Dz�@D(�@C��@CC�@B�\@B^5@BM�@BJ@A�#@A�^@Ax�@@Ĝ@@A�@@ �@@  @?��@?l�@?l�@?K�@>�@=��@=?}@=V@<�@<�@<�j@<�@<z�@<9X@;�
@;33@:�H@:��@:��@:�\@:M�@9�#@9��@9hs@9&�@8��@8r�@7�w@7�P@7|�@7\)@7
=@6ff@6$�@5�@5@5O�@5V@4��@4��@5V@5�@4��@4j@4�@3�m@3S�@333@333@2�!@2�@1��@1G�@1&�@1%@0��@0�`@0r�@/�;@/�@/;d@/+@/
=@.ff@.{@-�@-��@-��@-��@-`B@-?}@-/@-�@,�/@,�@,1@+�@+33@*�@*��@*�!@*��@*�\@*J@)��@)G�@)&�@)%@(��@(��@(Ĝ@(�u@(A�@(b@'�w@'\)@&�@&�+@&ff@&V@&5?@&@%�T@%�@%V@$�j@$�@$��@$9X@#ƨ@#C�@#"�@#"�@#33@#"�@#"�@"�@"n�@"J@!��@!��@!��@!�#@!�@!�@!�^@!��@!x�@!G�@!&�@ ��@ Ĝ@ Ĝ@ ��@ A�@ A�@  �@�w@
=@��@{@�@V@��@�j@j@9X@��@��@C�@�!@~�@M�@=q@J@�^@�7@�`@bN@�@��@��@K�@�@�@
=@ȴ@�+@�+@��@�@�h@?}@/@V@�/@�@Z@��@ƨ@ƨ@��@�@��@S�@"�@"�@��@��@��@~�@�@�@��@��@hs@X@G�@&�@Ĝ@bN@Q�@1'@b@b@  @�;@��@�w@��@\)@��@�@ȴ@��@v�@ff@5?@�T@��@�@O�@O�@/@�@�j@�j@�@�@�@�j@�@��@��@�D@j@I�@I�@(�@��@�
@��@�@�@C�@o@
�@
��@
��@
^5@
^5@
^5@
M�@
=q@
�Aӡ�Aӧ�Aӣ�Aӥ�Aӥ�Aӣ�Aӥ�Aӣ�Aӥ�Aӣ�Aӡ�Aӣ�Aӥ�Aӥ�Aӡ�Aӡ�Aӣ�Aӡ�Aӟ�Aӥ�AӮAө�AӴ9AӴ9AӲ-AӲ-AӬAө�AӬAӬAӮAӮAө�AӰ!Aӧ�Aө�Aө�Aө�Aӣ�Aӡ�Aӣ�Aӥ�Aӡ�Aӟ�Aӡ�Aӣ�Aӣ�Aӡ�Aӡ�Aӡ�Aӥ�Aө�Aө�Aө�AӮAө�Aө�AӬAӬAӬAӬAӮAӰ!AӬAӮAӰ!AӰ!AӴ9AӼjAӺ^AӸRAӺ^AӼjAӺ^AӶFAӴ9AӴ9AӶFAӴ9AӲ-AӲ-A���A���A���A���A���AӾwAӾwA���AӾwAӺ^AӺ^AӼjAӾwAӶFA���AӶFA�A���AӾwAӾwAӺ^AӺ^AӸRAӼjAӶFAӸRAӾwA�AӾwAӾwAӺ^AӺ^AӼjAӼjAӼjAӼjAӼjA�ƨA�ȴA�ĜAӸRAӺ^AӸRAӶFAӴ9AӶFAӺ^AӶFAӴ9AӴ9AӲ-AӸRAӸRAӣ�Aӟ�Aӣ�Aӣ�Aӝ�Aӟ�Aӟ�Aӡ�Aӕ�AӇ+AӇ+Aӏ\AӍPAӃAӅAӑhAӏ\AӍPA�r�A�^5Aӗ�A�E�A�+A�p�A�{A�+A�+A�?}A�?}A�&�A��A���A�AѰ!Aѣ�Aї�AёhAэPAуA�z�A�l�A�dZA�dZA�^5A�VA�G�A�7LA�$�A�
=A��A��mA��HA��/A��#A��#A��A���A���A���A���Aк^Aа!AЧ�AУ�AН�AЛ�AЉ7AЅAЅA�t�A�dZA�ZA�K�A�;dA�/A��A���A��A���AϺ^Aϰ!AϬAϥ�Aϟ�Aϗ�AϓuAύPAσA�z�A�p�A�hsA�dZA�bNA�^5A�ZA�ZA�XA�VA�S�A�Q�A�Q�A�M�A�M�A�K�A�I�A�G�A�=qA�;dA�5?A�1'A�-A�$�A��A�oA΍PA�G�A�;dA�1'A�"�A��A��A�{A�1A���A���A��A��A��mA��mA��HA��HA��;A��#A��A��
A�ȴA�ĜA�ƨA�ƨA;wA͸RAͮAͧ�A͡�A͓uAͅA͇+A�|�A�VA�{A�VA�%A��`A���A̸RA̲-A̰!A̟�A̓uÃA�n�A�bNA�\)A�C�A�5?A�(�A�JA��A˼jA˼jA˥�A�jA�;dA�{A�%A��A���AʬAʍPA�z�A�t�A�l�A�hsA�bNA�S�A�1'A��A�p�A�A�A�VA���A���AȰ!AȑhA�v�A�O�A�=qA�1'A�-A�-A�+A�(�A� �A��A��A��A��A�oA�
=A�%A�A�A�A���A��A��/AǸRAǍPA�A�A��A�A��A��/A���AƬAƗ�AƇ+A�v�A�p�A�hsA�\)A�&�A���AŶFAũ�AœuAŉ7A�~�A�z�A�p�A�hsA�\)A�I�A�?}A�&�A��A��A�
=A��`AĴ9AăA�t�A�r�A�XA�C�A�-A��A��A�JA���A��A���AöFAã�AÁA�r�A�dZA�XA�Q�A�9XA�"�A��A�oA�1A���A��`A��;A���A�ȴAº^A¸RA©�A�A\A7AA�z�A�t�A�l�A�bNA�33A���A�\)A�1'A�1A��A��-A���A���A��+A�VA�O�A�M�A�C�A�
=A��RA��hA�n�A�M�A�33A�(�A�{A��A��RA�n�A�  A���A�z�A�jA�VA�-A�bA���A��mA��A��^A���A��DA�x�A�dZA�ZA�K�A�bA�p�A���A��hA�33A���A��+A�E�A�"�A��A��A�S�A�&�A��yA�t�A��TA�A���A�K�A���A�=qA��`A��PA�\)A�bA�9XA��A�\)A���A�/A��FA�E�A�1A��^A��mA���A��A��A�p�A�bNA�M�A�A�A�/A�"�A�1A�%A�1A�A���A��`A���A��#A��A���A��-A��!A���A���A��\A��A�x�A�t�A�dZA�bNA�I�A�;dA�5?A�1'A�$�A��A�
=A���A��TA��HA���A���A�"�A�z�A��A���A�E�A���A���A�=qA�/A���A��RA���A��PA��A�t�A�ffA�Q�A�C�A�&�A�  A��`A���A��^A��!A���A���A��\A��+A��A�~�A�v�A�t�A�n�A�dZA�^5A�ZA�M�A�?}A�33A�-A�"�A�VA���A��
A���A��\A��7A�~�A�jA�K�A�5?A��A��HA���A�ffA�7LA�oA��HA��A��A�hsA�E�A�;dA�$�A�
=A��#A��DA�jA�/A���A��mA��A�ĜA���A�VA��A��mA���A��A�VA�7LA�(�A��A��A��9A��+A�^5A�9XA��A���A���A���A��A�|�A�|�A��A��A��A��7A��A�|�A�x�A�v�A�n�A�l�A�l�A�hsA�ffA�ffA�\)A�ZA�\)A�ZA�Q�A�M�A�C�A�1'A�1'A�33A�33A�33A�(�A�A��mA���A���A��A��A�bNA�XA�I�A�(�A�bA��A���A��jA�x�A�G�A�/A��TA�hsA���A�VA�t�A�9XA���A��HA��HA��/A���A���A�t�A�9XA���A��A�ĜA���A�ffA�+A�VA��mA��9A��PA�l�A�n�A�bNA�VA�S�A�=qA�9XA�33A�&�A�oA�JA�A�  A��`A��
A���A���A��RA��\A�jA�Q�A��A��
A��^A�-A���A�ĜA�1'A�1A��
A��RA��A�C�A�
=A���A��A�`BA�1'A�oA��A�A���A��A�~�A�x�A�r�A�`BA�Q�A�;dA�(�A��A�JA���A��A��A��A��/A��/A���A�A��9A���A���A��A�Q�A�/A�JA��A���A��!A��PA�p�A�VA�7LA�$�A�1A���A��yA�ȴA���A�ffA�C�A��A���A�  A���A��A��/A��A��A��
A���A��hA�JA��wA��A�A�bA���A�r�A�ZA�C�A�$�A��A��;A��^A��-A��A���A���A���A��PA��7A��A��A�z�A�ZA�VA�-A�oA���A��`A���A���A�p�A�Q�A�5?A�$�A��A�VA�A�A���A��`A���A�ĜA��^A���A��DA�r�A�dZA�9XA�|�A� �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                               Aӣ�Aӣ�Aӡ�Aӣ�Aӥ�Aӣ�Aӣ�AӬAӲ-AӬAӮAӮAӣ�Aӥ�AӬAӲ-AӶFA�AӼjAӺ^AӾwAӼjAӶFAӣ�AӍPA�`BAҬAљ�A�XA��A�ƨAГuA�9XAϺ^AσA�^5A�O�A�9XAΩ�A�bA��mA���AͮA�Q�A�ƨA�hsA��#A�VA�z�AɾwAȬA�1'A��A�Aǉ7A�ȴA�^5Aş�A�S�A��;A�G�A��TA�hsA�oA���AA��wA���A�bA�-A��`A�%A�|�A�O�A��A�5?A�p�A�?}A�bNA�1A�I�A�"�A��A�E�A��`A��!A�hsA���A�`BA���A�z�A��A�ffA��jA��^A�  A�"�A��DA�VA�+A��wA���A�~�A���A��HA�jA�JA��A}��A{7LAy��AwhsAu�Ar��Ap�\An��An��AnZAmx�Ak��AhQ�Ae�7Ab��A_�7A]��A]S�A\ZA[�^A[33AY�ARv�AP�jANr�AJn�AF�AD�uAC�AB��A@=qA<�9A:z�A9/A6�A6=qA4M�A1�TA0��A/�;A.�jA.=qA,�HA-S�A,I�A)%A'�A'�A&v�A%��A$��A#��A"�+A"�A!�#A �+A��A�A��Ap�A;dAVA�yAA�A��AS�A��At�A��A�AO�A�yA�\A^5AdZA1'AA��Ax�A�AI�A�/A�A��A
ĜA
��A
��A�AVA
5?A	+A��A�yA��A$�Al�@���@�?}@��j@�1@�"�@��@�O�@�Z@��@�;d@�o@���@���@�5?@���@��@�M�@�^@��D@���@�x�@�Z@��@�33@�@�V@�9@�I�@���@�;d@柾@�{@�@�@�`B@�O�@�%@�r�@��@� �@��@�ƨ@���@�=q@�^@�7L@�(�@�t�@�ȴ@���@��@ܬ@�b@���@�x�@�%@ج@�1@�l�@�;d@�C�@�+@���@և+@���@��@�ȴ@ҧ�@�n�@�$�@�5?@�E�@�J@���@�%@�l�@�@͉7@���@̓u@�bN@��m@�"�@ʰ!@�-@���@�x�@ɲ-@�hs@�7L@��/@���@� �@Ǖ�@�"�@�S�@�@���@Ɵ�@ź^@��@��/@�j@��;@�S�@�@¸R@�=q@�$�@�$�@�E�@�E�@�Z@�(�@��
@�dZ@���@�I�@�(�@��;@��m@� �@�1'@���@�dZ@��@��!@��#@���@���@��7@�x�@��@��u@���@���@���@��u@���@�o@���@��@���@��-@�O�@��@���@���@�Q�@��@�|�@�"�@��@�o@�@���@��+@�5?@���@�p�@�G�@���@���@���@�Ĝ@�bN@��;@��@��P@�C�@���@���@���@��+@�~�@�n�@�-@���@��^@�p�@���@�z�@�K�@��y@�ȴ@��!@��+@�$�@��#@�p�@�?}@��@���@�z�@�A�@��;@��P@�l�@�"�@�~�@�M�@�E�@�5?@��@��#@��7@��@���@��j@��u@�j@�Z@��
@���@�t�@�S�@��@��@���@���@�5?@�x�@�%@���@�Z@��@��
@���@�dZ@�\)@�33@��@�n�@�ff@�V@��@�x�@��@�Ĝ@��@�Z@�Z@�j@��u@�b@��
@��@�l�@�dZ@�K�@��@���@�V@�{@��^@�X@�&�@��@�b@�"�@�ff@�M�@�5?@��@��@�@��^@��7@�7L@�%@�z�@� �@�1@��;@���@�S�@�ȴ@�M�@���@��#@��^@���@��h@�x�@�%@��@��@�\)@�S�@�S�@�K�@�o@�ȴ@���@���@�~�@�$�@�@��T@���@�G�@��/@��D@�I�@�b@��P@�o@��@��y@��@���@�5?@��@��T@���@��@�`B@�&�@��@���@��/@�r�@�1'@�1@���@���@�K�@�"�@�o@��@�ȴ@��\@�n�@�V@�{@�@��h@�X@�V@���@��@���@�9X@�(�@�1@�t�@�K�@�
=@��y@���@�v�@�E�@�{@���@��#@��7@�X@�O�@��@���@���@� �@��F@�t�@�;d@�@��R@�=q@�{@��@�@��7@�G�@�V@�%@���@���@��@��`@���@�r�@�@
=@~�R@~V@|��@{�F@{�@{�@{�@{S�@z=q@y7L@w
=@v�R@v��@v�+@v�+@vff@u�@u�@u?}@u/@t�@t�@tI�@s��@s�@sC�@r��@r�\@r^5@r-@q��@qX@q&�@p�9@p �@o�w@o+@o�@nv�@n$�@n@m�@m�h@mV@l�/@lj@l1@kƨ@kƨ@k��@k�@kS�@kS�@kC�@j��@j-@i��@i�^@i��@i�@h1'@hb@hb@g�;@g+@f�@f��@f5?@e/@dI�@d�@c��@c"�@b=q@a��@a�@`b@_��@_K�@_
=@^�R@^$�@]@]/@\�j@\�@[�
@[��@[o@Z�\@Zn�@Z=q@Y�@Y7L@Xr�@W
=@VE�@U�@U�-@U�@Tj@Sƨ@R�H@R�\@Rn�@R-@RJ@Q��@Q�#@Q��@Q&�@P  @O��@O�@O�@Nv�@N{@M��@M�h@Mp�@M?}@L�@L��@L�D@L9X@L1@Kt�@Ko@J�\@Jn�@I�#@I�@H�u@HQ�@Hr�@HA�@G�P@Fȴ@F�R@Fȴ@Fȴ@E�@E��@E�@Ep�@E`B@E�@EV@D�@D��@Dz�@D(�@C��@CC�@B�\@B^5@BM�@BJ@A�#@A�^@Ax�@@Ĝ@@A�@@ �@@  @?��@?l�@?l�@?K�@>�@=��@=?}@=V@<�@<�@<�j@<�@<z�@<9X@;�
@;33@:�H@:��@:��@:�\@:M�@9�#@9��@9hs@9&�@8��@8r�@7�w@7�P@7|�@7\)@7
=@6ff@6$�@5�@5@5O�@5V@4��@4��@5V@5�@4��@4j@4�@3�m@3S�@333@333@2�!@2�@1��@1G�@1&�@1%@0��@0�`@0r�@/�;@/�@/;d@/+@/
=@.ff@.{@-�@-��@-��@-��@-`B@-?}@-/@-�@,�/@,�@,1@+�@+33@*�@*��@*�!@*��@*�\@*J@)��@)G�@)&�@)%@(��@(��@(Ĝ@(�u@(A�@(b@'�w@'\)@&�@&�+@&ff@&V@&5?@&@%�T@%�@%V@$�j@$�@$��@$9X@#ƨ@#C�@#"�@#"�@#33@#"�@#"�@"�@"n�@"J@!��@!��@!��@!�#@!�@!�@!�^@!��@!x�@!G�@!&�@ ��@ Ĝ@ Ĝ@ ��@ A�@ A�@  �@�w@
=@��@{@�@V@��@�j@j@9X@��@��@C�@�!@~�@M�@=q@J@�^@�7@�`@bN@�@��@��@K�@�@�@
=@ȴ@�+@�+@��@�@�h@?}@/@V@�/@�@Z@��@ƨ@ƨ@��@�@��@S�@"�@"�@��@��@��@~�@�@�@��@��@hs@X@G�@&�@Ĝ@bN@Q�@1'@b@b@  @�;@��@�w@��@\)@��@�@ȴ@��@v�@ff@5?@�T@��@�@O�@O�@/@�@�j@�j@�@�@�@�j@�@��@��@�D@j@I�@I�@(�@��@�
@��@�@�@C�@o@
�@
��@
��@
^5@
^5@
^5@
M�@
=q@
�Aӡ�Aӧ�Aӣ�Aӥ�Aӥ�Aӣ�Aӥ�Aӣ�Aӥ�Aӣ�Aӡ�Aӣ�Aӥ�Aӥ�Aӡ�Aӡ�Aӣ�Aӡ�Aӟ�Aӥ�AӮAө�AӴ9AӴ9AӲ-AӲ-AӬAө�AӬAӬAӮAӮAө�AӰ!Aӧ�Aө�Aө�Aө�Aӣ�Aӡ�Aӣ�Aӥ�Aӡ�Aӟ�Aӡ�Aӣ�Aӣ�Aӡ�Aӡ�Aӡ�Aӥ�Aө�Aө�Aө�AӮAө�Aө�AӬAӬAӬAӬAӮAӰ!AӬAӮAӰ!AӰ!AӴ9AӼjAӺ^AӸRAӺ^AӼjAӺ^AӶFAӴ9AӴ9AӶFAӴ9AӲ-AӲ-A���A���A���A���A���AӾwAӾwA���AӾwAӺ^AӺ^AӼjAӾwAӶFA���AӶFA�A���AӾwAӾwAӺ^AӺ^AӸRAӼjAӶFAӸRAӾwA�AӾwAӾwAӺ^AӺ^AӼjAӼjAӼjAӼjAӼjA�ƨA�ȴA�ĜAӸRAӺ^AӸRAӶFAӴ9AӶFAӺ^AӶFAӴ9AӴ9AӲ-AӸRAӸRAӣ�Aӟ�Aӣ�Aӣ�Aӝ�Aӟ�Aӟ�Aӡ�Aӕ�AӇ+AӇ+Aӏ\AӍPAӃAӅAӑhAӏ\AӍPA�r�A�^5Aӗ�A�E�A�+A�p�A�{A�+A�+A�?}A�?}A�&�A��A���A�AѰ!Aѣ�Aї�AёhAэPAуA�z�A�l�A�dZA�dZA�^5A�VA�G�A�7LA�$�A�
=A��A��mA��HA��/A��#A��#A��A���A���A���A���Aк^Aа!AЧ�AУ�AН�AЛ�AЉ7AЅAЅA�t�A�dZA�ZA�K�A�;dA�/A��A���A��A���AϺ^Aϰ!AϬAϥ�Aϟ�Aϗ�AϓuAύPAσA�z�A�p�A�hsA�dZA�bNA�^5A�ZA�ZA�XA�VA�S�A�Q�A�Q�A�M�A�M�A�K�A�I�A�G�A�=qA�;dA�5?A�1'A�-A�$�A��A�oA΍PA�G�A�;dA�1'A�"�A��A��A�{A�1A���A���A��A��A��mA��mA��HA��HA��;A��#A��A��
A�ȴA�ĜA�ƨA�ƨA;wA͸RAͮAͧ�A͡�A͓uAͅA͇+A�|�A�VA�{A�VA�%A��`A���A̸RA̲-A̰!A̟�A̓uÃA�n�A�bNA�\)A�C�A�5?A�(�A�JA��A˼jA˼jA˥�A�jA�;dA�{A�%A��A���AʬAʍPA�z�A�t�A�l�A�hsA�bNA�S�A�1'A��A�p�A�A�A�VA���A���AȰ!AȑhA�v�A�O�A�=qA�1'A�-A�-A�+A�(�A� �A��A��A��A��A�oA�
=A�%A�A�A�A���A��A��/AǸRAǍPA�A�A��A�A��A��/A���AƬAƗ�AƇ+A�v�A�p�A�hsA�\)A�&�A���AŶFAũ�AœuAŉ7A�~�A�z�A�p�A�hsA�\)A�I�A�?}A�&�A��A��A�
=A��`AĴ9AăA�t�A�r�A�XA�C�A�-A��A��A�JA���A��A���AöFAã�AÁA�r�A�dZA�XA�Q�A�9XA�"�A��A�oA�1A���A��`A��;A���A�ȴAº^A¸RA©�A�A\A7AA�z�A�t�A�l�A�bNA�33A���A�\)A�1'A�1A��A��-A���A���A��+A�VA�O�A�M�A�C�A�
=A��RA��hA�n�A�M�A�33A�(�A�{A��A��RA�n�A�  A���A�z�A�jA�VA�-A�bA���A��mA��A��^A���A��DA�x�A�dZA�ZA�K�A�bA�p�A���A��hA�33A���A��+A�E�A�"�A��A��A�S�A�&�A��yA�t�A��TA�A���A�K�A���A�=qA��`A��PA�\)A�bA�9XA��A�\)A���A�/A��FA�E�A�1A��^A��mA���A��A��A�p�A�bNA�M�A�A�A�/A�"�A�1A�%A�1A�A���A��`A���A��#A��A���A��-A��!A���A���A��\A��A�x�A�t�A�dZA�bNA�I�A�;dA�5?A�1'A�$�A��A�
=A���A��TA��HA���A���A�"�A�z�A��A���A�E�A���A���A�=qA�/A���A��RA���A��PA��A�t�A�ffA�Q�A�C�A�&�A�  A��`A���A��^A��!A���A���A��\A��+A��A�~�A�v�A�t�A�n�A�dZA�^5A�ZA�M�A�?}A�33A�-A�"�A�VA���A��
A���A��\A��7A�~�A�jA�K�A�5?A��A��HA���A�ffA�7LA�oA��HA��A��A�hsA�E�A�;dA�$�A�
=A��#A��DA�jA�/A���A��mA��A�ĜA���A�VA��A��mA���A��A�VA�7LA�(�A��A��A��9A��+A�^5A�9XA��A���A���A���A��A�|�A�|�A��A��A��A��7A��A�|�A�x�A�v�A�n�A�l�A�l�A�hsA�ffA�ffA�\)A�ZA�\)A�ZA�Q�A�M�A�C�A�1'A�1'A�33A�33A�33A�(�A�A��mA���A���A��A��A�bNA�XA�I�A�(�A�bA��A���A��jA�x�A�G�A�/A��TA�hsA���A�VA�t�A�9XA���A��HA��HA��/A���A���A�t�A�9XA���A��A�ĜA���A�ffA�+A�VA��mA��9A��PA�l�A�n�A�bNA�VA�S�A�=qA�9XA�33A�&�A�oA�JA�A�  A��`A��
A���A���A��RA��\A�jA�Q�A��A��
A��^A�-A���A�ĜA�1'A�1A��
A��RA��A�C�A�
=A���A��A�`BA�1'A�oA��A�A���A��A�~�A�x�A�r�A�`BA�Q�A�;dA�(�A��A�JA���A��A��A��A��/A��/A���A�A��9A���A���A��A�Q�A�/A�JA��A���A��!A��PA�p�A�VA�7LA�$�A�1A���A��yA�ȴA���A�ffA�C�A��A���A�  A���A��A��/A��A��A��
A���A��hA�JA��wA��A�A�bA���A�r�A�ZA�C�A�$�A��A��;A��^A��-A��A���A���A���A��PA��7A��A��A�z�A�ZA�VA�-A�oA���A��`A���A���A�p�A�Q�A�5?A�$�A��A�VA�A�A���A��`A���A�ĜA��^A���A��DA�r�A�dZA�9XA�|�A� �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
�tB
�@B
�tB
��B
�@B
��B
�B
��B
��B
�tB
�tB
�@B
��B
��B
��B
�tB
�@B
��B
��B
��B
�'B
�^B
��B
��B
�EB
�HB
��B
	BCB)*B-�B49Bb�B�1B�$B�!B��B�TB�iB��B�B)�B9�BS�Bh>Bl�Bk�Bk�Bz�B�lB�4B�IB��B�0B��B��B�6B�B��B��BB��B�BB�aB��B��B��B��B�B{�Bp�BO�B#B�B��B��B�yB�|B��B��B�-B�xB��B� Bw�BP�B6B*eB
�B
��B
��B
�HB
�UB
�aB
��B
}�B
t�B
m]B
`�B
VB
=B
,�B
"hB
�B
	�B	�B	�oB	�B	�sB	ɺB	�tB	��B	�B	��B	��B	�MB	��B	z�B	o�B	l�B	h>B	d&B	_;B	\�B	H�B	:�B	9XB	2aB	&�B	)*B	'�B	*eB	/B	%zB	�B	�B	�B	\B	 B	
�B	{B	�B	B	AB	�B	"�B	8B	/B	*0B	)�B	&�B	'�B	&B	+�B	+kB	)�B	(�B	.B	/�B	0�B	0�B	1�B	3�B	7LB	9�B	<�B	>�B	EB	I�B	F�B	D�B	EmB	B[B	@�B	>wB	<�B	:�B	=qB	8�B	8RB	7B	7�B	6zB	4�B	4�B	-CB	0!B	+6B	1[B	9�B	B'B	F�B	HKB	C-B	8�B	.IB	.�B	�B	�B	�B	VB	~B	�B	B	�B	�B	 B	:B	�B	�B	oB	�B	�B	�B	�B	�B	B	�B	!�B	%B	'�B	($B	*�B	3hB	9�B	:�B	:�B	;dB	:�B	<B	=�B	>�B	@�B	A B	A�B	C�B	F?B	IRB	L�B	PB	R�B	U�B	U2B	UgB	VmB	Y�B	[�B	\�B	`�B	a�B	bB	c�B	h�B	l�B	o5B	p�B	s�B	t�B	t�B	t�B	t�B	t�B	uZB	��B	�B	�B	�%B	�fB	�=B	�B	��B	��B	�bB	��B	��B	��B	��B	�_B	�+B	�+B	�B	��B	�=B	��B	�CB	�IB	��B	��B	�4B	��B	��B	��B	��B	��B	�3B	��B	�B	�FB	��B	�dB	��B	��B	�^B	��B	��B	�XB	�jB	�BB	�B	��B	��B	��B	B	�gB	�tB	ȴB	�HB	��B	��B	ԕB	�2B	چB	��B	��B	چB	��B	�B	خB	�KB	�QB	�#B	�B	�B	�B	�ZB	��B	��B	�mB	�`B	�B	�yB	��B	��B	��B	�B	��B	�]B	��B	� B	��B	��B	�MB	�B	��B	�8B	��B	�8B	��B	�lB	�	B	��B	�lB	�8B	�B	�B	��B	��B	��B	�DB	��B	��B	��B	�PB	�PB	��B	�"B	�"B	��B	�"B	�VB	��B
;B
�B
GB
�B
B
�B
�B
�B
�B
GB
B
B
{B
�B
�B
�B
%B
1B
+B
�B
+B
+B
�B
�B
	�B
B
xB
~B
�B
�B
.B
�B
.B
�B
B
B
�B
�B
4B
�B
 B
hB
�B
:B
�B
B
B
MB
MB
B
$B
�B
�B
$B
SB
$B
$B
YB
�B
$B
$B
_B
�B
�B
+B
+B
�B
_B
_B
�B
_B
�B
�B
+B
�B
+B
+B
eB
�B
�B
=B
�B
�B
�B
kB
�B
=B
B
~B
�B
�B
�B
CB
�B
~B
~B
�B
xB
�B
�B
�B
�B
�B
~B
B
�B
~B
IB
IB
�B
OB
�B
OB
�B
VB
VB
�B
�B
�B
�B
 \B
 �B
 �B
!�B
!�B
"hB
"4B
!�B
"�B
#:B
#B
#�B
$B
$B
$B
$�B
$�B
$�B
$�B
%�B
%�B
&LB
&�B
'RB
'�B
'�B
'�B
'�B
(XB
(XB
(�B
(�B
)*B
)�B
)�B
*0B
*�B
*0B
*0B
+6B
+�B
+�B
,B
-CB
-CB
-�B
-wB
.}B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/OB
/�B
/�B
/�B
0�B
0�B
0�B
1[B
1�B
2-B
2�B
1�B
2�B
2�B
2-B
33B
2�B
33B
33B
2�B
33B
2�B
2�B
3�B
49B
49B
49B
4�B
5tB
6FB
5�B
5tB
5�B
5B
6zB
6�B
9�B
:*B
9�B
:^B
9�B
:�B
:�B
;0B
:�B
:�B
;�B
<6B
<jB
=B
=�B
>wB
?HB
?B
?B
?}B
?�B
@�B
@�B
AUB
A�B
A�B
B'B
A�B
C-B
B[B
B�B
B[B
C�B
C-B
C�B
C�B
DgB
D�B
C�B
D�B
D3B
D�B
C�B
D3B
DgB
E9B
D�B
D3B
D�B
D3B
D�B
D�B
E�B
E�B
E�B
EB
FtB
FtB
GEB
G�B
G�B
HKB
H�B
J#B
J�B
J�B
K�B
LdB
L0B
K�B
K�B
L�B
MB
MjB
N<B
N<B
NpB
OB
O�B
PB
OvB
O�B
P}B
P�B
QNB
QB
Q�B
Q�B
Q�B
Q�B
S&B
R�B
T,B
S�B
S�B
TaB
T�B
T�B
UgB
UgB
U2B
T�B
S�B
U2B
U2B
U2B
T�B
U2B
VB
U�B
V�B
V�B
V�B
WsB
W
B
V�B
VmB
W?B
XB
XB
ZB
Z�B
ZB
ZQB
Y�B
[�B
[WB
[�B
[WB
[#B
[�B
]�B
\�B
]�B
\�B
]dB
]�B
\�B
]�B
]/B
^5B
]�B
]dB
^�B
^�B
^5B
]�B
^5B
^�B
]�B
]�B
_B
_B
^�B
_;B
_�B
_pB
_;B
_pB
_�B
`B
_pB
_pB
_�B
`BB
`BB
`�B
a|B
a�B
bB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
b�B
b�B
bNB
c�B
dZB
d�B
d�B
e,B
d�B
e`B
e`B
e�B
f�B
gB
g�B
g�B
h�B
hsB
iB
iB
i�B
i�B
jKB
j�B
i�B
j�B
jKB
i�B
jB
iyB
i�B
iyB
iDB
jB
jB
jB
jB
jKB
jB
k�B
k�B
lWB
l�B
k�B
k�B
lWB
lWB
lWB
l"B
lWB
l�B
m�B
ncB
n�B
o5B
pB
o�B
oiB
poB
p�B
o5B
oiB
o B
n�B
n�B
o5B
oiB
o�B
pB
p;B
p;B
p�B
qAB
qB
q�B
q�B
qAB
qAB
q�B
rGB
rB
q�B
q�B
q�B
rB
sB
s�B
sB
r�B
r�B
r|B
rGB
sB
s�B
u%B
u�B
v+B
u�B
u�B
u�B
v�B
wfB
v�B
wfB
xB
x8B
xB
w�B
x8B
xlB
x�B
w�B
xB
x�B
xB
w�B
y	B
zB
zDB
zDB
zxB
zB
y�B
z�B
{JB
{B
z�B
zxB
{B
{B
z�B
{B
|B
}"B
}�B
}"B
}"B
|�B
}"B
}"B
}�B
~�B
.B
~�B
.B
~�B
~�B
~�B
~�B
~(B
}�B
~�B
~�B
~�B
~�B
.B
cB
�B
�4B
cB
�iB
��B
�iB
�;B
��B
�B
�B
�AB
�uB
�B
��B
�uB
�uB
�B
��B
��B
��B
�GB
��B
�MB
�B
��B
��B
��B
��B
��B
�B
�SB
�B
��B
�B
�SB
��B
�%B
��B
��B
��B
��B
��B
�_B
��B
��B
�_B
��B
�1B
��B
�_B
��B
�1B
��B
�fB
��B
��B
�fB
�fB
��B
��B
��B
�B
��B
��B
�7B
�B
�7B
�	B
�rB
��B
��B
�=B
�=B
�B
��B
�tB
�@B
�tB
�B
��B
�@B
��B
��B
�B
�zB
��B
��B
�B
��B
��B
��B
��B
�zB
�:B
�tB
��B
�B
�B
�B
�B
��B
�@B
�tB
�FB
�B
�FB
�tB
��B
�B
�B
��B
��B
�@B
�B
��B
�B
�tB
��B
��B
��B
��B
�FB
�B
��B
��B
��B
�zB
��B
��B
��B
�FB
��B
�tB
�B
��B
�@B
�tB
��B
��B
��B
�tB
��B
�:B
�B
�B
��B
�:B
��B
�B
�@B
�B
��B
�FB
�B
�FB
�nB
�tB
�B
�zB
��B
�FB
�FB
�B
�B
��B
�zB
��B
�B
��B
�B
�@B
�:B
�nB
��B
��B
��B
�B
�@B
��B
�FB
�B
�LB
�B
�LB
�zB
�zB
�zB
�FB
�FB
�B
��B
��B
�-B
�B
��B
��B
�:B
�B
�FB
�B
��B
��B
��B
�zB
��B
�FB
�tB
��B
��B
�B
��B
��B
��B
�nB
�zB
�B
�tB
��B
��B
�B
��B
�RB
�zB
�:B
�LB
�tB
��B
�@B
��B
�B
��B
�bB
��B
��B
��B
�hB
��B
��B
��B
��B
��B
�dB
�XB
��B
�dB
��B
��B
�0B
��B
��B
��B
��B
�jB
��B
��B
��B
�3B
�mB
��B
ŢB
�B
�B
�gB
ĜB
�mB
��B
ƨB
ȴB
��B
˒B
�dB
͟B
ΥB
ΥB
� B
��B
��B
�
B
�B
��B
��B
�B
�mB
�cB
�B  B{B
�BPB\B BBBB�BqBVB!�B%FB&�B'�B(�B)�B*�B+B+�B,�B-B-�B.�B/B/�B/�B0!B2�B3hB5?B6�B7�B9�B:�B>Bo B��B�fB�B�4B��B�$B�+B�B��B�B�FB�B��B��B��B�eB�B�=B��B��B��B��B��B�aB��B��B�dB��B�wBB�mBĜB�EB҉B�B�B�B��B�B�|B�B�vB�ZB��B��B��B  BB+B%B�B�B�BoBoB+BVB%�B.}B'�B,�B0�B5�B7�B9�B:*B:�B:�B<�B?�BIB\)BZ�B]�BbBb�Bd�BgmBhsBl�Bo Bn/BncBl"Bj�BjBkBl�Bl�BkQBjBjKBk�Bl"Bl�BlWBkBj�Bk�Bn/Bp�Bu�Bz�B��B��B�_B��B��B�=B�rB�~B�VB��B�VB��B�bB�7B��B�kB��B��B��B��B�B�!B��B��B�\B��B��B�:B�:B�zB��B��B��B�wB�wB�[B�[B��B�nB��B�9B�FB��B��B��B��B��B��B�B�^B�0B�UB�[B�B�6B�B�B��B�jB�jB��B��B�0B��B��B��B�jB��B�dB��B��B�XB��B��B�?BƨB��B�B�B��B��B��B�HB�B��B�dB�zB��B�^B�zB�FB��B�B�!B�}B��B�B�qB��B��B�oB�hB��B��B��B�.B�oB�oB��B��B�.B�.B�\B��B��B��B�nB�4B��B�bB��B��B��B�iB��B~]B{B{�B��Bz�Br�Bs�BxBzxB��B}"B\)BW
B`�BpoBVmBAUBS�B8�B:^B.�B$tB-CB/B�B�BB�B�B�B�B�BfB�B_B�B�B�BbB�.B�cB��B�BfB��B�]B�]B  B��B�B�8B�%B�`B��B�B�|B��B�B��B�B��B�B��B�2B�sB�B��B�ZB�MB�iB�GB��B��B��B��B�B�B�"B�B�KB��B��B�yB�QB�B�B�mB�B�B�B�2B�B�B��B�,B�`B�B�ZB��B�B�|B�B�B��B��BݘBޞB�dB��B��B��B֡BרB�BٴB�2B֡BخB��BӏB�pB�KBȀB�jB�zB��B��B�}B��B�wBĜBÖB�0B�aB��B��B�6B�6B�BB��B��B��B�FB�dB��B�}B��B�6B�B��B�RB�RB�B��B�B��B�qB��B�B��B��B��B�	B�B��B�xB��B�7B�=B��B��B�eB�B��B�_B��B��B��B�_B��B��B��B�hB�4B��B�uB��B�B�B�B�B��B��B�PB�lB�7B�lB��B�YB�ABz�B��B{Bt�B{B{�BcBl�BqvB]�BW
BS�BQ�BR�BY�BV9BXEBVBUgBM6BJ�BM�BK^BGEB?HBC�B=B9XB8�B3�B4B3�B1�B5tB1�B/OB2-B2aB.IB-�B,�B0UB-CB+�B'�B+B.�B$tB#�B'BOB�B%�B �B �B1B
��B
��B
��B
��B
�MB
��B
��B
�
B
�>B
�B
�TB
�B
��B
�B
ݘB
��B
��B
��B
��B
�EB
�)B
خB
��B
�EB
�
B
�,B
�&B
�aB
�mB
�NB
уB
ѷB
ѷB
͟B
ΥB
҉B
�B
ǮB
˒B
��B
ȴB
�gB
��B
�[B
�aB
�UB
�6B
��B
��B
��B
��B
�jB
�*B
�FB
��B
��B
�6B
�B
�B
�_B
��B
��B
�FB
�FB
��B
�?B
��B
��B
��B
��B
�JB
�+B
�B
��B
�MB
z�B
{JB
yrB
w�B
xlB
v+B
t�B
u�B
x�B
rB
qvB
t�B
tTB
u�B
qAB
wfB
o B
p;B
p;B
l�B
o�B
l�B
i�B
h
B
d&B
d&B
bNB
d�B
bNB
_;B
dZB
^jB
^B
\�B
^jB
[�B
ZQB
Y�B
VB
r|B
T�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                               B
��B
��B
��B
��B
��B
��B
��B
�LB
��B
��B
�LB
��B
�B
��B
��B
��B
��B
�LB
��B
��B
��B
��B
�LB
��B
��B
��B
�3B
�jB
��B
��B
�QB
�TB
��BBOB!6B%�B,EBZ�B�=B�0B�-B��B�`B�uB�B	�B!�B2BK�B`JBd�Bc�Bc�Br�B�xB�@B�UB��B�<B�B��B�BB�B��B��B��B�B�NB�mB�B��B��B��B|%Bs�Bh�BG�BB��B�B��B�BوB��B��B�9B��B��B�Bo�BH�B.B"qB�B
�B
��B
�TB
�aB
�mB
��B
u�B
l�B
eiB
X�B
NB
5B
$�B
tB

�B
�B	�B	�{B	��B	�B	��B	��B	�
B	�&B	�B	��B	�YB	��B	r�B	g�B	d�B	`JB	\2B	WGB	UB	@�B	3B	1dB	*mB	�B	!6B	�B	"qB	''B	�B	�B	�B	�B	hB		B	�B��B��B�%B�MB	�B	�B	0)B	''B	"<B	!�B	�B	�B	$B	#�B	#wB	"B	 �B	& B	'�B	(�B	(�B	)�B	+�B	/XB	1�B	4�B	6�B	=B	A�B	>�B	<�B	=yB	:gB	8�B	6�B	4�B	3B	5}B	0�B	0^B	/#B	/�B	.�B	,�B	,�B	%OB	(-B	#BB	)gB	1�B	:3B	>�B	@WB	;9B	0�B	&UB	&�B	�B	�B	�B	bB	�B	�B	'B	�B	�B		B	
FB		�B	
�B	
{B	
�B	�B	
�B	�B	B	B	�B	�B	B	�B	 0B	"�B	+tB	1�B	3B	2�B	3pB	2�B	4B	5�B	6�B	8�B	9,B	9�B	;�B	>KB	A^B	D�B	H B	J�B	M�B	M>B	MsB	NyB	Q�B	S�B	T�B	X�B	Y�B	Z%B	[�B	`�B	d�B	gAB	h�B	k�B	l�B	l�B	l�B	l�B	l�B	mfB	y�B	zB	|%B	~1B	�rB	�IB	�'B	��B	��B	�nB	��B	��B	��B	��B	�kB	�7B	�7B	�B	��B	�IB	��B	�OB	�UB	��B	��B	�@B	�B	��B	��B	��B	��B	�?B	��B	�B	�RB	�B	�pB	��B	��B	�jB	�B	��B	�dB	�vB	�NB	� B	��B	��B	�
B	��B	�sB	��B	��B	�TB	��B	��B	̡B	�>B	ҒB	��B	��B	ҒB	��B	�#B	кB	�WB	�]B	�/B	�B	�%B	��B	�fB	��B	�B	�yB	�lB	ާB	�B	��B	��B	��B	�B	� B	�iB	��B	�B	��B	��B	�YB	�+B	�B	�DB	��B	�DB	��B	�xB	�B	�B	�xB	�DB	�B	�B	�B	�B	�B	�PB	��B	��B	��B	�\B	�\B	��B	�.B	�.B	��B	�.B	�bB	��B	�GB	��B	�SB	��B	�%B	��B	��B	��B	��B	�SB	�B	�B	��B	��B	��B	��B	�1B
 =B	�7B	�B	�7B	�7B
 	B
 �B
�B
B
�B
�B
�B
�B
:B
�B
:B
�B

B

B
	�B
	�B
	@B
�B
	B
	tB
	�B

FB

�B
B
$B
YB
YB
*B
0B
�B
�B
0B
_B
0B
0B
eB
�B
0B
0B
kB
�B
�B
7B
7B
B
kB
kB
�B
kB
�B
�B
7B
�B
7B
7B
qB
�B
�B
IB
�B
�B
�B
wB
�B
IB
B
�B
�B
�B
�B
OB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!B
�B
�B
UB
UB
�B
[B
�B
[B
�B
bB
bB
�B
�B
�B
�B
hB
�B
�B
�B
B
tB
@B
B
�B
FB
B
�B
B
B
B
�B
�B
�B
�B
�B
�B
XB
�B
^B
�B
�B
�B
�B
 dB
 dB
 �B
 �B
!6B
!�B
!�B
"<B
"�B
"<B
"<B
#BB
#�B
#�B
$B
%OB
%OB
%�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'[B
'�B
'�B
'�B
(�B
(�B
(�B
)gB
*B
*9B
*�B
)�B
*�B
*�B
*9B
+?B
+B
+?B
+?B
*�B
+?B
+B
*�B
+�B
,EB
,EB
,EB
,�B
-�B
.RB
-�B
-�B
-�B
-B
.�B
.�B
1�B
26B
2B
2jB
2B
2�B
3B
3<B
3B
3B
3�B
4BB
4vB
5B
5�B
6�B
7TB
7 B
7 B
7�B
7�B
8�B
8�B
9aB
9�B
9�B
:3B
9�B
;9B
:gB
:�B
:gB
;�B
;9B
<
B
<
B
<sB
<�B
<
B
<�B
<?B
<�B
<
B
<?B
<sB
=EB
<�B
<?B
<�B
<?B
<�B
<�B
=�B
=�B
=�B
=B
>�B
>�B
?QB
?�B
?�B
@WB
@�B
B/B
B�B
B�B
C�B
DpB
D<B
C�B
C�B
D�B
EB
EvB
FHB
FHB
F|B
GB
G�B
H B
G�B
G�B
H�B
H�B
IZB
I&B
I�B
I�B
I�B
I�B
K2B
J�B
L8B
K�B
K�B
LmB
L�B
M
B
MsB
MsB
M>B
L�B
K�B
M>B
M>B
M>B
L�B
M>B
NB
M�B
N�B
N�B
N�B
OB
OB
N�B
NyB
OKB
PB
PB
R)B
R�B
R)B
R]B
Q�B
S�B
ScB
S�B
ScB
S/B
S�B
U�B
UB
U�B
UB
UpB
U�B
UB
U�B
U;B
VAB
U�B
UpB
V�B
V�B
VAB
U�B
VAB
V�B
U�B
U�B
WB
WB
V�B
WGB
W�B
W|B
WGB
W|B
W�B
XB
W|B
W|B
W�B
XNB
XNB
X�B
Y�B
Y�B
Z%B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[,B
Z�B
Z�B
ZZB
[�B
\fB
\�B
]B
]8B
]B
]lB
]lB
]�B
^�B
_B
_�B
_�B
`�B
`B
aB
aB
a�B
a�B
bWB
b�B
a�B
b�B
bWB
a�B
b"B
a�B
a�B
a�B
aPB
b"B
b"B
b"B
b�B
bWB
b�B
c�B
c�B
dcB
d�B
c�B
c�B
dcB
dcB
dcB
d.B
dcB
d�B
e�B
foB
f�B
gAB
hB
g�B
guB
h{B
h�B
gAB
guB
gB
f�B
f�B
gAB
guB
g�B
hB
hGB
hGB
h�B
iMB
iB
i�B
i�B
iMB
iMB
i�B
jSB
jB
i�B
i�B
i�B
jB
k%B
k�B
k%B
j�B
j�B
j�B
jSB
k%B
k�B
m1B
m�B
n7B
nB
m�B
m�B
n�B
orB
o	B
orB
pB
pDB
pB
o�B
pDB
pxB
p�B
o�B
pB
p�B
pB
o�B
qB
rB
rPB
rPB
r�B
rB
q�B
r�B
sVB
s�B
r�B
r�B
s"B
s"B
r�B
s"B
t(B
u.B
u�B
u.B
u.B
t�B
u.B
u.B
u�B
v�B
w:B
wB
w:B
v�B
v�B
v�B
v�B
v4B
v B
v�B
v�B
v�B
wB
w:B
woB
w�B
x@B
woB
xuB
x�B
xuB
yGB
y�B
yB
yB
zMB
z�B
zB
z�B
z�B
z�B
{B
z�B
z�B
{�B
{SB
{�B
|YB
|%B
{�B
{�B
{�B
|�B
|�B
}+B
}_B
}+B
|�B
}+B
}_B
}�B
~1B
B
~�B
~�B
~�B
~�B
kB
�B
�B
kB
�B
�=B
�B
kB
�	B
�=B
�B
�rB
�	B
��B
�rB
�rB
��B
��B
��B
�B
��B
��B
�CB
�B
�CB
�B
�~B
��B
��B
�IB
�IB
�B
��B
��B
�LB
��B
�B
��B
�LB
��B
��B
�B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�FB
��B
��B
�B
�B
�B
�B
��B
�LB
��B
�RB
�B
�RB
��B
��B
�*B
�B
��B
��B
�LB
�B
��B
�B
��B
��B
��B
��B
��B
�RB
�$B
��B
��B
��B
��B
��B
��B
��B
�RB
��B
��B
�B
��B
�LB
��B
��B
��B
��B
��B
��B
�FB
�B
�B
��B
�FB
��B
�B
�LB
�B
��B
�RB
�B
�RB
�zB
��B
�B
��B
��B
�RB
�RB
�B
�$B
��B
��B
��B
�B
��B
�B
�LB
�FB
�zB
��B
��B
��B
�B
�LB
��B
�RB
�B
�XB
�B
�XB
��B
��B
��B
�RB
�RB
�B
��B
��B
�9B
�B
��B
��B
�FB
�B
�RB
�B
��B
��B
��B
��B
��B
�RB
��B
��B
��B
�B
��B
�B
��B
�zB
��B
�B
��B
��B
��B
�B
��B
�^B
��B
�FB
�XB
��B
��B
�LB
��B
� B
��B
�nB
��B
��B
��B
�tB
�B
�B
��B
�B
��B
�pB
�dB
��B
�pB
��B
��B
�<B
�B
�B
��B
��B
�vB
��B
��B
��B
�?B
�yB
�
B
��B
�B
�B
�sB
��B
�yB
�
B
��B
��B
��B
ÞB
�pB
ūB
ƱB
ƱB
�,B
��B
��B
�B
ыB
�B
��B
��B
�yB
�oB
�%B
�B
��B�B\BhB	BB$B*B�B}BbBBRB�B�B!B!�B"�B#B#�B$�B%B%�B&�B''B'�B'�B(-B+B+tB-KB.�B/�B2B2�B6BgBz�B�rB�!B�@B��B�0B�7B�B��B�B�RB�*B��B��B��B�qB�B�IB��B��B��B��B��B�mB��B��B�pB��B��B��B�yB��B�QBʕBٽB�%B۔B��B�B�B�B�B�fB�	B�B��B�B�B�7B�1B��B�B�B
{B
{B7BbB�B&�B�B$�B(�B-�B/�B1�B26B2�B3B4�B7�BA)BT5BR�BU�BZ%BZ�B\�B_yB`Bd�BgBf;BfoBd.Bb�Bb�Bc(Be Be Bc]Bb"BbWBc�Bd.Bd�BdcBc(Bb�Bc�Bf;Bh�Bm�Br�B}�B}�BkB�B��B�IB�~B��B�bB��B�bB��B�nB�CB��B�wB��B��B��B��B�'B�-B��B��B�hB��B��B�FB�FB��B��B��B��B��B��B�gB�gB��B�zB�B�EB�RB��B�B��B�B��B��B�B�jB�<B�aB�gB�B�BB�B�B��B�vB�vB��B��B�<B��B��B��B�vB��B�pB��B��B�dB��B��B�KB��B��B�B�#B��B��B��B�TB�B��B�pB��B��B�jB��B�RB�B� B�-B��B��B�B�}B��B��B�{B�tB��B��B��B�:B�{B�{B��B��B�:B�:B�hB��B��B��B�zB�@B}�B�nB��B��Bx�BxuB|�BviBs�Bs�B|�Br�Bj�Bk�BpBr�B{�Bu.BT5BOBX�Bh{BNyB9aBK�B0�B2jB&�B�B%OB''B
�B�B$B�B�B�B�B��B rB �B�kB��B�B�BnB�:B�oB�B�(B rB��B�iB�iB�B��B�B�DB�1B�lB��B�B�B��B�B��B�B��B��B��B�>B�B�%B��B�fB�YB�uB�SB��B��B��B�B�B��B�.B�B�WB��B��B�B�]B�B�"B�yB�B�B��B�>B߭BܛB�B�8B�lB۔B�fB��BܛBوBضB�B��B�BդB֪B�pB��B��B��BέBϴB�#B��B�>BέBкB��B˛B�|B�WB��B�vB��B��B�B��B��B��B��B��B�<B�mB��B��B�BB�BB�NB��B�B��B�RB�pB��B��B��B�BB�B��B�^B�^B�$B�B�'B��B�}B��B�B��B��B��B�B�B��B��B��B�CB�IB��B��B�qB�B�B�kB��B��B��B�kB��B��B��B�tB�@B��B��B��B�*B�B�B�B��B��B�\B�xB�CB�xB{�B~eBzMBr�Bz�Bs�Bl�Bs�Bs�BwoBe Bi�BU�BOBK�BI�BJ�BQ�BNEBPQBNBMsBEBBCBE�BCjB?QB7TB<
B5B1dB0�B+�B,B+�B)�B-�B)�B'[B*9B*mB&UB%�B$�B(aB%OB#�B�B#B&�B�B�B*B[B�B�BB�B =B
�B
�B
�B
��B
�YB
�B
��B
�B
�JB
�B
�`B
ݡB
��B
�B
դB
�B
��B
��B
��B
�QB
�5B
кB
��B
�QB
�B
�8B
�2B
�mB
�yB
�ZB
ɏB
��B
��B
ūB
ƱB
ʕB
�B
��B
ÞB
��B
��B
�sB
��B
�gB
�mB
�aB
�BB
��B
��B
��B
��B
�vB
�6B
�RB
��B
��B
�BB
� B
�B
�kB
�B
��B
�RB
�RB
�B
�KB
��B
��B
��B
��B
�VB
7B
yB
y�B
|YB
r�B
sVB
q~B
o�B
pxB
n7B
l�B
nB
p�B
jB
i�B
l�B
l`B
m�B
iMB
orB
gB
hGB
hGB
d�B
g�B
d�B
a�B
`B
\2B
\2B
ZZB
\�B
ZZB
WGB
\fB
VvB
VB
T�B
VvB
S�B
R]B
Q�B
NB
j�B
M
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230511190112                            20230511190112AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023051119011220230511190112  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023051119011220230511190112QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023051119011220230511190112QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               