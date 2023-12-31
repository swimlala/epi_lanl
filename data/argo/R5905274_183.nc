CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:23Z creation      
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
_FillValue                 �  [h   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  cL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ѭ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � gx   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223223  20230426223223  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�
* <�N@�
* <�N11  @�
*O�@@�
*O�@@/�8�Cl@/�8�Cl�d'�����d'����11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?�\)@   @@  @�  @�  @�  @�  A ��A�A!G�A-p�AAG�A`  A�Q�A�Q�A�  A��A��A�\)A�\)A�  B Q�B�
B  B(�B (�B((�B0(�B8  B@  BH(�BP  BW�B_�
Bh  Bp  Bw�
B�  B�{B�  B�  B�  B�  B��B��B��B��B��
B��B��B��B�  B��B��B��B�  B�(�B�{B��
B��B��B��B�{B�  B�{B�  B��
B�  B�  C   C��C  C{C{C
  C��C�C��C
=C
=C
=C  C��C��C  C 
=C"  C$  C&
=C(
=C*
=C,  C.  C/��C2  C4  C5��C8  C:
=C<  C>  C@  CB  CD
=CF
=CG�CI�CL  CM�CO�CQ��CT  CV  CX  CY��C[�C]�C`  Cb  Cc��Cf
=Ch
=Cj  Cl
=Cn  Cp  Cr{Ct  Cu�Cw��Cz
=C{��C}��C��C�  C�C���C���C���C���C�C�  C���C�  C�C���C���C���C���C���C�
=C�
=C���C���C���C���C�  C�C�  C���C�  C�  C�C�
=C�
=C�C�  C�  C�C�  C��C���C�C�  C�
=C�
=C�
=C�\C�C�  C���C�C�C���C�C�C�
=C�
=C�  C���C���C���C���C���C�C�  C��C���C�  C�  C���C���C�  C�
=C���C��C���C�C���C�  C���C���C�C�  C���C���C�  C���C���C�C�  C�C�
=C�  C�  C�  C�C�C�  C�  C���C���C���C���C���C���C���C�  C�C�  C�C�C�C�C�  C�  C�  C�C�C���C���C���C���C���C���C�  C�  C�C�  C���C���C���D }qD �qD}qD�D� D�D�D�D� D  Dz�D  D� D�qD� D  Dz�D	  D	�D	�qD
z�D  D}qD��D}qD��DxRD�qD}qD��D� DD��D�D}qD  D��D  Dz�D�qD�DD}qD  D� D�D� D��D}qDD��D�D��D�D�D�D��D  D}qD�D��D  D}qD �D }qD �qD!��D"  D"}qD"�qD#� D$�D$}qD$�RD%� D&�D&��D'�D'� D(
=D(��D(�qD)��D)��D*xRD*�qD+� D,  D,�D,�qD-� D.D.}qD/  D/�D0  D0}qD1�D1}qD1�qD2��D2��D3z�D4  D4� D5�D5�=D6�D6��D7  D7��D7��D8� D9  D9��D:  D:}qD;  D;z�D;�qD<}qD=D=� D>�D>� D>�qD?� D@  D@}qDA  DA� DB  DBz�DB��DCz�DDDD��DE  DE� DF  DF��DG�DG� DH  DH� DI  DI�DI�qDJ� DJ��DK��DK�qDL� DL�qDM��DN  DN��DO  DO}qDP�DP}qDQ  DQ� DR�DR� DS�DS}qDS��DT}qDT�qDU}qDV  DV�DW�DW� DW�qDX��DX�qDY��DZ�DZ}qD[  D[� D[�qD\}qD\�qD]� D^  D^}qD_  D_}qD`  D`��Da  Da}qDb  Dbz�Dc�Dcz�Dd  Dd� Dd�qDe}qDf�Df��Dg�Dg��DhDh� Di  Di��Dj  Dj� Dk�Dk� Dl  Dl� Dm  Dm��Dn  Dn�Do�Do�Do�qDp� Dp��Dq��Dr�DrxRDr��Ds� Ds��Dt��Dt�qDu}qDu�qDvz�Dv�RDw}qDx�Dx}qDy�Dy��DzDz� D{D{� D|�D|� D}  D}��D~  D~�D�D}qD�HD�AHD�� D��HD���D�AHD�~�D���D��qD�>�D��HD���D�HD�@ D�� D��HD���D�@ D�� D�� D���D�AHD�� D��HD���D�B�D�}qD��HD���D�>�D��HD���D���D�=qD��HD��qD�  D�B�D�~�D���D��qD�AHD��HD�D�  D�B�D��HD�� D��D�@ D���D��HD�  D�>�D�� D�� D���D�=qD�~�D���D�HD�=qD�� D���D�HD�=qD�~�D�� D���D�<)D�}qD�� D�  D�>�D�� D�� D���D�>�D�~�D���D��)D�>�D�� D�� D�HD�AHD��HD��HD�  D�>�D�� D�� D���D�AHD��HD�� D���D�@ D�� D�� D�HD�@ D�~�D�� D���D�AHD�� D���D���D�@ D��HD�� D���D�>�D�~�D�� D�HD�B�D��HD���D���D�>�D�~�D�� D�HD�@ D�� D��HD���D�@ D��HD�D�HD�@ D�� D��HD��D�AHD�~�D��qD���D�@ D�~�D�� D�HD�@ D���D�� D��qD�@ D�� D��HD�HD�>�D�~�D�� D�  D�>�D�}qD���D�  D�AHD��HD���D���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�@ D���D�� D�  D�AHD���D���D�  D�@ D�~�D�� D�  D�@ D�� D��qD��qD�>�D�� D��HD���D�=qD�}qD��qD��qD�@ D�� D���D��qD�@ D��HD��HD�HD�@ D��HD�� D�HD�@ D��HD�� D��qD�=qD�� D�� D�HD�AHD��HD��HD��D�@ D�~�D��qD���D�>�D�� D�� D�  D�AHD��HD�� D�HD�AHD�� D�� D�HD�@ D�� D���D��qD�>�D�}qD�� D��D�B�D�� D��HD���D�B�D�� D��HD��qD�>�D�}qD��HD�  D�=qD�� D��HD�HD�@ D��HD���D�  D�AHD�~�D�� D�HD�=qD�}qD�� D�HD�@ DĀ DĽqD�HD�B�Dł�D��HD�  D�@ Dƀ DƽqD��D�AHDǂ�DǾ�D���D�@ DȀ D�� D�  D�@ Dɀ Dɾ�D�  D�@ Dʀ Dʾ�D�  D�@ Dˀ D�� D�HD�AHD̀ D�� D��D�AHD�~�D;�D�  D�>�D�~�Dξ�D�  D�@ D�~�D��HD�HD�>�DЀ D�� D�HD�AHDсHD�� D�HD�@ D�~�D�� D���D�@ DӁHD��HD�  D�AHD�~�D�� D��D�@ D�}qDվ�D�  D�AHDւ�D�D���D�@ Dׂ�D�� D��qD�AHD؁HDؽqD�HD�C�Dـ DٽqD�  D�AHDڀ D�� D�  D�@ D�|)D�� D�  D�=qD�~�D�� D���D�>�D݂�D�� D���D�AHDށHD��HD�  D�=qD�~�D߾�D�  D�@ D��HD�� D���D�=qD�}qD��HD�HD�@ D�~�D⾸D�  D�@ D� D�� D�HD�@ D�~�D�� D�  D�@ D� D��HD��D�@ D�~�D�� D�  D�AHD炏D�� D���D�>�D� D��HD��D�@ D�HD��HD�  D�@ D�HD�� D���D�@ D� D�� D�HD�@ D�~�D��HD�HD�AHD� D��HD�  D�AHD� D�� D�  D�AHD� DﾸD�  D�>�D�� D�D��D�AHD� D�� D���D�@ D� D��HD�  D�>�D� D�D�HD�>�D� D�� D�  D�@ D�� D��HD�  D�>�D�� D�� D�  D�@ D�~�D��qD���D�AHD�� D�� D�HD�@ D�� D�� D�HD�>�D��HD���?B�\?u?�\)?�{?\?�
=?��@�\@
=q@
=@#�
@(��@333@@  @G�@Q�@^�R@c�
@s33@xQ�@�G�@��@���@���@�z�@�(�@�  @�ff@�{@��@�Q�@�  @�ff@���@�@�p�@��
@�@�33@�p�A�\A
=A�A  A�\A�A�HA\)A#33A'�A+�A0  A3�
A8Q�A<(�A?\)AC�
AHQ�AL(�AO\)AS33AW
=A[�A`  Adz�Ah��Amp�Aq�AvffAz=qA\)A�=qA���A�\)A���A�(�A�ffA���A��\A���A��RA���A�33A�A�  A��HA���A�
=A��A���A��RA�G�A��A�{A���A��A�A�  A���A��
A�ffAȣ�A��HA��A�\)A�=qA�z�A�
=Aٙ�A�z�A�
=A���A�A�A�Q�A�\A�z�A�ffA��A�33A�A��A��\A��A�\)B�B=qB�Bz�BB
=B  B	G�B
ffB
=BQ�Bp�B�RB�B��B=qB�B��B{B33BQ�B��B�RB  B�B=qB
=B Q�B!p�B"�RB#�
B$��B&{B'\)B(z�B)��B+
=B,Q�B-B.�HB0(�B1G�B2�\B3�B4��B5B6�RB7�B8��B:{B;33B<Q�B=p�B>�RB@(�BAG�BBffBC�BD��BEp�BF�\BG�BHz�BIBJ�\BL  BMG�BNffBO�BP��BQBR�\BS�BT��BUp�BV�RBW�
BX��BZ{BZ�HB\(�B]p�B^�HB`  Ba�Bb=qBc33BdQ�Bep�Bf=qBg33Bhz�Bip�Bj�\Bk�
Bm�Bn=qBo\)Bpz�Bq��Br�\Bs�Btz�Bup�Bv�\Bw�Bx��By��Bz�HB|  B}p�B~ffB�B�Q�B���B�G�B�B�=qB���B�33B��B�(�B���B�33B��B�(�B���B��B���B�(�B��RB�G�B�B�=qB��RB�G�B�B�(�B��RB�33B��B�  B�z�B���B�p�B��B�ffB���B�\)B�  B�z�B���B�p�B��B�Q�B��HB��B���B�  B�z�B���B��B�  B�z�B��HB��B�{B���B��B��B�  B�z�B���B�G�B��B�{B��\B���B�p�B��
B�=qB��RB�\)B��
B�=qB��RB��B��B�  B�z�B���B�33B���B�{B�z�B��HB��B�{B��\B�
=B��B�B�(�B���B��B��B�  B�z�B�
=B���B�{B��\B�
=B��B��B�Q�B���B�
=B�p�B��B�ffB��HB�p�B�  B��\B�
=B�\)B�B�=qB���B��B��B�Q�B���B�33B��
B�{B��\B���B�p�B�  B��\B�33B��B�(�B��\B���B�p�B�  B�z�B�33B�B�Q�B���B�33BîB�=qB���BŅB�=qB���B�\)B��B�Q�B���BɅB�=qB��HB˅B�  B�z�B�
=B�B�z�B��BϮB�(�BиRB�G�B�{B���BӅB��Bԏ\B��B�  BָRB�\)B�{Bأ�B�33BٮB�Q�B���B��
B�z�B�33Bݙ�B�Q�B���B�B��\B�\)B�B�\B�33B�(�B��HB噚B�(�B���B�B�z�B��B��
B�ffB�
=B�  B��B�p�B��B��B�\)B�=qB���B�B�Q�B�
=B��B��RB�\)B�{B��HB�B�Q�B�
=B��
B���B��B�(�B��HB���B��\B�\)C   C \)C �C33C��C�HC33C��C
=CffC�C  C�C�HC(�Cp�CC(�Cz�C��C�
C33CffC�\C�RC��C(�CQ�Cp�C�C��C�HC	
=C	(�C	33C	\)C	�\C	C	��C	��C
�C
=qC
�C
�C
�
C
�HC
=C33Cp�C��C�RC��C��C(�C\)C�C�\C�RC�
C�CQ�C\)C�C�C�C{C(�CG�CffC�C�
C  C
=C33C\)C��CC�
C��C�CffC�C�\C�C�C�CG�CQ�Cp�C�RC�C�C{CG�Cz�C�\C�C�
C�C=qC\)Cp�C�C�
C��C
=C(�Cp�C��C��C��C  C
=C33C\)C�\C��CC�C(�CQ�CQ�Cz�C�RC�HC�C
=CQ�Cp�Cz�C��C�HC
=C�C33C\)C��C�RC��C��C=qC\)CffC�C�
C  C
=C(�Cp�C�\C��CC
=C33C=qCffC��CC�
C  C=qC\)Cp�C�C�
C�HC
=CQ�Cp�C�C��C�C
=C�CG�C�C�CC�C(�C=qC\)C�C�
C�HC 
=C G�C ffC �C ��C �C!{C!(�C!ffC!��C!�C!��C"
=C"=qC"=qC"ffC"�C"��C"�HC#�C#Q�C#\)C#�\C#��C#�C$
=C$G�C$z�C$�C$�C$�C%
=C%�C%ffC%��C%��C%��C&{C&�C&G�C&�\C&�RC&��C'  C'=qC'Q�C'p�C'�RC'��C'�C(=qC(G�C(p�C(C(�C(��C)=qC)p�C)z�C)��C)�HC)��C*{C*\)C*�C*��C*�
C+  C+{C+33C+�C+�C+�RC+�C,(�C,G�C,ffC,�C,��C,�C-(�C-\)C-ffC-��C-�
C-�HC.{C.Q�C.ffC.��C.�
C.�HC/
=C/\)C/p�C/�\C/�
C/��C0
=C0\)C0�C0��C0�HC1{C1(�C1\)C1��C1��C1�
C2�C2(�C2p�C2��C2�C2��C3{C333C3z�C3�\C3�RC4  C433C4=qC4�\C4��C4C5{C5=qC5Q�C5�C5C5�
C6{C6G�C6\)C6��C6C6�
C7�C7=qC7Q�C7��C7�C7�HC8(�C833C8z�C8��C8�RC9  C9{C9Q�C9��C9��C9�C:{C:33C:�C:��C:�HC;  C;(�C;z�C;�\C;�
C<{C<�C<p�C<�\C<�C=  C={C=\)C=�C=��C=��C>
=C>=qC>�\C>��C>�C?
=C?33C?�C?�\C?�HC?��C@33C@z�C@�C@�
C@��CA�CAz�CA�\CA�
CA�CB33CBffCBz�CB��CB�CC�CCp�CC�\CC�
CC�CD33CDz�CD�\CD�HCE  CEG�CE�CE��CE�CF
=CF\)CFz�CFCF�
CG(�CG\)CGz�CGCG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111411111111111111111411111111111411111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                   ?�\)@   @@  @�  @�  @�  @�  A ��A�A!G�A-p�AAG�A`  A�Q�A�Q�A�  A��A��A�\)A�\)A�  B Q�B�
B  B(�B (�B((�B0(�B8  B@  BH(�BP  BW�B_�
Bh  Bp  Bw�
B�  B�{B�  B�  B�  B�  B��B��B��B��B��
B��B��B��B�  B��B��B��B�  B�(�B�{B��
B��B��B��B�{B�  B�{B�  B��
B�  B�  C   C��C  C{C{C
  C��C�C��C
=C
=C
=C  C��C��C  C 
=C"  C$  C&
=C(
=C*
=C,  C.  C/��C2  C4  C5��C8  C:
=C<  C>  C@  CB  CD
=CF
=CG�CI�CL  CM�CO�CQ��CT  CV  CX  CY��C[�C]�C`  Cb  Cc��Cf
=Ch
=Cj  Cl
=Cn  Cp  Cr{Ct  Cu�Cw��Cz
=C{��C}��C��C�  C�C���C���C���C���C�C�  C���C�  C�C���C���C���C���C���C�
=C�
=C���C���C���C���C�  C�C�  C���C�  C�  C�C�
=C�
=C�C�  C�  C�C�  C��C���C�C�  C�
=C�
=C�
=C�\C�C�  C���C�C�C���C�C�C�
=C�
=C�  C���C���C���C���C���C�C�  C��C���C�  C�  C���C���C�  C�
=C���C��C���C�C���C�  C���C���C�C�  C���C���C�  C���C���C�C�  C�C�
=C�  C�  C�  C�C�C�  C�  C���C���C���C���C���C���C���C�  C�C�  C�C�C�C�C�  C�  C�  C�C�C���C���C���C���C���C���C�  C�  C�C�  C���C���C���D }qD �qD}qD�D� D�D�D�D� D  Dz�D  D� D�qD� D  Dz�D	  D	�D	�qD
z�D  D}qD��D}qD��DxRD�qD}qD��D� DD��D�D}qD  D��D  Dz�D�qD�DD}qD  D� D�D� D��D}qDD��D�D��D�D�D�D��D  D}qD�D��D  D}qD �D }qD �qD!��D"  D"}qD"�qD#� D$�D$}qD$�RD%� D&�D&��D'�D'� D(
=D(��D(�qD)��D)��D*xRD*�qD+� D,  D,�D,�qD-� D.D.}qD/  D/�D0  D0}qD1�D1}qD1�qD2��D2��D3z�D4  D4� D5�D5�=D6�D6��D7  D7��D7��D8� D9  D9��D:  D:}qD;  D;z�D;�qD<}qD=D=� D>�D>� D>�qD?� D@  D@}qDA  DA� DB  DBz�DB��DCz�DDDD��DE  DE� DF  DF��DG�DG� DH  DH� DI  DI�DI�qDJ� DJ��DK��DK�qDL� DL�qDM��DN  DN��DO  DO}qDP�DP}qDQ  DQ� DR�DR� DS�DS}qDS��DT}qDT�qDU}qDV  DV�DW�DW� DW�qDX��DX�qDY��DZ�DZ}qD[  D[� D[�qD\}qD\�qD]� D^  D^}qD_  D_}qD`  D`��Da  Da}qDb  Dbz�Dc�Dcz�Dd  Dd� Dd�qDe}qDf�Df��Dg�Dg��DhDh� Di  Di��Dj  Dj� Dk�Dk� Dl  Dl� Dm  Dm��Dn  Dn�Do�Do�Do�qDp� Dp��Dq��Dr�DrxRDr��Ds� Ds��Dt��Dt�qDu}qDu�qDvz�Dv�RDw}qDx�Dx}qDy�Dy��DzDz� D{D{� D|�D|� D}  D}��D~  D~�D�D}qD�HD�AHD�� D��HD���D�AHD�~�D���D��qD�>�D��HD���D�HD�@ D�� D��HD���D�@ D�� D�� D���D�AHD�� D��HD���D�B�D�}qD��HD���D�>�D��HD���D���D�=qD��HD��qD�  D�B�D�~�D���D��qD�AHD��HD�D�  D�B�D��HD�� D��D�@ D���D��HD�  D�>�D�� D�� D���D�=qD�~�D���D�HD�=qD�� D���D�HD�=qD�~�D�� D���D�<)D�}qD�� D�  D�>�D�� D�� D���D�>�D�~�D���D��)D�>�D�� D�� D�HD�AHD��HD��HD�  D�>�D�� D�� D���D�AHD��HD�� D���D�@ D�� D�� D�HD�@ D�~�D�� D���D�AHD�� D���D���D�@ D��HD�� D���D�>�D�~�D�� D�HD�B�D��HD���D���D�>�D�~�D�� D�HD�@ D�� D��HD���D�@ D��HD�D�HD�@ D�� D��HD��D�AHD�~�D��qD���D�@ D�~�D�� D�HD�@ D���D�� D��qD�@ D�� D��HD�HD�>�D�~�D�� D�  D�>�D�}qD���D�  D�AHD��HD���D���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�@ D���D�� D�  D�AHD���D���D�  D�@ D�~�D�� D�  D�@ D�� D��qD��qD�>�D�� D��HD���D�=qD�}qD��qD��qD�@ D�� D���D��qD�@ D��HD��HD�HD�@ D��HD�� D�HD�@ D��HD�� D��qD�=qD�� D�� D�HD�AHD��HD��HD��D�@ D�~�D��qD���D�>�D�� D�� D�  D�AHD��HD�� D�HD�AHD�� D�� D�HD�@ D�� D���D��qD�>�D�}qD�� D��D�B�D�� D��HD���D�B�D�� D��HD��qD�>�D�}qD��HD�  D�=qD�� D��HD�HD�@ D��HD���D�  D�AHD�~�D�� D�HD�=qD�}qD�� D�HD�@ DĀ DĽqD�HD�B�Dł�D��HD�  D�@ Dƀ DƽqD��D�AHDǂ�DǾ�D���D�@ DȀ D�� D�  D�@ Dɀ Dɾ�D�  D�@ Dʀ Dʾ�D�  D�@ Dˀ D�� D�HD�AHD̀ D�� D��D�AHD�~�D;�D�  D�>�D�~�Dξ�D�  D�@ D�~�D��HD�HD�>�DЀ D�� D�HD�AHDсHD�� D�HD�@ D�~�D�� D���D�@ DӁHD��HD�  D�AHD�~�D�� D��D�@ D�}qDվ�D�  D�AHDւ�D�D���D�@ Dׂ�D�� D��qD�AHD؁HDؽqD�HD�C�Dـ DٽqD�  D�AHDڀ D�� D�  D�@ D�|)D�� D�  D�=qD�~�D�� D���D�>�D݂�D�� D���D�AHDށHD��HD�  D�=qD�~�D߾�D�  D�@ D��HD�� D���D�=qD�}qD��HD�HD�@ D�~�D⾸D�  D�@ D� D�� D�HD�@ D�~�D�� D�  D�@ D� D��HD��D�@ D�~�D�� D�  D�AHD炏D�� D���D�>�D� D��HD��D�@ D�HD��HD�  D�@ D�HD�� D���D�@ D� D�� D�HD�@ D�~�D��HD�HD�AHD� D��HD�  D�AHD� D�� D�  D�AHD� DﾸD�  D�>�D�� D�D��D�AHD� D�� D���D�@ D� D��HD�  D�>�D� D�D�HD�>�D� D�� D�  D�@ D�� D��HD�  D�>�D�� D�� D�  D�@ D�~�D��qD���D�AHD�� D�� D�HD�@ D�� D�� D�HD�>�D��HD���?B�\?u?�\)?�{?\?�
=?��@�\@
=q@
=@#�
@(��@333@@  @G�@Q�@^�R@c�
@s33@xQ�@�G�@��@���@���@�z�@�(�@�  @�ff@�{@��@�Q�@�  @�ff@���@�@�p�@��
@�@�33@�p�A�\A
=A�A  A�\A�A�HA\)A#33A'�A+�A0  A3�
A8Q�A<(�A?\)AC�
AHQ�AL(�AO\)AS33AW
=A[�A`  Adz�Ah��Amp�Aq�AvffAz=qA\)A�=qA���A�\)A���A�(�A�ffA���A��\A���A��RA���A�33A�A�  A��HA���A�
=A��A���A��RA�G�A��A�{A���A��A�A�  A���A��
A�ffAȣ�A��HA��A�\)A�=qA�z�A�
=Aٙ�A�z�A�
=A���A�A�A�Q�A�\A�z�A�ffA��A�33A�A��A��\A��A�\)B�B=qB�Bz�BB
=B  B	G�B
ffB
=BQ�Bp�B�RB�B��B=qB�B��B{B33BQ�B��B�RB  B�B=qB
=B Q�B!p�B"�RB#�
B$��B&{B'\)B(z�B)��B+
=B,Q�B-B.�HB0(�B1G�B2�\B3�B4��B5B6�RB7�B8��B:{B;33B<Q�B=p�B>�RB@(�BAG�BBffBC�BD��BEp�BF�\BG�BHz�BIBJ�\BL  BMG�BNffBO�BP��BQBR�\BS�BT��BUp�BV�RBW�
BX��BZ{BZ�HB\(�B]p�B^�HB`  Ba�Bb=qBc33BdQ�Bep�Bf=qBg33Bhz�Bip�Bj�\Bk�
Bm�Bn=qBo\)Bpz�Bq��Br�\Bs�Btz�Bup�Bv�\Bw�Bx��By��Bz�HB|  B}p�B~ffB�B�Q�B���B�G�B�B�=qB���B�33B��B�(�B���B�33B��B�(�B���B��B���B�(�B��RB�G�B�B�=qB��RB�G�B�B�(�B��RB�33B��B�  B�z�B���B�p�B��B�ffB���B�\)B�  B�z�B���B�p�B��B�Q�B��HB��B���B�  B�z�B���B��B�  B�z�B��HB��B�{B���B��B��B�  B�z�B���B�G�B��B�{B��\B���B�p�B��
B�=qB��RB�\)B��
B�=qB��RB��B��B�  B�z�B���B�33B���B�{B�z�B��HB��B�{B��\B�
=B��B�B�(�B���B��B��B�  B�z�B�
=B���B�{B��\B�
=B��B��B�Q�B���B�
=B�p�B��B�ffB��HB�p�B�  B��\B�
=B�\)B�B�=qB���B��B��B�Q�B���B�33B��
B�{B��\B���B�p�B�  B��\B�33B��B�(�B��\B���B�p�B�  B�z�B�33B�B�Q�B���B�33BîB�=qB���BŅB�=qB���B�\)B��B�Q�B���BɅB�=qB��HB˅B�  B�z�B�
=B�B�z�B��BϮB�(�BиRB�G�B�{B���BӅB��Bԏ\B��B�  BָRB�\)B�{Bأ�B�33BٮB�Q�B���B��
B�z�B�33Bݙ�B�Q�B���B�B��\B�\)B�B�\B�33B�(�B��HB噚B�(�B���B�B�z�B��B��
B�ffB�
=B�  B��B�p�B��B��B�\)B�=qB���B�B�Q�B�
=B��B��RB�\)B�{B��HB�B�Q�B�
=B��
B���B��B�(�B��HB���B��\B�\)C   C \)C �C33C��C�HC33C��C
=CffC�C  C�C�HC(�Cp�CC(�Cz�C��C�
C33CffC�\C�RC��C(�CQ�Cp�C�C��C�HC	
=C	(�C	33C	\)C	�\C	C	��C	��C
�C
=qC
�C
�C
�
C
�HC
=C33Cp�C��C�RC��C��C(�C\)C�C�\C�RC�
C�CQ�C\)C�C�C�C{C(�CG�CffC�C�
C  C
=C33C\)C��CC�
C��C�CffC�C�\C�C�C�CG�CQ�Cp�C�RC�C�C{CG�Cz�C�\C�C�
C�C=qC\)Cp�C�C�
C��C
=C(�Cp�C��C��C��C  C
=C33C\)C�\C��CC�C(�CQ�CQ�Cz�C�RC�HC�C
=CQ�Cp�Cz�C��C�HC
=C�C33C\)C��C�RC��C��C=qC\)CffC�C�
C  C
=C(�Cp�C�\C��CC
=C33C=qCffC��CC�
C  C=qC\)Cp�C�C�
C�HC
=CQ�Cp�C�C��C�C
=C�CG�C�C�CC�C(�C=qC\)C�C�
C�HC 
=C G�C ffC �C ��C �C!{C!(�C!ffC!��C!�C!��C"
=C"=qC"=qC"ffC"�C"��C"�HC#�C#Q�C#\)C#�\C#��C#�C$
=C$G�C$z�C$�C$�C$�C%
=C%�C%ffC%��C%��C%��C&{C&�C&G�C&�\C&�RC&��C'  C'=qC'Q�C'p�C'�RC'��C'�C(=qC(G�C(p�C(C(�C(��C)=qC)p�C)z�C)��C)�HC)��C*{C*\)C*�C*��C*�
C+  C+{C+33C+�C+�C+�RC+�C,(�C,G�C,ffC,�C,��C,�C-(�C-\)C-ffC-��C-�
C-�HC.{C.Q�C.ffC.��C.�
C.�HC/
=C/\)C/p�C/�\C/�
C/��C0
=C0\)C0�C0��C0�HC1{C1(�C1\)C1��C1��C1�
C2�C2(�C2p�C2��C2�C2��C3{C333C3z�C3�\C3�RC4  C433C4=qC4�\C4��C4C5{C5=qC5Q�C5�C5C5�
C6{C6G�C6\)C6��C6C6�
C7�C7=qC7Q�C7��C7�C7�HC8(�C833C8z�C8��C8�RC9  C9{C9Q�C9��C9��C9�C:{C:33C:�C:��C:�HC;  C;(�C;z�C;�\C;�
C<{C<�C<p�C<�\C<�C=  C={C=\)C=�C=��C=��C>
=C>=qC>�\C>��C>�C?
=C?33C?�C?�\C?�HC?��C@33C@z�C@�C@�
C@��CA�CAz�CA�\CA�
CA�CB33CBffCBz�CB��CB�CC�CCp�CC�\CC�
CC�CD33CDz�CD�\CD�HCE  CEG�CE�CE��CE�CF
=CF\)CFz�CFCF�
CG(�CG\)CGz�CGCG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111411111111111111111411111111111411111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aљ�AѴ9AѮAѧ�Aѩ�AѬAѴ9A�A�ȴA�ĜA�ƨA���A���A�ȴA�ȴA���A�ȴA���A���A���A���A���A���A���A��/A��;A��;A��;A��TA��;A��HA��TA��mA��A��A��yA��`A��`A��yA��A��`A�XA�^5A��
A�ZA�E�AЧ�A��A�$�Aβ-A��/A̅A��`A���A�G�A��A�$�A�"�A�"�A��A���A�$�A��A���A��7A��A�E�A���A��A��DA��A��\A��yA�-A�;dA�O�A�7LA���A��RA�bNA��`A�G�A�bNA�oA�1'A���A��`A�JA�JA�v�A��#A�ĜA���A�S�A�M�A�XA��DA�ȴA� �A��A���A��^A���A���AA}oAy�mAt��Aq�Am��AiC�Ag?}Af$�Ae%Aa�FA^JAZ��AV �AT�/AS��AO&�AJ��AI��AH  AEG�ABn�A@v�A=�^A;oA8�`A6��A3��A1`BA.(�A-p�A,Q�A+&�A*1'A( �A%
=A"�HA!ƨA!/At�A"�A  A��A�-A�A&�A-A�7A��A�PA+A�A�A�yA�A��A��A�yA��AĜA��Av�Az�A~�AZA(�A�A�AdZA��A�#Ax�AJAp�A��A-A$�A(�A�mA	p�A�#A�hA+A�9AbNA9XA$�Al�A�9A�\AQ�A{A�;AG�A�!Ax�A ĜA �A �/AC�AS�A ��A �!A {@��P@�ȴ@�x�@���@���@��R@�J@���@�?}@�Z@���@��y@�n�@���@��j@��
@�S�@�~�@��@�z�@�@�\)@��y@�E�@�O�@��`@�Q�@땁@�\@�-@��`@�Z@�+@���@�J@�@�O�@�j@�bN@�I�@�(�@㝲@�
=@�R@�^5@�^@��@�33@ޗ�@�O�@���@܃@ۍP@ۍP@ۅ@�l�@��@�ȴ@�v�@�=q@�{@��#@�hs@��@�r�@�9X@� �@��
@�S�@���@�V@�J@��@Ցh@ԋD@�9X@Ӯ@���@�E�@��@љ�@�hs@��@���@Л�@�bN@ϕ�@��H@ΰ!@�@���@�b@��;@�t�@�+@�
=@��y@�v�@ɩ�@���@ț�@�bN@�1@��
@Ɵ�@�@őh@�j@î@��@�@�V@���@�`B@���@�I�@���@�33@�@�v�@�V@�n�@�$�@��T@���@�V@��@�(�@�b@��
@�\)@�33@�^5@���@��-@���@�j@�r�@�r�@�r�@�1'@�\)@��@��@��R@��!@���@���@�n�@�{@�/@�z�@�Q�@�I�@�(�@���@��@���@�K�@��!@��@��-@��7@�`B@���@��D@��
@�K�@�33@��\@�V@���@�X@�%@���@���@�bN@��P@��H@�=q@��#@��@�7L@��@��j@��@�K�@�ȴ@�v�@���@��-@�/@��@�(�@��@�"�@��R@�M�@�J@�@�hs@�%@�1'@���@�t�@�33@�@���@�^5@���@�O�@�/@��@�j@�1'@��
@���@��P@�\)@�+@���@�^5@�$�@��T@��@�&�@��9@�Z@�9X@�1'@��m@���@�"�@���@�V@��^@�?}@�V@���@�bN@���@��@��P@�33@��H@��!@�^5@�$�@���@���@��@��-@�G�@��j@�b@���@�33@��H@�n�@��@�@��7@�p�@�/@���@���@�b@��@�K�@�33@���@��R@�~�@�@���@���@�`B@��@�V@�V@���@��j@��D@�bN@�A�@�  @���@���@��@���@��P@�|�@�l�@��@���@�=q@�J@���@�x�@�7L@�&�@�V@���@��/@�9X@���@��;@��;@���@��@���@��!@�n�@�ff@�V@�M�@�=q@�-@��@���@�`B@�/@���@��u@�9X@�;@K�@~ȴ@~��@~v�@~$�@~@}�h@|��@|(�@{�m@{ƨ@{��@{33@z�@z�\@z-@y�#@y�7@yX@y%@w��@wl�@wK�@w;d@v�y@vv�@u�h@uO�@u�@t�/@t��@s�
@s33@r�@r�H@r��@r��@r��@r�!@r~�@r=q@rJ@q��@q��@q7L@pbN@o�@nv�@n5?@m��@m��@l�j@k��@kdZ@k"�@j�H@j��@jM�@jJ@i�7@iX@h��@h��@hA�@g��@gl�@f��@e�@ep�@d��@d�/@d�j@d�@c@b�@b��@b�\@a��@a%@`�u@`r�@`Q�@`b@_�;@_�w@_|�@_\)@_;d@_
=@^ȴ@^v�@^@]�-@\�/@\��@\z�@\I�@\I�@\Z@\I�@\9X@[dZ@["�@["�@[@Z~�@Y��@Y�7@Y7L@Y&�@Xr�@X  @W�;@W�@Wl�@W
=@V��@V��@V��@V�R@V$�@U�@Up�@UO�@UV@T��@T��@T�D@T(�@S�m@S�F@St�@So@R��@R-@Q��@Q��@P��@PQ�@Ol�@Nff@M�@M�@M�@L�@Lz�@L�@K��@K"�@Ko@J��@J��@Jn�@J=q@I��@I��@I&�@H�@HbN@HbN@HA�@H1'@G��@GK�@F�y@F��@F�+@FV@E�@E/@D��@D�@C�
@C��@B��@Bn�@BM�@B�@A�@A7L@@Ĝ@@ �@?�w@?|�@?
=@>�@>ȴ@>�R@>�+@=�T@=O�@<�@<z�@;��@;�
@;�@:�@:�H@:��@:�\@:~�@:M�@9x�@8Ĝ@8�u@8Q�@7�@7+@6ȴ@6v�@6{@5@5�h@5O�@5O�@4��@4��@3�m@3t�@3"�@2��@2��@2M�@2-@2�@1�@1��@1�7@17L@1%@0��@0Ĝ@0 �@/�@/��@/�w@/��@/|�@/
=@-�@-?}@-V@-V@,��@,�/@,�@,�D@,(�@+�
@+�F@+�@+t�@+�@+t�@+S�@+@*��@*�\@*n�@*M�@*J@)�^@)�7@)hs@)G�@)7L@)7L@(��@(�u@(r�@(Q�@(b@'��@'�P@'l�@'K�@'�@&�y@&v�@&V@&5?@%�@%��@%��@%�-@$�@$Z@$(�@#�F@#S�@"��@"n�@"�@"J@!�@!�#@!�#@!��@!�7@!x�@!�@ �9@ �u@ bN@ A�@ b@   @�;@�@�P@\)@;d@
=@�@��@V@5?@{@�@�@@/@/@�@j@I�@�m@��@S�@�H@�!@�!@�\@~�@~�@~�@n�@�@�#@�7@hs@G�@%@��@�9@�@1'@b@  @  @�w@�P@l�@;d@�R@�+@$�@��@�@��@��@��@�D@z�@Z@(�@��@S�@33@"�@o@�@�\@^5@-@�@��@�#@��@&�@��@�@r�@A�@  @�w@�P@K�@+@
=@�@ȴ@��@�+@V@E�@{@�@�T@@��@�h@`B@��@��@�@�D@z�@Z@9X@1@��@�
@�
@�F@��@��@��@�@�@dZ@"�@@
��@
�!@
��@
�\@
n�@
M�@
=q@
=q@
J@	��@	��@	��@	�7@	x�@	G�@	%@	%@��@�`@�9@r�@Q�@A�@ �@�@�;@�@l�@K�@;d@��@�R@��@��@ff@ff@V@V@ff@5?@$�@�@��@@�@O�@?}@�@�@��@��@�Aщ7AѓuAћ�Aѝ�AѲ-AѲ-AѴ9AѴ9AѴ9AѴ9AѴ9AѲ-AѲ-AѲ-Aѥ�Aџ�Aѥ�AѮAѧ�Aѝ�Aѥ�Aџ�Aѥ�AѰ!Aѩ�Aѣ�Aџ�Aѥ�AѬAѲ-AѮAѬAѬAѬAѲ-AѶFAѺ^A���A�A�ĜA�ƨA�ĜA�ƨA���A���A�ĜA�ĜA�ĜA�A�A�ĜA�A�A�ƨA�ȴA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A�ȴA���A���A���A�ȴA�ȴA���A�ȴA�ƨA�ĜA�ĜA�ȴA���A���A���A���A���A���A���A���A���A�ƨA�ƨA�ȴA�ƨA�ĜA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��
A���A���A��
A��
A��#A��#A��A��#A��#A��#A��#A��#A��#A��#A��#A��#A��;A��TA��HA��TA��TA��TA��TA��HA��;A��TA��/A��;A��/A��A��A��;A��A��;A��HA��HA��`A��mA��TA��TA��TA��HA��HA��HA��HA��/A��#A��;A��;A��;A��;A��;A��HA��TA��TA��HA��TA��;A��HA��HA��HA��/A��TA��TA��TA��yA��mA��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��HA��TA��mA��yA��A��A��yA��mA��mA��mA��mA��TA��mA��TA��;A��HA��TA��TA��TA��`A��mA��yA��yA��mA��A��A��A��A��A��A��A��A��A��A��A��A��`A��TA��`A��TA��HA��HA��HA��TA��TA��mA�=qA�p�A�p�A�p�A�p�A�bNA�dZA�dZA�bNA�^5A�^5A�^5A�XA�XA�\)A�XA�K�A�5?A��A��HA�ĜAѥ�Aя\A�|�A�`BA�^5A�^5A�^5A�^5A�\)A�M�A�K�A�M�A�M�A�K�A�I�A�I�A�I�A�I�A�G�A�C�A�=qA�&�A��A���AиRAЕ�A�r�A�l�A�n�A�ffA�ffA�K�A�VA�ZA�7LA��A���A���Aϩ�A�l�A�M�A�C�A�1'A�1A���A���A��A��A��A���A���A�ƨAάA΃A�v�A�n�A�\)A��A�%A��A��#Aͺ^AͮAͥ�A�t�A�=qA�"�A�  A��/A̶FA�1A�A�~�A�S�A�5?A� �A���A���AʾwAʟ�AʑhAʍPAʁA�hsA�7LA��A�x�A��A���Aȥ�Aȏ\A�z�A�dZA�=qA�1A��
AǛ�A�p�A�`BA�Q�A�%A�ĜAƛ�AƅA�n�A�1'A�{A�
=A���A��A��/A�ȴAš�A�S�AļjA�1Aç�A�dZA�%A�A���A���A���A��A��A���A���A��A�A�^5A�-A��wA�?}A��A��A�A�^5A���A�1A���A�+A�A��A��yA��;A��/A���A���A���A�ȴA�ƨA��wA��^A��-A���A�~�A�S�A�1'A��/A��\A�v�A�ZA��A�p�A��A�XA��A�A��uA��A�ȴA���A��A�x�A�r�A�VA� �A��#A�dZA�  A��uA�n�A�dZA�C�A�{A�A���A��A��#A�ĜA��RA���A��DA�jA�\)A�G�A�9XA�1A��TA�ƨA���A��A�M�A�33A�ȴA�I�A��A���A��#A���A���A���A��hA��A�z�A�x�A�n�A�`BA�XA�\)A�^5A�XA�I�A�7LA��yA���A��A�p�A�=qA�VA��wA�jA�33A�oA�  A��A�A���A��uA��A�r�A�l�A�hsA�ffA�`BA�O�A�;dA�(�A��A�
=A��A�ƨA��A���A���A���A���A���A���A���A���A���A���A��uA��\A��A�v�A�ffA�\)A�Q�A�?}A��A���A��9A�dZA�"�A��A��;A���A���A��9A��-A���A��DA�jA�VA�I�A�C�A�7LA�1'A�/A�&�A��A��A��A�oA�1A�%A�A���A��`A��;A��
A��
A��FA��+A�r�A�I�A�%A��
A��\A�ffA�ZA�M�A�7LA�
=A���A��9A��\A�t�A�XA�5?A�1A���A��\A�r�A�XA�1'A�
=A���A��A���A��^A���A��A�r�A�\)A�?}A�-A��A��A���A�ĜA�ƨA�A��jA��RA��RA��A���A��7A�^5A�A�A�+A�oA��A��
A���A�ȴA��wA���A��PA�O�A�%A��A��PA��A�|�A�p�A�n�A�ffA�O�A�C�A� �A��A�A���A���A��A��HA���A���A��A���A��DA�z�A�hsA�G�A�5?A��A�JA���A��HA���A���A�ȴA��jA���A�&�A�A�jA��yA��wA��A���A�l�A�I�A�/A�VA���A��A���A��A��hA��\A��A�jA�O�A�9XA�/A�+A��A�
=A�A��A��yA��yA��A���A���A���A��FA��!A���A���A��PA�l�A�^5A�/A�JA���A���A�I�A��wA��7A�dZA�M�A�9XA� �A�VA��mA���A�dZA�G�A�=qA�5?A��A�A�A���A��HA��^A���A�z�A�7LA�A��HA���A�XA�33A�VA��A���A��9A�r�A�33A��HA�ĜA��RA��A���A���A��PA�r�A�hsA�ffA�^5A�XA�VA�G�A�+A�JA��yA��#A��#A��
A�ƨA��A��PA�jA�?}A�"�A��A�oA�A��mA��A�\)A��A�|�A�K�A�A�A�33A� �A��A���A���A�ĜA��A��A�bA�C�A��#A���A�M�A���A�|�A�{A��jA�l�A�JA��RA�ZA�-A�VA��wA��\A�l�A�G�A��A��`A���A�|�A�\)A�1'A�bA���A���A�n�A�ZA�=qA���A��RA��A�n�A�Q�A��A�`BA��A��A��#A�ȴA��^A���A��\A�v�A�E�A��PA��yA��A��
A�ĜA���A���A��A�l�A�ZA�M�A�33A��AG�A�A~��A~��A~VA~bA}�;A}��A}��A}C�A}%A|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                   Aљ�AѴ9AѮAѧ�Aѩ�AѬAѴ9A�A�ȴA�ĜA�ƨA���A���A�ȴA�ȴA���A�ȴA���A���A���A���A���A���A���A��/A��;A��;A��;A��TA��;A��HA��TA��mA��A��A��yA��`A��`A��yA��A��`A�XA�^5A��
A�ZA�E�AЧ�A��A�$�Aβ-A��/A̅A��`A���A�G�A��A�$�A�"�A�"�A��A���A�$�A��A���A��7A��A�E�A���A��A��DA��A��\A��yA�-A�;dA�O�A�7LA���A��RA�bNA��`A�G�A�bNA�oA�1'A���A��`A�JA�JA�v�A��#A�ĜA���A�S�A�M�A�XA��DA�ȴA� �A��A���A��^A���A���AA}oAy�mAt��Aq�Am��AiC�Ag?}Af$�Ae%Aa�FA^JAZ��AV �AT�/AS��AO&�AJ��AI��AH  AEG�ABn�A@v�A=�^A;oA8�`A6��A3��A1`BA.(�A-p�A,Q�A+&�A*1'A( �A%
=A"�HA!ƨA!/At�A"�A  A��A�-A�A&�A-A�7A��A�PA+A�A�A�yA�A��A��A�yA��AĜA��Av�Az�A~�AZA(�A�A�AdZA��A�#Ax�AJAp�A��A-A$�A(�A�mA	p�A�#A�hA+A�9AbNA9XA$�Al�A�9A�\AQ�A{A�;AG�A�!Ax�A ĜA �A �/AC�AS�A ��A �!A {@��P@�ȴ@�x�@���@���@��R@�J@���@�?}@�Z@���@��y@�n�@���@��j@��
@�S�@�~�@��@�z�@�@�\)@��y@�E�@�O�@��`@�Q�@땁@�\@�-@��`@�Z@�+@���@�J@�@�O�@�j@�bN@�I�@�(�@㝲@�
=@�R@�^5@�^@��@�33@ޗ�@�O�@���@܃@ۍP@ۍP@ۅ@�l�@��@�ȴ@�v�@�=q@�{@��#@�hs@��@�r�@�9X@� �@��
@�S�@���@�V@�J@��@Ցh@ԋD@�9X@Ӯ@���@�E�@��@љ�@�hs@��@���@Л�@�bN@ϕ�@��H@ΰ!@�@���@�b@��;@�t�@�+@�
=@��y@�v�@ɩ�@���@ț�@�bN@�1@��
@Ɵ�@�@őh@�j@î@��@�@�V@���@�`B@���@�I�@���@�33@�@�v�@�V@�n�@�$�@��T@���@�V@��@�(�@�b@��
@�\)@�33@�^5@���@��-@���@�j@�r�@�r�@�r�@�1'@�\)@��@��@��R@��!@���@���@�n�@�{@�/@�z�@�Q�@�I�@�(�@���@��@���@�K�@��!@��@��-@��7@�`B@���@��D@��
@�K�@�33@��\@�V@���@�X@�%@���@���@�bN@��P@��H@�=q@��#@��@�7L@��@��j@��@�K�@�ȴ@�v�@���@��-@�/@��@�(�@��@�"�@��R@�M�@�J@�@�hs@�%@�1'@���@�t�@�33@�@���@�^5@���@�O�@�/@��@�j@�1'@��
@���@��P@�\)@�+@���@�^5@�$�@��T@��@�&�@��9@�Z@�9X@�1'@��m@���@�"�@���@�V@��^@�?}@�V@���@�bN@���@��@��P@�33@��H@��!@�^5@�$�@���@���@��@��-@�G�@��j@�b@���@�33@��H@�n�@��@�@��7@�p�@�/@���@���@�b@��@�K�@�33@���@��R@�~�@�@���@���@�`B@��@�V@�V@���@��j@��D@�bN@�A�@�  @���@���@��@���@��P@�|�@�l�@��@���@�=q@�J@���@�x�@�7L@�&�@�V@���@��/@�9X@���@��;@��;@���@��@���@��!@�n�@�ff@�V@�M�@�=q@�-@��@���@�`B@�/@���@��u@�9X@�;@K�@~ȴ@~��@~v�@~$�@~@}�h@|��@|(�@{�m@{ƨ@{��@{33@z�@z�\@z-@y�#@y�7@yX@y%@w��@wl�@wK�@w;d@v�y@vv�@u�h@uO�@u�@t�/@t��@s�
@s33@r�@r�H@r��@r��@r��@r�!@r~�@r=q@rJ@q��@q��@q7L@pbN@o�@nv�@n5?@m��@m��@l�j@k��@kdZ@k"�@j�H@j��@jM�@jJ@i�7@iX@h��@h��@hA�@g��@gl�@f��@e�@ep�@d��@d�/@d�j@d�@c@b�@b��@b�\@a��@a%@`�u@`r�@`Q�@`b@_�;@_�w@_|�@_\)@_;d@_
=@^ȴ@^v�@^@]�-@\�/@\��@\z�@\I�@\I�@\Z@\I�@\9X@[dZ@["�@["�@[@Z~�@Y��@Y�7@Y7L@Y&�@Xr�@X  @W�;@W�@Wl�@W
=@V��@V��@V��@V�R@V$�@U�@Up�@UO�@UV@T��@T��@T�D@T(�@S�m@S�F@St�@So@R��@R-@Q��@Q��@P��@PQ�@Ol�@Nff@M�@M�@M�@L�@Lz�@L�@K��@K"�@Ko@J��@J��@Jn�@J=q@I��@I��@I&�@H�@HbN@HbN@HA�@H1'@G��@GK�@F�y@F��@F�+@FV@E�@E/@D��@D�@C�
@C��@B��@Bn�@BM�@B�@A�@A7L@@Ĝ@@ �@?�w@?|�@?
=@>�@>ȴ@>�R@>�+@=�T@=O�@<�@<z�@;��@;�
@;�@:�@:�H@:��@:�\@:~�@:M�@9x�@8Ĝ@8�u@8Q�@7�@7+@6ȴ@6v�@6{@5@5�h@5O�@5O�@4��@4��@3�m@3t�@3"�@2��@2��@2M�@2-@2�@1�@1��@1�7@17L@1%@0��@0Ĝ@0 �@/�@/��@/�w@/��@/|�@/
=@-�@-?}@-V@-V@,��@,�/@,�@,�D@,(�@+�
@+�F@+�@+t�@+�@+t�@+S�@+@*��@*�\@*n�@*M�@*J@)�^@)�7@)hs@)G�@)7L@)7L@(��@(�u@(r�@(Q�@(b@'��@'�P@'l�@'K�@'�@&�y@&v�@&V@&5?@%�@%��@%��@%�-@$�@$Z@$(�@#�F@#S�@"��@"n�@"�@"J@!�@!�#@!�#@!��@!�7@!x�@!�@ �9@ �u@ bN@ A�@ b@   @�;@�@�P@\)@;d@
=@�@��@V@5?@{@�@�@@/@/@�@j@I�@�m@��@S�@�H@�!@�!@�\@~�@~�@~�@n�@�@�#@�7@hs@G�@%@��@�9@�@1'@b@  @  @�w@�P@l�@;d@�R@�+@$�@��@�@��@��@��@�D@z�@Z@(�@��@S�@33@"�@o@�@�\@^5@-@�@��@�#@��@&�@��@�@r�@A�@  @�w@�P@K�@+@
=@�@ȴ@��@�+@V@E�@{@�@�T@@��@�h@`B@��@��@�@�D@z�@Z@9X@1@��@�
@�
@�F@��@��@��@�@�@dZ@"�@@
��@
�!@
��@
�\@
n�@
M�@
=q@
=q@
J@	��@	��@	��@	�7@	x�@	G�@	%@	%@��@�`@�9@r�@Q�@A�@ �@�@�;@�@l�@K�@;d@��@�R@��@��@ff@ff@V@V@ff@5?@$�@�@��@@�@O�@?}@�@�@��@��@�Aщ7AѓuAћ�Aѝ�AѲ-AѲ-AѴ9AѴ9AѴ9AѴ9AѴ9AѲ-AѲ-AѲ-Aѥ�Aџ�Aѥ�AѮAѧ�Aѝ�Aѥ�Aџ�Aѥ�AѰ!Aѩ�Aѣ�Aџ�Aѥ�AѬAѲ-AѮAѬAѬAѬAѲ-AѶFAѺ^A���A�A�ĜA�ƨA�ĜA�ƨA���A���A�ĜA�ĜA�ĜA�A�A�ĜA�A�A�ƨA�ȴA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A�ȴA���A���A���A�ȴA�ȴA���A�ȴA�ƨA�ĜA�ĜA�ȴA���A���A���A���A���A���A���A���A���A�ƨA�ƨA�ȴA�ƨA�ĜA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��
A���A���A��
A��
A��#A��#A��A��#A��#A��#A��#A��#A��#A��#A��#A��#A��;A��TA��HA��TA��TA��TA��TA��HA��;A��TA��/A��;A��/A��A��A��;A��A��;A��HA��HA��`A��mA��TA��TA��TA��HA��HA��HA��HA��/A��#A��;A��;A��;A��;A��;A��HA��TA��TA��HA��TA��;A��HA��HA��HA��/A��TA��TA��TA��yA��mA��mA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��HA��TA��mA��yA��A��A��yA��mA��mA��mA��mA��TA��mA��TA��;A��HA��TA��TA��TA��`A��mA��yA��yA��mA��A��A��A��A��A��A��A��A��A��A��A��A��`A��TA��`A��TA��HA��HA��HA��TA��TA��mA�=qA�p�A�p�A�p�A�p�A�bNA�dZA�dZA�bNA�^5A�^5A�^5A�XA�XA�\)A�XA�K�A�5?A��A��HA�ĜAѥ�Aя\A�|�A�`BA�^5A�^5A�^5A�^5A�\)A�M�A�K�A�M�A�M�A�K�A�I�A�I�A�I�A�I�A�G�A�C�A�=qA�&�A��A���AиRAЕ�A�r�A�l�A�n�A�ffA�ffA�K�A�VA�ZA�7LA��A���A���Aϩ�A�l�A�M�A�C�A�1'A�1A���A���A��A��A��A���A���A�ƨAάA΃A�v�A�n�A�\)A��A�%A��A��#Aͺ^AͮAͥ�A�t�A�=qA�"�A�  A��/A̶FA�1A�A�~�A�S�A�5?A� �A���A���AʾwAʟ�AʑhAʍPAʁA�hsA�7LA��A�x�A��A���Aȥ�Aȏ\A�z�A�dZA�=qA�1A��
AǛ�A�p�A�`BA�Q�A�%A�ĜAƛ�AƅA�n�A�1'A�{A�
=A���A��A��/A�ȴAš�A�S�AļjA�1Aç�A�dZA�%A�A���A���A���A��A��A���A���A��A�A�^5A�-A��wA�?}A��A��A�A�^5A���A�1A���A�+A�A��A��yA��;A��/A���A���A���A�ȴA�ƨA��wA��^A��-A���A�~�A�S�A�1'A��/A��\A�v�A�ZA��A�p�A��A�XA��A�A��uA��A�ȴA���A��A�x�A�r�A�VA� �A��#A�dZA�  A��uA�n�A�dZA�C�A�{A�A���A��A��#A�ĜA��RA���A��DA�jA�\)A�G�A�9XA�1A��TA�ƨA���A��A�M�A�33A�ȴA�I�A��A���A��#A���A���A���A��hA��A�z�A�x�A�n�A�`BA�XA�\)A�^5A�XA�I�A�7LA��yA���A��A�p�A�=qA�VA��wA�jA�33A�oA�  A��A�A���A��uA��A�r�A�l�A�hsA�ffA�`BA�O�A�;dA�(�A��A�
=A��A�ƨA��A���A���A���A���A���A���A���A���A���A���A��uA��\A��A�v�A�ffA�\)A�Q�A�?}A��A���A��9A�dZA�"�A��A��;A���A���A��9A��-A���A��DA�jA�VA�I�A�C�A�7LA�1'A�/A�&�A��A��A��A�oA�1A�%A�A���A��`A��;A��
A��
A��FA��+A�r�A�I�A�%A��
A��\A�ffA�ZA�M�A�7LA�
=A���A��9A��\A�t�A�XA�5?A�1A���A��\A�r�A�XA�1'A�
=A���A��A���A��^A���A��A�r�A�\)A�?}A�-A��A��A���A�ĜA�ƨA�A��jA��RA��RA��A���A��7A�^5A�A�A�+A�oA��A��
A���A�ȴA��wA���A��PA�O�A�%A��A��PA��A�|�A�p�A�n�A�ffA�O�A�C�A� �A��A�A���A���A��A��HA���A���A��A���A��DA�z�A�hsA�G�A�5?A��A�JA���A��HA���A���A�ȴA��jA���A�&�A�A�jA��yA��wA��A���A�l�A�I�A�/A�VA���A��A���A��A��hA��\A��A�jA�O�A�9XA�/A�+A��A�
=A�A��A��yA��yA��A���A���A���A��FA��!A���A���A��PA�l�A�^5A�/A�JA���A���A�I�A��wA��7A�dZA�M�A�9XA� �A�VA��mA���A�dZA�G�A�=qA�5?A��A�A�A���A��HA��^A���A�z�A�7LA�A��HA���A�XA�33A�VA��A���A��9A�r�A�33A��HA�ĜA��RA��A���A���A��PA�r�A�hsA�ffA�^5A�XA�VA�G�A�+A�JA��yA��#A��#A��
A�ƨA��A��PA�jA�?}A�"�A��A�oA�A��mA��A�\)A��A�|�A�K�A�A�A�33A� �A��A���A���A�ĜA��A��A�bA�C�A��#A���A�M�A���A�|�A�{A��jA�l�A�JA��RA�ZA�-A�VA��wA��\A�l�A�G�A��A��`A���A�|�A�\)A�1'A�bA���A���A�n�A�ZA�=qA���A��RA��A�n�A�Q�A��A�`BA��A��A��#A�ȴA��^A���A��\A�v�A�E�A��PA��yA��A��
A�ĜA���A���A��A�l�A�ZA�M�A�33A��AG�A�A~��A~��A~VA~bA}�;A}��A}��A}C�A}%A|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
oB
 B
�B
�B
B
B
4B
B
�B
�B
B
�B
hB
hB
hB
�B
hB
�B
B
:B
�B
�B
�B
�B
FB
FB
�B
B
{B
uB
�B
�B
FB
SB
MB
FB
�B
oB
@B
�B
B
�lB
��B
�oB$B!-B4�BB'BZ�BV9B\)BzB��B�B�vB��B�B.B2�B/OB;�BS�BQBO�BP}BV�BbBS�BN<BR�BOBR BQNBI�BW?BI�B@�B?�B?�B(XB&�B�B�BVB	lB��B��B�B��B�B�'B�7BcTBRTB;dB33B0!B�B
�|B
�nB
�JB
h
B
D�B
)�B
B
	�B	��B	�9B	�B	�B	�hB	�FB	�VB	�B	|�B	m]B	f�B	cTB	[�B	ZB	\�B	W�B	PB	QB	N�B	@B	<B	0UB	5tB	%FB	1B�>B��B�B�yB� B��BܒB�B�HB�ZB�pB�)B��B��B��BʌB�0B�HB��B��BߤB�`B�cB�B��B	+B	B	*0B	+kB	,�B	0�B	6B	8RB	<B	>BB	>�B	?�B	A�B	C�B	F�B	L�B	M�B	M�B	S&B	U�B	`�B	\�B	aB	^B	^jB	`vB	aHB	g�B	e,B	b�B	a|B	`BB	`�B	bB	c�B	kQB	j�B	j�B	k�B	k�B	l�B	rB	w2B	v�B	z�B	~�B	��B	�PB	��B	��B	�SB	�qB	��B	�~B	�!B	��B	�-B	�-B	�bB	��B	�B	�$B	��B	��B	��B	��B	�B	�[B	�UB	��B	��B	��B	�zB	�B	�FB	�B	��B	��B	�B	�XB	��B	��B	��B	�jB	�6B	�dB	��B	�0B	�wB	��B	� B	�'B	�'B	�aB	��B	ÖB	�-B	��B	�9B	��B	��B	�B	�tB	ɆB	ɆB	ɆB	��B	�)B	�)B	��B	ʌB	�)B	��B	��B	�6B	�dB	�6B	�pB	�HB	�vB	�NB	�B	҉B	��B	�NB	҉B	�TB	��B	� B	ҽB	�TB	ҽB	�TB	҉B	ҽB	҉B	�TB	� B	�,B	ԕB	�,B	��B	�?B	�
B	֡B	�?B	�?B	�?B	��B	רB	�KB	�B	��B	�B	�QB	�QB	�5B	�]B	�dB	�;B	�;B	ߤB	�;B	��B	��B	�HB	�B	�B	�`B	�ZB	�ZB	��B	�`B	�sB	��B	��B	��B	�B	�B	�KB	�DB	��B	�B	�QB	�B	�B	�B	�]B	�B	��B	��B	�/B	�B	�B	�B	��B	�B	�oB	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�+B	�2B	��B	�>B	�lB	�	B	�8B	�DB	�>B	��B	�JB	�B	�B	�xB	��B	��B	�.B	��B	�cB	�cB
�B
uB
B
{B
�B
{B
{B
�B
B
�B
SB
�B
%B
%B
�B
�B
�B
1B
�B
�B
�B
	7B
	lB
	�B
	�B
DB

�B
�B
~B
�B
JB
�B
�B
VB
"B
(B
.B
4B
�B
�B
�B
�B
oB
�B
�B
B
�B
uB
�B
�B
SB
B
�B
_B
�B
�B
�B
�B
�B
	B
kB
	B
qB
�B
�B
B
=B
�B
CB
�B
xB
B
�B
!B
!-B
 �B
!-B
"4B
#nB
%B
&B
&�B
'RB
'�B
'�B
'B
($B
(XB
(�B
)_B
)�B
*0B
*�B
*�B
+kB
,B
,=B
-B
,�B
-B
-wB
.B
-wB
.B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
0UB
/�B
0UB
/�B
0!B
0UB
0�B
1[B
1�B
2aB
2-B
3hB
2�B
2�B
2�B
2�B
4�B
4B
4nB
3�B
4�B
6�B
5�B
6zB
7LB
6�B
7LB
6�B
7B
7B
6�B
8RB
8RB
8�B
8RB
9$B
9XB
9XB
9�B
:�B
:�B
:*B
;0B
:^B
;dB
;dB
<jB
;�B
<6B
<B
<jB
<�B
<�B
<�B
<�B
=qB
<�B
=B
>�B
>BB
>B
>wB
>BB
>wB
?�B
?B
?�B
?�B
?}B
@�B
@�B
A B
@�B
A B
AUB
@�B
@�B
A B
A B
AUB
@�B
AUB
@�B
B'B
A�B
B'B
A�B
B�B
B'B
C-B
C�B
D�B
D3B
D3B
EB
D�B
D�B
EB
D�B
E9B
E�B
E�B
E�B
F�B
GB
HB
H�B
G�B
GzB
GzB
H�B
H�B
H�B
H�B
I�B
J�B
K^B
K�B
K^B
K�B
K�B
K�B
K^B
K�B
K�B
K�B
K�B
K�B
L0B
L�B
L�B
MjB
M�B
NB
NpB
NB
MjB
MjB
MjB
OvB
N�B
N�B
N�B
OvB
O�B
P}B
O�B
O�B
P�B
P�B
Q�B
Q�B
QNB
Q�B
Q�B
Q�B
QNB
RTB
S�B
S�B
S[B
S�B
S�B
T,B
T,B
S�B
T,B
S�B
T,B
T�B
T�B
T,B
S&B
S&B
S�B
T�B
T�B
T�B
U�B
VB
VmB
V�B
W
B
W?B
V�B
XB
X�B
XyB
YKB
YB
Y�B
Y�B
Y�B
YB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[�B
\)B
\)B
[�B
[WB
[�B
\]B
]/B
]�B
]�B
]�B
^�B
_B
_;B
_pB
_�B
`vB
`�B
`�B
`vB
`�B
aHB
`BB
`BB
`vB
aB
a|B
aB
bB
bB
c B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
c�B
e`B
e�B
e�B
e�B
f2B
e�B
e�B
ffB
g8B
g8B
f�B
gmB
f�B
gB
g�B
h
B
hsB
h�B
jB
jKB
jB
j�B
jB
j�B
k�B
k�B
l"B
lWB
lWB
k�B
l�B
lWB
l�B
l"B
l�B
l"B
l�B
l�B
m]B
m�B
m�B
m�B
m�B
ncB
m�B
n/B
o B
ncB
o B
o B
n�B
n�B
n�B
o5B
o5B
oiB
o B
o5B
o5B
o5B
o B
n�B
n�B
o B
o B
oiB
o5B
o�B
o�B
poB
p�B
poB
poB
poB
poB
poB
qAB
qB
p�B
rB
rGB
q�B
qAB
r�B
sMB
r�B
sMB
sMB
tB
t�B
u�B
t�B
t�B
uZB
u�B
v+B
v`B
v�B
x8B
w�B
xlB
x�B
xlB
y	B
xlB
x�B
y>B
yrB
y>B
y>B
yrB
y�B
zDB
zB
zB
zDB
zDB
yrB
zB
zB
z�B
{B
{JB
z�B
{�B
|B
|�B
}VB
}�B
}VB
}�B
}�B
}VB
}"B
}�B
}�B
}�B
~]B
~�B
~]B
~�B
~�B
.B
�B
�B
�B
�B
cB
�B
cB
�B
�B
�iB
�4B
�B
�;B
��B
��B
��B
�AB
�B
��B
�B
��B
��B
��B
�B
�{B
��B
��B
�MB
��B
��B
��B
��B
��B
�MB
��B
�B
��B
��B
��B
��B
�YB
��B
��B
��B
��B
�+B
��B
��B
��B
�1B
�_B
��B
��B
�B
��B
�fB
��B
�7B
��B
�7B
�7B
�lB
��B
��B
�=B
��B
��B
��B
�rB
�rB
�rB
��B
�DB
�DB
�B
��B
�B
�xB
�B
�B
��B
�JB
��B
��B
�B
�B
��B
�PB
�PB
�PB
��B
�~B
�B
��B
�PB
�PB
��B
�"B
�"B
��B
��B
�VB
��B
�"B
��B
�(B
�\B
��B
�(B
�\B
��B
�\B
��B
��B
�bB
�.B
��B
�bB
� B
�hB
��B
��B
��B
��B
�B
��B
��B
�oB
��B
�@B
oB
�B
hB
B
B
�B
�B
:B
B
�B
�B
�B
:B
�B
�B
�B
B
uB
@B
�B
 B
$B
uB
@B
�B
{B
B
hB
:B
�B
�B
�B
�B
hB
�B
�B
4B
�B
�B
hB
 B
:B
hB
�B
hB
�B
hB
hB
�B
�B
B
B
uB
:B
�B
:B
�B
�B
oB
�B
�B
 B
 B
�B
4B
.B
.B
�B
bB
�B
�B
hB
�B
�B
:B
�B
�B
oB
�B
:B
:B
:B
:B
hB
4B
�B
�B
�B
:B
4B
 B
�B
:B
hB
�B
:B
oB
oB
hB
:B
B
4B
 B
 B
�B
B
�B
B
�B
:B
B
�B
�B
@B
�B
uB
�B
B
B
hB
�B
@B
hB
�B
B
�B
oB
�B
oB
�B
B
oB
�B
�B
�B
�B
�B
:B
�B
�B
�B
�B
uB
�B
�B
MB
MB
MB
MB
�B
�B
{B
@B
�B
@B
�B
�B
�B
�B
�B
FB
uB
�B
�B
B
uB
:B
$B
�B
�B
SB
MB
FB
MB
�B
:B
�B
oB
�B
{B
�B
�B
@B
�B
B
FB
B
:B
@B
�B
�B
{B
�B
B
�B
{B
�B
B
SB
B
oB
�B
:B
@B
�B
�B
FB
�B
�B
�B
�B
+B
1B
$B
B
�B
�B
B
{B
uB
�B
uB
�B
�B
�B
�B
SB
�B
�B
�B
:B
�B
:B
�B
�B
�B
B
�B
B
@B
B
uB
@B
�B
oB
oB
oB
�B
�B
�B
�B
@B
@B
�B
�B
FB
�B
�B
�B
FB
MB
B
FB
�B
�B
�B
�B
�B
��B
��B
��B
�B
�3B
�6B
�$B
�9B
��B
�B
�HB
�[B
��B
�HB
�B
��B
��B
ܒB
�%B
��B
��BoB�B4BBuB@B@B�B�B �B \B �B �B �B �B�B�B�B �B!�B'B4B.�B2�B8B9�B8�B7B8�B8RB?�B6FB:*BC�BEBL�BL�BP}BW�BZ�BX�BYBe`BXEBWsBXyBX�BW?BZ�BV9BV�BW�B[�BO�BQ�BR�B\)BW?B[WB[�B_�B]dB]/Be`Bj�Bj�Bl�Bl�Bs�B��B�=B�B��B�(B��B��B�@B�4B��B�0B�qB��B��B�B�dBʌB��B�&B�B�KB��B��B�|B�8B�/B��B��B�|B��B	lB�BB�B�B�B�BMB$B�B�BxB!bB0�BI�B:�B5�B7LB33B/�B/�B.IB.�B.}B.IB.B+�B/B?HB9�B49B>BCaB;�BJ�B[#B6zBE9Bo�BW
B]dBPHBPHBO�BQ�BQ�BQBOBBNpBPHBO�BO�BNBM�BPBS&BR�BU�B[WBP�BQ�BP�Bc�BcTBZ�BjKBa�BffB^�B[WBR�BQBJ�BG�BH�BL�BO�BM6BY�BT�BY�BN�BL�BP�BN�BP�BM6BM�BO�BR BPHBQNBR�BT�BS�BT�BS�BR�BS�BP}BM6BL�BG�BF?BW�BK^BF�BFBIRBGEBJ�BGEBIRBN<BOvBP}BP�BQ�BPHBV9B\]B\�BY�B]�Be�B[�BS�BQBT�BQ�BT�BN<BFBB[B>�BA�BFBB�BA�BA�BD3BC�BC-B@�B@�BB'BA�B>�B=<B>BB>�B>�B=qB;�B9XB9XB=�B=qB<jB<�B@�BDgBGBE�BDgBEmBGEBF�BE�BCaBE�BE�BD�BC�BC-B=�B8�B1[B0!B/OB,�B+kB*�B,=B+6B(�B&LB$�B%�B%FB#�B$�B$tB$@B"hB#:B$�B#�B$tB$�B&�B&B%�B'RB0!B,=B(�B*eB)�B&B*�B�BVBBkB!bB	B�BB�BhB BB�B B�BB�B{B�B�BB BhB@BbB�B B�B~BB~BDB	lB
	B�B
�B_B~B	�BPB
�B�BJBB
	B�B�BGBuB�BB�B	�B�B��B�JB�B�B��B��B�2B�fB��B�B�B��B�B�B��B�B�5B�)B��B�B��B�B�cB��B�B��B�B�ZB�B�B��B��B�B�B��B��B�)B�dB��B�0B�^B��B�9B�B�aB��B��B��B�[B�}B��B��B�IB�B�RB�_B�0B��B�zB�hB��B��B��B��B�~B�B��B��B�eB��B��B�4B��B��B��B��B�B��B�BrBm�Bj�Bh�Bg�Bc Bh
Bg8Be,B`�B]dB[�B^�BW�BU2BZ�BY�BWsBS�BS&BXBIBM�BP}BDgBD3BA B>B>�B;�B?HB>�BEB3�B3hB2�B2�B3hB2�B4�B2-B0�B2aB2�B1�B4nB4nB3�B3�B1�B/�B/OB2�B2�B3�B4B2�B.}B)�B'B'�B'�B(�B)�B+kB�B�BB"B	�B�B�BuB
�"B�B)�B
�B
��B
�WB
�B
רB
��B
��B
��B
�wB
�B
��B
�B
��B
��B
�B
��B
��B
��B
��B
�oB
��B
��B
�7B
�SB
��B
�oB
~�B
zB
r�B
l�B
p;B
tTB
d&B
_�B
[WB
X�B
k�B
W�B
IB
J�B
A B
@�B
?B
?}B
AUB
=�B
?�B
^�B
1'B
*eB
'�B
*�B
)�B
&�B
(�B
$�B
$�B
!�B
#�B
.IB
B
�B
�B
�B
MB
�B
@B
�B
MB
�B
�B
_G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                   B

{B
	B
	�B

�B

B

B
	@B

B
	�B
	�B

B
	�B
	tB
	tB
	tB
	�B
	tB
	�B

B

FB

�B
	�B

�B

�B
RB
RB
�B
B
�B
�B
�B
�B
RB
_B
YB
RB
�B

{B
LB
�B
B
�xB
��B
�{B0B9B,�B:3BR�BNEBT5BrB��B�B؂B��B�B& B+B'[B3�BK�BI&BG�BH�BN�BZ%BK�BFHBJ�BGBJ,BIZBA�BOKBA�B8�B7�B7�B dB�B�B	�BbBxB��B��B�B��B�'B�3B�CB[`BJ`B3pB+?B(-B�B
�B
�zB
�VB
`B
<�B
"B
B
�B	��B	�EB	�)B	�)B	�tB	�RB	�bB	}+B	t�B	eiB	^�B	[`B	S�B	R)B	UB	O�B	H B	I&B	F�B	8&B	4B	(aB	-�B	RB	=B�JB��B�B�B�,B��BԞBܛB�TB�fB�|B�5B��B��B��BB�<B�TB��B��BװB�lB�oB�B��B�7B	B	"<B	#wB	$�B	(�B	.B	0^B	4B	6NB	6�B	7�B	9�B	;�B	>�B	D�B	E�B	E�B	K2B	M�B	X�B	UB	YB	VB	VvB	X�B	YTB	_�B	]8B	Z�B	Y�B	XNB	X�B	Z%B	[�B	c]B	b�B	b�B	c�B	c�B	d�B	jB	o>B	n�B	r�B	wB	}�B	�\B	��B	��B	�_B	�}B	��B	��B	�-B	��B	�9B	�9B	�nB	��B	�B	�0B	��B	��B	��B	��B	�'B	�gB	�aB	��B	��B	��B	��B	�B	�RB	�#B	��B	��B	�)B	�dB	�B	��B	��B	�vB	�BB	�pB	��B	�<B	��B	��B	�,B	�3B	�3B	�mB	��B	��B	�9B	��B	�EB	��B	��B	�)B	��B	��B	��B	��B	��B	�5B	�5B	��B	B	�5B	�B	��B	�BB	�pB	�BB	�|B	�TB	ǂB	�ZB	�&B	ʕB	��B	�ZB	ʕB	�`B	��B	�,B	��B	�`B	��B	�`B	ʕB	��B	ʕB	�`B	�,B	�8B	̡B	�8B	��B	�KB	�B	έB	�KB	�KB	�KB	��B	ϴB	�WB	ыB	��B	�)B	�]B	�]B	�AB	�iB	�pB	�GB	�GB	װB	�GB	��B	��B	�TB	ڎB	ݡB	�lB	�fB	�fB	��B	�lB	�B	��B	��B	��B	�(B	�B	�WB	�PB	��B	�B	�]B	�B	�B	�B	�iB	�B	��B	��B	�;B	�B	�B	�B	��B	�B	�{B	�B	�B	��B	�B	�B	�B	�B	��B	�B	��B	��B	�7B	�>B	�B	�JB	�xB	�B	�DB	�PB	�JB	��B	�VB	�"B	�B	�B	��B	��B	�:B	��B	�oB	�oB	��B	��B	�B	��B	��B	��B	��B	��B	�+B	��B	�_B	��B	�1B	�1B	�B	�B
 	B
 =B
 �B
 �B
 �B
CB
xB
�B
�B
PB
�B
�B
�B
�B
VB
�B
�B
bB
.B
4B
:B
	@B
	�B
	�B
	�B
	�B

{B

�B
�B
B
�B
�B
�B
�B
_B
*B
�B
kB
�B
B
�B
�B
�B
B
wB
B
}B
�B
�B
B
IB
�B
OB
�B
�B
!B
�B
-B
9B
B
9B
@B
zB
B
$B
�B
^B
�B
�B
*B
 0B
 dB
 �B
!kB
"B
"<B
"�B
"�B
#wB
$B
$IB
%B
$�B
%B
%�B
& B
%�B
& B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(aB
'�B
(aB
'�B
(-B
(aB
(�B
)gB
)�B
*mB
*9B
+tB
+B
+B
+B
*�B
,�B
,B
,zB
+�B
,�B
.�B
-�B
.�B
/XB
.�B
/XB
.�B
/#B
/#B
.�B
0^B
0^B
0�B
0^B
10B
1dB
1dB
2B
2�B
2�B
26B
3<B
2jB
3pB
3pB
4vB
3�B
4BB
4B
4vB
4�B
4�B
4�B
4�B
5}B
4�B
5B
6�B
6NB
6B
6�B
6NB
6�B
7�B
7 B
7�B
7�B
7�B
8�B
8�B
9,B
8�B
9,B
9aB
8�B
8�B
9,B
9,B
9aB
8�B
9aB
8�B
:3B
9�B
:3B
9�B
:�B
:3B
;9B
<
B
<�B
<?B
<?B
=B
<�B
<�B
=B
<�B
=EB
=�B
=�B
=�B
>�B
?B
@#B
@�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
B�B
CjB
C�B
CjB
C�B
DB
C�B
CjB
C�B
DB
C�B
C�B
C�B
D<B
D�B
D�B
EvB
E�B
FB
F|B
FB
EvB
EvB
EvB
G�B
F�B
F�B
F�B
G�B
G�B
H�B
G�B
G�B
H�B
H�B
I�B
I�B
IZB
I�B
I�B
I�B
IZB
J`B
K�B
K�B
KgB
K�B
LB
L8B
L8B
LB
L8B
LB
L8B
L�B
L�B
L8B
K2B
K2B
K�B
L�B
M
B
M
B
M�B
NB
NyB
N�B
OB
OKB
N�B
PB
P�B
P�B
QWB
Q�B
Q�B
Q�B
Q�B
Q�B
R)B
R�B
R�B
R�B
R�B
R�B
ScB
S�B
T5B
T5B
S�B
ScB
TB
TiB
U;B
U�B
U�B
U�B
V�B
WB
WGB
W|B
W�B
X�B
X�B
X�B
X�B
X�B
YTB
XNB
XNB
X�B
YB
Y�B
YB
Z%B
Z%B
[,B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
]lB
]�B
]�B
]�B
^>B
]�B
^
B
^rB
_DB
_DB
^�B
_yB
^�B
_B
_�B
`B
`B
`�B
b�B
bWB
b�B
b�B
b�B
b�B
c�B
c�B
d.B
dcB
dcB
c�B
d�B
dcB
d�B
d.B
d�B
d.B
e B
e B
eiB
fB
e�B
fB
e�B
foB
fB
f;B
gB
foB
gB
gB
f�B
f�B
f�B
gAB
gAB
guB
gB
gAB
gAB
gAB
gB
f�B
f�B
gB
gB
guB
gAB
g�B
g�B
h{B
h�B
h{B
h{B
h{B
h{B
h{B
iMB
iB
h�B
jB
jSB
i�B
iMB
j�B
kYB
j�B
kYB
kYB
l+B
l�B
m�B
l�B
l�B
mfB
m�B
n7B
nlB
n�B
pDB
o�B
pxB
p�B
pxB
qB
pxB
p�B
qJB
q~B
qJB
qJB
q~B
q�B
rPB
rB
rB
rPB
rPB
q~B
rB
rB
r�B
s�B
sVB
r�B
s�B
t(B
t�B
ubB
u�B
ubB
u�B
u�B
ubB
u.B
u�B
u�B
u�B
viB
v�B
viB
v�B
wB
w:B
w�B
w�B
w�B
w�B
woB
w�B
woB
w�B
w�B
xuB
x@B
yB
yGB
y�B
y�B
y�B
zMB
zB
z�B
zB
z�B
z�B
z�B
{B
{�B
{�B
{�B
|YB
{�B
{�B
{�B
|�B
|�B
|YB
|�B
}+B
}�B
}�B
}�B
}�B
~eB
B
B
~�B
~�B
7B
�B
�B
�	B
�=B
kB
�	B
��B
�B
��B
�rB
��B
�CB
��B
�CB
�CB
�xB
��B
��B
�IB
��B
��B
��B
�~B
�~B
�~B
��B
�PB
�PB
�B
��B
�B
��B
�!B
�!B
��B
�VB
��B
��B
�!B
�!B
��B
�\B
�\B
�\B
��B
��B
�'B
��B
�\B
�\B
��B
�.B
�.B
��B
��B
�bB
��B
�.B
��B
�4B
�hB
��B
�4B
�hB
��B
�hB
��B
��B
�nB
�:B
��B
�nB
�B
�tB
��B
��B
��B
��B
�B
��B
��B
�{B
��B
�LB

{B

�B
	tB
$B
'B
�B

�B

FB

B

�B
	�B

�B

FB

�B
�B

�B
B
�B
LB
�B
	B
0B
�B
LB
�B
�B
B
	tB

FB
	�B
�B
	�B
	�B
	tB
�B
�B
	@B
�B
�B
	tB
	B

FB
	tB
	�B
	tB

�B
	tB
	tB

�B

�B
B
B
�B

FB

�B

FB

�B

�B

{B
	�B
	�B
	B
	B
�B
	@B
:B
:B
�B
nB
B
�B
	tB
�B
	�B

FB

�B
�B

{B
	�B

FB

FB

FB

FB
	tB
	@B
�B
�B
�B

FB
	@B
	B

�B

FB
	tB
�B

FB

{B

{B
	tB

FB

B
	@B
	B
	B
	�B

B
�B

B
	�B

FB
B

�B

�B
LB

�B
�B

�B

B

B
	tB
	�B
LB
	tB
	�B

B
	�B

{B
	�B

{B

�B
B

{B
B
�B
�B
�B
�B

FB
	�B

�B
�B
�B
�B
�B
�B
YB
YB
YB
YB
�B
�B
�B
LB
�B
LB

�B
�B
�B
�B
�B
RB
�B

�B
�B
$B
�B

FB
0B
�B
�B
_B
YB
RB
YB
�B

FB

�B

{B

�B
�B
�B
�B
LB
�B
B
RB
B

FB
LB
	�B
�B
�B
�B
B
�B
�B
�B
B
_B
B

{B
�B

FB
LB
�B
�B
RB
�B
�B
B
�B
7B
=B
0B
*B
�B
	�B
B
�B
�B
�B
�B
�B
�B
�B
�B
_B
�B
�B

�B

FB
	�B

FB
	�B
	�B

�B
B
	�B
B
LB
B
�B
LB
�B

{B

{B

{B

�B
�B

�B

�B
LB
LB

�B

�B
RB
�B
�B
�B
RB
YB
B
RB
�B
�B
�B
�B	��B
��B
��B
��B
�'B
�?B
�BB
�0B
�EB
��B
� B
�TB
�gB
��B
�TB
�&B
��B
�
B
ԞB
�1B
��B
�B
�{B
��B	@BB�BLBLB�B�BBhB�BBB�B�B�B�B�B�B*B,B&�B*�B0)B1�B0�B/#B0�B0^B7�B.RB26B;�B=BD�BD�BH�BO�BR�BP�BQ#B]lBPQBOBP�BP�BOKBR�BNEBN�BO�BTBG�BI�BJ�BT5BOKBScBS�BW�BUpBU;B]lBb�Bb�Bd�Be Bk�B��B�IB�B��B�4B��B��B�LB�@B��B�<B�}B��B��B�B�pBB�B�2B�)B�WB��B�BوB�DB�;B��B��B�B��BxB��BB��B�B�B�BYB0BBB�BnB(�BA�B2�B-�B/XB+?B'�B'�B&UB&�B&�B&UB& B#�B''B7TB1�B,EB6B;mB3�BB�BS/B.�B=EBg�BOBUpBHTBHTBG�BI�BI�BI&BGNBF|BHTBG�BG�BFBE�BH BK2BJ�BM�BScBH�BI�BH�B[�B[`BR�BbWBY�B^rBV�BScBJ�BI&BB�B?�B@�BD�BG�BEBBQ�BL�BQ�BF�BD�BH�BF�BH�BEBBE�BG�BJ,BHTBIZBJ�BL�BK�BM
BK�BJ�BK�BH�BEBBD�B?�B>KBO�BCjB>�B>BA^B?QBB�B?QBA^BFHBG�BH�BH�BI�BHTBNEBTiBUBQ�BU�B]�BS�BLBI&BM
BI�BL�BFHB>B:gB6�B9�B>B:�B9�B9�B<?B;�B;9B8�B8�B:3B9�B6�B5HB6NB6�B6�B5}B3�B1dB1dB5�B5}B4vB4�B8�B<sB?B=�B<sB=yB?QB>�B=�B;mB=�B=�B<�B<
B;9B5�B0�B)gB(-B'[B$�B#wB"�B$IB#BB!BXB�B�BRB�B�B�BLBtBFB�B�B�B�B�B$B�B^B(-B$IB �B"qB!�B$B"�B�BbB!BwBnBB�BB�B	tB	B
B�B	BB
B
�B�B
�B�BB	B	tBLBnB�B	B�B�B
B�BPBxBB�B�B�kB�B�B\B�B�BVBBB��B��B�SB��B��B�%B��B�B�B��B�VB�"B�B�B�B�>B�rB��B�B�B��B�B�B��B�B�AB�5B� B�B��B�B�oB��B�B��BާB�fBڎB�B��B��BڎB�B�B��B�5B�pB��B�<B�jB��B�EB�#B�mB��B��B��B�gB��B��B��B�UB�B�^B�kB�<B��B��B�tB��B��B��B��B��B�!B��B��B�qB��B��B�@B��B��B��B��ByB��BzBjBe�Bb�B`�B_�B[,B`B_DB]8BX�BUpBS�BV�BO�BM>BR�BQ�BOBK�BK2BPBA)BE�BH�B<sB<?B9,B6B6�B3�B7TB6�B=B+�B+tB*�B*�B+tB+B,�B*9B(�B*mB*�B)�B,zB,zB+�B+�B)�B'�B'[B*�B+B+�B,B*�B&�B!�B*B�B�B �B!�B#wB�B�B'B.B�B
��B�B
��B
�.B
��B!�B
�(B
��B
�cB
�#B
ϴB
��B
��B
��B
��B
� B
��B
�#B
��B
��B
�B
��B
��B
�B
��B
�{B
��B
��B
�CB
}_B
z�B
y{B
v�B
rB
j�B
e B
hGB
l`B
\2B
W�B
ScB
P�B
c�B
O�B
A)B
B�B
9,B
8�B
7 B
7�B
9aB
5�B
7�B
V�B
)3B
"qB
�B
"�B
"B
�B
!B
�B
�B
B
�B
&UB
'B
�B
B
�B
YB
�B
LB
�B
YB
B
�B	�kG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223223                            20230426223223AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622322320230426223223  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322320230426223223QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322320230426223223QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               