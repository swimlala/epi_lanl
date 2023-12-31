CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-27T09:00:47Z creation      
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
resolution        =���   axis      Z        8  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  b�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8 p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 5�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8 =x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � \�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8 d�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  20230727090047  20230727090047  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�; �[�@�; �[�11  @�; �l'@�; �l'@1�=p��
@1�=p��
�d��=�K�d��=�K11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?�\)@�\@B�\@�  @�  @�G�@�G�A ��A  A ��A,(�A@��A`��A\)A�\)A��A�  A�Q�A�  A�Q�A�A��BQ�B  B�B�
B(  B0Q�B8Q�B@  BH  BP  BW�
B`(�Bhz�Bp  Bw�B�
B��B�  B�  B��B�  B��B��B��B��B�  B��B��B�{B��B��B�{B��B��B�  B��B�{B�{B�{B��B��B�  B�{B�  B��B��B�{C {C  C  C
=C
=C
  C
=C{C
=C��C  C
=C  C
=C  C  C   C"  C#��C%��C(  C*
=C,  C.  C/��C2  C4
=C6  C8  C:
=C<
=C=��C?��CB  CC��CF  CH  CJ  CL  CN  CP
=CQ��CT
=CV
=CX{CZ{C\
=C^{C`
=Ca��Cd
=Cf
=Ch  Ci��Ck��Cm��Cp  Cr  Ct  Cv
=Cx  Cz  C|  C~  C�C�  C�  C�C�  C�  C�  C�  C�  C�  C���C�C�  C�C�C�  C���C�C�  C���C���C���C���C�C�  C�  C�  C�  C���C�  C�  C���C�  C�C�C�  C�  C���C���C���C�  C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C�C�C�C�  C�  C�C�
=C�  C��C���C�  C�  C�C�  C�  C���C���C���C�  C�C�C�C�C���C���C�C�C�  C�  C�  C�C�  C���C���C���C���C�  C�C���C���C���C�  C�C���C�C�C�  C�  C���C���C�  C�  C�  C�C���C���C�  C�  C�  C�C�C�  C�C�  C�C�C�  C�  C�C�
=C�C�C���C���C���C���C�  D D �D�D�D�D��D�D� D  D� D�D� D�D��D�D� D�qD}qD	  D	��D
�D
� D�D��DD��D�qD}qD  D� D�qD� D  D}qD�D��D  D}qD�qD}qD  D� D�D��D�qD� DD}qD��D� D  D��D�D� D  D}qD�qD}qD�qDz�D�qD}qD��D� D D ��D!  D!� D"  D"� D#  D#}qD$  D$��D%�D%}qD&�D&��D'D'��D(  D(}qD(��D)}qD*  D*� D*��D+}qD,  D,}qD,��D-}qD.�D.}qD.�qD/}qD0  D0� D0�qD1}qD2�D2�D3�D3�D4  D4z�D4�qD5�D6D6��D6�qD7}qD8  D8��D9  D9� D:�D:� D:�qD;��D<  D<}qD=  D=}qD=��D>� D>�qD?z�D@  D@��DA  DA��DB�DB� DCDC�DD  DD}qDD�qDE}qDF�DF�DG�DG� DH�DH�DI  DI� DJ  DJ� DK�DK� DL�DL�DM  DM�DN  DNz�DO�DO�DP  DP��DQ  DQ}qDR�DR��DR�qDS}qDS�qDT}qDT�qDU� DV  DV��DW  DW��DX�DX��DY�DYz�DY�qDZ� D[�D[�D\�D\� D\��D]z�D^  D^� D_�D_� D_��D`}qD`�qDa� Db�Db��Dc  Dc}qDd�Dd��De  De� Df  Df� Dg�Dg��Dg�qDh}qDi  Di� Di�qDj}qDj�qDk}qDl  Dl��Dm  Dm� Dn  Dn�Do�Do��Dp  Dp� Dp�qDq}qDq�qDr}qDr�qDs}qDs��Dtz�Dt�qDu� Du�qDv}qDw  Dw� Dx  Dx� Dy  Dy��Dz  Dz��D{  D{}qD|  D|��D}�D}� D~�D~��D  D}qD�qD�AHD���D�D�HD�@ D�� D��HD�HD�AHD��HD��HD�HD�>�D�� D��HD���D�>�D�� D�� D���D�@ D��HD�� D���D�@ D�� D���D�  D�>�D�}qD�� D�  D�AHD��HD�� D�HD�AHD�� D�D�HD�>�D�� D��HD�  D�@ D�� D��HD�HD�@ D�~�D�� D�HD�AHD�� D�� D�HD�B�D���D�� D�  D�AHD�� D���D�  D�AHD��HD�� D��qD�>�D�� D�� D��D�B�D�~�D���D�HD�AHD��HD���D�  D�AHD�� D���D�  D�@ D��HD�� D�  D�@ D��HD�� D�  D�@ D�� D�D�HD�@ D�� D��HD�HD�B�D�� D�� D�HD�AHD�~�D���D�HD�>�D�� D�D�HD�AHD�~�D���D���D�>�D��HD�� D�  D�AHD�~�D�� D�  D�>�D��HD��HD�  D�>�D�}qD���D�  D�@ D��HD�� D���D�>�D�� D��HD�  D�AHD�� D�� D�  D�>�D�}qD�� D��D�@ D�� D�� D�  D�AHD��HD�� D���D�@ D��HD��HD�HD�@ D�� D�� D���D�=qD�� D��HD�  D�>�D�~�D���D��D�@ D�~�D�� D�HD�@ D�}qD���D�HD�@ D�~�D�� D�HD�AHD�~�D���D�HD�>�D�~�D���D�  D�@ D�~�D���D�  D�>�D�~�D�� D���D�=qD�� D�� D���D�@ D��HD��HD��D�>�D�~�D���D���D�>�D�� D��HD�  D�AHD�� D���D�  D�AHD���D�� D�  D�AHD�� D�� D���D�@ D��HD��HD�HD�AHD��HD��HD�  D�@ D�~�D���D���D�>�D�� D���D�  D�AHD��HD�� D��qD�>�D�~�D�� D��D�@ D�� D��HD�  D�=qD�� D���D��qD�>�D�~�D��HD�HD�@ D D��HD�HD�>�D�}qDþ�D���D�@ DĀ Dľ�D�HD�AHDŀ D�� D�HD�@ D�~�D�� D�  D�@ Dǀ D��HD��D�AHDȂ�D��HD�HD�@ D�~�Dɾ�D��qD�=qD�}qD�� D��D�B�Dˀ D˾�D���D�>�D�~�D̾�D�  D�@ D�~�D��HD�HD�>�D΁HD�D�  D�@ DρHD��HD�HD�>�DЀ D�D��D�AHDсHD�� D���D�>�DҀ DҾ�D�  D�@ DӁHD��HD�HD�AHDԁHD�� D�  D�AHDՁHD�D�HD�AHDցHD�� D�  D�>�D�~�D׾�D��qD�=qD�}qDؾ�D�  D�@ Dـ D��HD���D�>�D�~�DڽqD�  D�@ D�}qD۽qD���D�>�D܀ D�� D���D�@ D݁HD��HD�  D�@ Dހ D޽qD���D�AHD߀ D�� D�  D�@ D�� DྸD�  D�@ D�HD��HD�  D�>�D�~�D⾸D�HD�AHD�HD��HD�  D�B�D�HD�� D�HD�@ D�HD��HD�  D�>�D�~�D�� D�  D�>�D�HD�D�HD�@ D�HD�� D���D�@ D�~�D龸D�  D�@ DꂏD��HD�  D�@ D� D��HD�  D�=qD�HD���D�HD�>�D� D�� D���D�>�D�~�DD���D�>�D�~�D�qD���D�>�D��HD��HD���D�>�D� D�� D��D�@ D�~�D�D�  D�AHD�D�D�HD�AHD�~�D��)D��qD�>�D�� D��HD�HD�>�D�~�D�� D�  D�AHD�}qD���D��)D�%>�?k�?��R?�Q�?��@
=q@��@5@G�@Y��@u@�ff@���@�p�@���@�33@\@���@�Q�@�ff@�{@�(�A�
AQ�A\)A�
A�HA!G�A%�A,(�A1G�A5A=p�AA�AG
=AMp�AQG�AW�A\��AaG�AhQ�Al(�Aq�Aw�A{�A�G�A��A�{A���A��
A�ffA��A�(�A��RA��\A�z�A��A��HA��A���A�33A�A���A�33A�p�A�G�A��A�A�G�A�33A�{A���A��HA�ffA�Q�AӅA�ffA�Q�A��
A�{A���A��
A�A�G�A�A�ffA�G�A�33A��RA���A��
A�
=B z�BB\)Bz�B{B33B��B
=qB\)Bz�BffB\)B��B=qB
=B��BB
=Bz�B��B�RBQ�Bp�B�\B (�B!B"�RB$z�B%�B'
=B(��B*=qB+�B-G�B.�RB/�
B1p�B2�HB4  B5B733B8(�B9B;33B<(�B=�B?33B@(�BA�BC
=BDQ�BE�BG33BH(�BI��BK33BLQ�BM��BO33BPz�BQp�BS
=BTz�BUp�BV�HBXz�BYG�BZ�HB\(�B\��B^�RB_�
Ba�Bb�RBc�Bd��BfffBg\)Bh��Bj{Bk
=Bl��BmBo
=Bpz�Bqp�Br�RBtQ�BuG�Bv=qBw�
Bx��By�B{�B|z�B}p�B
=B�
B��RB�G�B��B�ffB��B��B�=qB���B�p�B�=qB���B�G�B�  B��RB�33B��
B���B��B�B��\B�
=B��B�ffB���B��B�Q�B���B�G�B�  B��RB��B�B��\B�
=B��B�Q�B��RB�\)B�{B�z�B�
=B�B�ffB���B��B�(�B��\B�G�B��B�Q�B���B�B�=qB��RB��B�  B�z�B�33B��
B�=qB��HB���B�{B��\B�G�B��B�Q�B���B��B�=qB��RB��B�{B��\B�p�B�  B�z�B�33B��
B�Q�B�
=B�B�{B���B��B�  B��\B�\)B�  B�z�B��B��B�ffB���B�B�Q�B��HB���B�Q�B���B��B�Q�B�
=B�p�B�=qB�
=B���B�(�B�
=BîB�=qB���BŮB�ffB��HBǅB�=qB���Bə�B�{B��HB˙�B�{B̸RB͙�B�(�BθRBυB�Q�BиRB�\)B�(�B��HB�G�B��
Bԣ�B��BՅB�(�B���B�33Bי�B�Q�BظRB��Bٙ�B�(�Bڣ�B���B�\)B��
B�{B܏\B��HB�
=Bݙ�B�  B�=qB�z�B���B�\)Bߙ�B��B�z�B���B�
=B�p�B��B�=qB�z�B���B�p�B�B�(�B�\B��HB��B�B�  B�Q�B��B�33B癚B��
B�(�B��B��B�p�B�B�{B��B�
=B�33B뙚B�(�B�z�B�RB�G�B��B��B�Q�B��HB��B�p�B��
B�ffB���B�
=B�B��B�(�B�\B��B�p�B�B�=qB���B��HB�33B��B�{B�ffB���B�33B���B��
B�Q�B���B�
=B�p�B�  B�ffB���B�
=B�p�B�  B�Q�B��\B��B��B�B�(�B��RB���B�G�B��
C �C 33C p�C �C �HC  C(�C\)C��C��C�C(�C\)Cz�C��C�HC�C=qCffC��C��C�HC(�CQ�Cp�C�C��C�HC(�CG�CffC�C��C�C33CQ�Cp�C�C�HC��C(�CffC�C��C�C�C33Cp�C��CC�C	33C	\)C	z�C	C	��C

=C
=qC
�C
�RC
�
C{CQ�Cp�C��C�C
=C(�Cz�C�C��C  CG�CffC�\C�
C  C�C\)C�\CC�HC�C\)Cz�C�C�HC
=C(�Cp�C��CC�C33CQ�Cz�C�RC��C�CG�C�\C�RC�HC(�CQ�Cz�CC��C{C\)C��C�RC�C(�Cp�C��C�RC�C=qC\)C�C��C  C�CG�C��C��C�C{C\)C��CC��C=qCz�C��C��C
=CQ�C�\C�RC�C33CffC�\C��C{CQ�Cp�C�RC  C33C\)C��C�
C{C=qCffC�C��C(�CG�C�C��C 
=C (�C ffC �C �C!{C!G�C!��C!��C!�C"33C"z�C"��C"�
C#(�C#Q�C#�C#��C${C$=qC$p�C$��C$�C%33C%ffC%�\C%C&  C&G�C&�C&��C&�C'33C'\)C'�C'C(
=C(G�C(z�C(��C(�
C)(�C)\)C)�C)��C*�C*Q�C*z�C*�C*��C+=qC+p�C+��C+�HC,33C,\)C,�\C,�
C-{C-Q�C-z�C-�C-�C.33C.p�C.�C.�
C/
=C/G�C/��C/�
C0{C0G�C0z�C0C1
=C1G�C1p�C1�C1��C233C2p�C2��C2�
C3�C3p�C3��C3��C4
=C4\)C4��C4�
C5  C5=qC5�\C5�
C6
=C633C6p�C6C6�C7{C7\)C7��C7�HC8
=C8=qC8z�C8��C9  C9(�C9ffC9��C9�HC:(�C:ffC:�\C:C:��C;33C;�C;�RC;�HC<�C<ffC<��C<�HC=
=C=G�C=�C=��C>  C>33C>ffC>��C>�HC?(�C?G�C?z�C?�RC@  C@=qC@p�C@��C@��CA  CAG�CA�CACA�HCB{CB\)CB��CB��CC  CC33CC�CCCC��CD{CDQ�CD��CD�
CE{CEG�CEffCE��CE�HCF�CFffCF��CF�RCF�CG33CGz�CG��CG��CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                               ?�\)@�\@B�\@�  @�  @�G�@�G�A ��A  A ��A,(�A@��A`��A\)A�\)A��A�  A�Q�A�  A�Q�A�A��BQ�B  B�B�
B(  B0Q�B8Q�B@  BH  BP  BW�
B`(�Bhz�Bp  Bw�B�
B��B�  B�  B��B�  B��B��B��B��B�  B��B��B�{B��B��B�{B��B��B�  B��B�{B�{B�{B��B��B�  B�{B�  B��B��B�{C {C  C  C
=C
=C
  C
=C{C
=C��C  C
=C  C
=C  C  C   C"  C#��C%��C(  C*
=C,  C.  C/��C2  C4
=C6  C8  C:
=C<
=C=��C?��CB  CC��CF  CH  CJ  CL  CN  CP
=CQ��CT
=CV
=CX{CZ{C\
=C^{C`
=Ca��Cd
=Cf
=Ch  Ci��Ck��Cm��Cp  Cr  Ct  Cv
=Cx  Cz  C|  C~  C�C�  C�  C�C�  C�  C�  C�  C�  C�  C���C�C�  C�C�C�  C���C�C�  C���C���C���C���C�C�  C�  C�  C�  C���C�  C�  C���C�  C�C�C�  C�  C���C���C���C�  C�  C�  C�  C�  C�  C�  C�C�  C�  C�  C�C�C�C�  C�  C�C�
=C�  C��C���C�  C�  C�C�  C�  C���C���C���C�  C�C�C�C�C���C���C�C�C�  C�  C�  C�C�  C���C���C���C���C�  C�C���C���C���C�  C�C���C�C�C�  C�  C���C���C�  C�  C�  C�C���C���C�  C�  C�  C�C�C�  C�C�  C�C�C�  C�  C�C�
=C�C�C���C���C���C���C�  D D �D�D�D�D��D�D� D  D� D�D� D�D��D�D� D�qD}qD	  D	��D
�D
� D�D��DD��D�qD}qD  D� D�qD� D  D}qD�D��D  D}qD�qD}qD  D� D�D��D�qD� DD}qD��D� D  D��D�D� D  D}qD�qD}qD�qDz�D�qD}qD��D� D D ��D!  D!� D"  D"� D#  D#}qD$  D$��D%�D%}qD&�D&��D'D'��D(  D(}qD(��D)}qD*  D*� D*��D+}qD,  D,}qD,��D-}qD.�D.}qD.�qD/}qD0  D0� D0�qD1}qD2�D2�D3�D3�D4  D4z�D4�qD5�D6D6��D6�qD7}qD8  D8��D9  D9� D:�D:� D:�qD;��D<  D<}qD=  D=}qD=��D>� D>�qD?z�D@  D@��DA  DA��DB�DB� DCDC�DD  DD}qDD�qDE}qDF�DF�DG�DG� DH�DH�DI  DI� DJ  DJ� DK�DK� DL�DL�DM  DM�DN  DNz�DO�DO�DP  DP��DQ  DQ}qDR�DR��DR�qDS}qDS�qDT}qDT�qDU� DV  DV��DW  DW��DX�DX��DY�DYz�DY�qDZ� D[�D[�D\�D\� D\��D]z�D^  D^� D_�D_� D_��D`}qD`�qDa� Db�Db��Dc  Dc}qDd�Dd��De  De� Df  Df� Dg�Dg��Dg�qDh}qDi  Di� Di�qDj}qDj�qDk}qDl  Dl��Dm  Dm� Dn  Dn�Do�Do��Dp  Dp� Dp�qDq}qDq�qDr}qDr�qDs}qDs��Dtz�Dt�qDu� Du�qDv}qDw  Dw� Dx  Dx� Dy  Dy��Dz  Dz��D{  D{}qD|  D|��D}�D}� D~�D~��D  D}qD�qD�AHD���D�D�HD�@ D�� D��HD�HD�AHD��HD��HD�HD�>�D�� D��HD���D�>�D�� D�� D���D�@ D��HD�� D���D�@ D�� D���D�  D�>�D�}qD�� D�  D�AHD��HD�� D�HD�AHD�� D�D�HD�>�D�� D��HD�  D�@ D�� D��HD�HD�@ D�~�D�� D�HD�AHD�� D�� D�HD�B�D���D�� D�  D�AHD�� D���D�  D�AHD��HD�� D��qD�>�D�� D�� D��D�B�D�~�D���D�HD�AHD��HD���D�  D�AHD�� D���D�  D�@ D��HD�� D�  D�@ D��HD�� D�  D�@ D�� D�D�HD�@ D�� D��HD�HD�B�D�� D�� D�HD�AHD�~�D���D�HD�>�D�� D�D�HD�AHD�~�D���D���D�>�D��HD�� D�  D�AHD�~�D�� D�  D�>�D��HD��HD�  D�>�D�}qD���D�  D�@ D��HD�� D���D�>�D�� D��HD�  D�AHD�� D�� D�  D�>�D�}qD�� D��D�@ D�� D�� D�  D�AHD��HD�� D���D�@ D��HD��HD�HD�@ D�� D�� D���D�=qD�� D��HD�  D�>�D�~�D���D��D�@ D�~�D�� D�HD�@ D�}qD���D�HD�@ D�~�D�� D�HD�AHD�~�D���D�HD�>�D�~�D���D�  D�@ D�~�D���D�  D�>�D�~�D�� D���D�=qD�� D�� D���D�@ D��HD��HD��D�>�D�~�D���D���D�>�D�� D��HD�  D�AHD�� D���D�  D�AHD���D�� D�  D�AHD�� D�� D���D�@ D��HD��HD�HD�AHD��HD��HD�  D�@ D�~�D���D���D�>�D�� D���D�  D�AHD��HD�� D��qD�>�D�~�D�� D��D�@ D�� D��HD�  D�=qD�� D���D��qD�>�D�~�D��HD�HD�@ D D��HD�HD�>�D�}qDþ�D���D�@ DĀ Dľ�D�HD�AHDŀ D�� D�HD�@ D�~�D�� D�  D�@ Dǀ D��HD��D�AHDȂ�D��HD�HD�@ D�~�Dɾ�D��qD�=qD�}qD�� D��D�B�Dˀ D˾�D���D�>�D�~�D̾�D�  D�@ D�~�D��HD�HD�>�D΁HD�D�  D�@ DρHD��HD�HD�>�DЀ D�D��D�AHDсHD�� D���D�>�DҀ DҾ�D�  D�@ DӁHD��HD�HD�AHDԁHD�� D�  D�AHDՁHD�D�HD�AHDցHD�� D�  D�>�D�~�D׾�D��qD�=qD�}qDؾ�D�  D�@ Dـ D��HD���D�>�D�~�DڽqD�  D�@ D�}qD۽qD���D�>�D܀ D�� D���D�@ D݁HD��HD�  D�@ Dހ D޽qD���D�AHD߀ D�� D�  D�@ D�� DྸD�  D�@ D�HD��HD�  D�>�D�~�D⾸D�HD�AHD�HD��HD�  D�B�D�HD�� D�HD�@ D�HD��HD�  D�>�D�~�D�� D�  D�>�D�HD�D�HD�@ D�HD�� D���D�@ D�~�D龸D�  D�@ DꂏD��HD�  D�@ D� D��HD�  D�=qD�HD���D�HD�>�D� D�� D���D�>�D�~�DD���D�>�D�~�D�qD���D�>�D��HD��HD���D�>�D� D�� D��D�@ D�~�D�D�  D�AHD�D�D�HD�AHD�~�D��)D��qD�>�D�� D��HD�HD�>�D�~�D�� D�  D�AHD�}qD���D��)D�%>�?k�?��R?�Q�?��@
=q@��@5@G�@Y��@u@�ff@���@�p�@���@�33@\@���@�Q�@�ff@�{@�(�A�
AQ�A\)A�
A�HA!G�A%�A,(�A1G�A5A=p�AA�AG
=AMp�AQG�AW�A\��AaG�AhQ�Al(�Aq�Aw�A{�A�G�A��A�{A���A��
A�ffA��A�(�A��RA��\A�z�A��A��HA��A���A�33A�A���A�33A�p�A�G�A��A�A�G�A�33A�{A���A��HA�ffA�Q�AӅA�ffA�Q�A��
A�{A���A��
A�A�G�A�A�ffA�G�A�33A��RA���A��
A�
=B z�BB\)Bz�B{B33B��B
=qB\)Bz�BffB\)B��B=qB
=B��BB
=Bz�B��B�RBQ�Bp�B�\B (�B!B"�RB$z�B%�B'
=B(��B*=qB+�B-G�B.�RB/�
B1p�B2�HB4  B5B733B8(�B9B;33B<(�B=�B?33B@(�BA�BC
=BDQ�BE�BG33BH(�BI��BK33BLQ�BM��BO33BPz�BQp�BS
=BTz�BUp�BV�HBXz�BYG�BZ�HB\(�B\��B^�RB_�
Ba�Bb�RBc�Bd��BfffBg\)Bh��Bj{Bk
=Bl��BmBo
=Bpz�Bqp�Br�RBtQ�BuG�Bv=qBw�
Bx��By�B{�B|z�B}p�B
=B�
B��RB�G�B��B�ffB��B��B�=qB���B�p�B�=qB���B�G�B�  B��RB�33B��
B���B��B�B��\B�
=B��B�ffB���B��B�Q�B���B�G�B�  B��RB��B�B��\B�
=B��B�Q�B��RB�\)B�{B�z�B�
=B�B�ffB���B��B�(�B��\B�G�B��B�Q�B���B�B�=qB��RB��B�  B�z�B�33B��
B�=qB��HB���B�{B��\B�G�B��B�Q�B���B��B�=qB��RB��B�{B��\B�p�B�  B�z�B�33B��
B�Q�B�
=B�B�{B���B��B�  B��\B�\)B�  B�z�B��B��B�ffB���B�B�Q�B��HB���B�Q�B���B��B�Q�B�
=B�p�B�=qB�
=B���B�(�B�
=BîB�=qB���BŮB�ffB��HBǅB�=qB���Bə�B�{B��HB˙�B�{B̸RB͙�B�(�BθRBυB�Q�BиRB�\)B�(�B��HB�G�B��
Bԣ�B��BՅB�(�B���B�33Bי�B�Q�BظRB��Bٙ�B�(�Bڣ�B���B�\)B��
B�{B܏\B��HB�
=Bݙ�B�  B�=qB�z�B���B�\)Bߙ�B��B�z�B���B�
=B�p�B��B�=qB�z�B���B�p�B�B�(�B�\B��HB��B�B�  B�Q�B��B�33B癚B��
B�(�B��B��B�p�B�B�{B��B�
=B�33B뙚B�(�B�z�B�RB�G�B��B��B�Q�B��HB��B�p�B��
B�ffB���B�
=B�B��B�(�B�\B��B�p�B�B�=qB���B��HB�33B��B�{B�ffB���B�33B���B��
B�Q�B���B�
=B�p�B�  B�ffB���B�
=B�p�B�  B�Q�B��\B��B��B�B�(�B��RB���B�G�B��
C �C 33C p�C �C �HC  C(�C\)C��C��C�C(�C\)Cz�C��C�HC�C=qCffC��C��C�HC(�CQ�Cp�C�C��C�HC(�CG�CffC�C��C�C33CQ�Cp�C�C�HC��C(�CffC�C��C�C�C33Cp�C��CC�C	33C	\)C	z�C	C	��C

=C
=qC
�C
�RC
�
C{CQ�Cp�C��C�C
=C(�Cz�C�C��C  CG�CffC�\C�
C  C�C\)C�\CC�HC�C\)Cz�C�C�HC
=C(�Cp�C��CC�C33CQ�Cz�C�RC��C�CG�C�\C�RC�HC(�CQ�Cz�CC��C{C\)C��C�RC�C(�Cp�C��C�RC�C=qC\)C�C��C  C�CG�C��C��C�C{C\)C��CC��C=qCz�C��C��C
=CQ�C�\C�RC�C33CffC�\C��C{CQ�Cp�C�RC  C33C\)C��C�
C{C=qCffC�C��C(�CG�C�C��C 
=C (�C ffC �C �C!{C!G�C!��C!��C!�C"33C"z�C"��C"�
C#(�C#Q�C#�C#��C${C$=qC$p�C$��C$�C%33C%ffC%�\C%C&  C&G�C&�C&��C&�C'33C'\)C'�C'C(
=C(G�C(z�C(��C(�
C)(�C)\)C)�C)��C*�C*Q�C*z�C*�C*��C+=qC+p�C+��C+�HC,33C,\)C,�\C,�
C-{C-Q�C-z�C-�C-�C.33C.p�C.�C.�
C/
=C/G�C/��C/�
C0{C0G�C0z�C0C1
=C1G�C1p�C1�C1��C233C2p�C2��C2�
C3�C3p�C3��C3��C4
=C4\)C4��C4�
C5  C5=qC5�\C5�
C6
=C633C6p�C6C6�C7{C7\)C7��C7�HC8
=C8=qC8z�C8��C9  C9(�C9ffC9��C9�HC:(�C:ffC:�\C:C:��C;33C;�C;�RC;�HC<�C<ffC<��C<�HC=
=C=G�C=�C=��C>  C>33C>ffC>��C>�HC?(�C?G�C?z�C?�RC@  C@=qC@p�C@��C@��CA  CAG�CA�CACA�HCB{CB\)CB��CB��CC  CC33CC�CCCC��CD{CDQ�CD��CD�
CE{CEG�CEffCE��CE�HCF�CFffCF��CF�RCF�CG33CGz�CG��CG��CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ĜA�ƨA�ȴA�ȴA�A�A���A�A���A�ĜA���A���A���A���A���A���A��
A��
A���A���A��
A��A��/A��/A��/A��HA��TA��A��A��mA��A���A���A���A���A���A���A��yA���A۩�AۓuA�dZA�7LA��A���A�|�A���A�C�Aؗ�A��yA׺^A��;A���AӁAҼjA�XAρAͲ-A���A�~�A�bNAǁA���A�^5A��/A��
A��A���A���A���A�;dA���A��#A�p�A�A�A�A�K�A�Q�A�  A�  A��-A�;dA�;dA��DA�"�A�ĜA�dZA��!A�XA��A��HA�33A�^5A�A�A�$�A�"�A���A�C�A�r�A��A�$�A� �A��jA�n�A�A�C�A��RA�$�A�G�A�9XA��A�$�A���Az�9Aw;dAs��An��Am�hAl=qAk�-Aj��Ai
=Ae�AcdZAbA`Q�A_;dA]oAZ  AY�hAX��AW/AT=qAQG�AN��AM��AJ��AF�jAEXADn�AC��AC"�AB��AA&�A@JA>=qA<jA9��A7\)A4��A29XA/hsA-�-A,A*1A)|�A(�yA'�hA%|�A#/A!+AK�A��AXAA�A&�A(�A�FA�7A��A(�AA�TA��AdZA�A{A�#A|�AoA��A`BA�!A�AE�A�A�AĜA�A�uA5?A&�A1'A ��A E�@�t�@�E�@��@�r�@� �@�;d@��#@���@��D@�\)@���@�r�@�P@�=q@�I�@@�C�@�K�@�\)@�o@�n�@��#@�@�@��^@��@�x�@�hs@�h@���@��@�
=@�33@�
=@��@�/@蛦@�b@�R@�@��@��@�o@�;d@�p�@���@�;d@�M�@�x�@���@�-@�%@�t�@��@�p�@�V@�Ĝ@؋D@؃@ؓu@�j@�b@�1@�|�@�@��@���@���@�C�@ҏ\@�V@�v�@ҏ\@҇+@�ff@�?}@�Z@Ͼw@��#@�Q�@�ƨ@���@���@�S�@�t�@���@�=q@ɉ7@�Q�@ǥ�@�S�@�
=@�v�@š�@�r�@�9X@��@�33@���@�J@��@�G�@��u@�(�@��w@�;d@��H@�^5@��@��@��/@��9@���@��F@���@�33@��y@���@�^5@��^@���@�?}@�G�@���@��@�1'@�|�@�33@���@�~�@���@���@�^5@��@�{@��@��j@��@�r�@�A�@�(�@�9X@�Q�@�9X@���@�\)@��@�n�@�-@�@��@���@�`B@�7L@���@�j@���@��@�+@���@�v�@�-@��7@�x�@�V@�z�@�A�@��;@��w@��P@�l�@�@���@�^5@�@��T@�x�@�O�@��@�%@���@�j@�  @���@��R@�@��^@��@�7L@�Ĝ@���@�j@�I�@� �@��@�;d@�33@�33@�"�@�o@�@��H@��@���@���@�J@��-@��h@�x�@�?}@�/@���@��j@���@�I�@�9X@�  @���@�C�@��H@�E�@���@��@�O�@��@��@���@��/@��j@���@�Z@�1@���@�\)@�o@���@���@�E�@�@�V@���@�A�@�  @�ƨ@�l�@��H@�^5@��^@�x�@�O�@�7L@��/@�z�@�A�@� �@�  @��F@��@�"�@�@��!@�~�@�^5@�5?@���@�G�@�V@���@��u@�1'@���@���@�+@���@�ff@�J@��T@��#@��^@���@�x�@�G�@�&�@��`@��@�bN@�1'@��m@���@��P@��@��@�|�@�t�@��@���@�=q@��@���@�G�@��`@��9@���@��u@��@�r�@�I�@�1'@�(�@�b@���@�C�@��@���@�ff@��@���@���@��@���@��@�z�@�bN@�I�@� �@�  @��F@�dZ@�33@�"�@��@���@�ff@�$�@�J@���@��-@��@�`B@�&�@��@�A�@l�@~��@~ȴ@~��@~ff@~E�@~5?@}�T@}�h@}O�@}/@|��@|�D@{��@{�@{C�@zM�@y��@yX@y7L@x��@xr�@x1'@w�w@w;d@vv�@u��@u/@t��@t�@t�j@t�@t�D@t(�@s��@s�@s33@r��@r�@q��@q��@q�7@qX@qG�@q7L@q%@pQ�@o�@n��@n�R@n{@m@m��@mO�@m/@m/@mV@l�D@l�@k�m@k��@kdZ@j�\@i��@i��@iG�@h��@h�@hQ�@g�;@g�@gl�@f�y@f�R@f��@fv�@f{@e�@eO�@d��@d�D@d�@c��@cdZ@cC�@b�H@bn�@a��@a�7@`��@`��@`A�@`  @_\)@^�R@^�+@]��@]?}@\�@\�@\9X@[�m@[��@[o@ZM�@Yhs@X��@Xr�@W��@W
=@V��@V$�@U��@UO�@T�@T�@S�m@S�@S"�@S"�@So@R�@R��@R��@R��@R^5@R-@RJ@Q��@Q��@Q�@P��@P1'@Nȴ@N@M�-@M/@LZ@L�@L1@K��@K�m@K�
@K��@K33@J��@I��@H��@H��@H�9@H�u@HbN@H1'@H  @G�w@G\)@G+@F��@F�R@F�+@Fff@FV@F$�@E��@E�h@E`B@D��@DZ@Cƨ@C��@C�@CS�@C"�@B��@B^5@B-@A��@A�@A�#@A�#@A��@A��@AG�@@��@?�w@?l�@?K�@?�@>�@>E�@>$�@>@=�T@=��@=p�@<�/@<I�@<9X@;�
@;dZ@;"�@:��@:~�@:=q@9�@9��@97L@8��@8�u@8Q�@8 �@7�;@7�w@7��@7K�@6�y@6v�@6ff@6$�@5�@5�h@5�@5�@5�@5�@5V@4��@4��@4�@4��@4z�@4�@3ƨ@3��@3C�@2��@2��@2~�@2n�@2M�@1��@1�#@1��@1x�@1X@1X@17L@0�`@0�9@0��@0A�@/�@/l�@/�@.�y@.�R@.�+@.v�@.ff@.E�@.$�@.@-��@-��@-`B@-/@,��@,��@,�j@,�@,I�@+�m@+��@+S�@+o@*�@*�H@*��@*=q@)��@)X@)G�@)&�@(Ĝ@(r�@'�;@'l�@'K�@'K�@'K�@&��@&�+@&E�@&5?@&5?@&$�@&{@%�T@%�@%/@%V@$��@$�j@$��@$Z@$9X@$9X@$(�@$1@#ƨ@#S�@#"�@"�@"��@"~�@"=q@"-@!�#@!�^@!�7@!&�@ �9@ Q�@�@�@�P@\)@
=@�+@E�@@�-@O�@�@��@�@�/@��@�@��@�m@ƨ@�F@t�@33@�!@~�@^5@=q@J@�@��@��@X@&�@�@��@Ĝ@bN@1'@ �@  @��@K�@��@��@ff@V@$�@�-@`B@�@�/@Z@9X@�@�@C�@33@�H@�!@��@^5@-@��@�#@��@x�@&�@�`@��@Ĝ@��@bN@�@�P@+@�y@�@�R@��@v�@$�@@�h@p�@O�@/@��@�j@z�@I�@1@�
@�
@�F@�@S�@33@
�@
��@
^5@
�@	�@	�^@	�7@	X@	�@��@��@�u@r�@Q�@1'@b@��@��@�P@l�@K�@��@�@ȴ@�R@��@ff@5?@{@�@�T@�-@�@p�@`B@`B@?}@/@�@��@�/@��@��A�ĜA�ƨA�ĜA�A�ƨA�ƨA�ĜA���A�ȴA�ƨA���A�ƨA���A�ƨA�A���A�ĜA���A���A�ĜA۾wA�ĜA�A���A�ĜA���A���A�ĜA�ȴA���A�ȴA�ȴA���A���A���A���A���A���A���A�ȴA���A���A���A��
A���A��
A���A���A���A���A���A��
A���A���A��A���A���A��
A���A��A���A���A��A���A���A��A��
A��
A��#A���A��A��
A���A��
A���A��
A��
A���A���A���A���A���A���A��
A���A��
A��A���A��#A��
A��
A��#A��
A��A��HA��#A��;A��#A��#A��HA��/A��#A��;A��/A��/A��;A��#A��;A��A��#A��TA��;A��;A��TA��HA��;A��TA��`A��;A��HA��TA��;A��TA��yA��yA��A��A��mA��A��yA��TA��A��yA��mA��A��A��A��yA��mA��TA��A��yA��yA���A��A��A���A���A��A��A���A���A��A���A���A���A���A���A��A���A���A��A���A���A���A���A���A���A���A��A���A���A���A���A���A��A���A���A��A���A��A��A���A��A��A���A��A��A��A��mA��A��yA���A��
A��
A�ƨA���A�Aۺ^A۲-A۰!A۬A۬AۮAۣ�Aۗ�Aۛ�Aۙ�Aۗ�Aۛ�AۓuA�x�A�dZA�z�A�\)A�`BA�^5A�XA�VA�E�A�-A�-A�33A�33A�-A�1'A�(�A��A�(�A�%A���A���A���A��HA���A���A�ȴAڴ9AڬAڛ�Aڛ�Aڗ�AڅA�`BA�?}A��A�{A�bA�bA�A��A٩�A�t�A�Q�A�C�A�A�A�=qA�7LA�&�A� �A���A���Aء�A�/A�%A��A��A��A��A��`A��TA��TA��#A��A���A���A׼jAבhA�`BA֮A�$�A��A�x�A�5?A�5?A�1'A�/A�33A�(�A�Aԉ7A�&�A���Aӛ�A�|�A�G�A���A��A��/A���AҸRAҧ�A�hsA�5?A�JAѲ-A�1AЩ�A�n�A�(�A�oA��A�p�A�oAκ^A�\)A�%AͼjA͍PA�l�A�\)A�S�A�Q�A�C�A��A�ȴA�hsA�JA˸RA�n�A��A�A�ZA�1A�ĜA�v�A�"�A��mA�ƨAȰ!Aȏ\A�K�A��A�A��A���A��A��A��TA��
A���AǼjAǃA�dZA�;dA�bA���A��AƉ7A�VA�A�A�I�A�7LA�VA��A��HAŏ\A���Aě�AāA�ffA�bNA�`BA�^5A�ZA�ZA�XA�G�A�9XA�/A�$�A�oA���A��A��yA��#A�ĜAç�AËDA�ffA�M�A�;dA�$�A�%A��;A�ƨA¬AA�`BA�-A�ȴA��+A�O�A�5?A� �A���A��TA���A��9A��9A��-A���A��DA��A�M�A��A���A�\)A��A���A�z�A�\)A�K�A�;dA��A�A��A���A�ĜA��^A���A���A���A���A���A���A���A���A���A���A���A���A��hA��DA��A�Q�A�$�A�VA�JA�A���A���A��A��HA��HA��#A���A���A���A�ȴA���A�ĜA��wA��^A��RA��A���A�z�A�;dA��^A�ĜA�oA���A���A��-A�I�A�&�A�oA���A���A��A�dZA�G�A�&�A���A�A���A��A�bNA�;dA�{A�  A��A��/A��FA��9A��uA�~�A�?}A���A�|�A�/A�A��
A���A�ĜA��A���A��\A��+A�z�A�t�A�^5A�1'A�/A�$�A��A�A��;A���A�r�A�`BA�M�A�=qA�5?A�33A�+A��A�JA�%A���A��;A��^A�|�A��A�(�A��A���A���A���A���A��PA�&�A��
A��wA��-A���A�z�A�VA�JA�  A��A�ĜA���A���A��7A�v�A�ffA�M�A��A���A��
A���A�I�A�bA��/A���A�G�A�"�A��-A���A�+A��A��A���A�ȴA��wA��^A��FA��-A���A���A���A�v�A�jA�VA�K�A�C�A�;dA�9XA�1'A�/A��A���A���A���A���A��A��mA��#A���A��RA���A���A��DA�E�A�"�A���A���A�^5A�1A��A��wA�~�A�%A��`A���A���A���A���A��+A�~�A�~�A�~�A�z�A�v�A�p�A�\)A�A�A�33A�+A�(�A�&�A��A�%A��A��wA���A�x�A�?}A���A��jA���A�v�A�+A��HA���A�XA��A��`A�ĜA�9XA�33A�(�A�JA�A���A��A��A��A�ĜA��A�ffA�$�A�A�A���A��yA���A��RA��\A�bNA�+A��A�$�A�ȴA���A�|�A�\)A�9XA�{A��HA��FA��\A�5?A�A��RA�^5A�%A��hA�bNA�K�A�7LA�"�A�oA�%A���A��`A���A���A�A��-A���A���A���A�jA�33A��HA���A�Q�A��TA���A��7A�r�A�ZA�G�A�?}A�33A�&�A�$�A��A�JA���A��yA��HA�ĜA���A���A�|�A�VA�33A��A�oA��A���A�A��!A��\A�r�A�;dA�JA��A��
A���A���A��7A�\)A��A��-A���A��\A��7A�p�A�G�A�oA��TA�ĜA��A��hA�p�A�33A��A��jA��\A�jA�O�A�9XA�"�A�oA��`A��A�z�A�S�A�-A��A��hA�bNA�9XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                               A�ĜA�ƨA�ȴA�ȴA�A�A���A�A���A�ĜA���A���A���A���A���A���A��
A��
A���A���A��
A��A��/A��/A��/A��HA��TA��A��A��mA��A���A���A���A���A���A���A��yA���A۩�AۓuA�dZA�7LA��A���A�|�A���A�C�Aؗ�A��yA׺^A��;A���AӁAҼjA�XAρAͲ-A���A�~�A�bNAǁA���A�^5A��/A��
A��A���A���A���A�;dA���A��#A�p�A�A�A�A�K�A�Q�A�  A�  A��-A�;dA�;dA��DA�"�A�ĜA�dZA��!A�XA��A��HA�33A�^5A�A�A�$�A�"�A���A�C�A�r�A��A�$�A� �A��jA�n�A�A�C�A��RA�$�A�G�A�9XA��A�$�A���Az�9Aw;dAs��An��Am�hAl=qAk�-Aj��Ai
=Ae�AcdZAbA`Q�A_;dA]oAZ  AY�hAX��AW/AT=qAQG�AN��AM��AJ��AF�jAEXADn�AC��AC"�AB��AA&�A@JA>=qA<jA9��A7\)A4��A29XA/hsA-�-A,A*1A)|�A(�yA'�hA%|�A#/A!+AK�A��AXAA�A&�A(�A�FA�7A��A(�AA�TA��AdZA�A{A�#A|�AoA��A`BA�!A�AE�A�A�AĜA�A�uA5?A&�A1'A ��A E�@�t�@�E�@��@�r�@� �@�;d@��#@���@��D@�\)@���@�r�@�P@�=q@�I�@@�C�@�K�@�\)@�o@�n�@��#@�@�@��^@��@�x�@�hs@�h@���@��@�
=@�33@�
=@��@�/@蛦@�b@�R@�@��@��@�o@�;d@�p�@���@�;d@�M�@�x�@���@�-@�%@�t�@��@�p�@�V@�Ĝ@؋D@؃@ؓu@�j@�b@�1@�|�@�@��@���@���@�C�@ҏ\@�V@�v�@ҏ\@҇+@�ff@�?}@�Z@Ͼw@��#@�Q�@�ƨ@���@���@�S�@�t�@���@�=q@ɉ7@�Q�@ǥ�@�S�@�
=@�v�@š�@�r�@�9X@��@�33@���@�J@��@�G�@��u@�(�@��w@�;d@��H@�^5@��@��@��/@��9@���@��F@���@�33@��y@���@�^5@��^@���@�?}@�G�@���@��@�1'@�|�@�33@���@�~�@���@���@�^5@��@�{@��@��j@��@�r�@�A�@�(�@�9X@�Q�@�9X@���@�\)@��@�n�@�-@�@��@���@�`B@�7L@���@�j@���@��@�+@���@�v�@�-@��7@�x�@�V@�z�@�A�@��;@��w@��P@�l�@�@���@�^5@�@��T@�x�@�O�@��@�%@���@�j@�  @���@��R@�@��^@��@�7L@�Ĝ@���@�j@�I�@� �@��@�;d@�33@�33@�"�@�o@�@��H@��@���@���@�J@��-@��h@�x�@�?}@�/@���@��j@���@�I�@�9X@�  @���@�C�@��H@�E�@���@��@�O�@��@��@���@��/@��j@���@�Z@�1@���@�\)@�o@���@���@�E�@�@�V@���@�A�@�  @�ƨ@�l�@��H@�^5@��^@�x�@�O�@�7L@��/@�z�@�A�@� �@�  @��F@��@�"�@�@��!@�~�@�^5@�5?@���@�G�@�V@���@��u@�1'@���@���@�+@���@�ff@�J@��T@��#@��^@���@�x�@�G�@�&�@��`@��@�bN@�1'@��m@���@��P@��@��@�|�@�t�@��@���@�=q@��@���@�G�@��`@��9@���@��u@��@�r�@�I�@�1'@�(�@�b@���@�C�@��@���@�ff@��@���@���@��@���@��@�z�@�bN@�I�@� �@�  @��F@�dZ@�33@�"�@��@���@�ff@�$�@�J@���@��-@��@�`B@�&�@��@�A�@l�@~��@~ȴ@~��@~ff@~E�@~5?@}�T@}�h@}O�@}/@|��@|�D@{��@{�@{C�@zM�@y��@yX@y7L@x��@xr�@x1'@w�w@w;d@vv�@u��@u/@t��@t�@t�j@t�@t�D@t(�@s��@s�@s33@r��@r�@q��@q��@q�7@qX@qG�@q7L@q%@pQ�@o�@n��@n�R@n{@m@m��@mO�@m/@m/@mV@l�D@l�@k�m@k��@kdZ@j�\@i��@i��@iG�@h��@h�@hQ�@g�;@g�@gl�@f�y@f�R@f��@fv�@f{@e�@eO�@d��@d�D@d�@c��@cdZ@cC�@b�H@bn�@a��@a�7@`��@`��@`A�@`  @_\)@^�R@^�+@]��@]?}@\�@\�@\9X@[�m@[��@[o@ZM�@Yhs@X��@Xr�@W��@W
=@V��@V$�@U��@UO�@T�@T�@S�m@S�@S"�@S"�@So@R�@R��@R��@R��@R^5@R-@RJ@Q��@Q��@Q�@P��@P1'@Nȴ@N@M�-@M/@LZ@L�@L1@K��@K�m@K�
@K��@K33@J��@I��@H��@H��@H�9@H�u@HbN@H1'@H  @G�w@G\)@G+@F��@F�R@F�+@Fff@FV@F$�@E��@E�h@E`B@D��@DZ@Cƨ@C��@C�@CS�@C"�@B��@B^5@B-@A��@A�@A�#@A�#@A��@A��@AG�@@��@?�w@?l�@?K�@?�@>�@>E�@>$�@>@=�T@=��@=p�@<�/@<I�@<9X@;�
@;dZ@;"�@:��@:~�@:=q@9�@9��@97L@8��@8�u@8Q�@8 �@7�;@7�w@7��@7K�@6�y@6v�@6ff@6$�@5�@5�h@5�@5�@5�@5�@5V@4��@4��@4�@4��@4z�@4�@3ƨ@3��@3C�@2��@2��@2~�@2n�@2M�@1��@1�#@1��@1x�@1X@1X@17L@0�`@0�9@0��@0A�@/�@/l�@/�@.�y@.�R@.�+@.v�@.ff@.E�@.$�@.@-��@-��@-`B@-/@,��@,��@,�j@,�@,I�@+�m@+��@+S�@+o@*�@*�H@*��@*=q@)��@)X@)G�@)&�@(Ĝ@(r�@'�;@'l�@'K�@'K�@'K�@&��@&�+@&E�@&5?@&5?@&$�@&{@%�T@%�@%/@%V@$��@$�j@$��@$Z@$9X@$9X@$(�@$1@#ƨ@#S�@#"�@"�@"��@"~�@"=q@"-@!�#@!�^@!�7@!&�@ �9@ Q�@�@�@�P@\)@
=@�+@E�@@�-@O�@�@��@�@�/@��@�@��@�m@ƨ@�F@t�@33@�!@~�@^5@=q@J@�@��@��@X@&�@�@��@Ĝ@bN@1'@ �@  @��@K�@��@��@ff@V@$�@�-@`B@�@�/@Z@9X@�@�@C�@33@�H@�!@��@^5@-@��@�#@��@x�@&�@�`@��@Ĝ@��@bN@�@�P@+@�y@�@�R@��@v�@$�@@�h@p�@O�@/@��@�j@z�@I�@1@�
@�
@�F@�@S�@33@
�@
��@
^5@
�@	�@	�^@	�7@	X@	�@��@��@�u@r�@Q�@1'@b@��@��@�P@l�@K�@��@�@ȴ@�R@��@ff@5?@{@�@�T@�-@�@p�@`B@`B@?}@/@�@��@�/@��@��A�ĜA�ƨA�ĜA�A�ƨA�ƨA�ĜA���A�ȴA�ƨA���A�ƨA���A�ƨA�A���A�ĜA���A���A�ĜA۾wA�ĜA�A���A�ĜA���A���A�ĜA�ȴA���A�ȴA�ȴA���A���A���A���A���A���A���A�ȴA���A���A���A��
A���A��
A���A���A���A���A���A��
A���A���A��A���A���A��
A���A��A���A���A��A���A���A��A��
A��
A��#A���A��A��
A���A��
A���A��
A��
A���A���A���A���A���A���A��
A���A��
A��A���A��#A��
A��
A��#A��
A��A��HA��#A��;A��#A��#A��HA��/A��#A��;A��/A��/A��;A��#A��;A��A��#A��TA��;A��;A��TA��HA��;A��TA��`A��;A��HA��TA��;A��TA��yA��yA��A��A��mA��A��yA��TA��A��yA��mA��A��A��A��yA��mA��TA��A��yA��yA���A��A��A���A���A��A��A���A���A��A���A���A���A���A���A��A���A���A��A���A���A���A���A���A���A���A��A���A���A���A���A���A��A���A���A��A���A��A��A���A��A��A���A��A��A��A��mA��A��yA���A��
A��
A�ƨA���A�Aۺ^A۲-A۰!A۬A۬AۮAۣ�Aۗ�Aۛ�Aۙ�Aۗ�Aۛ�AۓuA�x�A�dZA�z�A�\)A�`BA�^5A�XA�VA�E�A�-A�-A�33A�33A�-A�1'A�(�A��A�(�A�%A���A���A���A��HA���A���A�ȴAڴ9AڬAڛ�Aڛ�Aڗ�AڅA�`BA�?}A��A�{A�bA�bA�A��A٩�A�t�A�Q�A�C�A�A�A�=qA�7LA�&�A� �A���A���Aء�A�/A�%A��A��A��A��A��`A��TA��TA��#A��A���A���A׼jAבhA�`BA֮A�$�A��A�x�A�5?A�5?A�1'A�/A�33A�(�A�Aԉ7A�&�A���Aӛ�A�|�A�G�A���A��A��/A���AҸRAҧ�A�hsA�5?A�JAѲ-A�1AЩ�A�n�A�(�A�oA��A�p�A�oAκ^A�\)A�%AͼjA͍PA�l�A�\)A�S�A�Q�A�C�A��A�ȴA�hsA�JA˸RA�n�A��A�A�ZA�1A�ĜA�v�A�"�A��mA�ƨAȰ!Aȏ\A�K�A��A�A��A���A��A��A��TA��
A���AǼjAǃA�dZA�;dA�bA���A��AƉ7A�VA�A�A�I�A�7LA�VA��A��HAŏ\A���Aě�AāA�ffA�bNA�`BA�^5A�ZA�ZA�XA�G�A�9XA�/A�$�A�oA���A��A��yA��#A�ĜAç�AËDA�ffA�M�A�;dA�$�A�%A��;A�ƨA¬AA�`BA�-A�ȴA��+A�O�A�5?A� �A���A��TA���A��9A��9A��-A���A��DA��A�M�A��A���A�\)A��A���A�z�A�\)A�K�A�;dA��A�A��A���A�ĜA��^A���A���A���A���A���A���A���A���A���A���A���A���A��hA��DA��A�Q�A�$�A�VA�JA�A���A���A��A��HA��HA��#A���A���A���A�ȴA���A�ĜA��wA��^A��RA��A���A�z�A�;dA��^A�ĜA�oA���A���A��-A�I�A�&�A�oA���A���A��A�dZA�G�A�&�A���A�A���A��A�bNA�;dA�{A�  A��A��/A��FA��9A��uA�~�A�?}A���A�|�A�/A�A��
A���A�ĜA��A���A��\A��+A�z�A�t�A�^5A�1'A�/A�$�A��A�A��;A���A�r�A�`BA�M�A�=qA�5?A�33A�+A��A�JA�%A���A��;A��^A�|�A��A�(�A��A���A���A���A���A��PA�&�A��
A��wA��-A���A�z�A�VA�JA�  A��A�ĜA���A���A��7A�v�A�ffA�M�A��A���A��
A���A�I�A�bA��/A���A�G�A�"�A��-A���A�+A��A��A���A�ȴA��wA��^A��FA��-A���A���A���A�v�A�jA�VA�K�A�C�A�;dA�9XA�1'A�/A��A���A���A���A���A��A��mA��#A���A��RA���A���A��DA�E�A�"�A���A���A�^5A�1A��A��wA�~�A�%A��`A���A���A���A���A��+A�~�A�~�A�~�A�z�A�v�A�p�A�\)A�A�A�33A�+A�(�A�&�A��A�%A��A��wA���A�x�A�?}A���A��jA���A�v�A�+A��HA���A�XA��A��`A�ĜA�9XA�33A�(�A�JA�A���A��A��A��A�ĜA��A�ffA�$�A�A�A���A��yA���A��RA��\A�bNA�+A��A�$�A�ȴA���A�|�A�\)A�9XA�{A��HA��FA��\A�5?A�A��RA�^5A�%A��hA�bNA�K�A�7LA�"�A�oA�%A���A��`A���A���A�A��-A���A���A���A�jA�33A��HA���A�Q�A��TA���A��7A�r�A�ZA�G�A�?}A�33A�&�A�$�A��A�JA���A��yA��HA�ĜA���A���A�|�A�VA�33A��A�oA��A���A�A��!A��\A�r�A�;dA�JA��A��
A���A���A��7A�\)A��A��-A���A��\A��7A�p�A�G�A�oA��TA�ĜA��A��hA�p�A�33A��A��jA��\A�jA�O�A�9XA�"�A�oA��`A��A�z�A�S�A�-A��A��hA�bNA�9XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B iB
��B iB iB iB  B �B 4B �B  B
�cB iB iB;B iB;B;B;B �B �B �BoBuB�B�BB{BYB�B�BfB	7B	7B	7B
	B�B�B�B�B@B:B�B	lB�B
��B
�B�Bk�B�FB�DB�B �B6�BW?B^�Bs�B�;B|PBuZBc BFB=<BO�B��B��B�B�$B�B��B�7B�7B��B}�Be�Bc�BbNBW�BU�BW
BR�BPBL�B33B�B_B�B
=B��B�B�QB�2B��B�YB�JB^jB@OB*eB$tB�BbB�B
�.B
�B
�B
�}B
�HB
��B
��B
|PB
oiB
^B
P}B
8�B

�B	�B	�>B	��B	��B	�?B	��B	��B	��B	��B	��B	~�B	u�B	kB	h�B	T�B	N<B	M6B	CaB	@�B	0UB	&�B	"hB	�B	hB	�B	�B�(B�B��B�%B��B�
B� B��B�
B̘B�B�-B��B��B�0B�XB�zB��B��B��B��B��B��B��B�qB��B�VB�VB��B��B�CB��B�BB��B��B�B��B��B�3B��B�B�B��B��B�B�wB��B�B}�B~�B��B��BzxByrBv�Bx8By>B|�B�B��B�+B�JB��B��B��B��B�xB��B�$B��B�[B�hB�nB��B��B��B��B��B�9B�6B�?B��B��B��B�B��B��B		�B	�B	�B	�B	B	B	�B	�B	�B	xB	&�B	,�B	.�B	-CB	0�B	+kB	1'B	0�B	1�B	0UB	/�B	0�B	3�B	3hB	2�B	4B	6�B	;0B	>BB	?�B	B[B	G�B	I�B	K�B	LdB	H�B	DgB	D�B	FB	JXB	K�B	K�B	L�B	NB	J�B	H�B	EmB	D�B	G�B	HB	L�B	S&B	Z�B	a�B	c B	a|B	d�B	f�B	jB	k�B	oiB	t�B	u%B	tB	t�B	u�B	t�B	v�B	x8B	~�B	�B	��B	��B	�B	�YB	�1B	�lB	��B	��B	��B	�.B	��B	��B	�B	��B	��B	�MB	��B	��B	��B	��B	�*B	��B	�B	��B	��B	�tB	��B	��B	��B	�^B	��B	��B	�qB	��B	��B	��B	�B	��B	�[B	�9B	�zB	�B	��B	ɆB	�B	ȀB	��B	��B	�pB	��B	�B	�HB	уB	�&B	��B	ԕB	՛B	՛B	�9B	�gB	�gB	֡B	�9B	�B	�?B	רB	�B	�B	ٴB	یB	��B	�jB	��B	�B	�vB	�B	�B	��B	�NB	�HB	�B	��B	��B	��B	�fB	�>B	�KB	�B	�QB	�QB	�B	�B	�)B	�)B	��B	�)B	�]B	�]B	�)B	�)B	�)B	��B	�/B	�]B	�]B	��B	��B	��B	�cB	� B	�B	�B	� B	�B	�B	��B	��B	�B	�TB	��B	�%B	��B	�ZB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�>B	��B	��B	��B	��B	��B	�.B	�cB	��B
 �B
;B
uB
uB
�B
uB
�B
{B
B
MB
�B
B
�B
�B
fB
fB
	7B
�B
�B
	�B

rB

rB

rB

rB

�B
�B
�B
�B
�B
B
"B
�B
�B
�B
.B
 B
hB
�B
:B
B
uB
FB
{B
�B
MB
MB
MB
MB
B
�B
�B
�B
�B
�B
�B
�B
1B
eB
eB
�B
�B
�B
B
B
B
B
kB
�B
�B
CB
CB
xB
�B
�B
�B
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
!B
VB
 \B
 �B
 �B
!bB
!bB
!�B
!bB
!�B
#B
#nB
$�B
%B
%FB
%�B
&B
&B
&LB
&�B
'�B
'�B
($B
(XB
(�B
)�B
(�B
)*B
(�B
(�B
(�B
(�B
)_B
)*B
)_B
)_B
*0B
*eB
+B
*�B
+6B
+B
+B
+B
+B
+6B
+�B
,B
+�B
,=B
,�B
-B
,�B
-CB
-CB
-CB
-CB
-wB
.B
.�B
.�B
/B
/�B
/�B
/�B
0UB
0UB
0UB
0UB
1'B
1'B
1[B
1�B
1�B
2�B
2�B
2�B
3�B
49B
5B
5B
5tB
5tB
6B
6�B
6�B
6�B
7B
7�B
8RB
8RB
8RB
8RB
8�B
9XB
9XB
9�B
9�B
9�B
:�B
:�B
;0B
;dB
;�B
;�B
<jB
<�B
<�B
=�B
>B
>wB
>�B
?B
?�B
?�B
@�B
A B
A�B
A�B
A�B
A�B
B'B
B'B
B�B
C-B
C-B
CaB
D3B
D3B
DgB
D�B
DgB
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
EB
EmB
E�B
FB
GEB
GEB
G�B
HB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IRB
IB
J�B
K)B
K)B
K)B
K^B
K�B
K�B
K�B
K�B
LdB
LdB
L�B
MB
MB
MB
MB
M6B
MjB
M�B
M�B
N<B
N�B
OBB
OBB
OBB
OvB
O�B
PHB
P}B
P}B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
S[B
S�B
S�B
S�B
S�B
S�B
TaB
T�B
UgB
U2B
U�B
VB
V9B
V�B
V�B
W
B
W?B
WsB
W�B
XEB
XEB
X�B
X�B
X�B
X�B
YB
YKB
Y�B
ZB
ZB
ZQB
Z�B
[#B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\]B
\�B
\�B
]dB
]dB
]dB
]�B
]dB
^B
^B
^5B
^5B
^jB
^5B
^5B
^�B
^�B
^�B
_B
_;B
`B
`B
`BB
`vB
`vB
`vB
`�B
`�B
`�B
`�B
`�B
`�B
aHB
a|B
a|B
a�B
a|B
a|B
bB
bB
b�B
b�B
b�B
c B
b�B
b�B
c�B
d&B
dZB
d&B
dZB
d�B
d�B
e�B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
gB
gB
gB
gB
g�B
g�B
g�B
h>B
h
B
h>B
hsB
h�B
hsB
hsB
h�B
iB
iDB
iyB
iyB
jB
jKB
jB
jB
j�B
j�B
j�B
kB
k�B
l"B
lWB
l�B
l�B
l�B
l�B
m]B
m�B
m�B
n/B
n�B
n�B
n�B
o B
n�B
oiB
o�B
oiB
o�B
o�B
pB
poB
p�B
qvB
qAB
qvB
q�B
q�B
q�B
rB
r|B
r|B
r�B
r|B
r|B
r�B
r�B
sB
r�B
sB
s�B
s�B
tB
t�B
t�B
t�B
t�B
uZB
u�B
u�B
v+B
v�B
v�B
v�B
w2B
w2B
wfB
w�B
xB
w�B
x8B
xlB
x�B
x�B
y	B
y	B
y�B
y�B
y�B
y�B
zB
zDB
z�B
{JB
|B
|B
{�B
|PB
|PB
|PB
}"B
}"B
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
~�B
.B
.B
cB
�B
�B
� B
�iB
��B
�B
��B
�oB
�oB
��B
��B
�AB
�B
�uB
�uB
��B
��B
�GB
�GB
�GB
�GB
��B
�B
�B
�MB
�MB
��B
��B
��B
�B
�SB
��B
��B
�%B
�%B
�%B
�%B
�YB
�%B
�YB
��B
��B
��B
��B�B
�cB iB;B
��B 4B 4B
��B 4BoB
�cBB�B
��B 4BB
��B �BB
��B�B
��B 4BB
�cB;B  B
��B�B
��B  B �B
�.B �B �B
��BB
��B
��BoB
��B
�cBoB iBuB
��BoBB
�.B iBB
��B �B�B 4BAB �B �BuB  BBoB 4B�BuB 4B�BAB iB�BoBB�B
�cBB
��B 4B�B iB�B
��B
��BoB
��B;BB �BAB iB�BB iBuB�B iBGBoB�B�BoBBGBB�BB �B{B �B�B �BB�B�B�B�B�B�BBMB{BABB�BuB�B�B�B	lB�B�B%BBGB�B�B�B1B�BABMBBMB�B�B�B	�B	�B	B
�B	�B1B�B	�B�BfB	�B	�B�B
rB	B	7B
	BfB	�B	lBfB
=B	�B	7B�B
	B	lB	�B1BxB�B�B�B�B(B.B B�BbB�B�B.B�B�BoB�B4BSBB�BFBB�B�B�B:B�B�B BB�B4B�BB�B�B�BDB�B7B
�B�BPB~B�B�B1B1B�B
	B�BfB�B �B:BuBoB �B
�B 4B
��B
�]B
��B
��B
��B
��B
��B
��B 4B
��B{B�B
rBxB�B+kB6�BM6B[#Bc�Bl�BqABzB}�B��B��B�uB�tB�dB�2B�B�KB�sB�B�B��B�>B�B�B�B�sB��B�B�BIBB�B2-B/OB,=B.B.�B.�B1[BB'BCaBP�BS�BS�BUgBYB_pB]�B\�B^B]dB_�Bd�Bf�BhsBzDB�Bw�BzDB}�BxBy�B�B�GB�YB�uB��B�BzDB}"Bs�BtBpoBr�BxBx�BxBq�BncBi�BiDBkQBe,BX�BYBT�BS�BM�BFBEmBF�BN�BD3BB�B@�B>�B>BB>�B?}B?}B;�B>wBB'B<B;�B<B8RB9�BB�B=qB<jB>�BIRBIRBH�B^Bn�Bw2Be�BiDBo5Bo Bu�B�+B��B��B�B�B��B��B��B�nB��B��B�$B�B�B��B�UB�?B��B�B�XB�B��B�qB�B�-B�UB�3B�B�<B�^B�B��B�$B�B�FB�?B��B�'B�$B�'B��B��B�B�9B�!B��B��B��B�4B�.B�VB��B�B��B��B�=B��B��B�B�fB�	B��B�fB��B�rB�lB��B��B��B�rB�=B�	B��B��B��B��B��B��B�%B��B�{B�B�AB��B�B�B~�B� B|�B|�B}�B{Bz�BzBy�B}�B��B�BxBq�B��B��Bh>BffBc BiyBg8Be�Ba�Bd�Bd�Bf2BgBe�BdZBe�Be�Bf�Ba|Bb�Bc�Bc�B`BgBa�Bk�Bp�BiyBb�B`vB`�BZ�BY�B\�B[�BW�BXEBYBV�BZBZ�BVBUgBU2BXBZQB\)BXEBV9BUgBT�BS�BR�BRTBS�BR�BO�BQBR�BT�BS�Bi�Bd�BQ�BP}BQNBM6BMjBOBe�BS�BQ�BP�BP�BT�BV�BVBN�BPBTaBN�BMjBPBNBMjBM�BPHBMjBL0BQ�BPBLdBI�BN�BI�BC-BN�BS[B=�B.�B'�B!bB"hB#B#:B �B!bB �B!�B�B"hB�B�BB7B7BBYB�B�BqB�BuBFB�B�B�BhBSBbB(B�B+B	�B B�B1B.B��B�B�B �B��B�VB�B��B�B�2B�B�B�AB��B�B�B�B�B�B�B��B�B�B�]B�]B�)B�B��B�sB�5B�5B�yBݘBٴB�EB˒B��B��B�jB�B��B�$B�XB��B��B��B��B�OB��B�\B�B�B��B�\B��B�(B�.B�(B��B�{B��B�4B��B��Bn�BjBc BbNB_B\�BZBS�BS[BO�BGzBHKBEmBJ�B>B0�B0UB.�B.�B+�B)�B)�B*�B)�B&B%�B&LB$B!�B \B&�B �B)�B&LB,qB&�B \B~B�B�B$B�B�B�B�B�B�B{B�B\B�B�B	�B�BDBfBAB_B
�B�BBuBSBB�B 4B
�B
�DB
�B
�8B
��B
��BoB
�5B
�5B
�"B
�B
�)B
�B
�)B
�mB
�&B
�B
��B
�B
�8B
��B
یB
�yB
��B
�,B
� B
ΥB
�B
��B
��B
̘B
ʌB
ƨB
��B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                               B
��B
�B
��B
��B
��B
�PB
��B
��B
��B
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
�"B
�"B
��B
��B
��B
��B
�bB
��B�B�BB�B�B�B�BYB
=BIB�B�B�B�B	B�BB
�B
��B�Bg�B��B�B�
BB2�BS�B[#BpB}�Bx�Bq�B_pBB[B9�BL/B.B�B�XB�tB�dB��B��B��B|�BzDBbB`B^�BT,BR BSZBN�BLdBIB/�BB�B�B�B��B�]B�BтB�B��B��BZ�B<�B&�B �B�B�BMB
�~B
��B
�B
��B
��B
�B
��B
x�B
k�B
ZQB
L�B
5B
+B	��B	�B	�NB	�KB	��B	�<B	��B	�IB	�B	�@B	{JB	rGB	glB	e,B	QB	J�B	I�B	?�B	<�B	,�B	#B	�B	�B	�B	�B�(B�xB�lB��B�uB�B�ZB�pB�>B�ZB��B�gB�}B�<B��B��B��B��B�'B�6B��B��B��B�=B�IB��B�B��B��B��B�6B��B� B��B�0B��B�XB�B�NB��B�HB�jB�jB�?B�9B�[B��B�B�iBzDB{JB~(B|�Bv�Bu�BsBt�Bu�By>B|B��B�{B��B�B�B��B�FB��B�CB�tB�0B��B��B��B�9B��B�B��B�BB��BɆBӏB�B�NB�"B�B��B�	B	%B	�B	�B	�B	hB	\B	'B		B	IB	�B	"�B	)*B	+B	)�B	-BB	'�B	-wB	-B	-�B	,�B	,<B	-B	0 B	/�B	/OB	0UB	2�B	7�B	:�B	<B	>�B	D3B	E�B	G�B	H�B	D�B	@�B	@�B	B[B	F�B	HKB	HB	IB	JWB	GEB	E9B	A�B	@�B	C�B	DgB	IB	OvB	V�B	^B	_pB	]�B	`�B	b�B	f�B	h>B	k�B	qAB	quB	poB	qB	rB	qAB	sMB	t�B	{B	~\B	}"B	}"B	�oB	��B	��B	��B	��B	�=B	�B	�~B	��B	�B	�VB	�'B	��B	��B	�B	��B	�7B	��B	�zB	��B	�XB	�B	� B	��B	�3B	�B	�B	��B	�B	��B	��B	��B	��B	�B	�jB	��B	��B	��B	��B	�mB	�?B	��B	�mB	��B	�?B	�B	��B	�)B	�^B	̘B	��B	�vB	�HB	��B	��B	��B	҉B	ѷB	ѷB	��B	҉B	�`B	ӏB	��B	�`B	��B	�B	��B	�B	ںB	�#B	��B	��B	�cB	�cB	�5B	ޞB	ݘB	�cB	�;B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	��B	�yB	�yB	�DB	�yB	�B	�B	�yB	�yB	�yB	�DB	�B	�B	�B	�B	�JB	�JB	�B	�PB	��B	��B	�PB	��B	��B	�(B	�.B	��B	�B	�B	�uB	��B	�B	�B	��B	�GB	�GB	��B	��B	�B	�B	��B	��B	�+B	�1B	�B	��B	�DB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 iB
 �B
B
oB
@B
�B
�B
�B
�B
B
B
%B
�B
�B
�B
�B
�B
�B
�B
�B
1B
	kB

rB

�B
CB
�B
~B
PB
�B
!B
�B
\B
�B
�B
�B
4B
�B
�B
�B
�B
hB
4B
B
@B
@B
@B
FB
�B
�B
�B
�B
�B
�B
B
RB
RB
RB
RB
�B
�B
*B
�B
�B
�B
0B
�B
7B
B
7B
kB
kB
�B
�B
kB
B
=B
=B
B
qB
�B
�B
B
IB
�B
�B
�B
�B
OB
UB
�B
!-B
!bB
!�B
!�B
"hB
"hB
"�B
#9B
$B
$@B
$tB
$�B
%FB
%�B
%FB
%zB
%B
%FB
%FB
%FB
%�B
%zB
%�B
%�B
&�B
&�B
'RB
'B
'�B
'RB
'RB
'RB
'RB
'�B
($B
(XB
($B
(�B
(�B
)^B
)*B
)�B
)�B
)�B
)�B
)�B
*dB
+6B
+6B
+kB
,B
,<B
,B
,�B
,�B
,�B
,�B
-wB
-wB
-�B
-�B
.B
.�B
/B
/B
/�B
0�B
1[B
1[B
1�B
1�B
2aB
2�B
33B
33B
3gB
4B
4�B
4�B
4�B
4�B
5B
5�B
5�B
5�B
6EB
6EB
6�B
7B
7�B
7�B
7�B
7�B
8�B
9#B
9#B
:)B
:^B
:�B
:�B
;dB
<B
<6B
<�B
=pB
>B
=�B
=�B
=�B
>wB
>wB
>�B
?}B
?}B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A B
A B
A B
A B
AUB
AUB
A�B
A�B
B[B
C�B
C�B
C�B
DgB
EB
EB
EB
EB
E9B
EB
EB
E�B
EmB
GB
GyB
GyB
GyB
G�B
G�B
G�B
G�B
HKB
H�B
H�B
H�B
IQB
IQB
IQB
IQB
I�B
I�B
I�B
I�B
J�B
K)B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
MB
MB
MB
MB
MB
M5B
M5B
NB
OBB
OB
OBB
OBB
O�B
PB
PB
PHB
PHB
PB
P�B
QB
Q�B
Q�B
R B
RTB
R�B
R�B
S&B
SZB
S�B
S�B
T,B
T�B
T�B
T�B
T�B
U2B
U2B
UgB
U�B
V8B
VmB
VmB
V�B
V�B
WsB
XB
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
XEB
XyB
X�B
X�B
YKB
Y�B
Y�B
Y�B
Y�B
Y�B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
Z�B
[#B
[WB
[�B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]/B
]�B
]�B
]�B
^B
]�B
]�B
^iB
^iB
_B
_B
_;B
_pB
_B
_B
`B
`vB
`�B
`vB
`�B
aB
aGB
bB
bNB
bNB
bB
bNB
b�B
cB
cB
cB
cTB
cTB
cTB
cTB
c�B
d%B
c�B
d�B
dZB
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
gB
g8B
gB
glB
g�B
hrB
h�B
iB
iB
iB
iDB
i�B
i�B
jB
jB
j�B
kB
j�B
kPB
kB
k�B
l"B
k�B
k�B
k�B
lWB
l�B
l�B
m�B
m�B
m�B
m�B
n.B
n.B
ncB
n�B
n�B
o B
n�B
n�B
o B
o5B
oiB
o5B
oiB
pB
pB
poB
p�B
p�B
qB
qAB
q�B
rGB
rGB
r{B
sMB
r�B
sB
s�B
s�B
s�B
tB
tSB
tB
t�B
t�B
t�B
t�B
uYB
uYB
u�B
v+B
v+B
v+B
v`B
v�B
w1B
w�B
xlB
xlB
x7B
x�B
x�B
x�B
yrB
yrB
y�B
y�B
zB
zB
zDB
z�B
z�B
{B
{JB
{JB
{B
{~B
{~B
{�B
{�B
|B
|PB
|�B
|�B
}VB
}"B
}�B
}�B
}�B
}�B
~�B
~\B
~�B
~�B
~�B
.B
�B
�B
�B
�B
�4B
�iB
�iB
��B
��B
��B
�B
�:B
�oB
��B
��B
�B
�uB
�uB
�uB
�uB
��B
�uB
��B
��B
��B
�B
��B
�(B
��B
��B
��B
�JB
��B
��B
�B
��B
��B
��B
�VB
��B
��B
��B
�VB
�JB
��B
�VB
�B
��B
��B
��B
�VB
��B
��B
�PB
��B
�(B
�B
�PB
��B
�~B
�"B
��B
��B
�\B
�B
�B
��B
��B
��B
��B
��B
��B
�B
��B
�\B
�~B
��B
�VB
��B
��B
�(B
��B
��B
�"B
��B
��B
�PB
�\B
��B
��B
��B
��B
��B
�(B
��B
��B
��B
��B
�VB
�(B
��B
�\B
�B
��B
�(B
��B
�(B
��B
�B
��B
�JB
��B
�VB
�"B
��B
��B
�(B
�VB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�bB
��B
�VB  B
�bB
�"B
��B
�"B  B
��B
�\B  B 4B
�(B
��B �B
�.B
�\B �B
��B
��B iB 4B
��BB@B�B�BB�BuBoB
��B�BBB�B
�.B
��B �B
�bB �BMB�BB%B�BSB+B�B�B�B%B�B�B%B�B�B�BSB�BYB�B%B�B�B�B�B�B1BYB�B%B�B�BB
�BIBIBxB~BPBCB�BBCB~B!BB�B�B�B�BVB.B�B\B.B�B�B�B.B�BPBbB'B�B'BVBIB.B�B�B 4B�B+B1B	�B�B!B1B�B�B�BYB�B�B
�.B
�"B�B
��B
��B
�"B+B
��B
�B
��B
�B
��B
�B
�%B
��B
��B
��B
��B
��BGB�B�BB'�B33BI�BWsB`Bh�Bm�Bv`BzB�B��B��B��BٴB�B�`B�B��B��B��B�,B�B�fB��B��B��B�>B��B��B�BhB�B.}B+�B(�B*dB+6B+B-�B>wB?�BM5BPHBPHBQ�BUgB[�BY�BYBZQBY�B[�BaGBb�Bd�Bv�B}VBtBv�BzDBtSBu�B�VB�B��B~�B�4B{�Bv�ByrBp;BpoBl�Bo BtSBt�BtSBm�Bj�Be�Be�Bg�Ba|BT�BUgBQNBPBI�BB[BA�BB�BJ�B@�B>�B=B;0B:�B;0B;�B;�B7�B:�B>wB8RB8B8RB4�B6B?HB9�B8�B:�BE�BE�BE9BZQBj�Bs�Ba�Be�Bk�BkPBrB�{B�+B��B�nB�_B�=B�B��B��B��B�FB�tB�RB�dB�<B��B��B�-B�UB��B�dB�B��B�XB�}B��B��B�aB��B��B�UB�B�tB�gB��B��B� B�wB�tB�wB�IB��B�UB��B�qB�B�CB�B��B�~B��B��B�eB�=B��B��B��B�1B�_B��B�YB�%B��B�B��B��B�MB��B�%B��B��B�YB��B�=B��B�@B�GB��B�uB�B�BbB~�B~(B}VB{�B{JB|PBx�By	By�Bw�Bv�Bv`Bu�BzDB��B�nBtSBn.B�@B��Bd�Bb�B_pBe�Bc�BbB^5BaB`�Bb�BcTBbB`�BbNBbBb�B]�B^�B_�B_�B\]BcTB^5Bg�Bm(Be�B_B\�B\�BV�BV8BX�BXBS�BT�BU�BR�BVmBW>BRTBQ�BQ�BT`BV�BXyBT�BR�BQ�BQBPHBOBBN�BPBOBBL/BMjBN�BQBPHBf2BaBN<BL�BM�BI�BI�BK^BbBPBN<BMBM5BQBS&BRTBJ�BLdBP�BJ�BI�BLdBJWBI�BI�BL�BI�BH�BM�BLdBH�BF?BK)BE�B?}BK)BO�B:)B+6B$B�B�BUB�BB�B�BOBB�B�B*B_B�B�BRB�B@B$B�B�B�B�B�B�B'B�B�B�BxB1B{B%BPB	B�B~B�	B�(B@B�"B��B��B��B�AB�oB�B��B� B�B�(B�]B�B��B�oB��B��B�DB��B��B�B�B�yB�lB�B��B�BڅB��B��B�BԕB��B�&B�?BɺB�
B�B�tB��B�B��B��B��B��B�B��B�kB�\B�7B��B�=B�xB�~B�xB�=B��B�B|�B�B�+Bj�Bf�B_pB^�B[WBX�BVmBPHBO�BK�BC�BD�BA�BGEB:^B-BB,�B+6B+B'�B&B&B&�B&B"hB!�B"�B [BB�B#BIB&LB"�B(�B"�B�B�BBLBtB�B:B:B�B�B�B�BB�B	�B
=B%B	B�B�B
��B�B�B�B
�bB
��B�B
�bBB
��B
��B
��B
�`B
�B
�%B
��B
��B
�B
�B
�rB
��B
�yB
��B
�yB
�B
�vB
�cB
�)B
�cB
�B
�#B
��B
��B
�HB
�|B
�pB
��B
�WB
�/B
�)B
��B
��B
��B
�KB
�&B
�0B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230727090047                            20230727090047AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072709004720230727090047  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072709004720230727090047QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072709004720230727090047QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               