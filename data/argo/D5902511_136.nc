CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-06-12T23:49:10Z creation; 2020-07-07T21:55:47Z DMQC;      
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
_FillValue        G�O�     h  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [p   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     h  c   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     h  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  �x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  �|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 0P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h 7�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � VT   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h ]�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` |X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   |�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200612234910  20210429202813  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_136                 6810_008521_136                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @� �лn�@� �лn�11  @� �S&@� �S&@39e+��a@39e+��a�e+٩T"�e+٩T"11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@�  @��R@�p�@�  A ��A��A   A,(�A@��A`  A\)A��A�  A��A��A�Q�A��A��B (�B(�B�
B  B   B((�B0Q�B8  B@(�BH  BO�
BW�
B_�
Bg�
Bp  Bw�
B�B��
B�  B�{B�  B�  B�{B�{B��B�  B�{B�  B��B�  B�{B�{B�{B��B�{B�{B��B��
B�{B�  B�{B�{B�  B��B�{B�  B�  B�{C 
=C
=C
=C  C��C	��C  C  C  C
=C
=C  C  C  C  C��C�C!��C$  C&
=C'��C*  C,  C.  C0  C2{C4  C6  C8
=C:
=C<
=C>{C@
=CB
=CD
=CF  CG��CI��CK��CN  CP  CR
=CT
=CV  CX  CZ
=C\  C]��C`  Ca�Cd{Cf
=Cg��Cj
=Cl  Cm��Cp  Cr
=Ct  Cu��Cx  Cz  C|
=C~  C�
=C�C�C�C�C�  C���C�  C�C�C�C�  C�  C�  C���C���C�  C���C�  C�  C�  C�C�C�C���C���C���C�  C�  C���C���C���C���C���C�  C���C�  C���C�  C�  C�  C�C�C�  C�C�  C�  C�  C���C���C���C���C�  C�  C�C�  C�  C�  C���C�
=C�  C���C�
=C�  C�C�
=C�  C�C�  C�C�C�C�  C�C�
=C�C��C���C�C���C���C���C���C���C�  C�  C�C���C�  C�  C�  C�
=C�  C���C�  C�  C���C���C�  C���C���C�  C���C���C�C�C���C�  C���C���C���C���C�C�C�C�
=C�
=C�  C�C�  C�  C���C���C���C�C�  C�  C���D   D ��D �qD}qD  D�DD��D  D� D  Dz�D��D� D�D}qD  D��D	  D	� D
  D
z�D  D��D  D}qD�qD}qD  D� D�qD� D  D}qD�qD� D�D}qD��D� D  D� D�D� D  D��D�D}qD�qD� D�D��D�D}qD�qD� D�D� D�qD� D�qD}qD  D�D   D z�D �qD!� D!�qD"z�D#�D#�D$�D$� D%  D%� D&�D&� D&�qD'}qD'�qD(� D(��D)� D)�qD*}qD+  D+��D+�qD,� D-  D-��D.�D.�D/�D/� D0�D0� D0�qD1� D1�qD2}qD3  D3��D4  D4� D5  D5}qD6  D6� D6�qD7� D8  D8}qD8�qD9� D:  D:}qD:�qD;}qD;�qD<}qD<��D=z�D=��D>� D?�D?� D@  D@� DA  DA��DB�DB��DCDC�DDDD��DD�qDE}qDE�qDF}qDF�qDG}qDG��DH}qDI�DI��DJ  DJ� DK  DK��DL�DL� DL��DMz�DM�qDN� DN�qDO}qDP�DP��DQ  DQ}qDQ�qDR}qDR�qDS}qDS�qDT� DU  DU� DU�qDV� DW�DW}qDX  DX}qDX��DY� DZ�DZ��D[�D[� D[�qD\}qD\�qD]� D^  D^}qD_  D_��D`  D`� Da�Da��Da�qDb��Dc�Dc��Dd�Dd� De  De� Df�Df�DgDg��Dh  Dh��DiDi��Di�qDj� Dk�Dk� Dl  Dl}qDm  Dm� Dn  Dn� Dn�qDo� Dp  Dp� Dq  Dq��Dr�Dr� Dr�qDs}qDt  Dt}qDt�qDu� Dv  Dv}qDw�Dw�Dx�Dx� Dy  Dy� Dy�qDz}qD{  D{� D{�qD|� D}�D}� D~  D~� D  D}qD�  D�B�D�� D���D�HD�@ D�� D��HD�HD�AHD��HD���D���D�@ D�~�D�� D���D�>�D�� D�� D���D�AHD��HD�� D��qD�>�D���D�D�HD�@ D�� D���D�  D�B�D��HD���D�  D�>�D�~�D���D�  D�B�D��HD�� D���D�>�D�� D�D��D�B�D��HD��HD�HD�@ D�� D�� D�  D�>�D�~�D�� D�HD�AHD�~�D�� D�  D�>�D�}qD�� D���D�@ D�~�D���D���D�@ D�� D���D�  D�AHD�� D�� D�  D�AHD��HD��HD��qD�@ D�� D���D���D�>�D�~�D��HD�HD�AHD��HD�� D�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�AHD��HD���D�HD�@ D�~�D�� D��D�>�D�~�D��HD�  D�>�D�� D��qD���D�@ D�~�D���D�  D�>�D�~�D���D�  D�B�D���D�D�HD�>�D�~�D��qD�HD�@ D�� D��HD�HD�@ D��HD�� D�  D�AHD�~�D�� D�HD�AHD��HD��HD���D�>�D�� D�� D�  D�>�D�~�D�� D���D�>�D�� D�� D���D�>�D��HD�D�HD�@ D�~�D���D���D�>�D�}qD��)D���D�AHD�� D�� D�HD�AHD�� D���D�HD�@ D�~�D���D�  D�@ D�~�D��qD��qD�>�D�� D���D�  D�@ D�� D�� D���D�>�D�� D���D���D�AHD�� D�� D��qD�>�D��HD��HD���D�>�D�� D��HD���D�>�D�~�D���D���D�@ D�~�D���D�HD�AHD�}qD��qD�  D�B�D�� D���D�HD�@ D�~�D�� D�  D�AHD�� D���D�HD�B�D�� D�� D�  D�>�D�|)D���D��qD�>�D��HD��HD���D�>�D��HD�� D���D�AHD�� D�� D�  D�@ D�� D�� D�HD�>�D D�� D���D�>�DÀ D��HD�HD�>�DĀ Dľ�D��qD�@ DŁHD��HD�  D�=qD�|)DƽqD���D�=qD�~�DǾ�D���D�=qD�~�DȾ�D���D�AHD�~�Dɾ�D�  D�@ D�~�D�� D�  D�@ Dˀ D��HD�  D�=qD�}qD̾�D�  D�B�D́HD�� D���D�>�D΀ D�� D�HD�AHDρHD��HD�  D�>�D�~�D�� D�HD�AHDсHDѽqD��)D�@ D�~�D�� D��D�B�Dӂ�D��HD�  D�AHDԁHD�� D���D�=qDՀ D�� D�  D�AHDցHD�D��D�@ D�~�D�� D��D�AHD؀ D�� D���D�@ DفHD��HD�HD�>�D�}qDھ�D�  D�@ Dۂ�D��HD�HD�@ D�~�D�� D�  D�@ D݁HD��HD��D�B�DށHD�D��D�B�D߁HD߾�D���D�@ D�~�D�qD�  D�AHD� D�� D�  D�>�D�~�D��HD��D�B�DわD��HD���D�@ D� D侸D��qD�@ D�HD�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�>�D�~�D�� D�  D�=qD�}qD�� D�HD�@ D� D�� D���D�@ D낏D��HD�  D�@ D삏D��HD���D�>�D�HD�D���D�@ D� D�� D�  D�@ DD��HD�  D�@ D�� D�� D�HD�@ D�HD�q?#�
?.{?L��?�  ?���?���?�(�?��H@�@�R@+�@B�\@Tz�@Y��@n{@}p�@��
@���@�@�(�@��
@�{@��@�(�@��@˅@�33@�p�@��@�@��@���A�A�
A
=A�A\)A�A�A��Ap�A ��A#�
A(��A,��A.�RA3�
A8Q�A;�A>�RAC33AG�AJ�HAN{AS�
AW
=AZ�HA`��Ac�
Ag�Al��Aq�Au�Ax��A~�RA�G�A��HA�p�A�Q�A��A��
A�ffA�Q�A��A�(�A��RA���A�=qA�z�A�\)A���A��\A�p�A��A�G�A��A�ffA�Q�A��A�z�A�
=A���A�=qA��A�\)A���A�33A�{AǮA�G�A�(�AθRA�  A�=qA���A�{A���AۅA�p�A�
=AᙚA�(�A�{A�A��A���A�RA�Q�A�\A��A�\)A���A��HA�p�A�\)B ��B�B�HB�B�B=qB�HB(�B	p�B
{B\)Bz�BG�B=qB�B��BG�BffB�B��Bp�B�\B  B��BB
=B  B��B�B33B   B ��B"=qB#\)B$(�B%�B&�\B'\)B(Q�B)G�B*�RB+�B,Q�B-B.�HB/�B0��B2{B2�HB3�B4��B6{B6�RB8  B9�B9�B:�HB<(�B=p�B>�\B?\)B@��BA�BB�HBC�
BE�BF=qBG
=BH(�BI��BJ=qBK\)BL��BM��BNffBO�
BQ�BR{BR�HBTQ�BU��BVffBW\)BX��BY�BZ�RB\  B]G�B^{B_33B`��Ba��BbffBc�Bd��BeBf�\Bg�
Bi�BiBk33Blz�BmG�Bn{Bo\)Bp��Bqp�BrffBs�
Bt��BuBw33Bx(�Bx��BzffB{�B|��B}p�B~�RB�{B��\B�
=B��B�Q�B��RB�33B��B�z�B���B�\)B�  B��RB��B���B�Q�B��HB�G�B��
B��\B��B��B�{B���B�G�B�B�ffB���B�\)B�  B��RB��B���B�=qB���B�33B��B�ffB��HB�G�B�B�ffB���B�p�B��
B�z�B��B��B��B���B�G�B��B�(�B���B�\)B��B�=qB��HB��B�{B�z�B��HB���B�=qB���B��B�B�ffB��HB�\)B�  B���B��B���B�(�B���B�p�B��
B�ffB��B��B�(�B���B�G�B��B�z�B���B��B�(�B���B��B�B�ffB��HB�\)B��
B��\B�33B���B�(�B���B�p�B��
B�ffB��B�B�(�B���B�G�B��B�ffB���B�\)B�  B�z�B��HB�33B��
B�Q�B���B���B��B��B�{B�ffB���B�p�BÙ�B��B�ffB���B��BŅB�  B�z�BƸRB�
=BǅB�{B�Q�Bȏ\B�
=Bə�B��
B�(�Bʣ�B�
=B�\)BˮB�{Ḅ�B���B�33B͙�B�=qBΏ\B��HB�33BϮB�(�BЏ\B���B�G�B��
B�(�B�ffB���B�33B�B�(�B�ffB���B�G�B�B�{B�Q�B���B�G�Bי�B��
B�Q�B��HB�33B�\)B�B�=qBڣ�B���B�33BۅB�  B�ffB��HB�33B�p�B��
B�ffB޸RB�
=B�G�B�B�=qB�z�B���B�\)B�B��B�=qB�RB�33B�B�B�{B�\B�
=B�33B�B�  B�ffB�RB�
=B�G�B�B�(�B��B��HB��B陚B�{B�z�B��B���B�p�B��B�Q�B�\B��HB�\)B��
B�{B�ffB���B�33B�B��B�=qB�RB�33B�p�B�B�(�B�\B���B�\)B�B��
B�Q�B��RB���B�33B�B�(�B��\B��RB��B��B�  B�=qB��\B�
=B�G�B���B�  B�ffB���B���B�p�B��
B�{B�Q�B��RB��B�p�B���B�  B�ffB���B���B�G�B�C   C �C Q�C �C �RC �
C ��C33Cp�C��C�C�HC�CG�C\)C�\C��C  C(�C=qCffC��C�
C  C�CQ�C�\CC�HC  C33Cp�C�\C�C�
C{CG�Cp�C�C�C�C�C=qC\)C��C��C  C�C=qCp�C��C�HC	  C	�C	G�C	�C	�RC	�
C	��C
33C
p�C
�\C
�C
�
C{CQ�Cp�C�\C�RC��C�C=qC\)C�\CC  C�C=qCp�C��C�
C
=C�C=qC�C�RC�
C��C{CG�C�C�C��C��C�CffC��CC�HC
=C=qCz�C�C�HC  C(�C\)C��C�
C��C�C\)C��CC�C{CG�C�\CC�C{C33Cp�C�C�
C  C�C\)C��C�
C��C{CG�C�\CC�HC
=CQ�C�C�C��C  C=qCp�C��C�RC��C=qCp�C�\CC�C(�C\)C�\C�C�HC�C\)C�C�C�
C
=CG�Cz�C��CC  C=qCp�C�\CC  C=qCffCz�CC 
=C 33C Q�C �\C ��C!
=C!=qC!ffC!�\C!��C"
=C"Q�C"z�C"��C"��C#33C#Q�C#�C#�
C${C$=qC$p�C$��C$�HC%�C%\)C%�\C%�RC%�C&33C&p�C&��C&��C'  C'G�C'�\C'��C(  C((�C(\)C(��C(�C)33C)ffC)�\C)C*
=C*Q�C*�\C*��C*��C+(�C+ffC+�C+��C,(�C,\)C,�\C,��C-{C-\)C-��C-C-��C.=qC.�\C.�
C/
=C/=qC/p�C/�C/�C033C0p�C0��C0�
C1�C1ffC1�RC1�HC2{C2Q�C2��C2�HC3{C3Q�C3�C3��C4�C4Q�C4�C4C5
=C5Q�C5�\C5�RC5��C6G�C6�\C6�
C7�C7Q�C7�C7C8{C8\)C8�\C8C8��C933C9�\C9��C:
=C:=qC:p�C:��C:�C;33C;z�C;�RC;��C<(�C<\)C<��C<�
C=�C=\)C=��C=�HC>{C>G�C>z�C>C?  C?G�C?z�C?��C?�HC@(�C@ffC@�C@�
CA{CAG�CA�CA��CB
=CBQ�CB�CBCB��CC(�CCffCC�CC��CD33CDffCD��CD��CE
=CEQ�CE�\CE�
CF  CF33CF\)CF��CF�CG(�CGffCG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                            ?�  @   @B�\@�  @��R@�p�@�  A ��A��A   A,(�A@��A`  A\)A��A�  A��A��A�Q�A��A��B (�B(�B�
B  B   B((�B0Q�B8  B@(�BH  BO�
BW�
B_�
Bg�
Bp  Bw�
B�B��
B�  B�{B�  B�  B�{B�{B��B�  B�{B�  B��B�  B�{B�{B�{B��B�{B�{B��B��
B�{B�  B�{B�{B�  B��B�{B�  B�  B�{C 
=C
=C
=C  C��C	��C  C  C  C
=C
=C  C  C  C  C��C�C!��C$  C&
=C'��C*  C,  C.  C0  C2{C4  C6  C8
=C:
=C<
=C>{C@
=CB
=CD
=CF  CG��CI��CK��CN  CP  CR
=CT
=CV  CX  CZ
=C\  C]��C`  Ca�Cd{Cf
=Cg��Cj
=Cl  Cm��Cp  Cr
=Ct  Cu��Cx  Cz  C|
=C~  C�
=C�C�C�C�C�  C���C�  C�C�C�C�  C�  C�  C���C���C�  C���C�  C�  C�  C�C�C�C���C���C���C�  C�  C���C���C���C���C���C�  C���C�  C���C�  C�  C�  C�C�C�  C�C�  C�  C�  C���C���C���C���C�  C�  C�C�  C�  C�  C���C�
=C�  C���C�
=C�  C�C�
=C�  C�C�  C�C�C�C�  C�C�
=C�C��C���C�C���C���C���C���C���C�  C�  C�C���C�  C�  C�  C�
=C�  C���C�  C�  C���C���C�  C���C���C�  C���C���C�C�C���C�  C���C���C���C���C�C�C�C�
=C�
=C�  C�C�  C�  C���C���C���C�C�  C�  C���D   D ��D �qD}qD  D�DD��D  D� D  Dz�D��D� D�D}qD  D��D	  D	� D
  D
z�D  D��D  D}qD�qD}qD  D� D�qD� D  D}qD�qD� D�D}qD��D� D  D� D�D� D  D��D�D}qD�qD� D�D��D�D}qD�qD� D�D� D�qD� D�qD}qD  D�D   D z�D �qD!� D!�qD"z�D#�D#�D$�D$� D%  D%� D&�D&� D&�qD'}qD'�qD(� D(��D)� D)�qD*}qD+  D+��D+�qD,� D-  D-��D.�D.�D/�D/� D0�D0� D0�qD1� D1�qD2}qD3  D3��D4  D4� D5  D5}qD6  D6� D6�qD7� D8  D8}qD8�qD9� D:  D:}qD:�qD;}qD;�qD<}qD<��D=z�D=��D>� D?�D?� D@  D@� DA  DA��DB�DB��DCDC�DDDD��DD�qDE}qDE�qDF}qDF�qDG}qDG��DH}qDI�DI��DJ  DJ� DK  DK��DL�DL� DL��DMz�DM�qDN� DN�qDO}qDP�DP��DQ  DQ}qDQ�qDR}qDR�qDS}qDS�qDT� DU  DU� DU�qDV� DW�DW}qDX  DX}qDX��DY� DZ�DZ��D[�D[� D[�qD\}qD\�qD]� D^  D^}qD_  D_��D`  D`� Da�Da��Da�qDb��Dc�Dc��Dd�Dd� De  De� Df�Df�DgDg��Dh  Dh��DiDi��Di�qDj� Dk�Dk� Dl  Dl}qDm  Dm� Dn  Dn� Dn�qDo� Dp  Dp� Dq  Dq��Dr�Dr� Dr�qDs}qDt  Dt}qDt�qDu� Dv  Dv}qDw�Dw�Dx�Dx� Dy  Dy� Dy�qDz}qD{  D{� D{�qD|� D}�D}� D~  D~� D  D}qD�  D�B�D�� D���D�HD�@ D�� D��HD�HD�AHD��HD���D���D�@ D�~�D�� D���D�>�D�� D�� D���D�AHD��HD�� D��qD�>�D���D�D�HD�@ D�� D���D�  D�B�D��HD���D�  D�>�D�~�D���D�  D�B�D��HD�� D���D�>�D�� D�D��D�B�D��HD��HD�HD�@ D�� D�� D�  D�>�D�~�D�� D�HD�AHD�~�D�� D�  D�>�D�}qD�� D���D�@ D�~�D���D���D�@ D�� D���D�  D�AHD�� D�� D�  D�AHD��HD��HD��qD�@ D�� D���D���D�>�D�~�D��HD�HD�AHD��HD�� D�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�AHD��HD���D�HD�@ D�~�D�� D��D�>�D�~�D��HD�  D�>�D�� D��qD���D�@ D�~�D���D�  D�>�D�~�D���D�  D�B�D���D�D�HD�>�D�~�D��qD�HD�@ D�� D��HD�HD�@ D��HD�� D�  D�AHD�~�D�� D�HD�AHD��HD��HD���D�>�D�� D�� D�  D�>�D�~�D�� D���D�>�D�� D�� D���D�>�D��HD�D�HD�@ D�~�D���D���D�>�D�}qD��)D���D�AHD�� D�� D�HD�AHD�� D���D�HD�@ D�~�D���D�  D�@ D�~�D��qD��qD�>�D�� D���D�  D�@ D�� D�� D���D�>�D�� D���D���D�AHD�� D�� D��qD�>�D��HD��HD���D�>�D�� D��HD���D�>�D�~�D���D���D�@ D�~�D���D�HD�AHD�}qD��qD�  D�B�D�� D���D�HD�@ D�~�D�� D�  D�AHD�� D���D�HD�B�D�� D�� D�  D�>�D�|)D���D��qD�>�D��HD��HD���D�>�D��HD�� D���D�AHD�� D�� D�  D�@ D�� D�� D�HD�>�D D�� D���D�>�DÀ D��HD�HD�>�DĀ Dľ�D��qD�@ DŁHD��HD�  D�=qD�|)DƽqD���D�=qD�~�DǾ�D���D�=qD�~�DȾ�D���D�AHD�~�Dɾ�D�  D�@ D�~�D�� D�  D�@ Dˀ D��HD�  D�=qD�}qD̾�D�  D�B�D́HD�� D���D�>�D΀ D�� D�HD�AHDρHD��HD�  D�>�D�~�D�� D�HD�AHDсHDѽqD��)D�@ D�~�D�� D��D�B�Dӂ�D��HD�  D�AHDԁHD�� D���D�=qDՀ D�� D�  D�AHDցHD�D��D�@ D�~�D�� D��D�AHD؀ D�� D���D�@ DفHD��HD�HD�>�D�}qDھ�D�  D�@ Dۂ�D��HD�HD�@ D�~�D�� D�  D�@ D݁HD��HD��D�B�DށHD�D��D�B�D߁HD߾�D���D�@ D�~�D�qD�  D�AHD� D�� D�  D�>�D�~�D��HD��D�B�DわD��HD���D�@ D� D侸D��qD�@ D�HD�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�>�D�~�D�� D�  D�=qD�}qD�� D�HD�@ D� D�� D���D�@ D낏D��HD�  D�@ D삏D��HD���D�>�D�HD�D���D�@ D� D�� D�  D�@ DD��HD�  D�@ D�� D�� D�HD�@ D�HG�O�?#�
?.{?L��?�  ?���?���?�(�?��H@�@�R@+�@B�\@Tz�@Y��@n{@}p�@��
@���@�@�(�@��
@�{@��@�(�@��@˅@�33@�p�@��@�@��@���A�A�
A
=A�A\)A�A�A��Ap�A ��A#�
A(��A,��A.�RA3�
A8Q�A;�A>�RAC33AG�AJ�HAN{AS�
AW
=AZ�HA`��Ac�
Ag�Al��Aq�Au�Ax��A~�RA�G�A��HA�p�A�Q�A��A��
A�ffA�Q�A��A�(�A��RA���A�=qA�z�A�\)A���A��\A�p�A��A�G�A��A�ffA�Q�A��A�z�A�
=A���A�=qA��A�\)A���A�33A�{AǮA�G�A�(�AθRA�  A�=qA���A�{A���AۅA�p�A�
=AᙚA�(�A�{A�A��A���A�RA�Q�A�\A��A�\)A���A��HA�p�A�\)B ��B�B�HB�B�B=qB�HB(�B	p�B
{B\)Bz�BG�B=qB�B��BG�BffB�B��Bp�B�\B  B��BB
=B  B��B�B33B   B ��B"=qB#\)B$(�B%�B&�\B'\)B(Q�B)G�B*�RB+�B,Q�B-B.�HB/�B0��B2{B2�HB3�B4��B6{B6�RB8  B9�B9�B:�HB<(�B=p�B>�\B?\)B@��BA�BB�HBC�
BE�BF=qBG
=BH(�BI��BJ=qBK\)BL��BM��BNffBO�
BQ�BR{BR�HBTQ�BU��BVffBW\)BX��BY�BZ�RB\  B]G�B^{B_33B`��Ba��BbffBc�Bd��BeBf�\Bg�
Bi�BiBk33Blz�BmG�Bn{Bo\)Bp��Bqp�BrffBs�
Bt��BuBw33Bx(�Bx��BzffB{�B|��B}p�B~�RB�{B��\B�
=B��B�Q�B��RB�33B��B�z�B���B�\)B�  B��RB��B���B�Q�B��HB�G�B��
B��\B��B��B�{B���B�G�B�B�ffB���B�\)B�  B��RB��B���B�=qB���B�33B��B�ffB��HB�G�B�B�ffB���B�p�B��
B�z�B��B��B��B���B�G�B��B�(�B���B�\)B��B�=qB��HB��B�{B�z�B��HB���B�=qB���B��B�B�ffB��HB�\)B�  B���B��B���B�(�B���B�p�B��
B�ffB��B��B�(�B���B�G�B��B�z�B���B��B�(�B���B��B�B�ffB��HB�\)B��
B��\B�33B���B�(�B���B�p�B��
B�ffB��B�B�(�B���B�G�B��B�ffB���B�\)B�  B�z�B��HB�33B��
B�Q�B���B���B��B��B�{B�ffB���B�p�BÙ�B��B�ffB���B��BŅB�  B�z�BƸRB�
=BǅB�{B�Q�Bȏ\B�
=Bə�B��
B�(�Bʣ�B�
=B�\)BˮB�{Ḅ�B���B�33B͙�B�=qBΏ\B��HB�33BϮB�(�BЏ\B���B�G�B��
B�(�B�ffB���B�33B�B�(�B�ffB���B�G�B�B�{B�Q�B���B�G�Bי�B��
B�Q�B��HB�33B�\)B�B�=qBڣ�B���B�33BۅB�  B�ffB��HB�33B�p�B��
B�ffB޸RB�
=B�G�B�B�=qB�z�B���B�\)B�B��B�=qB�RB�33B�B�B�{B�\B�
=B�33B�B�  B�ffB�RB�
=B�G�B�B�(�B��B��HB��B陚B�{B�z�B��B���B�p�B��B�Q�B�\B��HB�\)B��
B�{B�ffB���B�33B�B��B�=qB�RB�33B�p�B�B�(�B�\B���B�\)B�B��
B�Q�B��RB���B�33B�B�(�B��\B��RB��B��B�  B�=qB��\B�
=B�G�B���B�  B�ffB���B���B�p�B��
B�{B�Q�B��RB��B�p�B���B�  B�ffB���B���B�G�B�C   C �C Q�C �C �RC �
C ��C33Cp�C��C�C�HC�CG�C\)C�\C��C  C(�C=qCffC��C�
C  C�CQ�C�\CC�HC  C33Cp�C�\C�C�
C{CG�Cp�C�C�C�C�C=qC\)C��C��C  C�C=qCp�C��C�HC	  C	�C	G�C	�C	�RC	�
C	��C
33C
p�C
�\C
�C
�
C{CQ�Cp�C�\C�RC��C�C=qC\)C�\CC  C�C=qCp�C��C�
C
=C�C=qC�C�RC�
C��C{CG�C�C�C��C��C�CffC��CC�HC
=C=qCz�C�C�HC  C(�C\)C��C�
C��C�C\)C��CC�C{CG�C�\CC�C{C33Cp�C�C�
C  C�C\)C��C�
C��C{CG�C�\CC�HC
=CQ�C�C�C��C  C=qCp�C��C�RC��C=qCp�C�\CC�C(�C\)C�\C�C�HC�C\)C�C�C�
C
=CG�Cz�C��CC  C=qCp�C�\CC  C=qCffCz�CC 
=C 33C Q�C �\C ��C!
=C!=qC!ffC!�\C!��C"
=C"Q�C"z�C"��C"��C#33C#Q�C#�C#�
C${C$=qC$p�C$��C$�HC%�C%\)C%�\C%�RC%�C&33C&p�C&��C&��C'  C'G�C'�\C'��C(  C((�C(\)C(��C(�C)33C)ffC)�\C)C*
=C*Q�C*�\C*��C*��C+(�C+ffC+�C+��C,(�C,\)C,�\C,��C-{C-\)C-��C-C-��C.=qC.�\C.�
C/
=C/=qC/p�C/�C/�C033C0p�C0��C0�
C1�C1ffC1�RC1�HC2{C2Q�C2��C2�HC3{C3Q�C3�C3��C4�C4Q�C4�C4C5
=C5Q�C5�\C5�RC5��C6G�C6�\C6�
C7�C7Q�C7�C7C8{C8\)C8�\C8C8��C933C9�\C9��C:
=C:=qC:p�C:��C:�C;33C;z�C;�RC;��C<(�C<\)C<��C<�
C=�C=\)C=��C=�HC>{C>G�C>z�C>C?  C?G�C?z�C?��C?�HC@(�C@ffC@�C@�
CA{CAG�CA�CA��CB
=CBQ�CB�CBCB��CC(�CCffCC�CC��CD33CDffCD��CD��CE
=CEQ�CE�\CE�
CF  CF33CF\)CF��CF�CG(�CGffCG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                            @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aڝ�Aډ7Aډ7Aڇ+Aڇ+A�v�A�z�A�t�A�x�A�x�A�r�A�r�A�t�A�jA�n�A�l�A�\)A�S�A�Q�A�O�A�O�A�K�A�I�A�7LAٝ�A�O�A�l�A՗�A�n�A���A�jA�(�A��`Aқ�A�{A�hsA��A�"�A���A��A�S�A�JA�  A��mA�7LA���A˃A�l�A�ZA�O�A�+A�oA�A�S�A�;dA��A���Aɲ-Aɛ�AɍPA�l�A���A�v�A�%AǶFAǑhAǋDA�jA�`BA�r�AǕ�AǶFA�bNA��TA�/AŇ+A��A��A�A�A���A�XA��uA�bA�p�A�r�A�M�A���A���A�-A��A��A�|�A�(�A�n�A���A��DA���A��9A�oA���A��A��yA�O�A��wA��uA��TA�-A�ĜA�S�A���A�?}A�hsA�\)A��A���A���A�33A��7A��!A���A�JA���A�E�A�ZA�/A��A�Q�A�`BA��mA��jA�ĜA�I�A~��A}l�A{�Ay��AxbNAv�`ArE�Ao�Am`BAlz�Akp�Aj�\AjAi\)AhAf��Af5?AeƨAd��AaS�A_�A^I�A^  A]?}A[�mAZȴAW�#AU?}ATȴAS��ARjAP��AN�AM`BAI�AFE�AC��AB�AA��A@{A>=qA=�A<~�A;��A9��A8bA6{A4E�A2��A1ƨA0�yA0I�A/A.��A.~�A.VA-��A-�hA-\)A,z�A+��A+A*�A)�A)A(  A'\)A%��A#�A"A�A��A1'A`BA�A�jA�A�A��A��A��A5?A`BA��A��A  A�\A�hAt�A��AZA33A	�;A	\)AjA�#A`BAK�A��AhsAZA?}A\)A �/A M�@�;d@��@�ȴ@�ȴ@�G�@��@�%@��D@�+@�r�@�+@�ȴ@���@�
=@��@�^5@�{@�X@�  @�K�@��y@�h@�j@柾@�7L@�A�@��m@�l�@�@�\)@�V@���@ݩ�@��T@�$�@�=q@�^5@��@�?}@�bN@�1@ە�@�;d@�@ڰ!@�^5@�J@�(�@��H@���@�ff@�{@պ^@�G�@���@���@Լj@��@ҟ�@���@�dZ@�C�@�;d@�ff@�(�@�  @˶F@�dZ@��@��@ɩ�@ɑh@�x�@�hs@�G�@���@�r�@��
@�33@ư!@Ɵ�@�E�@��T@ř�@�%@��@�=q@��h@�V@��@��@�+@��y@���@���@�V@�J@��T@�X@���@��@���@���@�@��T@�@���@���@�x�@�p�@�hs@�hs@�O�@���@��9@�bN@��
@�S�@�33@��@�@��y@���@��@�?}@�7L@�/@�V@���@��/@���@��j@��u@�r�@�j@�Q�@�1'@��@��m@�t�@��!@���@�hs@�?}@�&�@���@��@��`@�Ĝ@��9@��9@���@�r�@�I�@�9X@�9X@�A�@�I�@�A�@�ƨ@�\)@�"�@��!@�M�@���@��@���@�Q�@�ƨ@��y@�^5@�J@���@���@�x�@�/@��@���@��@���@�j@�A�@��P@�o@�ȴ@�=q@��@�p�@�/@�&�@�&�@�&�@���@�b@�l�@�K�@�;d@�@��@���@�ȴ@���@�^5@���@�x�@�O�@�V@���@���@�9X@���@�ƨ@���@���@��@�dZ@�;d@�@��!@��+@�V@��@��T@���@�V@���@�I�@��@��w@��@�C�@�~�@�=q@�$�@�{@��@��@��@��#@��@��`@�bN@���@��@�dZ@�;d@�o@��@��!@���@�J@��#@��^@��7@�V@�Ĝ@�bN@��@�t�@�+@���@��R@���@�ff@�5?@�@���@��@�O�@���@��j@���@��u@��@�z�@�j@��@��@�K�@���@��T@�G�@���@�A�@��@�b@�  @��@��
@��w@��@�\)@�+@��@�@���@�-@�@���@���@���@�r�@��w@�dZ@���@���@��\@�~�@�n�@�^5@�=q@�5?@�5?@�-@�-@�-@�-@�-@�5?@�-@�-@�{@���@���@��@�/@��@�%@���@��/@�j@��@�ƨ@�|�@�
=@��\@�@���@���@��7@�O�@�/@�V@���@���@�bN@�1@|�@\)@;d@+@
=@~��@~{@}�@|��@{��@{dZ@{"�@z�@z��@z��@z�!@yhs@xr�@xb@w|�@wl�@w\)@w\)@wK�@wK�@w�@v��@v�y@v�R@v$�@u��@u�h@t�j@t1@sdZ@s"�@so@r~�@rJ@q��@q��@qx�@p��@p�@o�@o�@o�@o+@m�h@mV@l�@lI�@k��@kC�@j~�@j=q@j�@i�7@h�9@hb@g�w@g��@g��@g�P@gK�@f�y@fv�@e�@e��@d��@cdZ@cC�@c33@co@c@c@b�@b�@b�H@b�H@b~�@a�@a��@aG�@a&�@`��@`1'@`b@`  @_�@_�;@_�;@_��@_��@_�@_|�@_K�@_�@^��@^�@^v�@^V@^E�@^E�@^E�@^E�@^5?@]`B@[o@Y�#@Y�#@Y�#@Y��@Y�7@YX@Y7L@X�`@X��@X�@XQ�@X �@W�;@W�w@W�P@W�@VE�@V5?@U��@UV@Tz�@S�m@SdZ@R��@Q�@P�`@P�@P �@O��@O
=@M�h@Lz�@L�@K�F@KS�@J=q@I��@I�@I��@IG�@HbN@Fȴ@E?}@EV@D�@D��@D�j@D�j@D�j@Dj@C��@Co@B�@B�!@B^5@BJ@A�^@A7L@@��@@�9@@��@@��@@�u@@Q�@@1'@@b@@b@?�@?��@>��@>�R@>V@=�T@=��@=/@<�D@<�@<1@;�F@;dZ@;33@;"�@;@:��@:�\@:~�@:M�@:-@:J@9��@9��@9��@9x�@97L@8�`@8Q�@7��@7�P@7|�@7K�@6�y@6�R@6ff@5/@4��@4��@49X@3�F@3"�@2�@2��@2�@1&�@1%@0r�@0r�@0Q�@/��@/�w@/��@/�P@/��@/+@/�@/�@/�@/
=@/
=@/
=@/
=@/
=@/
=@.��@.�y@.�y@.�@.�@.ȴ@.�R@.�R@.�+@.v�@.ff@.E�@.{@-�@-��@-@-�-@-�-@-��@,��@,z�@,j@,j@,j@,j@,Z@,Z@,Z@,Z@,Z@,Z@,1@+�m@+�
@+��@*�@*��@*^5@*�@)�#@)��@)X@)&�@(��@(��@(��@(��@(��@(��@(��@(�u@(�u@(�u@(�@(�@(�@(�@(�@(r�@(bN@(A�@(b@'�w@&��@&{@%�-@$��@#��@#�m@#ƨ@#�F@#�F@#��@#��@#��@#��@#��@#��@#t�@#t�@#C�@#S�@"��@"=q@!G�@ Ĝ@ Q�@   @��@�P@\)@\)@\)@\)@\)@\)@K�@;d@;d@+@+@+@�@�R@v�@E�@$�@@��@�@�@�j@z�@��@t�@o@�!@^5@=q@-@�@�^@�7@�7@7L@&�@%@�u@  @�@�P@�R@�+@�@I�@�F@�F@33@~�@M�@��@�`@�@r�@r�@r�@r�@r�Aڝ�Aڟ�Aڝ�Aڝ�Aڛ�Aڟ�A�z�A�z�Aڏ\Aډ7AڅAډ7Aډ7Aڇ+AڋDAډ7AڅAځAڋDA�|�Aډ7AڋDA�p�A�t�A�|�A�x�A�|�AڃA�|�A�r�A�p�A�r�A�z�A�v�A�t�A�v�A�z�A�z�A�v�A�z�AځA�t�A�l�A�r�A�t�A�n�A�r�A�r�A�n�A�n�A�r�A�v�A�t�A�v�A�x�A�r�A�n�A�r�A�r�A�x�A�r�A�r�A�hsA�ffA�l�A�hsA�dZA�hsA�l�A�jA�hsA�p�A�v�A�n�A�l�A�n�A�l�A�hsA�jA�n�A�l�A�hsA�l�A�l�A�hsA�ffA�ZA�S�A�O�A�S�A�XA�S�A�O�A�Q�A�S�A�O�A�Q�A�VA�Q�A�O�A�Q�A�S�A�O�A�O�A�S�A�O�A�S�A�VA�Q�A�K�A�M�A�O�A�O�A�K�A�K�A�S�A�S�A�O�A�M�A�M�A�M�A�K�A�K�A�M�A�K�A�I�A�Q�A�G�A�G�A�I�A�K�A�G�A�I�A�M�A�G�A�I�A�M�A�G�A�E�A�E�A�5?A�+A�"�A��A��A�bA�%A���A�t�A�A�A��A���AؾwAة�A�hsA�VA�=qA�
=A���A�z�A��TA֗�A�\)A�I�A��A��A�1A��A��yA��HAվwAՉ7A�hsA�K�A��AԺ^Aԥ�AԓuA�n�A�K�A�5?A� �A�bA�  A��`A���A�ȴAӾwAӬAӝ�AӑhA�~�A�n�A�ffA�ZA�M�A�M�A�M�A�C�A�7LA�/A��A�
=A�  A�  A���A��A��mA��;A��
A�ȴA���AҴ9Aҩ�Aң�Aқ�AҋDA�~�A�r�A�dZA�M�A�7LA��A��A���A�ĜAѶFAї�A�z�A�hsA�dZA�O�A�7LA�1'A��A�A���A��/A�AЗ�A�jA�A�A�$�A�"�A��A��A�bA�
=A�  A��A��A���A���AϾwAϡ�A�x�A�XA�/A�bA�%A���A��yA��HA���A�~�A�A�A�+A�(�A��A��A�{A�oA�
=A�
=A�JA�%A�A�%A�%A�  A���A�A�  A���A���A���A���A���A��A��`A��/A���A͸RAͥ�A͓uA�n�A�O�A��A̰!A�jA�oA�1A��#A���A˲-A˧�A˛�AˑhAˉ7A˃A˃A˅A�|�A�x�A�x�A�t�A�p�A�jA�jA�jA�hsA�`BA�\)A�^5A�\)A�XA�S�A�XA�XA�XA�S�A�S�A�S�A�M�A�C�A�?}A�=qA�9XA�-A� �A�$�A�$�A��A��A��A��A�{A�JA�%A�  A���A��A��A��A��AʑhA�v�A�ffA�dZA�`BA�VA�XA�Q�A�I�A�E�A�G�A�G�A�A�A�?}A�A�A�=qA�9XA�9XA�;dA�;dA�5?A�-A�-A�/A�"�A��A��A��A�oA�VA�VA�VA�1A�A�%A�%A���A���A���A���A��A��yA��;A��A���A�ƨAɺ^Aɲ-Aɩ�Aɡ�Aɝ�Aɟ�Aɟ�Aɟ�Aɛ�Aɛ�Aɛ�Aɟ�Aɝ�Aə�Aɕ�Aɗ�Aɗ�Aɕ�Aɏ\AɑhAɓuAɏ\AɋDAɋDAɍPAɋDAɅAɅAɇ+Aɇ+AɃA�z�A�t�A�t�A�t�A�n�A�Q�A�A�A�+A��A�bA�  A��A��HA���AȮAȣ�Aȥ�Aȣ�Aȟ�Aț�Aȗ�Aȏ\Aȉ7AȁA�z�A�z�A�n�A�S�A�A�A�=qA�(�A�+A�"�A��A��A���A��A��A��A��A��HA��HA�ȴAǲ-Aǰ!Aǲ-Aǰ!Aǧ�Aǣ�Aǧ�Aǥ�Aǟ�AǕ�AǏ\AǓuAǓuAǏ\AǍPAǓuAǓuAǏ\AǋDAǍPAǍPAǏ\AǍPAǉ7Aǉ7AǍPAǏ\Aǉ7AǅAǁA�~�A�z�A�l�A�hsA�hsA�bNA�`BA�dZA�hsA�hsA�bNA�hsA�hsA�ffA�`BA�bNA�ffA�bNA�^5A�^5A�^5A�`BA�ZA�\)A�`BA�bNA�`BA�dZA�jA�p�A�t�A�~�AǅAǅAǃAǁAǃAǉ7Aǉ7AǋDAǕ�AǛ�Aǝ�Aǝ�Aǟ�Aǣ�AǛ�Aǡ�AǮAǰ!A���A�ƨA�AǾwAǼjAǼjAǼjAǰ!AǙ�AǏ\AǍPAǃA�v�A�`BA�ZA�Q�A�K�A�?}A�7LA�7LA�5?A�(�A�JA���A��yA��/A���A�AƲ-AƮAƧ�AƓuAƋDA�|�A�r�A�^5A�A�A� �A�bA�  A��A��HA���Ať�Aš�Aś�Aŕ�Aŕ�Aŗ�AœuAōPAŅA�`BA�S�A�I�A�;dA�+A��A�bA���A��A��/A���A�ƨA�ȴA���AĶFAđhAđhAė�Ać+A�O�A�(�A��A��Aú^A�x�A�ZA�M�A��A���A�ffA���A��#A��A��A��mA��mA��A��wA��-A���A�=qA��#A��DA�5?A�{A���A��A��A���A�A��9A��hA�Q�A�&�A�A��A��/A��
A���A��uA��uA��PA��DA��7A��PA��\A��hA��PA��PA��PA��DA��+A�~�A�t�A�`BA��A�S�A�%A�ȴA��A���A���A��hA��A�~�A�p�A�33A�(�A�JA���A��A��/A��9A�x�A�^5A�C�A�"�A�JA���A��TA��wA��!A���A��A�v�A�E�A���A��jA��7A�bNA�1A��A��A�\)A��TA��TA��wA��DA�hsA�C�A�
=A���A��#A���A���A��A��\A��A�t�A�l�A�bNA�S�A�A�A�/A�VA��;A���A���A�|�A�\)A�E�A��A�A��`A��wA���A��A�G�A�%A�A�1A�
=A�%A���A��A��TA��#A���A���A��A�VA��A��TA���A�XA��/A�M�A�ĜA�r�A�ZA�=qA�1A���A��!A���A�r�A�hsA�E�A�
=A��TA���A�jA�1'A���A��/A���A��jA���A�ffA�ZA�A�A�^5A�5?A��A�JA�  A���A��A��mA���A��^A���A�dZA�+A��A��
A�ƨA��jA��RA��9A���A��PA�p�A�S�A�E�A�-A�JA���A��mA��;A��/A��A���A���A��9A��DA�v�A�n�A�dZA�bNA�S�A�O�A�C�A��A���A��A�bNA�Q�A�;dA��A��A��
A���A�ȴA�ĜA���A���A�Q�A���A�Q�A�JA���A�A��\A�x�A�I�A�5?A�VA��^A��DA�jA�VA�I�A�7LA�bA���A���A�z�A�ffA�XA�=qA���A�-A���A�bA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                            Aڝ�Aډ7Aډ7Aڇ+Aڇ+A�v�A�z�A�t�A�x�A�x�A�r�A�r�A�t�A�jA�n�A�l�A�\)A�S�A�Q�A�O�A�O�A�K�A�I�A�7LAٝ�A�O�A�l�A՗�A�n�A���A�jA�(�A��`Aқ�A�{A�hsA��A�"�A���A��A�S�A�JA�  A��mA�7LA���A˃A�l�A�ZA�O�A�+A�oA�A�S�A�;dA��A���Aɲ-Aɛ�AɍPA�l�A���A�v�A�%AǶFAǑhAǋDA�jA�`BA�r�AǕ�AǶFA�bNA��TA�/AŇ+A��A��A�A�A���A�XA��uA�bA�p�A�r�A�M�A���A���A�-A��A��A�|�A�(�A�n�A���A��DA���A��9A�oA���A��A��yA�O�A��wA��uA��TA�-A�ĜA�S�A���A�?}A�hsA�\)A��A���A���A�33A��7A��!A���A�JA���A�E�A�ZA�/A��A�Q�A�`BA��mA��jA�ĜA�I�A~��A}l�A{�Ay��AxbNAv�`ArE�Ao�Am`BAlz�Akp�Aj�\AjAi\)AhAf��Af5?AeƨAd��AaS�A_�A^I�A^  A]?}A[�mAZȴAW�#AU?}ATȴAS��ARjAP��AN�AM`BAI�AFE�AC��AB�AA��A@{A>=qA=�A<~�A;��A9��A8bA6{A4E�A2��A1ƨA0�yA0I�A/A.��A.~�A.VA-��A-�hA-\)A,z�A+��A+A*�A)�A)A(  A'\)A%��A#�A"A�A��A1'A`BA�A�jA�A�A��A��A��A5?A`BA��A��A  A�\A�hAt�A��AZA33A	�;A	\)AjA�#A`BAK�A��AhsAZA?}A\)A �/A M�@�;d@��@�ȴ@�ȴ@�G�@��@�%@��D@�+@�r�@�+@�ȴ@���@�
=@��@�^5@�{@�X@�  @�K�@��y@�h@�j@柾@�7L@�A�@��m@�l�@�@�\)@�V@���@ݩ�@��T@�$�@�=q@�^5@��@�?}@�bN@�1@ە�@�;d@�@ڰ!@�^5@�J@�(�@��H@���@�ff@�{@պ^@�G�@���@���@Լj@��@ҟ�@���@�dZ@�C�@�;d@�ff@�(�@�  @˶F@�dZ@��@��@ɩ�@ɑh@�x�@�hs@�G�@���@�r�@��
@�33@ư!@Ɵ�@�E�@��T@ř�@�%@��@�=q@��h@�V@��@��@�+@��y@���@���@�V@�J@��T@�X@���@��@���@���@�@��T@�@���@���@�x�@�p�@�hs@�hs@�O�@���@��9@�bN@��
@�S�@�33@��@�@��y@���@��@�?}@�7L@�/@�V@���@��/@���@��j@��u@�r�@�j@�Q�@�1'@��@��m@�t�@��!@���@�hs@�?}@�&�@���@��@��`@�Ĝ@��9@��9@���@�r�@�I�@�9X@�9X@�A�@�I�@�A�@�ƨ@�\)@�"�@��!@�M�@���@��@���@�Q�@�ƨ@��y@�^5@�J@���@���@�x�@�/@��@���@��@���@�j@�A�@��P@�o@�ȴ@�=q@��@�p�@�/@�&�@�&�@�&�@���@�b@�l�@�K�@�;d@�@��@���@�ȴ@���@�^5@���@�x�@�O�@�V@���@���@�9X@���@�ƨ@���@���@��@�dZ@�;d@�@��!@��+@�V@��@��T@���@�V@���@�I�@��@��w@��@�C�@�~�@�=q@�$�@�{@��@��@��@��#@��@��`@�bN@���@��@�dZ@�;d@�o@��@��!@���@�J@��#@��^@��7@�V@�Ĝ@�bN@��@�t�@�+@���@��R@���@�ff@�5?@�@���@��@�O�@���@��j@���@��u@��@�z�@�j@��@��@�K�@���@��T@�G�@���@�A�@��@�b@�  @��@��
@��w@��@�\)@�+@��@�@���@�-@�@���@���@���@�r�@��w@�dZ@���@���@��\@�~�@�n�@�^5@�=q@�5?@�5?@�-@�-@�-@�-@�-@�5?@�-@�-@�{@���@���@��@�/@��@�%@���@��/@�j@��@�ƨ@�|�@�
=@��\@�@���@���@��7@�O�@�/@�V@���@���@�bN@�1@|�@\)@;d@+@
=@~��@~{@}�@|��@{��@{dZ@{"�@z�@z��@z��@z�!@yhs@xr�@xb@w|�@wl�@w\)@w\)@wK�@wK�@w�@v��@v�y@v�R@v$�@u��@u�h@t�j@t1@sdZ@s"�@so@r~�@rJ@q��@q��@qx�@p��@p�@o�@o�@o�@o+@m�h@mV@l�@lI�@k��@kC�@j~�@j=q@j�@i�7@h�9@hb@g�w@g��@g��@g�P@gK�@f�y@fv�@e�@e��@d��@cdZ@cC�@c33@co@c@c@b�@b�@b�H@b�H@b~�@a�@a��@aG�@a&�@`��@`1'@`b@`  @_�@_�;@_�;@_��@_��@_�@_|�@_K�@_�@^��@^�@^v�@^V@^E�@^E�@^E�@^E�@^5?@]`B@[o@Y�#@Y�#@Y�#@Y��@Y�7@YX@Y7L@X�`@X��@X�@XQ�@X �@W�;@W�w@W�P@W�@VE�@V5?@U��@UV@Tz�@S�m@SdZ@R��@Q�@P�`@P�@P �@O��@O
=@M�h@Lz�@L�@K�F@KS�@J=q@I��@I�@I��@IG�@HbN@Fȴ@E?}@EV@D�@D��@D�j@D�j@D�j@Dj@C��@Co@B�@B�!@B^5@BJ@A�^@A7L@@��@@�9@@��@@��@@�u@@Q�@@1'@@b@@b@?�@?��@>��@>�R@>V@=�T@=��@=/@<�D@<�@<1@;�F@;dZ@;33@;"�@;@:��@:�\@:~�@:M�@:-@:J@9��@9��@9��@9x�@97L@8�`@8Q�@7��@7�P@7|�@7K�@6�y@6�R@6ff@5/@4��@4��@49X@3�F@3"�@2�@2��@2�@1&�@1%@0r�@0r�@0Q�@/��@/�w@/��@/�P@/��@/+@/�@/�@/�@/
=@/
=@/
=@/
=@/
=@/
=@.��@.�y@.�y@.�@.�@.ȴ@.�R@.�R@.�+@.v�@.ff@.E�@.{@-�@-��@-@-�-@-�-@-��@,��@,z�@,j@,j@,j@,j@,Z@,Z@,Z@,Z@,Z@,Z@,1@+�m@+�
@+��@*�@*��@*^5@*�@)�#@)��@)X@)&�@(��@(��@(��@(��@(��@(��@(��@(�u@(�u@(�u@(�@(�@(�@(�@(�@(r�@(bN@(A�@(b@'�w@&��@&{@%�-@$��@#��@#�m@#ƨ@#�F@#�F@#��@#��@#��@#��@#��@#��@#t�@#t�@#C�@#S�@"��@"=q@!G�@ Ĝ@ Q�@   @��@�P@\)@\)@\)@\)@\)@\)@K�@;d@;d@+@+@+@�@�R@v�@E�@$�@@��@�@�@�j@z�@��@t�@o@�!@^5@=q@-@�@�^@�7@�7@7L@&�@%@�u@  @�@�P@�R@�+@�@I�@�F@�F@33@~�@M�@��@�`@�@r�@r�@r�@r�G�O�Aڝ�Aڟ�Aڝ�Aڝ�Aڛ�Aڟ�A�z�A�z�Aڏ\Aډ7AڅAډ7Aډ7Aڇ+AڋDAډ7AڅAځAڋDA�|�Aډ7AڋDA�p�A�t�A�|�A�x�A�|�AڃA�|�A�r�A�p�A�r�A�z�A�v�A�t�A�v�A�z�A�z�A�v�A�z�AځA�t�A�l�A�r�A�t�A�n�A�r�A�r�A�n�A�n�A�r�A�v�A�t�A�v�A�x�A�r�A�n�A�r�A�r�A�x�A�r�A�r�A�hsA�ffA�l�A�hsA�dZA�hsA�l�A�jA�hsA�p�A�v�A�n�A�l�A�n�A�l�A�hsA�jA�n�A�l�A�hsA�l�A�l�A�hsA�ffA�ZA�S�A�O�A�S�A�XA�S�A�O�A�Q�A�S�A�O�A�Q�A�VA�Q�A�O�A�Q�A�S�A�O�A�O�A�S�A�O�A�S�A�VA�Q�A�K�A�M�A�O�A�O�A�K�A�K�A�S�A�S�A�O�A�M�A�M�A�M�A�K�A�K�A�M�A�K�A�I�A�Q�A�G�A�G�A�I�A�K�A�G�A�I�A�M�A�G�A�I�A�M�A�G�A�E�A�E�A�5?A�+A�"�A��A��A�bA�%A���A�t�A�A�A��A���AؾwAة�A�hsA�VA�=qA�
=A���A�z�A��TA֗�A�\)A�I�A��A��A�1A��A��yA��HAվwAՉ7A�hsA�K�A��AԺ^Aԥ�AԓuA�n�A�K�A�5?A� �A�bA�  A��`A���A�ȴAӾwAӬAӝ�AӑhA�~�A�n�A�ffA�ZA�M�A�M�A�M�A�C�A�7LA�/A��A�
=A�  A�  A���A��A��mA��;A��
A�ȴA���AҴ9Aҩ�Aң�Aқ�AҋDA�~�A�r�A�dZA�M�A�7LA��A��A���A�ĜAѶFAї�A�z�A�hsA�dZA�O�A�7LA�1'A��A�A���A��/A�AЗ�A�jA�A�A�$�A�"�A��A��A�bA�
=A�  A��A��A���A���AϾwAϡ�A�x�A�XA�/A�bA�%A���A��yA��HA���A�~�A�A�A�+A�(�A��A��A�{A�oA�
=A�
=A�JA�%A�A�%A�%A�  A���A�A�  A���A���A���A���A���A��A��`A��/A���A͸RAͥ�A͓uA�n�A�O�A��A̰!A�jA�oA�1A��#A���A˲-A˧�A˛�AˑhAˉ7A˃A˃A˅A�|�A�x�A�x�A�t�A�p�A�jA�jA�jA�hsA�`BA�\)A�^5A�\)A�XA�S�A�XA�XA�XA�S�A�S�A�S�A�M�A�C�A�?}A�=qA�9XA�-A� �A�$�A�$�A��A��A��A��A�{A�JA�%A�  A���A��A��A��A��AʑhA�v�A�ffA�dZA�`BA�VA�XA�Q�A�I�A�E�A�G�A�G�A�A�A�?}A�A�A�=qA�9XA�9XA�;dA�;dA�5?A�-A�-A�/A�"�A��A��A��A�oA�VA�VA�VA�1A�A�%A�%A���A���A���A���A��A��yA��;A��A���A�ƨAɺ^Aɲ-Aɩ�Aɡ�Aɝ�Aɟ�Aɟ�Aɟ�Aɛ�Aɛ�Aɛ�Aɟ�Aɝ�Aə�Aɕ�Aɗ�Aɗ�Aɕ�Aɏ\AɑhAɓuAɏ\AɋDAɋDAɍPAɋDAɅAɅAɇ+Aɇ+AɃA�z�A�t�A�t�A�t�A�n�A�Q�A�A�A�+A��A�bA�  A��A��HA���AȮAȣ�Aȥ�Aȣ�Aȟ�Aț�Aȗ�Aȏ\Aȉ7AȁA�z�A�z�A�n�A�S�A�A�A�=qA�(�A�+A�"�A��A��A���A��A��A��A��A��HA��HA�ȴAǲ-Aǰ!Aǲ-Aǰ!Aǧ�Aǣ�Aǧ�Aǥ�Aǟ�AǕ�AǏ\AǓuAǓuAǏ\AǍPAǓuAǓuAǏ\AǋDAǍPAǍPAǏ\AǍPAǉ7Aǉ7AǍPAǏ\Aǉ7AǅAǁA�~�A�z�A�l�A�hsA�hsA�bNA�`BA�dZA�hsA�hsA�bNA�hsA�hsA�ffA�`BA�bNA�ffA�bNA�^5A�^5A�^5A�`BA�ZA�\)A�`BA�bNA�`BA�dZA�jA�p�A�t�A�~�AǅAǅAǃAǁAǃAǉ7Aǉ7AǋDAǕ�AǛ�Aǝ�Aǝ�Aǟ�Aǣ�AǛ�Aǡ�AǮAǰ!A���A�ƨA�AǾwAǼjAǼjAǼjAǰ!AǙ�AǏ\AǍPAǃA�v�A�`BA�ZA�Q�A�K�A�?}A�7LA�7LA�5?A�(�A�JA���A��yA��/A���A�AƲ-AƮAƧ�AƓuAƋDA�|�A�r�A�^5A�A�A� �A�bA�  A��A��HA���Ať�Aš�Aś�Aŕ�Aŕ�Aŗ�AœuAōPAŅA�`BA�S�A�I�A�;dA�+A��A�bA���A��A��/A���A�ƨA�ȴA���AĶFAđhAđhAė�Ać+A�O�A�(�A��A��Aú^A�x�A�ZA�M�A��A���A�ffA���A��#A��A��A��mA��mA��A��wA��-A���A�=qA��#A��DA�5?A�{A���A��A��A���A�A��9A��hA�Q�A�&�A�A��A��/A��
A���A��uA��uA��PA��DA��7A��PA��\A��hA��PA��PA��PA��DA��+A�~�A�t�A�`BA��A�S�A�%A�ȴA��A���A���A��hA��A�~�A�p�A�33A�(�A�JA���A��A��/A��9A�x�A�^5A�C�A�"�A�JA���A��TA��wA��!A���A��A�v�A�E�A���A��jA��7A�bNA�1A��A��A�\)A��TA��TA��wA��DA�hsA�C�A�
=A���A��#A���A���A��A��\A��A�t�A�l�A�bNA�S�A�A�A�/A�VA��;A���A���A�|�A�\)A�E�A��A�A��`A��wA���A��A�G�A�%A�A�1A�
=A�%A���A��A��TA��#A���A���A��A�VA��A��TA���A�XA��/A�M�A�ĜA�r�A�ZA�=qA�1A���A��!A���A�r�A�hsA�E�A�
=A��TA���A�jA�1'A���A��/A���A��jA���A�ffA�ZA�A�A�^5A�5?A��A�JA�  A���A��A��mA���A��^A���A�dZA�+A��A��
A�ƨA��jA��RA��9A���A��PA�p�A�S�A�E�A�-A�JA���A��mA��;A��/A��A���A���A��9A��DA�v�A�n�A�dZA�bNA�S�A�O�A�C�A��A���A��A�bNA�Q�A�;dA��A��A��
A���A�ȴA�ĜA���A���A�Q�A���A�Q�A�JA���A�A��\A�x�A�I�A�5?A�VA��^A��DA�jA�VA�I�A�7LA�bA���A���A�z�A�ffA�XA�=qA���A�-A���A�bA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
T�B
T�B
T�B
U2B
TaB
T�B
T�B
T�B
T,B
T�B
S�B
S�B
TaB
T,B
S�B
S�B
T�B
TaB
T�B
T�B
T,B
T,B
S[B
R�B
TaB
OvB
LdB
IB
M�B
VB
ZQB
[#B
]/B
_�B
`�B
^B
\�B
XB
U�B
R�B
QNB
M6B
LdB
K�B
M6B
M�B
P}B
Q�B
P�B
P}B
U�B
Z�B
`�B
a�B
b�B
f2B
iDB
poB
r|B
sB
v+B
�SB
�(B
��B
�B
��B
��B
ߤB
�BPB`�B�lB��B��B�NB�TB�B!�B/B8�B,qB.�BIBZ�BC�B=�B:�B#�B�B7B BhB�B�PB�B��B�B��B��B��B��B��B�B��B��B��B��B��B��B��B� BsMBa�BVmBI�B=qB0!B%zB%FB�BB�BB
�DB
�B
уB
��B
��B
��B
��B
��B
�B
�B
u%B
l�B
`B
Q�B
E9B
3hB
B
B
�B	��B	�B	��B	�B	��B	��B	��B	��B	ٴB	�zB	��B	�=B	��B	�4B	�!B	�B	�=B	v�B	ncB	l"B	bNB	]�B	TaB	M�B	D�B	0�B	%B	qB	�B	�B		�B	SB��B��B�rB�;B��B��B�B��B�BܒB�#BچB�mB�gB�B�,B�&B��B�}BбBѷB�HB��B�pB��BΥB�KB�)BŢBŢBBŢB�aB�gB�-B�#B��B��B�BȀBʌB�RB�B��B�^B�tB��BƨB�zB�)B�B��B�EB��B��B�zB�B��B��B�B��B�WB�BݘB��B�B�B��B��B�B�fB�KB�B�]B�"B�B�B��B��B��B�AB�B�B�B�`B��B�B��B��B��B��B	 4B	SB	�B	�B	MB	eB	B	IB	�B	#nB	&B	($B	)�B	,=B	.�B	1'B	3�B	5B	6FB	=B	?HB	@B	A�B	CaB	EmB	GzB	HKB	G�B	IB	MB	Q�B	Q�B	PHB	O�B	PB	S�B	R�B	T,B	S&B	Q�B	S&B	YB	X�B	X�B	X�B	XyB	X�B	Z�B	]dB	a�B	f�B	i�B	j�B	j�B	kQB	j�B	m�B	rGB	q�B	sB	sMB	t�B	w�B	y�B	{B	{�B	|�B	}�B	~�B	~�B	��B	�GB	��B	�_B	��B	��B	��B	�_B	�eB	��B	�=B	��B	�CB	�xB	�IB	�VB	��B	��B	��B	��B	�kB	�kB	��B	��B	�qB	�OB	�hB	��B	�3B	��B	�B	�tB	��B	�zB	�B	�LB	�LB	��B	�B	�RB	�XB	�6B	�UB	�EB	�RB	��B	�#B	��B	��B	��B	̘B	��B	��B	��B	�<B	��B	�HB	бB	��B	�NB	ѷB	�2B	�B	�9B	��B	֡B	�
B	��B	��B	�BB	�|B	�fB	�>B	��B	�KB	�B	�"B	�"B	�"B	�B	�B	��B	�B	��B	�AB	�oB	�vB	�B	�MB	�B	�+B	�`B	�`B	��B	��B	�xB	�B	�JB	�B	�B	�PB	�PB	��B	��B	�"B	��B	�cB	�.B
  B	��B
;B
�B
AB
uB
B
�B
B
�B
�B
MB
B
B
�B
�B
�B
�B
�B
�B
�B
	7B
	lB
	7B
	�B
B
�B
�B
�B
B
�B
�B
xB
�B
PB
�B
�B
�B
�B
�B
�B
�B
 B
 B
�B
:B
�B
�B
uB
�B
{B
�B
SB
�B
$B
_B
_B
�B
�B
�B
�B
�B
�B
�B
=B
	B
	B
	B
=B
	B
B
B
xB
�B
VB
�B
 �B
!bB
!�B
!bB
!bB
!�B
!�B
!�B
"4B
"�B
"�B
"�B
"�B
#�B
#�B
#nB
$�B
%�B
%�B
&LB
'�B
'�B
(�B
(�B
(�B
)_B
(�B
)*B
)_B
)�B
)_B
)_B
)�B
)�B
)�B
)_B
)_B
)_B
)_B
)�B
)�B
)�B
*�B
*�B
+B
+B
*�B
+B
+�B
,B
,B
,=B
,qB
-�B
.�B
/B
/�B
/OB
/�B
/�B
0!B
0!B
0�B
0�B
2-B
1�B
1�B
1�B
1�B
1�B
2aB
2aB
2�B
3�B
49B
5B
5B
5?B
5?B
4nB
5B
7B
7B
7B
7�B
7LB
7�B
7LB
7�B
7LB
7�B
7�B
7�B
7�B
8�B
8RB
8B
9�B
9�B
:�B
9�B
:*B
:�B
:�B
:�B
:�B
:�B
;0B
;�B
<6B
;�B
;�B
<6B
=<B
=<B
=qB
=qB
=�B
>BB
>wB
>�B
>wB
?HB
?�B
@OB
@�B
@�B
@OB
@�B
@�B
@�B
AUB
A�B
AUB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D3B
DgB
D�B
EB
D�B
E9B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
FB
F?B
F?B
FtB
FtB
FtB
GB
GB
GB
GB
GB
F�B
FtB
HB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J#B
J�B
J�B
JXB
J�B
J�B
J�B
J�B
J�B
K)B
K�B
K�B
K�B
L�B
LdB
M6B
MjB
M�B
NB
OB
N�B
N�B
N�B
OvB
P�B
QNB
QB
QB
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
U�B
VmB
VmB
VmB
VmB
V�B
V�B
V9B
VmB
XB
XyB
XEB
X�B
YKB
ZB
ZQB
[#B
Z�B
[#B
[WB
[#B
[WB
[�B
[WB
[#B
[�B
[WB
[�B
\)B
\)B
\]B
\�B
\�B
\�B
]�B
^B
]�B
^5B
^jB
^jB
^5B
^jB
_B
^�B
^�B
^�B
_B
_;B
_;B
_;B
_;B
_pB
_pB
`B
`vB
`�B
aB
`�B
aHB
a|B
a|B
a�B
c B
cTB
c�B
c�B
dZB
d�B
d�B
d�B
e`B
f�B
f2B
gmB
f�B
gB
gmB
gmB
g�B
gmB
g�B
h
B
h
B
h
B
h
B
h>B
h
B
h
B
h
B
h
B
h
B
h>B
h>B
h>B
h>B
h>B
h>B
hsB
h>B
h�B
h�B
h�B
h�B
iB
iB
iDB
iB
iDB
iB
iB
jB
jKB
jKB
jB
jB
j�B
j�B
j�B
j�B
j�B
j�B
jB
kB
kB
kB
kB
k�B
k�B
lWB
l�B
l�B
l�B
m)B
m)B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
o B
o�B
pB
qAB
qAB
q�B
q�B
q�B
q�B
q�B
rB
q�B
rB
q�B
q�B
rB
rGB
rGB
q�B
sMB
sB
tB
tTB
t�B
u%B
t�B
uZB
uZB
u�B
u�B
uZB
uZB
uZB
u�B
u�B
u�B
u�B
u�B
u�B
uZB
u�B
v+B
v`B
v`B
v`B
v�B
v�B
w�B
w�B
w�B
xlB
xlB
x�B
y	B
y>B
y>B
y>B
yrB
y�B
zDB
zDB
zxB
z�B
z�B
{B
{JB
z�B
{B
|B
{B
~]B
~]B
.B
~�B
� B
��B
��B
�oB
�B
�B
�B
�B
�B
�B
�B
Q�B
Q�B
U�B
V�B
UgB
TaB
^B
W�B
P�B
T�B
VB
S[B
S�B
VB
S�B
T�B
U�B
W
B
TaB
V�B
S[B
S�B
U�B
S�B
P�B
T�B
T�B
T�B
S�B
U2B
U�B
T�B
S[B
U2B
UgB
TaB
S[B
U�B
U�B
S�B
S�B
W�B
UgB
S&B
S[B
U2B
S�B
R�B
T�B
T�B
S�B
S&B
T�B
S�B
R B
UgB
T,B
R�B
S[B
T,B
U�B
S�B
U2B
T�B
R�B
S�B
T�B
T,B
S&B
T,B
TaB
T�B
S&B
U2B
T�B
S&B
S�B
UgB
T,B
R�B
S�B
T�B
S[B
S&B
T�B
T�B
XB
S�B
U�B
T�B
R�B
S�B
U�B
T�B
S�B
U�B
T�B
R�B
U2B
UgB
T,B
S�B
UgB
T�B
S[B
UgB
T,B
S[B
T�B
VmB
TaB
S[B
T,B
U�B
T�B
R�B
S[B
UgB
T�B
S�B
S�B
T�B
UgB
S&B
S�B
T�B
R�B
U2B
T�B
R�B
R�B
T�B
S&B
RTB
S�B
R�B
Q�B
R�B
S&B
Q�B
S[B
S�B
RTB
Q�B
P}B
R B
R B
WsB
XyB
T,B
T,B
[�B
M6B
P}B
U2B
M6B
K�B
OB
LdB
[�B
RTB
MB
K)B
HKB
H�B
EB
E9B
GEB
E�B
CaB
I�B
G�B
EmB
FtB
`B
J�B
J#B
K�B
R�B
N�B
NpB
QNB
QB
S[B
S�B
XB
VB
VB
YKB
ZQB
YB
ZQB
Y�B
Z�B
ZB
[#B
Y�B
X�B
[�B
Z�B
Z�B
Z�B
[�B
\�B
[#B
[�B
]dB
^B
]dB
\�B
_pB
_B
^jB
_pB
_�B
^jB
`BB
`�B
`�B
_B
`�B
b�B
bNB
e`B
_�B
]dB
^�B
^�B
^�B
^5B
Z�B
^�B
_�B
Z�B
]�B
^5B
Y�B
\)B
\�B
_pB
\)B
\�B
X�B
WsB
VmB
VmB
V�B
VmB
U�B
U2B
V�B
UgB
R�B
T�B
U�B
VB
VB
YB
TaB
QNB
OvB
PHB
PB
O�B
X�B
R�B
O�B
L0B
NB
N�B
M6B
L�B
N�B
M6B
K�B
M�B
NB
L�B
K�B
L�B
L�B
K)B
K�B
L�B
L�B
K^B
I�B
K^B
L0B
MB
J�B
K^B
MB
J#B
J#B
L0B
J#B
M�B
S�B
NB
OB
J�B
PHB
K�B
K�B
L�B
NpB
OB
PHB
QB
PHB
O�B
P}B
Q�B
P�B
P�B
Q�B
R�B
R�B
QB
P�B
Q�B
R B
PB
O�B
QNB
Q�B
P}B
OvB
O�B
P�B
QB
O�B
P�B
P�B
QNB
PHB
QNB
VmB
V�B
W�B
XyB
ZB
Z�B
YKB
YB
[WB
[WB
[WB
[�B
\�B
]�B
\�B
\)B
_�B
p�B
a�B
d&B
`BB
aHB
dZB
aHB
bNB
b�B
c B
b�B
aB
b�B
c�B
a�B
a�B
c�B
c B
b�B
a�B
c�B
e,B
dZB
b�B
e`B
e�B
e�B
e`B
gmB
h
B
gB
ffB
h>B
iB
g8B
f�B
h�B
iDB
iDB
h>B
h�B
kQB
l�B
lWB
m]B
n�B
p;B
qAB
q�B
qvB
r�B
rB
qAB
qAB
r�B
sMB
r�B
qAB
q�B
s�B
s�B
r�B
q�B
r|B
tTB
r�B
q�B
sB
tB
sMB
rGB
rB
t�B
tB
rGB
r|B
s�B
t�B
u%B
t�B
t�B
u�B
{JB
zB
~�B
}�B
.B
� B
�B
�B
��B
��B
�rB
�B
�lB
�rB
�xB
�DB
��B
��B
��B
�"B
�B
��B
��B
��B
��B
��B
��B
�B
�	B
��B
��B
�!B
��B
�\B
��B
��B
��B
�*B
��B
��B
�6B
��B
��B
�OB
��B
�B
��B
��B
�nB
�3B
��B
��B
��B
��B
��B
�nB
�?B
�B
�nB
�B
�9B
��B
�B
�9B
�9B
�B
�B
�0B
��B
�zB
�HB
��B
�)B
�mB
�DB
��B
�B
��B
�cB
�]B
��B
�cB
�B
�B
�B
�B
�B
�B
�B
��B
�oB
�B
�B
��B
�oB
�GB
�B
�>B�B�B-�B1'BDgBO�BP�BO�BR�BV9B]/BaBf2BlWBo�BtTBs�BsB{�B��B��B��B�B��B�"B�VB�(B�{B��B��B��B��B��B�_B��B��B��B�=B��B��B�=B��B��B�LB�$B��B�}B� B��B��B�'BĜB��B�mBŢB�EB̘B�TB�TBרBیB�vB�8B�>B�B�B�B� B�B�B�HB�HB�mB�`B�B�
B�B�B�B�B��B��B�fB��B�B �B
=B{B@B�B=B/�B(XB%zB+6B'�B&�B(�B&�B2-B;�B)_B4�BVB�B-�B9$B:�B=�BA B<jB:�BL0B<�B>BB7B0�B-CB-�B.B-�B,B+�B-B4B0�B/�B)�B%zB'�B-B+B)�B+�B-�B/B.�B/B1�B5B49B6�B8�B:�B<�B<�B<6B]dBc�Bg8Be�Be�B_B^�B\�B[WB[�BT,Ba�BP�BR BR�BMBI�BI�BB'BA�B8�B;dB;dB8�B=qB5B9�B6�B7�B1�BA�BH�BF�BFBB[BE�Bm)B<BA�B(�B#B)�B.B($B(XB(XB#nB$�B�B!�B"4B!�B!BOBqB�B�B�BOBVB($B�B�B$B~B+B�B=B�B�BB�BFBFB�B�B B.B�B.B�B�BhB.B�B�B�B{B�BMBBB�B�B  B�B�BB�B�B�	B�%B�B�B��B��B��B�ZB�B�|B�WB��B�5B�iB�B�|BDB�B�B�|B�|B��B�B�B�|B�MB�ZB��B�8B��B�B�oB�vB�/B�)B�;B�B�|B�B�B�B��B��B�;B� B�B�/B�B��B��B�5B��B�B�B�B�B��B�B�]B�lB�B�/B�mB�2B�>B�QB��B�B�|B�B��B�B��B�;B��B��B�QBߤB�B��B��B�9B��B�|B�NB�&B�pB͟B��BбB�BB˒B�^B�B�B�mB�pB�B�RB��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202007072154312020070721543120200707215431202007072154312020070721543120200707215431SI  SI  ARFMARFM                                                                                                                                                2020061223491020200612234910IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020062300003720200623000037QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020062300003720200623000037QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020070612390620200706123906IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20200707214337              CF      PSAL                            ?�  G�O�D�qG�O�?�  G�O�Bad PSAL Drift                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20200707214833    CF                  PSAL            G�O�?#�
G�O�CG�
G�O�?�                  Bad PSAL Drift  SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2020070721552020200707215520IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2020V01                                            CTD_for_DMQC_2020V01                                            2020070721552020200707215520IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2020070721552020200707215520IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                