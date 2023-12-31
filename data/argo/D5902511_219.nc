CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  ,   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-07-16T09:04:04Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     `  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  Vh   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     `  \�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  v    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     `  |x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     `  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` `   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 X '�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` .   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` Gx   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   G�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   M�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   S�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T Y�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   Z,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   Z4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   Z<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ZD   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ZL   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   Z�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   Z�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Z�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        [   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        [   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       [    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    [(Argo profile    3.1 1.2 19500101000000  20220716090404  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_219                 6810_008521_219                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��!�AJM@��!�AJM11  @��!�ᰊ@��!�ᰊ@0��+��@@0��+��@�d�g��t~�d�g��t~11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�?�@@  @z�H@�p�@�  @�p�@��RA��A\)A*=qA@  A`  A�  A�Q�A�Q�A�  A�Q�A���A�Q�A�A�\)B�
B  B  B�
B'�
B0  B8Q�B@(�BH(�BP  BW�
B`  Bh(�Bp  Bx  B�{B��
B��
B�  B�{B�{B�{B�{B�{B�{B��B�  B�(�B�{B�  B�  B�  B��
B�  B�{B��B��B�  B��B��B�  B�  B��B�  B�  B�  B��C 
=C
=C  C
=C
=C

=C��C
=C
=C
=C��C��C  C  C��C
=C   C"
=C#��C&  C(
=C*  C,  C.  C0{C2  C4  C6  C7��C:  C<  C>
=C?��CB  CC��CF  CH
=CJ  CK�CM�CO��CR  CT  CV  CW��CZ  C\  C^  C_��Ca��Cd  Cf
=Ch
=Ci��Ck��Cn{Cp{Cr  Ct
=Cu��Cw��Cy��C{�C~  C�C�
=C�C�
=C�
=C�C�  C�C�C���C���C�C�C���C���C���C���C�  C�\C�C�  C�  C���C�C�C���C�  C�  C���C���C�C�  C���C���C���C�  C�  C�  C�  C���C���C�  C���C�  C�C�C���C���C�  C���C�C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C�  C�C�C���C���C�  C�  C�C�
=C�C�C�  C���C�  C�  C���C���C���C�C�  C�C�
=C�C���C���C�C�C�C�C���C�  C�
=C�C���C���C�  C�  C���C���C���C�  C���C���C���C�  C�C���C�  C�  C�  C�  C���C�  C�C�  C�  C�C�  C�C�C�C�\C�
=C�C�C�  D   D � D  D}qD  D� D�qD}qD�qD��D�D}qD  D�D�D� D  D}qD	  D	� D
  D
� D�D� D  D� D��D}qD�D�D�qD}qD�D��D  D� D��D}qD�D��D  D� D  D}qD�qD� D�D� D  D�D�D� D�qD}qD�qD}qD  D�D�D��D  D� D  D� D   D z�D �qD!� D"  D"�D#D#��D$D$�D%�D%��D&  D&� D'  D'� D'�qD(}qD(��D)}qD*  D*��D+�D+��D,�D,}qD-  D-� D-�qD.}qD.�qD/z�D/�qD0}qD0�qD1� D2�D2}qD2�qD3}qD4  D4��D5  D5� D6�D6�D7�D7}qD7�qD8� D9�D9��D:  D:}qD:�qD;��D<D<� D<�qD=z�D=�qD>� D?�D?��D?�qD@}qDA  DA� DA�qDB}qDB�qDC� DD  DD� DE�DE�DF  DF��DG�DG}qDH  DH��DI  DI}qDI��DJ}qDK�DK��DL  DL� DM  DM��DNDN� DO  DO� DO�qDP� DP�qDQ� DR�DR��DS  DS��DT�DT��DU�DU� DV  DV� DV��DW}qDX�DX� DY  DY��DZ�DZ� DZ�qD[}qD\  D\}qD\�qD]� D^  D^� D_�D_� D`  D`��Da  Da� Da�qDb}qDc�Dc��Dc�qDdz�De  De�Df  Df� DgDg� Dh  Dh}qDh��Di� Dj  Dj}qDk�Dk��Dl�Dl��Dl�qDm}qDn�Dn�Do  Do}qDo�qDp� Dq  Dq� Dq�qDrz�Dr�qDs� Ds�qDt� Dt�qDuz�Dv  Dv��Dw  Dw� Dw�qDx}qDyDy� Dy�qDz� D{D{��D|  D|��D}  D}}qD}�qD~� D�D��D�  D�>�D�� D�� D���D�@ D�� D��HD�  D�AHD�� D�� D�  D�>�D�� D��HD�HD�AHD�~�D��qD��qD�@ D��HD�� D�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�>�D�� D���D���D�@ D�~�D���D���D�AHD���D��HD���D�=qD�}qD�� D��D�G�D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�� D�  D�@ D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?#�
?aG�?��R?\?�G�@   @�R@+�@8Q�@Tz�@fff@s33@��@��@���@�  @��@�@��R@���@�33@��H@��
@��@���AG�AffA��A�AffA��A"�\A(Q�A,��A4z�A:=qA>�RAEAL��AQ�AW�A_\)AeAj=qAq�AxQ�A~{A��A�p�A���A��A�ffA��A�p�A��A��HA�ffA���A��A�
=A�=qA��A��A��\A�{A�G�A��
A�ffA���A��AǮAʏ\A�{A�G�A�(�A�ffAٙ�A��A߮A��A���A�Q�A��HA��A�Q�A��
A�{A���A�(�A�\)B ��B=qB  B�B�\BQ�B	��B
�HBz�B=qB�
B�B�RB��B{B\)B�B�HB(�B��B�B!G�B"�HB$(�B%B'�B(��B*ffB,(�B-B/\)B0��B2{B3�
B5��B6�HB8  B9�B;\)B<Q�B=B?\)B@Q�BA��BC
=BDz�BEp�BF�\BH  BI�BI�BK
=BLz�BMp�BN=qBO\)BPz�BQ�BR{BS
=BTQ�BT��BUBV�HBW�BXQ�BYG�BZffB[33B\(�B\��B]��B^�RB_�B`(�BaG�Bb=qBc\)Bc�
Bd��Be�Bf�HBg�Bhz�Bi��Bj�\Bk33Bl  BmG�Bn{Bn�RBo�Bp��Bq��Br{Bs33Bt(�Bt��BuBv=qBw\)Bxz�Bx��By��Bz�RB{�B|  B|��B}�B~�RB�B�  B�Q�B��RB�33B�p�B��B�(�B��\B��RB�
=B��B��B�=qB�z�B���B�\)B��
B�=qB��\B��HB�G�B��
B�=qB�z�B��HB�\)B��B�Q�B��\B���B�p�B�  B�=qB��\B��B��B��B�=qB��RB�33B���B��
B�Q�B���B�G�B���B��
B�Q�B���B�33B��B��
B�Q�B���B�33B��B��
B�Q�B���B�33B�p�B�B�=qB��RB�33B��B�B�{B��\B�
=B�p�B��B�  B�Q�B��HB�G�B�B�  B�Q�B��RB�33B�p�B��B�(�B���B�
=B�\)B���B��B�z�B���B�33B��B��B�ffB��HB�33B�\)B�B�Q�B���B���B�33B�B�(�B�ffB���B�
=B�p�B��
B�(�B�ffB���B�\)B��B��
B�=qB��RB��B�\)B��B�  B�ffB���B�\)B���B��B�=qB��RB��B�\)B��B�(�B��\B���B�G�B���B��
B�ffB���B��B�p�B�B�{B���B�
=B�\)B��B�  B�z�B�
=B�\)B��B�  B�ffB��HB�\)B�B�{B�ffB��HB�p�B��B�{B�ffB��HB��B��
B�(�B��\B�
=B��B��B�=qB\B���BÅB�  B�Q�Bģ�B���B�p�B�  B�ffB��HB�33BǅB��B�z�B�
=B�p�B�B�(�Bʣ�B�33B˙�B��
B�=qB̸RB�G�B�B�(�B�ffB��HB�\)B��
B�=qBУ�B�
=Bљ�B�(�B�z�B���B�G�B�B�Q�B���B��B�p�B�  B�z�B�
=B�\)B�B�=qBظRB�G�B�B�{B�z�B��Bۙ�B�  B�Q�BܸRB�33B�B�Q�B���B��B�p�B��B�z�B�
=B�p�B��
B�=qB�RB�G�B��
B�ffB�RB��B�B�=qB�RB�
=B�p�B�  B�z�B�
=B�p�B��
B�=qB�RB�G�B��
B�=qB��B�
=B홚B�(�B��B��B�p�B��
B�z�B�
=B�B��B�=qB�RB�G�B��
B�Q�B���B��B��B�{B��\B�
=B��B��B�=qB��\B�33B��B�(�B�ffB���B�G�B�B�(�B���B�33B��B�(�B�z�B��HB�G�B��C �C Q�C z�C �C �
C{CQ�C�\CC��C{CG�C�CC��C�CG�Cz�C�RC��C=qCffC�\CC��C33Cp�C�C�HC{CG�Cp�C��C�
C{C\)C�\CC��C(�C\)C��C�
C	{C	Q�C	p�C	��C	�
C
{C
G�C
�C
�C
�
C  C=qCz�C�RC�C{CG�Cp�C��C�C(�CffC�C�RC�C(�CffC��C�
C  C33C\)C��C�
C{CG�C�C�RC�HC{CG�Cz�C�RC��C=qCp�C�RC�
C  C33Cp�C�RC��C33Cz�CC  C33CffC��C�
C
=CG�C�C��C
=CQ�C�CC  C33CffC�\C��C
=CG�C�C��C
=CG�C�CC��C(�C\)C��C�
C
=C\)C��C�
C{CQ�C�\C��C
=CG�C�CC��C33Cp�C�C�C 33C p�C �C �C!(�C!p�C!�RC!��C"33C"p�C"�C"�HC#{C#G�C#�C#C$
=C$=qC$�C$�RC%  C%=qC%z�C%�RC&  C&=qC&z�C&�RC&��C'=qC'p�C'�RC'��C(33C(z�C(C)  C)G�C)z�C)�RC*
=C*G�C*�C*��C+{C+Q�C+�\C+��C,
=C,G�C,�C,��C-
=C-G�C-�C-��C.{C.Q�C.��C.�HC/�C/ffC/�C/�C033C0z�C0�RC1  C1=qC1�C1C2  C2G�C2�C2C3
=C3Q�C3�\C3�
C4�C4ffC4�C4��C5=qC5z�C5C6
=C6Q�C6�C6��C7  C7G�C7�C7C8  C8G�C8�C8��C9
=C9Q�C9��C9�
C:�C:ffC:��C:�HC;(�C;ffC;�RC<  C<G�C<�\C<�
C=�C=p�C=�RC>  C>G�C>�\C>C?
=C?G�C?�\C?�
C@(�C@z�C@CA
=CA\)CA��CA��CB=qCB�CB��CC
=CC=qCC�\CC�HCD33CDp�CDCE
=CE\)CE��CE�
CF{CF\)CF�CF��CGG�CG��CG�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?k�?�@@  @z�H@�p�@�  @�p�@��RA��A\)A*=qA@  A`  A�  A�Q�A�Q�A�  A�Q�A���A�Q�A�A�\)B�
B  B  B�
B'�
B0  B8Q�B@(�BH(�BP  BW�
B`  Bh(�Bp  Bx  B�{B��
B��
B�  B�{B�{B�{B�{B�{B�{B��B�  B�(�B�{B�  B�  B�  B��
B�  B�{B��B��B�  B��B��B�  B�  B��B�  B�  B�  B��C 
=C
=C  C
=C
=C

=C��C
=C
=C
=C��C��C  C  C��C
=C   C"
=C#��C&  C(
=C*  C,  C.  C0{C2  C4  C6  C7��C:  C<  C>
=C?��CB  CC��CF  CH
=CJ  CK�CM�CO��CR  CT  CV  CW��CZ  C\  C^  C_��Ca��Cd  Cf
=Ch
=Ci��Ck��Cn{Cp{Cr  Ct
=Cu��Cw��Cy��C{�C~  C�C�
=C�C�
=C�
=C�C�  C�C�C���C���C�C�C���C���C���C���C�  C�\C�C�  C�  C���C�C�C���C�  C�  C���C���C�C�  C���C���C���C�  C�  C�  C�  C���C���C�  C���C�  C�C�C���C���C�  C���C�C�  C�  C�  C�  C�C�C�  C�  C�  C���C���C�  C�C�C���C���C�  C�  C�C�
=C�C�C�  C���C�  C�  C���C���C���C�C�  C�C�
=C�C���C���C�C�C�C�C���C�  C�
=C�C���C���C�  C�  C���C���C���C�  C���C���C���C�  C�C���C�  C�  C�  C�  C���C�  C�C�  C�  C�C�  C�C�C�C�\C�
=C�C�C�  D   D � D  D}qD  D� D�qD}qD�qD��D�D}qD  D�D�D� D  D}qD	  D	� D
  D
� D�D� D  D� D��D}qD�D�D�qD}qD�D��D  D� D��D}qD�D��D  D� D  D}qD�qD� D�D� D  D�D�D� D�qD}qD�qD}qD  D�D�D��D  D� D  D� D   D z�D �qD!� D"  D"�D#D#��D$D$�D%�D%��D&  D&� D'  D'� D'�qD(}qD(��D)}qD*  D*��D+�D+��D,�D,}qD-  D-� D-�qD.}qD.�qD/z�D/�qD0}qD0�qD1� D2�D2}qD2�qD3}qD4  D4��D5  D5� D6�D6�D7�D7}qD7�qD8� D9�D9��D:  D:}qD:�qD;��D<D<� D<�qD=z�D=�qD>� D?�D?��D?�qD@}qDA  DA� DA�qDB}qDB�qDC� DD  DD� DE�DE�DF  DF��DG�DG}qDH  DH��DI  DI}qDI��DJ}qDK�DK��DL  DL� DM  DM��DNDN� DO  DO� DO�qDP� DP�qDQ� DR�DR��DS  DS��DT�DT��DU�DU� DV  DV� DV��DW}qDX�DX� DY  DY��DZ�DZ� DZ�qD[}qD\  D\}qD\�qD]� D^  D^� D_�D_� D`  D`��Da  Da� Da�qDb}qDc�Dc��Dc�qDdz�De  De�Df  Df� DgDg� Dh  Dh}qDh��Di� Dj  Dj}qDk�Dk��Dl�Dl��Dl�qDm}qDn�Dn�Do  Do}qDo�qDp� Dq  Dq� Dq�qDrz�Dr�qDs� Ds�qDt� Dt�qDuz�Dv  Dv��Dw  Dw� Dw�qDx}qDyDy� Dy�qDz� D{D{��D|  D|��D}  D}}qD}�qD~� D�D��D�  D�>�D�� D�� D���D�@ D�� D��HD�  D�AHD�� D�� D�  D�>�D�� D��HD�HD�AHD�~�D��qD��qD�@ D��HD�� D�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�>�D�� D���D���D�@ D�~�D���D���D�AHD���D��HD���D�=qD�}qD�� D��D�G�D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�� D�  D�@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?#�
?aG�?��R?\?�G�@   @�R@+�@8Q�@Tz�@fff@s33@��@��@���@�  @��@�@��R@���@�33@��H@��
@��@���AG�AffA��A�AffA��A"�\A(Q�A,��A4z�A:=qA>�RAEAL��AQ�AW�A_\)AeAj=qAq�AxQ�A~{A��A�p�A���A��A�ffA��A�p�A��A��HA�ffA���A��A�
=A�=qA��A��A��\A�{A�G�A��
A�ffA���A��AǮAʏ\A�{A�G�A�(�A�ffAٙ�A��A߮A��A���A�Q�A��HA��A�Q�A��
A�{A���A�(�A�\)B ��B=qB  B�B�\BQ�B	��B
�HBz�B=qB�
B�B�RB��B{B\)B�B�HB(�B��B�B!G�B"�HB$(�B%B'�B(��B*ffB,(�B-B/\)B0��B2{B3�
B5��B6�HB8  B9�B;\)B<Q�B=B?\)B@Q�BA��BC
=BDz�BEp�BF�\BH  BI�BI�BK
=BLz�BMp�BN=qBO\)BPz�BQ�BR{BS
=BTQ�BT��BUBV�HBW�BXQ�BYG�BZffB[33B\(�B\��B]��B^�RB_�B`(�BaG�Bb=qBc\)Bc�
Bd��Be�Bf�HBg�Bhz�Bi��Bj�\Bk33Bl  BmG�Bn{Bn�RBo�Bp��Bq��Br{Bs33Bt(�Bt��BuBv=qBw\)Bxz�Bx��By��Bz�RB{�B|  B|��B}�B~�RB�B�  B�Q�B��RB�33B�p�B��B�(�B��\B��RB�
=B��B��B�=qB�z�B���B�\)B��
B�=qB��\B��HB�G�B��
B�=qB�z�B��HB�\)B��B�Q�B��\B���B�p�B�  B�=qB��\B��B��B��B�=qB��RB�33B���B��
B�Q�B���B�G�B���B��
B�Q�B���B�33B��B��
B�Q�B���B�33B��B��
B�Q�B���B�33B�p�B�B�=qB��RB�33B��B�B�{B��\B�
=B�p�B��B�  B�Q�B��HB�G�B�B�  B�Q�B��RB�33B�p�B��B�(�B���B�
=B�\)B���B��B�z�B���B�33B��B��B�ffB��HB�33B�\)B�B�Q�B���B���B�33B�B�(�B�ffB���B�
=B�p�B��
B�(�B�ffB���B�\)B��B��
B�=qB��RB��B�\)B��B�  B�ffB���B�\)B���B��B�=qB��RB��B�\)B��B�(�B��\B���B�G�B���B��
B�ffB���B��B�p�B�B�{B���B�
=B�\)B��B�  B�z�B�
=B�\)B��B�  B�ffB��HB�\)B�B�{B�ffB��HB�p�B��B�{B�ffB��HB��B��
B�(�B��\B�
=B��B��B�=qB\B���BÅB�  B�Q�Bģ�B���B�p�B�  B�ffB��HB�33BǅB��B�z�B�
=B�p�B�B�(�Bʣ�B�33B˙�B��
B�=qB̸RB�G�B�B�(�B�ffB��HB�\)B��
B�=qBУ�B�
=Bљ�B�(�B�z�B���B�G�B�B�Q�B���B��B�p�B�  B�z�B�
=B�\)B�B�=qBظRB�G�B�B�{B�z�B��Bۙ�B�  B�Q�BܸRB�33B�B�Q�B���B��B�p�B��B�z�B�
=B�p�B��
B�=qB�RB�G�B��
B�ffB�RB��B�B�=qB�RB�
=B�p�B�  B�z�B�
=B�p�B��
B�=qB�RB�G�B��
B�=qB��B�
=B홚B�(�B��B��B�p�B��
B�z�B�
=B�B��B�=qB�RB�G�B��
B�Q�B���B��B��B�{B��\B�
=B��B��B�=qB��\B�33B��B�(�B�ffB���B�G�B�B�(�B���B�33B��B�(�B�z�B��HB�G�B��C �C Q�C z�C �C �
C{CQ�C�\CC��C{CG�C�CC��C�CG�Cz�C�RC��C=qCffC�\CC��C33Cp�C�C�HC{CG�Cp�C��C�
C{C\)C�\CC��C(�C\)C��C�
C	{C	Q�C	p�C	��C	�
C
{C
G�C
�C
�C
�
C  C=qCz�C�RC�C{CG�Cp�C��C�C(�CffC�C�RC�C(�CffC��C�
C  C33C\)C��C�
C{CG�C�C�RC�HC{CG�Cz�C�RC��C=qCp�C�RC�
C  C33Cp�C�RC��C33Cz�CC  C33CffC��C�
C
=CG�C�C��C
=CQ�C�CC  C33CffC�\C��C
=CG�C�C��C
=CG�C�CC��C(�C\)C��C�
C
=C\)C��C�
C{CQ�C�\C��C
=CG�C�CC��C33Cp�C�C�C 33C p�C �C �C!(�C!p�C!�RC!��C"33C"p�C"�C"�HC#{C#G�C#�C#C$
=C$=qC$�C$�RC%  C%=qC%z�C%�RC&  C&=qC&z�C&�RC&��C'=qC'p�C'�RC'��C(33C(z�C(C)  C)G�C)z�C)�RC*
=C*G�C*�C*��C+{C+Q�C+�\C+��C,
=C,G�C,�C,��C-
=C-G�C-�C-��C.{C.Q�C.��C.�HC/�C/ffC/�C/�C033C0z�C0�RC1  C1=qC1�C1C2  C2G�C2�C2C3
=C3Q�C3�\C3�
C4�C4ffC4�C4��C5=qC5z�C5C6
=C6Q�C6�C6��C7  C7G�C7�C7C8  C8G�C8�C8��C9
=C9Q�C9��C9�
C:�C:ffC:��C:�HC;(�C;ffC;�RC<  C<G�C<�\C<�
C=�C=p�C=�RC>  C>G�C>�\C>C?
=C?G�C?�\C?�
C@(�C@z�C@CA
=CA\)CA��CA��CB=qCB�CB��CC
=CC=qCC�\CC�HCD33CDp�CDCE
=CE\)CE��CE�
CF{CF\)CF�CF��CGG�CG��CG�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A���A���A���A���A���A���A���A���A��
A��HA��;A��HA��TA��`A��`A��`A��mA��mA��mA��yA��yA��mA��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��AӬA���A��A�E�A��yAθRAΛ�AΡ�A�Q�A͏\A̕�A�bAˍPAʸRA�Q�A�XA�;dA���A�A�z�A�1A�jA���A���A�A�A��A��A�t�A��RA��A�$�A��\A���A�ffA�33A��wA��A�bA�S�A���A�ƨA�A�33A���A��uA�A�A��
A�p�A���A�E�A�ĜA�+A��mA�VA���A��A�A���A���A�O�A���A��A�{A��A��A�C�A�ƨA��A���A�$�A���A��mA�ƨA�VA{��Ay��Ax^5Aw%Ap��AnVAm�^Al�/Ai|�Ae��AcK�A`�RA_AZ�AX�!AW|�AVjASS�AR �AO"�AM"�AJ  AGoAE�TAE��AE/AD$�ABbA?t�A=�FA<��A<r�A<9XA;�^A:��A:n�A8�yA8I�A7?}A6�uA5�;A5�A3t�A2��A1��A05?A/7LA-\)A+\)A)|�A(z�A'S�A%�A#33A��A�uAZA5?A1A��A=qAz�AA�jA�A�A��A`BA��A{A�^A��A�PA�/A��A�`AƨA�A��A"�AK�A��A
�\A
=qA
A�yAt�A�uA��Ax�AAVA��A�7A�A�DA^5AE�AdZA I�@���@�;d@�
=@�ff@�V@�M�@�J@���@�ƨ@��@���@�p�@��`@��u@��@��@��!@�M�@�E�@�hs@���@�(�@�;d@�n�@�R@��/@�bN@�@�-@�D@�dZ@��y@�ȴ@��@�t�@���@�G�@�7@�7L@�ƨ@�ȴ@�7L@܋D@�A�@�A�@܋D@�Z@�(�@�dZ@�J@؛�@ו�@��y@�E�@ա�@ԋD@�o@��@���@��@��m@υ@�o@���@�^5@͉7@��/@˅@�@�v�@ț�@Ǖ�@�"�@Ƨ�@�M�@�@��/@�9X@�bN@�|�@�C�@���@�ff@�@�@�{@�5?@���@���@�9X@��@�l�@�\)@��H@�-@���@��^@���@���@��T@���@�p�@�%@��u@��u@���@���@�7L@��7@���@���@���@��@�j@�"�@�I�@�@��@�S�@��!@�;d@��R@�-@���@�%@��@��P@��P@�;d@�$�@�G�@���@���@���@�%@��@��F@�+@��R@��+@�^5@��\@���@��@�dZ@�l�@�S�@���@�E�@�M�@�@��/@���@��/@�&�@�7L@�X@�%@���@�z�@�  @�|�@�S�@��@�;d@�;d@��y@�ff@��-@��@��u@��;@�E�@���@���@�x�@�@�^5@�p�@��/@��@��@�%@�9X@�
=@�M�@�/@�?}@�x�@�G�@�%@���@��@�I�@��
@���@�S�@�C�@��@�^5@�J@���@���@�X@�X@���@��@�  @�|�@�@�~�@�ff@�ff@�^5@�M�@�-@�{@�{@�{@��@��T@���@��^@���@�p�@��u@�  @�t�@���@���@���@��+@�ff@�-@��@���@���@�I�@�9X@�9X@�9X@�(�@�  @�S�@�~�@�E�@�J@���@���@��7@�x�@�O�@��@��u@�bN@��
@��y@�V@�$�@�J@��@��T@��T@���@��@�hs@�V@���@��u@��@�z�@�r�@�Z@�A�@���@��m@��
@���@��@�E�@�$�@�@���@��@���@���@�Ĝ@��9@���@��u@��@� �@��@��P@�l�@�"�@��H@�~�@�E�@��@��T@���@�`B@���@�Ĝ@�j@�(�@��w@��@�K�@�33@��@�
=@��@�ȴ@���@�n�@�M�@��@�@���@��@�?}@�/@���@��j@�j@�A�@�1'@� �@� �@�1@��
@�"�@�@��!@�^5@�-@�$�@�{@�@��#@���@�@��^@���@�?}@��`@���@�Ĝ@���@�j@�9X@�b@��@+@~ȴ@~��@~V@}�@}p�@|�j@|(�@{ƨ@z�@y�#@y��@yx�@yG�@x��@xr�@w�;@w|�@v�@vff@u��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A��#A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ĜA���A��A���A���A���A���A���A���A���A��
A��/A��HA��TA��HA��;A��TA��HA��;A��HA��`A��TA��HA��`A��`A��HA��TA��mA��TA��HA��`A��mA��TA��TA��mA��`A��TA��`A��yA��`A��TA��`A��yA��mA��`A��TA��mA��yA��mA��`A��yA��A��yA��`A��mA��A��yA��`A��mA��yA��yA��mA��mA��A��yA��`A��mA��A��mA��TA��mA��yA��`A��mA��A��mA��mA��yA��mA��TA��mA��yA��A��mA��yA��A��A��yA��A��A��A��yA��A��A��A��yA��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��`A��yA��A��yA��`A��yA��A��A��yA��yA��yA��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A���A���A��AӓuA�M�A��A��/A�1A�hsA�  A��#A�AжFAЮAХ�AЩ�AЧ�AЗ�AЃA�dZA�\)A�M�A��A϶FAύPAω7A�x�A�r�A�p�A�jA�ZA�Q�A�C�A�9XA��A�
=A���A���A��A��A��yA��mA��yA��yA��HA��#A���A���A���A���A�ȴA�AξwAΩ�AΛ�AΗ�AΓuAΓuAΗ�AΛ�AΛ�AΙ�AΗ�AΙ�AΛ�AΟ�AΟ�AΝ�AΛ�AΟ�AΣ�AΡ�AΟ�AΡ�AΥ�AΥ�AΥ�AΡ�AΛ�A΍PA�|�A�|�A�v�A�bNA�G�A�7LA�"�A�1A�A�A��`A͸RAͣ�A͗�A͇+A�v�A�dZA�M�A�?}A�1'A� �A�VA���A���A��`A̍PA�?}A�7LA�1'A�+A�(�A�&�A�$�A�$�A�$�A� �A��A�{A�VA�A��A��;A���A���A���A�A˸RA˩�A˛�A˃A�O�A�(�A��A�oA���A��
A�ĜAʶFAʾwAʴ9Aʗ�AʅA�n�A�\)A�\)A�^5A�^5A�S�A�E�A�?}A�A�A�M�A�ZA�^5A�`BA�`BA�dZA�ZA�S�A�O�A�Q�A�I�A�E�A�A�A�=qA�;dA�=qA�=qA�;dA�7LA�5?A�7LA�9XA�9XA�33A�&�A�VA��Aɧ�AɑhA�I�A���A���A�A�ĜA�ĜA���A���A�A�AȺ^Aȧ�Aȝ�Aȕ�AȍPAȇ+A�z�A�dZA�VA�C�A�-A��A�oA�bA�JA�JA�  A��A��A��yA��TAǲ-AǋDA�ZA�I�A�=qA�oA���A��A��A��A��A��A��A���AƍPA�VA���AŁA�\)A�&�Aě�A�M�A�=qA��A���Aú^AÅA�\)A�C�A�"�A�VA���A���A���A���A��A��A��A��A��A��`A��/A²-A�A�~�A�M�A�JA���A��\A�ZA�-A�ZA��A�bA�%A�%A�1A�%A�A�A�A�A�  A��`A��7A�C�A��A��A��HA��HA���A���A�G�A�5?A�1'A�{A��
A���A�x�A�p�A�`BA�-A��^A��9A�hsA�1'A��A���A��!A���A��hA�t�A�Q�A�9XA��A��A��;A���A��A���A���A���A��PA�x�A�XA�$�A���A��;A�ĜA��A�p�A�;dA���A�M�A�+A�JA��TA���A�^5A��A���A�A���A�r�A���A��
A��9A��mA���A�^5A��A��A��/A��FA�`BA���A��PA�^5A�;dA�/A�+A��A�A��A��;A���A��A�hsA�A�A��`A�G�A��A��
A���A��wA��FA��A���A�|�A�$�A��!A���A�ffA��A��9A�ZA�=qA�%A���A�r�A�C�A���A��jA���A�n�A�=qA�%A���A���A��A�K�A��A��A�/A��A�
=A��yA��9A��A�ZA�O�A�Q�A�O�A�C�A�7LA�"�A�bA�1A���A��;A���A���A�A���A�r�A�33A�  A���A�l�A�ȴA��A�n�A�S�A�=qA�1A���A�-A���A�v�A�C�A�{A��HA��FA��DA�bNA�VA�K�A�=qA��A��yA��-A�+A� �A���A��A��hA�|�A�^5A�5?A��A�
=A���A���A��\A��\A��DA��A��A�x�A�t�A�l�A�hsA�XA�?}A�9XA�$�A�VA���A�n�A�&�A�VA��!A��+A�Q�A�VA��A�ȴA���A�dZA�+A�{A��;A��9A�XA�-A�oA���A��/A���A���A��RA��DA�\)A�7LA� �A�JA��A�ȴA���A�l�A�1'A���A�ȴA���A��DA��A��A��A��+A���A���A��DA�~�A�ffA�XA�?}A�  A�ĜA���A���A��7A�v�A�M�A�VA��yA���A���A�S�A�-A��A��A���A��^A���A��PA�~�A�r�A�C�A��A��A��^A�`BA��yA���A�$�A��A��jA��PA�E�A�
=A�A���A��DA�l�A�I�A�C�A�-A��yA�r�A�7LA��A���A�n�A��mA��RA�v�A�^5A�I�A�"�A���A��mA��;A��#A��wA��!A���A��uA�v�A�v�A�v�A�jA�ZA�VA�Q�A�I�A�A�A�9XA�+A�
=A��A��`A��/A���A��-A��hA�XA�33A�(�A��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A��
A��HA��;A��HA��TA��`A��`A��`A��mA��mA��mA��yA��yA��mA��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��AӬA���A��A�E�A��yAθRAΛ�AΡ�A�Q�A͏\A̕�A�bAˍPAʸRA�Q�A�XA�;dA���A�A�z�A�1A�jA���A���A�A�A��A��A�t�A��RA��A�$�A��\A���A�ffA�33A��wA��A�bA�S�A���A�ƨA�A�33A���A��uA�A�A��
A�p�A���A�E�A�ĜA�+A��mA�VA���A��A�A���A���A�O�A���A��A�{A��A��A�C�A�ƨA��A���A�$�A���A��mA�ƨA�VA{��Ay��Ax^5Aw%Ap��AnVAm�^Al�/Ai|�Ae��AcK�A`�RA_AZ�AX�!AW|�AVjASS�AR �AO"�AM"�AJ  AGoAE�TAE��AE/AD$�ABbA?t�A=�FA<��A<r�A<9XA;�^A:��A:n�A8�yA8I�A7?}A6�uA5�;A5�A3t�A2��A1��A05?A/7LA-\)A+\)A)|�A(z�A'S�A%�A#33A��A�uAZA5?A1A��A=qAz�AA�jA�A�A��A`BA��A{A�^A��A�PA�/A��A�`AƨA�A��A"�AK�A��A
�\A
=qA
A�yAt�A�uA��Ax�AAVA��A�7A�A�DA^5AE�AdZA I�@���@�;d@�
=@�ff@�V@�M�@�J@���@�ƨ@��@���@�p�@��`@��u@��@��@��!@�M�@�E�@�hs@���@�(�@�;d@�n�@�R@��/@�bN@�@�-@�D@�dZ@��y@�ȴ@��@�t�@���@�G�@�7@�7L@�ƨ@�ȴ@�7L@܋D@�A�@�A�@܋D@�Z@�(�@�dZ@�J@؛�@ו�@��y@�E�@ա�@ԋD@�o@��@���@��@��m@υ@�o@���@�^5@͉7@��/@˅@�@�v�@ț�@Ǖ�@�"�@Ƨ�@�M�@�@��/@�9X@�bN@�|�@�C�@���@�ff@�@�@�{@�5?@���@���@�9X@��@�l�@�\)@��H@�-@���@��^@���@���@��T@���@�p�@�%@��u@��u@���@���@�7L@��7@���@���@���@��@�j@�"�@�I�@�@��@�S�@��!@�;d@��R@�-@���@�%@��@��P@��P@�;d@�$�@�G�@���@���@���@�%@��@��F@�+@��R@��+@�^5@��\@���@��@�dZ@�l�@�S�@���@�E�@�M�@�@��/@���@��/@�&�@�7L@�X@�%@���@�z�@�  @�|�@�S�@��@�;d@�;d@��y@�ff@��-@��@��u@��;@�E�@���@���@�x�@�@�^5@�p�@��/@��@��@�%@�9X@�
=@�M�@�/@�?}@�x�@�G�@�%@���@��@�I�@��
@���@�S�@�C�@��@�^5@�J@���@���@�X@�X@���@��@�  @�|�@�@�~�@�ff@�ff@�^5@�M�@�-@�{@�{@�{@��@��T@���@��^@���@�p�@��u@�  @�t�@���@���@���@��+@�ff@�-@��@���@���@�I�@�9X@�9X@�9X@�(�@�  @�S�@�~�@�E�@�J@���@���@��7@�x�@�O�@��@��u@�bN@��
@��y@�V@�$�@�J@��@��T@��T@���@��@�hs@�V@���@��u@��@�z�@�r�@�Z@�A�@���@��m@��
@���@��@�E�@�$�@�@���@��@���@���@�Ĝ@��9@���@��u@��@� �@��@��P@�l�@�"�@��H@�~�@�E�@��@��T@���@�`B@���@�Ĝ@�j@�(�@��w@��@�K�@�33@��@�
=@��@�ȴ@���@�n�@�M�@��@�@���@��@�?}@�/@���@��j@�j@�A�@�1'@� �@� �@�1@��
@�"�@�@��!@�^5@�-@�$�@�{@�@��#@���@�@��^@���@�?}@��`@���@�Ĝ@���@�j@�9X@�b@��@+@~ȴ@~��@~V@}�@}p�@|�j@|(�@{ƨ@z�@y�#@y��@yx�@yG�@x��@xr�@w�;@w|�@v�@vffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A��#A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ĜA���A��A���A���A���A���A���A���A���A��
A��/A��HA��TA��HA��;A��TA��HA��;A��HA��`A��TA��HA��`A��`A��HA��TA��mA��TA��HA��`A��mA��TA��TA��mA��`A��TA��`A��yA��`A��TA��`A��yA��mA��`A��TA��mA��yA��mA��`A��yA��A��yA��`A��mA��A��yA��`A��mA��yA��yA��mA��mA��A��yA��`A��mA��A��mA��TA��mA��yA��`A��mA��A��mA��mA��yA��mA��TA��mA��yA��A��mA��yA��A��A��yA��A��A��A��yA��A��A��A��yA��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��`A��yA��A��yA��`A��yA��A��A��yA��yA��yA��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A���A���A��AӓuA�M�A��A��/A�1A�hsA�  A��#A�AжFAЮAХ�AЩ�AЧ�AЗ�AЃA�dZA�\)A�M�A��A϶FAύPAω7A�x�A�r�A�p�A�jA�ZA�Q�A�C�A�9XA��A�
=A���A���A��A��A��yA��mA��yA��yA��HA��#A���A���A���A���A�ȴA�AξwAΩ�AΛ�AΗ�AΓuAΓuAΗ�AΛ�AΛ�AΙ�AΗ�AΙ�AΛ�AΟ�AΟ�AΝ�AΛ�AΟ�AΣ�AΡ�AΟ�AΡ�AΥ�AΥ�AΥ�AΡ�AΛ�A΍PA�|�A�|�A�v�A�bNA�G�A�7LA�"�A�1A�A�A��`A͸RAͣ�A͗�A͇+A�v�A�dZA�M�A�?}A�1'A� �A�VA���A���A��`A̍PA�?}A�7LA�1'A�+A�(�A�&�A�$�A�$�A�$�A� �A��A�{A�VA�A��A��;A���A���A���A�A˸RA˩�A˛�A˃A�O�A�(�A��A�oA���A��
A�ĜAʶFAʾwAʴ9Aʗ�AʅA�n�A�\)A�\)A�^5A�^5A�S�A�E�A�?}A�A�A�M�A�ZA�^5A�`BA�`BA�dZA�ZA�S�A�O�A�Q�A�I�A�E�A�A�A�=qA�;dA�=qA�=qA�;dA�7LA�5?A�7LA�9XA�9XA�33A�&�A�VA��Aɧ�AɑhA�I�A���A���A�A�ĜA�ĜA���A���A�A�AȺ^Aȧ�Aȝ�Aȕ�AȍPAȇ+A�z�A�dZA�VA�C�A�-A��A�oA�bA�JA�JA�  A��A��A��yA��TAǲ-AǋDA�ZA�I�A�=qA�oA���A��A��A��A��A��A��A���AƍPA�VA���AŁA�\)A�&�Aě�A�M�A�=qA��A���Aú^AÅA�\)A�C�A�"�A�VA���A���A���A���A��A��A��A��A��A��`A��/A²-A�A�~�A�M�A�JA���A��\A�ZA�-A�ZA��A�bA�%A�%A�1A�%A�A�A�A�A�  A��`A��7A�C�A��A��A��HA��HA���A���A�G�A�5?A�1'A�{A��
A���A�x�A�p�A�`BA�-A��^A��9A�hsA�1'A��A���A��!A���A��hA�t�A�Q�A�9XA��A��A��;A���A��A���A���A���A��PA�x�A�XA�$�A���A��;A�ĜA��A�p�A�;dA���A�M�A�+A�JA��TA���A�^5A��A���A�A���A�r�A���A��
A��9A��mA���A�^5A��A��A��/A��FA�`BA���A��PA�^5A�;dA�/A�+A��A�A��A��;A���A��A�hsA�A�A��`A�G�A��A��
A���A��wA��FA��A���A�|�A�$�A��!A���A�ffA��A��9A�ZA�=qA�%A���A�r�A�C�A���A��jA���A�n�A�=qA�%A���A���A��A�K�A��A��A�/A��A�
=A��yA��9A��A�ZA�O�A�Q�A�O�A�C�A�7LA�"�A�bA�1A���A��;A���A���A�A���A�r�A�33A�  A���A�l�A�ȴA��A�n�A�S�A�=qA�1A���A�-A���A�v�A�C�A�{A��HA��FA��DA�bNA�VA�K�A�=qA��A��yA��-A�+A� �A���A��A��hA�|�A�^5A�5?A��A�
=A���A���A��\A��\A��DA��A��A�x�A�t�A�l�A�hsA�XA�?}A�9XA�$�A�VA���A�n�A�&�A�VA��!A��+A�Q�A�VA��A�ȴA���A�dZA�+A�{A��;A��9A�XA�-A�oA���A��/A���A���A��RA��DA�\)A�7LA� �A�JA��A�ȴA���A�l�A�1'A���A�ȴA���A��DA��A��A��A��+A���A���A��DA�~�A�ffA�XA�?}A�  A�ĜA���A���A��7A�v�A�M�A�VA��yA���A���A�S�A�-A��A��A���A��^A���A��PA�~�A�r�A�C�A��A��A��^A�`BA��yA���A�$�A��A��jA��PA�E�A�
=A�A���A��DA�l�A�I�A�C�A�-A��yA�r�A�7LA��A���A�n�A��mA��RA�v�A�^5A�I�A�"�A���A��mA��;A��#A��wA��!A���A��uA�v�A�v�A�v�A�jA�ZA�VA�Q�A�I�A�A�A�9XA�+A�
=A��A��`A��/A���A��-A��hA�XA�33A�(�A��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
l�B
m)B
m)B
m)B
m]B
m]B
l�B
m�B
l�B
l�B
m�B
l�B
m)B
l�B
l�B
m)B
l�B
l�B
m)B
l�B
l�B
m]B
m)B
m)B
l�B
m)B
m�B
m]B
m)B
m�B
m�B
m)B
m)B
m�B
l�B
m]B
l�B
kQB
m�B
�B
�B
یB
��BGB=B �B1�B0�B:^BE9BMB`Bm)B�uB��B�CB��B�B��B�B��B��B��B�B��B)�B5BP}BW
BL�BAUB:�B%zB�BxB{B��B�/B��B�XB��B�B�QB��B�9B��B�+B��Bu�Ba|BN<BG�BQ�BrBz�Br�BbNBXEB?�B �BB
��B
�B
�
B
�B
�
B
ɺB
��B
��B
�B
�B
p�B
YB
FtB
7B
.�B
7B	�"B	�B	��B	�NB	ȴB	�$B	�B	��B	��B	tTB	k�B	d�B	Y�B	K�B	F?B	6FB	1�B	�B	�B	B	�B	�B	�B	uB�`B�B��B�)B�cB�B�B�+B�TB�;B�5B�KB�DB� B�|B�B�B�"B�B�fB�B��B�B�TB��B��B�qB��B�<B�wB��B��B�9B�<B�}B�B��B�dB�B�`B�B�B��B�B�%B�B��B�B��B�B�B�oB�fB� B��B�BB�B�|B�/B�dB��B��B�]BܒB�)B��B�jB�HB�B��B�B�QB�]B�5B�AB��B�oB�B�xB	 4B	MB	
	B	"B	B	�B	!�B	"�B	%B	(�B	-wB	3hB	4B	7LB	7LB	6zB	1�B	*�B	.}B	0�B	0UB	/�B	/�B	8RB	8�B	D�B	?�B	9�B	<B	D3B	EmB	B'B	DgB	A B	?HB	A B	C�B	I�B	K^B	LdB	L0B	LdB	K�B	J�B	I�B	K^B	K�B	K�B	LdB	L�B	OB	N�B	NpB	OvB	Q�B	T�B	Z�B	[WB	\�B	[WB	ZB	[�B	_�B	aHB	`vB	bNB	cTB	e�B	g�B	jB	r|B	o�B	o5B	r�B	v+B	yrB	}"B	}"B	�_B	� B	�	B	�IB	��B	��B	��B	��B	��B	��B	��B	�nB	�tB	�zB	�B	�6B	�0B	��B	�B	�'B	��B	��B	�B	�UB	��B	��B	��B	�6B	�$B	�<B	ΥB	��B	�B	��B	�;B	�`B	�fB	�,B	��B	�B	�;B	�pB	�;B	�jB	یB	רB	ΥB	��B	ϫB	�vB	�6B	�B	�BB	ѷB	��B	�B	�5B	��B	�B	�ZB	�>B	�B	��B	�B	��B	�KB	�B	��B	�B	�%B	��B	��B	��B	�8B	��B	�fB	��B	�B	�B	�.B	�cB	��B	�B	��B	�+B	�+B	�|B	�MB	��B	�B	�%B	��B	�]B	��B
  B
 �B
SB
�B
�B
YB
{B
�B
�B
	�B

	B

rB

�B
DB
�B
�B
JB
B
JB
�B
JB
�B
�B
�B
�B
PB
�B
B
B
.B
�B
{B
{B
�B
�B
MB
�B
�B
�B
�B
�B
B
�B
�B
!B
!-B
!�B
"�B
#nB
$B
$B
$@B
$@B
$�B
%B
%FB
'RB
'�B
'�B
'�B
'�B
'�B
'�B
)�B
)�B
)�B
*0B
*0B
*eB
*0B
*0B
*eB
+kB
+�B
+�B
-wB
.IB
.IB
.�B
.�B
/OB
/B
/B
/�B
/�B
0�B
1�B
1�B
2aB
2-B
2aB
2aB
2aB
2-B
33B
2�B
2�B
3hB
4�B
5?B
5?B
5?B
5tB
6FB
6FB
6zB
6zB
6�B
6�B
6zB
6�B
8B
9XB
9$B
9$B
9XB
9�B
9�B
9�B
:^B
:�B
:�B
;�B
<6B
<6B
<jB
<�B
=B
=<B
=<B
=<B
=qB
=qB
=�B
>B
>BB
>wB
>wB
>�B
?�B
?�B
@OB
@B
@OB
A B
B�B
B�B
C-B
C-B
C-B
B�B
B�B
B�B
D�B
C�B
EB
E9B
EB
EB
EB
E9B
E�B
E�B
FB
F?B
F�B
G�B
G�B
G�B
G�B
HB
HKB
H�B
H�B
H�B
I�B
IRB
IB
I�B
I�B
J#B
J�B
J�B
K)B
L�B
M6B
M�B
M�B
M�B
NB
NpB
O�B
PHB
P}B
P�B
Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
m]B
kQB
k�B
kB
m)B
m�B
m�B
m�B
m)B
m]B
l�B
k�B
n�B
l�B
k�B
m)B
ncB
lWB
m�B
ncB
m�B
iB
l�B
l"B
m]B
m�B
m�B
n�B
k�B
k�B
l�B
l�B
l"B
l�B
m�B
k�B
l�B
m�B
m]B
k�B
m]B
m�B
lWB
k�B
m�B
l�B
l"B
m�B
m�B
l�B
l"B
m�B
m�B
lWB
l�B
m�B
m�B
k�B
m)B
ncB
l�B
k�B
l�B
m�B
n/B
l�B
k�B
m)B
m�B
l�B
l"B
l�B
n/B
m]B
k�B
l�B
m�B
m�B
lWB
l�B
m�B
m]B
k�B
l�B
m�B
m�B
k�B
m]B
n/B
l�B
lWB
m�B
m]B
k�B
m]B
m�B
lWB
m)B
ncB
m]B
l�B
l�B
m�B
m]B
l"B
l�B
m�B
m)B
k�B
m�B
n/B
l�B
lWB
lWB
ncB
m�B
k�B
m]B
n�B
m�B
lWB
lWB
m�B
n�B
m]B
l"B
l�B
n�B
m�B
l�B
n�B
m�B
l�B
n�B
n�B
m)B
l�B
ncB
m�B
l�B
l�B
ncB
m�B
lWB
l�B
ncB
m)B
k�B
n/B
m�B
l�B
lWB
n/B
m�B
l"B
l�B
n/B
m]B
l�B
l�B
m)B
n/B
n�B
l�B
l�B
ncB
m�B
m�B
l"B
n/B
m�B
lWB
l�B
m�B
m�B
k�B
k�B
l�B
m�B
m)B
l�B
m�B
m]B
k�B
l�B
m�B
m)B
k�B
k�B
ncB
m�B
m�B
k�B
m)B
l�B
l�B
l"B
l�B
l�B
kQB
kB
jKB
lWB
lWB
jB
i�B
j�B
kB
h�B
h�B
iB
i�B
f�B
e�B
g�B
p;B
x8B
w�B
s�B
��B
��B
��B
��B
��B
��B
�$B
��B
��B
��B
��B
��B
�tB
��B
��B
��B
�aB
��B
уB
�,B
�2B
ԕB
՛B
��B
�B
�B
ںB
�B
�B
�B
��B
�B
�B
�vB
�B
�B
�B
�GB
�TB
�+B
��B
�fB
�fB
�rB
�JB
��B�B�B�B1B1BeB�B�B�BxB�B�B�BB~B�B�B�B�B �B!�B �B!bB"hB$@B%�B)_B)�B-CB3�B7�B7LB3hB4�B3�B2�B1�B8�B6zB33B1[B.�B/�B.�B.IB+6B)_B)�B+kB)�B'�B.�BGzBHBE�BEBF?BFtBF�BFBE�BC�BDgBEmBFBD3BC-BEBGzBF�BE�BFBF�BH�BLdBK�BQ�BW�BS�BS&BU2BZ�B[�BZ�B]�B[�BhsBd�Bd�BffBd�BgmBg�Bh>Bm�Bn�Bo�Bo�BqBu�By>B}�B� B�B��B�MB�YB��B��B�_B�	B��B�B�xB��B��B��B��B��B�~B�JB�PB�@B��B�nB�wB�B�B��B�xB��B�FB��B��B��B��B��B�SB��B��B��B��B��B�FB��B�!B�-B��B�RB�LB�$B�RB��B�^B�XB��B�0B��B�EB�EB� B�BBбB�5B�EBٴB��BרB��B�mBרB�]B�DB��BޞB�|BݘB�8B��B�B�B�QB��B��B�oB��B��B��B�"B�WB��B�B�B�yB�B�yB�sB�8B�8B�mB�B�B�/B�MB�lB�PB�BMB�B-wB;�B6FB)*B(�B'�B'�B(�B*0B*0B)_B)�B2�BE9BC-BD3BJXBC�BB[BG�BU�Bc BW
BTaBVmB]/BW�BUgBQBUgBYB_BRTBV�BQBU�BN�BM�BJ�BJ�BJ�BK)BJXBGEBLdBB[BD3BC-B>BB?HB>�B@�B:�BCaBA BA�B:�B:�B7�B=�B5?BF?B4�B,qB,�B,�B-wB(�B&LB �B �B	B�B(�B�BeB&�BB�BuB�BDB�BFB�B B�B	B%B�B�B�B�BB�B;B�]BuBB�B��B�B�|B��B�cB�cB�B�vB�`B�B�NB�DB�
B�]B��B�BޞB�vB�mB��B�B�vB��BбBȴB�0BŢBȴBרBɺB�aBĜB�^B��B��B�hB��B��B�FB��B�LB��B�B�OBƨBȴB��B��B��B�QBߤB��B��B��B�`B�pB�B�2B��B�B��B� BбB՛BרB�B�HBɆB��B��B��B��B��B�tB��B��B�OB�'B��B�'B�tB�}B�B��B��B��B��B�uB�PB��B� B�B��B��B�{B�MB�B��B�B�MB��B��B��B� B~�B~�B��B�Bs�Bs�Bm�Bu%Bv�Bi�Bg�Bd�BcTBg�Bc�B[WB[#BXBZBQ�BQ�BQBNBI�BH�BJ�BK�BK^BH�BC�BE�BG�BGzBEBI�BG�BL�BFBM�BT�B]/Be,Bg�BkBm)Bp;Br|B{�BxlBzDB{B}�B~�By	Bw�ByrBx�BzBz�Bu�BsBt�Bn�Bk�Bg�BiyBd�BbBa�BaB]dB]�B_pB]�BY�BW�B]/BVBRTBR BFBCaB@B>BB;�B8�BZQB(�B&�B#B �BxB'B!�B B"B�B#�B�BYBB
��B
��B
��B
�.B
��B
��B
�B
��B
�B
��B
�oB
�B
�B
�yB
��B
�B
�B
�B
�DB
�B
��B
��B
��B
�B
�B
�B
�HB
�B
�B
��B
�sB
�mB
��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022071609040420220716090404IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022071610012420220716100124QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022071610012420220716100124QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194620230210131946IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                