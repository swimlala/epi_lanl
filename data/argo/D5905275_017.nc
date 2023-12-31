CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-07-02T01:25:42Z creation; 2023-04-26T19:14:26Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20180702012542  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_017                 7316_008644_017                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�n�k΅4@�n�k΅411  @�n���v�@�n���v�@)!���7@)!���7�dOLnm��dOLnm�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@@  @}p�@�  @\@�  @�p�A  A   A+�A@  A`  A�Q�A�  A�  A�Q�A�Q�A�  A�Q�A��B Q�BQ�B  B  B   B'�
B0  B8  B@  BH  BO�
BX  B`(�Bg�
Bo�
ByB\)B��
B�  B��B�  B�  B��B�  B��B�  B�{B��B��B�  B�  B�  B��B��B�  B��B�  B�{B�{B��B�  B�{B��B�  B�{B�  B�  B��B��C
=C
=C
=C  C
  C  C  C  C{C  C��C  C��C��C��C   C"  C#��C&  C(
=C*�C,
=C.
=C0
=C2
=C4  C6  C8  C:  C<  C>  C@
=CA��CD  CF
=CH
=CI��CK��CN  CP  CR  CT  CV  CX  CZ  C[��C^  C`
=Cb  Cd  Cf  Ch  Ci��Cl  Cm�Co��Cq��Ct  Cv  Cx  Cz  C{��C}��C��C�  C�C�  C�  C�  C���C���C�  C�C�  C���C�  C���C�  C�C�C�  C���C���C�C�C�  C�C�  C���C�  C�C���C���C���C�  C�  C�C�C�C�C�C���C���C�  C�C�C�C�C�
=C�C�  C���C�  C�  C���C�  C�  C�  C�  C�  C���C�  C�C�  C�C�C�  C���C�  C���C���C�  C�C�  C�  C�  C�  C�  C�  C���C���C���C���C���C���C�  C�C���C�  C�C�C�  C�  C�  C���C���C�C�  C���C�  C�  C���C���C���C�  C�  C�C�  C�  C�C�C�  C�  C�C�  C�  C�  C���C���C�  C���C���C�  C�
=C�
=C�C�  C���C�C�C���C���D ��D  D� D  D� D  D}qD�qD� D  D� D�qD}qD�qD}qD�D�D	  D	}qD
  D
� D  D��D  D}qD  D� D  D� D�qD}qD�qD� D  D� D�D� D  D� D  D}qD�qD}qD�D��D�qD}qD�D��DD�D  Dz�D�qD� D�D��D  D� D  D}qD  D}qD   D ��D �qD!}qD"  D"� D#  D#� D$  D$}qD%�D%��D&  D&��D'�D'��D(�D(� D)  D)}qD*  D*}qD*��D+}qD,  D,��D-  D-� D.�D.��D/�D/��D0�D0��D1�D1� D2�D2�D3�D3� D3�qD4� D5  D5��D6  D6z�D6�qD7}qD8  D8� D9  D9��D:  D:� D;  D;}qD;�qD<��D=�D=� D>�D>� D>�qD?��D@�D@}qD@�qDA��DB  DB}qDB�qDC��DDDD� DE�DE�DF  DF��DGDG� DH  DH��DIDI}qDI�qDJ�DK  DK� DL  DL� DM  DM}qDN�DN��DO�DO� DP  DP}qDQ  DQ}qDR  DR� DR��DS}qDT  DT� DT�qDU}qDV  DV� DW  DW� DW�qDX}qDY  DY� DZ  DZ� D[  D[� D\�D\� D]  D]}qD^  D^��D_�D_� D`  D`}qD`�qDa}qDb  Db��Dc�Dc� Dd  Dd}qDd�qDe� Df  Df��Dg  Dg� Dh�Dh� Di  Di��Dj�Dj� Dk�Dk��Dl  Dl� Dm�Dm� Dn  Dn��DoDo� Dp�Dp��Dp�qDqz�Dq�qDr� Ds�Ds� Ds�qDt� Dt�qDu}qDv  Dv��DwDw�DxDx��Dy  Dy� Dy�qDzz�Dz�qD{}qD|  D|��D}  D}� D~  D~� D~�qD}qD�qD�@ D��HD��HD�HD�AHD�� D�� D�HD�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�~�D���D���D�>�D�}qD��qD��qD�>�D�� D�� D��qD�>�D�~�D���D���D�>�D�� D��HD�HD�AHD�� D�� D���D�>�D�� D�� D���D�@ D��HD��HD���D�>�D�~�D�� D�  D�>�D�� D�D�HD�B�D���D�� D���D�>�D�� D�� D�HD�B�D��HD���D�HD�AHD�� D�� D��qD�>�D�� D��HD��D�@ D�~�D�� D�  D�>�D�}qD�� D��D�AHD��HD��HD�HD�AHD�� D��qD���D�AHD��HD���D�  D�@ D�}qD��qD�  D�B�D��HD���D�  D�AHD���D�� D�  D�AHD�� D���D�HD�@ D�~�D�� D�HD�@ D�~�D��HD��D�B�D��HD��HD�HD�@ D�}qD��qD�  D�>�D�~�D���D���D�AHD�� D�� D�  D�>�D�~�D���D�  D�AHD���D��HD�  D�@ D�� D�� D���D�AHD�� D�� D���D�>�D�� D��HD��D�AHD�� D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�HD�>�D�~�D�� D�HD�AHD�~�D��qD���D�@ D�� D�� D�HD�AHD�� D�� D�  D�@ D�~�D�� D�  D�@ D�~�D���D�  D�@ D��HD�� D�  D�@ D�� D���D�  D�AHD�~�D���D���D�>�D�~�D�� D�HD�B�D��HD�� D�  D�AHD�� D��qD���D�>�D�~�D���D���D�=qD�~�D��HD�HD�AHD��HD���D���D�@ D�~�D��qD���D�>�D�� D���D�HD�AHD��HD���D���D�>�D�~�D��HD�  D�@ D�~�D�� D���D�@ D��HD��HD�  D�=qD�~�D���D�  D�@ D��HD��HD�  D�@ D��HD�� D���D�>�D�~�D¾�D�HD�B�DÁHD��HD���D�@ D�~�Dľ�D�  D�@ D�~�Dž�D���D�@ Dƀ D�� D�  D�B�Dǂ�D�� D���D�>�D�~�D�� D�HD�AHDɀ Dɾ�D�HD�B�DʁHD�� D���D�@ D�~�D˾�D�HD�>�D̀ D�� D�  D�>�D�~�D�� D�  D�>�D΀ D�D�HD�AHDπ D�� D�  D�@ DЁHD�� D���D�@ DсHD�� D���D�>�D�}qD�� D��D�B�DӁHD�� D�HD�@ D�~�D�� D���D�AHDՁHDսqD�  D�AHDցHD��HD��D�AHDׁHD�� D���D�>�D؀ D��HD�  D�AHD�~�Dپ�D�  D�>�D�~�D�� D�HD�@ D�~�D�� D���D�>�D܀ DܽqD���D�>�D�~�D��HD�  D�>�Dހ D��HD�HD�>�D�~�D�� D�HD�AHD��HD��HD�HD�AHD�HD��HD�  D�>�D�HD��HD�HD�B�D� D�qD���D�@ D�~�D侸D�  D�AHD� D徸D�HD�AHD� D�D��D�@ D� D��HD�  D�@ D� D�� D���D�@ D�HD�� D�  D�AHD� D�� D��D�@ D� D��HD��D�AHD�~�D�qD�HD�B�D�HD��HD��D�B�D� D�� D��D�AHD�}qD��HD�  D�>�D��HD�� D���D�AHD�D�D�HD�@ D� D�� D�  D�@ D� D�D��D�@ D�HD��HD���D�@ D��HD��HD�HD�@ D�}qD���D�  D�@ D�� D�D�  D�@ D�� D���D���D�@ D��HD���D��qD�H�>�?�?8Q�?��?���?�Q�?�(�?��H@��@(�@.{@E�@O\)@aG�@u@��@��@�33@��R@���@�\)@�Q�@\@���@�z�@�(�@�ff@��@���A ��A�A
=qA\)A�
A�A(�A!G�A%A)��A.{A3�
A8Q�A<(�A@��AEAJ�HAN{AR�\AXQ�A\��A`��Ae�Aj�HAo\)As33Aw�A}p�A���A��HA�p�A�  A�=qA�(�A�
=A���A��A�p�A�Q�A��HA��A�
=A���A�(�A�ffA�Q�A��HA�p�A��A���A�z�A��RA���A�33A�A�Q�A\A���A�\)A��A��
A�{A���AӅA��A�\)A�=qA�z�A�ffA���A�A�p�A�A�=qA�z�A�RA�G�A��
A�A��A�=qA���A��RB z�B�B
=B  B�BffB�B��B	B
=B  B��B=qB�B��Bp�B�HB(�B�B{B33Bz�BB�\B  B�B{B
=B Q�B!��B"�\B#�B$��B&{B'
=B(  B)G�B*�\B+�B,��B-��B.�HB0(�B1�B2{B3\)B4z�B5��B6�\B7�B8��B:{B;
=B<  B=G�B>ffB?�B@z�BA��BB�HBC�
BD��BF{BG\)BHz�BIG�BJffBK�BL��BMBN�HBP(�BQp�BRffBS�BT��BUBW
=BX(�BY�BZ{B[\)B\��B]��B^�RB`  Ba�Bb{Bc
=BdQ�Be��Bf�\Bg�Bhz�BiBk
=Bl  Bl��Bm�Bo33BpQ�BqG�Br{Bs
=BtQ�BuG�Bu�Bv�RBw�BxQ�By�Byp�By�Bz=qBz�RBz�HBz�RBz�HB{
=B{\)B{\)B{33B{\)B{�B{�
B{�B{�B{�
B|  B|Q�B|z�B|Q�B|z�B|��B|��B}�B}�B}�B}p�B}��B}B}B}�B~=qB~�\B~�\B~�\B
=B33B\)B�B�B�{B�(�B�(�B�Q�B�z�B���B���B��RB��HB��B�33B�G�B�\)B��B�B��
B��B�{B�=qB�z�B���B���B��HB��B�\)B��B��B��
B�{B�ffB��\B��RB�
=B�\)B���B�B�  B�ffB��RB��B�G�B��B��
B�=qB���B���B�G�B��B��B�Q�B��RB��B�\)B�B�{B�z�B���B�\)B��B�  B�ffB���B�33B���B��B�Q�B���B��B��B�  B�Q�B���B���B�\)B�B�=qB��\B��HB�33B���B�  B�z�B���B��B�p�B�B�(�B��\B���B�p�B�B�(�B�z�B���B��B��B��B�Q�B��RB�
=B�\)B��B�{B�z�B��HB�33B���B��
B�(�B��\B��HB�G�B��B�{B�ffB���B���B�G�B��B�{B�ffB���B��B�\)B��B�  B�Q�B���B�
=B�p�B��B�{B�Q�B���B���B�G�B���B��B�Q�B���B��B��B��B�Q�B���B�
=B�\)B�B�{B��\B���B�\)B��B�{B��\B���B�\)B��B�{B�ffB���B�G�B��B�{B��\B��HB�G�B�B�{B�z�B��HB�33B���B�  B�z�B��HB�33B��B�(�B��\B���B�\)B��
B�=qB���B�
=B�p�B��
B�Q�B��RB�33B��B�(�B���B�
=B���B�  B�ffB��HB�\)B�B�=qBĸRB�G�B�B�=qBƸRB�33B�B�(�BȸRB�33BɮB�(�Bʣ�B�33BˮB�(�B̸RB�33B�B�=qB���B�\)B��B�z�B�
=Bљ�B�(�BҸRB�\)B��B�z�B�
=Bՙ�B�=qB���B�\)B��B؏\B��BٮB�=qBڸRB�G�B��B�ffB���B݅B�  Bޏ\B��B߮B�(�B�RB�G�B�B�Q�B��HB�B�  B�\B��B�B�=qB���B�G�B��
B�ffB���B�B�{B�\B��B�B�=qB�RB�G�B��
B�Q�B��HB�p�B��B�ffB���B�B�{B��B��B�B�=qB��RB�G�B��
B�Q�B��HB�\)B�  B�z�B�
=B���B�(�B��RB�G�B�B�Q�B��HB�p�B�  B�z�B�
=B���C {C Q�C ��C �
C�CffC�C��C=qCz�CC{CQ�C��C�HC(�CffC�RC  C=qC�C��C{C\)C��C�C33Cz�C�RC
=CQ�C�\C�
C	{C	ffC	��C	�HC
(�C
p�C
�RC
=CG�C�\C�
C�CffC�C��CG�C�\C�HC33Cz�C��C{CffC�C  CQ�C��C�C=qC�\C�C=qC��C�HC=qC�C��C(�Cz�C��C�Cp�CC�Cp�C��C�Cz�CC{CffC�RC
=C\)C�RC{CffCC
=C\)C�C  CQ�C�C
=CffC�RC{CffC�RC
=CffCC �C p�C C!
=C!\)C!�RC"{C"ffC"C#{C#ffC#�RC$  C$Q�C$�C%  C%\)C%�RC&
=C&\)C&�C&��C'Q�C'�C(  C(G�C(�\C(�
C){C)Q�C)��C)�
C*
=C*33C*Q�C*p�C*��C*��C*�C+{C+G�C+ffC+�C+��C+��C+��C,�C,G�C,p�C,�\C,�RC,�
C,��C-�C-G�C-p�C-��C-��C-��C.{C.33C.\)C.z�C.��C.��C.��C/�C/G�C/p�C/�\C/�RC/��C/��C0{C0G�C0p�C0��C0C0�HC1  C1�C1=qC1ffC1�\C1C1�HC2
=C2(�C2G�C2ffC2�\C2�C2�
C3  C3(�C3G�C3p�C3�\C3��C3��C3�HC4
=C433C4\)C4�C4��C4C4�HC5  C5�C5=qC5ffC5�\C5�RC5�HC6  C6(�C6G�C6ffC6�C6��C6C6��C7�C7=qC7ffC7�\C7�C7��C7�C8
=C833C8\)C8�C8�RC8�
C8��C9�C9=qC9ffC9�\C9C9�C:
=C:33C:Q�C:p�C:��C:��C:��C;(�C;Q�C;p�C;�\C;�RC;�HC<
=C<=qC<ffC<�\C<C<�HC=
=C=(�C=Q�C=z�C=��C=�
C>
=C>33C>\)C>�C>�C>��C>��C?(�C?\)C?�C?�RC?�HC@
=C@(�C@Q�C@z�C@�C@�HCA{CA=qCAffCA�\CA�RCA�HCB{CBQ�CBz�CB�CB�
CC  CC(�CC\)CC�CC�RCC�CD�CD=qCDp�CD��CD��CE  CE33CEffCE�\CE�CE�
CF
=CFG�CFz�CF��CF��CF��CG(�CGffCG��CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                      ?�  @�\@@  @}p�@�  @\@�  @�p�A  A   A+�A@  A`  A�Q�A�  A�  A�Q�A�Q�A�  A�Q�A��B Q�BQ�B  B  B   B'�
B0  B8  B@  BH  BO�
BX  B`(�Bg�
Bo�
ByB\)B��
B�  B��B�  B�  B��B�  B��B�  B�{B��B��B�  B�  B�  B��B��B�  B��B�  B�{B�{B��B�  B�{B��B�  B�{B�  B�  B��B��C
=C
=C
=C  C
  C  C  C  C{C  C��C  C��C��C��C   C"  C#��C&  C(
=C*�C,
=C.
=C0
=C2
=C4  C6  C8  C:  C<  C>  C@
=CA��CD  CF
=CH
=CI��CK��CN  CP  CR  CT  CV  CX  CZ  C[��C^  C`
=Cb  Cd  Cf  Ch  Ci��Cl  Cm�Co��Cq��Ct  Cv  Cx  Cz  C{��C}��C��C�  C�C�  C�  C�  C���C���C�  C�C�  C���C�  C���C�  C�C�C�  C���C���C�C�C�  C�C�  C���C�  C�C���C���C���C�  C�  C�C�C�C�C�C���C���C�  C�C�C�C�C�
=C�C�  C���C�  C�  C���C�  C�  C�  C�  C�  C���C�  C�C�  C�C�C�  C���C�  C���C���C�  C�C�  C�  C�  C�  C�  C�  C���C���C���C���C���C���C�  C�C���C�  C�C�C�  C�  C�  C���C���C�C�  C���C�  C�  C���C���C���C�  C�  C�C�  C�  C�C�C�  C�  C�C�  C�  C�  C���C���C�  C���C���C�  C�
=C�
=C�C�  C���C�C�C���C���D ��D  D� D  D� D  D}qD�qD� D  D� D�qD}qD�qD}qD�D�D	  D	}qD
  D
� D  D��D  D}qD  D� D  D� D�qD}qD�qD� D  D� D�D� D  D� D  D}qD�qD}qD�D��D�qD}qD�D��DD�D  Dz�D�qD� D�D��D  D� D  D}qD  D}qD   D ��D �qD!}qD"  D"� D#  D#� D$  D$}qD%�D%��D&  D&��D'�D'��D(�D(� D)  D)}qD*  D*}qD*��D+}qD,  D,��D-  D-� D.�D.��D/�D/��D0�D0��D1�D1� D2�D2�D3�D3� D3�qD4� D5  D5��D6  D6z�D6�qD7}qD8  D8� D9  D9��D:  D:� D;  D;}qD;�qD<��D=�D=� D>�D>� D>�qD?��D@�D@}qD@�qDA��DB  DB}qDB�qDC��DDDD� DE�DE�DF  DF��DGDG� DH  DH��DIDI}qDI�qDJ�DK  DK� DL  DL� DM  DM}qDN�DN��DO�DO� DP  DP}qDQ  DQ}qDR  DR� DR��DS}qDT  DT� DT�qDU}qDV  DV� DW  DW� DW�qDX}qDY  DY� DZ  DZ� D[  D[� D\�D\� D]  D]}qD^  D^��D_�D_� D`  D`}qD`�qDa}qDb  Db��Dc�Dc� Dd  Dd}qDd�qDe� Df  Df��Dg  Dg� Dh�Dh� Di  Di��Dj�Dj� Dk�Dk��Dl  Dl� Dm�Dm� Dn  Dn��DoDo� Dp�Dp��Dp�qDqz�Dq�qDr� Ds�Ds� Ds�qDt� Dt�qDu}qDv  Dv��DwDw�DxDx��Dy  Dy� Dy�qDzz�Dz�qD{}qD|  D|��D}  D}� D~  D~� D~�qD}qD�qD�@ D��HD��HD�HD�AHD�� D�� D�HD�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�~�D���D���D�>�D�}qD��qD��qD�>�D�� D�� D��qD�>�D�~�D���D���D�>�D�� D��HD�HD�AHD�� D�� D���D�>�D�� D�� D���D�@ D��HD��HD���D�>�D�~�D�� D�  D�>�D�� D�D�HD�B�D���D�� D���D�>�D�� D�� D�HD�B�D��HD���D�HD�AHD�� D�� D��qD�>�D�� D��HD��D�@ D�~�D�� D�  D�>�D�}qD�� D��D�AHD��HD��HD�HD�AHD�� D��qD���D�AHD��HD���D�  D�@ D�}qD��qD�  D�B�D��HD���D�  D�AHD���D�� D�  D�AHD�� D���D�HD�@ D�~�D�� D�HD�@ D�~�D��HD��D�B�D��HD��HD�HD�@ D�}qD��qD�  D�>�D�~�D���D���D�AHD�� D�� D�  D�>�D�~�D���D�  D�AHD���D��HD�  D�@ D�� D�� D���D�AHD�� D�� D���D�>�D�� D��HD��D�AHD�� D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�HD�>�D�~�D�� D�HD�AHD�~�D��qD���D�@ D�� D�� D�HD�AHD�� D�� D�  D�@ D�~�D�� D�  D�@ D�~�D���D�  D�@ D��HD�� D�  D�@ D�� D���D�  D�AHD�~�D���D���D�>�D�~�D�� D�HD�B�D��HD�� D�  D�AHD�� D��qD���D�>�D�~�D���D���D�=qD�~�D��HD�HD�AHD��HD���D���D�@ D�~�D��qD���D�>�D�� D���D�HD�AHD��HD���D���D�>�D�~�D��HD�  D�@ D�~�D�� D���D�@ D��HD��HD�  D�=qD�~�D���D�  D�@ D��HD��HD�  D�@ D��HD�� D���D�>�D�~�D¾�D�HD�B�DÁHD��HD���D�@ D�~�Dľ�D�  D�@ D�~�Dž�D���D�@ Dƀ D�� D�  D�B�Dǂ�D�� D���D�>�D�~�D�� D�HD�AHDɀ Dɾ�D�HD�B�DʁHD�� D���D�@ D�~�D˾�D�HD�>�D̀ D�� D�  D�>�D�~�D�� D�  D�>�D΀ D�D�HD�AHDπ D�� D�  D�@ DЁHD�� D���D�@ DсHD�� D���D�>�D�}qD�� D��D�B�DӁHD�� D�HD�@ D�~�D�� D���D�AHDՁHDսqD�  D�AHDցHD��HD��D�AHDׁHD�� D���D�>�D؀ D��HD�  D�AHD�~�Dپ�D�  D�>�D�~�D�� D�HD�@ D�~�D�� D���D�>�D܀ DܽqD���D�>�D�~�D��HD�  D�>�Dހ D��HD�HD�>�D�~�D�� D�HD�AHD��HD��HD�HD�AHD�HD��HD�  D�>�D�HD��HD�HD�B�D� D�qD���D�@ D�~�D侸D�  D�AHD� D徸D�HD�AHD� D�D��D�@ D� D��HD�  D�@ D� D�� D���D�@ D�HD�� D�  D�AHD� D�� D��D�@ D� D��HD��D�AHD�~�D�qD�HD�B�D�HD��HD��D�B�D� D�� D��D�AHD�}qD��HD�  D�>�D��HD�� D���D�AHD�D�D�HD�@ D� D�� D�  D�@ D� D�D��D�@ D�HD��HD���D�@ D��HD��HD�HD�@ D�}qD���D�  D�@ D�� D�D�  D�@ D�� D���D���D�@ D��HD���D��qG�O�>�?�?8Q�?��?���?�Q�?�(�?��H@��@(�@.{@E�@O\)@aG�@u@��@��@�33@��R@���@�\)@�Q�@\@���@�z�@�(�@�ff@��@���A ��A�A
=qA\)A�
A�A(�A!G�A%A)��A.{A3�
A8Q�A<(�A@��AEAJ�HAN{AR�\AXQ�A\��A`��Ae�Aj�HAo\)As33Aw�A}p�A���A��HA�p�A�  A�=qA�(�A�
=A���A��A�p�A�Q�A��HA��A�
=A���A�(�A�ffA�Q�A��HA�p�A��A���A�z�A��RA���A�33A�A�Q�A\A���A�\)A��A��
A�{A���AӅA��A�\)A�=qA�z�A�ffA���A�A�p�A�A�=qA�z�A�RA�G�A��
A�A��A�=qA���A��RB z�B�B
=B  B�BffB�B��B	B
=B  B��B=qB�B��Bp�B�HB(�B�B{B33Bz�BB�\B  B�B{B
=B Q�B!��B"�\B#�B$��B&{B'
=B(  B)G�B*�\B+�B,��B-��B.�HB0(�B1�B2{B3\)B4z�B5��B6�\B7�B8��B:{B;
=B<  B=G�B>ffB?�B@z�BA��BB�HBC�
BD��BF{BG\)BHz�BIG�BJffBK�BL��BMBN�HBP(�BQp�BRffBS�BT��BUBW
=BX(�BY�BZ{B[\)B\��B]��B^�RB`  Ba�Bb{Bc
=BdQ�Be��Bf�\Bg�Bhz�BiBk
=Bl  Bl��Bm�Bo33BpQ�BqG�Br{Bs
=BtQ�BuG�Bu�Bv�RBw�BxQ�By�Byp�By�Bz=qBz�RBz�HBz�RBz�HB{
=B{\)B{\)B{33B{\)B{�B{�
B{�B{�B{�
B|  B|Q�B|z�B|Q�B|z�B|��B|��B}�B}�B}�B}p�B}��B}B}B}�B~=qB~�\B~�\B~�\B
=B33B\)B�B�B�{B�(�B�(�B�Q�B�z�B���B���B��RB��HB��B�33B�G�B�\)B��B�B��
B��B�{B�=qB�z�B���B���B��HB��B�\)B��B��B��
B�{B�ffB��\B��RB�
=B�\)B���B�B�  B�ffB��RB��B�G�B��B��
B�=qB���B���B�G�B��B��B�Q�B��RB��B�\)B�B�{B�z�B���B�\)B��B�  B�ffB���B�33B���B��B�Q�B���B��B��B�  B�Q�B���B���B�\)B�B�=qB��\B��HB�33B���B�  B�z�B���B��B�p�B�B�(�B��\B���B�p�B�B�(�B�z�B���B��B��B��B�Q�B��RB�
=B�\)B��B�{B�z�B��HB�33B���B��
B�(�B��\B��HB�G�B��B�{B�ffB���B���B�G�B��B�{B�ffB���B��B�\)B��B�  B�Q�B���B�
=B�p�B��B�{B�Q�B���B���B�G�B���B��B�Q�B���B��B��B��B�Q�B���B�
=B�\)B�B�{B��\B���B�\)B��B�{B��\B���B�\)B��B�{B�ffB���B�G�B��B�{B��\B��HB�G�B�B�{B�z�B��HB�33B���B�  B�z�B��HB�33B��B�(�B��\B���B�\)B��
B�=qB���B�
=B�p�B��
B�Q�B��RB�33B��B�(�B���B�
=B���B�  B�ffB��HB�\)B�B�=qBĸRB�G�B�B�=qBƸRB�33B�B�(�BȸRB�33BɮB�(�Bʣ�B�33BˮB�(�B̸RB�33B�B�=qB���B�\)B��B�z�B�
=Bљ�B�(�BҸRB�\)B��B�z�B�
=Bՙ�B�=qB���B�\)B��B؏\B��BٮB�=qBڸRB�G�B��B�ffB���B݅B�  Bޏ\B��B߮B�(�B�RB�G�B�B�Q�B��HB�B�  B�\B��B�B�=qB���B�G�B��
B�ffB���B�B�{B�\B��B�B�=qB�RB�G�B��
B�Q�B��HB�p�B��B�ffB���B�B�{B��B��B�B�=qB��RB�G�B��
B�Q�B��HB�\)B�  B�z�B�
=B���B�(�B��RB�G�B�B�Q�B��HB�p�B�  B�z�B�
=B���C {C Q�C ��C �
C�CffC�C��C=qCz�CC{CQ�C��C�HC(�CffC�RC  C=qC�C��C{C\)C��C�C33Cz�C�RC
=CQ�C�\C�
C	{C	ffC	��C	�HC
(�C
p�C
�RC
=CG�C�\C�
C�CffC�C��CG�C�\C�HC33Cz�C��C{CffC�C  CQ�C��C�C=qC�\C�C=qC��C�HC=qC�C��C(�Cz�C��C�Cp�CC�Cp�C��C�Cz�CC{CffC�RC
=C\)C�RC{CffCC
=C\)C�C  CQ�C�C
=CffC�RC{CffC�RC
=CffCC �C p�C C!
=C!\)C!�RC"{C"ffC"C#{C#ffC#�RC$  C$Q�C$�C%  C%\)C%�RC&
=C&\)C&�C&��C'Q�C'�C(  C(G�C(�\C(�
C){C)Q�C)��C)�
C*
=C*33C*Q�C*p�C*��C*��C*�C+{C+G�C+ffC+�C+��C+��C+��C,�C,G�C,p�C,�\C,�RC,�
C,��C-�C-G�C-p�C-��C-��C-��C.{C.33C.\)C.z�C.��C.��C.��C/�C/G�C/p�C/�\C/�RC/��C/��C0{C0G�C0p�C0��C0C0�HC1  C1�C1=qC1ffC1�\C1C1�HC2
=C2(�C2G�C2ffC2�\C2�C2�
C3  C3(�C3G�C3p�C3�\C3��C3��C3�HC4
=C433C4\)C4�C4��C4C4�HC5  C5�C5=qC5ffC5�\C5�RC5�HC6  C6(�C6G�C6ffC6�C6��C6C6��C7�C7=qC7ffC7�\C7�C7��C7�C8
=C833C8\)C8�C8�RC8�
C8��C9�C9=qC9ffC9�\C9C9�C:
=C:33C:Q�C:p�C:��C:��C:��C;(�C;Q�C;p�C;�\C;�RC;�HC<
=C<=qC<ffC<�\C<C<�HC=
=C=(�C=Q�C=z�C=��C=�
C>
=C>33C>\)C>�C>�C>��C>��C?(�C?\)C?�C?�RC?�HC@
=C@(�C@Q�C@z�C@�C@�HCA{CA=qCAffCA�\CA�RCA�HCB{CBQ�CBz�CB�CB�
CC  CC(�CC\)CC�CC�RCC�CD�CD=qCDp�CD��CD��CE  CE33CEffCE�\CE�CE�
CF
=CFG�CFz�CF��CF��CF��CG(�CGffCG��CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                      @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ĜA�Aݕ�A�7LA��A��/Aܩ�A�z�A�O�A�7LA�1'A�(�A�"�A�oA�JA�
=A�1A�%A�A�A�  A���A���A���A���A��A���A���A���A���A��A��A��A���A۩�A�bA؅A�5?A��TAϕ�A�t�Aɗ�A�bNAǙ�A��A�$�AþwA©�A�~�A�&�A�1'A�^5A���A�\)A��FA��+A�-A��A�oA�jA�(�A�p�A�v�A�=qA�|�A�33A�K�A�?}A� �A���A��uA���A�%A���A���A�^5A�&�A�oA|��AyS�AvbAt�Aq��An��AjA�Ae��AcAaA^~�AY��AU7LARbNANȴAM?}AKp�AJ�jAFȴAB�+A=x�A:ĜA7+A3��A1C�A/33A.A,�uA+��A+"�A*�/A)�PA'�A&��A&ĜA&bNA%�TA%dZA&M�A&�9A&$�A%?}A$��A$v�A$ �A#��A#`BA#"�A"��A"�A#�A"��A"{A!%A (�A��A;dA^5A�wA�A�HA�A��AG�AȴA�+A�A�AdZA&�A��AȴA�uA(�AVA�\AZA  A�hAC�A�/A�9Av�A=qA�A�#AAt�A�A�HA�RA��A�DA�+A�Av�AjA9XA$�A��AO�A�/A�AA�AbA��A��A��AhsA�A�/A��A�uAbNAbA��A�^A��A��AXA%AĜA^5A5?A�AƨAdZA/AoA
��A
A�A	�PA�HA�!Av�A5?AA�PA`BA��AI�A�7A�A��A��A�+A=qA�TA��A�`A��A�An�AE�A��AO�A ��A z�A {@���@��H@�ȴ@�ȴ@��@��@��\@���@�G�@��/@� �@�ȴ@�V@���@�G�@��@��@��@��@�S�@��y@��\@��@�`B@��j@�9X@�S�@�ff@��T@�G�@�u@�;d@�M�@�@�X@�j@�1'@��m@�F@�|�@�;d@���@�=q@�@�hs@�7L@���@�;d@柾@�V@�{@��#@�7@�7L@��@�z�@��@��@�1@�1@���@��;@�S�@�R@�M�@��#@��@���@��;@�C�@ݡ�@ܓu@�dZ@�M�@���@�@ى7@�z�@�C�@��H@�~�@��@ՙ�@�O�@���@ԋD@�
=@�V@Ѳ-@�hs@�X@�7L@���@�Ĝ@�z�@�  @ύP@�|�@��@͙�@���@���@̬@ˮ@�+@ʟ�@ʇ+@�^5@���@�x�@�/@ȓu@��
@�"�@�-@�V@��/@�b@Å@�|�@�K�@�ȴ@�{@�?}@��`@�Q�@�  @���@���@���@�dZ@���@��@���@�9X@�A�@�I�@��m@�33@�{@�hs@��@�b@�t�@�ȴ@�M�@�@�p�@�%@���@��@�(�@�ƨ@�;d@�@�M�@���@��7@�O�@�V@��`@��9@��@�I�@��m@���@�l�@��@��@���@�G�@�&�@��`@�r�@�1@��
@��@��@�S�@�"�@��@��!@�v�@��T@��@��@��`@��/@���@��;@��P@���@�~�@��#@���@��^@�%@��9@�Ĝ@�9X@�|�@�"�@��@��+@�E�@�{@�@��7@�G�@�%@��u@��;@���@��F@�l�@��@�^5@�=q@��h@��9@�bN@� �@��@���@���@�;d@���@�ff@�@��^@��7@�`B@�%@��@�bN@��@���@�l�@��H@��!@���@�n�@�{@��#@��-@�O�@��@���@��D@�r�@�1'@�ƨ@�t�@�K�@�"�@���@��\@�ff@��@���@��@�G�@�/@�V@���@��`@�bN@��
@���@�ƨ@��F@���@��@�n�@�E�@��T@�X@��9@��@�Q�@�9X@���@���@��@�$�@���@�p�@�?}@��@�Z@�1@��;@��@�S�@���@���@��R@��+@�-@��@���@�p�@�V@���@�r�@�1'@���@�t�@�K�@�+@�ȴ@�v�@�5?@�J@��#@��@�G�@��@��/@��9@��u@�z�@�z�@�r�@�I�@�(�@�9X@� �@�@��@�P@K�@~�y@~{@}��@}O�@|�/@|�D@|1@{��@{33@{@z�H@z�\@z-@y�@y�7@yG�@x�9@x��@xr�@xA�@x �@w�w@w;d@w�@w
=@v��@v{@up�@t�@tI�@s��@st�@s"�@r�H@r��@r�!@rn�@q��@qG�@p�u@pQ�@pb@o
=@n�@nȴ@n$�@m�@m�@l��@lj@l(�@k��@k@j�@j��@j-@i��@ix�@iG�@h��@h�u@hQ�@h  @g|�@g+@f�R@fv�@f$�@e��@e`B@eO�@e�@d��@d(�@c�m@c�@co@b��@bJ@a�^@ahs@a7L@`��@`r�@`b@_+@^E�@^@]��@]�@\��@\�@\j@\�@[�F@[C�@[o@Z�H@Z�\@Zn�@ZJ@Yx�@YX@X��@X�9@X�@X �@W�P@Wl�@W\)@W;d@W
=@V�@V��@V$�@U��@U�h@UV@Tj@S�m@SdZ@S@R~�@RM�@R-@Q�#@Q7L@PA�@O�;@O\)@O
=@N��@N��@N5?@M�-@Mp�@M`B@M?}@L�/@MV@L��@L��@L�@L�D@Kƨ@Kt�@Ko@J�!@J��@J-@I�#@Ihs@H��@H �@H �@G�@F�@FE�@FV@F5?@E?}@D�j@D�@D�@CS�@C"�@B�H@B^5@A��@Ax�@A7L@A�@@�`@@�9@@r�@@1'@?�@?�w@?��@?|�@?+@>��@=�@=`B@=?}@=V@<�j@<�@<��@<Z@;��@;o@:�H@:n�@9��@9��@9�#@9X@9&�@8�9@7�@7��@7��@7�@7�P@7
=@6�@6��@6��@6��@6�+@6v�@6ff@6��@6{@5�-@5`B@5V@4�/@4�j@4�D@4Z@4Z@4I�@41@3��@3��@3dZ@3"�@3"�@2��@2J@1�7@17L@0��@0Ĝ@0��@/�@.ȴ@.�R@.��@.$�@-��@-��@-�h@-O�@-�@-�@-�@,1@+��@,I�@,I�@,Z@,(�@+�
@+t�@+S�@+C�@*�@*~�@*^5@*-@)�@)�^@)�^@)��@)�7@)&�@)�@)%@)%@(��@(�`@(bN@(A�@(b@'�;@'�@'\)@'+@&��@&�@&ȴ@&�@&ȴ@&��@&�+@&ff@&5?@&{@%�T@%�-@%�h@%p�@%O�@%?}@%V@$��@$I�@#t�@#C�@#@"�H@"��@"n�@!��@!x�@!hs@!7L@!�@ �`@ �9@ Q�@�w@��@v�@V@E�@$�@@�@�@z�@Z@9X@�@�
@�@��@�@�^@��@x�@hs@G�@&�@�@&�@&�@7L@&�@��@�@��@�w@�@�;@l�@;d@��@�y@��@ff@E�@O�@?}@�@��@�@�@j@9X@�m@ƨ@��@S�@C�@33@"�@�@�!@�\@~�@^5@�#@��@��@�7@X@�`@��@Ĝ@Ĝ@��@��@�@bN@1'@��@l�@l�@K�@�@�@��@�+@v�@ff@$�@�T@��@`B@?}@/@V@�@�j@��@z�@I�@�m@��@S�@33@o@
�H@
��@
��@
�!@
�\@
^5@
-@	��@	�@	��@	hs@	x�@	hs@	hs@	&�@�`@Ĝ@��@�u@�@bN@bNA�ƨA�ƨAݲ-A���A���AݸRA���A�ƨA���Aݴ9Aݝ�AݑhA݋DA�dZA�VA� �A��A�$�A�+A�oA��A��HA��HA���AܾwAܧ�Aܥ�Aܗ�A�|�A�p�A�ffA�^5A�O�A�G�A�?}A�;dA�9XA�33A�/A�/A�1'A�+A�(�A�(�A�+A�$�A�"�A�$�A�(�A�$�A� �A� �A� �A��A�{A�{A�{A�bA�VA�bA�VA�
=A�JA�VA�JA�1A�
=A�VA�VA�
=A�1A�JA�JA�1A�1A�
=A�
=A�A�1A�
=A�%A�A�%A�1A�%A�A�A�1A�%A�A�A�1A�A�  A�A�%A�A�  A�A�A���A�  A�A���A�  A�A�A���A���A�  A���A���A�  A�A���A���A�  A�  A���A���A�  A�  A���A���A�  A�  A���A���A���A���A���A���A���A���A��A��A���A���A��A��A���A���A��A��A���A���A��A��A���A���A���A��A���A���A���A��A��A���A���A��A��A���A���A���A��A���A���A���A��A��A���A��A��A��A���A���A��A��A���A���A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��A��yA��/A���A���A���A�A���A۾wAۺ^A۶FAۮA۬A۟�AۓuA�r�A�S�A�C�A��A�A��yA��/A�Aڡ�A�z�A�G�A���A�5?A��#A�x�A�A�A�/A�&�A��A��A�bA�VA�oA�VA�
=A�%A�1A�1A�A���A���A���A���A��A��`A��/A��A���A�ȴA׼jA׮Aף�Aי�A׏\A׃A�~�AׁA�~�A�z�A�r�A�t�A�t�A�l�A�^5A�S�A�C�A�33A�$�A��A��A�JA��A��/A���AֶFA֝�AցA�bNA�;dA��A���A��AռjA�ffA��Aԣ�A�p�A�S�A�5?A�;dA� �A���Aӛ�AӇ+AӑhAӏ\AӇ+A�n�A�VA�5?A�A���AҁA�
=Aѩ�A�x�A�AЇ+A��Aβ-A� �A�A�-A��`A���A̡�Ả7A�K�A�-A��A�E�A��`A�v�A�VA�"�A�A��yAɸRAɰ!AɬAɝ�AɅA�z�A�l�A�Q�A� �A���A���AȺ^A�v�A�?}A�+A�"�A��A��A�
=A���A��A��
AǺ^Aǧ�Aǟ�AǙ�AǍPA�~�A�XA� �A��yAƬA�\)A��A���AŸRAś�AŃAŃAŅAŁA�jA�jA�hsA�ZA�M�A�+A��A���A��A��#A���A�ĜAľwAĸRAď\Aô9A�~�A�r�A�dZA�Q�A�9XA�&�A� �A�VA���A��HA��#A��
A���A�ƨA´9A�A�v�A�^5A�O�A��A��!A�7LA��/A�I�A��A�JA�\)A���A���A�p�A�S�A�E�A�9XA�1'A�(�A�$�A�$�A��A�JA��A���A���A�jA��A�{A�JA�%A�A�  A���A���A��A��/A��-A�|�A�VA�7LA�A���A��FA��A���A�n�A�7LA��A���A��^A�ffA���A��A�\)A��wA���A��+A�A�A��A���A���A��DA��A�|�A�XA��A�C�A�
=A�A���A���A���A���A���A��7A��A�z�A�K�A�{A�ĜA�ZA�  A���A��A�A�A��;A��A�/A��TA���A�9XA���A�x�A�%A��FA���A��A��jA��A�M�A�5?A�1A���A�|�A�33A��A���A��\A�jA�VA�9XA��A��A���A��A���A��PA��A�t�A�dZA�Q�A�5?A�1A���A�I�A�M�A�ȴA�v�A�S�A�JA���A�r�A�S�A��A��yA��-A�-A��!A��A���A�XA��yA���A�K�A��A�%A��A���A��A��7A�ȴA�|�A�C�A���A�&�A�A��FA���A���A��uA�~�A�^5A�A�A��A�%A��A�ȴA��9A�v�A���A���A���A��A���A�~�A�K�A�"�A��/A��A���A�~�A�I�A��mA�jA���A���A�l�A��hA�5?A�bA���A��A��A��A��mA�ȴA��A��yA��PA�K�A��9A��PA�l�A��A�VA�%A���A��;A���A��A��DA��A�(�A��`A��wA���A���A���A��A��uA�l�A�VA�I�A��A�A�K�A�ZA�O�A��A�\)A�^5A��PA�ffA�`BA�\)A�7LA���A���A?}A~^5A|��A{�wA{33Az-Ay�#Ay��Ay�FAyt�Ax��Aw��Av��AvZAv(�Au�
Au�Au\)AuAt�/AtȴAt�At��Atn�As��Ar�Ar  Aq�Aq33ApffAp=qAp(�Ao�TAo?}AnJAm33Al  Aj��Aj�DAjffAi��Ah�HAgXAf=qAe�TAex�Ae"�Ad�DAd=qAc��Ac��Ac�-Ac��Acl�Ac;dAb��AbI�Aa��Aa&�A`9XA_x�A_33A_�A^��A^E�A]A\�A[�PAZ�uAY��AX�yAX  AW/AV��AVr�AVJAUAUt�AUS�AU;dAT�HAT�AS�FASp�ASS�ASO�ASG�ASC�AS?}AS7LAS�AR��ARjAQ��AQ7LAP��AP��AP^5APE�APbAO�AO�#AOAO��AOG�AM�AMp�AMhsAMdZAM\)AMXAMS�AMO�AMK�AMK�AMG�AMG�AMC�AM;dAM33AM33AM33AM/AM33AM&�AMoALĜAL9XAKƨAKXAK7LAKoAJ��AJ�yAJ�yAJ�/AJ��AJ��AJĜAJĜAJĜAJĜAJĜAJĜAJĜAJĜAJ��AJ��AJ��AJ�RAJ�!AJ�AJ�AJ��AJ��AJz�AJVAJJAIt�AH�+AE��ADJAC�;AC�wAC��AC��AC�7ACx�ACl�AC`BACS�ACC�AC7LAC�AC�AC%AB�yAB��AB(�AAl�A@$�A?O�A?oA>��A>ZA=�A=A=l�A<�A<�HA<��A<ĜA<�jA<��A<A�A<A;�wA;�A;��A;G�A;oA:�`A:�!A:ffA9��A9XA8��A8ffA81'A81A7�TA7��A7�A7\)A7"�A6�yA6~�A69XA5�TA5`BA5
=A4�`A4�\A4$�A3�FA3dZA3/A3�A2��A2�A2jA29XA2bA1�A1�TA1��A1�A1�7A1S�A0z�A/�TA/ƨA/��A/�hA/�A/p�A/`BA/\)A/K�A/�A.��A.��A.�jA.�\A.ffA.bNA.^5A.M�A.1'A.�A.bA-�mA-�-A-XA-
=A,�A,��A,�9A,�A,��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                      A�ĜA�Aݕ�A�7LA��A��/Aܩ�A�z�A�O�A�7LA�1'A�(�A�"�A�oA�JA�
=A�1A�%A�A�A�  A���A���A���A���A��A���A���A���A���A��A��A��A���A۩�A�bA؅A�5?A��TAϕ�A�t�Aɗ�A�bNAǙ�A��A�$�AþwA©�A�~�A�&�A�1'A�^5A���A�\)A��FA��+A�-A��A�oA�jA�(�A�p�A�v�A�=qA�|�A�33A�K�A�?}A� �A���A��uA���A�%A���A���A�^5A�&�A�oA|��AyS�AvbAt�Aq��An��AjA�Ae��AcAaA^~�AY��AU7LARbNANȴAM?}AKp�AJ�jAFȴAB�+A=x�A:ĜA7+A3��A1C�A/33A.A,�uA+��A+"�A*�/A)�PA'�A&��A&ĜA&bNA%�TA%dZA&M�A&�9A&$�A%?}A$��A$v�A$ �A#��A#`BA#"�A"��A"�A#�A"��A"{A!%A (�A��A;dA^5A�wA�A�HA�A��AG�AȴA�+A�A�AdZA&�A��AȴA�uA(�AVA�\AZA  A�hAC�A�/A�9Av�A=qA�A�#AAt�A�A�HA�RA��A�DA�+A�Av�AjA9XA$�A��AO�A�/A�AA�AbA��A��A��AhsA�A�/A��A�uAbNAbA��A�^A��A��AXA%AĜA^5A5?A�AƨAdZA/AoA
��A
A�A	�PA�HA�!Av�A5?AA�PA`BA��AI�A�7A�A��A��A�+A=qA�TA��A�`A��A�An�AE�A��AO�A ��A z�A {@���@��H@�ȴ@�ȴ@��@��@��\@���@�G�@��/@� �@�ȴ@�V@���@�G�@��@��@��@��@�S�@��y@��\@��@�`B@��j@�9X@�S�@�ff@��T@�G�@�u@�;d@�M�@�@�X@�j@�1'@��m@�F@�|�@�;d@���@�=q@�@�hs@�7L@���@�;d@柾@�V@�{@��#@�7@�7L@��@�z�@��@��@�1@�1@���@��;@�S�@�R@�M�@��#@��@���@��;@�C�@ݡ�@ܓu@�dZ@�M�@���@�@ى7@�z�@�C�@��H@�~�@��@ՙ�@�O�@���@ԋD@�
=@�V@Ѳ-@�hs@�X@�7L@���@�Ĝ@�z�@�  @ύP@�|�@��@͙�@���@���@̬@ˮ@�+@ʟ�@ʇ+@�^5@���@�x�@�/@ȓu@��
@�"�@�-@�V@��/@�b@Å@�|�@�K�@�ȴ@�{@�?}@��`@�Q�@�  @���@���@���@�dZ@���@��@���@�9X@�A�@�I�@��m@�33@�{@�hs@��@�b@�t�@�ȴ@�M�@�@�p�@�%@���@��@�(�@�ƨ@�;d@�@�M�@���@��7@�O�@�V@��`@��9@��@�I�@��m@���@�l�@��@��@���@�G�@�&�@��`@�r�@�1@��
@��@��@�S�@�"�@��@��!@�v�@��T@��@��@��`@��/@���@��;@��P@���@�~�@��#@���@��^@�%@��9@�Ĝ@�9X@�|�@�"�@��@��+@�E�@�{@�@��7@�G�@�%@��u@��;@���@��F@�l�@��@�^5@�=q@��h@��9@�bN@� �@��@���@���@�;d@���@�ff@�@��^@��7@�`B@�%@��@�bN@��@���@�l�@��H@��!@���@�n�@�{@��#@��-@�O�@��@���@��D@�r�@�1'@�ƨ@�t�@�K�@�"�@���@��\@�ff@��@���@��@�G�@�/@�V@���@��`@�bN@��
@���@�ƨ@��F@���@��@�n�@�E�@��T@�X@��9@��@�Q�@�9X@���@���@��@�$�@���@�p�@�?}@��@�Z@�1@��;@��@�S�@���@���@��R@��+@�-@��@���@�p�@�V@���@�r�@�1'@���@�t�@�K�@�+@�ȴ@�v�@�5?@�J@��#@��@�G�@��@��/@��9@��u@�z�@�z�@�r�@�I�@�(�@�9X@� �@�@��@�P@K�@~�y@~{@}��@}O�@|�/@|�D@|1@{��@{33@{@z�H@z�\@z-@y�@y�7@yG�@x�9@x��@xr�@xA�@x �@w�w@w;d@w�@w
=@v��@v{@up�@t�@tI�@s��@st�@s"�@r�H@r��@r�!@rn�@q��@qG�@p�u@pQ�@pb@o
=@n�@nȴ@n$�@m�@m�@l��@lj@l(�@k��@k@j�@j��@j-@i��@ix�@iG�@h��@h�u@hQ�@h  @g|�@g+@f�R@fv�@f$�@e��@e`B@eO�@e�@d��@d(�@c�m@c�@co@b��@bJ@a�^@ahs@a7L@`��@`r�@`b@_+@^E�@^@]��@]�@\��@\�@\j@\�@[�F@[C�@[o@Z�H@Z�\@Zn�@ZJ@Yx�@YX@X��@X�9@X�@X �@W�P@Wl�@W\)@W;d@W
=@V�@V��@V$�@U��@U�h@UV@Tj@S�m@SdZ@S@R~�@RM�@R-@Q�#@Q7L@PA�@O�;@O\)@O
=@N��@N��@N5?@M�-@Mp�@M`B@M?}@L�/@MV@L��@L��@L�@L�D@Kƨ@Kt�@Ko@J�!@J��@J-@I�#@Ihs@H��@H �@H �@G�@F�@FE�@FV@F5?@E?}@D�j@D�@D�@CS�@C"�@B�H@B^5@A��@Ax�@A7L@A�@@�`@@�9@@r�@@1'@?�@?�w@?��@?|�@?+@>��@=�@=`B@=?}@=V@<�j@<�@<��@<Z@;��@;o@:�H@:n�@9��@9��@9�#@9X@9&�@8�9@7�@7��@7��@7�@7�P@7
=@6�@6��@6��@6��@6�+@6v�@6ff@6��@6{@5�-@5`B@5V@4�/@4�j@4�D@4Z@4Z@4I�@41@3��@3��@3dZ@3"�@3"�@2��@2J@1�7@17L@0��@0Ĝ@0��@/�@.ȴ@.�R@.��@.$�@-��@-��@-�h@-O�@-�@-�@-�@,1@+��@,I�@,I�@,Z@,(�@+�
@+t�@+S�@+C�@*�@*~�@*^5@*-@)�@)�^@)�^@)��@)�7@)&�@)�@)%@)%@(��@(�`@(bN@(A�@(b@'�;@'�@'\)@'+@&��@&�@&ȴ@&�@&ȴ@&��@&�+@&ff@&5?@&{@%�T@%�-@%�h@%p�@%O�@%?}@%V@$��@$I�@#t�@#C�@#@"�H@"��@"n�@!��@!x�@!hs@!7L@!�@ �`@ �9@ Q�@�w@��@v�@V@E�@$�@@�@�@z�@Z@9X@�@�
@�@��@�@�^@��@x�@hs@G�@&�@�@&�@&�@7L@&�@��@�@��@�w@�@�;@l�@;d@��@�y@��@ff@E�@O�@?}@�@��@�@�@j@9X@�m@ƨ@��@S�@C�@33@"�@�@�!@�\@~�@^5@�#@��@��@�7@X@�`@��@Ĝ@Ĝ@��@��@�@bN@1'@��@l�@l�@K�@�@�@��@�+@v�@ff@$�@�T@��@`B@?}@/@V@�@�j@��@z�@I�@�m@��@S�@33@o@
�H@
��@
��@
�!@
�\@
^5@
-@	��@	�@	��@	hs@	x�@	hs@	hs@	&�@�`@Ĝ@��@�u@�@bNG�O�A�ƨA�ƨAݲ-A���A���AݸRA���A�ƨA���Aݴ9Aݝ�AݑhA݋DA�dZA�VA� �A��A�$�A�+A�oA��A��HA��HA���AܾwAܧ�Aܥ�Aܗ�A�|�A�p�A�ffA�^5A�O�A�G�A�?}A�;dA�9XA�33A�/A�/A�1'A�+A�(�A�(�A�+A�$�A�"�A�$�A�(�A�$�A� �A� �A� �A��A�{A�{A�{A�bA�VA�bA�VA�
=A�JA�VA�JA�1A�
=A�VA�VA�
=A�1A�JA�JA�1A�1A�
=A�
=A�A�1A�
=A�%A�A�%A�1A�%A�A�A�1A�%A�A�A�1A�A�  A�A�%A�A�  A�A�A���A�  A�A���A�  A�A�A���A���A�  A���A���A�  A�A���A���A�  A�  A���A���A�  A�  A���A���A�  A�  A���A���A���A���A���A���A���A���A��A��A���A���A��A��A���A���A��A��A���A���A��A��A���A���A���A��A���A���A���A��A��A���A���A��A��A���A���A���A��A���A���A���A��A��A���A��A��A��A���A���A��A��A���A���A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��A��yA��/A���A���A���A�A���A۾wAۺ^A۶FAۮA۬A۟�AۓuA�r�A�S�A�C�A��A�A��yA��/A�Aڡ�A�z�A�G�A���A�5?A��#A�x�A�A�A�/A�&�A��A��A�bA�VA�oA�VA�
=A�%A�1A�1A�A���A���A���A���A��A��`A��/A��A���A�ȴA׼jA׮Aף�Aי�A׏\A׃A�~�AׁA�~�A�z�A�r�A�t�A�t�A�l�A�^5A�S�A�C�A�33A�$�A��A��A�JA��A��/A���AֶFA֝�AցA�bNA�;dA��A���A��AռjA�ffA��Aԣ�A�p�A�S�A�5?A�;dA� �A���Aӛ�AӇ+AӑhAӏ\AӇ+A�n�A�VA�5?A�A���AҁA�
=Aѩ�A�x�A�AЇ+A��Aβ-A� �A�A�-A��`A���A̡�Ả7A�K�A�-A��A�E�A��`A�v�A�VA�"�A�A��yAɸRAɰ!AɬAɝ�AɅA�z�A�l�A�Q�A� �A���A���AȺ^A�v�A�?}A�+A�"�A��A��A�
=A���A��A��
AǺ^Aǧ�Aǟ�AǙ�AǍPA�~�A�XA� �A��yAƬA�\)A��A���AŸRAś�AŃAŃAŅAŁA�jA�jA�hsA�ZA�M�A�+A��A���A��A��#A���A�ĜAľwAĸRAď\Aô9A�~�A�r�A�dZA�Q�A�9XA�&�A� �A�VA���A��HA��#A��
A���A�ƨA´9A�A�v�A�^5A�O�A��A��!A�7LA��/A�I�A��A�JA�\)A���A���A�p�A�S�A�E�A�9XA�1'A�(�A�$�A�$�A��A�JA��A���A���A�jA��A�{A�JA�%A�A�  A���A���A��A��/A��-A�|�A�VA�7LA�A���A��FA��A���A�n�A�7LA��A���A��^A�ffA���A��A�\)A��wA���A��+A�A�A��A���A���A��DA��A�|�A�XA��A�C�A�
=A�A���A���A���A���A���A��7A��A�z�A�K�A�{A�ĜA�ZA�  A���A��A�A�A��;A��A�/A��TA���A�9XA���A�x�A�%A��FA���A��A��jA��A�M�A�5?A�1A���A�|�A�33A��A���A��\A�jA�VA�9XA��A��A���A��A���A��PA��A�t�A�dZA�Q�A�5?A�1A���A�I�A�M�A�ȴA�v�A�S�A�JA���A�r�A�S�A��A��yA��-A�-A��!A��A���A�XA��yA���A�K�A��A�%A��A���A��A��7A�ȴA�|�A�C�A���A�&�A�A��FA���A���A��uA�~�A�^5A�A�A��A�%A��A�ȴA��9A�v�A���A���A���A��A���A�~�A�K�A�"�A��/A��A���A�~�A�I�A��mA�jA���A���A�l�A��hA�5?A�bA���A��A��A��A��mA�ȴA��A��yA��PA�K�A��9A��PA�l�A��A�VA�%A���A��;A���A��A��DA��A�(�A��`A��wA���A���A���A��A��uA�l�A�VA�I�A��A�A�K�A�ZA�O�A��A�\)A�^5A��PA�ffA�`BA�\)A�7LA���A���A?}A~^5A|��A{�wA{33Az-Ay�#Ay��Ay�FAyt�Ax��Aw��Av��AvZAv(�Au�
Au�Au\)AuAt�/AtȴAt�At��Atn�As��Ar�Ar  Aq�Aq33ApffAp=qAp(�Ao�TAo?}AnJAm33Al  Aj��Aj�DAjffAi��Ah�HAgXAf=qAe�TAex�Ae"�Ad�DAd=qAc��Ac��Ac�-Ac��Acl�Ac;dAb��AbI�Aa��Aa&�A`9XA_x�A_33A_�A^��A^E�A]A\�A[�PAZ�uAY��AX�yAX  AW/AV��AVr�AVJAUAUt�AUS�AU;dAT�HAT�AS�FASp�ASS�ASO�ASG�ASC�AS?}AS7LAS�AR��ARjAQ��AQ7LAP��AP��AP^5APE�APbAO�AO�#AOAO��AOG�AM�AMp�AMhsAMdZAM\)AMXAMS�AMO�AMK�AMK�AMG�AMG�AMC�AM;dAM33AM33AM33AM/AM33AM&�AMoALĜAL9XAKƨAKXAK7LAKoAJ��AJ�yAJ�yAJ�/AJ��AJ��AJĜAJĜAJĜAJĜAJĜAJĜAJĜAJĜAJ��AJ��AJ��AJ�RAJ�!AJ�AJ�AJ��AJ��AJz�AJVAJJAIt�AH�+AE��ADJAC�;AC�wAC��AC��AC�7ACx�ACl�AC`BACS�ACC�AC7LAC�AC�AC%AB�yAB��AB(�AAl�A@$�A?O�A?oA>��A>ZA=�A=A=l�A<�A<�HA<��A<ĜA<�jA<��A<A�A<A;�wA;�A;��A;G�A;oA:�`A:�!A:ffA9��A9XA8��A8ffA81'A81A7�TA7��A7�A7\)A7"�A6�yA6~�A69XA5�TA5`BA5
=A4�`A4�\A4$�A3�FA3dZA3/A3�A2��A2�A2jA29XA2bA1�A1�TA1��A1�A1�7A1S�A0z�A/�TA/ƨA/��A/�hA/�A/p�A/`BA/\)A/K�A/�A.��A.��A.�jA.�\A.ffA.bNA.^5A.M�A.1'A.�A.bA-�mA-�-A-XA-
=A,�A,��A,�9A,�A,��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	J�B	I�B	J�B	M�B	H�B	I�B	K^B	J�B	JXB	JXB	JXB	JXB	I�B	J#B	JXB	J#B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	L0B	L0B	L�B	N<B	T,B	Y�B	g�B	��B	��B	�$B
.B
��B
��B
��B�BF?BYBl"Bu%B��B�VB�tB�tB��B��B��B��B�<B��B��B��B�FB�nB��B�uB^5B8RBSB
��B
�B
� B
�$B
l�B
a�B
)�B
�B	�B	�B	��B	��B	l�B	`BB	Q�B	LdB	<jB	,�B	CB	JB	JB��B	{B�B��B��B�B��B�5B�B��B�B�B�cB	�B	%zB	:�B	F?B	QNB	R B	T�B	ZB	`BB	]�B	bB	r�B	|B	�oB	�B	�B	�<B	͟B	уB	��B	��B	�BB	��B	�HB	�B
�B
�B
�B
SB
�B
!�B
 �B
#�B
'�B
,�B
-�B
-�B
2�B
5?B
<B
?�B
?B
?�B
?B
@�B
@�B
@�B
@�B
?�B
>�B
=B
9$B
6FB
7B
<�B
?HB
C�B
F�B
GzB
HKB
I�B
J�B
L�B
L�B
NpB
N<B
NpB
NpB
N�B
NpB
NpB
N�B
N�B
NpB
N�B
N�B
OBB
P�B
QNB
RTB
S&B
Q�B
Q�B
QB
P�B
QB
QB
PB
P�B
Q�B
RTB
R�B
Q�B
Q�B
P�B
P}B
PB
OB
N<B
NB
N<B
OBB
O�B
OB
N<B
M�B
L�B
K�B
L�B
J�B
I�B
H�B
G�B
GzB
HB
J�B
K�B
JXB
H�B
G�B
GEB
F�B
F?B
FtB
E9B
D�B
E�B
C-B
B�B
B'B
A�B
AUB
A B
A�B
@�B
>�B
>B
<�B
<B
<B
<B
<B
<jB
;�B
;0B
9�B
;dB
8�B
7B
6zB
6zB
5�B
5�B
5?B
5?B
49B
33B
2�B
1�B
0�B
/OB
.B
-wB
)�B
(�B
&�B
$B
$@B
!bB
 �B
�B
VB
B
�B
CB
�B
�B
B
B
�B
�B
+B
�B
+B
B
�B
�B
�B
�B
�B
{B
�B
B
B
MB
B
B
B
$B
�B
�B
�B
�B
�B
�B
{B
B
@B
:B
�B
�B
�B
VB
�B
�B
B
�B
�B
DB
DB
DB
�B
�B
�B
�B
�B
�B
PB
�B
B
B
�B
JB
JB
PB
�B
�B
~B
JB
�B
~B
�B
�B
�B
B
~B
B

�B

=B
	�B

�B
�B
1B
	�B
1B
�B
�B
1B
�B
�B
	�B
	�B

	B
	lB
	7B
	B
	lB

rB
B
�B
DB

�B

rB
�B

�B
�B

�B
�B
�B
PB
\B
�B
�B
.B
�B
.B
.B
bB
�B
bB
 B
�B
�B
:B
oB
oB
oB
oB
�B
�B
�B
�B
:B
�B
B
FB
B
B
{B
�B
B
B
B
MB
B
MB
�B
�B
MB
�B
{B
FB
�B
uB
�B
SB
�B
YB
_B
�B
�B
�B
�B
�B
�B
�B
=B
kB
7B
B
B
�B
�B
�B
�B
B
xB
~B
B
~B
�B
B
xB
�B
IB
IB
�B
B
�B
�B
�B
�B
B
IB
�B
~B
~B
~B
B
B
B
�B
�B
�B
�B
�B
 'B
 �B
 �B
!bB
!bB
!�B
!�B
"�B
"4B
"hB
#B
#B
#:B
#nB
#�B
$�B
%�B
%�B
&B
&�B
&�B
'RB
'B
&�B
&�B
&LB
'�B
'�B
'�B
'�B
'�B
'B
(XB
'�B
)*B
(�B
)*B
)*B
)*B
(�B
(�B
)_B
)_B
*eB
+�B
-CB
-wB
-B
.IB
.B
.�B
.}B
.�B
/OB
/�B
/OB
/B
/OB
0!B
/�B
0�B
0�B
1'B
1�B
1�B
1[B
2-B
1�B
2-B
2�B
33B
33B
3hB
3�B
4nB
4nB
4nB
4nB
4�B
4�B
4�B
4�B
4�B
5B
6�B
7�B
9�B
:�B
:�B
;0B
:�B
:�B
:�B
;�B
;dB
<�B
<jB
<�B
=B
=B
>BB
>wB
>�B
?}B
>�B
>�B
>�B
?�B
@OB
A B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B[B
CaB
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
DgB
D�B
EB
E9B
FB
E�B
F�B
GB
F�B
GEB
GB
GB
GB
GB
F�B
F�B
GzB
GzB
GzB
GzB
G�B
HB
G�B
G�B
G�B
H�B
H�B
H�B
IB
IB
I�B
I�B
I�B
J#B
J�B
K)B
K^B
K�B
K�B
K�B
K�B
K�B
K^B
K�B
K�B
K�B
L0B
LdB
L0B
L�B
MB
M6B
MB
MB
M�B
M�B
M6B
M�B
MjB
MjB
M�B
M�B
M�B
NB
M�B
NpB
N�B
N�B
OvB
OvB
PB
PB
P}B
PHB
P}B
QNB
RTB
R�B
R�B
S�B
S[B
S�B
S�B
T,B
T,B
TaB
S�B
T,B
T,B
T,B
TaB
T�B
T,B
S�B
TaB
TaB
TaB
T�B
TaB
T�B
T�B
T�B
U�B
U�B
XEB
X�B
YKB
YKB
Y�B
Y�B
Y�B
YB
YKB
Y�B
[#B
[#B
[�B
[WB
[WB
[#B
[�B
[#B
Z�B
\)B
^5B
^�B
^B
]�B
^�B
^B
]/B
\�B
\]B
[�B
[�B
[�B
[�B
\)B
\)B
\]B
\�B
\�B
]/B
]/B
]/B
]�B
]�B
^jB
_pB
_;B
_pB
_�B
_�B
_pB
_�B
_�B
`B
`BB
a|B
a|B
aB
a|B
bB
a|B
b�B
c B
b�B
bNB
b�B
b�B
c�B
c�B
d�B
dZB
dZB
d�B
d�B
d&B
d�B
e�B
f�B
f�B
f�B
gmB
h>B
h�B
h�B
iB
h�B
i�B
iB
h�B
h�B
h�B
h>B
h�B
g�B
hsB
h
B
h>B
h
B
g8B
hsB
gmB
f�B
ffB
g8B
gB
g8B
g8B
g8B
g�B
hsB
iDB
jKB
jB
k�B
lWB
l�B
m)B
m]B
m�B
m)B
m)B
m�B
m�B
ncB
o5B
o5B
o�B
o5B
pB
o�B
qB
qvB
qvB
qAB
q�B
qvB
rGB
q�B
r|B
rB
r|B
r�B
s�B
s�B
tB
s�B
sMB
s�B
tTB
s�B
tTB
t�B
tB
tTB
t�B
t�B
tB
tB
s�B
s�B
sMB
tB
t�B
t�B
t�B
t�B
t�B
tTB
t�B
t�B
s�B
s�B
tB
s�B
sMB
r�B
sMB
r|B
qvB
q�B
q�B
rGB
r�B
u�B
v+B
u�B
u�B
u�B
v+B
v+B
v+B
w�B
x�B
x�B
x�B
y>B
y>B
yrB
y�B
y�B
yrB
y>B
x�B
y�B
y	B
y	B
yrB
y�B
zDB
{�B
{B
z�B
zxB
z�B
{B
{B
{B
|PB
{�B
{�B
|B
{�B
|B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~]B
~]B
~�B
~]B
~�B
~�B
~�B
.B
�B
�4B
�B
� B
�iB
�B
�B
��B
�;B
�;B
��B
�uB
��B
��B
�GB
��B
�{B
�B
��B
�{B
�B
�B
�B
��B
�SB
��B
�SB
�SB
��B
�%B
�YB
��B
��B
��B
��B
��B
�1B
�1B
��B
�fB
�1B
�1B
�fB
�fB
�fB
�fB
�B
�7B
��B
�B
��B
�B
��B
��B
�7B
��B
�lB
�	B
�lB	JXB	JXB	NpB	J#B	IRB	MjB	GB	I�B	J�B	PHB	K)B	K�B	L�B	U�B	K^B	G�B	J�B	F�B	F�B	K^B	IRB	J#B	IRB	E9B	M�B	I�B	I�B	J�B	HKB	J�B	J�B	I�B	J�B	J�B	K�B	J#B	IB	K�B	K�B	I�B	IB	J�B	K^B	JXB	IB	JXB	K)B	I�B	H�B	I�B	J�B	JXB	H�B	J�B	K)B	I�B	IB	JXB	J�B	I�B	I�B	K)B	J�B	I�B	I�B	K^B	J�B	IRB	IRB	J�B	J�B	I�B	I�B	K^B	K)B	IRB	I�B	K�B	J�B	IRB	J�B	K)B	JXB	I�B	J#B	K�B	J�B	IRB	J�B	K�B	J�B	IRB	J�B	K^B	JXB	IRB	J�B	K)B	I�B	J#B	K�B	J�B	IRB	K)B	K)B	I�B	J#B	K�B	J�B	J#B	K)B	K)B	I�B	I�B	K�B	K^B	I�B	I�B	J�B	K^B	I�B	J#B	K�B	J�B	I�B	I�B	K^B	K)B	I�B	JXB	K�B	J�B	I�B	I�B	K)B	K)B	I�B	JXB	K�B	K)B	I�B	J#B	K�B	K^B	I�B	J#B	K^B	K�B	J#B	I�B	J#B	K�B	J�B	I�B	J�B	K�B	K^B	J#B	JXB	K�B	K�B	J�B	J#B	K)B	L0B	K�B	J�B	K�B	MB	L�B	K^B	L�B	MjB	LdB	J�B	L0B	MjB	MjB	K�B	K^B	L�B	M�B	L0B	K)B	K�B	L�B	NB	M�B	L�B	MjB	N<B	OBB	OBB	NpB	OvB	S&B	TaB	S&B	T�B	V�B	V9B	U�B	VB	XB	Y�B	YB	\)B	\�B	b�B	dZB	e�B	f�B	g�B	jB	h�B	jB	k�B	oiB	r�B	��B	��B	�uB	��B	�B	��B	�B	�GB	��B	��B	�{B	��B	�uB	�{B	��B	�AB	��B	�B	�{B	�{B	�AB	��B	�;B	�B	��B	�AB	��B	�oB	�uB	��B	��B	��B	�%B	�1B	�fB	��B	�%B	��B	��B	�+B	�B	��B	�1B	�_B	��B	�fB	�B	�1B	��B	��B	�lB	��B	��B	��B	��B	��B	�_B	��B	�7B	�xB	�B	�7B	��B	��B	��B	��B	�tB	��B	�'B	��B	��B	�B	��B	�XB	��B	��B	��B	��B	��B	��B	�hB	��B	��B	خB	چB	�B	�B
_B
33B
1'B
;�B
V�B
^jB
_pB
_�B
dZB
i�B
j�B
r|B
z�B
��B
��B
��B
�$B
�OB
�9B
�B
�<B
��B
��B
��B
��B
چB
�dB
��B
��B
�)B
�"B
��B�B�B	�B
rBDB�B�BbBB�B�B�B�BkB�B!�B)�B49B9$BK�B@B@�B?HBB[BNBP�BP}BS&BV�BUgBV�BW�BY�B_�BXB_�BV�B\�BYKBV�BVBVB]dB��BpBqABp�Br�BsMBv+Bu�Bw2By>Bv+BtTBtBr�Br�Bw�Bt�BxBs�BsBw�B|B|�Bw�B��BzB��B�B�qB��B��B��B�!B��B�B�~B�B��B�OB�'B��B��B��B�B��B�4B��B��B��B�bB�-B�bB�bB�hB��B�B�B�:B��B��B�FB��B��B�_B��B�hB��B�XB��B��B�^B�B�qB��B��B�UB�tB��B��B�B��B��B��B�RB�-B��B��B��B��B�B�B��B�tB�:B�nB�0B�OB�B��B��B��B�"B�9B��BϫB�<B��B�XB�}B��B�BбB҉B�B� B˒B��BB��B�[BĜB�9B��B�B��B�$B��B��B�?B�9B��B�hB�B��B�B��B�B��B�IB�wB��B�eB�0B��B�@B�VB�B��B��B�B~�B��Bx�Bv�B�4BqABh�Bd�BZ�BS&BK�BFB>�B7LB8RB=<BCaB-CB3hB\B�B	BxB
�JB
�B
�B
�B
��B
��B
�|B
�oB
�B
�DB
�ZB
�B
��B
��B
�B
�HB
��B
ʌB
�^B
�#B
��B
�aB
ȀB
��B
��B
��B
��B
��B
��B
��B
�*B
�B
��B
v+B
y>B
lWB
j�B
i�B
g�B
h
B
jKB
l�B
k�B
\�B
XB
h
B
c B
cTB
IRB
!-B
 �B
�B
~B
%FB
'�B
eB
JB	�rB
�B	�.B	��B	�PB	�B	�]B	��B	��B	��B	� B	�"B	چB	�B
�B
:^B	רB	��B	�TB	��B	��B	�~B	��B	��B	�jB	ÖB	�OB	�hB	�B	�=B	y>B	.B	j�B	gmB	h�B	n�B	t�B	m)B	q�B	\)B	^�B	]�B	YKB	[#B	X�B	RTB	P}B	OvB	L�B	O�B	L�B	qvB	GzB	C�B	I�B	GzB	8B	8B	7�B	D�B	B�B	:�B	K^B	'�B	'�B	kB	*�B	5�B	1�B	CB	_B	YB	�B	=B	.B	�B	�B	~B	xB		lB	
=B	�B	�B	%B	�B	B		lB�B�8B�cB��B	B	B	.B�xB��B	�B	AB�"B��B�2B�B�oB�AB��B��B�B	�B��B�oB��B��B�B�B�B��B��B�B�B��B��B�%B�AB��B�TB��B��B�GB�GB�AB�B	�B�%B��B�ZB��B��B�ZB��B�B�B�B�B�B��B�%B��B�B�B�B�B�B�JB	 �B��B��B��B��B�B�GB��B�vB�B�AB�B�;B��B�B�5B� B�5B��B�oB�B�B��B�B�B��B�B��B�B�DB�DB�B�JB	+B�B�)B��BںBچBچB�WBںB�QB�B��B�B�WB��B��B�
B�BܒB�B�fB�BޞB�TB�B�`B�fB�B��B�B��B�2B�B�B��B�KB��B�B��B�B�B�B�B��B��B�WB��B�B�NB�ZB�B��B�B��B�"B�5B�>B�lB�(B	�B	�B	uB	�B	�B		�B	B	VB	B	xB	�B	�B	_B	�B	CB	�B	�B	~B	B	$@B	>B	6�B	6FB	6FB	7�B	8B	8�B	8�B	7LB	8�B	<�B	=qB	>B	?�B	C-B	B�B	A B	@�B	A�B	D3B	EmB	C�B	GzB	K^B	P}B	OvB	L�B	O�B	P�B	O�B	PHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                      B	K�B	L~B	NpB	O(B	K^B	LB	MPB	LJB	KDB	J�B	J�B	J�B	JXB	JXB	JrB	J=B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	J�B	KB	KB	LJB	LdB	L�B	N�B	UgB	]�B	r�B	�jB	��B	��B
'B
��B
�7B�B#�BKBaHBshB��B�B��B��B��B�iB��B��B��B�B�;BּBɆB��B�OB�MB��Bl�BI�B�B
�9B
��B
ϑB
�8B
z^B
{JB
5�B
)B
6B
�B	��B	�sB	wfB	ezB	[WB	V9B	K�B	;JB	"�B	�B	$B	�B	NB�<B��B�B�*B�RB��B�fB��B��B�B�^B	FB	,WB	>�B	K)B	T�B	S�B	VSB	^�B	e�B	a�B	b4B	tB	}�B	��B	��B	��B	�cB	�}B	�B	�SB	�=B	��B	��B	�4B	�-B
�B
�B
MB
�B
"4B
$�B
"hB
%�B
*�B
.�B
/�B
.�B
5�B
6B
=�B
A�B
@B
AoB
@�B
A�B
A�B
AUB
AUB
@�B
@iB
@�B
:�B
7B
8lB
>(B
@�B
EB
GEB
HfB
I7B
J�B
J�B
MB
M�B
O�B
OB
OB
N�B
N�B
N�B
N�B
N�B
OB
O(B
OBB
PB
QB
R�B
R B
S�B
S�B
R B
R B
Q�B
Q�B
R:B
Q�B
P�B
QhB
R�B
SuB
S�B
RB
Q�B
Q4B
QhB
Q4B
PB
O�B
N�B
N�B
P}B
Q4B
O�B
N�B
N<B
O\B
N�B
OB
K�B
JXB
I�B
H�B
IB
H�B
LdB
N"B
MB
J	B
H�B
G�B
G_B
G_B
G�B
F?B
G_B
F�B
C�B
B�B
B�B
C�B
B�B
B�B
B�B
B'B
@ B
?HB
<�B
<B
;�B
<B
<�B
=�B
<�B
<B
;B
=�B
9rB
8B
7fB
6�B
5�B
6`B
6`B
6�B
5B
3�B
3�B
3hB
1�B
0UB
/�B
/B
*�B
)�B
($B
&�B
%�B
"hB
!�B
 �B
 \B
�B
IB
�B
)B
�B
#B
�B
�B
�B
B
�B
KB
�B
B
2B
�B
MB
�B
MB
SB
2B
2B
MB
MB
gB
B
EB
�B
SB
mB
�B
eB
�B
�B
$B
gB
,B
�B
BB
(B
bB
�B
�B
�B
�B
�B
�B
�B
JB
VB
BB
�B
pB
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
6B
B
"B
�B
�B
�B
JB
�B
B
B
6B
JB
�B
�B
�B
	RB
	�B

�B
KB
fB
�B
	�B

#B
�B

�B

�B

�B
	�B
	RB
	�B

�B
�B
�B
�B
DB

�B
DB
B
�B
6B
B
�B
"B
�B
HB
�B
4B
 B
 B
�B
B
4B
 B
�B
:B
�B
&B
�B
�B
�B
�B
�B
&B
[B
@B
@B
uB
�B
�B
�B
aB
�B
MB
�B
�B
gB
gB
�B
�B
�B
�B
gB
mB
�B
MB
�B
�B
�B
B

B
�B
EB
B
�B
EB
1B
_B

B
�B
B
�B
�B
	B
�B
xB
IB
dB
dB
~B
B
�B
�B
IB
B
�B
jB
�B
/B
�B
�B
dB
/B
/B
IB
~B
�B
�B
B
5B
�B
�B
5B
�B
�B
�B
!B
;B
�B
 \B
 'B
 �B
!-B
!HB
!�B
"B
"4B
"hB
# B
"hB
#B
#�B
#�B
#�B
#�B
$tB
%`B
%�B
&LB
&�B
'B
'B
'�B
'mB
'B
&�B
'RB
(�B
'�B
'�B
'�B
'�B
(�B
)*B
($B
)�B
)�B
*eB
)�B
)�B
)DB
)_B
)�B
*�B
+�B
,�B
-�B
-�B
./B
.�B
.�B
/ B
.�B
/iB
0B
/�B
/�B
/�B
0B
0�B
0UB
1B
1[B
1�B
1�B
2B
2B
2�B
2aB
2|B
3�B
3�B
3�B
3�B
49B
5B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5ZB
6�B
7�B
:*B
;JB
;0B
;B
;JB
:�B
;�B
<6B
;�B
=B
<�B
=VB
=qB
=qB
>wB
>�B
?.B
?�B
?.B
?HB
>�B
@4B
@iB
AUB
A�B
A�B
B[B
CB
CB
B�B
B�B
CGB
C�B
DMB
C�B
DMB
D�B
D�B
D�B
D�B
D�B
D�B
E9B
E�B
E�B
FYB
E�B
G�B
GEB
F�B
G�B
G�B
GzB
G�B
GEB
G+B
G_B
HB
G�B
G�B
HB
HB
HfB
HB
H1B
HKB
IB
IB
I7B
IlB
I�B
J=B
J	B
J=B
J�B
J�B
K^B
K�B
K�B
LB
K�B
K�B
LJB
K�B
LB
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M6B
MjB
N<B
M�B
M�B
M�B
M�B
M�B
NB
NB
N"B
N<B
N<B
N�B
N�B
OBB
O�B
O�B
P}B
P�B
P�B
PbB
P�B
Q�B
R�B
R�B
S[B
S�B
S�B
TB
T,B
T�B
T�B
T�B
T{B
TaB
TaB
T{B
UB
U�B
T�B
T{B
T�B
T{B
T�B
T�B
T�B
T�B
T�B
U2B
U�B
U�B
X_B
YB
YeB
YB
ZkB
ZB
ZB
Y�B
YeB
ZQB
[�B
[�B
\CB
[�B
[qB
[�B
\]B
[�B
Z�B
\]B
_!B
_!B
^B
^5B
_VB
^5B
]~B
]~B
\�B
\CB
\CB
\B
[�B
\]B
\xB
\�B
\�B
]/B
]IB
]dB
]~B
^jB
^jB
_B
_�B
_pB
_�B
_�B
_�B
_�B
`\B
`'B
`BB
`�B
a�B
a|B
aHB
a�B
bNB
a�B
c:B
c:B
b�B
b�B
c B
cnB
d&B
d&B
d�B
dtB
dtB
eB
d�B
dB
ezB
ffB
f�B
f�B
gB
g�B
hsB
iB
h�B
i*B
i*B
i�B
iyB
iB
h�B
h�B
h�B
i_B
h$B
h�B
hXB
hsB
h>B
h$B
iDB
g�B
f�B
f�B
g�B
g8B
gRB
g�B
gmB
g�B
h�B
j0B
jeB
i�B
k�B
lWB
l�B
mwB
m�B
m�B
mCB
mwB
n/B
n/B
n�B
oiB
oiB
o�B
oOB
p!B
p;B
q'B
q�B
qvB
q[B
q�B
q�B
raB
q�B
r�B
rGB
r�B
sB
s�B
s�B
t9B
s�B
shB
s�B
tnB
s�B
t�B
t�B
tTB
t�B
uB
t�B
t9B
t9B
tB
tB
s�B
t�B
u%B
uB
uB
u?B
t�B
t�B
uB
t�B
s�B
tB
tTB
tB
s�B
sMB
tTB
r�B
q�B
q�B
q�B
r�B
s�B
v`B
v`B
vB
vB
v+B
vzB
v�B
wB
xRB
y	B
x�B
x�B
yXB
yXB
y�B
y�B
y�B
yrB
y>B
x�B
y�B
yrB
y�B
y�B
y�B
z^B
|B
{JB
z�B
z�B
{0B
{JB
{�B
|jB
|jB
|B
{�B
|6B
|6B
|jB
|�B
}<B
|�B
}�B
}�B
~B
~B
}�B
}�B
~�B
~wB
~wB
~�B
~�B
.B
~�B
~�B
cB
�4B
�OB
�B
� B
�iB
�B
�B
��B
�oB
��B
�B
�uB
�B
�B
��B
��B
��B
�3B
��B
��B
�gB
�gB
�gB
��B
�mB
��B
�mB
��B
��B
�?B
��B
�+B
��B
�B
��B
��B
�fB
�KB
�B
��B
�KB
�fB
��B
��B
��B
��B
�7B
�7B
��B
�B
��B
�RB
��B
��B
�RB
��B
��B
�	G�O�B	JXB	JXB	NpB	J#B	IRB	MjB	GB	I�B	J�B	PHB	K)B	K�B	L�B	U�B	K^B	G�B	J�B	F�B	F�B	K^B	IRB	J#B	IRB	E9B	M�B	I�B	I�B	J�B	HKB	J�B	J�B	I�B	J�B	J�B	K�B	J#B	IB	K�B	K�B	I�B	IB	J�B	K^B	JXB	IB	JXB	K)B	I�B	H�B	I�B	J�B	JXB	H�B	J�B	K)B	I�B	IB	JXB	J�B	I�B	I�B	K)B	J�B	I�B	I�B	K^B	J�B	IRB	IRB	J�B	J�B	I�B	I�B	K^B	K)B	IRB	I�B	K�B	J�B	IRB	J�B	K)B	JXB	I�B	J#B	K�B	J�B	IRB	J�B	K�B	J�B	IRB	J�B	K^B	JXB	IRB	J�B	K)B	I�B	J#B	K�B	J�B	IRB	K)B	K)B	I�B	J#B	K�B	J�B	J#B	K)B	K)B	I�B	I�B	K�B	K^B	I�B	I�B	J�B	K^B	I�B	J#B	K�B	J�B	I�B	I�B	K^B	K)B	I�B	JXB	K�B	J�B	I�B	I�B	K)B	K)B	I�B	JXB	K�B	K)B	I�B	J#B	K�B	K^B	I�B	J#B	K^B	K�B	J#B	I�B	J#B	K�B	J�B	I�B	J�B	K�B	K^B	J#B	JXB	K�B	K�B	J�B	J#B	K)B	L0B	K�B	J�B	K�B	MB	L�B	K^B	L�B	MjB	LdB	J�B	L0B	MjB	MjB	K�B	K^B	L�B	M�B	L0B	K)B	K�B	L�B	NB	M�B	L�B	MjB	N<B	OBB	OBB	NpB	OvB	S&B	TaB	S&B	T�B	V�B	V9B	U�B	VB	XB	Y�B	YB	\)B	\�B	b�B	dZB	e�B	f�B	g�B	jB	h�B	jB	k�B	oiB	r�B	��B	��B	�uB	��B	�B	��B	�B	�GB	��B	��B	�{B	��B	�uB	�{B	��B	�AB	��B	�B	�{B	�{B	�AB	��B	�;B	�B	��B	�AB	��B	�oB	�uB	��B	��B	��B	�%B	�1B	�fB	��B	�%B	��B	��B	�+B	�B	��B	�1B	�_B	��B	�fB	�B	�1B	��B	��B	�lB	��B	��B	��B	��B	��B	�_B	��B	�7B	�xB	�B	�7B	��B	��B	��B	��B	�tB	��B	�'B	��B	��B	�B	��B	�XB	��B	��B	��B	��B	��B	��B	�hB	��B	��B	خB	چB	�B	�B
_B
33B
1'B
;�B
V�B
^jB
_pB
_�B
dZB
i�B
j�B
r|B
z�B
��B
��B
��B
�$B
�OB
�9B
�B
�<B
��B
��B
��B
��B
چB
�dB
��B
��B
�)B
�"B
��B�B�B	�B
rBDB�B�BbBB�B�B�B�BkB�B!�B)�B49B9$BK�B@B@�B?HBB[BNBP�BP}BS&BV�BUgBV�BW�BY�B_�BXB_�BV�B\�BYKBV�BVBVB]dB��BpBqABp�Br�BsMBv+Bu�Bw2By>Bv+BtTBtBr�Br�Bw�Bt�BxBs�BsBw�B|B|�Bw�B��BzB��B�B�qB��B��B��B�!B��B�B�~B�B��B�OB�'B��B��B��B�B��B�4B��B��B��B�bB�-B�bB�bB�hB��B�B�B�:B��B��B�FB��B��B�_B��B�hB��B�XB��B��B�^B�B�qB��B��B�UB�tB��B��B�B��B��B��B�RB�-B��B��B��B��B�B�B��B�tB�:B�nB�0B�OB�B��B��B��B�"B�9B��BϫB�<B��B�XB�}B��B�BбB҉B�B� B˒B��BB��B�[BĜB�9B��B�B��B�$B��B��B�?B�9B��B�hB�B��B�B��B�B��B�IB�wB��B�eB�0B��B�@B�VB�B��B��B�B~�B��Bx�Bv�B�4BqABh�Bd�BZ�BS&BK�BFB>�B7LB8RB=<BCaB-CB3hB\B�B	BxB
�JB
�B
�B
�B
��B
��B
�|B
�oB
�B
�DB
�ZB
�B
��B
��B
�B
�HB
��B
ʌB
�^B
�#B
��B
�aB
ȀB
��B
��B
��B
��B
��B
��B
��B
�*B
�B
��B
v+B
y>B
lWB
j�B
i�B
g�B
h
B
jKB
l�B
k�B
\�B
XB
h
B
c B
cTB
IRB
!-B
 �B
�B
~B
%FB
'�B
eB
JB	�rB
�B	�.B	��B	�PB	�B	�]B	��B	��B	��B	� B	�"B	چB	�B
�B
:^B	רB	��B	�TB	��B	��B	�~B	��B	��B	�jB	ÖB	�OB	�hB	�B	�=B	y>B	.B	j�B	gmB	h�B	n�B	t�B	m)B	q�B	\)B	^�B	]�B	YKB	[#B	X�B	RTB	P}B	OvB	L�B	O�B	L�B	qvB	GzB	C�B	I�B	GzB	8B	8B	7�B	D�B	B�B	:�B	K^B	'�B	'�B	kB	*�B	5�B	1�B	CB	_B	YB	�B	=B	.B	�B	�B	~B	xB		lB	
=B	�B	�B	%B	�B	B		lB�B�8B�cB��B	B	B	.B�xB��B	�B	AB�"B��B�2B�B�oB�AB��B��B�B	�B��B�oB��B��B�B�B�B��B��B�B�B��B��B�%B�AB��B�TB��B��B�GB�GB�AB�B	�B�%B��B�ZB��B��B�ZB��B�B�B�B�B�B��B�%B��B�B�B�B�B�B�JB	 �B��B��B��B��B�B�GB��B�vB�B�AB�B�;B��B�B�5B� B�5B��B�oB�B�B��B�B�B��B�B��B�B�DB�DB�B�JB	+B�B�)B��BںBچBچB�WBںB�QB�B��B�B�WB��B��B�
B�BܒB�B�fB�BޞB�TB�B�`B�fB�B��B�B��B�2B�B�B��B�KB��B�B��B�B�B�B�B��B��B�WB��B�B�NB�ZB�B��B�B��B�"B�5B�>B�lB�(B	�B	�B	uB	�B	�B		�B	B	VB	B	xB	�B	�B	_B	�B	CB	�B	�B	~B	B	$@B	>B	6�B	6FB	6FB	7�B	8B	8�B	8�B	7LB	8�B	<�B	=qB	>B	?�B	C-B	B�B	A B	@�B	A�B	D3B	EmB	C�B	GzB	K^B	P}B	OvB	L�B	O�B	P�B	O�B	PHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D=0<Kڦ<���<���<�T�<4W�<#�
<#�
<'�<#�
<#�
<#�
<���<czg<#�
<#�
<7S?<���</��<#�
<�w<�Q�<r�W<1_<#�
<#�
<M��<t�9<���<r�W<�E�<#�
<#�
<#�
<t�9<�μ<k9<�n�<N�<#�
<�(�<�`<��<H�x<;*M<#�
<,:</1�<�K<t=<#�
<#�
<>.v<���<j�<9�<JS�<#�
<#�
<#�
<d}�<x��<���</1�<V�,<N�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018070201254220180702012542IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018071207041620180712070416QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018071207041620180712070416QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550920190521075509IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                