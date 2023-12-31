CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2023-01-14T22:04:44Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  U�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  [�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  t<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  z`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $ P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $ �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � &    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` >�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ?   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   E   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   K   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T Q   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   Q\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   Qd   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   Ql   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   Qt   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � Q|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   Q�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   R   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    R    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        R@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        RH   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       RP   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    RXArgo profile    3.1 1.2 19500101000000  20230114220444  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_244                 6810_008521_244                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�4>BZ�@�4>BZ�11  @�4q��@�4q��@2�j��f�@2�j��f��d��x�d��x11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @@  @�  @��R@�G�@�\A ��A  A\)A,(�A?\)A`  A�  A�  A�Q�A��A�Q�A�  A�
=A�B   B  B(�B(�B (�B(  B/�
B8(�B@Q�BH(�BP(�BX  B`  BhQ�BpQ�Bx(�B�
B�B��
B��B�  B�{B�{B�{B�  B��B��
B��B�  B�  B�  B�  B�  B�(�B�=qB�(�B��
B�  B�=qB�=qB�  B�B��B�  B�(�B�(�B�(�B�(�C 
=C��C��C��C��C

=C
=C
=C{C  C�C  C  C
=C��C  C 
=C"
=C$  C%��C(  C*  C+��C-��C0  C2
=C4
=C6
=C8
=C:  C;��C>  C@  CA��CD  CF  CG��CJ  CK��CN  CP  CR  CT
=CV
=CX
=CZ
=C\{C^{C`  Cb  Cd
=Cf  Cg��Cj  Ck��Cn
=Co��Cr  Cs��Cv  Cx  Cz  C|  C~  C�  C�C�  C�  C���C�C���C�C�  C�  C�  C���C���C�  C�  C���C�  C�C�  C�  C�  C�
=C���C�  C�  C�  C���C�  C�  C�C�
=C�
=C�  C���C�  C�C�  C���C�  C�C�  C���C�  C���C�C�C�  C���C�  C���C���C���C���C�C�C���C�C�C�  C���C�
=C�C���C�  C�  C�C�  C�  C�C�C���C���C���C���C���C�C�  C���C���C���C���C�C�C�C���C���C�  C�C���C���C�C�  C���C�  C�  C�  C�C�
=C�C���C���C�C�C�  C�  C�  C���C���C�  C�C�  C�  C�  C�C�C�  C�  C�C�  C���C���C�  C���C�  C�\C�
=C�C�
=D �D � D �qD� D�D� D  D}qD�qD� D  D� D�D�DD}qD  D� D�qD	��D	�qD
z�D  D� DD� D�qD}qD  D� D�D��D�qD}qD  D��D  D� D�D� DD��D  D� D  D� DD�D�qDz�D  D� D�qD� D�qDz�D  D��D�qD� D  D� D�qDz�D�qD � D �qD!� D"  D"}qD#  D#}qD$  D$�D%  D%��D%�qD&}qD'  D'}qD(  D(��D)  D)��D*�D*� D+  D+��D,D,��D,�qD-}qD-�qD.��D/�D/��D0  D0}qD1  D1��D2  D2� D2�qD3� D3�qD4}qD5�D5� D5��D6z�D6��D7� D8�D8z�D9  D9�D:  D:�D;D;��D<�D<� D=�D=��D>�D>��D>��D?��D@�D@� DA  DA� DB  DB� DC  DC}qDD  DD� DE  DE}qDF  DF��DG  DG� DHDH��DI  DI��DJ�DJ��DK  DK� DL  DL� DM�DM� DM�qDNz�DN�qDO��DP�DP}qDP��DQz�DR  DR�DS  DS� DTDT�DUDU��DU�qDV� DW�DW��DX�DX� DY  DY}qDY�qDZ��D[�D[� D[��D\z�D\�qD]� D]�qD^z�D_  D_� D`  D`}qDa  Da� Db�Db��Dc  Dc}qDc�qDd}qDe�De}qDe�qDf� Dg  Dg� Dh  Dh� DiDi� Di�qDj}qDk  Dk��Dl�Dl� Dl��Dmz�Dn�Dn��Do  Do}qDo�qDp� Dp�qDq}qDq�qDr}qDs  Ds}qDs�qDt}qDu  Du��Du�qDv� Dw  Dw� Dx�Dx��Dx�qDyz�Dz  Dz��Dz�qD{� D|�D|� D}  D}z�D}�qD~}qD~��Dz�D�  D�@ D�� D�� D�  D�AHD�� D���D�HD�B�D�� D�� D�HD�@ D�~�D�� D���D�=qD�~�D��HD���D�=qD�}qD���D�  D�@ D��HD�� D��qD�<)D�|)D��qD���D�@ D��HD�� D�HD�@ D�~�D��qD��qD�@ D��HD�� D���D�=qD�}qD���D�  D�>�D�~�D�� D�HD�@ D�� D��HD�HD�AHD��HD���D�  D�@ D�� D�� D���D�@ D�� D��HD�  D�>�D�� D��HD�  D�@ D�� D���D���D�@ D�� D�� D�  D�>�D�� D��HD�  D�@ D�� D�� D���D�@ D��D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?�?aG�?�z�?�33?�@�@z�@0��@:�H@W
=@c�
@}p�@�ff@�33@�(�@��@�33@��H@�ff@�\)@�Q�@�ff@�\)@���A33A
=A��A�A�A(�A\)A%A+�A.�RA3�
A9��A=p�ADz�AG
=AMp�AR�\AW
=A]p�A`��Ag
=Aj�HAp  AuAy��A~�RA�=qA�(�A��RA���A�33A��RA�Q�A��HA�A�\)A�=qA��A�ffA���A��A�{A���A��\A�A��A�=qA��A��RA��A�(�A�{A���AÅA�p�AǮAʏ\A�z�A�\)A��A��
A�ffA�G�A�33A�p�A��A�\A��A�  A陚A�z�A�\)A���A�(�A��RA�Q�A��
A�A��Bp�B=qB�B�B�B\)B��B	��B\)BQ�Bp�B
=B  B�B�HB�
B��B�\B�B��B{B\)BQ�B�B�HB z�B!��B"�\B#�
B%p�B&=qB'�B)�B*{B+�B,��B.{B/�
B0��B2{B3�B4��B6ffB7\)B8��B:=qB;33B<��B>{B?
=B@��BA�BB�HBD��BE��BF�RBHQ�BI�BJ�RBK�
BL��BN�\BO\)BP��BR=qBS
=BTz�BV{BV�HBXz�BYBZ�RB\(�B]G�B^�\B`  B`��Bb=qBc�BdQ�Bf{Bg
=Bh(�BiBj�HBk�
Bmp�Bn�\Bo�Bq�Br=qBs33Bt��Bv{Bv�RBx(�Byp�BzffB|  B}�B~{B�B�Q�B��HB���B�=qB��RB�p�B�(�B���B�G�B�  B��\B�33B��B�Q�B��B��
B�ffB��HB��B�=qB���B���B�{B���B�p�B��B��\B�\)B��
B�z�B�G�B�B�Q�B�
=B��B�(�B���B��B�  B���B�p�B��
B��\B�G�B��B�Q�B��B��B�{B��HB�\)B��
B��RB��B�B�ffB��HB���B�Q�B���B�\)B�(�B��RB��B��B���B�
=B���B�ffB��HB�\)B�(�B���B��B��B�z�B�
=B�p�B�{B���B�33B��
B��\B�
=B��B�(�B���B�33B�B�z�B�
=B��B�(�B��HB�G�B��
B��\B�
=B��B�ffB��HB�\)B�{B���B�G�B��B��RB�33B��B£�B�33B�B�z�B�33BŮB�Q�B��BǙ�B�{B��HBɅB��BʸRB�\)B��
Ḅ�B�\)B��
B�z�B�33B��B�z�B��B�  Bҏ\B�33B�  B���B�G�B��BָRB�p�B�  BظRBمB�  BڸRB�p�B��Bܣ�B�p�B�  Bޏ\B�G�B�{B��\B�\)B�{B�\B�33B�  B��B�33B�{B�\B��B�  B�z�B�33B��B�Q�B�33B�B�=qB��B홚B�=qB�
=BB�(�B�
=B�B�(�B���B�B�=qB�
=B���B�(�B��B��B�=qB�
=B��B�Q�B�
=B���B�ffB��B���B�z�B��B���C =qC �\C ��C=qC�C�HC33C�C��C33C�C�C=qCz�C�C=qC�C�HCG�Cz�C�
CG�Cz�C�
C=qC�C��C	=qC	�C	�
C
G�C
�C
��C(�C�\C��C{Cp�CC
=C=qC��C�C�C\)C��C�
C
=CQ�C��C�RC�HC�C\)Cp�C�C�HC  C{C\)Cp�C�\C��C�HC  CG�CffCz�C��C�HC  C{C=qCp�C�C�RC�C  C{CQ�C�C�\C�RC�C�C33CQ�C�C�CC�HC{CG�C\)Cz�C�C�HC��C
=CQ�Cp�C�C�RC�C
=C{C=qCz�C��C�RC�HC{C33CQ�C�C�RCC��C�CQ�CffC�C�RC�HC��C
=C=qCffC��C�CC  C33CG�CffC�CC��C�C(�C\)C��CC�HC��C33C\)Cp�C�RC�
C�C�CQ�Cp�C�\C�RC��C �C 33C \)C �\C C �HC ��C!33C!\)C!z�C!�\C!�RC!�C"�C"=qC"Q�C"�C"C"�HC"��C#(�C#ffC#z�C#��C#�HC$  C$(�C$=qC$z�C$�C$��C$�C%�C%Q�C%z�C%��C%�RC%�C&�C&G�C&ffC&�C&C&��C'{C'33C'ffC'��C'��C'��C(
=C(=qC(p�C(��C(�RC(�HC){C)G�C)z�C)��C)C)��C*=qC*\)C*z�C*�C*�HC+�C+Q�C+p�C+�\C+C,
=C,=qC,\)C,z�C,�RC,��C-�C-33C-ffC-��C-�
C-��C.�C.G�C.�\C.�RC.�
C/
=C/G�C/�C/��C/��C0  C0=qC0p�C0��C0��C1{C133C1\)C1��C1�HC2  C2(�C2ffC2�C2�
C3  C3(�C3p�C3��C3��C3�C4(�C4ffC4��C4��C4�C5�C5\)C5��C5C5�C6{C6G�C6�C6C6�HC7
=C733C7z�C7�RC7�C8
=C8=qC8z�C8�RC8�HC9
=C9=qC9z�C9C9��C:�C:Q�C:z�C:�RC;  C;33C;\)C;z�C;C<  C<33C<ffC<�C<��C=
=C==qC=p�C=�\C=C=��C>(�C>ffC>��C>�
C?  C?(�C?ffC?��C?�
C?��C@(�C@\)C@��C@�
CA
=CA=qCAffCA�CA�RCA�CB33CBp�CB��CBCB�CC�CC\)CC�\CC�RCC�
CD
=CDG�CDz�CD��CDCD�CE�CEQ�CE�\CE�RCE�
CF  CF33CFp�CF��CF��CF�CG�CGG�CGp�CG�\CGCH  CH33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @   @@  @�  @��R@�G�@�\A ��A  A\)A,(�A?\)A`  A�  A�  A�Q�A��A�Q�A�  A�
=A�B   B  B(�B(�B (�B(  B/�
B8(�B@Q�BH(�BP(�BX  B`  BhQ�BpQ�Bx(�B�
B�B��
B��B�  B�{B�{B�{B�  B��B��
B��B�  B�  B�  B�  B�  B�(�B�=qB�(�B��
B�  B�=qB�=qB�  B�B��B�  B�(�B�(�B�(�B�(�C 
=C��C��C��C��C

=C
=C
=C{C  C�C  C  C
=C��C  C 
=C"
=C$  C%��C(  C*  C+��C-��C0  C2
=C4
=C6
=C8
=C:  C;��C>  C@  CA��CD  CF  CG��CJ  CK��CN  CP  CR  CT
=CV
=CX
=CZ
=C\{C^{C`  Cb  Cd
=Cf  Cg��Cj  Ck��Cn
=Co��Cr  Cs��Cv  Cx  Cz  C|  C~  C�  C�C�  C�  C���C�C���C�C�  C�  C�  C���C���C�  C�  C���C�  C�C�  C�  C�  C�
=C���C�  C�  C�  C���C�  C�  C�C�
=C�
=C�  C���C�  C�C�  C���C�  C�C�  C���C�  C���C�C�C�  C���C�  C���C���C���C���C�C�C���C�C�C�  C���C�
=C�C���C�  C�  C�C�  C�  C�C�C���C���C���C���C���C�C�  C���C���C���C���C�C�C�C���C���C�  C�C���C���C�C�  C���C�  C�  C�  C�C�
=C�C���C���C�C�C�  C�  C�  C���C���C�  C�C�  C�  C�  C�C�C�  C�  C�C�  C���C���C�  C���C�  C�\C�
=C�C�
=D �D � D �qD� D�D� D  D}qD�qD� D  D� D�D�DD}qD  D� D�qD	��D	�qD
z�D  D� DD� D�qD}qD  D� D�D��D�qD}qD  D��D  D� D�D� DD��D  D� D  D� DD�D�qDz�D  D� D�qD� D�qDz�D  D��D�qD� D  D� D�qDz�D�qD � D �qD!� D"  D"}qD#  D#}qD$  D$�D%  D%��D%�qD&}qD'  D'}qD(  D(��D)  D)��D*�D*� D+  D+��D,D,��D,�qD-}qD-�qD.��D/�D/��D0  D0}qD1  D1��D2  D2� D2�qD3� D3�qD4}qD5�D5� D5��D6z�D6��D7� D8�D8z�D9  D9�D:  D:�D;D;��D<�D<� D=�D=��D>�D>��D>��D?��D@�D@� DA  DA� DB  DB� DC  DC}qDD  DD� DE  DE}qDF  DF��DG  DG� DHDH��DI  DI��DJ�DJ��DK  DK� DL  DL� DM�DM� DM�qDNz�DN�qDO��DP�DP}qDP��DQz�DR  DR�DS  DS� DTDT�DUDU��DU�qDV� DW�DW��DX�DX� DY  DY}qDY�qDZ��D[�D[� D[��D\z�D\�qD]� D]�qD^z�D_  D_� D`  D`}qDa  Da� Db�Db��Dc  Dc}qDc�qDd}qDe�De}qDe�qDf� Dg  Dg� Dh  Dh� DiDi� Di�qDj}qDk  Dk��Dl�Dl� Dl��Dmz�Dn�Dn��Do  Do}qDo�qDp� Dp�qDq}qDq�qDr}qDs  Ds}qDs�qDt}qDu  Du��Du�qDv� Dw  Dw� Dx�Dx��Dx�qDyz�Dz  Dz��Dz�qD{� D|�D|� D}  D}z�D}�qD~}qD~��Dz�D�  D�@ D�� D�� D�  D�AHD�� D���D�HD�B�D�� D�� D�HD�@ D�~�D�� D���D�=qD�~�D��HD���D�=qD�}qD���D�  D�@ D��HD�� D��qD�<)D�|)D��qD���D�@ D��HD�� D�HD�@ D�~�D��qD��qD�@ D��HD�� D���D�=qD�}qD���D�  D�>�D�~�D�� D�HD�@ D�� D��HD�HD�AHD��HD���D�  D�@ D�� D�� D���D�@ D�� D��HD�  D�>�D�� D��HD�  D�@ D�� D���D���D�@ D�� D�� D�  D�>�D�� D��HD�  D�@ D�� D�� D���D�@ D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?�?aG�?�z�?�33?�@�@z�@0��@:�H@W
=@c�
@}p�@�ff@�33@�(�@��@�33@��H@�ff@�\)@�Q�@�ff@�\)@���A33A
=A��A�A�A(�A\)A%A+�A.�RA3�
A9��A=p�ADz�AG
=AMp�AR�\AW
=A]p�A`��Ag
=Aj�HAp  AuAy��A~�RA�=qA�(�A��RA���A�33A��RA�Q�A��HA�A�\)A�=qA��A�ffA���A��A�{A���A��\A�A��A�=qA��A��RA��A�(�A�{A���AÅA�p�AǮAʏ\A�z�A�\)A��A��
A�ffA�G�A�33A�p�A��A�\A��A�  A陚A�z�A�\)A���A�(�A��RA�Q�A��
A�A��Bp�B=qB�B�B�B\)B��B	��B\)BQ�Bp�B
=B  B�B�HB�
B��B�\B�B��B{B\)BQ�B�B�HB z�B!��B"�\B#�
B%p�B&=qB'�B)�B*{B+�B,��B.{B/�
B0��B2{B3�B4��B6ffB7\)B8��B:=qB;33B<��B>{B?
=B@��BA�BB�HBD��BE��BF�RBHQ�BI�BJ�RBK�
BL��BN�\BO\)BP��BR=qBS
=BTz�BV{BV�HBXz�BYBZ�RB\(�B]G�B^�\B`  B`��Bb=qBc�BdQ�Bf{Bg
=Bh(�BiBj�HBk�
Bmp�Bn�\Bo�Bq�Br=qBs33Bt��Bv{Bv�RBx(�Byp�BzffB|  B}�B~{B�B�Q�B��HB���B�=qB��RB�p�B�(�B���B�G�B�  B��\B�33B��B�Q�B��B��
B�ffB��HB��B�=qB���B���B�{B���B�p�B��B��\B�\)B��
B�z�B�G�B�B�Q�B�
=B��B�(�B���B��B�  B���B�p�B��
B��\B�G�B��B�Q�B��B��B�{B��HB�\)B��
B��RB��B�B�ffB��HB���B�Q�B���B�\)B�(�B��RB��B��B���B�
=B���B�ffB��HB�\)B�(�B���B��B��B�z�B�
=B�p�B�{B���B�33B��
B��\B�
=B��B�(�B���B�33B�B�z�B�
=B��B�(�B��HB�G�B��
B��\B�
=B��B�ffB��HB�\)B�{B���B�G�B��B��RB�33B��B£�B�33B�B�z�B�33BŮB�Q�B��BǙ�B�{B��HBɅB��BʸRB�\)B��
Ḅ�B�\)B��
B�z�B�33B��B�z�B��B�  Bҏ\B�33B�  B���B�G�B��BָRB�p�B�  BظRBمB�  BڸRB�p�B��Bܣ�B�p�B�  Bޏ\B�G�B�{B��\B�\)B�{B�\B�33B�  B��B�33B�{B�\B��B�  B�z�B�33B��B�Q�B�33B�B�=qB��B홚B�=qB�
=BB�(�B�
=B�B�(�B���B�B�=qB�
=B���B�(�B��B��B�=qB�
=B��B�Q�B�
=B���B�ffB��B���B�z�B��B���C =qC �\C ��C=qC�C�HC33C�C��C33C�C�C=qCz�C�C=qC�C�HCG�Cz�C�
CG�Cz�C�
C=qC�C��C	=qC	�C	�
C
G�C
�C
��C(�C�\C��C{Cp�CC
=C=qC��C�C�C\)C��C�
C
=CQ�C��C�RC�HC�C\)Cp�C�C�HC  C{C\)Cp�C�\C��C�HC  CG�CffCz�C��C�HC  C{C=qCp�C�C�RC�C  C{CQ�C�C�\C�RC�C�C33CQ�C�C�CC�HC{CG�C\)Cz�C�C�HC��C
=CQ�Cp�C�C�RC�C
=C{C=qCz�C��C�RC�HC{C33CQ�C�C�RCC��C�CQ�CffC�C�RC�HC��C
=C=qCffC��C�CC  C33CG�CffC�CC��C�C(�C\)C��CC�HC��C33C\)Cp�C�RC�
C�C�CQ�Cp�C�\C�RC��C �C 33C \)C �\C C �HC ��C!33C!\)C!z�C!�\C!�RC!�C"�C"=qC"Q�C"�C"C"�HC"��C#(�C#ffC#z�C#��C#�HC$  C$(�C$=qC$z�C$�C$��C$�C%�C%Q�C%z�C%��C%�RC%�C&�C&G�C&ffC&�C&C&��C'{C'33C'ffC'��C'��C'��C(
=C(=qC(p�C(��C(�RC(�HC){C)G�C)z�C)��C)C)��C*=qC*\)C*z�C*�C*�HC+�C+Q�C+p�C+�\C+C,
=C,=qC,\)C,z�C,�RC,��C-�C-33C-ffC-��C-�
C-��C.�C.G�C.�\C.�RC.�
C/
=C/G�C/�C/��C/��C0  C0=qC0p�C0��C0��C1{C133C1\)C1��C1�HC2  C2(�C2ffC2�C2�
C3  C3(�C3p�C3��C3��C3�C4(�C4ffC4��C4��C4�C5�C5\)C5��C5C5�C6{C6G�C6�C6C6�HC7
=C733C7z�C7�RC7�C8
=C8=qC8z�C8�RC8�HC9
=C9=qC9z�C9C9��C:�C:Q�C:z�C:�RC;  C;33C;\)C;z�C;C<  C<33C<ffC<�C<��C=
=C==qC=p�C=�\C=C=��C>(�C>ffC>��C>�
C?  C?(�C?ffC?��C?�
C?��C@(�C@\)C@��C@�
CA
=CA=qCAffCA�CA�RCA�CB33CBp�CB��CBCB�CC�CC\)CC�\CC�RCC�
CD
=CDG�CDz�CD��CDCD�CE�CEQ�CE�\CE�RCE�
CF  CF33CFp�CF��CF��CF�CG�CGG�CGp�CG�\CGCH  CH33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�Aϧ�AϬAϬAϰ!AϮAϬAϮAϰ!Aϰ!AϮAϬAϬAϩ�Aϩ�Aϩ�AϬAϬAϬAϬAϬAϬAϬAϩ�Aϥ�Aϣ�Aϥ�Aϧ�Aϧ�Aϧ�Aϧ�AϮA϶FAϸRAϼjAϾwA��`A���A��/A�ĜAϗ�A�|�A�\)A�5?A��A�%A��A���AΕ�A�5?A��
A��
A��A�(�A�~�A��A�oA��
Aɩ�AɑhAɏ\A�jA�1'A�dZA�(�A�1'A��/A�+A�v�A���A��A���A�A�/A�bA�E�A�?}A�r�A��FA���A�t�A��A���A��`A�A�oA�A�A���A�S�A���A��A��A�VA�"�A�C�A��uA��A��^A�+A���A�r�A�JA�v�A��`A�S�A��9A�~�A�=qA��\A�`BA�VAG�A|�A{oAw�-At=qAs%Ar  Ao�;An^5Am�mAm;dAln�AjAgS�AchsAa
=A\n�AZAYoAVE�AS�-AQ�
AQ�AO;dAKXAJE�AH��AE�AD�yADz�AD$�AC
=A@9XA>ffA<bNA7O�A4�A3�;A1�FA0(�A/K�A//A.�`A+��A)hsA(��A'`BA%A$�A$A�A"��A"I�A!��A n�A -A�TA��A�Ax�A�A�#A�A33Ar�A5?A��A��A?}A�DA�A��A�`A�DAI�AƨAK�AM�A�A?}AffA��AVA
n�A
jA
1'A	��A	
=A�HAv�AĜA	O�A	�A�!AQ�A-A�7A��AM�A�\A��A�/A�\AM�A$�A5?A��A&�AĜA�uAI�A�mA;dAE�A&�A �HA �!@���@��`@�/@��@��@�C�@�j@�h@�Q�@�ƨ@�dZ@�o@��H@�R@�M�@�5?@�$�@�J@�$�@�V@��@��@�P@�(�@��@�&�@��@���@�b@�33@�=q@�%@�@�\@�ȴ@��@�+@�=q@���@�^@�h@��/@���@�5?@��@��@߅@�ƨ@��;@��y@��@�o@�"�@�"�@�o@��y@�~�@�@ݺ^@�&�@�j@�K�@��@��@ش9@�Z@�Q�@�A�@�Q�@�1@ם�@�K�@�33@�"�@���@�v�@թ�@��/@�ƨ@ҏ\@�{@�p�@���@Ь@ϥ�@�n�@ͺ^@�Q�@˅@�9X@�(�@�v�@���@�7L@��@��@ǥ�@��/@�\)@ǝ�@��m@��@�hs@��@ļj@�j@�r�@�I�@�(�@��y@�V@�Z@�A�@�V@��@�$�@��@�/@�j@��F@���@�V@�@�hs@�7L@��@��@�Ĝ@�bN@�1@��w@��P@��H@�~�@�~�@�M�@�M�@�=q@��T@�x�@��@���@��@��D@�z�@�r�@�j@�A�@��@�l�@�C�@��R@�V@�{@��@���@�%@�Ĝ@�Q�@��@���@���@���@�C�@��@�M�@���@��@��@��@�;d@�
=@��@��H@��+@��T@��7@�7L@��9@�bN@�r�@�Z@���@�dZ@�;d@�@���@�V@���@�7L@�&�@�&�@��@�%@���@���@��@�+@��!@��@�hs@�?}@�&�@�%@�z�@�I�@��@���@��;@��@�33@�
=@���@�-@���@�@���@�V@��/@�Ĝ@��u@�b@���@�+@��!@�^5@���@�`B@��@���@��9@��D@��@���@��m@��@�
=@��@��@���@���@�^5@�@���@�X@�X@�X@�%@��/@��/@��/@���@���@���@�r�@�  @��w@�l�@�@��y@��H@���@��\@�ff@�{@��#@���@�hs@��@�%@��u@�1@��@�\)@�o@��@���@���@�^5@��@�x�@�V@�V@���@��@� �@���@�K�@�ȴ@��R@���@��+@�v�@�M�@�$�@��@���@�V@��9@�%@���@�Q�@�(�@���@���@���@�l�@�;d@�o@��@��y@���@��R@��R@���@�V@���@���@��h@�hs@�G�@��@��9@���@��@�j@�Q�@�I�@�1'@�1@�|�@�\)@�;d@���@���@�E�@��T@���@�x�@�`B@�G�@�&�@�%@�Ĝ@��D@�Z@�b@��@+@~��@�w@\)@�@~�+@}�T@}`B@}�@|�@|(�@{�F@{C�@z�H@z~�@z-@y�@y��@yhs@y7L@x��@x�@x  @w|�@w
=@v��@v@uV@t9X@s�
@s��@s33@r~�@q�7@qhsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aϛ�Aϗ�AϮAϮAϩ�AϮAϬAϩ�AϮAϧ�Aϲ-AϮAϴ9AϬAϲ-AϮAϬAϮAϧ�AϬAϬAϬAϰ!AϮAϬAϲ-AϮAϲ-Aϰ!AϮAϴ9AϬAϩ�AϮAϩ�AϮAϰ!AϬAϰ!Aϧ�AϮAϩ�Aϧ�AϮAϧ�AϮAϬAϧ�AϬAϧ�AϬAϬAϥ�AϬAϩ�Aϧ�AϮAϧ�Aϩ�AϮAϧ�AϬAϮAϧ�AϮAϩ�AϬAϮAϩ�Aϰ!AϬAϬAϮAϩ�AϮAϮAϩ�AϬAϮAϧ�AϬAϮAϩ�Aϩ�AϮAϧ�AϬAϬAϩ�AϬAϰ!AϬAϬAϰ!AϬAϮAϰ!Aϧ�AϮAϮAϩ�AϮAϬAϩ�AϮAϩ�Aϩ�AϬAϧ�Aϩ�Aϩ�Aϧ�AϬAϧ�Aϥ�Aϩ�Aϣ�Aϡ�Aϧ�Aϥ�Aϣ�Aϧ�Aϣ�Aϡ�Aϥ�Aϥ�Aϡ�Aϧ�Aϣ�Aϥ�Aϧ�Aϣ�Aϥ�Aϩ�Aϣ�Aϧ�Aϧ�Aϣ�Aϧ�Aϧ�Aϥ�Aϩ�Aϥ�Aϧ�Aϩ�Aϥ�Aϩ�Aϥ�Aϥ�AϬAϧ�Aϧ�Aϩ�Aϣ�Aϩ�Aϩ�Aϩ�AϮAϩ�AϬAϰ!AϬAϰ!Aϰ!Aϴ9AϸRAϲ-AϸRAϸRA϶FA϶FAϺ^Aϴ9AϺ^AϸRAϸRAϼjAϺ^AϺ^AϾwAϺ^AϼjAϼjAϺ^AϾwAϺ^AϺ^A�A�A�ȴA��TA��TA��HA��yA��mA��yA��A���A���A���A���A���A�  A��A��mA��#A��
A�ȴA���A�ȴA�ĜA�ƨA�ȴA�A���AϬAϡ�Aϝ�Aϙ�AϑhAϋDAϋDAσA�~�A�~�A�z�A�x�A�x�A�n�A�l�A�bNA�XA�S�A�S�A�C�A�=qA�A�A�9XA�-A�+A�+A�"�A� �A�{A�bA�oA�bA�1A�
=A�
=A�A�A�%A�  A�  A�  A��A��A��A��;A��/A��
A���A���A���A���A�ƨA�ĜAξwAκ^AΡ�A�z�A�`BA�VA�S�A�G�A�;dA�1'A�$�A��A��A�oA���A��mA��;A�ĜA͡�AͅA�C�A���A��A���A�ĜA̧�A�A�A��A��A�A���A��A˴9A˅A�x�A�VA�=qA�&�A��yA��AʶFAʕ�AʁA�|�A�v�A�dZA�S�A�+A� �A��A��A��A�bA�bA�{A�oA�{A��A�oA�
=A���A��A�ȴA���AɼjAɲ-AɮAɰ!Aɰ!Aɩ�Aɟ�Aɛ�AɑhAɑhAɏ\AɑhAɏ\Aɏ\AɑhAɏ\AɍPAɏ\AɑhAɍPAɍPA�x�A�^5A�^5A�^5A�XA�O�A�Q�A�Q�A�;dA���AȅA�hsA�^5A�`BA�\)A�`BA�dZA�^5A�?}A�/A�{A�A���AǙ�A�|�A�O�A�"�A�AƧ�AƉ7A�ȴA�
=A���A�~�AÑhA¬A��A�S�A�$�A��
A�
=A���A�A��A�p�A���A��DA�jA�  A�A�|�A�A�A��A�
=A�A��A��TA��/A��
A��
A���A���A���A��A�v�A�ZA�JA�A�XA��A�ƨA��A�G�A���A�p�A���A�Q�A�-A��A�JA�JA�  A��A��FA��A�+A���A�z�A�^5A�XA�S�A�M�A�C�A�;dA�"�A��;A��A��hA��+A�hsA�M�A�A�A�+A��A�oA��A�A��-A��A���A��PA��A�~�A�n�A�Q�A�A�A�&�A�oA�1A��`A�ĜA���A���A��DA�ffA��A��+A�9XA�  A��A��/A���A��-A�t�A�E�A� �A���A��RA�z�A�C�A��A�JA���A���A��A��A��yA��mA��mA��TA��TA��yA��A��mA��`A��TA��HA��;A��HA��`A��`A��HA��HA��/A��A���A�^5A�/A�oA���A��A�C�A�{A��mA��-A�|�A�Q�A�7LA��A�bA���A��;A���A��uA�ffA�\)A�VA�Q�A�A��A�S�A�A�A��`A���A��+A�`BA�M�A�=qA�33A��A�VA�
=A�%A��A��wA��A��DA�~�A�~�A�v�A�n�A�jA�hsA�bNA�XA�=qA�bA���A��;A���A�n�A�=qA�
=A��yA���A�ĜA��jA���A���A��hA��A�v�A�C�A�JA��#A��A�ffA�dZA�E�A�=qA�&�A�oA���A��;A���A���A���A��jA��!A��A���A���A��PA��+A�p�A�l�A�JA�A���A�A�z�A��A��yA��!A�r�A�VA�I�A�A�A�(�A�"�A��A�%A�ĜA�dZA�A���A���A�XA�VA���A���A��7A�r�A�S�A�+A���A���A��jA���A��7A�|�A�r�A�hsA�dZA�ZA�9XA��A�A���A���A��;A�ȴA���A�M�A��/A���A�S�A��A�A��A�z�A�?}A��A�VA�JA�JA���A�l�A�+A�+A�-A�+A�&�A�"�A�"�A��A��A�{A�oA�bA�bA�
=A�A���A���A��A��mA���A���A��PA��A�~�A�z�A�v�A�t�A�r�A�n�A�`BA�I�A�-A���A��wA��DA�|�A�x�A�S�A�-A�"�A�JA��jA��A��A���A���A��A�r�A�p�A�n�A�l�A�ffA�^5A�^5A�ZA�M�A�9XA�&�A��A�bA�  A��A��;A�ƨA��^A��A���A���A��PA�r�A�bNA�VA�K�A�C�A��A��wA�$�A��A�A��!A���A���A��7A��A�z�A�v�A�t�A�l�A�bNA�^5A�XA�S�A�K�A�;dA��A�1A��A��!A�p�A��;A�r�A�E�A�=qA�&�A�$�A�{A�A���A���A��;A��
A���A���A��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aϧ�AϬAϬAϰ!AϮAϬAϮAϰ!Aϰ!AϮAϬAϬAϩ�Aϩ�Aϩ�AϬAϬAϬAϬAϬAϬAϬAϩ�Aϥ�Aϣ�Aϥ�Aϧ�Aϧ�Aϧ�Aϧ�AϮA϶FAϸRAϼjAϾwA��`A���A��/A�ĜAϗ�A�|�A�\)A�5?A��A�%A��A���AΕ�A�5?A��
A��
A��A�(�A�~�A��A�oA��
Aɩ�AɑhAɏ\A�jA�1'A�dZA�(�A�1'A��/A�+A�v�A���A��A���A�A�/A�bA�E�A�?}A�r�A��FA���A�t�A��A���A��`A�A�oA�A�A���A�S�A���A��A��A�VA�"�A�C�A��uA��A��^A�+A���A�r�A�JA�v�A��`A�S�A��9A�~�A�=qA��\A�`BA�VAG�A|�A{oAw�-At=qAs%Ar  Ao�;An^5Am�mAm;dAln�AjAgS�AchsAa
=A\n�AZAYoAVE�AS�-AQ�
AQ�AO;dAKXAJE�AH��AE�AD�yADz�AD$�AC
=A@9XA>ffA<bNA7O�A4�A3�;A1�FA0(�A/K�A//A.�`A+��A)hsA(��A'`BA%A$�A$A�A"��A"I�A!��A n�A -A�TA��A�Ax�A�A�#A�A33Ar�A5?A��A��A?}A�DA�A��A�`A�DAI�AƨAK�AM�A�A?}AffA��AVA
n�A
jA
1'A	��A	
=A�HAv�AĜA	O�A	�A�!AQ�A-A�7A��AM�A�\A��A�/A�\AM�A$�A5?A��A&�AĜA�uAI�A�mA;dAE�A&�A �HA �!@���@��`@�/@��@��@�C�@�j@�h@�Q�@�ƨ@�dZ@�o@��H@�R@�M�@�5?@�$�@�J@�$�@�V@��@��@�P@�(�@��@�&�@��@���@�b@�33@�=q@�%@�@�\@�ȴ@��@�+@�=q@���@�^@�h@��/@���@�5?@��@��@߅@�ƨ@��;@��y@��@�o@�"�@�"�@�o@��y@�~�@�@ݺ^@�&�@�j@�K�@��@��@ش9@�Z@�Q�@�A�@�Q�@�1@ם�@�K�@�33@�"�@���@�v�@թ�@��/@�ƨ@ҏ\@�{@�p�@���@Ь@ϥ�@�n�@ͺ^@�Q�@˅@�9X@�(�@�v�@���@�7L@��@��@ǥ�@��/@�\)@ǝ�@��m@��@�hs@��@ļj@�j@�r�@�I�@�(�@��y@�V@�Z@�A�@�V@��@�$�@��@�/@�j@��F@���@�V@�@�hs@�7L@��@��@�Ĝ@�bN@�1@��w@��P@��H@�~�@�~�@�M�@�M�@�=q@��T@�x�@��@���@��@��D@�z�@�r�@�j@�A�@��@�l�@�C�@��R@�V@�{@��@���@�%@�Ĝ@�Q�@��@���@���@���@�C�@��@�M�@���@��@��@��@�;d@�
=@��@��H@��+@��T@��7@�7L@��9@�bN@�r�@�Z@���@�dZ@�;d@�@���@�V@���@�7L@�&�@�&�@��@�%@���@���@��@�+@��!@��@�hs@�?}@�&�@�%@�z�@�I�@��@���@��;@��@�33@�
=@���@�-@���@�@���@�V@��/@�Ĝ@��u@�b@���@�+@��!@�^5@���@�`B@��@���@��9@��D@��@���@��m@��@�
=@��@��@���@���@�^5@�@���@�X@�X@�X@�%@��/@��/@��/@���@���@���@�r�@�  @��w@�l�@�@��y@��H@���@��\@�ff@�{@��#@���@�hs@��@�%@��u@�1@��@�\)@�o@��@���@���@�^5@��@�x�@�V@�V@���@��@� �@���@�K�@�ȴ@��R@���@��+@�v�@�M�@�$�@��@���@�V@��9@�%@���@�Q�@�(�@���@���@���@�l�@�;d@�o@��@��y@���@��R@��R@���@�V@���@���@��h@�hs@�G�@��@��9@���@��@�j@�Q�@�I�@�1'@�1@�|�@�\)@�;d@���@���@�E�@��T@���@�x�@�`B@�G�@�&�@�%@�Ĝ@��D@�Z@�b@��@+@~��@�w@\)@�@~�+@}�T@}`B@}�@|�@|(�@{�F@{C�@z�H@z~�@z-@y�@y��@yhs@y7L@x��@x�@x  @w|�@w
=@v��@v@uV@t9X@s�
@s��@s33@r~�@q�7G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aϛ�Aϗ�AϮAϮAϩ�AϮAϬAϩ�AϮAϧ�Aϲ-AϮAϴ9AϬAϲ-AϮAϬAϮAϧ�AϬAϬAϬAϰ!AϮAϬAϲ-AϮAϲ-Aϰ!AϮAϴ9AϬAϩ�AϮAϩ�AϮAϰ!AϬAϰ!Aϧ�AϮAϩ�Aϧ�AϮAϧ�AϮAϬAϧ�AϬAϧ�AϬAϬAϥ�AϬAϩ�Aϧ�AϮAϧ�Aϩ�AϮAϧ�AϬAϮAϧ�AϮAϩ�AϬAϮAϩ�Aϰ!AϬAϬAϮAϩ�AϮAϮAϩ�AϬAϮAϧ�AϬAϮAϩ�Aϩ�AϮAϧ�AϬAϬAϩ�AϬAϰ!AϬAϬAϰ!AϬAϮAϰ!Aϧ�AϮAϮAϩ�AϮAϬAϩ�AϮAϩ�Aϩ�AϬAϧ�Aϩ�Aϩ�Aϧ�AϬAϧ�Aϥ�Aϩ�Aϣ�Aϡ�Aϧ�Aϥ�Aϣ�Aϧ�Aϣ�Aϡ�Aϥ�Aϥ�Aϡ�Aϧ�Aϣ�Aϥ�Aϧ�Aϣ�Aϥ�Aϩ�Aϣ�Aϧ�Aϧ�Aϣ�Aϧ�Aϧ�Aϥ�Aϩ�Aϥ�Aϧ�Aϩ�Aϥ�Aϩ�Aϥ�Aϥ�AϬAϧ�Aϧ�Aϩ�Aϣ�Aϩ�Aϩ�Aϩ�AϮAϩ�AϬAϰ!AϬAϰ!Aϰ!Aϴ9AϸRAϲ-AϸRAϸRA϶FA϶FAϺ^Aϴ9AϺ^AϸRAϸRAϼjAϺ^AϺ^AϾwAϺ^AϼjAϼjAϺ^AϾwAϺ^AϺ^A�A�A�ȴA��TA��TA��HA��yA��mA��yA��A���A���A���A���A���A�  A��A��mA��#A��
A�ȴA���A�ȴA�ĜA�ƨA�ȴA�A���AϬAϡ�Aϝ�Aϙ�AϑhAϋDAϋDAσA�~�A�~�A�z�A�x�A�x�A�n�A�l�A�bNA�XA�S�A�S�A�C�A�=qA�A�A�9XA�-A�+A�+A�"�A� �A�{A�bA�oA�bA�1A�
=A�
=A�A�A�%A�  A�  A�  A��A��A��A��;A��/A��
A���A���A���A���A�ƨA�ĜAξwAκ^AΡ�A�z�A�`BA�VA�S�A�G�A�;dA�1'A�$�A��A��A�oA���A��mA��;A�ĜA͡�AͅA�C�A���A��A���A�ĜA̧�A�A�A��A��A�A���A��A˴9A˅A�x�A�VA�=qA�&�A��yA��AʶFAʕ�AʁA�|�A�v�A�dZA�S�A�+A� �A��A��A��A�bA�bA�{A�oA�{A��A�oA�
=A���A��A�ȴA���AɼjAɲ-AɮAɰ!Aɰ!Aɩ�Aɟ�Aɛ�AɑhAɑhAɏ\AɑhAɏ\Aɏ\AɑhAɏ\AɍPAɏ\AɑhAɍPAɍPA�x�A�^5A�^5A�^5A�XA�O�A�Q�A�Q�A�;dA���AȅA�hsA�^5A�`BA�\)A�`BA�dZA�^5A�?}A�/A�{A�A���AǙ�A�|�A�O�A�"�A�AƧ�AƉ7A�ȴA�
=A���A�~�AÑhA¬A��A�S�A�$�A��
A�
=A���A�A��A�p�A���A��DA�jA�  A�A�|�A�A�A��A�
=A�A��A��TA��/A��
A��
A���A���A���A��A�v�A�ZA�JA�A�XA��A�ƨA��A�G�A���A�p�A���A�Q�A�-A��A�JA�JA�  A��A��FA��A�+A���A�z�A�^5A�XA�S�A�M�A�C�A�;dA�"�A��;A��A��hA��+A�hsA�M�A�A�A�+A��A�oA��A�A��-A��A���A��PA��A�~�A�n�A�Q�A�A�A�&�A�oA�1A��`A�ĜA���A���A��DA�ffA��A��+A�9XA�  A��A��/A���A��-A�t�A�E�A� �A���A��RA�z�A�C�A��A�JA���A���A��A��A��yA��mA��mA��TA��TA��yA��A��mA��`A��TA��HA��;A��HA��`A��`A��HA��HA��/A��A���A�^5A�/A�oA���A��A�C�A�{A��mA��-A�|�A�Q�A�7LA��A�bA���A��;A���A��uA�ffA�\)A�VA�Q�A�A��A�S�A�A�A��`A���A��+A�`BA�M�A�=qA�33A��A�VA�
=A�%A��A��wA��A��DA�~�A�~�A�v�A�n�A�jA�hsA�bNA�XA�=qA�bA���A��;A���A�n�A�=qA�
=A��yA���A�ĜA��jA���A���A��hA��A�v�A�C�A�JA��#A��A�ffA�dZA�E�A�=qA�&�A�oA���A��;A���A���A���A��jA��!A��A���A���A��PA��+A�p�A�l�A�JA�A���A�A�z�A��A��yA��!A�r�A�VA�I�A�A�A�(�A�"�A��A�%A�ĜA�dZA�A���A���A�XA�VA���A���A��7A�r�A�S�A�+A���A���A��jA���A��7A�|�A�r�A�hsA�dZA�ZA�9XA��A�A���A���A��;A�ȴA���A�M�A��/A���A�S�A��A�A��A�z�A�?}A��A�VA�JA�JA���A�l�A�+A�+A�-A�+A�&�A�"�A�"�A��A��A�{A�oA�bA�bA�
=A�A���A���A��A��mA���A���A��PA��A�~�A�z�A�v�A�t�A�r�A�n�A�`BA�I�A�-A���A��wA��DA�|�A�x�A�S�A�-A�"�A�JA��jA��A��A���A���A��A�r�A�p�A�n�A�l�A�ffA�^5A�^5A�ZA�M�A�9XA�&�A��A�bA�  A��A��;A�ƨA��^A��A���A���A��PA�r�A�bNA�VA�K�A�C�A��A��wA�$�A��A�A��!A���A���A��7A��A�z�A�v�A�t�A�l�A�bNA�^5A�XA�S�A�K�A�;dA��A�1A��A��!A�p�A��;A�r�A�E�A�=qA�&�A�$�A�{A�A���A���A��;A��
A���A���A��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��BI�BI�BI�BI�BIRBI�BIRBIRBIBI�BI�BI�BJ�BK�BL0BK�BK�BLdBL�BL�BM6BN�BO�BR BS�BUgBVmBW
BW
BXEB[#Ba|BffBlWBqAB�B��B�kB�!B�tB�zB��B�wBɺB҉B�9BںB��BޞB�B�B�/B��B �B
	B
�B�B�B�BB�B~B��B��B�ZB�wB��B��B��B��B��B��B��Bt�B|Bd�B_;BV9BN�BGEB>BGEBA B-wB�BB�]B�vB�fB�
BƨB��B�xB��Bv`BncBi�Ba�BT�B@�B<6B33B'RBeB
rB
��B
ܒB
��B
��B
y>B
Y�B
B�B
5tB
%�B
PB
�B	��B	��B	��B	�DB	��B	�vB	� B	�gB	��B	�B	�rB	wfB	ncB	d�B	S�B	GzB	A�B	:�B	+�B	 �B	VB	B	
rB	�B	�B	B��B�"B�yB�pB�}B�B�dBȀB��BŢB�B�zB�gB��B�-B�RBΥB�B�?B�
B��B�B�cB�B�;B�B�B�B�8B	�B	B	�B		�B	
�B	�B	
�B	
=B	�B	�B	�B	B	FB	+B	�B	�B	
	B	�B	uB�JB�B�B�oB�vB�B�B��B�lB	�B	�B	"hB	(�B	*0B	+�B	-CB	(�B	$�B	4nB	C�B	PHB	S�B	R�B	R�B	Y�B	X�B	X�B	U�B	T�B	S�B	R�B	Q�B	PB	M�B	J�B	H�B	D�B	6�B	3�B	"4B	/�B	.�B	+kB	xB	IB	�B	IB	!B	�B	 �B	"hB	$�B	&�B	+�B	4�B	:�B	?B	B�B	GEB	M�B	V�B	c�B	e�B	n/B	t�B	u�B	rGB	o�B	p;B	t�B	y	B	{�B	}VB	~�B	�4B	�4B	�B	��B	�B	�+B	��B	x�B	y�B	�;B	��B	�DB	�VB	� B	�{B	��B	�YB	�+B	��B	��B	��B	�B	��B	�CB	��B	��B	��B	�~B	�VB	�'B	�B	�FB	��B	�nB	�nB	��B	��B	�tB	�B	�B	��B	�VB	��B	��B	��B	��B	�'B	��B	�=B	�+B	��B	�B	�tB	��B	�xB	��B	��B	��B	�-B	��B	��B	��B	�B	��B	�LB	�B	��B	�B	�tB	�<B	��B	��B	�B	�LB	��B	�dB	��B	��B	��B	��B	��B	�B	�B	��B	��B	�aB	�9B	ŢB	��B	ŢB	�EB	�zB	ȴB	��B	�^B	˒B	�B	уB	�TB	�aB	�[B	�TB	� B	� B	�aB	��B	�2B	�2B	�2B	�
B	��B	ٴB	��B	�QB	��B	�B	ٴB	ںB	�B	�B	�;B	��B	�B	�B	�NB	�B	�B	�`B	�,B	��B	�B	�B	�B	��B	��B	�B	�B	�QB	�B	��B	��B	�;B	�vB	�B	�B	��B	�B	��B	��B	�ZB	�2B	��B	�2B	�2B	�2B	�fB	��B	�fB	��B	�	B	�rB	�>B	�rB	�>B	�	B	��B	�DB	��B	�B	�B	�JB	��B	�B	��B	��B	��B	��B
 4B
B
�B
AB
�B
AB
B
GB
�B
�B
SB
�B
YB
�B
%B
�B
�B
�B
�B
�B
1B

	B
	�B
	�B
	�B

=B
�B
B
�B
�B
�B
"B
\B
VB
�B
�B
VB
VB
(B
(B
�B
 B
hB
B
hB
�B
B
�B
@B
uB
B
@B
�B
�B
�B
�B
YB
+B
_B
+B
�B
B
�B
B
�B
�B
�B
CB
OB
�B
B
CB
�B
�B
�B
�B
VB
�B
�B
�B
�B
�B
!-B
 �B
#:B
#�B
#:B
$B
#�B
#�B
#:B
#B
#B
#B
#�B
#�B
$�B
%B
%FB
'RB
(XB
(�B
'�B
'�B
($B
(XB
)�B
)�B
)�B
*0B
*eB
*�B
*�B
*�B
+kB
,qB
,�B
.B
/B
/B
/B
/�B
0UB
0UB
0�B
0�B
0�B
0�B
0�B
0UB
0�B
0�B
0�B
1�B
33B
2-B
1�B
2-B
2�B
3hB
3hB
3�B
3�B
3�B
4B
4nB
4�B
5�B
5�B
6B
6FB
6zB
6zB
6zB
6�B
7B
7LB
7�B
8B
8�B
8�B
9�B
9�B
9�B
:�B
;�B
<�B
<jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BJ�BM6BF?BIRBJ�BHKBI�BJXBIBJ�BGzBJXBH�BJXBH�BI�BI�BH�BJ�BI�BI�BI�BHBI�BJ#BG�BJ#BH�BH�BJ#BG�BK)BJ�BH�BJ�BI�BH�BJ�BH�BK�BI�BK)BL0BH�BK�BJ#BJ�BK�BK)BLdBK^BK^BM�BK^BL0BMBJ�BMBL�BJ�BMBK�BJ�BMjBK)BLdBK�BJ�BLdBJXBK�BL0BJ�BMBJ�BK�BMjBK�BK�BM�BL�BK^BM�BMjBLdBNBM6BLdBM�BM6BK^BMBL�BK�BL�BL�BL0BN�BMBMBOBBMBNpBOvBMjBOvBOvBN�BP�BOBBO�BPHBOBBP�BQNBOBBS�BTaBQ�BR�BS�BR BS�BT�BS[BT�BVBS�BVmBT�BT�BVmBVmBS[BW
BVmBV�BXEBV9BV9BWsBV�BXEBWsBV9BXEBV9BW�BWsBU�BWsBW
BV�BX�BW�BYBZ�BYKB[�B\)BZ�B]/BZ�B]/B`B`B_�Ba�Bb�Bc�BcTBa�BgBffBh�Bi�Bh�Bj�Bm)Bk�Bn�Bm�Bm)Bo Bn�Bo5BpBqABs�Bu�B�B�fB��B�fB�xB�~B��B��B��B�_B�+B��B�xB�:B�qB�qB��B��B�}B�B��B��B��B��B��B�9B��B�B�9B�B��B�B��B�B�B��B�zB�tB��B��B��B��B��B�B�dB�6B��B�<B�UB�B�OBÖBB�?B��B�B�B��BѷB�B�&B�&B� B��B�,B�[B�9BרB՛B�B��B�BٴBچBںB��B�dB�]BܒBݘB��B�B�B��B�vB�B��B��B��B��B��BیBݘB��B�B�B�8B��B�B�cB�B�8B�fB�)B��B�"B�B��B�B�B�B�+B�B�B�%B�xB��B�B�]B��B �B��B��B�B�BPB	�B
rB�BfB
�B
�B�B	�BB
=BPB�B�BPB�B�BB�B�B~B�B�B�B�B�BPBJB�B~BB�B
�BBDB
=B
�B�BxB\B
rB	B
rB~B1B�B
�B$BAB�]B�.B�cB��B iB�]B  BB��B�B�2B�JB��B�B�B�B�BخB�pB��B�hB��B��B��B�B�FB��B�@B�MB��B�BB�:B�xB�\B�}B��B��B�B��B�bB�=B��B�_B��B�7B�B��B�$B�uB�qB�B��B��B�"B��B�:B�tB�B��B�xB�B�B�B��B��B�Bt�Bu�Bu�BrBr|BrGB�B�(B��BtTBsBf�BgBc�Bd&Bc BbNBffBh
Be`B^jB[WBbB[WB[�B[WBYBXyB]�BW�BWsBS�BU2BU�BS�BPHBT,BR BPBQNBNpBM�BOvBQ�BI�BH�BH�BK)BW�BN�BN<BJ�BA�BD�BB�BE9BI�BI�BEmBE9BM6BGzBD�B?HB=�B@�B;�B;dB<�B<�B<�B<jB>BB>�B?�B?HBB�BC-BB�BC�BD�BDgBDgBD3BF�BF�BF�BE�BT�BZ�BO�BIBR�BNB?}BFtB?HB?B<�B;0B8�B6zB3�B4�B5�B49B4nB1'B+B*�B*0B(�B6�B/OB�B%FB �B�B�BYBqB4B�B$BoB{BOB \BB{B�BPB�BVB�BDB
�BB.BB+B
=B�B�B�BBoB��B��B�B�xB�B��B�+B�fB�cB�PB�DB	�B�B�B�TB�B�B�AB�>B�oB�B��B�8B�B�8B��B�&B�B�HB�B�B�HB�B�B�pB��B��B�
BیB�
B��B��B��B͟B�pB��B�XB��B��B�BʌB��B��BÖB��B��B��B��B�B��B��B��B�XB��B�B��B�VB�'B�VB��B��B�!B�\B�CB�1B�YB��B��B�1B��B�B�B��B��B�SB�PB��B��BzDBw�BxBv�B�MB�GBrGBoiBn�Bo5BpBp�Bn�BncBo Bo5Bn�Bm�BlWBm)Bn�Bm]Bl�BkBk�Bm�Bo BiyBd�Bc Bc�Bc�Bc�Bb�BaBa�Bc�Bb�Ba�Bb�B\�BUgBV9B[�BR�BN<BQ�B[�BcTBOvBB'BCaBGzBA�BA B@B?}B?HB@OB?B>�B?�B?B>�B=�B<�B<6B:*B9XB:*B6zB8B5?B4nB4B6B33B1�B/�B-CB1�B:�B9�B)�B#nB"4B!-B�B�B~B�BCBkBkB�BkB�BYB�B�B7B�B�BBB#:BVB�B �B
��B
�B
��B
�DB
��B
��B
�]B
��B
�B
��B
�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2023011422044420230114220444IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023011601013420230116010134QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023011601013420230116010134QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194920230210131949IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                