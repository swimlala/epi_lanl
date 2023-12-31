CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-12-26T21:01:21Z creation; 2022-02-04T23:30:07Z DMQC;      
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
_FillValue        G�O�     8  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  S@   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     8  X�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     8  t�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8  ؘ   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 
�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8 (   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` &`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   &�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ,�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   2�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T 8�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   9   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   9   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   9$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   9,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 94   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   9�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   9�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    9�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        9�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        :    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       :   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    :Argo profile    3.1 1.2 19500101000000  20211226210121  20220204223521  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_194                 6810_008521_194                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @٭���@٭���11  @٭I�^@٭I�^@0���,<�@0���,<��driu��driu�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�@G�@�G�@�G�@\@�  A   A\)A\)A,(�A?\)A`��A�Q�A���A���A�Q�A�  AϮA�Q�A���B Q�B�
B�B�B�
B(  B0  B8  B@  BH(�BP  BW�
B_�
Bh  Bp(�Bx(�B�  B�  B�(�B�{B�  B��
B��B�  B��
B��B�(�B�{B�{B�{B��B�  B�(�B�{B�  B��
B�{B�(�B�(�B�{B��B��B��B��B��B��B��B�  C   C��C��C��C  C	��C  C  C��C��C��C��C
=C
=C  C  C��C!��C$  C&  C(
=C*
=C,  C-��C0  C2  C4  C6  C8{C:
=C<{C>
=C@
=CB{CD  CE��CG��CI��CL{CN  CO�CQ��CS��CU��CX
=CZ  C\  C^  C`  Ca��Cc��Cf  Cg�Ci��Cl  Cn
=Cp
=Cr  Ct  Cv
=Cx  Cy��C{��C~  C�C�  C�  C�
=C�  C�  C�  C�  C�C�  C���C�  C�  C�C�C�
=C�C���C���C���C���C�  C�  C�C�C�C�C�  C�  C���C�  C�  C�C�C�  C�  C�  C�C�C�C�C�C���C�C�
=C�  C���C�C�  C�  C�C�  C���C���C�  C�C�
=C���C���C�  C���C���C�C�  C�  C�
=C���C�C�  C���C�  C�  C���C���C���C�  C�  C�C�C�  C���C�  C���C���C���C�C�C���C���C���C���C�  C�  C���C�  C���C���C���C���C�  C�  C�C�C�C�\C�  C���C�C�C���C���C�C���C�  C�
=C�
=C���C���C�C�C�C�
=C�C�  C�C�  C�  C�D �D � D  D}qD�D� D�qD� D�D��D  D� D  D� D  D� D�qD��D	�D	��D	�qD
� D  D��D�D��D�D}qD�qD}qD  D� D�qD}qD  D��D�D� D�D��D  D��D  D}qD�qD�D  D}qD  D��D�qD� DD}qD�RD}qD  D��D  Dz�D�RDz�D  D��D�qD }qD!�D!��D"  D"��D#D#��D$  D$� D%�D%��D&�D&� D&�qD'� D(�D(��D)  D)� D*  D*� D*�qD+}qD+�qD,� D-�D-��D.�D.}qD/  D/��D0�D0� D1�D1� D2  D2��D3�D3��D4  D4� D5  D5� D6  D6�D7�D7� D8  D8}qD8�qD9� D:  D:� D;�D;� D<  D<� D=  D=� D>  D>� D>�qD?}qD?�qD@z�DA  DA� DB  DB� DC�DC��DD  DD� DE  DE��DE�qDF� DG  DG}qDHDH�DH�qDI� DI�qDJ� DKDK� DL�DL�DM  DMz�DM�qDN}qDN�qDO��DP�DP��DQ�DQ�DR�DR}qDR�RDSz�DT  DT�DUDU� DU�qDV}qDV�qDW��DXDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD\  D\��D]�D]��D]�qD^}qD_�D_��D`  D`� D`�qDa}qDb  Db��Db�qDc}qDc�qDd}qDd�qDe��Df�Df��Dg�Dg��Dh�Dh}qDh�qDi}qDj  Dj� Dk�Dk� Dl  Dl}qDl��Dmz�Dm�qDn� Do�Do}qDp  Dp� Dp��Dq� Dr�Dr� Dr�qDs}qDt  Dt��Du  Du}qDu��Dv}qDw�Dw�Dx�Dx}qDy  Dy� Dz  Dz� D{  D{� D|D|��D}�D}}qD~�D~��D~�qD��D�HD�@ D��HD�� D���D�@ D�� D�� D��D�AHD�� D��HD��D�AHD��HD�� D��qD�>�D�� D�� D�  D�AHD�� D��qD���D�@ D�� D�� D�  D�@ D�� D��HD�  D�>�D�~�D��HD�  D�@ D���D�D�  D�@ D�� D���D���D�@ D�~�D��qD���D�9�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�\)?8Q�?��?��
?���@   @
=q@(�@5@J=q@W
=@fff@z�H@��@�{@�z�@�  @��@�\)@���@��
@�=q@�33@�p�@��
@���@�
=A   A�
A
=qAp�A�AQ�Ap�A!G�A%A,(�A1�A5A9��A@  AE�AH��AN{ATz�AY��A^{Ab�\AhQ�Amp�AqG�AvffA|(�A~�RA��A���A�ffA���A��A�A��A��A��A�\)A���A�(�A�{A�Q�A�33A�A�\)A�=qA��A�
=A�G�A�z�A��RA���A��
A�ffA���A��HA�{A���A�33A�p�A���A�33A�p�Aأ�A��HA�p�A��A��
A�p�A�  A�33A�A�A�33A�A�  A��HA�A��B�B�\B  B��BffB�
B��B	�B\)B��B��B�\B(�Bp�B{B�B��B��B�HB(�BG�B=qB\)B��B{B�HB   B!��B"ffB#�B$��B%�B&�RB'�
B)G�B*=qB+
=B,z�B-p�B.{B/
=B0Q�B1p�B2=qB3\)B4��B5G�B6=qB7�B8Q�B8��B:ffB;\)B<  B=G�B>ffB?\)B@Q�BA��BB�HBC�
BD��BFffBG�
BH��BJ{BK33BL��BN=qBO\)BPz�BQBS\)BTz�BU��BW33BX��BYBZ�HB\Q�B]B_�B`��BaBc\)Be�Bf{Bg�Bi�Bj�\Bk�BmG�Bo
=Bp(�Bqp�Bs
=Bt��Bu��Bw
=Bx��By�B{
=B|��B~{B33B�=qB��B�B�Q�B�33B�  B��\B��B��B��RB�G�B��B���B�\)B��B���B�p�B�{B���B�\)B�(�B��RB�\)B�=qB��HB�p�B�{B���B���B�=qB���B��B�Q�B��HB��B�Q�B���B�p�B�(�B���B���B�{B��RB�p�B�(�B���B�\)B�{B��\B�G�B�{B��\B�33B�  B��\B��B��
B���B�33B��B�ffB�33B�B�Q�B��B��
B�ffB���B�B��\B�
=B���B�z�B��B���B�=qB�
=B���B�(�B�
=B��B�(�B���B��B�=qB��HB���B�ffB���B��B�ffB�33B��B�ffB�33B��
B�ffB��B�  B£�B�33B��
Bģ�B�\)B�  BƏ\B�\)B�(�Bȣ�B�G�B�{B���B�G�B��Ḅ�BͅB�{BΣ�B�\)B�(�B���B�\)B�  B���BӅB�{BԸRBՅB�Q�B��HB�p�B�Q�B�
=BٮB�Q�B�
=B��
B܏\B��B�Bޏ\B�\)B��
B�z�B�G�B�  B��B�33B�  B��B�33B��
B��B�\)B��
B�ffB�33B�{B�RB�G�B�  B���B�B�(�B�RB�p�B�=qB���B�\)B�(�B��HB�B�{B���B�p�B�(�B��RB�G�B�  B��HB��B�{B��RB�p�B�=qB���B�p�B�=qB�
=B��C {C z�C �HC33C�C�
C=qC��C�HC(�C��C�C(�C�C�C33Cz�C��C=qC��C��CG�C�\C��CQ�C�\C�HC	G�C	��C	�
C
(�C
�C
�C=qCz�CC33C�\C�
C�Cz�C�
C33Cz�C��C=qC�C��C33C��C�C=qC��C
=CffC��C  CffC�RC��C\)CC  CQ�C�RC{CffC��C
=Cp�C�RC  CffC�RC��CG�C�C��C(�Cp�C�RC
=CG�Cz�C�C��C=qCp�C��C�
C�C=qC\)C��C�
C��C�CffC��C��C�HC�CffC�\C�C�HC �C \)C z�C ��C �HC!(�C!G�C!p�C!�C!�C"{C"=qC"�C"��C"��C#
=C#G�C#\)C#��C#�
C#��C${C$\)C$�C$��C$�
C%�C%\)C%p�C%��C%�HC&�C&G�C&p�C&�RC&��C'�C'G�C'�C'C'�C({C(\)C(��C(�RC(�HC){C)\)C)�C)��C)�HC*�C*Q�C*p�C*��C*�
C+�C+G�C+p�C+��C+�HC,�C,G�C,z�C,��C,��C-{C-=qC-�C-C-�C.
=C.Q�C.�C.��C.��C/
=C/G�C/ffC/�\C/��C0
=C033C0Q�C0�C0��C0��C1�C1Q�C1�\C1��C1��C2�C2ffC2��C2�RC2��C3=qC3\)C3z�C3C4  C4�C4G�C4�C4C4��C5{C5=qC5p�C5�RC5��C6
=C633C6z�C6�RC6�
C6��C7G�C7�C7��C7��C8  C8=qC8z�C8��C8C8�C933C9p�C9��C9�RC9�C:(�C:ffC:�\C:�RC:�HC;{C;\)C;�\C;�RC;�HC<�C<Q�C<�\C<�C<�
C={C=\)C=�\C=�C=�
C>{C>Q�C>�C>��C>��C?  C?=qC?p�C?��C?�RC?��C@(�C@ffC@��C@�RC@�HCA{CAQ�CA�\CACA�HCB
=CBG�CBz�CB�RCB��CC{CC=qCCp�CC�RCC�CD{CDG�CDffCD��CD�
CE{CE33CE\)CE��CE�
CF
=CF�CFQ�CF�CFCF��CG�CG=qCGp�CG�RCG��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @�@G�@�G�@�G�@\@�  A   A\)A\)A,(�A?\)A`��A�Q�A���A���A�Q�A�  AϮA�Q�A���B Q�B�
B�B�B�
B(  B0  B8  B@  BH(�BP  BW�
B_�
Bh  Bp(�Bx(�B�  B�  B�(�B�{B�  B��
B��B�  B��
B��B�(�B�{B�{B�{B��B�  B�(�B�{B�  B��
B�{B�(�B�(�B�{B��B��B��B��B��B��B��B�  C   C��C��C��C  C	��C  C  C��C��C��C��C
=C
=C  C  C��C!��C$  C&  C(
=C*
=C,  C-��C0  C2  C4  C6  C8{C:
=C<{C>
=C@
=CB{CD  CE��CG��CI��CL{CN  CO�CQ��CS��CU��CX
=CZ  C\  C^  C`  Ca��Cc��Cf  Cg�Ci��Cl  Cn
=Cp
=Cr  Ct  Cv
=Cx  Cy��C{��C~  C�C�  C�  C�
=C�  C�  C�  C�  C�C�  C���C�  C�  C�C�C�
=C�C���C���C���C���C�  C�  C�C�C�C�C�  C�  C���C�  C�  C�C�C�  C�  C�  C�C�C�C�C�C���C�C�
=C�  C���C�C�  C�  C�C�  C���C���C�  C�C�
=C���C���C�  C���C���C�C�  C�  C�
=C���C�C�  C���C�  C�  C���C���C���C�  C�  C�C�C�  C���C�  C���C���C���C�C�C���C���C���C���C�  C�  C���C�  C���C���C���C���C�  C�  C�C�C�C�\C�  C���C�C�C���C���C�C���C�  C�
=C�
=C���C���C�C�C�C�
=C�C�  C�C�  C�  C�D �D � D  D}qD�D� D�qD� D�D��D  D� D  D� D  D� D�qD��D	�D	��D	�qD
� D  D��D�D��D�D}qD�qD}qD  D� D�qD}qD  D��D�D� D�D��D  D��D  D}qD�qD�D  D}qD  D��D�qD� DD}qD�RD}qD  D��D  Dz�D�RDz�D  D��D�qD }qD!�D!��D"  D"��D#D#��D$  D$� D%�D%��D&�D&� D&�qD'� D(�D(��D)  D)� D*  D*� D*�qD+}qD+�qD,� D-�D-��D.�D.}qD/  D/��D0�D0� D1�D1� D2  D2��D3�D3��D4  D4� D5  D5� D6  D6�D7�D7� D8  D8}qD8�qD9� D:  D:� D;�D;� D<  D<� D=  D=� D>  D>� D>�qD?}qD?�qD@z�DA  DA� DB  DB� DC�DC��DD  DD� DE  DE��DE�qDF� DG  DG}qDHDH�DH�qDI� DI�qDJ� DKDK� DL�DL�DM  DMz�DM�qDN}qDN�qDO��DP�DP��DQ�DQ�DR�DR}qDR�RDSz�DT  DT�DUDU� DU�qDV}qDV�qDW��DXDX}qDX�qDY}qDY�qDZ}qDZ�qD[}qD\  D\��D]�D]��D]�qD^}qD_�D_��D`  D`� D`�qDa}qDb  Db��Db�qDc}qDc�qDd}qDd�qDe��Df�Df��Dg�Dg��Dh�Dh}qDh�qDi}qDj  Dj� Dk�Dk� Dl  Dl}qDl��Dmz�Dm�qDn� Do�Do}qDp  Dp� Dp��Dq� Dr�Dr� Dr�qDs}qDt  Dt��Du  Du}qDu��Dv}qDw�Dw�Dx�Dx}qDy  Dy� Dz  Dz� D{  D{� D|D|��D}�D}}qD~�D~��D~�qD��D�HD�@ D��HD�� D���D�@ D�� D�� D��D�AHD�� D��HD��D�AHD��HD�� D��qD�>�D�� D�� D�  D�AHD�� D��qD���D�@ D�� D�� D�  D�@ D�� D��HD�  D�>�D�~�D��HD�  D�@ D���D�D�  D�@ D�� D���D���D�@ D�~�D��qD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�\)?8Q�?��?��
?���@   @
=q@(�@5@J=q@W
=@fff@z�H@��@�{@�z�@�  @��@�\)@���@��
@�=q@�33@�p�@��
@���@�
=A   A�
A
=qAp�A�AQ�Ap�A!G�A%A,(�A1�A5A9��A@  AE�AH��AN{ATz�AY��A^{Ab�\AhQ�Amp�AqG�AvffA|(�A~�RA��A���A�ffA���A��A�A��A��A��A�\)A���A�(�A�{A�Q�A�33A�A�\)A�=qA��A�
=A�G�A�z�A��RA���A��
A�ffA���A��HA�{A���A�33A�p�A���A�33A�p�Aأ�A��HA�p�A��A��
A�p�A�  A�33A�A�A�33A�A�  A��HA�A��B�B�\B  B��BffB�
B��B	�B\)B��B��B�\B(�Bp�B{B�B��B��B�HB(�BG�B=qB\)B��B{B�HB   B!��B"ffB#�B$��B%�B&�RB'�
B)G�B*=qB+
=B,z�B-p�B.{B/
=B0Q�B1p�B2=qB3\)B4��B5G�B6=qB7�B8Q�B8��B:ffB;\)B<  B=G�B>ffB?\)B@Q�BA��BB�HBC�
BD��BFffBG�
BH��BJ{BK33BL��BN=qBO\)BPz�BQBS\)BTz�BU��BW33BX��BYBZ�HB\Q�B]B_�B`��BaBc\)Be�Bf{Bg�Bi�Bj�\Bk�BmG�Bo
=Bp(�Bqp�Bs
=Bt��Bu��Bw
=Bx��By�B{
=B|��B~{B33B�=qB��B�B�Q�B�33B�  B��\B��B��B��RB�G�B��B���B�\)B��B���B�p�B�{B���B�\)B�(�B��RB�\)B�=qB��HB�p�B�{B���B���B�=qB���B��B�Q�B��HB��B�Q�B���B�p�B�(�B���B���B�{B��RB�p�B�(�B���B�\)B�{B��\B�G�B�{B��\B�33B�  B��\B��B��
B���B�33B��B�ffB�33B�B�Q�B��B��
B�ffB���B�B��\B�
=B���B�z�B��B���B�=qB�
=B���B�(�B�
=B��B�(�B���B��B�=qB��HB���B�ffB���B��B�ffB�33B��B�ffB�33B��
B�ffB��B�  B£�B�33B��
Bģ�B�\)B�  BƏ\B�\)B�(�Bȣ�B�G�B�{B���B�G�B��Ḅ�BͅB�{BΣ�B�\)B�(�B���B�\)B�  B���BӅB�{BԸRBՅB�Q�B��HB�p�B�Q�B�
=BٮB�Q�B�
=B��
B܏\B��B�Bޏ\B�\)B��
B�z�B�G�B�  B��B�33B�  B��B�33B��
B��B�\)B��
B�ffB�33B�{B�RB�G�B�  B���B�B�(�B�RB�p�B�=qB���B�\)B�(�B��HB�B�{B���B�p�B�(�B��RB�G�B�  B��HB��B�{B��RB�p�B�=qB���B�p�B�=qB�
=B��C {C z�C �HC33C�C�
C=qC��C�HC(�C��C�C(�C�C�C33Cz�C��C=qC��C��CG�C�\C��CQ�C�\C�HC	G�C	��C	�
C
(�C
�C
�C=qCz�CC33C�\C�
C�Cz�C�
C33Cz�C��C=qC�C��C33C��C�C=qC��C
=CffC��C  CffC�RC��C\)CC  CQ�C�RC{CffC��C
=Cp�C�RC  CffC�RC��CG�C�C��C(�Cp�C�RC
=CG�Cz�C�C��C=qCp�C��C�
C�C=qC\)C��C�
C��C�CffC��C��C�HC�CffC�\C�C�HC �C \)C z�C ��C �HC!(�C!G�C!p�C!�C!�C"{C"=qC"�C"��C"��C#
=C#G�C#\)C#��C#�
C#��C${C$\)C$�C$��C$�
C%�C%\)C%p�C%��C%�HC&�C&G�C&p�C&�RC&��C'�C'G�C'�C'C'�C({C(\)C(��C(�RC(�HC){C)\)C)�C)��C)�HC*�C*Q�C*p�C*��C*�
C+�C+G�C+p�C+��C+�HC,�C,G�C,z�C,��C,��C-{C-=qC-�C-C-�C.
=C.Q�C.�C.��C.��C/
=C/G�C/ffC/�\C/��C0
=C033C0Q�C0�C0��C0��C1�C1Q�C1�\C1��C1��C2�C2ffC2��C2�RC2��C3=qC3\)C3z�C3C4  C4�C4G�C4�C4C4��C5{C5=qC5p�C5�RC5��C6
=C633C6z�C6�RC6�
C6��C7G�C7�C7��C7��C8  C8=qC8z�C8��C8C8�C933C9p�C9��C9�RC9�C:(�C:ffC:�\C:�RC:�HC;{C;\)C;�\C;�RC;�HC<�C<Q�C<�\C<�C<�
C={C=\)C=�\C=�C=�
C>{C>Q�C>�C>��C>��C?  C?=qC?p�C?��C?�RC?��C@(�C@ffC@��C@�RC@�HCA{CAQ�CA�\CACA�HCB
=CBG�CBz�CB�RCB��CC{CC=qCCp�CC�RCC�CD{CDG�CDffCD��CD�
CE{CE33CE\)CE��CE�
CF
=CF�CFQ�CF�CFCF��CG�CG=qCGp�CG�RCG��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A���A���A���A���A���A���A���A���A��
A��;A��HA��/A��/A��/A��;A��TA��TA��TA��`A��mA��yA��mA��yA��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A�  A���A�A�  A�  A���A��A�ƨAѧ�Aч+A�`BA�K�A�(�A�oA��A�ffA��A�ƨAͰ!A�%A�33A�n�A�n�A�jAǗ�AžwA�O�A�|�A�Q�A��A���A��wA��A��7A��9A�v�A�ƨA�C�A���A�~�A�;dA���A���A��/A��\A���A�Q�A�O�A��A��uA�1A��FA��A��uA��`A��A�ZA�33A��A�ffA�z�A�t�A�$�A�%A���A�oA�ȴA���A�$�A���A��A�|�A��9A�/A���A�/A���A��PA��;A�33A�ƨA��A|��AzbAx�RAtQ�Ar�Aq�
ApZAn~�Am+Aj�Ad�AbȴAaXA_�FA\�jAZ��AX��AUdZARĜAP�9AO�AL�AJM�AG7LAE�AE&�AA�TA>��A=�A;t�A8�A6�9A5�
A4E�A2��A0v�A.ȴA,�A+�mA+?}A*~�A)�mA)|�A(�A(A�A'��A'��A'"�A%��A%oA$�A$�A$A�A#hsA#7LA"�A"z�A"A!A �DA I�AA�AffA��A��A�FAVAXA��A�`A��Az�A`BAĜA �A��A\)A%A~�A�#Ax�AĜA��A�^Ap�A
=A�;A
�+A
JA	XA��A�A�!Az�AVAJA�HAM�A  At�A
=A�FA�yA=qA��A�A ��@�K�@��\@�M�@��9@��@�K�@��@��!@�Z@���@�^5@�M�@��@���@���@��@�7L@�V@���@���@�@���@��^@�7L@���@�1@���@�ȴ@���@��H@�@���@���@�5?@�%@��@��;@��@��@�O�@���@�bN@�w@��@�V@�$�@�X@���@�@��m@�n�@�/@�A�@�ƨ@�ȴ@�J@�h@�/@�r�@��m@�\)@�^5@���@�@߮@ߝ�@�ȴ@�=q@���@ۥ�@�K�@�;d@�33@ڗ�@��@؃@�t�@���@և+@�$�@��@�9X@�C�@ҧ�@�@�Ĝ@�z�@�I�@��
@�^5@��@�@�x�@��@�I�@�o@�n�@�=q@�J@��@��@���@���@ɑh@�?}@�%@ț�@�bN@�(�@�\)@�@�n�@�V@�=q@�{@���@�%@�  @Õ�@�ff@+@��@�33@�
=@�@��/@�(�@��@��
@��@�
=@��R@��!@���@���@�A�@��y@���@�/@��j@�Z@�1@���@�K�@�+@��y@�@��@�I�@��;@�K�@��y@�n�@��T@�&�@���@���@��D@�Q�@� �@��@��F@��P@�+@�@��H@��!@�v�@��T@��7@�x�@�1@�C�@��!@���@���@��\@��\@�~�@��#@�`B@��@��h@��7@��-@��@���@�I�@�S�@�"�@��R@���@��7@��@�O�@�?}@�7L@�G�@�V@��j@��u@��@�I�@��@��w@�dZ@���@�@��-@���@���@��@�bN@�Z@�I�@�9X@�1@���@�
=@��R@��+@�^5@�$�@���@��#@��7@�p�@�X@�/@��@���@��/@��@�A�@���@�K�@��!@��#@�X@��@��j@��u@��@�r�@�Z@���@�ƨ@�\)@��y@���@��!@���@�~�@�n�@�E�@��@��#@��^@��@�O�@�G�@�7L@�%@���@�1'@�1@��;@�;d@�o@�
=@�@�@�@��@���@�v�@��@��T@��#@��#@���@��@���@���@��`@��/@��j@���@��D@�j@� �@��m@���@��@�+@��!@�~�@�V@�J@��^@�x�@�?}@���@��@���@�r�@�(�@��@��P@�o@�ȴ@��@�@�x�@�`B@�7L@�%@��`@���@��@�1'@��F@�33@�ff@�=q@���@��7@�O�@�V@���@��u@�A�@��m@���@���@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A���A���A���A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A���A���A��
A���A���A��A���A��
A��/A��;A��/A��;A��HA��;A��#A��A��/A��/A��#A��/A��/A��#A��#A��A��;A��/A��A��/A��;A��#A��;A��;A��#A��/A��;A��HA��/A��/A��HA��HA��;A��TA��TA��;A��TA��`A��HA��TA��mA��TA��TA��mA��`A��HA��`A��mA��TA��HA��`A��mA��TA��TA��mA��`A��TA��mA��TA��`A��yA��yA��mA��mA��A��A��`A��yA��mA��mA��yA��mA��TA��TA��mA��yA��`A��mA��A��yA��mA��yA��A��yA��mA��A��A��yA��A��A��mA��yA��A��A��yA��A��A��A��A��A��A��yA��A��A��A��yA��yA��A��A��yA��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��yA��A��A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A���A�  A���A���A���A�  A���A���A�  A���A���A���A�  A���A���A���A�  A���A���A�  A�A���A���A�  A�A�  A���A�A�A���A���A�  A���A���A���A�A�A�  A���A�A�A���A�  A�A���A�  A�A���A�  A�A�  A���A�A�%A�%A���A���A���A���A���A���A���A���A��A��HA��#A���A�ȴA�ĜAѼjAѴ9AѰ!AѲ-AѬAѡ�Aѣ�Aѝ�AѓuAёhAэPAч+A�~�A�x�A�r�A�\)A�\)A�\)A�\)A�Q�A�Q�A�S�A�Q�A�M�A�E�A�;dA�33A�&�A�&�A�(�A�&�A�"�A��A�{A�oA�VA�JA�bA�
=A��A��mA��TA��;Aв-AГuA�|�A�\)A�I�A�?}A�&�A�bA�1A���A��HA���A�jA�;dA�A���A�`BA�7LA��A��AͼjA͗�A�v�A�bNA�5?A��A�1A��A���A̧�A�z�A�Q�A�?}A��A��`A˧�A˩�AˬA˼jA�ƨAʩ�A��TA���Aɗ�A�\)A� �A�
=A���A���Aȇ+A�O�A�$�A�  A��yA��`AǶFAǅA�n�A�E�A��A���A�M�A�C�A��Aģ�Ać+A�x�A�bNA�C�A�/A�+A��A�ƨAîA�hsA�{A��`A¸RA�A�VA�A�A�%A��!A�n�A�G�A� �A��HA�\)A�G�A�33A��A�%A��yA���A��A�VA��A���A���A��A�r�A�`BA�G�A�bA���A��hA�33A��A��FA��hA�hsA�S�A�9XA�"�A���A���A���A�l�A�(�A��/A���A�jA�C�A�7LA�"�A�  A��mA�A���A��DA��A�~�A�hsA�G�A��A��A��TA���A��mA�;dA��A�ĜA���A�l�A��mA�O�A���A��RA��hA�l�A�^5A�G�A�1'A��A�A��A��A��`A�ȴA��hA�n�A�dZA�I�A�9XA�(�A� �A�{A�A��`A��A�ƨA��9A��uA�hsA�33A��A���A�Q�A�JA��/A���A���A�O�A��A���A�z�A�5?A��A�A��uA�bNA�7LA��A���A��TA��#A���A���A��^A��9A��A���A���A��uA�~�A�n�A�S�A�"�A��`A���A�`BA��A���A�ȴA��A�|�A�5?A��mA���A��A�VA� �A�%A��TA�ȴA���A�`BA�^5A�Q�A�C�A�9XA�-A��A��A�ĜA��uA�bNA�/A�A��;A�ƨA���A�jA� �A��^A��DA�bNA�G�A�A�A�-A�A��A��HA��A���A��wA��9A���A��\A�v�A�?}A��yA�hsA�
=A��^A���A�l�A�E�A�JA��#A���A�hsA�?}A�/A��A���A��A���A��A���A���A���A��7A��A�|�A�n�A�bNA�A�A�33A�$�A��A�oA��A���A���A���A�z�A�jA�dZA�K�A�5?A��A�A��A�ƨA��RA��+A�G�A��A��A��jA��\A�r�A�O�A��A��A�ȴA���A��PA�z�A�dZA�ZA�K�A�C�A�/A��A��A��^A�hsA�K�A�5?A�(�A��A�bA�A��A��A��`A��`A��HA��A��^A�r�A�M�A�1'A�(�A� �A�{A���A��;A�ƨA���A���A�t�A�`BA�VA�Q�A�I�A�=qA�$�A���A���A�x�A�VA�M�A�C�A�=qA�;dA�7LA�5?A�1'A�-A�-A�+A�(�A�$�A��A��A��A�VA���A�ĜA��7A�XA� �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A��
A��;A��HA��/A��/A��/A��;A��TA��TA��TA��`A��mA��yA��mA��yA��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A�  A���A�A�  A�  A���A��A�ƨAѧ�Aч+A�`BA�K�A�(�A�oA��A�ffA��A�ƨAͰ!A�%A�33A�n�A�n�A�jAǗ�AžwA�O�A�|�A�Q�A��A���A��wA��A��7A��9A�v�A�ƨA�C�A���A�~�A�;dA���A���A��/A��\A���A�Q�A�O�A��A��uA�1A��FA��A��uA��`A��A�ZA�33A��A�ffA�z�A�t�A�$�A�%A���A�oA�ȴA���A�$�A���A��A�|�A��9A�/A���A�/A���A��PA��;A�33A�ƨA��A|��AzbAx�RAtQ�Ar�Aq�
ApZAn~�Am+Aj�Ad�AbȴAaXA_�FA\�jAZ��AX��AUdZARĜAP�9AO�AL�AJM�AG7LAE�AE&�AA�TA>��A=�A;t�A8�A6�9A5�
A4E�A2��A0v�A.ȴA,�A+�mA+?}A*~�A)�mA)|�A(�A(A�A'��A'��A'"�A%��A%oA$�A$�A$A�A#hsA#7LA"�A"z�A"A!A �DA I�AA�AffA��A��A�FAVAXA��A�`A��Az�A`BAĜA �A��A\)A%A~�A�#Ax�AĜA��A�^Ap�A
=A�;A
�+A
JA	XA��A�A�!Az�AVAJA�HAM�A  At�A
=A�FA�yA=qA��A�A ��@�K�@��\@�M�@��9@��@�K�@��@��!@�Z@���@�^5@�M�@��@���@���@��@�7L@�V@���@���@�@���@��^@�7L@���@�1@���@�ȴ@���@��H@�@���@���@�5?@�%@��@��;@��@��@�O�@���@�bN@�w@��@�V@�$�@�X@���@�@��m@�n�@�/@�A�@�ƨ@�ȴ@�J@�h@�/@�r�@��m@�\)@�^5@���@�@߮@ߝ�@�ȴ@�=q@���@ۥ�@�K�@�;d@�33@ڗ�@��@؃@�t�@���@և+@�$�@��@�9X@�C�@ҧ�@�@�Ĝ@�z�@�I�@��
@�^5@��@�@�x�@��@�I�@�o@�n�@�=q@�J@��@��@���@���@ɑh@�?}@�%@ț�@�bN@�(�@�\)@�@�n�@�V@�=q@�{@���@�%@�  @Õ�@�ff@+@��@�33@�
=@�@��/@�(�@��@��
@��@�
=@��R@��!@���@���@�A�@��y@���@�/@��j@�Z@�1@���@�K�@�+@��y@�@��@�I�@��;@�K�@��y@�n�@��T@�&�@���@���@��D@�Q�@� �@��@��F@��P@�+@�@��H@��!@�v�@��T@��7@�x�@�1@�C�@��!@���@���@��\@��\@�~�@��#@�`B@��@��h@��7@��-@��@���@�I�@�S�@�"�@��R@���@��7@��@�O�@�?}@�7L@�G�@�V@��j@��u@��@�I�@��@��w@�dZ@���@�@��-@���@���@��@�bN@�Z@�I�@�9X@�1@���@�
=@��R@��+@�^5@�$�@���@��#@��7@�p�@�X@�/@��@���@��/@��@�A�@���@�K�@��!@��#@�X@��@��j@��u@��@�r�@�Z@���@�ƨ@�\)@��y@���@��!@���@�~�@�n�@�E�@��@��#@��^@��@�O�@�G�@�7L@�%@���@�1'@�1@��;@�;d@�o@�
=@�@�@�@��@���@�v�@��@��T@��#@��#@���@��@���@���@��`@��/@��j@���@��D@�j@� �@��m@���@��@�+@��!@�~�@�V@�J@��^@�x�@�?}@���@��@���@�r�@�(�@��@��P@�o@�ȴ@��@�@�x�@�`B@�7L@�%@��`@���@��@�1'@��F@�33@�ff@�=q@���@��7@�O�@�V@���@��u@�A�@��m@���@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A���A���A���A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A���A���A��
A���A���A��A���A��
A��/A��;A��/A��;A��HA��;A��#A��A��/A��/A��#A��/A��/A��#A��#A��A��;A��/A��A��/A��;A��#A��;A��;A��#A��/A��;A��HA��/A��/A��HA��HA��;A��TA��TA��;A��TA��`A��HA��TA��mA��TA��TA��mA��`A��HA��`A��mA��TA��HA��`A��mA��TA��TA��mA��`A��TA��mA��TA��`A��yA��yA��mA��mA��A��A��`A��yA��mA��mA��yA��mA��TA��TA��mA��yA��`A��mA��A��yA��mA��yA��A��yA��mA��A��A��yA��A��A��mA��yA��A��A��yA��A��A��A��A��A��A��yA��A��A��A��yA��yA��A��A��yA��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��yA��A��A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A���A�  A���A���A���A�  A���A���A�  A���A���A���A�  A���A���A���A�  A���A���A�  A�A���A���A�  A�A�  A���A�A�A���A���A�  A���A���A���A�A�A�  A���A�A�A���A�  A�A���A�  A�A���A�  A�A�  A���A�A�%A�%A���A���A���A���A���A���A���A���A��A��HA��#A���A�ȴA�ĜAѼjAѴ9AѰ!AѲ-AѬAѡ�Aѣ�Aѝ�AѓuAёhAэPAч+A�~�A�x�A�r�A�\)A�\)A�\)A�\)A�Q�A�Q�A�S�A�Q�A�M�A�E�A�;dA�33A�&�A�&�A�(�A�&�A�"�A��A�{A�oA�VA�JA�bA�
=A��A��mA��TA��;Aв-AГuA�|�A�\)A�I�A�?}A�&�A�bA�1A���A��HA���A�jA�;dA�A���A�`BA�7LA��A��AͼjA͗�A�v�A�bNA�5?A��A�1A��A���A̧�A�z�A�Q�A�?}A��A��`A˧�A˩�AˬA˼jA�ƨAʩ�A��TA���Aɗ�A�\)A� �A�
=A���A���Aȇ+A�O�A�$�A�  A��yA��`AǶFAǅA�n�A�E�A��A���A�M�A�C�A��Aģ�Ać+A�x�A�bNA�C�A�/A�+A��A�ƨAîA�hsA�{A��`A¸RA�A�VA�A�A�%A��!A�n�A�G�A� �A��HA�\)A�G�A�33A��A�%A��yA���A��A�VA��A���A���A��A�r�A�`BA�G�A�bA���A��hA�33A��A��FA��hA�hsA�S�A�9XA�"�A���A���A���A�l�A�(�A��/A���A�jA�C�A�7LA�"�A�  A��mA�A���A��DA��A�~�A�hsA�G�A��A��A��TA���A��mA�;dA��A�ĜA���A�l�A��mA�O�A���A��RA��hA�l�A�^5A�G�A�1'A��A�A��A��A��`A�ȴA��hA�n�A�dZA�I�A�9XA�(�A� �A�{A�A��`A��A�ƨA��9A��uA�hsA�33A��A���A�Q�A�JA��/A���A���A�O�A��A���A�z�A�5?A��A�A��uA�bNA�7LA��A���A��TA��#A���A���A��^A��9A��A���A���A��uA�~�A�n�A�S�A�"�A��`A���A�`BA��A���A�ȴA��A�|�A�5?A��mA���A��A�VA� �A�%A��TA�ȴA���A�`BA�^5A�Q�A�C�A�9XA�-A��A��A�ĜA��uA�bNA�/A�A��;A�ƨA���A�jA� �A��^A��DA�bNA�G�A�A�A�-A�A��A��HA��A���A��wA��9A���A��\A�v�A�?}A��yA�hsA�
=A��^A���A�l�A�E�A�JA��#A���A�hsA�?}A�/A��A���A��A���A��A���A���A���A��7A��A�|�A�n�A�bNA�A�A�33A�$�A��A�oA��A���A���A���A�z�A�jA�dZA�K�A�5?A��A�A��A�ƨA��RA��+A�G�A��A��A��jA��\A�r�A�O�A��A��A�ȴA���A��PA�z�A�dZA�ZA�K�A�C�A�/A��A��A��^A�hsA�K�A�5?A�(�A��A�bA�A��A��A��`A��`A��HA��A��^A�r�A�M�A�1'A�(�A� �A�{A���A��;A�ƨA���A���A�t�A�`BA�VA�Q�A�I�A�=qA�$�A���A���A�x�A�VA�M�A�C�A�=qA�;dA�7LA�5?A�1'A�-A�-A�+A�(�A�$�A��A��A��A�VA���A�ĜA��7A�XA� �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��Bk�BlWBk�BlWBlWBk�BlWBk�Bk�Bk�Bk�BlWBk�BlWBk�Bl"Bk�Bl"Bl"Bl"Bk�Bl"BlWBk�Bl"Bl"BlWBl"Bl"BlWBl"Bk�BlWBlWBlWBl"BlWBlWBlWBlWBl�Bl�BlWBl�Bl"Bl�Bl�Bm)Bn�Bv�By	By	B{B�B��B� B��B��B�VB�.B��B�B{�B��BŢB��B�jBںB�vB��B֡BیB�WB�mB��B iB	B_B�BB-�B=�B?�BA�BB�BR�BN�BHBH�BE�B@OB>wB0UB"hB�B�	B�B�B��BںB�aB�BB��B��B��B�B�xB��B{JB\�BXBS�B@B)�BfB
�5B
�)B
��B
�nB
��B
}�B
s�B
k�B
X�B
DgB
;�B
(�B
B
�B

=B	��B	�B	�>B	бB	��B	��B	�kB	��B	��B	��B	}�B	m�B	a�B	\)B	T�B	H�B	;dB	0�B	-wB	&�B	�B	�B	�B	VB	 B	\B	�B	YB	=B	IB	�B	!�B	#�B	'�B	(�B	,�B	/OB	5�B	9�B	<�B	D�B	P}B	W?B	XB	YB	]�B	e�B	gmB	jKB	m]B	q�B	xlB	x�B	v�B	�4B	u�B	qvB	l�B	kQB	p�B	m�B	iDB	h�B	g�B	h�B	ffB	f2B	j�B	l�B	kB	kQB	l�B	n/B	o�B	u�B	y�B	zxB	{B	|�B	��B	��B	�fB	��B	��B	��B	�hB	��B	��B	��B	�bB	��B	�bB	��B	��B	��B	�6B	��B	��B	�=B	�qB	�eB	��B	�-B	��B	�FB	�9B	�3B	�B	��B	�^B	�0B	��B	��B	�'B	�B	�?B	�RB	�B	�,B	҉B	��B	�,B	�QB	�5B	�5B	�;B	�QB	�;B	�B	� B	�B	��B	��B	�fB	�mB	�B	�mB	�8B	�8B	�`B	�B	��B	�&B	� B	�B	�,B	�B	�DB	�B	�B	��B	�B	�pB	ݘB	�)B	�QB	��B	�EB	�B	�
B	�9B	֡B	��B	�EB	�mB	�B	�B	�B	�B	�B	�WB	�WB	�cB	�GB	�oB	�vB	�B	�GB	�GB	��B	�MB	��B	�5B	�5B	��B	��B	�B	��B	�B	�%B	�B	�MB	�B	��B	��B	��B	�+B	�+B	�2B	�2B	�B	��B	��B
 iB
B
B
�B
�B
{B
B
_B
_B
_B
+B
�B
%B
_B
�B
fB
%B
�B

�B
JB
�B
xB
�B
B
MB
B
�B
�B
GB
YB
	B
B
�B

	B

	B

	B
	�B
	7B
	�B
	lB
	�B
	�B

rB
�B
B
B
xB
	�B
	7B
�B
fB
1B
1B
1B
�B
�B
�B
	B
�B
�B
	B
�B
1B
1B
�B
�B
	�B

rB
B
�B
�B
bB
�B
�B
bB
�B
@B
{B
�B
�B
YB
1B
qB
CB
�B
OB
�B
VB
!�B
!-B
!bB
#�B
$@B
%FB
&LB
(�B
*�B
+B
*�B
+kB
+�B
,B
,�B
.�B
/�B
/�B
1�B
1�B
1�B
1�B
2-B
1�B
1�B
2-B
2aB
4�B
49B
49B
4nB
4�B
4�B
4�B
5�B
5tB
5�B
5�B
5�B
5�B
5�B
6FB
6�B
6zB
8�B
8�B
;0B
:�B
;�B
<6B
<6B
<B
<B
<B
=B
=B
>�B
>wB
>�B
>�B
>�B
>�B
?B
?�B
?�B
?�B
@�B
@�B
A B
@�B
@�B
@�B
B[B
B�B
B�B
C-B
EB
D�B
D�B
D�B
DgB
DgB
DgB
DgB
EB
F?B
FB
FB
E�B
F�B
GzB
G�B
HB
H�B
H�B
IRB
IB
IRB
IRB
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K^B
LdB
L�B
L�B
L�B
NB
NpB
N<B
OBB
OB
PHB
P�B
PHB
P�B
P}B
P�B
QB
QNB
QNB
Q�B
Q�B
R�B
S&B
R�B
RTB
R�B
R�B
S�B
R�B
R�B
R�B
S[B
T,B
U2B
VmB
V�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bj�Bl"BlWBncBk�BjBm�Bm]Bk�BkBl�Bm]BlWBj�Bl�Bl�BkQBl"Bm)Bj�BkBm]Bl"Bk�Bl�Bm)BjBlWBl�BkBl�BlWBkBl"Bk�Bl�Bk�Bk�Bl"Bm]Bk�BkBl�Bk�Bk�BkBl�Bl�Bj�Bk�Bm)Bk�Bk�Bl�BlWBkBm)BlWBkBj�Bl�Bl�BkBkQBm)Bk�Bl"Bm)Bk�Bk�Bm)Bl"Bj�Bl�Bl�Bj�Bk�Bl�Bk�Bj�BlWBm)Bl"Bj�Bl"Bl�Bj�Bk�Bm�Bj�Bl�Bl�Bk�BkBlWBl�BkQBl"Bm]Bk�Bl"Bm)BlWBkQBl�Bm)Bk�BkBm)Bl�BkBk�Bm)Bk�Bj�BlWBm)BkQBj�Bm]Bk�Bj�Bm]BlWBj�Bk�Bl�Bl�Bk�BkBm)Bl�BkBl�Bl�Bj�Bl"Bm)Bl�BkBlWBl�BkBkQBm)Bm)BkQBkBm)Bk�BkBl�Bl�BkBlWBm]Bk�Bl"Bm�Bl�BkBk�Bl�Bl"BkQBl�Bm�Bk�BkQBjBlWBm)Bl"Bj�Bk�Bl�Bl�Bj�Bl"Bl�Bl"BkBl�Bm]Bm�BlWBj�Bk�Bm)Bl"Bk�Bl�Bl�Bk�Bk�Bm)Bl"BkQBl�Bm]Bk�BkBm�Bl�Bk�Bl"Bm]Bk�Bk�Bm]Bm�Bk�BlWBm]Bk�BkBk�Bl�Bl�BkBm�Bm]Bk�Bk�Bm]Bm]Bk�Bl�Bm�Bl�BkQBm]Bm]Bk�Bk�Bm)Bm]Bk�BkQBlWBm�BkBk�Bm�Bm]Bk�Bl�Bm�Bm]Bj�Bk�Bl�Bm)Bk�Bk�Bm�Bl�Bl"Bn/BlWBk�Bm]Bm)Bk�Bl�Bn/Bl"BjBk�Bm�Bm�BlWBm�Bo Bm�Bl�Bm�Bo Bt�Br|Bx�Bv�Bw2Bx8By>BzBw2By�Bz�Bw�Bx�Bz�Bx�Bv�Bx8BzBxlByrB~�B{�BzBz�B}�B}VB|PB}"B~]B��B��B�"B��B� B�bB�\B�bB�B�.B��B� B�4B��B��B�+B�$B��B��B�hB�B�B��B�xB��B�4B��B��B�JB��B��B��B��B��B�VB��B�1B�rB�xB�SB�B��B��B��B��B�iB�iB��B}VB}�B|BxlByrB{�B~�B{JB��B�fB��B�B�gB��BȀB�XB�6B��BĜB�pBбB��B͟B�BʌB��B��B��B�B��B�BٴB�jB�B�2B�}B�BбB�HB�}B�6B̘B�BбB��BچB�BбBѷBԕBרB�TBںB�B��B�mB�aB��B�/B�B�B�#BޞB�dB�BܒB�B��B�ZB�B�ZB�&B��B�,B�/B��B�B��B�(B��B��B��B�]B�B�B_B�BfB�BBSB�B�B�B�BeB�B=B7B=BqB�B�B�B7B �BYB�B$B9�B2�B/OB,�B+6B2�BD3BF�BB�B?�B?�B@�B<�B>�B@OB@�B@�B>BB=B>wBDgBE�BB[B?BAUBC�B>�B@�BB'B@�B?HB@�BCaB@�BCaBFtBL0BS�BS�BR�BQ�BN�BK�BP}BS�B[�BS�BM�BR�BR�BPHBNpBOBBOvBL0BL�BK�BIRBH�BJXBH�BG�BHBH�BFBGzBHKBGzBI�BK�BMjBK)BNBI�BF�B?�BD�BE�BHBHBC�BB�BH�BGEBB�BD�BD3BHKBB�B?B@�BB'B@�B?}B=qBB�BB�B@OB?B=<B>wB=B;0B<6B?HBD�BE9B6�B8�B4B/�B1[B1�B/�B-B+�B,�B,�B*�B(�B*0B'�B0!B)�B*eB�BB�B~B�B\BMB�B�B�B��B��B��B��B�]B��B��B�B�2B�8B�8B�B�8B�B��B�+B��B��B�B��B��B�;B�;B�vB��B�"B�B�B�WB�"B��B��B�`B��B��B�fB�&B��B�B�B�B�BߤB�jB�jB��B��B�QB�yB�KB��B�QB�sBںB�BB�BרB՛B�2B�gB��B҉B�TBуBуB�B�vB�B�aB�9BҽB��B�XBɺBǮB�jB̘B˒B��BĜB�B��B�OB��B�jB�<B�BB�UB��B��B�[B��B��B��B��B��B��B��B�'B��B�!B�'B�[B�[B��B�IB�[B�tB��B�hB�!444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021122621012120211226210121IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022010508011020220105080110QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022010508011020220105080110QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365620220126093656IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295920220204232959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295920220204232959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295920220204232959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                