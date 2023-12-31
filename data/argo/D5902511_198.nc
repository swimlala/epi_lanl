CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-01-25T20:49:21Z creation; 2022-02-04T23:30:08Z DMQC;      
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
_FillValue        G�O�        =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   U(   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�        [0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   sP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�        yX   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        �x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�          PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       "8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` :X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   :�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   @�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   F�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T L�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   M   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   M   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   M   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   M$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � M,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   M�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   M�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    M�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        M�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        M�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       N    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    NArgo profile    3.1 1.2 19500101000000  20220125204921  20220204223523  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_198                 6810_008521_198                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٴ�!Bo�@ٴ�!Bo�11  @ٴ�Ov_�@ٴ�Ov_�@0��D�_@0��D�_�d�t�ҳ�d�t�ҳ11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @@  @�  @��R@��R@�p�A   A��A!G�A-p�A@  A_\)A~�RA��A��A�Q�A�  A�  A�Q�A�  A��B  B  B  B (�B'�
B/�
B8  B@(�BHQ�BP(�BX(�B`  Bg�
Bo�
Bx  B�  B�  B�  B�  B�{B��B��
B�  B�(�B�  B�{B�  B�  B�  B�{B�{B��
B��B�  B�{B�  B�  B�  B�{B�(�B�  B��B�  B�  B�  B�(�B�(�C   C��C  C  C  C	��C��C  C  C��C��C�C�HC�C  C  C 
=C"{C${C&  C(
=C*  C+��C.  C0  C2  C4  C6  C8  C:
=C<  C=��C@  CA��CC��CE��CH  CJ
=CL
=CM��CO�CQ��CS�HCU��CX
=CZ{C\{C]��C_��Ca��Cd  Cf
=Ch
=Cj  Ck��Cn
=Cp
=Cr  Ct  Cv
=Cx
=Cz  C|
=C~{C�  C���C�  C�  C�C���C�  C���C���C�  C�C�  C�  C�  C�
=C�C�  C�  C�C���C�  C���C���C���C�C���C�  C���C���C�  C�C���C�  C���C�C�\C�C���C���C�C���C���C���C�  C�  C�  C�C�C�  C�  C�  C���C�  C�C�C�C���C�  C�C�  C�C�
=C�C�  C�  C�  C�  C�C�  C���C���C���C�  C�
=C�C���C���C�C�  C�C���C�  C�C�C�
=C���C���C�  C�  C���C���C���C���C�  C�
=C�C�  C���C�  C�  C�  C�  C�C�C�C�C�C�C�  C�  C�  C�  C�  C�C�  C���C���C�C�C�  C�C�\C�  C�  C�C�  C�  C���D   D � D �qD� D�D��D  D}qD�qD��DD��D  D}qD�qD}qD  D��D	�D	� D	�qD
}qD  D��D  D� D�D�D�D}qD�qD}qD�qD� D�D��D  D� D�D� D  D� D�D� D  D� D�D�D�D��D�D}qD��D}qD�D�D  D� D�qDz�D��D� D�D��D   D ��D!D!� D!�qD"� D#�D#� D#�qD$}qD%  D%��D&�D&��D'  D'� D(  D(� D)  D)�D*�D*� D+  D+��D,�D,� D-  D-}qD-�qD.� D/  D/�D0  D0� D1  D1� D2  D2� D2�qD3}qD4  D4}qD4�qD5� D6  D6}qD6�qD7}qD8  D8� D8�qD9}qD:  D:� D:�qD;� D<�D<��D=�D=� D=�qD>}qD?�D?��D@  D@� DA  DA� DB  DB� DB�qDC��DDDD� DD��DEz�DE��DF}qDG  DG� DH  DH}qDI  DI}qDJ  DJ� DK  DK� DK�qDL� DM  DM� DN  DN}qDN�qDO}qDO��DP� DP�qDQ� DR  DR��DS�DS� DT  DT� DU�DU� DU��DV}qDW�DW� DX  DX� DY  DY� DZ  DZ��D[�D[��D\�D\��D]  D]� D^  D^� D_  D_��D`D`��Da  Da}qDa��Db� DcDc��Dd  Dd� De  De}qDe�qDf� Dg  Dg� Dh�Dh}qDi  Di� Di�qDj}qDk  Dk��Dl  Dl}qDl�qDm}qDn  Dn� Do  Do��Dp  Dp� Dp�qDq��Dr�Dr� Dr�qDs}qDt�Dt�Du  Du}qDv  Dv� Dv��Dw}qDx�Dx�Dy�Dy��Dz�Dz� D{  D{��D|  D|� D}  D}� D~�D~� D~�qD}qD�  D�AHD�� D���D�  D�>�D�~�D���D�  D�AHD�� D�� D�HD�>�D�� D��HD�  D�@ D��HD�� D�  D�@ D�� D��HD�  D�@ D�� D���D���D�@ D�� D��HD�  D�>�D�}qD���D���D�@ D���D��fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��?.{?aG�?���?���?��?�@�@��@&ff@5@J=q@Tz�@fff@}p�@��\@���@�33@�(�@�ff@��@�
=@�  @�ff@У�@�Q�@�  @�@�z�@��HA�
A�A�AG�A�A=qA\)A#33A'�A-p�A0��A7
=A<(�A?\)ADz�AJ�HAN�RAR�\AX��A^{Aa�AfffAl��Ap��AuA{�A�Q�A�=qA�p�A�Q�A�=qA���A�  A��A�z�A��A���A��
A�
=A���A�33A�ffA�Q�A��\A�A��A�=qA��A��RA���A�(�A�A���AÅA��A�  A��A�(�A�
=A���A��HA�{A׮Aٙ�A���A�ffA���A�A��A�
=A�=qA�(�A�{A���A�=qA�p�A�
=A�G�A�z�A�B Q�BG�B=qB�
B��BB33B  B	�B
�\B\)B��B�B�RB(�BG�B{B�B��Bp�B�HB�
B��BffB
=Bz�BB�\B�
B!�B!�B"�HB$z�B%p�B&=qB'�B(��B)��B*�HB,  B,��B-�B/33B0  B1�B2ffB2�HB4Q�B5G�B5�B733B8Q�B9�B:=qB;�B<Q�B=G�B>�\B?�B@Q�BA�BB�HBC�BE�BFffBG\)BHz�BIBJ�HBK�BMG�BNffBO
=BPz�BQ��BR�\BT  BT��BV{BW33BX  BYG�BZffB[33B\��B]��B^�\B_�B`��BaBb�RBd(�Bd��Bf{Bg\)BhQ�Bi�Bj�\Bk�Blz�BmBn�HBo�Bq�Br=qBs
=Bt  Bup�Bv�\Bw33Bx��By��BzffB{�B|��B}B~�\B�{B��\B��HB��B�(�B��\B�
=B�B�  B���B�G�B��B�{B���B�G�B���B�=qB���B��B�B�=qB���B�G�B�B�(�B���B�\)B�B�{B��RB�G�B���B�{B���B��B���B�=qB���B��B���B�Q�B���B��B�B�Q�B��RB��B�B�=qB��\B��B�B�=qB��\B��B�B�  B���B�33B��B��B�z�B��B���B��B��\B��B�p�B��
B��\B��HB�G�B�  B�z�B��HB�33B��B�ffB��RB�33B��
B�(�B���B�G�B��B��B�z�B��B��B�B�ffB���B�33B��B�Q�B���B��B�B�{B�z�B���B���B��B�Q�B���B�p�B�B�(�B���B��B�p�B�{B��\B���B�G�B��
B�=qB�z�B���B���B��
B�(�B���B�33B��B��B��\B�
=B�\)B�B�Q�B��RB��B��B�(�B�z�B��HB��B�{B�ffB¸RB�G�B��B�Q�Bģ�B��B�B�(�B�ffB�
=BǙ�B��
B�Q�B��HB�\)BɮB�{Bʣ�B��B�p�B��
B�ffB���B�33BͮB�=qBΣ�B��HB�\)B��B�ffBУ�B�
=BѮB�{B�ffB��HB�p�B��B�=qBԸRB�\)BծB�{B֏\B��BׅB��B�z�B�
=B�p�B��
B�Q�B���B�\)BۮB�=qB���B�33Bݙ�B�  B�z�B��B�p�B��
B�ffB���B�p�B�B�(�B�RB�G�B㙚B�  B�\B��B�B�{B�z�B��B�p�B��
B�z�B�
=B�p�B�B�Q�B��HB�p�B��
B�Q�B���B�B�  B�ffB��HB�B�  B�ffB�
=B�B�{B�ffB�
=B�B�  B�ffB�
=B���B�  B�z�B��B��B��B���B�33B��B�  B���B�33B��B�(�B���B�G�B��B�(�B���B�p�B��
C (�C p�C C
=CG�Cz�C��C�CffC�\C�C=qCffC��C  CG�Cz�C��C(�CffC��C�HC=qC�C�RC  C\)C��C�
C{Cp�C�RC��C	(�C	z�C	��C
�C
G�C
�\C
�C33C\)C��C  C=qCz�C�RC{C\)C�\C�
C33Cp�C�C�C=qC�C�RC��CQ�C�CC{C\)C�C�HC�CQ�C��C�C�CQ�C��C��C33C\)C��C�HC(�Cp�C�RC�C�Cp�CC  C33Cp�CC{CG�C�C�HC�CQ�C�\C�
C(�Cp�C��C�HC33CffC��C�C33CffC��C��C=qCp�C��C�CG�Cp�C��C��C=qCp�C�C   C G�C p�C �C ��C!G�C!p�C!��C!�C"33C"�C"�RC"�HC#33C#z�C#�C#�HC${C$\)C$��C$�HC%
=C%G�C%��C%�HC&
=C&G�C&�\C&�
C'
=C'G�C'z�C'C({C(Q�C(�\C(C(��C)=qC)�\C)��C)��C*33C*�C*�
C+{C+33C+z�C+��C,{C,=qC,p�C,C-
=C-Q�C-��C-�
C.
=C.=qC.�\C.�HC/{C/Q�C/��C/�C033C0z�C0�C0�C1(�C1z�C1C2{C2=qC2z�C2��C3�C3ffC3��C3�
C4{C4\)C4�C4��C533C5z�C5�RC5�C633C6�C6��C7�C7Q�C7�C7��C8(�C8z�C8�C8��C9(�C9p�C9��C:�C:\)C:��C:�
C;{C;p�C;�RC<  C<33C<p�C<�RC=
=C=Q�C=��C=��C>{C>Q�C>��C>��C?=qC?z�C?C@  C@G�C@��C@�CA�CAffCA�RCB  CBQ�CB��CB�CC33CCz�CC�RCD  CDG�CD��CD��CEG�CE�CE�
CF{CFQ�CF�CG
=CGQ�CG��CG��CH=qCH�CH��CI{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@   @@  @�  @��R@��R@�p�A   A��A!G�A-p�A@  A_\)A~�RA��A��A�Q�A�  A�  A�Q�A�  A��B  B  B  B (�B'�
B/�
B8  B@(�BHQ�BP(�BX(�B`  Bg�
Bo�
Bx  B�  B�  B�  B�  B�{B��B��
B�  B�(�B�  B�{B�  B�  B�  B�{B�{B��
B��B�  B�{B�  B�  B�  B�{B�(�B�  B��B�  B�  B�  B�(�B�(�C   C��C  C  C  C	��C��C  C  C��C��C�C�HC�C  C  C 
=C"{C${C&  C(
=C*  C+��C.  C0  C2  C4  C6  C8  C:
=C<  C=��C@  CA��CC��CE��CH  CJ
=CL
=CM��CO�CQ��CS�HCU��CX
=CZ{C\{C]��C_��Ca��Cd  Cf
=Ch
=Cj  Ck��Cn
=Cp
=Cr  Ct  Cv
=Cx
=Cz  C|
=C~{C�  C���C�  C�  C�C���C�  C���C���C�  C�C�  C�  C�  C�
=C�C�  C�  C�C���C�  C���C���C���C�C���C�  C���C���C�  C�C���C�  C���C�C�\C�C���C���C�C���C���C���C�  C�  C�  C�C�C�  C�  C�  C���C�  C�C�C�C���C�  C�C�  C�C�
=C�C�  C�  C�  C�  C�C�  C���C���C���C�  C�
=C�C���C���C�C�  C�C���C�  C�C�C�
=C���C���C�  C�  C���C���C���C���C�  C�
=C�C�  C���C�  C�  C�  C�  C�C�C�C�C�C�C�  C�  C�  C�  C�  C�C�  C���C���C�C�C�  C�C�\C�  C�  C�C�  C�  C���D   D � D �qD� D�D��D  D}qD�qD��DD��D  D}qD�qD}qD  D��D	�D	� D	�qD
}qD  D��D  D� D�D�D�D}qD�qD}qD�qD� D�D��D  D� D�D� D  D� D�D� D  D� D�D�D�D��D�D}qD��D}qD�D�D  D� D�qDz�D��D� D�D��D   D ��D!D!� D!�qD"� D#�D#� D#�qD$}qD%  D%��D&�D&��D'  D'� D(  D(� D)  D)�D*�D*� D+  D+��D,�D,� D-  D-}qD-�qD.� D/  D/�D0  D0� D1  D1� D2  D2� D2�qD3}qD4  D4}qD4�qD5� D6  D6}qD6�qD7}qD8  D8� D8�qD9}qD:  D:� D:�qD;� D<�D<��D=�D=� D=�qD>}qD?�D?��D@  D@� DA  DA� DB  DB� DB�qDC��DDDD� DD��DEz�DE��DF}qDG  DG� DH  DH}qDI  DI}qDJ  DJ� DK  DK� DK�qDL� DM  DM� DN  DN}qDN�qDO}qDO��DP� DP�qDQ� DR  DR��DS�DS� DT  DT� DU�DU� DU��DV}qDW�DW� DX  DX� DY  DY� DZ  DZ��D[�D[��D\�D\��D]  D]� D^  D^� D_  D_��D`D`��Da  Da}qDa��Db� DcDc��Dd  Dd� De  De}qDe�qDf� Dg  Dg� Dh�Dh}qDi  Di� Di�qDj}qDk  Dk��Dl  Dl}qDl�qDm}qDn  Dn� Do  Do��Dp  Dp� Dp�qDq��Dr�Dr� Dr�qDs}qDt�Dt�Du  Du}qDv  Dv� Dv��Dw}qDx�Dx�Dy�Dy��Dz�Dz� D{  D{��D|  D|� D}  D}� D~�D~� D~�qD}qD�  D�AHD�� D���D�  D�>�D�~�D���D�  D�AHD�� D�� D�HD�>�D�� D��HD�  D�@ D��HD�� D�  D�@ D�� D��HD�  D�@ D�� D���D���D�@ D�� D��HD�  D�>�D�}qD���D���D�@ D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��?.{?aG�?���?���?��?�@�@��@&ff@5@J=q@Tz�@fff@}p�@��\@���@�33@�(�@�ff@��@�
=@�  @�ff@У�@�Q�@�  @�@�z�@��HA�
A�A�AG�A�A=qA\)A#33A'�A-p�A0��A7
=A<(�A?\)ADz�AJ�HAN�RAR�\AX��A^{Aa�AfffAl��Ap��AuA{�A�Q�A�=qA�p�A�Q�A�=qA���A�  A��A�z�A��A���A��
A�
=A���A�33A�ffA�Q�A��\A�A��A�=qA��A��RA���A�(�A�A���AÅA��A�  A��A�(�A�
=A���A��HA�{A׮Aٙ�A���A�ffA���A�A��A�
=A�=qA�(�A�{A���A�=qA�p�A�
=A�G�A�z�A�B Q�BG�B=qB�
B��BB33B  B	�B
�\B\)B��B�B�RB(�BG�B{B�B��Bp�B�HB�
B��BffB
=Bz�BB�\B�
B!�B!�B"�HB$z�B%p�B&=qB'�B(��B)��B*�HB,  B,��B-�B/33B0  B1�B2ffB2�HB4Q�B5G�B5�B733B8Q�B9�B:=qB;�B<Q�B=G�B>�\B?�B@Q�BA�BB�HBC�BE�BFffBG\)BHz�BIBJ�HBK�BMG�BNffBO
=BPz�BQ��BR�\BT  BT��BV{BW33BX  BYG�BZffB[33B\��B]��B^�\B_�B`��BaBb�RBd(�Bd��Bf{Bg\)BhQ�Bi�Bj�\Bk�Blz�BmBn�HBo�Bq�Br=qBs
=Bt  Bup�Bv�\Bw33Bx��By��BzffB{�B|��B}B~�\B�{B��\B��HB��B�(�B��\B�
=B�B�  B���B�G�B��B�{B���B�G�B���B�=qB���B��B�B�=qB���B�G�B�B�(�B���B�\)B�B�{B��RB�G�B���B�{B���B��B���B�=qB���B��B���B�Q�B���B��B�B�Q�B��RB��B�B�=qB��\B��B�B�=qB��\B��B�B�  B���B�33B��B��B�z�B��B���B��B��\B��B�p�B��
B��\B��HB�G�B�  B�z�B��HB�33B��B�ffB��RB�33B��
B�(�B���B�G�B��B��B�z�B��B��B�B�ffB���B�33B��B�Q�B���B��B�B�{B�z�B���B���B��B�Q�B���B�p�B�B�(�B���B��B�p�B�{B��\B���B�G�B��
B�=qB�z�B���B���B��
B�(�B���B�33B��B��B��\B�
=B�\)B�B�Q�B��RB��B��B�(�B�z�B��HB��B�{B�ffB¸RB�G�B��B�Q�Bģ�B��B�B�(�B�ffB�
=BǙ�B��
B�Q�B��HB�\)BɮB�{Bʣ�B��B�p�B��
B�ffB���B�33BͮB�=qBΣ�B��HB�\)B��B�ffBУ�B�
=BѮB�{B�ffB��HB�p�B��B�=qBԸRB�\)BծB�{B֏\B��BׅB��B�z�B�
=B�p�B��
B�Q�B���B�\)BۮB�=qB���B�33Bݙ�B�  B�z�B��B�p�B��
B�ffB���B�p�B�B�(�B�RB�G�B㙚B�  B�\B��B�B�{B�z�B��B�p�B��
B�z�B�
=B�p�B�B�Q�B��HB�p�B��
B�Q�B���B�B�  B�ffB��HB�B�  B�ffB�
=B�B�{B�ffB�
=B�B�  B�ffB�
=B���B�  B�z�B��B��B��B���B�33B��B�  B���B�33B��B�(�B���B�G�B��B�(�B���B�p�B��
C (�C p�C C
=CG�Cz�C��C�CffC�\C�C=qCffC��C  CG�Cz�C��C(�CffC��C�HC=qC�C�RC  C\)C��C�
C{Cp�C�RC��C	(�C	z�C	��C
�C
G�C
�\C
�C33C\)C��C  C=qCz�C�RC{C\)C�\C�
C33Cp�C�C�C=qC�C�RC��CQ�C�CC{C\)C�C�HC�CQ�C��C�C�CQ�C��C��C33C\)C��C�HC(�Cp�C�RC�C�Cp�CC  C33Cp�CC{CG�C�C�HC�CQ�C�\C�
C(�Cp�C��C�HC33CffC��C�C33CffC��C��C=qCp�C��C�CG�Cp�C��C��C=qCp�C�C   C G�C p�C �C ��C!G�C!p�C!��C!�C"33C"�C"�RC"�HC#33C#z�C#�C#�HC${C$\)C$��C$�HC%
=C%G�C%��C%�HC&
=C&G�C&�\C&�
C'
=C'G�C'z�C'C({C(Q�C(�\C(C(��C)=qC)�\C)��C)��C*33C*�C*�
C+{C+33C+z�C+��C,{C,=qC,p�C,C-
=C-Q�C-��C-�
C.
=C.=qC.�\C.�HC/{C/Q�C/��C/�C033C0z�C0�C0�C1(�C1z�C1C2{C2=qC2z�C2��C3�C3ffC3��C3�
C4{C4\)C4�C4��C533C5z�C5�RC5�C633C6�C6��C7�C7Q�C7�C7��C8(�C8z�C8�C8��C9(�C9p�C9��C:�C:\)C:��C:�
C;{C;p�C;�RC<  C<33C<p�C<�RC=
=C=Q�C=��C=��C>{C>Q�C>��C>��C?=qC?z�C?C@  C@G�C@��C@�CA�CAffCA�RCB  CBQ�CB��CB�CC33CCz�CC�RCD  CDG�CD��CD��CEG�CE�CE�
CF{CFQ�CF�CG
=CGQ�CG��CG��CH=qCH�CH��CI{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�AЮAЮAЩ�Aа!Aд9Aв-Aд9Aд9Aв-Aв-AжFAв-Aд9Aд9AоwA���A���A�A���A���A�ĜA�ĜA�ĜA�ƨA�ƨA�ȴA�ĜA�A�ĜA�ĜA�ƨA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A��
A�ȴA�r�A��A�Q�A���A�bNA�r�A̧�A˩�A�^5A���AǶFA���A�S�A��;A��hA�A�A�1'A��A���A�"�A�hsA��mA�XA��FA��`A��DA���A���A���A��A��A�%A���A�VA�bNA���A���A�A�C�A�ffA��^A�ZA�A�O�A��A�l�A�A�  A���A�1'A���A��7A�(�A���A��A��9A���A�x�A��mA��9A�v�A�A�A���A��A��#A�1'A�{A�x�A~�DA|��Az1'Au��An�yAjE�Ah��Ag�Ae�mA`��AZ��AX�AWAT�HAR{AM�hAKG�AH�jAE�AD$�AB�AB�AA��A?�A>�A<jA:=qA8��A7�hA6 �A1�FA0ZA/�7A.z�A-
=A+�A*A�A(�!A'�A&�DA%��A%G�A$�A$M�A#��A"�A"{A!"�A�RA33AQ�A33AffA(�Al�A�HA�DA�AC�A�HAv�AdZA��A\)A+A/A+A&�A��A�AbNAbAA33A��A�`A�+AA�/An�A��AC�A�AQ�AƨA
�HA
�uA	�;A	7LAz�AA�A�
A?}A��A1A�A9XAM�AffAQ�A��Al�A��A�
AS�AVA �A ��A jA {@�l�@���@�1'@���@�C�@�M�@��^@���@�r�@���@�+@���@�`B@�%@�Q�@�C�@��@�%@�@�b@�+@�@��@��T@���@蛦@��m@�P@�S�@��@�\@�^5@�@��@��@�\)@�=q@��@�Z@�33@��@�dZ@���@ޏ\@�n�@���@�l�@�@���@�^5@���@ٲ-@ٙ�@�G�@�V@��`@�r�@��
@���@���@�l�@�Z@�33@�M�@�p�@��#@�@ղ-@�&�@�A�@�  @ҟ�@�&�@�l�@�n�@�@͙�@�X@�?}@�`B@��@���@�j@���@�J@�X@�X@Ȭ@�1@���@��m@��
@Ǿw@�S�@��y@ƸR@Ɨ�@���@�Ĝ@Õ�@¸R@�J@�n�@��H@�n�@�=q@�^5@�ff@�M�@�{@��h@��@��D@��@���@���@�=q@��@�G�@�&�@��@��j@� �@�$�@��7@�%@��`@��@�I�@��F@�;d@��H@�n�@�V@�$�@��@���@�&�@��`@���@�Ĝ@���@��D@���@��@��@�z�@�Q�@�(�@��w@�\)@�"�@��@���@�V@���@���@��j@��D@��@�Z@��m@��@�$�@�-@�=q@��@��#@��@���@��@���@�z�@�r�@� �@��w@��F@�dZ@�;d@�ȴ@��\@�E�@�{@��@�7L@��/@�Ĝ@���@�bN@�b@���@�|�@�E�@�`B@��@���@��@���@�|�@���@���@�o@�ff@��T@��#@��^@�&�@��9@�I�@�  @���@���@�l�@�S�@�o@�@�"�@�K�@�S�@�
=@�v�@�J@���@��-@��@�%@��u@��@��@��@�t�@�dZ@�K�@�"�@�@��y@��H@�ȴ@���@�V@���@���@��@�hs@�&�@���@��/@��j@���@��@�\)@��@�^5@�5?@�J@��@�@���@�x�@��@���@�r�@� �@��m@���@�|�@�l�@��@��\@���@�X@�V@��/@�Ĝ@��D@�Z@� �@�ƨ@���@�+@��!@�-@�J@���@��7@�X@�V@�Ĝ@��@��@�r�@�bN@�Q�@�I�@�A�@�b@��m@��w@���@�K�@��H@�n�@�V@�{@�@��#@�O�@���@�A�@���@���@�S�@�@��H@���@�ff@�E�@�5?@�@��T@��#@��-@��@���@�z�@�Z@�I�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AЩ�Aв-AЮAа!AЩ�Aа!AЮAЬAЮAЩ�AЩ�AЬAЧ�AЩ�Aв-AЮAд9Aв-Aв-AиRAв-Aд9Aд9AЮAв-Aд9Aв-AиRAд9Aв-AиRAд9Aа!AжFAв-Aа!Aд9Aа!Aв-AжFAа!Aд9Aд9Aа!AЮAд9Aв-Aв-AжFAиRAв-Aв-AжFAв-Aв-Aд9Aд9Aа!Aд9AмjAмjAмjA�AоwAоwA�AмjAмjA�ĜAоwA���A�AоwA���A�AоwA���A�AоwA���A�ĜA���A�ĜA�AоwA�A���A���A�ĜA���AоwA�ĜAоwAоwA�ĜA���A�A�AоwA���A�ƨA�A�A�ƨA���A�ĜA���A�A�ƨA���A�ƨA�ĜA�A�ȴA�A�ĜA�ȴA�ĜA�ĜA�ȴA�ĜA�ƨA�ȴA�A�ƨA�ƨA�ĜA�ȴA�ȴA�ĜA���A�ƨA�ĜA���A�ĜA�ȴA���A�ƨA�ƨA���A�ƨA�ƨA���A�ȴA�ĜA���A�AоwA�ĜA�ĜA���A�A�ĜA���A�A�ĜA���A�ĜA�A���A�ĜA�ĜA���A�A�ƨA�ĜA���A�ĜA�ĜA�A�ƨA�ĜA�A�ƨA�ȴA�ĜA�ĜA���A�ƨA�ĜA�ȴA�ȴA�ȴA���A���A�ƨA���A�ƨA���A���A�ƨA���A���A�ȴA�ȴA�ȴA�ĜA�ȴA���A�ƨA�ƨA���A�ƨA�ƨA���A�ȴA�ƨA���A���A�ƨA���A���A�ȴA���A���A�ȴA�ȴA���A���A�ȴA���A���A�ȴA���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A��
A��
A���A���A���A���A���A���A���A���A���A��
A��
A���A���A��
A���A���A��
A��#A���A���A��#A���A���A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��#A��
A��
A��#A��#A��
A���A��A��
A���A���A���A���A���A���A���A�ȴA�ȴA�ĜAЮAЕ�AЏ\A�~�A�z�A�t�A�t�A�ffA�XA�5?A�1'A�+A�&�A��A��/A�Aϣ�AϓuAϑhAρA�hsA�\)A�Q�A�;dA�7LA�1'A�/A��A�oA�JA�A���A���A��A��;A�ȴAμjAξwAθRAΩ�A΃A�bNA�;dA���A�AͶFAͶFAͩ�A͓uÁA�^5A�I�A�1'A�/A��A�A��yA���A���A̛�A�~�A�`BA��A�%A��
A˶FAˡ�A˛�A˓uA˕�AˍPAˁA�z�A�x�A�l�A�`BA�^5A�XA�I�A�E�A�&�A��
A�`BA�AɾwA�z�A�K�A�&�A��A���Aș�A�$�A��A�XA�/A�(�A���AƼjA�K�A�33A�oAť�Aŗ�AŃA�9XA�oA��#AļjAĮAğ�A�|�A���AÍPA�E�A�+A��A���A��A���A¬AA�XA�A�A��A���A��\A�E�A��A���A��!A��\A�v�A�\)A�C�A�;dA�-A���A���A��9A��uA�33A�{A�%A���A�ƨA��uA��A��A�`BA�C�A�;dA�&�A�{A�%A��A���A���A�p�A�A��TA���A�\)A��A�A���A��mA��
A���A���A��+A�\)A�M�A�E�A�7LA�33A��A�  A��yA���A��
A�ƨA��+A�x�A�n�A�p�A�dZA�G�A�5?A�{A���A��A��9A��!A��A��hA�x�A�dZA�A�A���A���A���A��FA���A���A���A���A��PA��+A��DA��7A�z�A�O�A�"�A��mA�ĜA���A�ffA��A�  A��mA��wA���A���A���A��hA��A�|�A�dZA�=qA�%A���A�p�A��A��
A���A��RA���A��A�r�A�hsA�VA�E�A�7LA��A�bA�%A��A���A���A�hsA�S�A�?}A�&�A��A�%A��TA��-A�v�A�A���A��yA��`A���A���A�jA��A��-A�A��\A�&�A��#A���A���A���A���A��A�v�A�l�A�\)A�O�A�?}A�-A��A�1A���A��
A��jA��FA��!A�|�A�Q�A�I�A��A��A��A�=qA��A��A��!A�^5A�7LA��A���A��
A��wA��A���A���A��PA�~�A�S�A� �A��A��/A�A���A�|�A�^5A�ZA�O�A�E�A�33A�1A��A��/A�bNA��+A���A��A�9XA��hA�v�A�jA�ffA�\)A�M�A�33A�&�A��A��A�oA�bA�A��A���A��RA���A�z�A�E�A�$�A��A��A��A��A��A��A�{A�{A��A��A�VA��;A��RA��hA�p�A�=qA��A��A�1'A�bA���A��FA���A�t�A�`BA�Q�A�A�A�-A�{A���A��A���A�`BA�33A�bA��;A��-A�C�A���A���A�bNA���A���A��hA���A���A�|�A�v�A�jA�Q�A�{A���A�^5A�7LA��A��A��wA��uA�I�A���A��
A��^A�z�A�G�A���A�A��FA���A�n�A�1'A��A���A�C�A��jA�X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AЮAЮAЩ�Aа!Aд9Aв-Aд9Aд9Aв-Aв-AжFAв-Aд9Aд9AоwA���A���A�A���A���A�ĜA�ĜA�ĜA�ƨA�ƨA�ȴA�ĜA�A�ĜA�ĜA�ƨA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A��
A�ȴA�r�A��A�Q�A���A�bNA�r�A̧�A˩�A�^5A���AǶFA���A�S�A��;A��hA�A�A�1'A��A���A�"�A�hsA��mA�XA��FA��`A��DA���A���A���A��A��A�%A���A�VA�bNA���A���A�A�C�A�ffA��^A�ZA�A�O�A��A�l�A�A�  A���A�1'A���A��7A�(�A���A��A��9A���A�x�A��mA��9A�v�A�A�A���A��A��#A�1'A�{A�x�A~�DA|��Az1'Au��An�yAjE�Ah��Ag�Ae�mA`��AZ��AX�AWAT�HAR{AM�hAKG�AH�jAE�AD$�AB�AB�AA��A?�A>�A<jA:=qA8��A7�hA6 �A1�FA0ZA/�7A.z�A-
=A+�A*A�A(�!A'�A&�DA%��A%G�A$�A$M�A#��A"�A"{A!"�A�RA33AQ�A33AffA(�Al�A�HA�DA�AC�A�HAv�AdZA��A\)A+A/A+A&�A��A�AbNAbAA33A��A�`A�+AA�/An�A��AC�A�AQ�AƨA
�HA
�uA	�;A	7LAz�AA�A�
A?}A��A1A�A9XAM�AffAQ�A��Al�A��A�
AS�AVA �A ��A jA {@�l�@���@�1'@���@�C�@�M�@��^@���@�r�@���@�+@���@�`B@�%@�Q�@�C�@��@�%@�@�b@�+@�@��@��T@���@蛦@��m@�P@�S�@��@�\@�^5@�@��@��@�\)@�=q@��@�Z@�33@��@�dZ@���@ޏ\@�n�@���@�l�@�@���@�^5@���@ٲ-@ٙ�@�G�@�V@��`@�r�@��
@���@���@�l�@�Z@�33@�M�@�p�@��#@�@ղ-@�&�@�A�@�  @ҟ�@�&�@�l�@�n�@�@͙�@�X@�?}@�`B@��@���@�j@���@�J@�X@�X@Ȭ@�1@���@��m@��
@Ǿw@�S�@��y@ƸR@Ɨ�@���@�Ĝ@Õ�@¸R@�J@�n�@��H@�n�@�=q@�^5@�ff@�M�@�{@��h@��@��D@��@���@���@�=q@��@�G�@�&�@��@��j@� �@�$�@��7@�%@��`@��@�I�@��F@�;d@��H@�n�@�V@�$�@��@���@�&�@��`@���@�Ĝ@���@��D@���@��@��@�z�@�Q�@�(�@��w@�\)@�"�@��@���@�V@���@���@��j@��D@��@�Z@��m@��@�$�@�-@�=q@��@��#@��@���@��@���@�z�@�r�@� �@��w@��F@�dZ@�;d@�ȴ@��\@�E�@�{@��@�7L@��/@�Ĝ@���@�bN@�b@���@�|�@�E�@�`B@��@���@��@���@�|�@���@���@�o@�ff@��T@��#@��^@�&�@��9@�I�@�  @���@���@�l�@�S�@�o@�@�"�@�K�@�S�@�
=@�v�@�J@���@��-@��@�%@��u@��@��@��@�t�@�dZ@�K�@�"�@�@��y@��H@�ȴ@���@�V@���@���@��@�hs@�&�@���@��/@��j@���@��@�\)@��@�^5@�5?@�J@��@�@���@�x�@��@���@�r�@� �@��m@���@�|�@�l�@��@��\@���@�X@�V@��/@�Ĝ@��D@�Z@� �@�ƨ@���@�+@��!@�-@�J@���@��7@�X@�V@�Ĝ@��@��@�r�@�bN@�Q�@�I�@�A�@�b@��m@��w@���@�K�@��H@�n�@�V@�{@�@��#@�O�@���@�A�@���@���@�S�@�@��H@���@�ff@�E�@�5?@�@��T@��#@��-@��@���@�z�@�ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AЩ�Aв-AЮAа!AЩ�Aа!AЮAЬAЮAЩ�AЩ�AЬAЧ�AЩ�Aв-AЮAд9Aв-Aв-AиRAв-Aд9Aд9AЮAв-Aд9Aв-AиRAд9Aв-AиRAд9Aа!AжFAв-Aа!Aд9Aа!Aв-AжFAа!Aд9Aд9Aа!AЮAд9Aв-Aв-AжFAиRAв-Aв-AжFAв-Aв-Aд9Aд9Aа!Aд9AмjAмjAмjA�AоwAоwA�AмjAмjA�ĜAоwA���A�AоwA���A�AоwA���A�AоwA���A�ĜA���A�ĜA�AоwA�A���A���A�ĜA���AоwA�ĜAоwAоwA�ĜA���A�A�AоwA���A�ƨA�A�A�ƨA���A�ĜA���A�A�ƨA���A�ƨA�ĜA�A�ȴA�A�ĜA�ȴA�ĜA�ĜA�ȴA�ĜA�ƨA�ȴA�A�ƨA�ƨA�ĜA�ȴA�ȴA�ĜA���A�ƨA�ĜA���A�ĜA�ȴA���A�ƨA�ƨA���A�ƨA�ƨA���A�ȴA�ĜA���A�AоwA�ĜA�ĜA���A�A�ĜA���A�A�ĜA���A�ĜA�A���A�ĜA�ĜA���A�A�ƨA�ĜA���A�ĜA�ĜA�A�ƨA�ĜA�A�ƨA�ȴA�ĜA�ĜA���A�ƨA�ĜA�ȴA�ȴA�ȴA���A���A�ƨA���A�ƨA���A���A�ƨA���A���A�ȴA�ȴA�ȴA�ĜA�ȴA���A�ƨA�ƨA���A�ƨA�ƨA���A�ȴA�ƨA���A���A�ƨA���A���A�ȴA���A���A�ȴA�ȴA���A���A�ȴA���A���A�ȴA���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A��
A��
A���A���A���A���A���A���A���A���A���A��
A��
A���A���A��
A���A���A��
A��#A���A���A��#A���A���A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��#A��
A��
A��#A��#A��
A���A��A��
A���A���A���A���A���A���A���A�ȴA�ȴA�ĜAЮAЕ�AЏ\A�~�A�z�A�t�A�t�A�ffA�XA�5?A�1'A�+A�&�A��A��/A�Aϣ�AϓuAϑhAρA�hsA�\)A�Q�A�;dA�7LA�1'A�/A��A�oA�JA�A���A���A��A��;A�ȴAμjAξwAθRAΩ�A΃A�bNA�;dA���A�AͶFAͶFAͩ�A͓uÁA�^5A�I�A�1'A�/A��A�A��yA���A���A̛�A�~�A�`BA��A�%A��
A˶FAˡ�A˛�A˓uA˕�AˍPAˁA�z�A�x�A�l�A�`BA�^5A�XA�I�A�E�A�&�A��
A�`BA�AɾwA�z�A�K�A�&�A��A���Aș�A�$�A��A�XA�/A�(�A���AƼjA�K�A�33A�oAť�Aŗ�AŃA�9XA�oA��#AļjAĮAğ�A�|�A���AÍPA�E�A�+A��A���A��A���A¬AA�XA�A�A��A���A��\A�E�A��A���A��!A��\A�v�A�\)A�C�A�;dA�-A���A���A��9A��uA�33A�{A�%A���A�ƨA��uA��A��A�`BA�C�A�;dA�&�A�{A�%A��A���A���A�p�A�A��TA���A�\)A��A�A���A��mA��
A���A���A��+A�\)A�M�A�E�A�7LA�33A��A�  A��yA���A��
A�ƨA��+A�x�A�n�A�p�A�dZA�G�A�5?A�{A���A��A��9A��!A��A��hA�x�A�dZA�A�A���A���A���A��FA���A���A���A���A��PA��+A��DA��7A�z�A�O�A�"�A��mA�ĜA���A�ffA��A�  A��mA��wA���A���A���A��hA��A�|�A�dZA�=qA�%A���A�p�A��A��
A���A��RA���A��A�r�A�hsA�VA�E�A�7LA��A�bA�%A��A���A���A�hsA�S�A�?}A�&�A��A�%A��TA��-A�v�A�A���A��yA��`A���A���A�jA��A��-A�A��\A�&�A��#A���A���A���A���A��A�v�A�l�A�\)A�O�A�?}A�-A��A�1A���A��
A��jA��FA��!A�|�A�Q�A�I�A��A��A��A�=qA��A��A��!A�^5A�7LA��A���A��
A��wA��A���A���A��PA�~�A�S�A� �A��A��/A�A���A�|�A�^5A�ZA�O�A�E�A�33A�1A��A��/A�bNA��+A���A��A�9XA��hA�v�A�jA�ffA�\)A�M�A�33A�&�A��A��A�oA�bA�A��A���A��RA���A�z�A�E�A�$�A��A��A��A��A��A��A�{A�{A��A��A�VA��;A��RA��hA�p�A�=qA��A��A�1'A�bA���A��FA���A�t�A�`BA�Q�A�A�A�-A�{A���A��A���A�`BA�33A�bA��;A��-A�C�A���A���A�bNA���A���A��hA���A���A�|�A�v�A�jA�Q�A�{A���A�^5A�7LA��A��A��wA��uA�I�A���A��
A��^A�z�A�G�A���A�A��FA���A�n�A�1'A��A���A�C�A��jA�X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��BJ�BJ�BK)BJ�BJ�BJ�BJXBJ�BJ�BJ�BJXBJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BJXBJ�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BK)BK)BK^BK�BJ�BJ�BK�BK�BK)BK�BK�BK�BL0BK�BL0BK�BL�BMBM�BOvBQBS�B_pB�SB�1B�-B��B�$B��B�9B��B�0B�BBȀBƨB��B��B�dB�,B��B�B�5B�BbB�B�B%�B.}B-CB8�B<BF?BJ�BMBO�BWsBa|BXBU�BX�BS�BMjBGBFB*�B&LB�B+B�B�B�B��B�2B��B�tB�B��B{JBo B\�BOBB8RB*eB	�B
�`B
��B
�&B
�B
�{B
��B
xlB
oiB
\)B
Q�B
3�B
#nB	��B	��B	�B	�B	�zB	��B	�YB	��B	�YB	{B	iB	YB	S�B	J�B	A�B	<jB	9XB	9�B	49B	/OB	3hB	*�B	(�B	&B	%B	3�B	%FB	%B	&B	&LB	*�B	+B	.B	7B	9�B	A�B	@�B	A�B	GzB	L0B	QNB	R�B	[�B	q�B	y	B	}"B	�B	�uB	��B	�%B	��B	��B	��B	��B	�B	�1B	�oB	�4B	�B	�_B	�B	��B	�0B	�B	��B	�-B	�KB	˒B	�jB	�jB	̘B	�BB	��B	�aB	��B	��B	�B	՛B	��B	՛B	��B	��B	��B	�B	�B	�vB	�HB	уB	��B	҉B	ԕB	��B	�B	�B	��B	��B	��B	�B	�ZB	�B	�B	��B	�B	�)B	��B	�]B	��B	�B	�yB	�yB	�B	�5B	��B	�B	�vB	�B	�|B	��B	�oB	�GB	��B	��B	�B	�MB	�GB	�B	��B	��B	�B	�B	�B	��B	�B	�B	��B	�%B	��B	�B	�B	��B	�AB	��B	�)B	��B	�QB	��B	��B	�8B	��B	��B	�B	��B	�JB	��B	��B	�(B	�]B	��B	��B	�DB	��B	��B	��B	��B	�MB	�ZB
 iB	��B	��B	��B	�cB
�B
MB
�B
+B
�B
	B
SB	�]B	��B	��B	��B	��B
B
�B
fB
�B
	�B

rB
	lB
�B
fB
	�B
DB
�B
�B
B
�B
�B
�B
PB
�B
B
�B
B

=B
	B

=B
�B
VB
�B
�B
 B
:B
{B
�B
�B
4B
�B
�B
(B
.B
(B
�B
�B
�B
�B
�B

	B

�B

�B

�B
�B
�B
�B
�B
"B
�B
"B
�B
�B
�B
 B
4B
4B
�B
:B
�B
B
@B
@B
B
�B
B
�B
�B
B
�B
�B
�B
�B
VB
�B
�B
�B
VB
!bB
 �B
�B
!B
!�B
"4B
#B
$@B
!-B
!�B
B
$tB
'RB
'RB
&�B
(�B
(XB
'�B
)*B
(�B
(�B
(XB
(�B
)*B
(�B
(�B
(�B
(�B
(�B
($B
)_B
(XB
&�B
&LB
%�B
$�B
#nB
$tB
%�B
&�B
,qB
.}B
-�B
.B
.IB
/B
/�B
0!B
0�B
1�B
2�B
4�B
49B
3hB
3�B
4�B
6FB
7�B
8�B
9�B
:*B
:*B
9�B
:^B
;�B
<6B
=<B
=�B
>B
>B
>BB
>BB
>�B
>�B
>�B
>�B
>�B
?HB
?�B
A B
A B
A B
A B
A�B
A�B
A�B
A�B
AUB
B�B
C�B
C�B
D�B
E9B
E9B
E9B
E�B
E�B
F?B
F�B
F�B
HKB
H�B
HKB
IB
H�B
H�B
I�B
JXB
K�B
K�B
L�B
L�B
M6B
M�B
M�B
NpB
N<B
NpB
OB
PB
PHB
PHB
P�B
QB
QNB
R�B
R�B
R�B
S&B
R�B
R�B
S[B
S&B
S&B
S�B
S[B
S&B
S�B
S�B
T�B
U2B
UgB
VB
U�B
V9B
W
B
W
B
W�B
W�B
XB
X�B
XyB
X�B
Y�B
Y�B
Y�B
Y�B
ZQB
Z�B
ZB
ZQB
\)B
\)B
\�B
\�B
\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BK�BHKBK�BJ#BK�BJXBJ�BK�BJ#BK^BK^BJXBL0BK)BIBK�BJ#BK�BK^BI�BK�BJ�BJ�BK�BI�BI�BK�BIRBJ#BK�BI�BJ�BK�BI�BJXBK^BI�BK)BJ�BJ#BLdBI�BJ�BK�BK�BI�BJ�BK)BJ�BI�BK)BK^BI�BK^BK�BI�BJ�BLdBI�BI�BK�BJ�BIBK)BJ�BIBK)BLdBIRBK�BJ�BJ#BK�BJ�BJXBK�BJ�BI�BK�BK)BI�BK�BI�BJXBLdBI�BK�BJ�BI�BJ�BK�BIRBK^BJ�BIBK^BI�BI�BK)BJ�BI�BJ�BJ�BIRBK�BI�BK^BJ�BI�BK�BI�BJ�BK)BIBK)BJ�BI�BK)BJ�BIRBK)BJ#BI�BK�BJXBJXBK^BI�BI�BK�BIBJ�BK^BI�BK�BJXBI�BK)BJ�BI�BK^BK^BIBJXBK�BI�BK�BK�BI�BJ#BK�BJ�BJ#BK�BJ�BJ#BK�BJ#BJ�BL0BJ�BJXBL0BK)BI�BJ�BLdBJ�BJ�BK�BI�BK^BK�BJ�BI�BK^BK�BJ#BK�BL�BJ�BI�BK�BJ�BK�BK�BJXBL�BJ�BJ�BK�BK)BK^BLdBJ�BJ�BL0BJ�BI�BK�BK�BJ#BK�BK^BI�BK^BLdBJ#BJ�BL�BJ�BJ�BL�BJ�BK)BL0BLdBJ�BJ�BL�BJ�BJ�BL�BJ�BJ#BK^BL0BI�BK)BL�BK^BJXBL�BL0BJ�BMBK�BJ�BL�BL�BJ�BJ�BL�BJ�BJ�BM6BK)BK�BM6BK�BK�BMjBL�BK)BK�BMjBK�BK)BL�BL�BJ�BL�BL�BK)BJ�BMBL�BK)BK�BMBK^BK)BL�BL�BK^BK�BM6BMjBJ�BK�BMBK�BK)BMBK�BJ�BK)BL�BMBK�BK�BMjBL�BK�BM6BM�BK^BM6BNBH�BL�BNBNpBM6BMBOBM�BLdBM�BM�BL�BMjBO�BO�BOBBN�BP}BO�BM�BP}BOBOBBQ�BP�BO�BO�BR BQBR BS�BS�BQ�BRTBS�BT�BS�BTaBUgBU�BU�BY�B^�B\�B^jB`BBa�Bc Bm�ByrB}"B��B�uB��B�MB��B�"B�FB��B�B��B��B��B��B��B�\B��B��B��B�B�\B��B��B�4B�'B��B��B��B��B�hB�4B�B�:B��B�B��B��B�nB�_B��B��B�tB��B��B�=B�UB�aB��B��B�tB��B�'B�nB��B��B�aB��B�B��B�9B�[B��B�$B��B��B��B��B��B�BBÖB�tBŢB�BʌB̘B�<B��B�&B��B�B�B�2B̘B�jB�mB��BƨB�3B��B�#B��B�2B�gB� B�<B�jB�KB�gB�tB�pB�BB��B�3B� B��B�dB��B�XB�OB�9B��B�'B�B�UB�B��B�aB�-B��B��BB��BɺB��B��B�B��B��BҽB�&B�&B�2B��BҽB�mB�gB��B��B�B�/B�yB�QBߤB�&B��B�>B�B�>B��B�
B��B�B�B��B�WB�|B�B�%B�(BoB	lB�BB1B	B
�B"B"BuB.B�B:B�B�B�B$B�BSB�B�B�B~BkB�B �B�B!bB"�B%FB'�B$�B#�B(XB*�B)_B,�B8RB1[B,B/�B-CB/B-B-B-�B-wB+B+B.}B2�B5�B6�B6FB8�B<BA B:^B;�BA B=<B:�B9�B;�B<jB:*B>BB=�BEBD�BM�BT�BGEBG�BJXBL�BJ�BIBI�BK�BJ�BI�BLdBJ�BJ#BL�BJ�BT�BQNBM�BNBQBK�BP�BP�BM6B_pBR BQNBU�BT�BU�BYBZQB\�BaBm]Be,B\�B_pB`BBXEBW?BWsBYBYBYBXyBVmBV9BXyBW
BW�BT,BX�BT�BR�BR�BV�BS�BP}BXEBa�B_B_pB[�BPBYKB[WBS�BS�BS�BS[BP}BP}BP}BM�BL0BJ�BS�BQNBK)BH�BI�BJXBK�BGzBC�BC�BD�BC�BC�B?B>�BS�Bj�B;0B>�BD�B8RB-�B)_B(�B($B'�B(XB(�B'�B'B$�B$tB%FB'�B%FB#�B!�B#nB&�BBIB�B�B�BeB�B�B_B�B�B�BCBB�B�B�BxBB{B\B�B	7B	B�BB�B�BB iB��B �B{B��B��B��B��B��B��B� B�B�B�>B��B�2B�rB��B�zB�mB��B��B��B�#B�}B��B�B�*B��B�B�jB��B�_B�B��B�B��B��B��B�=B�B�7B�uB��B�YB�B�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                       4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                       4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2022012520492120220125204921IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022012523323620220125233236QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022012523323620220125233236QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365820220126093658IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423300020220204233000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423300020220204233000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423300020220204233000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                