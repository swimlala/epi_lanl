CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-12-15T14:35:52Z creation; 2022-02-04T23:30:06Z DMQC;      
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
_FillValue        G�O�        =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  R   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�        WH   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  lH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�        q�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�        �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�        �H   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �H   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   %�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T +�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ,<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ,D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ,L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ,T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ,\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ,�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ,�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    -    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        -    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        -(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       -0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    -8Argo profile    3.1 1.2 19500101000000  20211215143552  20220204223521  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_192                 6810_008521_192                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @٪_���@٪_���11  @٪_��r@٪_��r@0���}At@0���}At�db���<`�db���<`11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�\)@�\@@  @�  @�  @��R@�  @�p�A\)A!G�A,��A?\)A^�RA~�RA�  A�  A��A��A�  A�Q�A��B   B�
B  B�
B�
B(  B0(�B8(�B?�
BG�
BO�
BX  B`Q�Bh  Bp  Bw�B�  B�  B��B�{B�  B�  B��
B��B�{B�{B�{B�(�B��B�  B�{B��B��
B�{B�  B��
B��B��B��
B��B��B��B��B��B�{B�{B�  B�(�C {C  C��C��C
=C
  C��C  C  C�C  C  C�C��C�C��C   C"{C$  C&  C(
=C*  C+��C.  C/��C1�C3��C6
=C8{C9��C;�HC=��C@
=CB
=CD{CF{CH
=CI��CL
=CN  CP
=CR{CT�CV{CX  CY��C\  C^
=C`
=Cb
=Cd  Cf
=Ch(�Cj(�Cl
=Cm��Cp  Cr
=Cs��Cv  Cx
=Cy��C{��C}��C�C�C�C�  C���C���C�C�  C�  C�C�  C���C�  C�  C�  C���C�  C�C�  C�C�C���C�C���C�C�C�  C�  C�C�  C�  C�  C�C�  C�  C�  C���C�  C�  C���C�  C�  C���C���C�  C���C�C���C�C�  C���C�  C���C�C�  C�  C�C���C�  C�C�  C���C�C�C�  C�C�
=C�  C�  C�C���C���C�C���C�C�  C���C�  C�C�C�C�C�  C���C���C���C���C�C�C���C���C�  C�C���C���C�  C���C���C���C���C���C���C���C���C���C���C���C�  C���C�  C�  C���C���C�  C�  C�  C���C�  C���C�C�  C���C�  C�C�  C���C�C�D   D ��D  D}qD�qD� D�qDz�D��D� D  D� D�D��D  D��D�qD}qD�qD	� D
�D
� D  D}qD��D� D�D�D�D��D  D��D  D� D��Dz�D  D��D�D��D  D}qD�qD��D  D� D�D� D�qDz�D��Dz�D  D� D  D��D�qD}qD�D��D�qD� D�qD� D   D � D!�D!� D"�D"� D#  D#}qD$  D$��D%  D%}qD&  D&� D'�D'� D'��D(}qD(�qD)� D*�D*��D+�D+��D,  D,}qD-  D-� D-�qD.� D/D/� D/��D0}qD1  D1� D2  D2��D3�D3}qD3��D4� D5  D5z�D5�qD6� D7�D7� D8�D8�D9  D9� D:�D:��D;  D;}qD;�qD<� D=�D=��D>�D>� D?  D?z�D?�qD@��DA  DA� DBDB� DC  DC��DD�DD� DE�DE� DF  DF}qDG  DG��DH  DH� DI�DI� DJ�DJ� DK  DK��DL  DL� DL�qDM}qDM�qDN}qDO  DO��DP�DP� DP�qDQ��DR  DR� DS�DS� DT�DT� DU  DU� DV  DV}qDV��DW}qDW�qDXz�DY  DY��DZ�DZ��D[�D[� D\  D\� D]  D]� D^  D^}qD^�qD_� D_�qD`� Da�Da� Da�qDb��Dc�Dc}qDd  Dd��De  De� Df�Df� Dg�Dg��Dh�Dh� Di�Di�Dj�Dj��Dk�Dk�Dl�Dl��Dm  Dm}qDm�qDn� Do�Do}qDp  Dp}qDp�qDqz�Dq�qDr}qDr�qDs}qDt  Dtz�Du  Du� Du�qDvz�Dw  Dw� Dw�qDx}qDy  Dy��DzDz��D{  D{}qD|  D|}qD}  D}��D~�D~��D  D� D�HD�@ D�� D��HD�  D�>�D�~�D���D�  D�>�D�~�D�� D�  D�@ D�� D���D���D�@ D�� D��HD�HD�@ D�� D���D��qD�=qD�|)D��qD��qD�>�D��HD�� D��qD�>�D�� D��HD�HD�>�D�� D��HD�  D�@ D�� D�� D�HD�B�D��HD�� D���D�@ D��HD�� D�  D�@ D�� D�� D�  D�5�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?L��?aG�?�=q?���?�{?�
=?�ff?��@��@(�@��@.{@333@=p�@O\)@^�R@fff@xQ�@�ff@�=q@�z�@��H@�G�@�{@�
=@�p�@���@��@ٙ�@��@�\)@�@��RAA��A��A�\AA��A\)A#33A'
=A,��A0��A5�A:�HA?\)AC33AH��AO\)AS33AW�A^�RAc33Ag�Amp�Aq�Aw
=A|��A�Q�A��HA�{A�  A��HA�A��A�=qA�p�A�  A��A��A�Q�A���A�(�A�\)A���A��A��RA���A�33A�A���A��\A��A�Q�A�=qA�(�A�\)A��A��
A�{A�G�A��
A�Aأ�A��
A�A�  A��HA�A�  A�=qA��A�A�A�z�A�\)A���A�(�A��RB Q�Bp�B�HB  B��B=qB�B��B	B33BQ�BG�B�HBQ�B�BffB�
B��BB\)B��Bp�B�HB(�B�BffB�B z�B"{B#
=B$  B%p�B&�RB'�B(��B*=qB+33B,(�B-B.�RB/�
B0��B2ffB3\)B4z�B5�B733B8  B9�B:ffB;�B<z�B=�B?33B@Q�BA�BBffBC�BD��BE��BF�HBHQ�BIG�BJffBL  BM�BN{BO�BQ�BQ�BS33BT��BU�BV�HBXz�BYBZ�RB\  B]��B^�HB_�
BaG�Bb�RBc�
Bd��Bf�\Bg�
Bh��Bj=qBk�
Bm�Bn=qBo�Bq�BrffBs�Bt��Bv�RBw�
Bx��BzffB|(�B}G�B~ffB�{B���B�\)B�  B��HB���B�(�B���B��B�=qB��HB��B�Q�B�
=B��B�(�B���B��B�=qB���B���B�=qB��RB��B�=qB���B�\)B�(�B��HB�p�B�{B��HB��B�  B���B��B�  B��\B�G�B�{B���B�33B��B���B��B�B��\B��B��B�z�B�33B���B�=qB��B��B�{B��HB��B�(�B��RB��B�=qB���B�G�B�  B���B�\)B��B��\B�\)B�  B�z�B��B��
B�ffB��HB��B�Q�B���B��B�=qB��RB�G�B�  B���B��B��B�=qB��HB���B�{B���B�\)B�  B�ffB�
=B��B�Q�B��RB�\)B�{B��\B�
=B��B�ffB��HB�\)B�  BĸRB�G�BŮB�=qB��HBǅB�(�Bȏ\B�
=B�B�Q�BʸRB�33B��B�z�B��HB�\)B�  B�z�B���B�p�B�  B�ffB���B�\)B��B�ffBҸRB�G�B��
B�{B�ffB���BՅB�  B�Q�BָRB�33B�B�=qB؏\B���Bٙ�B�{Bڏ\B���B�G�B��
B�ffB���B��BݮB�(�Bޣ�B��HB�G�B�B�=qB��B��HB�G�B��
B�Q�B��B��HB�p�B��B�=qB��B�33B�B�{B�z�B���B�p�B�{B�ffB�RB�\)B��B�=qB��B�33B�B�{B�\B��B��B�{B�z�B�
=BB�{B�ffB��HB�B��
B�=qB��HB�G�B�B�  B�\B�
=B�\)B�B�ffB���B�\)B��B�(�B���B�\)B��B�{B��RB�G�B��B�  B��\B�33B���B�  B���B�33B���C   C \)C ��C �HC{CffC��C�
C{Cp�C�RC�HC(�Cz�C��C{C=qC�\C�HC(�CffC��C��CG�C�\C��C  CQ�C�C�HC�Cp�CC��C	33C	�C	�
C
{C
G�C
��C
�C(�CffCC{CQ�C��C�HCG�C�\C��C�C�C��C{CffCC�C\)C�C{Cp�C�RC��CG�C�C  CG�C�\C�HCG�C��C�
C33C�C�C�Cp�C�
C(�Cp�C�C{Cz�C�RC
=CffC��C{C\)C�C{Cp�C�C��CG�C�C{C\)C�C  Cp�C��C{C\)C�RC �C z�C ��C!{C!p�C!�
C"33C"z�C"��C#33C#��C#��C$=qC$��C%
=C%ffC%�C&�C&�C&�
C'(�C'z�C'�HC(G�C(��C(��C)=qC)�C*
=C*\)C*�C+  C+ffC+�
C,33C,z�C,�
C-G�C-�C.  C.G�C.�C/�C/p�C/�RC0{C0z�C0�HC1=qC1�\C1�HC2G�C2�C3
=C3\)C3��C4  C4p�C4��C5{C5ffC5��C633C6�C6��C7(�C7��C7��C8G�C8�\C8�HC9G�C9��C9�C:=qC:�C;
=C;G�C;��C;�C<G�C<�C=  C=G�C=��C>  C>\)C>�C>��C?=qC?��C?��C@Q�C@�C@��CAG�CA��CA��CBQ�CB�CB��CC33CC�\CC��CDG�CD�\CD�
CE�CEffCE��CF(�CFffCF�CF��CGQ�CG��CG��CHG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�\)@�\@@  @�  @�  @��R@�  @�p�A\)A!G�A,��A?\)A^�RA~�RA�  A�  A��A��A�  A�Q�A��B   B�
B  B�
B�
B(  B0(�B8(�B?�
BG�
BO�
BX  B`Q�Bh  Bp  Bw�B�  B�  B��B�{B�  B�  B��
B��B�{B�{B�{B�(�B��B�  B�{B��B��
B�{B�  B��
B��B��B��
B��B��B��B��B��B�{B�{B�  B�(�C {C  C��C��C
=C
  C��C  C  C�C  C  C�C��C�C��C   C"{C$  C&  C(
=C*  C+��C.  C/��C1�C3��C6
=C8{C9��C;�HC=��C@
=CB
=CD{CF{CH
=CI��CL
=CN  CP
=CR{CT�CV{CX  CY��C\  C^
=C`
=Cb
=Cd  Cf
=Ch(�Cj(�Cl
=Cm��Cp  Cr
=Cs��Cv  Cx
=Cy��C{��C}��C�C�C�C�  C���C���C�C�  C�  C�C�  C���C�  C�  C�  C���C�  C�C�  C�C�C���C�C���C�C�C�  C�  C�C�  C�  C�  C�C�  C�  C�  C���C�  C�  C���C�  C�  C���C���C�  C���C�C���C�C�  C���C�  C���C�C�  C�  C�C���C�  C�C�  C���C�C�C�  C�C�
=C�  C�  C�C���C���C�C���C�C�  C���C�  C�C�C�C�C�  C���C���C���C���C�C�C���C���C�  C�C���C���C�  C���C���C���C���C���C���C���C���C���C���C���C�  C���C�  C�  C���C���C�  C�  C�  C���C�  C���C�C�  C���C�  C�C�  C���C�C�D   D ��D  D}qD�qD� D�qDz�D��D� D  D� D�D��D  D��D�qD}qD�qD	� D
�D
� D  D}qD��D� D�D�D�D��D  D��D  D� D��Dz�D  D��D�D��D  D}qD�qD��D  D� D�D� D�qDz�D��Dz�D  D� D  D��D�qD}qD�D��D�qD� D�qD� D   D � D!�D!� D"�D"� D#  D#}qD$  D$��D%  D%}qD&  D&� D'�D'� D'��D(}qD(�qD)� D*�D*��D+�D+��D,  D,}qD-  D-� D-�qD.� D/D/� D/��D0}qD1  D1� D2  D2��D3�D3}qD3��D4� D5  D5z�D5�qD6� D7�D7� D8�D8�D9  D9� D:�D:��D;  D;}qD;�qD<� D=�D=��D>�D>� D?  D?z�D?�qD@��DA  DA� DBDB� DC  DC��DD�DD� DE�DE� DF  DF}qDG  DG��DH  DH� DI�DI� DJ�DJ� DK  DK��DL  DL� DL�qDM}qDM�qDN}qDO  DO��DP�DP� DP�qDQ��DR  DR� DS�DS� DT�DT� DU  DU� DV  DV}qDV��DW}qDW�qDXz�DY  DY��DZ�DZ��D[�D[� D\  D\� D]  D]� D^  D^}qD^�qD_� D_�qD`� Da�Da� Da�qDb��Dc�Dc}qDd  Dd��De  De� Df�Df� Dg�Dg��Dh�Dh� Di�Di�Dj�Dj��Dk�Dk�Dl�Dl��Dm  Dm}qDm�qDn� Do�Do}qDp  Dp}qDp�qDqz�Dq�qDr}qDr�qDs}qDt  Dtz�Du  Du� Du�qDvz�Dw  Dw� Dw�qDx}qDy  Dy��DzDz��D{  D{}qD|  D|}qD}  D}��D~�D~��D  D� D�HD�@ D�� D��HD�  D�>�D�~�D���D�  D�>�D�~�D�� D�  D�@ D�� D���D���D�@ D�� D��HD�HD�@ D�� D���D��qD�=qD�|)D��qD��qD�>�D��HD�� D��qD�>�D�� D��HD�HD�>�D�� D��HD�  D�@ D�� D�� D�HD�B�D��HD�� D���D�@ D��HD�� D�  D�@ D�� D�� D�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?L��?aG�?�=q?���?�{?�
=?�ff?��@��@(�@��@.{@333@=p�@O\)@^�R@fff@xQ�@�ff@�=q@�z�@��H@�G�@�{@�
=@�p�@���@��@ٙ�@��@�\)@�@��RAA��A��A�\AA��A\)A#33A'
=A,��A0��A5�A:�HA?\)AC33AH��AO\)AS33AW�A^�RAc33Ag�Amp�Aq�Aw
=A|��A�Q�A��HA�{A�  A��HA�A��A�=qA�p�A�  A��A��A�Q�A���A�(�A�\)A���A��A��RA���A�33A�A���A��\A��A�Q�A�=qA�(�A�\)A��A��
A�{A�G�A��
A�Aأ�A��
A�A�  A��HA�A�  A�=qA��A�A�A�z�A�\)A���A�(�A��RB Q�Bp�B�HB  B��B=qB�B��B	B33BQ�BG�B�HBQ�B�BffB�
B��BB\)B��Bp�B�HB(�B�BffB�B z�B"{B#
=B$  B%p�B&�RB'�B(��B*=qB+33B,(�B-B.�RB/�
B0��B2ffB3\)B4z�B5�B733B8  B9�B:ffB;�B<z�B=�B?33B@Q�BA�BBffBC�BD��BE��BF�HBHQ�BIG�BJffBL  BM�BN{BO�BQ�BQ�BS33BT��BU�BV�HBXz�BYBZ�RB\  B]��B^�HB_�
BaG�Bb�RBc�
Bd��Bf�\Bg�
Bh��Bj=qBk�
Bm�Bn=qBo�Bq�BrffBs�Bt��Bv�RBw�
Bx��BzffB|(�B}G�B~ffB�{B���B�\)B�  B��HB���B�(�B���B��B�=qB��HB��B�Q�B�
=B��B�(�B���B��B�=qB���B���B�=qB��RB��B�=qB���B�\)B�(�B��HB�p�B�{B��HB��B�  B���B��B�  B��\B�G�B�{B���B�33B��B���B��B�B��\B��B��B�z�B�33B���B�=qB��B��B�{B��HB��B�(�B��RB��B�=qB���B�G�B�  B���B�\)B��B��\B�\)B�  B�z�B��B��
B�ffB��HB��B�Q�B���B��B�=qB��RB�G�B�  B���B��B��B�=qB��HB���B�{B���B�\)B�  B�ffB�
=B��B�Q�B��RB�\)B�{B��\B�
=B��B�ffB��HB�\)B�  BĸRB�G�BŮB�=qB��HBǅB�(�Bȏ\B�
=B�B�Q�BʸRB�33B��B�z�B��HB�\)B�  B�z�B���B�p�B�  B�ffB���B�\)B��B�ffBҸRB�G�B��
B�{B�ffB���BՅB�  B�Q�BָRB�33B�B�=qB؏\B���Bٙ�B�{Bڏ\B���B�G�B��
B�ffB���B��BݮB�(�Bޣ�B��HB�G�B�B�=qB��B��HB�G�B��
B�Q�B��B��HB�p�B��B�=qB��B�33B�B�{B�z�B���B�p�B�{B�ffB�RB�\)B��B�=qB��B�33B�B�{B�\B��B��B�{B�z�B�
=BB�{B�ffB��HB�B��
B�=qB��HB�G�B�B�  B�\B�
=B�\)B�B�ffB���B�\)B��B�(�B���B�\)B��B�{B��RB�G�B��B�  B��\B�33B���B�  B���B�33B���C   C \)C ��C �HC{CffC��C�
C{Cp�C�RC�HC(�Cz�C��C{C=qC�\C�HC(�CffC��C��CG�C�\C��C  CQ�C�C�HC�Cp�CC��C	33C	�C	�
C
{C
G�C
��C
�C(�CffCC{CQ�C��C�HCG�C�\C��C�C�C��C{CffCC�C\)C�C{Cp�C�RC��CG�C�C  CG�C�\C�HCG�C��C�
C33C�C�C�Cp�C�
C(�Cp�C�C{Cz�C�RC
=CffC��C{C\)C�C{Cp�C�C��CG�C�C{C\)C�C  Cp�C��C{C\)C�RC �C z�C ��C!{C!p�C!�
C"33C"z�C"��C#33C#��C#��C$=qC$��C%
=C%ffC%�C&�C&�C&�
C'(�C'z�C'�HC(G�C(��C(��C)=qC)�C*
=C*\)C*�C+  C+ffC+�
C,33C,z�C,�
C-G�C-�C.  C.G�C.�C/�C/p�C/�RC0{C0z�C0�HC1=qC1�\C1�HC2G�C2�C3
=C3\)C3��C4  C4p�C4��C5{C5ffC5��C633C6�C6��C7(�C7��C7��C8G�C8�\C8�HC9G�C9��C9�C:=qC:�C;
=C;G�C;��C;�C<G�C<�C=  C=G�C=��C>  C>\)C>�C>��C?=qC?��C?��C@Q�C@�C@��CAG�CA��CA��CBQ�CB�CB��CC33CC�\CC��CDG�CD�\CD�
CE�CEffCE��CF(�CFffCF�CF��CGQ�CG��CG��CHG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A��yA��yA��yA��yA��A���A��A��A��A��A��A��yA���A�  A�A�A�A�A�A�%A�1A�1A�1A�1A�1A�
=A�
=A�JA�VA�VA�VA�bA�bA�oA�VA�VA�VA�bA�{A�{A�VA���A���AҲ-AҁA��A�VA�JA�JA�JA�A���A��A��#AѲ-Aѥ�AѓuAч+A�VAк^Aϰ!A�v�A˲-A�^5AǇ+AžwA�oA���A���A��RA�r�A��
A��A�C�A���A���A�%A���A��FA�^5A��`A�~�A��A�
=A�dZA��`A�{A���A� �A�ȴA�~�A�M�A���A���A��A�t�A���A�33A��A���A��DA�1A��;A���A�l�A��A�33A�x�A��RA�1A��A��A���A�ZA�Q�A�{A�ƨA� �A��A��hA��/A�-A�/A�z�A~(�Av1'Ao7LAl�`Ak"�Ai�Ad�RAc7LAbv�Aa�
A`��A]��AY��AT=qAPQ�AN�AL�AJZAG�AE�AA&�A;�A9�wA9oA8��A8�DA8VA8�A8JA7�;A7�A6�!A4�yA2~�A1K�A-�-A*r�A(ffA'�A&�+A&JA%�7A#p�A!XA ��A �Ap�Az�A��A�A`BA�Ap�A�A��A�A�AbNA�/A(�A��AjA�A(�AbNAQ�A��A
=A��A;dA��A�yA��AG�A�;A|�A�A"�A33A�AA��A�\A=qA��A�TA��A�wAS�A�\AbA�-AC�A
��A
ĜA
�uA
�+A
�uA
ffA
{A	C�A{A��A�/AE�AJA�#A�FA|�AdZAC�A~�A��A�AM�A��AS�A �A �/A �!A -@��
@�C�@�{@���@�&�@�@��@�/@���@���@�r�@�l�@�5?@��@�D@��@�S�@�+@��H@�~�@�-@�J@�@�I�@�P@@�=q@���@�X@�G�@��@�9@�1'@�33@�@�Q�@��;@�ƨ@�@�@�S�@�"�@���@�ff@�@�-@��@�?}@�D@�Q�@�I�@�S�@��@�?}@���@�b@ޗ�@�{@���@݁@ۥ�@�{@�X@���@ج@���@�`B@ӕ�@�G�@�dZ@́@�7L@�r�@��H@�^5@�@ə�@��;@��@Ĭ@�  @öF@�C�@�O�@�j@���@�
=@�v�@�{@��-@�p�@�G�@��@���@���@��D@���@���@�%@�%@���@�Ĝ@�j@�  @��
@�+@�v�@��@��@��u@�b@���@�;d@�ȴ@��!@�^5@��@��-@���@�`B@��/@�bN@� �@���@���@���@�-@�{@���@��@�@�?}@���@�ƨ@��H@���@���@�n�@�$�@�O�@���@��u@�z�@�Z@��@�dZ@�+@��@��H@��R@�v�@���@��@�9X@���@�|�@�"�@��@��R@���@�V@�5?@���@���@�p�@�7L@���@��@�9X@��w@�;d@���@�V@��@��-@���@��@�X@�G�@�&�@��@�Ĝ@�9X@���@�33@��@���@��+@�$�@���@��T@���@�`B@��@��/@���@�(�@��w@���@���@�t�@���@�5?@��@���@��h@�hs@���@�Ĝ@�bN@� �@��;@��@�C�@�ȴ@���@��\@��\@�~�@�ff@�M�@�{@���@���@���@��7@�p�@�G�@�%@���@��9@� �@��;@��
@���@���@�l�@�"�@��@���@�n�@�M�@�$�@��@��-@�hs@�?}@��@���@��D@� �@�  @��
@��@�l�@�@��H@��R@�-@���@���@��7@�x�@�G�@��@���@�1'@�ƨ@���@�"�@���@���@��#@���@�X@�V@��@�Ĝ@��u@�(�@�  @�t�@�33@�
=@��!@�n�@�=q@�5?@�-@��T@��#@��#@���@�@�@�@�@��^@��-@���@�p�@�?}@��@�Ĝ@��u@��@�j@�(�@�1@�ƨ@�K�@��y@�^5@�J@���@��^@���@��7@�X@�V@���@��j@���@��u@��@�1'@�@~�@~ff@}��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��A��A��A��TA��A��mA��mA��A��A��yA��A��A��HA��TA��A��yA��A��A��yA��A��A��A���A���A��A���A��A��yA��A��A��A��A��A���A���A��A��A��A��A��A��yA��A��A��mA��yA��A��yA��yA��A��A��A���A���A���A�A���A���A�A���A�  A�A���A�A�A�  A�  A�A�A�  A�A�%A�A�A�%A�  A�A�%A�A�  A�A�%A�A�A�%A�A�A�A�%A�A�A�%A�%A�A�A�1A�%A�A�%A�1A�%A�A�1A�1A�%A�1A�JA�%A�
=A�JA�%A�A�1A�1A�%A�1A�JA�%A�%A�
=A�1A�A�1A�
=A�%A�%A�
=A�1A�%A�
=A�
=A�%A�
=A�JA�%A�
=A�
=A�%A�
=A�
=A�1A�JA�JA�1A�
=A�VA�
=A�
=A�bA�JA�
=A�JA�bA�JA�
=A�bA�bA�JA�
=A�VA�VA�JA�VA�bA�VA�
=A�VA�bA�VA�JA�bA�oA�VA�VA�oA�oA�VA�bA�{A�bA�bA�{A�bA�VA�bA�oA�VA�bA�{A�oA�VA�oA�{A�JA�JA�oA�bA�JA�JA�oA�VA�JA�VA�bA�bA�JA�JA�oA�VA�JA�VA�oA�bA�VA�bA�oA�bA�VA�{A��A�oA�oA��A�{A�oA�oA��A��A�bA�VA�bA�bA�VA�JA�bA�VA���A���A�  A��A��TA��A��A��
A���A���AҾwA�ĜAҺ^AҲ-AҲ-Aҧ�Aҩ�AҬAҩ�AґhA�~�A�n�A�`BA�M�A�33A��A�VA�bA�bA�JA�JA�bA�JA�
=A�VA�bA�JA�JA�VA�bA�VA�
=A�
=A�VA�VA�
=A�
=A�VA�VA�
=A�
=A�VA�JA�1A�JA�JA�%A�1A�
=A���A���A���A���A���A���A��A���A���A���A��A��A��A��yA��yA��A��A��mA��`A��#A���A�ƨA�AѼjAѶFAѰ!AѰ!AѲ-AѮAѥ�Aѥ�Aѥ�Aѧ�Aѧ�Aѡ�Aѡ�Aѣ�Aѝ�AѓuAя\AёhAѓuAя\AэPAя\Aя\AыDAыDAэPAщ7AсA�x�A�r�A�jA�hsA�l�A�ffA�\)A�?}A�9XA�9XA�5?A�33A�&�A��A���AЅA�p�A�\)A�I�A�{A��A��/A���A�ȴAϬAϑhAχ+A�7LA�VA���A���AΑhA�z�A�n�A�dZA�5?A��mAͶFA�G�A���A�hsA�33A��mA�l�A�Aʉ7A��/A�G�A��
Aȩ�A�M�A�7LA��A�
=A�1A���A��HA���Aǟ�AǁA�t�A�p�A�^5A�7LA�&�A�oA���AƁAŬA�VA��;Aĉ7A�`BA��A�1A¥�A�+A�
=A�1A���A�A�5?A��A�~�A�K�A��A�%A���A�ffA�
=A�XA�(�A���A��
A���A�K�A�9XA�JA���A�+A��A��A�ƨA��FA���A��A���A���A�x�A�x�A�`BA�VA�S�A�E�A�5?A�"�A���A���A��!A��A�E�A�33A�
=A��HA��TA��`A��HA��
A���A���A���A� �A��A��A��A��A��A��mA��#A���A�ƨA��9A�p�A�/A��A��A��9A��PA�JA�VA�+A��A�
=A���A��A��HA���A���A�A��wA��FA��FA��^A��^A��RA��9A��9A��9A��-A���A��PA�dZA�C�A�(�A�JA�%A�A���A��yA��jA��A���A���A��PA�v�A�l�A�ZA��A��HA�VA���A��-A��\A�A�A�5?A��A�A��mA��#A���A���A�l�A�O�A�9XA�&�A�bA�1A���A��A���A��A�z�A�G�A��A�%A��A��A���A��A���A��hA��DA�r�A�ZA�-A�
=A���A��A��;A���A���A���A��jA���A���A��7A�v�A�n�A�l�A�dZA�`BA�VA�VA�G�A�%A�v�A��-A�r�A�S�A�/A�bA���A���A��A��7A�bNA�33A�A���A�K�A�n�A�JA���A���A�ffA�(�A��A���A���A��jA��FA��+A�t�A�E�A��A�?}A���A���A�l�A�I�A�-A���A���A�x�A�&�A��mA��
A�ĜA��^A��hA�7LA���A���A�z�A�dZA�?}A�&�A��A�bA�A�A���A���A��A��`A��TA��A��
A���A��jA��9A��A���A���A��DA��A�~�A�v�A�n�A�bNA�\)A�E�A�?}A�1'A�$�A�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��yA��yA��yA��yA��A���A��A��A��A��A��A��yA���A�  A�A�A�A�A�A�%A�1A�1A�1A�1A�1A�
=A�
=A�JA�VA�VA�VA�bA�bA�oA�VA�VA�VA�bA�{A�{A�VA���A���AҲ-AҁA��A�VA�JA�JA�JA�A���A��A��#AѲ-Aѥ�AѓuAч+A�VAк^Aϰ!A�v�A˲-A�^5AǇ+AžwA�oA���A���A��RA�r�A��
A��A�C�A���A���A�%A���A��FA�^5A��`A�~�A��A�
=A�dZA��`A�{A���A� �A�ȴA�~�A�M�A���A���A��A�t�A���A�33A��A���A��DA�1A��;A���A�l�A��A�33A�x�A��RA�1A��A��A���A�ZA�Q�A�{A�ƨA� �A��A��hA��/A�-A�/A�z�A~(�Av1'Ao7LAl�`Ak"�Ai�Ad�RAc7LAbv�Aa�
A`��A]��AY��AT=qAPQ�AN�AL�AJZAG�AE�AA&�A;�A9�wA9oA8��A8�DA8VA8�A8JA7�;A7�A6�!A4�yA2~�A1K�A-�-A*r�A(ffA'�A&�+A&JA%�7A#p�A!XA ��A �Ap�Az�A��A�A`BA�Ap�A�A��A�A�AbNA�/A(�A��AjA�A(�AbNAQ�A��A
=A��A;dA��A�yA��AG�A�;A|�A�A"�A33A�AA��A�\A=qA��A�TA��A�wAS�A�\AbA�-AC�A
��A
ĜA
�uA
�+A
�uA
ffA
{A	C�A{A��A�/AE�AJA�#A�FA|�AdZAC�A~�A��A�AM�A��AS�A �A �/A �!A -@��
@�C�@�{@���@�&�@�@��@�/@���@���@�r�@�l�@�5?@��@�D@��@�S�@�+@��H@�~�@�-@�J@�@�I�@�P@@�=q@���@�X@�G�@��@�9@�1'@�33@�@�Q�@��;@�ƨ@�@�@�S�@�"�@���@�ff@�@�-@��@�?}@�D@�Q�@�I�@�S�@��@�?}@���@�b@ޗ�@�{@���@݁@ۥ�@�{@�X@���@ج@���@�`B@ӕ�@�G�@�dZ@́@�7L@�r�@��H@�^5@�@ə�@��;@��@Ĭ@�  @öF@�C�@�O�@�j@���@�
=@�v�@�{@��-@�p�@�G�@��@���@���@��D@���@���@�%@�%@���@�Ĝ@�j@�  @��
@�+@�v�@��@��@��u@�b@���@�;d@�ȴ@��!@�^5@��@��-@���@�`B@��/@�bN@� �@���@���@���@�-@�{@���@��@�@�?}@���@�ƨ@��H@���@���@�n�@�$�@�O�@���@��u@�z�@�Z@��@�dZ@�+@��@��H@��R@�v�@���@��@�9X@���@�|�@�"�@��@��R@���@�V@�5?@���@���@�p�@�7L@���@��@�9X@��w@�;d@���@�V@��@��-@���@��@�X@�G�@�&�@��@�Ĝ@�9X@���@�33@��@���@��+@�$�@���@��T@���@�`B@��@��/@���@�(�@��w@���@���@�t�@���@�5?@��@���@��h@�hs@���@�Ĝ@�bN@� �@��;@��@�C�@�ȴ@���@��\@��\@�~�@�ff@�M�@�{@���@���@���@��7@�p�@�G�@�%@���@��9@� �@��;@��
@���@���@�l�@�"�@��@���@�n�@�M�@�$�@��@��-@�hs@�?}@��@���@��D@� �@�  @��
@��@�l�@�@��H@��R@�-@���@���@��7@�x�@�G�@��@���@�1'@�ƨ@���@�"�@���@���@��#@���@�X@�V@��@�Ĝ@��u@�(�@�  @�t�@�33@�
=@��!@�n�@�=q@�5?@�-@��T@��#@��#@���@�@�@�@�@��^@��-@���@�p�@�?}@��@�Ĝ@��u@��@�j@�(�@�1@�ƨ@�K�@��y@�^5@�J@���@��^@���@��7@�X@�V@���@��j@���@��u@��@�1'@�@~�@~ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��A��A��A��TA��A��mA��mA��A��A��yA��A��A��HA��TA��A��yA��A��A��yA��A��A��A���A���A��A���A��A��yA��A��A��A��A��A���A���A��A��A��A��A��A��yA��A��A��mA��yA��A��yA��yA��A��A��A���A���A���A�A���A���A�A���A�  A�A���A�A�A�  A�  A�A�A�  A�A�%A�A�A�%A�  A�A�%A�A�  A�A�%A�A�A�%A�A�A�A�%A�A�A�%A�%A�A�A�1A�%A�A�%A�1A�%A�A�1A�1A�%A�1A�JA�%A�
=A�JA�%A�A�1A�1A�%A�1A�JA�%A�%A�
=A�1A�A�1A�
=A�%A�%A�
=A�1A�%A�
=A�
=A�%A�
=A�JA�%A�
=A�
=A�%A�
=A�
=A�1A�JA�JA�1A�
=A�VA�
=A�
=A�bA�JA�
=A�JA�bA�JA�
=A�bA�bA�JA�
=A�VA�VA�JA�VA�bA�VA�
=A�VA�bA�VA�JA�bA�oA�VA�VA�oA�oA�VA�bA�{A�bA�bA�{A�bA�VA�bA�oA�VA�bA�{A�oA�VA�oA�{A�JA�JA�oA�bA�JA�JA�oA�VA�JA�VA�bA�bA�JA�JA�oA�VA�JA�VA�oA�bA�VA�bA�oA�bA�VA�{A��A�oA�oA��A�{A�oA�oA��A��A�bA�VA�bA�bA�VA�JA�bA�VA���A���A�  A��A��TA��A��A��
A���A���AҾwA�ĜAҺ^AҲ-AҲ-Aҧ�Aҩ�AҬAҩ�AґhA�~�A�n�A�`BA�M�A�33A��A�VA�bA�bA�JA�JA�bA�JA�
=A�VA�bA�JA�JA�VA�bA�VA�
=A�
=A�VA�VA�
=A�
=A�VA�VA�
=A�
=A�VA�JA�1A�JA�JA�%A�1A�
=A���A���A���A���A���A���A��A���A���A���A��A��A��A��yA��yA��A��A��mA��`A��#A���A�ƨA�AѼjAѶFAѰ!AѰ!AѲ-AѮAѥ�Aѥ�Aѥ�Aѧ�Aѧ�Aѡ�Aѡ�Aѣ�Aѝ�AѓuAя\AёhAѓuAя\AэPAя\Aя\AыDAыDAэPAщ7AсA�x�A�r�A�jA�hsA�l�A�ffA�\)A�?}A�9XA�9XA�5?A�33A�&�A��A���AЅA�p�A�\)A�I�A�{A��A��/A���A�ȴAϬAϑhAχ+A�7LA�VA���A���AΑhA�z�A�n�A�dZA�5?A��mAͶFA�G�A���A�hsA�33A��mA�l�A�Aʉ7A��/A�G�A��
Aȩ�A�M�A�7LA��A�
=A�1A���A��HA���Aǟ�AǁA�t�A�p�A�^5A�7LA�&�A�oA���AƁAŬA�VA��;Aĉ7A�`BA��A�1A¥�A�+A�
=A�1A���A�A�5?A��A�~�A�K�A��A�%A���A�ffA�
=A�XA�(�A���A��
A���A�K�A�9XA�JA���A�+A��A��A�ƨA��FA���A��A���A���A�x�A�x�A�`BA�VA�S�A�E�A�5?A�"�A���A���A��!A��A�E�A�33A�
=A��HA��TA��`A��HA��
A���A���A���A� �A��A��A��A��A��A��mA��#A���A�ƨA��9A�p�A�/A��A��A��9A��PA�JA�VA�+A��A�
=A���A��A��HA���A���A�A��wA��FA��FA��^A��^A��RA��9A��9A��9A��-A���A��PA�dZA�C�A�(�A�JA�%A�A���A��yA��jA��A���A���A��PA�v�A�l�A�ZA��A��HA�VA���A��-A��\A�A�A�5?A��A�A��mA��#A���A���A�l�A�O�A�9XA�&�A�bA�1A���A��A���A��A�z�A�G�A��A�%A��A��A���A��A���A��hA��DA�r�A�ZA�-A�
=A���A��A��;A���A���A���A��jA���A���A��7A�v�A�n�A�l�A�dZA�`BA�VA�VA�G�A�%A�v�A��-A�r�A�S�A�/A�bA���A���A��A��7A�bNA�33A�A���A�K�A�n�A�JA���A���A�ffA�(�A��A���A���A��jA��FA��+A�t�A�E�A��A�?}A���A���A�l�A�I�A�-A���A���A�x�A�&�A��mA��
A�ĜA��^A��hA�7LA���A���A�z�A�dZA�?}A�&�A��A�bA�A�A���A���A��A��`A��TA��A��
A���A��jA��9A��A���A���A��DA��A�~�A�v�A�n�A�bNA�\)A�E�A�?}A�1'A�$�A�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��Be�Bf2Bf2Bf�Be`BffBf2BffBffBf�Be�BffBe�Be�Be�Be�Be�Bf2Bf2Be�Be�Bf2Be�Bf2Be�Bf2Bf2Bf2Bf2Bf2Be�Bf2Bf2Be�BffBf2Bf�BffBe�Bf2Be�Bi�Bn�Bs�B{JB�B�B��B��B��B�nB�qB�!B�hB�RB��B��B�0B��B�jB��B��B�3B}VBz�B��B�jB(�B:�BI�BM�BU�BOBBF�BC�BJ�BC-BD3BB�BJ#BN<BM�B^�BZ�BZBW?BV�BU�BXEBT�BP�BN<BV9BDgBEmB.IB \B#nB�BfB�VB�B��B�AB�oB� B�5B�|BٴBѷB�dB��B��B�!B�iBhsBOBB1�B
��B
��B
̘B
��B
� B
n/B
[WB
/OB	��B	�B	�|B	�sB	�}B	��B	��B	�*B	��B	�B	~(B	k�B	W�B	M�B	HB	B[B	4nB	1[B	+kB	"4B	xB	kB	�B	+B	�B	1B	_B	�B	kB	�B	B	�B	B	uB	 iB�fB�AB�5B�"B�"B�;B��B�B�2B�cB��B�JB��B��B�B�oB�B	
=B	�B	�B	%�B	EmB	L�B	E�B	@�B	4B	,�B	4nB	6�B	B�B	F?B	@�B	G�B	MjB	PB	T�B	\�B	j�B	rGB	�YB	��B	��B	�HB	��B	ŢB	�#B	ΥB	�mB	�sB	�
B	��B	�]B	ߤB	��B	�/B	�5B	��B	ޞB	�HB	�B	�`B	�B	� B	��B	�B	�B	�B	�mB	�2B	�fB	��B	�B	�B	�ZB	�TB	�B	�/B	��B	�yB	خB	֡B	��B	�mB	�mB	��B	�
B	�EB	�B	��B	�mB	��B	уB	�HB	ϫB	�B	�BB	��B	ҽB	�&B	��B	�9B	�9B	�?B	�B	�EB	�EB	��B	��B	�jB	�pB	��B	�pB	�B	��B	�B	�B	�vB	�B	�B	�B	�`B	�,B	�`B	�B	��B	��B	�B	�B	��B	��B	�mB	��B	��B	�B	�B	��B	�)B	�WB	��B	� B	�AB	��B	�oB	��B	��B	�|B	�B	��B	�B	�B	�oB	�KB	��B	�,B	�HB	�HB	��B	�QB	��B	�WB	�]B	�B	�/B	�B	��B	��B	�B	�B	�
B	�KB	�B	�B	�QB	�WB	��B	�B	�|B	��B	��B	��B
B
�B
	B

	B
�B
�B
�B
�B
hB
SB
�B
YB
�B
eB
�B
�B
�B
YB
�B
�B
�B
+B
�B
�B
eB
7B
B
B
�B
B
B
IB
B
�B
�B
~B
�B
�B
�B
!B
VB
�B
VB
"�B
"hB
"�B
"�B
"hB
%B
$�B
%FB
%�B
%zB
%�B
&LB
&�B
(XB
*�B
+�B
-CB
-CB
-�B
-�B
.IB
.�B
.�B
.�B
/B
/�B
/�B
0�B
0!B
0�B
1�B
0�B
1�B
33B
2�B
2�B
2�B
3hB
3hB
33B
3hB
2�B
3hB
4nB
5?B
4�B
4�B
4nB
5?B
5�B
5�B
5�B
6�B
6B
7LB
6�B
7�B
8RB
9$B
8�B
8RB
8�B
:^B
;0B
;dB
;dB
;0B
;�B
<�B
=B
=�B
=�B
>B
?B
?B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
C-B
C�B
C�B
C�B
EB
EmB
EB
EB
E�B
F?B
F�B
G�B
G�B
HKB
HKB
H�B
H�B
IRB
I�B
J#B
JXB
J�B
K�B
K�B
K�B
L0B
K�B
L�B
L�B
K�B
L�B
M�B
M�B
NB
M�B
MjB
NpB
NB
OB
PB
PHB
O�B
P�B
P}B
Q�B
RTB
Q�B
R B
RTB
Q�B
RTB
R B
RTB
R�B
S�B
S[B
S�B
T�B
T�B
T�B
T�B
T�B
U�B
UgB
UgB
UgB
U�B
UgB
UgB
UgB
UgB
UgB
UgB
U�B
U�B
U�B
V9B
VB
U�B
VB
VB
VB
U�B
V9B
V9B
W?B
W?B
W�B
W�B
W�B
XB
XEB
X�B
X�B
YB
YB
YB
YB
YB
ZB
[�B
[�B
\�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Be�Be,Bb�Bf2Bg8Bd�Bg�Bh>BdZBf2Bh
Be,Be,BiBd�Bd�Bf�Bd�Bd�Bg8Be`Be�Be�Bd�Be�Bh�Be,Bf2Bg�Be�Be,BffBg8Bd�Be�Bf�Be`Bf�Bf�Be�Bf2Bh�Be�Bf�Bg8Bf�Be�Bf�BffBd�Be�BiBe�Bc Bf2Bd�BffBf�Bd�Bf�Bf�Bd�BgBe�Bd�Bf�Bf�Be,Be�Bg8Be�Be,Bf�Bf2Bd�BgBf�Bd�Be�BgBe�Bd�Bf�BffBd�BffBf�Be�Be,Bf�Bf�Bd�Be`BgmBffBd�Bf2Bg8Be�Be,Bf2Bf�Be`Be�BgBf2Be,Bg8Be�Be`BgBgmBe`Be�BffBffBd�Bf�BgBe`Be�Bg8Be�Bd�Bg8BgBe,Be�Bg8Be�Be`Bg8Be�Be,Bg8BffBe�BgmBe�Bf2Bg8Be�Be,Bg�Bf�Bd�Bf�Bf�Bd�Bf2BgmBf�Bd�BffBg8Be,Be,Bf�BgmBe�Be�Bg8Bf�Be,Be�BgmBe�Be,Be�Bg�Bf2Bd�BffBffBe,Be�Bg8Bf�Bd�Bf�Bf�Be`Bf2BgmBe�Be`Bg8Bf�Bd�Be�BgmBe�Bd�Bf�Bg8Be,Be�Bg�BgBd�BffBg8Bf2Be`Bf2BgBgmBe,BffBgmBf�Be,Be�Bg8Be�Bd�Bf�BgmBe�Bd�Bf�Bf�Be,Be�BgBf�Be`Be`Bf�BgmBe�Be,Be�BgBd�Be`Bk�BiBgmBj�BlWBj�Bl�Bm]Bo�BoiBs�BqBsBr�BrGBu�Bt�Br�BsMB{�B|�B.B�4B��B��B��B�"B��B��B��B�4B�hB��B��B�B��B��B��B�B��B��B�7B��B��B�qB��B�IB�B��B�~B�~B�CB�~B�VB�B��B�-B��B��B�zB�eB�0B��B��B�wB��B��B�B�B��B�B��B��B��B��B�UB�aB��B�hB�zB��B�B��B�LB�$B�$B��B��B��B�^B��B��B�$B��B�0B��B��B��B�jB�0B�^B��B�jB�^B��B��B��B��B��B�B��B��B��B��B�*B��B��B��B��B��B��B��B�6B��B�B�-B�*B�^B��B�HB��B��B��B��B��B��B��B�gB�zB�FB�^B�*B��B�wB��B�0B��B�B�hB�'B� B��B��B��B��B��B��B��B�	B��By�Bt�Br|Bs�BtBv�Bv�By>Bv�Bu�Bv�By>B�uB��B�+B�%B��B�B��B��B�B��B�B��B�B�B��B�&B�BޞBfB�B�B�BBOB�BYB?�BAUBC�B2�B7LB5tB;dBC�B7�B8�BD�BM�B>BBC-BC�BFBF�Bg�BC�BG�BLdBMjBQNBO�BN�BR�BT,BR�BT�BT�BV�BZBYBS[BW
BQ�BL�BK�BJ�BK^BFtBI�BMjBX�BA B?�B>wB?�BA BAUBD�BD3BB�BD3BM�BG�BCaBGzBE�BCaBYKBPBB�BB�BC-BB�BB�BB'BF�BCaBDgBC�BDgBC�BB[BB'BB'BC�BC-BB�BB�BC�BFtBPHBM�BL�BN�BM6BK�BK^BNBQ�BN<BL0BLdBNpBPHBK�BM6BR�B\�Bs�BXEB[�B^jB]dBX�B\�BY�B^BYBWsB`BBZQBZBYBVBW
BVmBU�BXyBW
BW�BZ�B[�BW�BS[BVBT�BT,BVBT�BVmBU�BY�BV�B\�BZ�BVBVBVmBV9BT,BS[BV�BTaBR�BU2BQNBN<BMBNpBN<BK^BI�BL�BZBpoBiyBL�BJ�BJ�BF?BD�BF�BGzBB'BA�B@OB>wB@�BRTBJ�B5�B2-B/B+kB.IB$�B#B"hB�B�B"hB�B �B)_B-CB#�B	BCB�B{B_B�BMBB	7B�B�B�B
	B�B �B��B��B�rB�ZB�`B�GB��B��B�B��B�B��B�B��B��B�B�AB�MB�B�B�;B�B�B�B��B�B�B�iB�B��B��B�5B��B��44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021121514355220211215143552IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021121620012120211216200121QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021121620012120211216200121QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365620220126093656IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295820220204232958IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295820220204232958IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295820220204232958IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                