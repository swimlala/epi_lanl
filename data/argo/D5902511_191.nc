CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-12-05T20:44:58Z creation; 2022-02-04T23:30:06Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  S�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  Y�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  vP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ƀ   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` -�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   -�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   3�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   9�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ?�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   @L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   @T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   @\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   @d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � @l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   @�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   A   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    A   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        A0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        A8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       A@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    AHArgo profile    3.1 1.2 19500101000000  20211205204458  20220204223520  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_191                 6810_008521_191                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @٧�e@٧�e11  @٧�    @٧�    @0sr�z%@0sr�z%�dT���&��dT���&�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?�@@  @��
@��
@�G�@�  A   A��A ��A+�A>�RA`  A\)A��A��A�\)A�\)AϮA�  A�  A��B  B�
B�
B   B(  B0  B7�
B@  BHQ�BP  BW�B_�
Bh  Bo�Bx  B�(�B�  B��B�{B�(�B��B�  B�(�B�  B�  B�{B�{B��B��
B�{B�(�B�{B�{B�  B��B�{B��B��B��B�{B�{B�  B��B�  B�  B�  B�  B��C
=C
=C
=C
=C
  C  C
=C  C  C
=C��C
=C
=C  C  C   C"  C$
=C&  C(
=C*
=C+��C.  C0  C2  C4
=C6  C8  C:
=C<
=C>  C@  CB  CD  CE��CG��CJ
=CL
=CN  CP
=CR  CS��CU��CX
=CZ
=C\{C^  C`
=Cb  Cd  Ce��Ch
=Ci��Ck��Cm��Co��Cr
=Ct
=Cv  Cx  Cz
=C|
=C~
=C�C�  C���C���C���C�
=C�  C�C�C�C���C�  C�C�  C���C�  C�  C���C���C�C�  C���C�C�C�  C�
=C�C���C�C���C���C�  C�C���C���C�  C�C�C�  C�C�\C�C�
=C�C���C���C�  C���C���C���C�  C�  C���C�C�C�  C�  C�  C�  C�
=C�C�  C���C�  C�C�C�  C�C�  C�  C�C�C���C���C�  C�  C�C�  C���C�  C�  C�  C���C���C���C�  C�  C�  C�  C���C���C�  C�  C�C�  C�  C�C�C�  C�  C�  C�C�C�C�C�C���C�  C�C���C���C�  C�C�  C���C�C�  C���C�  C�  C���C���C���C���C�  C���C���C�  D �D ��D  D}qD�qD� D  D}qD�qD}qD  D� D�qDz�D��D}qD�qDz�D��D	}qD
  D
}qD
��Dz�D�qD� D��D}qD�D�D�D��D�D��D  D� D�D��D�qDz�D��Dz�D��D� D  D��D�D}qD��D}qD�D��DD��D�qD� D  D}qD�qDz�D��D}qD�qD� D�qD ��D!D!� D"  D"� D#�D#��D$D$}qD$�qD%� D&�D&��D'D'�D(�D(}qD(�qD)z�D*  D*� D*��D+}qD,  D,}qD-�D-� D.  D.��D/  D/}qD/�qD0� D0�qD1}qD1�qD2� D2�qD3��D4�D4� D4�qD5xRD5�qD6��D7  D7}qD8  D8}qD9  D9��D:  D:��D;�D;� D;�qD<}qD=  D=}qD=�qD>��D?�D?�D@D@��DA�DA� DA�qDB� DB�qDC}qDC��DD}qDEDE�DF�DF� DG  DG��DH�DH��DI  DI}qDJ  DJ}qDJ�qDK��DL  DL}qDM  DM}qDM�qDN�DO  DOz�DO��DP}qDQ  DQ��DR�DR��DSDS� DS�qDT� DU  DU� DV  DV��DW�DW}qDW�qDX� DY�DY��DZ�DZ� D[  D[��D\D\�D]�D]� D]�qD^z�D^�qD_� D_�qD`� Da  Daz�Da�qDb}qDb�qDc��Dd�Dd�De  De� Df  Df� Dg�Dg��DhDh��Dh��Di}qDj  Dj� Dk  Dk� Dk�qDl}qDl�qDm}qDn�Dn� Dn��Doz�Do�qDp� Dp�qDq}qDr  Dr� Dr�qDs��DtDt�Du�Du��Dv�Dv� Dw  Dw� Dx�Dx��Dy  Dy� Dz�Dz��D{�D{��D|D|��D}  D}}qD}��D~� D�D}qD�qD�@ D�~�D��qD��qD�>�D�~�D�� D�HD�B�D���D�� D�  D�@ D�� D��HD�HD�@ D��HD��HD�HD�>�D�}qD���D���D�=qD�~�D��HD�HD�@ D�}qD���D�  D�@ D�� D�� D�  D�@ D��HD�� D�  D�AHD��HD��HD��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��
>�?L��?�\)?�{?��?�@��@#�
@.{@=p�@W
=@^�R@s33@��@�{@�@��
@���@��@�p�@��@�{@ٙ�@޸R@���@��@���A�A
=A
=qAG�Az�A
=A(�A!G�A$z�A(Q�A.{A2�\A5�A9��A>{AAG�AFffAJ=qAMp�AR�\AUAX��A^�RAaG�Ae�Aj�HAn{AqG�Aw
=AxQ�A~�RA���A�33A�p�A�Q�A�G�A��
A�A�\)A��A��A��A�  A�G�A�33A�{A�\)A��A�z�A�A�  A�33A���A�
=A�=qA�(�A��RA���A�33A�A���A\A�p�Aȣ�Aʏ\A��A�  Aљ�A�(�A�
=A�G�AۅA�{A߮A�\A��A�RA陚A��
A�{A�A��HA�z�A�ffA���A��\A�p�A�
=B z�B�B�HB�B��B{B�RB�
B	p�B
{B33BQ�Bp�B=qB�B��B��B
=B(�B��B�\B�Bz�B�B33B  B��BffB33B   B!��B"=qB#33B$z�B%��B&ffB'�B(��B)B*�RB,Q�B-�B-�B/\)B0Q�B1�B2=qB3�B4Q�B5�B6ffB733B8(�B9p�B:�RB;�B<z�B=B?\)B@(�BAG�BB�HBD  BD��BF�\BG�BH��BJ=qBK�BLz�BM��BO33BPQ�BQG�BR�HBT(�BT��BVffBW�
BX��BYB[\)B\z�B]G�B^�RB`  B`��Bb=qBc�Bdz�BeBg33BhQ�BiG�Bj�HBl(�Bl��Bn=qBo�
Bp��BqBs33Bt��Bu�Bv�HBxQ�ByBz�RB{�
B}p�B~�RB�B�ffB��B��B�{B���B��B�  B��\B�G�B��B�ffB���B��B�Q�B��HB�p�B�(�B��HB�G�B��
B���B�G�B���B�Q�B�
=B��B�{B��RB�\)B��
B�Q�B��B�B�(�B��RB�p�B�{B��\B�
=B�B�ffB��HB�p�B��B���B�G�B��B�(�B��HB��B�  B��\B�\)B�  B��\B�
=B��B�ffB���B�p�B�(�B���B�G�B�B��\B�
=B���B�(�B��HB�p�B��B��\B�G�B��
B�=qB��HB��B�{B���B�G�B�  B�ffB�
=B�B�Q�B���B�\)B�{B��\B�
=B��B�ffB��HB�\)B��B��RB�33B��B�=qB���B��B��B�z�B�33B�B�=qB��\B�G�B��B�ffB���B�G�B�  B�z�B��HB�\)B��Bģ�B��BŅB�  BƏ\B�33BǮB�{BȸRB�p�B�  B�z�B�
=BˮB�Q�B���B�\)B�  BΣ�B�G�BϮB�=qB��HBљ�B�(�Bҏ\B��B��
Bԏ\B�
=B�p�B�{B���B�p�B��
B�ffB��B��
B�Q�B���BۅB�(�Bܣ�B�33B�B�z�B�G�B߮B�=qB�
=B�B�(�B�RB�B�=qB�RB�G�B�  B�\B�
=B�B�ffB���B�B�{B���B뙚B�{B��B�\)B�{B��B�33B�  B�RB�33B�B�\B�G�B��B�z�B���B��B�z�B���B��B�Q�B�
=B���B�=qB���B�B�ffB��HB��B�ffB��HB��C (�C z�C �RC{Cz�C�RC
=Cz�C�
C{Cz�C�
C�Cp�C��C33Cp�C�RC�Cz�CC{Cz�C�
C{CffC�
C	�C	\)C	�RC
�C
ffC
�C{Cp�C�C
=Cp�CC
=C\)C��C33Cp�CC�C�C�
C�Cz�C�HC33Cp�C�HC=qC�C�
C33C��C�C=qC�C
=CG�C��C
=Cp�CC
=CffC�
C(�Cp�CC33C�\C�
C(�C�\C��CG�C�C�HCQ�C�C�C33C��C��C=qC�C�CG�C��C�HC =qC ��C �C!(�C!z�C!�
C"�C"\)C"��C"��C#G�C#p�C#��C#�HC$33C$ffC$�\C$�
C%
=C%33C%\)C%�\C%�
C&  C&�C&Q�C&�\C&�RC&�
C'�C'Q�C'ffC'��C'�HC(
=C(�C(\)C(��C(�
C)  C)�C)Q�C)�\C)��C)��C*{C*Q�C*�\C*�RC*�
C+
=C+G�C+z�C+��C+��C,  C,G�C,p�C,�C,��C-
=C-33C-\)C-��C-��C-��C.�C.ffC.��C.C.�C/33C/p�C/�C/�C/��C0(�C0=qC0z�C0C0��C1{C133C1z�C1�C1�
C2  C233C2p�C2�C2��C2��C3=qC3p�C3��C3C3��C4(�C4ffC4��C4C4�C5�C5\)C5��C5��C5�C6{C6Q�C6��C6��C6�C7�C7ffC7��C7��C7��C8(�C8p�C8�C8�
C9  C9G�C9�C9�C9��C:
=C:G�C:�\C:�C:�HC;{C;\)C;�\C;�RC;�HC<(�C<ffC<�\C<�RC<�C=33C=ffC=�\C=�RC>  C>=qC>\)C>�\C>C?
=C?=qC?z�C?��C?��C@  C@33C@p�C@��C@�HCA
=CA(�CA\)CA��CA�
CB
=CB=qCBffCB�\CB�RCB�CC33CCffCC��CCCC��CD33CDz�CD�CD�HCE  CEG�CE�CECF  CF�CFQ�CFz�CF�RCG  CG=qCGffCG�\CGCH
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                           1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  ?�@@  @��
@��
@�G�@�  A   A��A ��A+�A>�RA`  A\)A��A��A�\)A�\)AϮA�  A�  A��B  B�
B�
B   B(  B0  B7�
B@  BHQ�BP  BW�B_�
Bh  Bo�Bx  B�(�B�  B��B�{B�(�B��B�  B�(�B�  B�  B�{B�{B��B��
B�{B�(�B�{B�{B�  B��B�{B��B��B��B�{B�{B�  B��B�  B�  B�  B�  B��C
=C
=C
=C
=C
  C  C
=C  C  C
=C��C
=C
=C  C  C   C"  C$
=C&  C(
=C*
=C+��C.  C0  C2  C4
=C6  C8  C:
=C<
=C>  C@  CB  CD  CE��CG��CJ
=CL
=CN  CP
=CR  CS��CU��CX
=CZ
=C\{C^  C`
=Cb  Cd  Ce��Ch
=Ci��Ck��Cm��Co��Cr
=Ct
=Cv  Cx  Cz
=C|
=C~
=C�C�  C���C���C���C�
=C�  C�C�C�C���C�  C�C�  C���C�  C�  C���C���C�C�  C���C�C�C�  C�
=C�C���C�C���C���C�  C�C���C���C�  C�C�C�  C�C�\C�C�
=C�C���C���C�  C���C���C���C�  C�  C���C�C�C�  C�  C�  C�  C�
=C�C�  C���C�  C�C�C�  C�C�  C�  C�C�C���C���C�  C�  C�C�  C���C�  C�  C�  C���C���C���C�  C�  C�  C�  C���C���C�  C�  C�C�  C�  C�C�C�  C�  C�  C�C�C�C�C�C���C�  C�C���C���C�  C�C�  C���C�C�  C���C�  C�  C���C���C���C���C�  C���C���C�  D �D ��D  D}qD�qD� D  D}qD�qD}qD  D� D�qDz�D��D}qD�qDz�D��D	}qD
  D
}qD
��Dz�D�qD� D��D}qD�D�D�D��D�D��D  D� D�D��D�qDz�D��Dz�D��D� D  D��D�D}qD��D}qD�D��DD��D�qD� D  D}qD�qDz�D��D}qD�qD� D�qD ��D!D!� D"  D"� D#�D#��D$D$}qD$�qD%� D&�D&��D'D'�D(�D(}qD(�qD)z�D*  D*� D*��D+}qD,  D,}qD-�D-� D.  D.��D/  D/}qD/�qD0� D0�qD1}qD1�qD2� D2�qD3��D4�D4� D4�qD5xRD5�qD6��D7  D7}qD8  D8}qD9  D9��D:  D:��D;�D;� D;�qD<}qD=  D=}qD=�qD>��D?�D?�D@D@��DA�DA� DA�qDB� DB�qDC}qDC��DD}qDEDE�DF�DF� DG  DG��DH�DH��DI  DI}qDJ  DJ}qDJ�qDK��DL  DL}qDM  DM}qDM�qDN�DO  DOz�DO��DP}qDQ  DQ��DR�DR��DSDS� DS�qDT� DU  DU� DV  DV��DW�DW}qDW�qDX� DY�DY��DZ�DZ� D[  D[��D\D\�D]�D]� D]�qD^z�D^�qD_� D_�qD`� Da  Daz�Da�qDb}qDb�qDc��Dd�Dd�De  De� Df  Df� Dg�Dg��DhDh��Dh��Di}qDj  Dj� Dk  Dk� Dk�qDl}qDl�qDm}qDn�Dn� Dn��Doz�Do�qDp� Dp�qDq}qDr  Dr� Dr�qDs��DtDt�Du�Du��Dv�Dv� Dw  Dw� Dx�Dx��Dy  Dy� Dz�Dz��D{�D{��D|D|��D}  D}}qD}��D~� D�D}qD�qD�@ D�~�D��qD��qD�>�D�~�D�� D�HD�B�D���D�� D�  D�@ D�� D��HD�HD�@ D��HD��HD�HD�>�D�}qD���D���D�=qD�~�D��HD�HD�@ D�}qD���D�  D�@ D�� D�� D�  D�@ D��HD�� D�  D�AHD��HD��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��
>�?L��?�\)?�{?��?�@��@#�
@.{@=p�@W
=@^�R@s33@��@�{@�@��
@���@��@�p�@��@�{@ٙ�@޸R@���@��@���A�A
=A
=qAG�Az�A
=A(�A!G�A$z�A(Q�A.{A2�\A5�A9��A>{AAG�AFffAJ=qAMp�AR�\AUAX��A^�RAaG�Ae�Aj�HAn{AqG�Aw
=AxQ�A~�RA���A�33A�p�A�Q�A�G�A��
A�A�\)A��A��A��A�  A�G�A�33A�{A�\)A��A�z�A�A�  A�33A���A�
=A�=qA�(�A��RA���A�33A�A���A\A�p�Aȣ�Aʏ\A��A�  Aљ�A�(�A�
=A�G�AۅA�{A߮A�\A��A�RA陚A��
A�{A�A��HA�z�A�ffA���A��\A�p�A�
=B z�B�B�HB�B��B{B�RB�
B	p�B
{B33BQ�Bp�B=qB�B��B��B
=B(�B��B�\B�Bz�B�B33B  B��BffB33B   B!��B"=qB#33B$z�B%��B&ffB'�B(��B)B*�RB,Q�B-�B-�B/\)B0Q�B1�B2=qB3�B4Q�B5�B6ffB733B8(�B9p�B:�RB;�B<z�B=B?\)B@(�BAG�BB�HBD  BD��BF�\BG�BH��BJ=qBK�BLz�BM��BO33BPQ�BQG�BR�HBT(�BT��BVffBW�
BX��BYB[\)B\z�B]G�B^�RB`  B`��Bb=qBc�Bdz�BeBg33BhQ�BiG�Bj�HBl(�Bl��Bn=qBo�
Bp��BqBs33Bt��Bu�Bv�HBxQ�ByBz�RB{�
B}p�B~�RB�B�ffB��B��B�{B���B��B�  B��\B�G�B��B�ffB���B��B�Q�B��HB�p�B�(�B��HB�G�B��
B���B�G�B���B�Q�B�
=B��B�{B��RB�\)B��
B�Q�B��B�B�(�B��RB�p�B�{B��\B�
=B�B�ffB��HB�p�B��B���B�G�B��B�(�B��HB��B�  B��\B�\)B�  B��\B�
=B��B�ffB���B�p�B�(�B���B�G�B�B��\B�
=B���B�(�B��HB�p�B��B��\B�G�B��
B�=qB��HB��B�{B���B�G�B�  B�ffB�
=B�B�Q�B���B�\)B�{B��\B�
=B��B�ffB��HB�\)B��B��RB�33B��B�=qB���B��B��B�z�B�33B�B�=qB��\B�G�B��B�ffB���B�G�B�  B�z�B��HB�\)B��Bģ�B��BŅB�  BƏ\B�33BǮB�{BȸRB�p�B�  B�z�B�
=BˮB�Q�B���B�\)B�  BΣ�B�G�BϮB�=qB��HBљ�B�(�Bҏ\B��B��
Bԏ\B�
=B�p�B�{B���B�p�B��
B�ffB��B��
B�Q�B���BۅB�(�Bܣ�B�33B�B�z�B�G�B߮B�=qB�
=B�B�(�B�RB�B�=qB�RB�G�B�  B�\B�
=B�B�ffB���B�B�{B���B뙚B�{B��B�\)B�{B��B�33B�  B�RB�33B�B�\B�G�B��B�z�B���B��B�z�B���B��B�Q�B�
=B���B�=qB���B�B�ffB��HB��B�ffB��HB��C (�C z�C �RC{Cz�C�RC
=Cz�C�
C{Cz�C�
C�Cp�C��C33Cp�C�RC�Cz�CC{Cz�C�
C{CffC�
C	�C	\)C	�RC
�C
ffC
�C{Cp�C�C
=Cp�CC
=C\)C��C33Cp�CC�C�C�
C�Cz�C�HC33Cp�C�HC=qC�C�
C33C��C�C=qC�C
=CG�C��C
=Cp�CC
=CffC�
C(�Cp�CC33C�\C�
C(�C�\C��CG�C�C�HCQ�C�C�C33C��C��C=qC�C�CG�C��C�HC =qC ��C �C!(�C!z�C!�
C"�C"\)C"��C"��C#G�C#p�C#��C#�HC$33C$ffC$�\C$�
C%
=C%33C%\)C%�\C%�
C&  C&�C&Q�C&�\C&�RC&�
C'�C'Q�C'ffC'��C'�HC(
=C(�C(\)C(��C(�
C)  C)�C)Q�C)�\C)��C)��C*{C*Q�C*�\C*�RC*�
C+
=C+G�C+z�C+��C+��C,  C,G�C,p�C,�C,��C-
=C-33C-\)C-��C-��C-��C.�C.ffC.��C.C.�C/33C/p�C/�C/�C/��C0(�C0=qC0z�C0C0��C1{C133C1z�C1�C1�
C2  C233C2p�C2�C2��C2��C3=qC3p�C3��C3C3��C4(�C4ffC4��C4C4�C5�C5\)C5��C5��C5�C6{C6Q�C6��C6��C6�C7�C7ffC7��C7��C7��C8(�C8p�C8�C8�
C9  C9G�C9�C9�C9��C:
=C:G�C:�\C:�C:�HC;{C;\)C;�\C;�RC;�HC<(�C<ffC<�\C<�RC<�C=33C=ffC=�\C=�RC>  C>=qC>\)C>�\C>C?
=C?=qC?z�C?��C?��C@  C@33C@p�C@��C@�HCA
=CA(�CA\)CA��CA�
CB
=CB=qCBffCB�\CB�RCB�CC33CCffCC��CCCC��CD33CDz�CD�CD�HCE  CEG�CE�CECF  CF�CFQ�CFz�CF�RCG  CG=qCGffCG�\CGCH
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                           1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�$�A�$�A� �A��A��A�(�A�+A�(�A�$�A��A��A�$�A�1'A�33A�7LA�5?A�7LA�7LA�7LA�7LA�9XA�9XA�9XA�=qA�A�A�C�A�E�A�E�A҇+A���A��yAӍPA�t�A�ZA�+A�A���A��TA���AҼjAҧ�AғuA�x�A�ZA�G�A�{A��AѰ!Aѣ�AѓuA�C�A���A�A�A�&�A�l�A���A� �A�$�AÅA��A�~�A���A�$�A�ZA�v�A���A�z�A��wA���A��A�ĜA���A���A���A��yA���A�ĜA��uA�bA�1A��#A���A�-A���A�9XA�M�A���A��hA���A���A��A�-A���A�|�A��mA��A���A��mA�VA���A���A� �A�9XA�=qA�t�A�+A��
A��A���A�JA�TAyC�At�+Aq&�Am;dAi�Ae�AcoA`v�A^A�A\��AY�AX��AW�PATĜAP��AO;dAJA�AH��AEXAB��A@1'A?A=K�A:��A733A4��A1��A/A,��A*�HA(�A'��A%A%%A$�A$Q�A#dZA#XA"�DA ��A33A5?A33A��A��AE�A�AdZA��A�A$�A��A�A�!AI�A^5Ax�AI�A�;AhsA�uA�TAt�A��A�uA/A`BA�7At�A|�A�
A�A��A��A�!AI�A  A+A=qA��AdZA7LA
�`A
��A	��A	|�A	x�A	t�A	O�A�!AZAE�A�;AG�AoA�A�HAE�A��A|�AK�A�Av�A9XA{A1A��A��AjAQ�A �A`BA�A ��A E�@�"�@�V@��@���@��@��#@��u@�I�@�l�@�V@��@�z�@��;@�@��@�+@�O�@�%@�1'@��;@�@�ȴ@�G�@�@� �@� �@�w@��@��@�o@��y@�7@�O�@�u@�b@�v�@���@�K�@ް!@�{@�&�@�A�@�\)@ڏ\@���@�V@ج@�Q�@��;@��@�$�@�p�@�V@ԓu@ӥ�@�~�@��@�J@��#@ѩ�@�p�@�p�@�p�@�G�@ЋD@��m@�C�@���@�ȴ@���@�$�@��@��`@̴9@�bN@���@ˮ@�\)@���@�ff@�=q@��@�&�@�V@���@�1'@�^5@��T@��#@�@�`B@Ĵ9@�1@Ý�@�"�@�@�{@���@�hs@�7L@��9@�r�@�1'@��;@��@��R@���@�7L@���@�bN@�  @��@���@�V@���@�x�@�7L@�Ĝ@��@���@��@�1@�t�@�K�@��y@�n�@�$�@�x�@���@��9@���@�l�@�"�@���@�E�@�J@��-@�G�@�V@��`@�Ĝ@�z�@�Q�@�A�@���@��@�l�@��@���@�~�@�@�?}@��/@���@�Ĝ@�Ĝ@��j@���@��@��@�  @��F@���@��+@�E�@��@��@�%@��j@�A�@���@��@�@���@��+@�-@���@�p�@�hs@�hs@�O�@�7L@��@���@���@��`@��D@��
@�dZ@�@�n�@�@�X@�/@��/@��u@�bN@��;@�dZ@�C�@��@��H@��!@�ff@�J@��@���@�`B@�Ĝ@�Z@���@�ƨ@���@��@�S�@��y@���@�ff@�=q@�$�@�{@��@���@�7L@���@��@�ƨ@�dZ@�o@��+@�J@��@��@���@�Ĝ@���@�j@�1@��
@��P@�|�@�t�@�l�@�C�@�@���@���@�E�@��@���@��^@�`B@�V@�j@��@��
@�|�@�\)@�C�@�33@�
=@��y@�ȴ@���@�^5@��T@�`B@�/@��@���@�j@�ƨ@��P@�S�@�
=@���@���@���@��+@�n�@�V@�M�@�E�@�E�@�5?@�$�@���@�?}@��@�r�@�I�@�9X@� �@��;@��@�t�@�K�@�"�@��H@��!@�5?@��@���@�@���@��@�O�@�7L@���@�bN@�b@�b@�1@���@��
@��w@���@��P@�l�@�\)@�C�@�o@�ȴ@�^5@�=q@�$�@�{@���@���@���@��h@��@�O�@��@���@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A�&�A�$�A�$�A�(�A�"�A�&�A�&�A��A��A� �A��A��A� �A��A��A� �A��A�&�A�+A�+A�&�A�/A�+A�-A�&�A�(�A�+A�(�A�$�A�$�A�&�A�"�A��A�$�A�oA��A��A� �A��A�"�A�+A��A�$�A�&�A�(�A�/A�/A�/A�33A�1'A�/A�5?A�33A�1'A�5?A�-A�33A�7LA�33A�33A�9XA�33A�7LA�7LA�33A�9XA�7LA�5?A�7LA�33A�5?A�9XA�33A�5?A�9XA�33A�5?A�9XA�5?A�5?A�9XA�7LA�5?A�9XA�5?A�7LA�9XA�5?A�7LA�9XA�5?A�7LA�9XA�5?A�7LA�;dA�7LA�7LA�9XA�5?A�7LA�7LA�33A�9XA�=qA�9XA�7LA�=qA�9XA�7LA�;dA�5?A�;dA�9XA�9XA�=qA�;dA�9XA�;dA�;dA�5?A�7LA�;dA�;dA�9XA�?}A�;dA�9XA�?}A�?}A�9XA�?}A�A�A�=qA�A�A�C�A�?}A�C�A�E�A�A�A�A�A�E�A�C�A�C�A�G�A�E�A�C�A�G�A�G�A�C�A�G�A�G�A�C�A�E�A�I�A�G�A�E�A�I�A�G�A�A�A�C�A�G�A�A�A�?}A�E�A�A�A�A�AҍPA�oA�x�Aӧ�A�ĜA���A�ƨA���A��A���A�  A�
=A�A��`A���AӾwAӝ�AӑhAӍPAӉ7AӅAӉ7AӅA�z�A�x�A�x�A�p�A�hsA�jA�ffA�^5A�`BA�^5A�VA�VA�Q�A�E�A�?}A�9XA�"�A��A��A�VA�%A�A�%A�A���A���A�A���A���A���A���A���A��A��A��A��TA��HA��HA���A���A���A���A���A���A���A���A�ȴA�AҾwAҼjAҰ!AҴ9AҴ9AҮAҧ�Aҥ�Aҥ�Aҥ�Aҟ�Aҝ�Aҝ�Aҕ�Aҏ\Aҏ\AҍPA҉7A҅A҅AҁA�v�A�n�A�jA�bNA�^5A�\)A�^5A�^5A�ZA�Q�A�M�A�O�A�M�A�I�A�G�A�I�A�?}A�5?A�+A�$�A��A�
=A�  A���A��A��yA��HA��/A���AѺ^AѶFAѺ^AѴ9AѰ!AѰ!AѰ!AѬAѥ�Aѥ�Aѧ�Aѥ�Aѡ�Aѡ�Aѣ�Aѡ�Aѝ�Aѝ�Aљ�Aя\AыDA�t�A�XA�G�A�C�A�C�A�=qA�5?A�/A�/A�$�A�{A���A��A���AиRAП�AЇ+A�jA�9XA�"�A�VA���A��TAϾwAϓuA�t�A��A�ĜA�~�A��A��`A;wA͟�AͅA�jA�K�A��mA̮A�"�A���A�O�A�?}A�M�A��AǾwAǓuA�dZA�ZA�7LA��`AƇ+A�A�A�Aş�A�9XA��Aġ�A�`BA��A��;Aé�AÃA�ffA�E�A��A��A���A�A�;dA��-A�`BA���A��9A�v�A�dZA�hsA�v�A�~�A�^5A���A�=qA�A�`BA��A�n�A�`BA�Q�A�1'A��A�
=A�A���A��TA���A�ZA�&�A��A�  A��;A�ĜA��+A�bNA�A�A�-A��A�1A��TA�A��A�+A��/A��-A���A��A�dZA�K�A�C�A�33A�JA���A���A�z�A�M�A�C�A�{A�ƨA�jA�C�A��A���A��A��A��yA��HA��
A���A���A�ƨA�A��wA��RA��A���A���A���A���A��7A�bNA�1A���A�r�A�O�A�33A�"�A��A�
=A��A��;A���A��A��PA�`BA�VA�VA��A��A��TA�ƨA���A���A��uA�|�A�VA�1'A��A�I�A��A��jA���A��PA��+A��A�x�A�^5A�I�A�1'A�%A��;A��FA�\)A��A��A�dZA��A�|�A�bA�XA���A��A�x�A�5?A��A���A��9A�jA�XA�Q�A�I�A�7LA� �A�{A�%A���A��A��`A��
A���A��DA�l�A�bNA�ZA�E�A�  A��-A���A���A�|�A�5?A�
=A��hA�C�A�A���A��A��A��/A��9A��!A���A���A��uA��\A��+A�~�A�x�A�XA���A���A�^5A�;dA�{A��/A���A�5?A���A�  A�A��PA�x�A�VA�1'A��A�%A��mA��\A��;A��RA��A���A�v�A�5?A���A��`A���A���A���A�ƨA��A���A��+A�v�A�ZA�7LA��A��A��A��A�n�A�XA�/A�$�A��A�1A���A���A��9A���A��hA��uA��hA��DA��7A��7A��+A�z�A�l�A�S�A�C�A�7LA�/A�-A� �A�VA�A��A��RA��uA�z�A��A��FA�M�A�A��TA���A���A�A��FA���A���A��+A�Q�A�oA��yA���A���A���A��A�^5A��TA�|�A�=qA�
=A��;A���A��wA���A���A��DA��A�|�A�x�A�n�A�ffA�bNA�K�A�;dA�$�A��A�%A�  A���A��A��yA��;A���A��9A���A�O�A���A���A�bNA�K�A�VA��#A���A�/A��A��jA���A���A��A�jA�E�A�1A��HA��A�+A���A��jA���A��uA�~�A�G�A� �A���A��FA���A�~�A�Q�A�&�A�{A��HA�n�A�G�A�p�A��A��!A��+A�z�A�x�A�z�A�v�A�p�A�n�A�l�A�l�A�hsA�bNA�`BA�\)A�VA�Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                           1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�$�A�$�A� �A��A��A�(�A�+A�(�A�$�A��A��A�$�A�1'A�33A�7LA�5?A�7LA�7LA�7LA�7LA�9XA�9XA�9XA�=qA�A�A�C�A�E�A�E�A҇+A���A��yAӍPA�t�A�ZA�+A�A���A��TA���AҼjAҧ�AғuA�x�A�ZA�G�A�{A��AѰ!Aѣ�AѓuA�C�A���A�A�A�&�A�l�A���A� �A�$�AÅA��A�~�A���A�$�A�ZA�v�A���A�z�A��wA���A��A�ĜA���A���A���A��yA���A�ĜA��uA�bA�1A��#A���A�-A���A�9XA�M�A���A��hA���A���A��A�-A���A�|�A��mA��A���A��mA�VA���A���A� �A�9XA�=qA�t�A�+A��
A��A���A�JA�TAyC�At�+Aq&�Am;dAi�Ae�AcoA`v�A^A�A\��AY�AX��AW�PATĜAP��AO;dAJA�AH��AEXAB��A@1'A?A=K�A:��A733A4��A1��A/A,��A*�HA(�A'��A%A%%A$�A$Q�A#dZA#XA"�DA ��A33A5?A33A��A��AE�A�AdZA��A�A$�A��A�A�!AI�A^5Ax�AI�A�;AhsA�uA�TAt�A��A�uA/A`BA�7At�A|�A�
A�A��A��A�!AI�A  A+A=qA��AdZA7LA
�`A
��A	��A	|�A	x�A	t�A	O�A�!AZAE�A�;AG�AoA�A�HAE�A��A|�AK�A�Av�A9XA{A1A��A��AjAQ�A �A`BA�A ��A E�@�"�@�V@��@���@��@��#@��u@�I�@�l�@�V@��@�z�@��;@�@��@�+@�O�@�%@�1'@��;@�@�ȴ@�G�@�@� �@� �@�w@��@��@�o@��y@�7@�O�@�u@�b@�v�@���@�K�@ް!@�{@�&�@�A�@�\)@ڏ\@���@�V@ج@�Q�@��;@��@�$�@�p�@�V@ԓu@ӥ�@�~�@��@�J@��#@ѩ�@�p�@�p�@�p�@�G�@ЋD@��m@�C�@���@�ȴ@���@�$�@��@��`@̴9@�bN@���@ˮ@�\)@���@�ff@�=q@��@�&�@�V@���@�1'@�^5@��T@��#@�@�`B@Ĵ9@�1@Ý�@�"�@�@�{@���@�hs@�7L@��9@�r�@�1'@��;@��@��R@���@�7L@���@�bN@�  @��@���@�V@���@�x�@�7L@�Ĝ@��@���@��@�1@�t�@�K�@��y@�n�@�$�@�x�@���@��9@���@�l�@�"�@���@�E�@�J@��-@�G�@�V@��`@�Ĝ@�z�@�Q�@�A�@���@��@�l�@��@���@�~�@�@�?}@��/@���@�Ĝ@�Ĝ@��j@���@��@��@�  @��F@���@��+@�E�@��@��@�%@��j@�A�@���@��@�@���@��+@�-@���@�p�@�hs@�hs@�O�@�7L@��@���@���@��`@��D@��
@�dZ@�@�n�@�@�X@�/@��/@��u@�bN@��;@�dZ@�C�@��@��H@��!@�ff@�J@��@���@�`B@�Ĝ@�Z@���@�ƨ@���@��@�S�@��y@���@�ff@�=q@�$�@�{@��@���@�7L@���@��@�ƨ@�dZ@�o@��+@�J@��@��@���@�Ĝ@���@�j@�1@��
@��P@�|�@�t�@�l�@�C�@�@���@���@�E�@��@���@��^@�`B@�V@�j@��@��
@�|�@�\)@�C�@�33@�
=@��y@�ȴ@���@�^5@��T@�`B@�/@��@���@�j@�ƨ@��P@�S�@�
=@���@���@���@��+@�n�@�V@�M�@�E�@�E�@�5?@�$�@���@�?}@��@�r�@�I�@�9X@� �@��;@��@�t�@�K�@�"�@��H@��!@�5?@��@���@�@���@��@�O�@�7L@���@�bN@�b@�b@�1@���@��
@��w@���@��P@�l�@�\)@�C�@�o@�ȴ@�^5@�=q@�$�@�{@���@���@���@��h@��@�O�@��@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A�&�A�$�A�$�A�(�A�"�A�&�A�&�A��A��A� �A��A��A� �A��A��A� �A��A�&�A�+A�+A�&�A�/A�+A�-A�&�A�(�A�+A�(�A�$�A�$�A�&�A�"�A��A�$�A�oA��A��A� �A��A�"�A�+A��A�$�A�&�A�(�A�/A�/A�/A�33A�1'A�/A�5?A�33A�1'A�5?A�-A�33A�7LA�33A�33A�9XA�33A�7LA�7LA�33A�9XA�7LA�5?A�7LA�33A�5?A�9XA�33A�5?A�9XA�33A�5?A�9XA�5?A�5?A�9XA�7LA�5?A�9XA�5?A�7LA�9XA�5?A�7LA�9XA�5?A�7LA�9XA�5?A�7LA�;dA�7LA�7LA�9XA�5?A�7LA�7LA�33A�9XA�=qA�9XA�7LA�=qA�9XA�7LA�;dA�5?A�;dA�9XA�9XA�=qA�;dA�9XA�;dA�;dA�5?A�7LA�;dA�;dA�9XA�?}A�;dA�9XA�?}A�?}A�9XA�?}A�A�A�=qA�A�A�C�A�?}A�C�A�E�A�A�A�A�A�E�A�C�A�C�A�G�A�E�A�C�A�G�A�G�A�C�A�G�A�G�A�C�A�E�A�I�A�G�A�E�A�I�A�G�A�A�A�C�A�G�A�A�A�?}A�E�A�A�A�A�AҍPA�oA�x�Aӧ�A�ĜA���A�ƨA���A��A���A�  A�
=A�A��`A���AӾwAӝ�AӑhAӍPAӉ7AӅAӉ7AӅA�z�A�x�A�x�A�p�A�hsA�jA�ffA�^5A�`BA�^5A�VA�VA�Q�A�E�A�?}A�9XA�"�A��A��A�VA�%A�A�%A�A���A���A�A���A���A���A���A���A��A��A��A��TA��HA��HA���A���A���A���A���A���A���A���A�ȴA�AҾwAҼjAҰ!AҴ9AҴ9AҮAҧ�Aҥ�Aҥ�Aҥ�Aҟ�Aҝ�Aҝ�Aҕ�Aҏ\Aҏ\AҍPA҉7A҅A҅AҁA�v�A�n�A�jA�bNA�^5A�\)A�^5A�^5A�ZA�Q�A�M�A�O�A�M�A�I�A�G�A�I�A�?}A�5?A�+A�$�A��A�
=A�  A���A��A��yA��HA��/A���AѺ^AѶFAѺ^AѴ9AѰ!AѰ!AѰ!AѬAѥ�Aѥ�Aѧ�Aѥ�Aѡ�Aѡ�Aѣ�Aѡ�Aѝ�Aѝ�Aљ�Aя\AыDA�t�A�XA�G�A�C�A�C�A�=qA�5?A�/A�/A�$�A�{A���A��A���AиRAП�AЇ+A�jA�9XA�"�A�VA���A��TAϾwAϓuA�t�A��A�ĜA�~�A��A��`A;wA͟�AͅA�jA�K�A��mA̮A�"�A���A�O�A�?}A�M�A��AǾwAǓuA�dZA�ZA�7LA��`AƇ+A�A�A�Aş�A�9XA��Aġ�A�`BA��A��;Aé�AÃA�ffA�E�A��A��A���A�A�;dA��-A�`BA���A��9A�v�A�dZA�hsA�v�A�~�A�^5A���A�=qA�A�`BA��A�n�A�`BA�Q�A�1'A��A�
=A�A���A��TA���A�ZA�&�A��A�  A��;A�ĜA��+A�bNA�A�A�-A��A�1A��TA�A��A�+A��/A��-A���A��A�dZA�K�A�C�A�33A�JA���A���A�z�A�M�A�C�A�{A�ƨA�jA�C�A��A���A��A��A��yA��HA��
A���A���A�ƨA�A��wA��RA��A���A���A���A���A��7A�bNA�1A���A�r�A�O�A�33A�"�A��A�
=A��A��;A���A��A��PA�`BA�VA�VA��A��A��TA�ƨA���A���A��uA�|�A�VA�1'A��A�I�A��A��jA���A��PA��+A��A�x�A�^5A�I�A�1'A�%A��;A��FA�\)A��A��A�dZA��A�|�A�bA�XA���A��A�x�A�5?A��A���A��9A�jA�XA�Q�A�I�A�7LA� �A�{A�%A���A��A��`A��
A���A��DA�l�A�bNA�ZA�E�A�  A��-A���A���A�|�A�5?A�
=A��hA�C�A�A���A��A��A��/A��9A��!A���A���A��uA��\A��+A�~�A�x�A�XA���A���A�^5A�;dA�{A��/A���A�5?A���A�  A�A��PA�x�A�VA�1'A��A�%A��mA��\A��;A��RA��A���A�v�A�5?A���A��`A���A���A���A�ƨA��A���A��+A�v�A�ZA�7LA��A��A��A��A�n�A�XA�/A�$�A��A�1A���A���A��9A���A��hA��uA��hA��DA��7A��7A��+A�z�A�l�A�S�A�C�A�7LA�/A�-A� �A�VA�A��A��RA��uA�z�A��A��FA�M�A�A��TA���A���A�A��FA���A���A��+A�Q�A�oA��yA���A���A���A��A�^5A��TA�|�A�=qA�
=A��;A���A��wA���A���A��DA��A�|�A�x�A�n�A�ffA�bNA�K�A�;dA�$�A��A�%A�  A���A��A��yA��;A���A��9A���A�O�A���A���A�bNA�K�A�VA��#A���A�/A��A��jA���A���A��A�jA�E�A�1A��HA��A�+A���A��jA���A��uA�~�A�G�A� �A���A��FA���A�~�A�Q�A�&�A�{A��HA�n�A�G�A�p�A��A��!A��+A�z�A�x�A�z�A�v�A�p�A�n�A�l�A�l�A�hsA�bNA�`BA�\)A�VA�Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                           1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B�ZB��B�ZB��B�%B�%B�%B�ZB�ZB�+B�%B�%B�ZB�ZB�ZB��B��B�ZB�%B��B�ZB�ZB��B�ZB��B��B��B�2B	;�B!�B��B�B� B��B��BŢB�tB�tBɆB˒B˒B�^B�^B�BѷB�,BѷB��B��B�<B��B�RB�[B��B�LB�B��B��B�9B�CB�@B��B�B��B��B�B�BB#�B$B#nB"hB*�B*�B8�B)*B7LB0UB.B"hB�BoB��B�B�B$�B/�B)*B(�BfB�B��B�mB��B�tB��B��B��B}�Bs�B`�BA�B#�BPB
��B
� B
��B
�_B
{�B
sMB
WsB
1�B
�B	��B	�;B	�pB	�hB	�XB	��B	�VB	�B	}�B	s�B	o5B	kB	YKB	S[B	OBB	C�B	<6B	0�B	'RB	VB	OB	!B	"�B	#:B	!�B	%�B	"�B	#�B	 �B	+kB	(XB	"hB	 \B	B	CB	%B	,�B	H�B	8�B	0!B	(�B	1'B	)�B	B	�B	~B	E�B	OBB	w�B	{B	{JB	uZB	v`B	��B	�:B	�\B	��B	��B	�B	� B	��B	�YB	��B	ӏB	��B	ںB	�B	�WB	�QB	�^B	ޞB	�QB	� B	��B	�2B	��B	�DB	��B	�B	��B	��B	�sB	�B	�>B	�>B	�B	�B	�DB	�sB	�B	�B	�B	��B	�QB	��B	��B	� B	�cB	�cB	�/B	��B	�B	�]B	��B	��B	��B	��B	�cB	�5B	�B	��B	�xB	��B	�rB	��B	�JB	��B	��B	�ZB	�|B	�AB	�B	��B	��B	�fB	�%B	��B	��B	�`B	��B	��B	�fB	��B
 4B
B
 4B	�(B	��B	�cB
;B
 �B	�"B	�B	�B	�JB	�rB	�rB	�B	��B	�B	��B	��B	�B	��B	��B	��B	�>B	��B	�B	�B	��B	�B	��B	��B	��B	�]B	��B
�B
MB
{B
�B
�B
�B
uB
�B
�B
GB
GB
B
MB
B
B
�B
�B
YB
SB
�B
�B
�B
B
�B
_B
_B
�B
�B
�B
fB
�B

	B
B
�B
DB
�B
B
VB
�B
.B
�B
�B
oB
�B
oB
�B
�B
�B
�B
�B
MB
B
�B
�B
�B
B
�B
�B
�B
YB
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
_B
�B
eB
kB
�B
1B
1B
~B
B
xB
�B
�B
�B
�B
!B
!B
�B
 �B
!�B
"hB
%B
%FB
$�B
$B
$B
%B
%zB
$�B
%FB
%FB
%zB
%�B
%B
%�B
%�B
%�B
'B
'�B
($B
'RB
'B
'�B
'�B
&�B
'RB
'�B
(�B
(XB
($B
(XB
(�B
)�B
+6B
+�B
,qB
,�B
-CB
-wB
-wB
-wB
.IB
.}B
/�B
1�B
0�B
1'B
1�B
49B
4�B
4�B
5�B
6FB
6FB
7�B
7�B
7�B
7�B
8B
7�B
8�B
8�B
8�B
8�B
9XB
:^B
:�B
;0B
;0B
;�B
;dB
<B
<�B
<�B
=qB
=qB
>B
=�B
>BB
>�B
?}B
@�B
A�B
A�B
B�B
B�B
C�B
DgB
EmB
E�B
E�B
FB
F�B
F�B
G�B
HB
H�B
HKB
HKB
HKB
H�B
H�B
I�B
IRB
I�B
J�B
JXB
JXB
K^B
K^B
M6B
LdB
M6B
M6B
MjB
MjB
MjB
M�B
M�B
M�B
M�B
NpB
OBB
PHB
PHB
O�B
Q�B
Q�B
R�B
R�B
RTB
R B
RTB
RTB
RTB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S�B
TaB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
V9B
V9B
V9B
V9B
VmB
V�B
V�B
W�B
X�B
YB
YKB
YKB
YKB
Y�B
Y�B
ZB
ZQB
Z�B
ZQB
ZQB
Z�B
Z�B
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
\�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�vB��B��B��B�ZB��B��B��B�B�fB�ZB�TB��B�+B�MB��B�+B��B��B��B�TB�B�fB�B��B��B�`B��B��B�B�2B��B�TB��B�ZB��B�DB�|B�B��B��B�%B��B�fB��B��B�%B��B��B��B��B��B�+B�%B�+B��B��B��B��B��B��B��B�TB�`B�TB��B�+B��B�%B�`B��B��B��B��B��B�ZB�TB��B��B�TB�`B�`B�B��B�+B�TB��B�ZB�B�+B��B�TB�`B��B�TB��B��B�B��B��B�B�+B��B�ZB�`B��B�TB��B�`B�B��B�`B�B��B��B�%B�`B��B��B��B��B�%B�`B�+B��B��B�`B�B��B�+B�B�TB�`B��B�ZB��B��B�ZB��B��B��B�`B�`B��B��B��B��B�`B��B�%B��B��B��B��B��B��B��B��B��B��B�`B�8B�8B��B��B�	B��B��B��B�B	�9B
��B
�B�B�B#:B-�BXB~�B��B��B�!B��B��B��B��B��B�dB�B��B�<B�}B�UB�B�B��B�[B� B��B�-B� B��B��B��B��B�3B�gB��B�9B�gB�aB��B�mBŢBĜB�mB��B�EB�9B�B�B�B�9B�zB��BŢB�mB�zB�B�mB�KB�RB��B��B�RBʌBɺB��B�0B͟B̘B�XB��B��B��B��B��B��B�^B��B̘B�0BʌB��B��B�)B�#B��B�dB��BʌB��B��B��B˒B�6B�pB�jB�6B�pB�}B�HB�BB�BB��B�TBѷBԕB՛B՛B��B��BҽB��BӏB��B҉B�TB��B�[B�B�HB�<B�BϫB�BB͟B�B�vB��B�B�6BΥB�B�dB�BΥB͟B��BΥB�BϫB��B��B�dBɺB��B��B˒B�B�B��B��B��B��B��BȀB��B�zB��B� B�B�<B�6B�B�dB�BÖB��B��B��B�qB��B��B��B�IB��B��B�OB��B��B�FB�0B�wB��B��B�@B��B�bB�tB�B�XB�B��B�_B�XB��B��B��B��B�'B�-B�'B��B��B��B��B��B�=B�OB��B�RB�LB��B�LB�B��B��B��B��B��B��B�[B�[B��B�^B��B�B��B��B��B��B�<B�[B�)B�jB�jB�0B�HB��B�BݘB�)B��B�BߤB�B�B��B� B 4B�B�B�B�B1B	�B�B	lB(B�B�BSBYB�BB&�B)�B&�B(�B$�B#B$tB#nB"�B$@B#nB"�B$�B#�B"�B#:B#�B#�B!�B!�B!�B!�B'�B'RB+6B1�B)_B&�B'�B%FB'�B,�B-B/B49B4�B8B<BGzB33B+6B)�B'�B'�B)�B)�B*�B.IB/�B8�BL�B;0B3hB2�B1'B/�B,�B.}B.B.�B.}B/�B*�B,qB.B \B�BEmBB�BoB�B�B�B
�B%B�B�B{B��B�"B��B��B��B��B��B�JB�B�xB�B�B��B��B�B �B�.B{BB�B�B�B%B#:B-CB1�B33B.}B-CB.B0!B0�B.�B,B*�B+�B*eB&�B(�B&�B$�B(�B3�B-B,qB$@B 'B%�B"hB-CB	B6zB(�B
	B�B	lBBBB;B:BB�B��B�8B�%B�	B�vB�5B�B�B�KB�QB�B�B�mB�
B�QB��B��B��B��BܒB�#B�]B��B�gB��B�B�,B��BԕBбB�BB�B�dB�<B��B�^B˒B��B�BB�dB�KBʌB��B��BƨB�9B�mB�?B�#B�UB� B�pB��B�6B�B��B�B�0B��B��B��B�B��B�qB��B��B��B��B��B�kB�CB��B��B�:B�@B��B�YB�YB��B��B�B��BcB~(B~]B~�B|�B� B{B{JBzDBy	Bt�Bt�Bt�Bt�Bs�Bq�BqvBp�BtBn�BrB`�B`B`BYKB^�B[�BN�BOBFtBCaBB�BD�BDgBC�B;0BEmB/�B3�B,�B'�B$tB&�B$tB!�B!�B�B�B7B�B�BBSBBB
	B
��B
�)B
�2B
�B
�B
�B
�>B
��B
�>B
�B
��B
��B
�B
�NB
�HB
�|B
ݘ4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                           4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                           4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021120520445820211205204458IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021121515010820211215150108QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021121515010820211215150108QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365520220126093655IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295820220204232958IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295820220204232958IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295820220204232958IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                