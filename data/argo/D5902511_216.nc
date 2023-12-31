CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  $   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-06-15T23:08:20Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�        =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  V(   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�        \p   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  u�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�        {�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        �`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  ̀   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H %p   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       +�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` D�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   E8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   K8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   Q8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T W8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   W�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   W�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   W�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   W�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � W�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   X,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   XH   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    XP   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        Xp   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        Xx   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       X�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    X�Argo profile    3.1 1.2 19500101000000  20220615230820  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_216                 6810_008521_216                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @���Ew�U@���Ew�U11  @���|���@���|���@0�<��*�@0�<��*��d�X�6��d�X�6�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�\@@  @�G�@�  @��R@޸RA   A  A   A,��A@��A`��A�Q�A�Q�A�  A�  A�  AϮA�\)A�\)B   B  B  B�
B   B(Q�B0(�B8  B@  BH(�BP(�BX  B_�
Bh  BpQ�Bx  B�
B�{B�  B�  B��B�{B�{B�  B��B�  B�  B�  B�{B�  B��B�  B�{B�  B�{B�{B�  B�  B�  B��B�  B�  B��B��B��B�  B�{B�  C   C  C  C�C��C
  C
=C
=C��C�C��C  C  C
=C
=C{C 
=C"  C#��C&  C(  C*  C+��C-��C/�C1��C3��C5�C8  C:
=C<  C=�C?��CA��CD  CF  CH  CJ
=CL
=CN  CP  CQ��CT
=CV  CX
=CZ  C\  C^  C`{Ca��Cc�Ce��Ch  Cj  Cl  Cn  Cp
=Cr
=Ct  Cv
=Cw��Cz
=C{��C~  C�C�  C�  C���C�  C�  C�  C�  C�  C�  C���C�  C�C�  C�  C�
=C�  C�  C�C�C�  C���C�  C�C�  C���C�  C���C���C�  C�  C�  C�  C���C�C�
=C���C���C�  C�
=C�  C�  C�C���C���C�  C�C�C���C���C�C���C�C���C�  C���C�  C���C�  C�C�C�\C�  C�  C�C�  C�  C�  C�  C�  C�C�  C�C�
=C�C�  C���C���C�C�C�  C���C�  C�
=C�  C���C�C�  C�  C�C�
=C�C�C�  C�  C�C�  C���C���C���C���C�  C�C�  C�  C�C�C�  C�C�C�  C���C���C�  C���C���C���C���C�  C�C�
=C�  C�  C�  C���C���C���C���C���D }qD  D��D  D}qD  D�D  D� D  D}qD  D��D�D��D  D��D	D	�D
  D
}qD
��D}qD�D� D  D��D  D��D�D}qD��D� DD�D�qD}qD  D��D�D}qD��D}qD�qD� D�qD� D�qD� D�qD� D�D��D  D� D�qD� DD�DD��D  D� D   D }qD �qD!}qD!�qD"}qD"��D#}qD$  D$� D$�qD%}qD&  D&� D'�D'� D'�qD(}qD)  D)��D*�D*�D+  D+� D+�qD,}qD,�qD-}qD.�D.}qD.�qD/}qD0  D0}qD0�qD1��D2�D2��D3�D3��D4D4� D5  D5}qD6  D6}qD6�qD7� D7�qD8}qD9  D9� D:  D:}qD;  D;� D<  D<}qD<�qD=}qD=��D>}qD>��D?}qD@�D@}qDA  DA�DB�DB��DC�DCz�DD�DD��DE�DE}qDF  DF��DG  DG}qDG�qDH��DIDI��DJ  DJ� DK  DK� DL  DL� DM�DM� DM�qDN� DO�DO��DP�DP� DP��DQ}qDR�DR�DS  DS}qDS�qDT}qDU�DU��DVDV��DV��DW}qDX  DX}qDY  DY��DZDZ��D[�D[}qD[�qD\� D]  D]��D^�D^}qD^��D_}qD`  D`� Da  Da}qDb�Db� Dc  Dc� Dc�qDd� De  De� Df  Df�Dg�Dg}qDh�Dh}qDh��Di}qDj  Dj}qDk  Dk� Dl�Dl��Dm  Dm}qDn  Dn��DoDo��Dp  Dp}qDp�qDq}qDq�qDr��Ds�Ds� Dt�Dt��Du�Du��Dv  Dvz�Dv�RDw� Dx�Dx� Dx�qDy� Dz  Dz}qDz��D{z�D{��D|xRD|��D}z�D~  D~}qD~�qD��D�  D�>�D�}qD���D�  D�@ D�� D��HD�  D�@ D�� D�� D�HD�@ D�~�D���D��D�AHD�� D�� D��qD�>�D�� D���D��qD�@ D��HD�� D�  D�AHD�~�D���D�  D�AHD��HD�� D�HD�@ D�~�D���D�  D�>�D�~�D��HD�HD�AHD�� D��HD�  D�>�D�� D���D�  D�@ D�~�D�� D�HD�AHD�~�D��HD�HD�AHD���D��HD�HD�AHD�� D��HD�HD�>�D���D��HD�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�@ D�� D��HD�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�\)>��
?.{?�\)?�33?���@�\@�R@(��@B�\@Tz�@fff@}p�@���@�@�(�@�ff@�33@��H@��@У�@�(�@�G�@���@�Q�AG�AA(�AG�AA(�A!G�A&ffA,��A2�\A7
=A<(�AC33AHQ�AL(�AQ�AX��A\��AaG�Ag
=Amp�Ar�\Aw
=A|��A���A�33A�{A�G�A��
A�{A�Q�A��
A��RA���A�33A�ffA�G�A�33A�{A���A��A�p�A���A��A�p�A�  A�33A�p�A�\)A\A��A�
=A�G�A���A�\)A�G�A�(�A�
=A�G�A��
A�
=A�G�A�A�ffA�G�A��HA�{A���A��HA��A�Q�A��HA���B (�BG�BffB�
BG�BffB�B	�B
ffB\)B��B=qB\)BQ�B�B\)BQ�Bp�B
=BQ�BG�B�HBz�Bp�B�\B (�B!��B"ffB$  B%p�B&=qB'�B)�B*=qB+
=B,��B.=qB/33B0(�B1B333B4  B5G�B7
=B7�
B9�B:�\B;�B<��B>=qB?�B@��BABC33BD��BEBF�RBH(�BIp�BJ�\BK�BMG�BN{BO�BQ�BQ�BS
=BTz�BU�BV�RBW�
BYG�BZ�\B[�B\��B^{B_�B`Q�Ba��Bc
=BdQ�BeG�BfffBg�
Bi�Bj=qBk33Blz�Bn{Bo33Bp  BqG�Br�RBt  Bt��Bv=qBx  Bx��Bz{B{�B|��B}B33B�ffB��HB�p�B�{B��HB�\)B��B���B�\)B��B�ffB�
=B�B�ffB��HB�p�B�(�B��HB�\)B��B��\B�\)B��
B�Q�B�
=B�B�Q�B��RB�p�B�(�B��RB�G�B��
B��\B�33B�B�=qB��HB���B�{B��\B�\)B�  B���B�
=B���B�ffB�
=B�p�B�  B��RB�p�B�  B��\B���B�B�ffB��HB�\)B�{B���B�p�B��
B�ffB��B�B�Q�B���B��B�(�B���B�33B��B��\B�
=B���B�Q�B���B�\)B�  B���B�G�B�B��\B�33B��B�(�B��HB���B�  B��\B�G�B��B�Q�B��HB��B�=qB��RB�33B�B�z�B��B��B�{B���B�p�B��
B�ffB��B��B�{B£�B�\)B�  B�z�B���BŅB�=qB��HB�G�B��
Bȏ\B��BɅB�=qB���B�p�B��Ḅ�B�G�B��B�Q�B��HBϙ�B�=qBиRB�33B��
Bң�B�33Bә�B�(�B��HBՙ�B�{B֣�BׅB�{B؏\B��B��Bڏ\B�G�B�B�Q�B���B�B�Q�B��HB�\)B�{B��HB�\)B��
B�\B�G�B��B�ffB�
=B�B�z�B��B癚B�=qB�
=B�B�Q�B��HB�B�ffB���B�B�{B��HB�B�{B�z�B�G�B�  B��B�
=B�B�\B��B��B�Q�B�
=B��B�(�B���B��B�=qB���B�\)B�  B���B�\)B��
B�z�B�33B��C 33C �C �HC=qCz�C��C(�C�C�
C{CffC�RC{Cp�C�RC  CQ�C��C  CffC�RC
=CG�C��C  CffC��C�HC	=qC	��C	�HC
�C
\)C
�RC
=C=qCz�C�C�C33Cz�C��C�
C��C=qCp�C�\CC��C�C=qC\)C��C��C��C
=C33Cp�C��CC�HC
=CQ�Cp�C�C�RC��C{C33CQ�C�C�RC�C{C33CQ�Cz�C�RC��C{C33Cp�C�C��C��C�C\)C�\C�RC�
C
=CG�Cp�C�\C�C�C�C=qC\)C�\C��C��C{C=qCp�C��C��C�HC
=CG�Cz�C�\C�RC�HC{CG�C\)Cz�C�C�HC�C33CQ�C��C�CC  C(�CQ�CffC��CC��C�C=qCQ�Cz�C�RC�HC  C�C\)C�\C��C�
C{C=qC\)C�CC�C  CG�CffC�CC��C   C =qC p�C �C ��C �HC!{C!�C!Q�C!�C!�RC!��C!��C"(�C"\)C"p�C"�\C"��C#  C#{C#=qC#\)C#�\C#C#�HC$  C$(�C$ffC$��C$�C$C%  C%33C%Q�C%ffC%��C%�
C%��C&{C&33C&ffC&��C&�RC&��C'
=C'=qC'Q�C'ffC'��C'�
C'�HC(
=C(33C(ffC(��C(�C(�
C(��C)=qC)\)C)p�C)�RC)�HC*  C*�C*\)C*�\C*�C*��C+  C+33C+G�C+z�C+�C+C+�HC,�C,Q�C,p�C,�\C,�C,��C-�C-33C-\)C-��C-�RC-��C-��C.33C.\)C.p�C.�\C.��C/  C/
=C/(�C/Q�C/�\C/�RC/��C/�C0�C0Q�C0z�C0��C0�C0�HC1{C1=qC1Q�C1p�C1��C1�
C2  C2{C2=qC2z�C2��C2C2�
C3  C3=qC3ffC3�\C3��C3C3��C4(�C4\)C4z�C4��C4C5
=C5=qC5Q�C5p�C5�C5�
C6
=C633C6Q�C6z�C6�RC6��C7
=C733C7\)C7��C7��C7��C8{C8G�C8�C8�RC8��C9  C933C9p�C9��C9�C9�HC:�C:Q�C:z�C:��C:C;  C;33C;ffC;�\C;�C;�
C<{C<Q�C<p�C<�\C<��C=  C=(�C=G�C=p�C=�C=�C>{C>33C>ffC>�\C>��C?
=C?G�C?p�C?�\C?C@
=C@=qC@ffC@�C@CA  CA33CAQ�CA�CA��CA��CB�CBQ�CB�\CB��CB��CC�CCQ�CC��CC��CC��CD�CDQ�CD�\CD��CE  CE{CEQ�CE�\CE��CF  CF(�CFQ�CF��CF��CF�CG�CGffCG��CG�RCG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�=q@�\@@  @�G�@�  @��R@޸RA   A  A   A,��A@��A`��A�Q�A�Q�A�  A�  A�  AϮA�\)A�\)B   B  B  B�
B   B(Q�B0(�B8  B@  BH(�BP(�BX  B_�
Bh  BpQ�Bx  B�
B�{B�  B�  B��B�{B�{B�  B��B�  B�  B�  B�{B�  B��B�  B�{B�  B�{B�{B�  B�  B�  B��B�  B�  B��B��B��B�  B�{B�  C   C  C  C�C��C
  C
=C
=C��C�C��C  C  C
=C
=C{C 
=C"  C#��C&  C(  C*  C+��C-��C/�C1��C3��C5�C8  C:
=C<  C=�C?��CA��CD  CF  CH  CJ
=CL
=CN  CP  CQ��CT
=CV  CX
=CZ  C\  C^  C`{Ca��Cc�Ce��Ch  Cj  Cl  Cn  Cp
=Cr
=Ct  Cv
=Cw��Cz
=C{��C~  C�C�  C�  C���C�  C�  C�  C�  C�  C�  C���C�  C�C�  C�  C�
=C�  C�  C�C�C�  C���C�  C�C�  C���C�  C���C���C�  C�  C�  C�  C���C�C�
=C���C���C�  C�
=C�  C�  C�C���C���C�  C�C�C���C���C�C���C�C���C�  C���C�  C���C�  C�C�C�\C�  C�  C�C�  C�  C�  C�  C�  C�C�  C�C�
=C�C�  C���C���C�C�C�  C���C�  C�
=C�  C���C�C�  C�  C�C�
=C�C�C�  C�  C�C�  C���C���C���C���C�  C�C�  C�  C�C�C�  C�C�C�  C���C���C�  C���C���C���C���C�  C�C�
=C�  C�  C�  C���C���C���C���C���D }qD  D��D  D}qD  D�D  D� D  D}qD  D��D�D��D  D��D	D	�D
  D
}qD
��D}qD�D� D  D��D  D��D�D}qD��D� DD�D�qD}qD  D��D�D}qD��D}qD�qD� D�qD� D�qD� D�qD� D�D��D  D� D�qD� DD�DD��D  D� D   D }qD �qD!}qD!�qD"}qD"��D#}qD$  D$� D$�qD%}qD&  D&� D'�D'� D'�qD(}qD)  D)��D*�D*�D+  D+� D+�qD,}qD,�qD-}qD.�D.}qD.�qD/}qD0  D0}qD0�qD1��D2�D2��D3�D3��D4D4� D5  D5}qD6  D6}qD6�qD7� D7�qD8}qD9  D9� D:  D:}qD;  D;� D<  D<}qD<�qD=}qD=��D>}qD>��D?}qD@�D@}qDA  DA�DB�DB��DC�DCz�DD�DD��DE�DE}qDF  DF��DG  DG}qDG�qDH��DIDI��DJ  DJ� DK  DK� DL  DL� DM�DM� DM�qDN� DO�DO��DP�DP� DP��DQ}qDR�DR�DS  DS}qDS�qDT}qDU�DU��DVDV��DV��DW}qDX  DX}qDY  DY��DZDZ��D[�D[}qD[�qD\� D]  D]��D^�D^}qD^��D_}qD`  D`� Da  Da}qDb�Db� Dc  Dc� Dc�qDd� De  De� Df  Df�Dg�Dg}qDh�Dh}qDh��Di}qDj  Dj}qDk  Dk� Dl�Dl��Dm  Dm}qDn  Dn��DoDo��Dp  Dp}qDp�qDq}qDq�qDr��Ds�Ds� Dt�Dt��Du�Du��Dv  Dvz�Dv�RDw� Dx�Dx� Dx�qDy� Dz  Dz}qDz��D{z�D{��D|xRD|��D}z�D~  D~}qD~�qD��D�  D�>�D�}qD���D�  D�@ D�� D��HD�  D�@ D�� D�� D�HD�@ D�~�D���D��D�AHD�� D�� D��qD�>�D�� D���D��qD�@ D��HD�� D�  D�AHD�~�D���D�  D�AHD��HD�� D�HD�@ D�~�D���D�  D�>�D�~�D��HD�HD�AHD�� D��HD�  D�>�D�� D���D�  D�@ D�~�D�� D�HD�AHD�~�D��HD�HD�AHD���D��HD�HD�AHD�� D��HD�HD�>�D���D��HD�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�@ D�� D��HD�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�\)>��
?.{?�\)?�33?���@�\@�R@(��@B�\@Tz�@fff@}p�@���@�@�(�@�ff@�33@��H@��@У�@�(�@�G�@���@�Q�AG�AA(�AG�AA(�A!G�A&ffA,��A2�\A7
=A<(�AC33AHQ�AL(�AQ�AX��A\��AaG�Ag
=Amp�Ar�\Aw
=A|��A���A�33A�{A�G�A��
A�{A�Q�A��
A��RA���A�33A�ffA�G�A�33A�{A���A��A�p�A���A��A�p�A�  A�33A�p�A�\)A\A��A�
=A�G�A���A�\)A�G�A�(�A�
=A�G�A��
A�
=A�G�A�A�ffA�G�A��HA�{A���A��HA��A�Q�A��HA���B (�BG�BffB�
BG�BffB�B	�B
ffB\)B��B=qB\)BQ�B�B\)BQ�Bp�B
=BQ�BG�B�HBz�Bp�B�\B (�B!��B"ffB$  B%p�B&=qB'�B)�B*=qB+
=B,��B.=qB/33B0(�B1B333B4  B5G�B7
=B7�
B9�B:�\B;�B<��B>=qB?�B@��BABC33BD��BEBF�RBH(�BIp�BJ�\BK�BMG�BN{BO�BQ�BQ�BS
=BTz�BU�BV�RBW�
BYG�BZ�\B[�B\��B^{B_�B`Q�Ba��Bc
=BdQ�BeG�BfffBg�
Bi�Bj=qBk33Blz�Bn{Bo33Bp  BqG�Br�RBt  Bt��Bv=qBx  Bx��Bz{B{�B|��B}B33B�ffB��HB�p�B�{B��HB�\)B��B���B�\)B��B�ffB�
=B�B�ffB��HB�p�B�(�B��HB�\)B��B��\B�\)B��
B�Q�B�
=B�B�Q�B��RB�p�B�(�B��RB�G�B��
B��\B�33B�B�=qB��HB���B�{B��\B�\)B�  B���B�
=B���B�ffB�
=B�p�B�  B��RB�p�B�  B��\B���B�B�ffB��HB�\)B�{B���B�p�B��
B�ffB��B�B�Q�B���B��B�(�B���B�33B��B��\B�
=B���B�Q�B���B�\)B�  B���B�G�B�B��\B�33B��B�(�B��HB���B�  B��\B�G�B��B�Q�B��HB��B�=qB��RB�33B�B�z�B��B��B�{B���B�p�B��
B�ffB��B��B�{B£�B�\)B�  B�z�B���BŅB�=qB��HB�G�B��
Bȏ\B��BɅB�=qB���B�p�B��Ḅ�B�G�B��B�Q�B��HBϙ�B�=qBиRB�33B��
Bң�B�33Bә�B�(�B��HBՙ�B�{B֣�BׅB�{B؏\B��B��Bڏ\B�G�B�B�Q�B���B�B�Q�B��HB�\)B�{B��HB�\)B��
B�\B�G�B��B�ffB�
=B�B�z�B��B癚B�=qB�
=B�B�Q�B��HB�B�ffB���B�B�{B��HB�B�{B�z�B�G�B�  B��B�
=B�B�\B��B��B�Q�B�
=B��B�(�B���B��B�=qB���B�\)B�  B���B�\)B��
B�z�B�33B��C 33C �C �HC=qCz�C��C(�C�C�
C{CffC�RC{Cp�C�RC  CQ�C��C  CffC�RC
=CG�C��C  CffC��C�HC	=qC	��C	�HC
�C
\)C
�RC
=C=qCz�C�C�C33Cz�C��C�
C��C=qCp�C�\CC��C�C=qC\)C��C��C��C
=C33Cp�C��CC�HC
=CQ�Cp�C�C�RC��C{C33CQ�C�C�RC�C{C33CQ�Cz�C�RC��C{C33Cp�C�C��C��C�C\)C�\C�RC�
C
=CG�Cp�C�\C�C�C�C=qC\)C�\C��C��C{C=qCp�C��C��C�HC
=CG�Cz�C�\C�RC�HC{CG�C\)Cz�C�C�HC�C33CQ�C��C�CC  C(�CQ�CffC��CC��C�C=qCQ�Cz�C�RC�HC  C�C\)C�\C��C�
C{C=qC\)C�CC�C  CG�CffC�CC��C   C =qC p�C �C ��C �HC!{C!�C!Q�C!�C!�RC!��C!��C"(�C"\)C"p�C"�\C"��C#  C#{C#=qC#\)C#�\C#C#�HC$  C$(�C$ffC$��C$�C$C%  C%33C%Q�C%ffC%��C%�
C%��C&{C&33C&ffC&��C&�RC&��C'
=C'=qC'Q�C'ffC'��C'�
C'�HC(
=C(33C(ffC(��C(�C(�
C(��C)=qC)\)C)p�C)�RC)�HC*  C*�C*\)C*�\C*�C*��C+  C+33C+G�C+z�C+�C+C+�HC,�C,Q�C,p�C,�\C,�C,��C-�C-33C-\)C-��C-�RC-��C-��C.33C.\)C.p�C.�\C.��C/  C/
=C/(�C/Q�C/�\C/�RC/��C/�C0�C0Q�C0z�C0��C0�C0�HC1{C1=qC1Q�C1p�C1��C1�
C2  C2{C2=qC2z�C2��C2C2�
C3  C3=qC3ffC3�\C3��C3C3��C4(�C4\)C4z�C4��C4C5
=C5=qC5Q�C5p�C5�C5�
C6
=C633C6Q�C6z�C6�RC6��C7
=C733C7\)C7��C7��C7��C8{C8G�C8�C8�RC8��C9  C933C9p�C9��C9�C9�HC:�C:Q�C:z�C:��C:C;  C;33C;ffC;�\C;�C;�
C<{C<Q�C<p�C<�\C<��C=  C=(�C=G�C=p�C=�C=�C>{C>33C>ffC>�\C>��C?
=C?G�C?p�C?�\C?C@
=C@=qC@ffC@�C@CA  CA33CAQ�CA�CA��CA��CB�CBQ�CB�\CB��CB��CC�CCQ�CC��CC��CC��CD�CDQ�CD�\CD��CE  CE{CEQ�CE�\CE��CF  CF(�CFQ�CF��CF��CF�CG�CGffCG��CG�RCG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A��yA��#A��#A��#A��#A��#A��/A��/A��/A��#A��A��A��#A��A��#A��A��#A��#A��#A��#A��#A��#A��/A��;A��/A��/A��;A��;A��/A��;A��;A��HA��#A���A��
AԼjA�5?AҍPA�A��mA�ƨA�`BA��A��
AиRAа!AП�A�z�A�\)A�9XA�Aϰ!A�O�A���A�VA͸RA�ȴA���A��TA�`BA���A�l�AǇ+A��;AƁA�(�Aŕ�Ać+AüjA�VA� �A��
A�n�A���A�(�A���A�9XA��A��A�t�A��jA�ZA�A�v�A��RA�v�A��;A��A�A��A���A��A��FA�I�A��A�v�A���A��TA��+A�ZA��^A�`BA���A�v�A�?}A�z�A��!A��^A�A�A��`A��PA���A�VA���A�33A��PA�oA�r�A���A���A�AdZA~�/A}��Azn�Axz�AvȴAu�;Au/AtbNAsƨAq�7Ao;dAmS�Ah��Ac�A^{A\�jAZE�AV�!AUl�AOK�AL(�AG��AD(�A@��A>�A<JA:z�A7A2��A0jA.��A-�wA-+A,��A,~�A,�A+|�A+&�A*�yA*�uA)�FA(�A(Q�A'hsA'A&��A&ffA%?}A#�PA#33A!�^A!oA �A��A�RA{AhsA7LA=qA��A�-A(�AhsAVAn�A9XAQ�A�FAA�AbNA$�A��A/A��A�DAE�A�uAXA�/AVAA�A�A�TA��A�HA��A?}A��A�AE�A�mA&�A
A	|�A	A�jA�A;dA�An�A��A�wA�7A\)A��AffA��A�A33A�Av�A$�A�wA��At�A?}A v�@��
@�S�@���@��R@���@�V@�7L@��;@�=q@�J@�7L@��
@�K�@�"�@���@���@�V@�r�@�1@��H@�-@�x�@�z�@�"�@�^5@���@�`B@�G�@��@�V@�D@��@�\@�E�@��@��#@�hs@�w@��;@�l�@�+@�M�@�`B@�  @�n�@��@�-@�j@�ȴ@��@���@��@��H@��@�/@�1@�dZ@�ȴ@�n�@���@�G�@�&�@���@ԃ@�C�@�$�@�x�@�7L@�/@�V@���@У�@Ѓ@�I�@��
@�
=@Ώ\@�ff@͙�@��`@��m@˥�@�l�@�;d@�=q@ɡ�@�Q�@ǍP@ư!@�X@�?}@�&�@��@Ĵ9@�z�@��
@�|�@�|�@�|�@���@�^5@��-@��`@�Z@�9X@�1'@��@��@��
@�33@���@�@��h@��@�hs@�Ĝ@���@��@�M�@�-@���@���@��@�r�@�bN@�Q�@�9X@���@�$�@���@�x�@��@�Ĝ@��@�I�@��
@��H@���@��R@��!@���@���@���@��@�G�@�7L@�V@���@���@�j@���@��@�C�@���@�v�@�p�@�V@��@��u@�j@�  @��
@�;d@���@�ff@��@�@���@�X@��@��`@��D@�bN@�9X@��@���@��@��
@�|�@���@���@��#@��@�O�@�%@���@��u@�1'@���@�t�@�K�@�o@��R@�n�@�$�@��@�?}@��@�r�@���@��@�33@��@��+@�5?@��@��-@�hs@�?}@�&�@�%@���@��@�j@�I�@�9X@���@�o@��\@�^5@��@��#@��-@��@��`@�bN@��@�C�@��@���@�-@���@�@���@�V@��j@��D@�A�@��m@���@�t�@���@��@��\@�ff@�ff@�^5@�M�@��T@�`B@���@���@���@���@���@��D@��u@�z�@� �@��;@��P@�S�@�"�@��@���@��R@�ff@�J@��T@���@��@��/@��u@�Q�@�1@���@���@�dZ@�o@��!@�5?@��@�@��-@�x�@��@��@���@��D@�bN@�1'@�b@���@��w@�t�@�33@��@�ȴ@�~�@�5?@��7@�x�@�x�@�hs@�?}@�&�@��@���@��/@�I�@�  @��
@���@�;d@�@���@�=q@���@��^@�x�@�`B@�X@�?}@�?}@���@��@�I�@�1'@�1@\)@~��@~�@~��@~ff@~5?@}�T@}O�@}V@|z�@|Z@|9X@|1@{S�@z�!@z^5@z�@yx�@zJ@y�#@y��@y7L@x��@x1'@x  @w�@w�@v��@vȴ@v�R@v��@v��@vff@vE�@v$�@u@uV@t�/@t9X@st�@s@rn�@q��@q��@q�@pr�@o�;@o��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA��A��A��A��HA��A��#A��A���A��;A��/A��
A��#A��;A��/A��A��#A��/A��A��A��;A��;A��#A��/A��;A��/A��#A��;A��;A��#A��/A��#A��
A��#A��#A��
A��
A��#A��#A��
A��A��/A��A��
A��#A��/A��#A��A��#A��/A��
A��A��/A��/A��#A��
A��#A��/A��A��
A��#A��#A��
A��A��/A��A��
A��#A��/A��A��#A��/A��/A��
A��/A��/A��A��A��#A��/A��
A��/A��;A��#A��#A��;A��#A��A��#A��#A��A��/A��/A��A��A��/A��#A��A��;A��#A��A��#A��;A��#A��A��/A��;A��#A��/A��HA��;A��#A��;A��;A��/A��#A��;A��;A��#A��/A��;A��/A��#A��;A��HA��#A��;A��HA��/A��#A��HA��;A��/A��;A��TA��;A��/A��;A��HA��/A��/A��;A��/A��/A��HA��/A��#A��;A��TA��HA��/A��;A��HA��;A��/A��HA��HA��/A��;A��`A��/A��;A��`A��HA��;A��TA��;A��#A��A��A��
A���A���A���A��
A���A���A��
A��
A���A���A��A��/A��A���A���A���AԾwAԶFAԴ9AԴ9AԲ-Aԣ�AԁA�I�A��A���AӓuA�K�AҮA�|�A�bNA�9XA�$�A��A�{A�JA���A���A���A��A��A��yA��yA��A��`A��/A��#A���A���A���A���A���AѲ-Aѣ�Aѝ�AхA�VA�33A���A���A���A��A��A���A���A��A��A��A��A��#A�Aк^AмjAк^AиRAжFAк^Aк^AжFAд9AжFAд9Aа!AЬAЩ�Aа!AЮAЧ�AС�AН�AН�AЙ�AЉ7A�~�AЁAЁA�~�A�x�A�p�A�hsA�bNA�`BA�^5A�^5A�S�A�O�A�O�A�E�A�;dA�7LA�5?A�-A�$�A�"�A��A�%A���A���A��yA���A�ƨA���AϸRAϩ�Aϛ�Aω7A�z�A�l�A�\)A�M�A�G�A�A�A�1'A��A�A��A���AζFAΧ�AΛ�A�v�A�G�A� �A���A��A��`A��;A��/A��#A�ȴA;wAʹ9A͛�A͏\A�z�A�;dA���A̟�ȂhA�~�A�t�A�ffA�XA�M�A�VA˛�A�v�A�hsA�A�A�A��A��HAʼjAʋDA��AɸRA�x�A�VA�&�A���A��A��;A���AȼjAȰ!AȮAȩ�Aț�Aȕ�Aȉ7A�r�A�\)A�7LA�{A��;Aǰ!AǋDA�r�A�C�A��A�JA���A��HA�ƨAƺ^AƲ-Aƣ�AƋDA�~�A�v�A�p�A�hsA�`BA�M�A�7LA��A�
=A�A���A��HA�AŲ-Aš�A�jA�A��
AĲ-Aĕ�A�~�A�p�A�G�A�VA��;A���Að!Aß�AÙ�AÅA�r�A�ZA�(�A��A���A�^5A�+A�ȴA�bA��RA���A�Q�A�&�A�A��HA���A��jA���A�r�A�oA��7A�7LA��A���A��wA��-A��A���A��+A�r�A�`BA�\)A�XA�1'A�bA�1A���A��yA��/A�ȴA��!A���A���A��hA��A�p�A�bNA�\)A�XA�M�A�C�A�=qA�7LA�/A�"�A��A��A��A�VA�  A���A���A��yA���A��-A��uA�^5A���A�I�A�{A��A��FA�n�A�1'A�"�A��A�A��TA�ƨA��RA���A��A�XA��A��jA�x�A�\)A�O�A�E�A�5?A��A�  A��A��`A��/A���A���A�A���A��jA��9A���A���A���A��PA�|�A�t�A�p�A�hsA�`BA�VA�Q�A�O�A�M�A�E�A�A�A�=qA�;dA�5?A�&�A� �A��A��A�bA���A��mA��`A��A���A�ȴA�ȴA�A��-A���A���A��PA�n�A�\)A�G�A�;dA�5?A�-A��A��
A�7LA�?}A��/A���A�v�A�?}A�{A��A���A���A��A���A���A��+A��A�|�A�t�A�p�A�l�A�dZA�ZA�S�A�?}A�-A� �A��A�bA��A��/A���A��9A���A���A��PA��7A�x�A�jA�`BA�`BA�\)A�A�A��A���A��mA��A��^A���A���A�t�A�dZA�E�A�;dA�5?A�(�A��A���A���A��\A�`BA�M�A�K�A�G�A�9XA�+A�$�A��A�
=A���A��/A�ĜA���A��\A�|�A�jA�Q�A�33A�A���A��A��+A�n�A�VA�K�A�A�A�7LA�5?A�7LA�1'A�33A�1'A�-A�&�A�"�A�VA�A�  A�  A�A�  A���A���A���A��A��mA��/A���A��A��A�K�A��A��A���A��!A��PA�v�A�jA�XA�A�A�33A�&�A��A�%A���A���A��hA�~�A�O�A�=qA��A���A���A�n�A�7LA�1A���A��TA��
A���A���A�ȴA���A��9A���A���A��PA�x�A�=qA��#A��!A�^5A��A��/A���A���A��A��7A�jA�XA�K�A�A�A�7LA�(�A� �A��A�{A�bA�  A��A��TA���A���A�l�A�/A���A�ĜA���A�x�A�t�A�n�A�l�A�hsA�`BA�\)A�^5A�`BA�`BA�^5A�\)A�\)A�^5A�^5A�XA�S�A�O�A�E�A�7LA�"�A�oA�%A�A���A��`A���A�A�O�A��;A��!A��7A�jA�ZA�S�A�I�A�=qA�+A��A�{A�A���A��A��`A���A��FA��7A�9XA��#A��A��A�K�A���A���A�ZA�-A�  A���A��FA���A��A�x�A�r�A�l�A�S�A�5?A�oA�A���A��HA�ĜA��9A���A���A��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��yA��#A��#A��#A��#A��#A��/A��/A��/A��#A��A��A��#A��A��#A��A��#A��#A��#A��#A��#A��#A��/A��;A��/A��/A��;A��;A��/A��;A��;A��HA��#A���A��
AԼjA�5?AҍPA�A��mA�ƨA�`BA��A��
AиRAа!AП�A�z�A�\)A�9XA�Aϰ!A�O�A���A�VA͸RA�ȴA���A��TA�`BA���A�l�AǇ+A��;AƁA�(�Aŕ�Ać+AüjA�VA� �A��
A�n�A���A�(�A���A�9XA��A��A�t�A��jA�ZA�A�v�A��RA�v�A��;A��A�A��A���A��A��FA�I�A��A�v�A���A��TA��+A�ZA��^A�`BA���A�v�A�?}A�z�A��!A��^A�A�A��`A��PA���A�VA���A�33A��PA�oA�r�A���A���A�AdZA~�/A}��Azn�Axz�AvȴAu�;Au/AtbNAsƨAq�7Ao;dAmS�Ah��Ac�A^{A\�jAZE�AV�!AUl�AOK�AL(�AG��AD(�A@��A>�A<JA:z�A7A2��A0jA.��A-�wA-+A,��A,~�A,�A+|�A+&�A*�yA*�uA)�FA(�A(Q�A'hsA'A&��A&ffA%?}A#�PA#33A!�^A!oA �A��A�RA{AhsA7LA=qA��A�-A(�AhsAVAn�A9XAQ�A�FAA�AbNA$�A��A/A��A�DAE�A�uAXA�/AVAA�A�A�TA��A�HA��A?}A��A�AE�A�mA&�A
A	|�A	A�jA�A;dA�An�A��A�wA�7A\)A��AffA��A�A33A�Av�A$�A�wA��At�A?}A v�@��
@�S�@���@��R@���@�V@�7L@��;@�=q@�J@�7L@��
@�K�@�"�@���@���@�V@�r�@�1@��H@�-@�x�@�z�@�"�@�^5@���@�`B@�G�@��@�V@�D@��@�\@�E�@��@��#@�hs@�w@��;@�l�@�+@�M�@�`B@�  @�n�@��@�-@�j@�ȴ@��@���@��@��H@��@�/@�1@�dZ@�ȴ@�n�@���@�G�@�&�@���@ԃ@�C�@�$�@�x�@�7L@�/@�V@���@У�@Ѓ@�I�@��
@�
=@Ώ\@�ff@͙�@��`@��m@˥�@�l�@�;d@�=q@ɡ�@�Q�@ǍP@ư!@�X@�?}@�&�@��@Ĵ9@�z�@��
@�|�@�|�@�|�@���@�^5@��-@��`@�Z@�9X@�1'@��@��@��
@�33@���@�@��h@��@�hs@�Ĝ@���@��@�M�@�-@���@���@��@�r�@�bN@�Q�@�9X@���@�$�@���@�x�@��@�Ĝ@��@�I�@��
@��H@���@��R@��!@���@���@���@��@�G�@�7L@�V@���@���@�j@���@��@�C�@���@�v�@�p�@�V@��@��u@�j@�  @��
@�;d@���@�ff@��@�@���@�X@��@��`@��D@�bN@�9X@��@���@��@��
@�|�@���@���@��#@��@�O�@�%@���@��u@�1'@���@�t�@�K�@�o@��R@�n�@�$�@��@�?}@��@�r�@���@��@�33@��@��+@�5?@��@��-@�hs@�?}@�&�@�%@���@��@�j@�I�@�9X@���@�o@��\@�^5@��@��#@��-@��@��`@�bN@��@�C�@��@���@�-@���@�@���@�V@��j@��D@�A�@��m@���@�t�@���@��@��\@�ff@�ff@�^5@�M�@��T@�`B@���@���@���@���@���@��D@��u@�z�@� �@��;@��P@�S�@�"�@��@���@��R@�ff@�J@��T@���@��@��/@��u@�Q�@�1@���@���@�dZ@�o@��!@�5?@��@�@��-@�x�@��@��@���@��D@�bN@�1'@�b@���@��w@�t�@�33@��@�ȴ@�~�@�5?@��7@�x�@�x�@�hs@�?}@�&�@��@���@��/@�I�@�  @��
@���@�;d@�@���@�=q@���@��^@�x�@�`B@�X@�?}@�?}@���@��@�I�@�1'@�1@\)@~��@~�@~��@~ff@~5?@}�T@}O�@}V@|z�@|Z@|9X@|1@{S�@z�!@z^5@z�@yx�@zJ@y�#@y��@y7L@x��@x1'@x  @w�@w�@v��@vȴ@v�R@v��@v��@vff@vE�@v$�@u@uV@t�/@t9X@st�@s@rn�@q��@q��@q�@pr�@o�;G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA��A��A��A��HA��A��#A��A���A��;A��/A��
A��#A��;A��/A��A��#A��/A��A��A��;A��;A��#A��/A��;A��/A��#A��;A��;A��#A��/A��#A��
A��#A��#A��
A��
A��#A��#A��
A��A��/A��A��
A��#A��/A��#A��A��#A��/A��
A��A��/A��/A��#A��
A��#A��/A��A��
A��#A��#A��
A��A��/A��A��
A��#A��/A��A��#A��/A��/A��
A��/A��/A��A��A��#A��/A��
A��/A��;A��#A��#A��;A��#A��A��#A��#A��A��/A��/A��A��A��/A��#A��A��;A��#A��A��#A��;A��#A��A��/A��;A��#A��/A��HA��;A��#A��;A��;A��/A��#A��;A��;A��#A��/A��;A��/A��#A��;A��HA��#A��;A��HA��/A��#A��HA��;A��/A��;A��TA��;A��/A��;A��HA��/A��/A��;A��/A��/A��HA��/A��#A��;A��TA��HA��/A��;A��HA��;A��/A��HA��HA��/A��;A��`A��/A��;A��`A��HA��;A��TA��;A��#A��A��A��
A���A���A���A��
A���A���A��
A��
A���A���A��A��/A��A���A���A���AԾwAԶFAԴ9AԴ9AԲ-Aԣ�AԁA�I�A��A���AӓuA�K�AҮA�|�A�bNA�9XA�$�A��A�{A�JA���A���A���A��A��A��yA��yA��A��`A��/A��#A���A���A���A���A���AѲ-Aѣ�Aѝ�AхA�VA�33A���A���A���A��A��A���A���A��A��A��A��A��#A�Aк^AмjAк^AиRAжFAк^Aк^AжFAд9AжFAд9Aа!AЬAЩ�Aа!AЮAЧ�AС�AН�AН�AЙ�AЉ7A�~�AЁAЁA�~�A�x�A�p�A�hsA�bNA�`BA�^5A�^5A�S�A�O�A�O�A�E�A�;dA�7LA�5?A�-A�$�A�"�A��A�%A���A���A��yA���A�ƨA���AϸRAϩ�Aϛ�Aω7A�z�A�l�A�\)A�M�A�G�A�A�A�1'A��A�A��A���AζFAΧ�AΛ�A�v�A�G�A� �A���A��A��`A��;A��/A��#A�ȴA;wAʹ9A͛�A͏\A�z�A�;dA���A̟�ȂhA�~�A�t�A�ffA�XA�M�A�VA˛�A�v�A�hsA�A�A�A��A��HAʼjAʋDA��AɸRA�x�A�VA�&�A���A��A��;A���AȼjAȰ!AȮAȩ�Aț�Aȕ�Aȉ7A�r�A�\)A�7LA�{A��;Aǰ!AǋDA�r�A�C�A��A�JA���A��HA�ƨAƺ^AƲ-Aƣ�AƋDA�~�A�v�A�p�A�hsA�`BA�M�A�7LA��A�
=A�A���A��HA�AŲ-Aš�A�jA�A��
AĲ-Aĕ�A�~�A�p�A�G�A�VA��;A���Að!Aß�AÙ�AÅA�r�A�ZA�(�A��A���A�^5A�+A�ȴA�bA��RA���A�Q�A�&�A�A��HA���A��jA���A�r�A�oA��7A�7LA��A���A��wA��-A��A���A��+A�r�A�`BA�\)A�XA�1'A�bA�1A���A��yA��/A�ȴA��!A���A���A��hA��A�p�A�bNA�\)A�XA�M�A�C�A�=qA�7LA�/A�"�A��A��A��A�VA�  A���A���A��yA���A��-A��uA�^5A���A�I�A�{A��A��FA�n�A�1'A�"�A��A�A��TA�ƨA��RA���A��A�XA��A��jA�x�A�\)A�O�A�E�A�5?A��A�  A��A��`A��/A���A���A�A���A��jA��9A���A���A���A��PA�|�A�t�A�p�A�hsA�`BA�VA�Q�A�O�A�M�A�E�A�A�A�=qA�;dA�5?A�&�A� �A��A��A�bA���A��mA��`A��A���A�ȴA�ȴA�A��-A���A���A��PA�n�A�\)A�G�A�;dA�5?A�-A��A��
A�7LA�?}A��/A���A�v�A�?}A�{A��A���A���A��A���A���A��+A��A�|�A�t�A�p�A�l�A�dZA�ZA�S�A�?}A�-A� �A��A�bA��A��/A���A��9A���A���A��PA��7A�x�A�jA�`BA�`BA�\)A�A�A��A���A��mA��A��^A���A���A�t�A�dZA�E�A�;dA�5?A�(�A��A���A���A��\A�`BA�M�A�K�A�G�A�9XA�+A�$�A��A�
=A���A��/A�ĜA���A��\A�|�A�jA�Q�A�33A�A���A��A��+A�n�A�VA�K�A�A�A�7LA�5?A�7LA�1'A�33A�1'A�-A�&�A�"�A�VA�A�  A�  A�A�  A���A���A���A��A��mA��/A���A��A��A�K�A��A��A���A��!A��PA�v�A�jA�XA�A�A�33A�&�A��A�%A���A���A��hA�~�A�O�A�=qA��A���A���A�n�A�7LA�1A���A��TA��
A���A���A�ȴA���A��9A���A���A��PA�x�A�=qA��#A��!A�^5A��A��/A���A���A��A��7A�jA�XA�K�A�A�A�7LA�(�A� �A��A�{A�bA�  A��A��TA���A���A�l�A�/A���A�ĜA���A�x�A�t�A�n�A�l�A�hsA�`BA�\)A�^5A�`BA�`BA�^5A�\)A�\)A�^5A�^5A�XA�S�A�O�A�E�A�7LA�"�A�oA�%A�A���A��`A���A�A�O�A��;A��!A��7A�jA�ZA�S�A�I�A�=qA�+A��A�{A�A���A��A��`A���A��FA��7A�9XA��#A��A��A�K�A���A���A�ZA�-A�  A���A��FA���A��A�x�A�r�A�l�A�S�A�5?A�oA�A���A��HA�ĜA��9A���A���A��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B�IB�wB�wB�wB�wB�CB�CB�B�CB�wB�B�wB�CB�wB�B��B�wB�CB��B��B�wB�wB�wB�CB��B��B�wB�CB�wB��B�wB�CB�B�qB��B��B��B�OB�{B��B�B�MB��B�.B��B�\B�\B�\B��B��B��B�4B��B�B�1B��B��B�@B�hB��B�zB�*B�}B��B�B�5B��B�B�BYB+�B.�B;�B:�B=�B=�B;�B?}BJ�BP}BN�BN�BM�BJ�BX�BS&BS�BS�BT�BS�BW
BS[BT�BR�BL0BM�BJ�BE9B>BB9$B6zB�BbB��B�NB՛B�?B�RB��B��B�=Bj�BV�B9�B�B
�
B
�?B
��B
�bB
g�B
[�B
TaB
P�B
H�B
9�B
)�B
CB
�B
VB
�B
 4B	�+B	��B	�2B	�zB	�$B	q�B	e`B	Z�B	E9B	;0B	33B	'RB	�B	B	�B	�B		�B	�B	�B��B�%B�+B�B�.B	YB	JB	�B	+6B	1[B	3�B	6B	6�B	7LB	7�B	6FB	6zB	8�B	8B	=<B	6zB	8�B	4�B	7�B	:*B	?B	<jB	?HB	;0B	9$B	;dB	7LB	B�B	I�B	N<B	S�B	ZQB	[�B	e,B	a�B	m�B	s�B	t�B	s�B	r|B	s�B	sMB	sMB	v�B	qB	y	B	y�B	y�B	y	B	y	B	xlB	zB	{�B	��B	��B	��B	�lB	��B	�lB	�B	�_B	��B	��B	�hB	��B	�kB	��B	�!B	��B	��B	��B	�aB	��B	��B	��B	�tB	�B	�B	�FB	�B	��B	�LB	��B	�RB	��B	��B	�6B	�jB	�6B	�dB	�0B	�qB	�B	� B	�}B	�UB	�aB	��B	B	�aB	ÖB	�3B	ÖB	�aB	B	ÖB	�B	�B	��B	�KB	ɺB	ɆB	ɆB	�B	��B	�XB	�)B	�dB	�HB	�BB	�<B	�pB	�RB	��B	͟B	�<B	͟B	��B	уB	��B	ϫB	�B	бB	�B	��B	��B	�gB	�2B	�B	�B	��B	רB	��B	�9B	�?B	רB	�?B	�sB	�
B	چB	�#B	��B	ܒB	�]B	��B	�dB	ݘB	�/B	�B	ޞB	�B	�B	�5B	�B	�HB	��B	�B	�B	�pB	ޞB	ޞB	�dB	�WB	�B	�KB	خB	��B	�KB	یB	ޞB	��B	�B	�HB	�|B	�B	�B	�;B	�B	�B	�B	�B	�|B	�B	�B	��B	�]B	� B	��B	�]B	��B	�]B	�cB	�B	�B	��B	��B	�ZB	��B	��B	�2B	��B	��B	��B	��B	��B	��B	��B	�VB	��B	��B	��B	��B	�PB	��B	��B	��B	��B	��B
B
uB
�B
GB
{B
�B
�B
B
B
�B
�B
�B
�B
1B
fB
	7B
	B
B

�B
�B
�B
�B
"B
VB
�B
\B
�B
�B
�B
�B
�B
 B
 B
 B
 B
�B
B
�B
�B
oB
B
FB
MB
B
�B
�B
�B
�B
+B
�B
1B
�B
CB
qB
xB
~B
�B
�B
�B
!B
 \B
 �B
!bB
!�B
"�B
"�B
"�B
"�B
"hB
"�B
#:B
#B
"�B
#nB
%zB
&B
&B
&�B
'B
&�B
&�B
(XB
'�B
(�B
(�B
)*B
)_B
*eB
*0B
*�B
+B
,�B
,qB
,qB
.IB
.B
.IB
.IB
/B
/B
0!B
0!B
/�B
0�B
0�B
1'B
33B
2�B
2�B
33B
33B
3�B
4B
4�B
5�B
5�B
5�B
6B
5�B
6FB
6FB
6B
5�B
6zB
6B
5�B
6FB
6�B
6�B
7�B
8B
8�B
8�B
8�B
9�B
9�B
:�B
;�B
<B
<6B
<6B
<jB
=qB
<�B
<�B
<�B
=B
=B
=B
=B
=<B
=qB
>B
=�B
>B
>B
?B
@�B
A B
A�B
B[B
B�B
B�B
B�B
B�B
B�B
D3B
C�B
C�B
C�B
EB
EB
E�B
E9B
FB
F�B
GEB
GEB
GEB
GEB
F�B
GzB
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
J�B
J�B
J�B
K^B
K)B
K^B
K)B
K)B
K^B
J�B
J�B
J�B
K)B
K)B
K)B
K)B
K�B
K�B
K�B
K�B
LdB
L0B
L�B
MB
L�B
M�B
OBB
O�B
P}B
QB
QNB
QB
QB
P�B
P}B
PHB
PHB
P�B
QB
Q�B
Q�B
Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B�6B��B��B�qB�wB��B�6B��B�B�B�qB��B�}B��B�=B��B��B�=B�qB�IB��B�qB�wB�B�qB��B�IB�CB�B�IB�CB�=B�IB��B�qB�B�}B��B�qB�CB�}B�B��B��B��B�CB�=B�IB�IB�qB�qB�wB�}B�CB�=B��B��B�B�qB��B��B�qB��B��B��B�=B�B�CB��B�B�}B�B�qB�B�}B�CB��B��B��B�qB�IB�IB��B��B�}B�B�B�}B��B��B�}B�IB�qB�CB�IB�qB�wB�}B��B��B�B�}B��B�B�IB�wB�=B��B��B�CB�qB�B�IB��B��B�}B�CB��B��B�}B��B��B��B�B�=B�}B��B�qB�B�}B�wB�=B��B�}B�B��B�IB�IB�wB��B�wB�B��B�}B��B�B�B��B��B�qB�CB�}B��B��B�wB��B�B��B��B�B�wB�IB��B��B�B��B�kB�CB�}B��B�=B��B�B��B�B��B�qB�qB�B��B�B��B�=B��B�XB�_B�_B��B�B��B�B�kB��B��B��B��B��B��B�+B��B��B�MB�uB��B��B�FB�uB��B�B�SB�{B��B��B�$B��B�B��B��B��B��B��B��B��B��B��B�$B�+B�(B��B�bB��B�\B��B�\B�bB��B��B�4B��B��B�VB��B�.B�bB��B��B��B��B��B��B�VB��B�.B��B��B��B��B��B��B��B�@B� B��B�"B��B�.B��B��B��B�.B��B��B�bB� B�\B�4B�hB��B��B��B�4B�"B�bB�.B�4B��B��B��B�B�\B��B� B�:B�@B��B�bB�oB�:B��B��B� B�:B��B��B�B��B��B�B�B�	B�	B�7B��B�_B��B�$B��B�1B��B��B��B�7B��B�RB�B��B��B�VB��B�'B��B��B�kB�*B�B��B��B��B�~B��B��B�zB�CB�wB�kB�=B�9B�B�B��B�B��B�LB��B�FB��B��B�XB��B��B��B�}B�KB��B�B�BچB��B��B�dB�B�B�B�B�B��B�B��B�B��B��B�5B��B��B�iB�)B��B�cB�B�cB�B�DB.B�B
=B
=BxBB4B�B�B\B�B�B�BBB�BB�B+B�B�B*0B?HB,�B+6B/�B0UB0!B0�B.IB+�B-�B3�B=qBH�B9�B?�B8RB7LB9�B9XB;�B;0B:�B<�B;dB:�BC-B=qB?B<6B<jB<�B?}B>�B=qB<jB;dB=�B>wB>wB;�B;�B=<B=qB;�B;0B<B=qB;0B9�B:�B;dB<�B9$B8�B:*B;0B7LB<6B?�BV9BJXBFtBB�BK�BQ�BMjBH�BH�BJ�BM6BK)BJ�BLdBI�BP�BP�BYBR�BN�BNpBN�BO�BPBOBP�BN�BNBM�BOBBO�BNBMBN�BO�BOvBM�BOBBO�BO�BN�BNpBNpBOBBOvBM�BM6BN�BOBBN<BL�BMjBOBNpBNBL�BM6BOBN�BLdBM�BNBM6BJ#BJ�BM6BMjBJXBN�BJ�BLdBL�BJ�BG�BGEBG�BX�BYBj�BW?BY�BS�BXBWsBWsBR�BS&BS�BS[BS�BUgBRTBRTBT,BR�BQ�BR�BS�BR�BR�BT�BS�BR�BR�BVBUgBT�BR�BS�BT�BS[BQ�BT,BS�BS�BQ�BQNBS&BW�BT�BS�BP}BQ�BS&BQ�BVBS�BTaBQ�BP}BQ�BR�BT�BZ�BXyBY�BU�BS�BS&BT,BS�BS�BQ�BS�BT�BT�BS�BT,BS�BU�BS�BT�BT�B[�BX�BZ�BXBX�BW�BS�BT�BUgBR�BR�BS�BR�BR BS[BS�BS[BU2BT,BS[BS[BQ�BQ�BS�BS&BQNBR�BS�BQ�BT,BS�BX�B[�BX�BW�BVBV�BVBR�BP�BS&BS�BP�BPHBNpBO�BYKBOBBIRBNpBOvBI�BOBI�BJ�BOvBO�BJ#BG�BI�BI�BJXBI�BH�BIRBL�BK^BK�BK^BL�BS�BT�BFtBN<BOBBP}BJ�BI�BJ�BK^BH�BIRBHKBGEBE9BGzBFBE9BDgBB�BD3BE�BC-BE�BCaBGBGzBD3B@�B@OB>�B>BB;0B9�B:�B<�B;�B:�B9XB8�B9XB:*B:*B8�B7�B9$B8�B7LB7�B5tB8�B6zB5B0�B/�B0�B;dBB�B:�B(�B�BVB=B7BYB�B�B�B�BBuB�B�B�B.B�B�BB�BBABBB �B��B�ZB��B��B�B�B��B��B�fB�B��B��B�B�)BیB��BچB�?B�
B֡B�
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                           444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                           444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022061523082020220615230820IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022061704011020220617040110QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022061704011020220617040110QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194520230210131945IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V3.1                                                                                                                                    20230210220014              CF      PRES                            D�� G�O�D���G�O�@@  G�O�Valu passes DMQC                SI      ARSQ    SIQC    V3.1                                                                                                                                    20230210220014              CF      TEMP                            D�� G�O�D���G�O�@�  G�O�Valu passes DMQC                SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                