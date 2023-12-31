CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:50:06Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  T�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        [    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  t    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        z`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ˠ   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @ �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       
    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @ #    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       )`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` B`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   B�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   H�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   N�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T T�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   U   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   U   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   U$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   U,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � U4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   U�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   U�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    U�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        U�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        V    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       V   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    VArgo profile    3.1 1.2 19500101000000  20230721225006  20230721225006  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�.Y���@�.Y���11  @�.Z`��@�.Z`��@2�?�ײ@2�?�ײ�d���7���d���7��11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?�  @�\@B�\@��\@��R@�(�@�  AG�A��A   A,��A?\)A`  A�Q�A��A�  A�Q�A�  A�\)A߮A�Q�A��B  B(�B(�B�
B'�
B/�
B8(�B@  BG�BO�BX(�B`Q�Bh  Bo�
Bw�
B�B��B�  B�  B�{B�=qB�{B��B�  B�{B�  B�{B�{B�{B�{B�  B�  B�  B�{B�  B��
B�  B�  B��B��
B��B��B�  B�{B�  B�  B��C 
=C
=C
=C
=C
=C	��C  C  C
=C  C��C��C  C  C  C
=C   C"  C#��C&  C(
=C*
=C,  C.  C0  C1��C3��C6  C8
=C:  C<
=C>
=C@
=CB  CC��CF  CH
=CJ{CL  CN  CO��CR  CS��CV
=CX
=CY��C\
=C^  C`  Cb  Cd  Ce��Cg��Cj  Ck��Cn  Cp
=Cr
=Ct  Cu��Cx  Cz
=C{��C}��C�  C���C�C�C�C�  C���C�  C�C�C�C�  C���C�  C�  C�C�
=C�  C���C�  C�C�  C���C���C���C�  C�C�C���C���C���C���C���C���C�C�C���C���C�C�  C�  C�C�  C�  C�C�C�  C�  C�  C���C���C���C���C�  C���C�  C���C���C�  C�C�C�  C���C���C�C�  C���C���C���C�  C�  C���C���C�  C�C�C�  C�  C�
=C�
=C�
=C�C�C�  C���C���C�C�C�  C�C�C�  C�C�C�  C���C�  C�C�C�  C�  C�C�  C���C���C�  C�C�
=C�C�  C���C���C�  C�C�  C�  C�C�
=C�C���C���C���C���C�  C�  C�  C�  C���C���D �DD}qD��D}qD�qD}qD�D�D  D� D  D}qD�qD� D�qD� D	D	��D
  D
��D�D��D�D� D  D}qD�qD� D�D��DD��D�D��D�D� D�qDz�D  D� D�qD}qD  D��D  D��D�D��D  Dz�D�qD��DD��DD� D�qDz�D�qD��D��D}qD�RD }qD!�D!� D!�qD"� D"�qD#� D$�D$}qD$�qD%� D&  D&��D'D'��D'�qD(� D)�D)��D)�qD*� D+�D+}qD+��D,� D-  D-�D.D.� D/  D/z�D/��D0� D1  D1� D2D2��D2�qD3}qD3��D4}qD5�D5�D5�qD6� D7�D7��D8�D8� D8�qD9� D9�qD:� D;�D;}qD<  D<��D=�D=� D=�qD>��D?�D?� D?�qD@��DADA��DB  DB� DCDC��DD  DD}qDD�qDE� DF  DF� DG  DG� DH�DH��DI�DI� DJ  DJ}qDJ�qDK� DLDL�DM�DM� DN�DN� DN�qDO}qDO�qDP��DQ  DQ}qDR�DR��DR�qDS}qDS�qDT� DU  DU}qDV  DV� DW  DW��DXDX��DY  DYz�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^}qD_�D_� D_�qD`��Da  Da� Da�qDb}qDc  Dc}qDc�qDd}qDe  De� DfDf� Dg  Dg}qDh  Dh��Di  Di� Di�qDj� Dk�Dk}qDk��Dl}qDm  Dm� Dm�qDn}qDn�qDo� Dp�Dp� Dq  Dq� Dr  Dr� Ds�Ds��Dt�Dt��Du  Duz�Dv  Dv��Dw�Dw� Dw�qDx��Dy�Dy�DzDz� D{�D{�D|�D|�D}D}��D~  D~}qD~�qD}qD�qD�AHD�~�D���D���D�@ D�� D�� D�  D�@ D��HD��HD�HD�AHD��HD�� D���D�@ D�� D�� D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�}qD��qD��qD�>�D�� D�� D�HD�AHD��HD��HD��D�+�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?B�\?�  ?�p�?�G�@�@�R@:�H@J=q@h��@��
@���@�(�@�=q@�@�G�@У�@�p�@���@�z�A�\A�A��Az�A�HA�RA&ffA,��A1G�A7�A?\)AC�
AH��AP��AUAZ=qAa�Ag
=Al(�As�
Ay��A~�RA��A�p�A�Q�A��\A�A���A�z�A��RA��\A��A�  A��A�A���A�z�A��RA�G�A���A�\)A��A�A�  A\A�{A���A��HA�{A���A�33A�ffAٙ�AۅA�ffAᙚA��
A�ffA�G�A�(�A�ffA��A�(�A�ffA�Q�A��
A�ffB z�BB�B��B{B�
B��B
{B  B��B�\B  B�B=qB�
Bp�B=qB�BG�B=qB�BG�B{B�B ��B!B#\)B$��B%B'
=B(��B)B*�RB,Q�B-p�B.�\B0(�B1��B2�RB4(�B5B6�RB7�
B9p�B:�RB<  B=�B>�RB@(�BAG�BBffBD(�BEG�BFffBH  BI��BJ�HBL  BMG�BN�HBPQ�BQG�BR�RBTQ�BU��BV�\BX(�BY��B[
=B[�
B]G�B^�HB`  B`��Bb�\Bd  Bd��Bf=qBg�
Bi�Bj{Bk�Bm�Bm�Bo�Bq�Br{Bs\)Bu�Bv=qBw33Bx��BzffB{�B|��B~�\B�B�z�B�G�B�{B���B�G�B�{B��HB���B�(�B���B�B�Q�B���B��
B�z�B�
=B�B���B�33B�B��\B�33B�B�Q�B��B���B�{B��RB�\)B��
B�=qB���B�p�B�B�Q�B��HB�G�B���B�{B���B���B�G�B��
B�(�B�ffB���B�p�B��B�{B��\B���B�33B��B�{B�z�B��RB�33B��B�{B�ffB���B�G�B�B�  B�Q�B��HB�\)B���B�{B���B�
=B�\)B��B�=qB��RB��B�p�B��
B�ffB��RB��B��B�{B�ffB���B�p�B��B�{B�z�B��B�p�B�B�(�B��RB�33B�p�B��B�z�B���B��B��B�(�B�z�B���B�\)B��
B�(�B�z�B���B��B��
B�(�B���B��B�p�B�B�=qB���B��B�p�B��B�z�B���B�
=B�p�B�{B�z�B��RB��B��B�(�B�z�B���B�33B�B�(�B�ffB��HB�p�B�B�{B£�B�
=B�G�BÙ�B�(�Bģ�B���B�33Bř�B�(�BƏ\B���B�33BǮB�(�B�ffBȣ�B�33BɮB�  B�=qBʸRB�33B˙�B�B�{B̏\B���B�33BͅB�  B�Q�BΏ\B���B�p�BϮB�  B�Q�B���B�
=B�G�B��
B�=qB�z�Bң�B��BӅBӮB�(�Bԣ�B���B�33Bՙ�B�B�=qB֣�B��HB��B׮B�  B�(�Bأ�B��B�G�BٮB�(�B�z�B���B�\)Bۙ�B��
B�Q�B���B�
=B�G�B��
B�=qB�z�B޸RB�33Bߙ�B�B�Q�B�RB���B�\)B��
B�(�B�ffB�RB�\)B�B��B�Q�B��HB�33B�p�B�  B�ffB��B���B�B��B�{B��B�33B�\)B�B�Q�B��B���B�B��B�(�B�z�B���B�p�B��B�(�B��B��HB�\)B��
B�(�B�z�B�
=B�B�B�(�B��B��B�\)B��
B�ffB��RB�
=B���B�{B�Q�B��HB�p�B��
B�{B���B��B��B��
B�=qB���B�G�B���B��B�z�B�
=B�G�B�B�Q�B��\B��B��C   C 33C �C C �C�CffC��C��C  CQ�C�\C�C�C33CQ�C�C�
C{C=qCz�CC�C
=C\)C��CC�C(�Cz�C��C��C
=C\)C�\C�RC�C33Cp�C��C��C	{C	\)C	�C	�RC	�C
=qC
z�C
��C
�
C(�CffC�C��C{CG�Cp�C�RC  C33C\)C�\C�
C�C\)C�C�C��C=qCz�C�C�
C{CG�C��C��C  C(�Cz�CC�C�C\)C�C�C{CQ�C��C�HC{CG�Cz�CC
=C=qCffC�RC  C33C\)C��C�C33C\)C�\CC
=CQ�C��C��C��C(�Cp�C�RC�C�CG�C��C�
C�CG�Cz�C�C  C=qCffC��C��C
=CQ�C�\CC�C(�Cz�C�C�HC{C\)C��C�HC {C G�C z�C C!
=C!G�C!�C!C!�C"33C"�C"C#  C#(�C#ffC#�C#��C$(�C$\)C$��C$�HC%(�C%\)C%�\C%C&  C&G�C&�\C&��C'  C'(�C'\)C'��C'�C((�C(ffC(�\C(C)  C)G�C)z�C)�RC)��C*�C*\)C*�\C*�
C+{C+\)C+��C+�
C,
=C,=qC,z�C,C-  C-=qC-p�C-��C-�
C.�C.ffC.��C.C/  C/G�C/�\C/C/��C0(�C0\)C0��C0�HC1�C1ffC1�\C1C1��C233C2z�C2�RC2�C3�C3Q�C3�\C3C3��C433C4z�C4�RC4��C5(�C5Q�C5��C5�HC6�C6Q�C6z�C6�RC7  C7G�C7z�C7�C7�HC8{C8Q�C8�\C8�
C9{C9Q�C9�C9�RC9�C:�C:\)C:��C:�
C;�C;\)C;��C;��C;��C<(�C<\)C<�\C<��C={C=Q�C=��C=��C>  C>33C>\)C>��C>�HC?{C?\)C?��C?�
C@{C@Q�C@�\C@�
CA
=CA=qCAp�CA��CA�HCB(�CBffCB��CB�HCC�CCQ�CC�\CC�RCC�CD(�CDffCD�CD�CE33CEp�CE��CE�
CF{CF=qCFz�CF�CF�HCG�CGffCG��CG�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @�\@B�\@��\@��R@�(�@�  AG�A��A   A,��A?\)A`  A�Q�A��A�  A�Q�A�  A�\)A߮A�Q�A��B  B(�B(�B�
B'�
B/�
B8(�B@  BG�BO�BX(�B`Q�Bh  Bo�
Bw�
B�B��B�  B�  B�{B�=qB�{B��B�  B�{B�  B�{B�{B�{B�{B�  B�  B�  B�{B�  B��
B�  B�  B��B��
B��B��B�  B�{B�  B�  B��C 
=C
=C
=C
=C
=C	��C  C  C
=C  C��C��C  C  C  C
=C   C"  C#��C&  C(
=C*
=C,  C.  C0  C1��C3��C6  C8
=C:  C<
=C>
=C@
=CB  CC��CF  CH
=CJ{CL  CN  CO��CR  CS��CV
=CX
=CY��C\
=C^  C`  Cb  Cd  Ce��Cg��Cj  Ck��Cn  Cp
=Cr
=Ct  Cu��Cx  Cz
=C{��C}��C�  C���C�C�C�C�  C���C�  C�C�C�C�  C���C�  C�  C�C�
=C�  C���C�  C�C�  C���C���C���C�  C�C�C���C���C���C���C���C���C�C�C���C���C�C�  C�  C�C�  C�  C�C�C�  C�  C�  C���C���C���C���C�  C���C�  C���C���C�  C�C�C�  C���C���C�C�  C���C���C���C�  C�  C���C���C�  C�C�C�  C�  C�
=C�
=C�
=C�C�C�  C���C���C�C�C�  C�C�C�  C�C�C�  C���C�  C�C�C�  C�  C�C�  C���C���C�  C�C�
=C�C�  C���C���C�  C�C�  C�  C�C�
=C�C���C���C���C���C�  C�  C�  C�  C���C���D �DD}qD��D}qD�qD}qD�D�D  D� D  D}qD�qD� D�qD� D	D	��D
  D
��D�D��D�D� D  D}qD�qD� D�D��DD��D�D��D�D� D�qDz�D  D� D�qD}qD  D��D  D��D�D��D  Dz�D�qD��DD��DD� D�qDz�D�qD��D��D}qD�RD }qD!�D!� D!�qD"� D"�qD#� D$�D$}qD$�qD%� D&  D&��D'D'��D'�qD(� D)�D)��D)�qD*� D+�D+}qD+��D,� D-  D-�D.D.� D/  D/z�D/��D0� D1  D1� D2D2��D2�qD3}qD3��D4}qD5�D5�D5�qD6� D7�D7��D8�D8� D8�qD9� D9�qD:� D;�D;}qD<  D<��D=�D=� D=�qD>��D?�D?� D?�qD@��DADA��DB  DB� DCDC��DD  DD}qDD�qDE� DF  DF� DG  DG� DH�DH��DI�DI� DJ  DJ}qDJ�qDK� DLDL�DM�DM� DN�DN� DN�qDO}qDO�qDP��DQ  DQ}qDR�DR��DR�qDS}qDS�qDT� DU  DU}qDV  DV� DW  DW��DXDX��DY  DYz�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^}qD_�D_� D_�qD`��Da  Da� Da�qDb}qDc  Dc}qDc�qDd}qDe  De� DfDf� Dg  Dg}qDh  Dh��Di  Di� Di�qDj� Dk�Dk}qDk��Dl}qDm  Dm� Dm�qDn}qDn�qDo� Dp�Dp� Dq  Dq� Dr  Dr� Ds�Ds��Dt�Dt��Du  Duz�Dv  Dv��Dw�Dw� Dw�qDx��Dy�Dy�DzDz� D{�D{�D|�D|�D}D}��D~  D~}qD~�qD}qD�qD�AHD�~�D���D���D�@ D�� D�� D�  D�@ D��HD��HD�HD�AHD��HD�� D���D�@ D�� D�� D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�}qD��qD��qD�>�D�� D�� D�HD�AHD��HD��HD��D�+�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?B�\?�  ?�p�?�G�@�@�R@:�H@J=q@h��@��
@���@�(�@�=q@�@�G�@У�@�p�@���@�z�A�\A�A��Az�A�HA�RA&ffA,��A1G�A7�A?\)AC�
AH��AP��AUAZ=qAa�Ag
=Al(�As�
Ay��A~�RA��A�p�A�Q�A��\A�A���A�z�A��RA��\A��A�  A��A�A���A�z�A��RA�G�A���A�\)A��A�A�  A\A�{A���A��HA�{A���A�33A�ffAٙ�AۅA�ffAᙚA��
A�ffA�G�A�(�A�ffA��A�(�A�ffA�Q�A��
A�ffB z�BB�B��B{B�
B��B
{B  B��B�\B  B�B=qB�
Bp�B=qB�BG�B=qB�BG�B{B�B ��B!B#\)B$��B%B'
=B(��B)B*�RB,Q�B-p�B.�\B0(�B1��B2�RB4(�B5B6�RB7�
B9p�B:�RB<  B=�B>�RB@(�BAG�BBffBD(�BEG�BFffBH  BI��BJ�HBL  BMG�BN�HBPQ�BQG�BR�RBTQ�BU��BV�\BX(�BY��B[
=B[�
B]G�B^�HB`  B`��Bb�\Bd  Bd��Bf=qBg�
Bi�Bj{Bk�Bm�Bm�Bo�Bq�Br{Bs\)Bu�Bv=qBw33Bx��BzffB{�B|��B~�\B�B�z�B�G�B�{B���B�G�B�{B��HB���B�(�B���B�B�Q�B���B��
B�z�B�
=B�B���B�33B�B��\B�33B�B�Q�B��B���B�{B��RB�\)B��
B�=qB���B�p�B�B�Q�B��HB�G�B���B�{B���B���B�G�B��
B�(�B�ffB���B�p�B��B�{B��\B���B�33B��B�{B�z�B��RB�33B��B�{B�ffB���B�G�B�B�  B�Q�B��HB�\)B���B�{B���B�
=B�\)B��B�=qB��RB��B�p�B��
B�ffB��RB��B��B�{B�ffB���B�p�B��B�{B�z�B��B�p�B�B�(�B��RB�33B�p�B��B�z�B���B��B��B�(�B�z�B���B�\)B��
B�(�B�z�B���B��B��
B�(�B���B��B�p�B�B�=qB���B��B�p�B��B�z�B���B�
=B�p�B�{B�z�B��RB��B��B�(�B�z�B���B�33B�B�(�B�ffB��HB�p�B�B�{B£�B�
=B�G�BÙ�B�(�Bģ�B���B�33Bř�B�(�BƏ\B���B�33BǮB�(�B�ffBȣ�B�33BɮB�  B�=qBʸRB�33B˙�B�B�{B̏\B���B�33BͅB�  B�Q�BΏ\B���B�p�BϮB�  B�Q�B���B�
=B�G�B��
B�=qB�z�Bң�B��BӅBӮB�(�Bԣ�B���B�33Bՙ�B�B�=qB֣�B��HB��B׮B�  B�(�Bأ�B��B�G�BٮB�(�B�z�B���B�\)Bۙ�B��
B�Q�B���B�
=B�G�B��
B�=qB�z�B޸RB�33Bߙ�B�B�Q�B�RB���B�\)B��
B�(�B�ffB�RB�\)B�B��B�Q�B��HB�33B�p�B�  B�ffB��B���B�B��B�{B��B�33B�\)B�B�Q�B��B���B�B��B�(�B�z�B���B�p�B��B�(�B��B��HB�\)B��
B�(�B�z�B�
=B�B�B�(�B��B��B�\)B��
B�ffB��RB�
=B���B�{B�Q�B��HB�p�B��
B�{B���B��B��B��
B�=qB���B�G�B���B��B�z�B�
=B�G�B�B�Q�B��\B��B��C   C 33C �C C �C�CffC��C��C  CQ�C�\C�C�C33CQ�C�C�
C{C=qCz�CC�C
=C\)C��CC�C(�Cz�C��C��C
=C\)C�\C�RC�C33Cp�C��C��C	{C	\)C	�C	�RC	�C
=qC
z�C
��C
�
C(�CffC�C��C{CG�Cp�C�RC  C33C\)C�\C�
C�C\)C�C�C��C=qCz�C�C�
C{CG�C��C��C  C(�Cz�CC�C�C\)C�C�C{CQ�C��C�HC{CG�Cz�CC
=C=qCffC�RC  C33C\)C��C�C33C\)C�\CC
=CQ�C��C��C��C(�Cp�C�RC�C�CG�C��C�
C�CG�Cz�C�C  C=qCffC��C��C
=CQ�C�\CC�C(�Cz�C�C�HC{C\)C��C�HC {C G�C z�C C!
=C!G�C!�C!C!�C"33C"�C"C#  C#(�C#ffC#�C#��C$(�C$\)C$��C$�HC%(�C%\)C%�\C%C&  C&G�C&�\C&��C'  C'(�C'\)C'��C'�C((�C(ffC(�\C(C)  C)G�C)z�C)�RC)��C*�C*\)C*�\C*�
C+{C+\)C+��C+�
C,
=C,=qC,z�C,C-  C-=qC-p�C-��C-�
C.�C.ffC.��C.C/  C/G�C/�\C/C/��C0(�C0\)C0��C0�HC1�C1ffC1�\C1C1��C233C2z�C2�RC2�C3�C3Q�C3�\C3C3��C433C4z�C4�RC4��C5(�C5Q�C5��C5�HC6�C6Q�C6z�C6�RC7  C7G�C7z�C7�C7�HC8{C8Q�C8�\C8�
C9{C9Q�C9�C9�RC9�C:�C:\)C:��C:�
C;�C;\)C;��C;��C;��C<(�C<\)C<�\C<��C={C=Q�C=��C=��C>  C>33C>\)C>��C>�HC?{C?\)C?��C?�
C@{C@Q�C@�\C@�
CA
=CA=qCAp�CA��CA�HCB(�CBffCB��CB�HCC�CCQ�CC�\CC�RCC�CD(�CDffCD�CD�CE33CEp�CE��CE�
CF{CF=qCFz�CF�CF�HCG�CGffCG��CG�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A֟�A֥�A֧�A֬A֩�A֩�A֮Aֲ-Aִ9Aְ!Aְ!A֮A֮Aְ!Aֲ-Aֲ-Aִ9Aִ9A֮A֮A֩�A֡�A֝�A�`BA�\)A�Q�A��TAԥ�A�7LA�33A���A�+A��mA��A�=qA�5?A�S�A���AɾwA�jA���A�1A�S�A�\)A���A��#A¶FA���A�x�A�oA�ȴA���A��#A�I�A���A��9A��A���A�XA�^5A��\A�v�A�\)A�/A�A�+A�G�A�&�A��uA���A��FA���A��A�?}A��-A�7LA��FA��A��DA��A���A�1A���A���A�1'A�M�A�(�A���A��^A��uA�\)A�1A���A��`A�7LA��^A�oA�1'A�x�A���A�bNA~{A|E�Ax�9Au|�As�wAp�Al1Aix�Af�Ae�Adn�AahsA]ƨAX��AVJAT~�AR-AM\)AH��AG�AFAC�hAA�FA?oA<r�A:�DA9�A7+A6�A3��A1S�A1A0jA.ZA-l�A,ĜA+�hA'A&1A%+A$A�A#"�A"�DA"bA!��AS�A�/A��AdZA��A��A��A�AA�A�^A;dA�;A`BAK�AVAM�A��A{AXA��A
�jA	?}A �A��AA;dA��A��AA��A��A^5A=qA�TA\)A��A��A��A~�A��A �u@��@���@��
@���@�dZ@�;d@���@���@�?}@��@���@���@�R@�-@���@�G�@�  @ꟾ@�I�@��@旍@�@���@��T@��@�9@ߍP@�o@ޟ�@�n�@ݡ�@��`@ۍP@��@ڧ�@�V@���@���@ם�@�ȴ@Չ7@ԣ�@��@Ѻ^@�7L@��`@Гu@�Q�@�1'@�1@��
@�o@��@�1@���@��T@��@��@�;d@���@�$�@š�@�x�@�&�@ě�@�A�@�1@��
@�ff@�@��T@���@�/@�j@��@�|�@���@�t�@�\)@��m@�C�@���@���@�~�@�~�@��+@���@��u@��/@�V@�?}@�hs@���@�ff@�@��@�/@���@�|�@��P@���@���@��
@��@���@���@��h@�/@�Ĝ@�z�@�b@��;@��y@��@�=q@�E�@��@��7@���@�A�@�(�@���@�ȴ@�ff@�M�@�=q@�$�@�@��@��#@���@��^@��-@���@���@��h@��h@���@���@�I�@�A�@�j@�r�@��@��D@�r�@�9X@�  @��@��@�E�@��@�p�@�/@���@�z�@�b@��F@�S�@���@�M�@��7@�/@���@�Z@�(�@��@���@�\)@�;d@��@��@��@��\@�E�@�$�@��@���@�G�@��@���@��
@�ƨ@��@��P@�K�@��y@��\@��+@��+@�n�@�M�@�=q@�$�@�{@�@���@�?}@��@��u@�z�@� �@�  @��@��;@�;d@�E�@���@��7@�/@���@�Ĝ@��9@�bN@�b@��
@��@�C�@��@�=q@���@��-@��7@�`B@�?}@���@�1'@��@�  @��@��;@���@�l�@�C�@�@�n�@�=q@�$�@�{@���@��^@��@��@��`@���@���@�1@��@��@�S�@�;d@��@���@�ff@��@��@��#@��-@�&�@��/@��9@��@�Q�@�9X@�9X@� �@�  @��m@�l�@�C�@��@��\@�V@��@�J@��@���@�X@�%@��/@�Ĝ@��D@�j@�I�@�9X@� �@��;@���@��@�dZ@�o@��@��H@�~�@��T@�?}@��@��`@�r�@�A�@�(�@��@�  @���@��@��m@��@�S�@�o@��y@��H@��!@�5?@��@��-@��7@�`B@�G�@���@���@�r�@�Z@�(�@��
@��@���@��@�K�@�o@���@��R@�n�@�5?@��@�J@�@��@�@�?}@�r�@|�@K�@
=@~ȴ@~��@~��@~��@~�+@~$�@~$�@~$�@~@}�T@}�h@}O�@}O�@}�@|�/@|�D@|9X@|1@{�
@{�F@{��@{dZ@{o@z��@z�\@z^5@z�@y�@y��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A֡�A֝�A֛�A֡�A֧�A֟�A֩�A֩�A֧�A֧�Aְ!A֩�A֩�A֮A֧�A֩�A֩�A֬A֮A֮Aִ9Aֲ-Aְ!AֶFAִ9A֬A֮Aֲ-A֩�A֬Aֲ-A֬A֬Aְ!A֬A֬Aֲ-A֮A֩�Aֲ-Aֲ-A֮A֮Aִ9Aִ9Aְ!Aְ!Aִ9Aִ9Aְ!Aִ9Aֲ-Aְ!Aִ9Aְ!Aֲ-AֶFAֲ-Aְ!AֶFAֶFAֲ-AָRAִ9Aֲ-AֶFAִ9A֮Aֲ-A֬A֩�Aְ!Aִ9A֮A֮Aְ!A֩�A֩�Aְ!A֮Aְ!A֬A֧�A֡�A֝�A֣�A֣�A֙�A֟�A֩�A֩�A֧�A֮A֬Aև+A�t�A�bNA�bNA�`BA�`BA�ZA�^5A�^5A�XA�bNA�`BA�ZA�XA�ZA�O�A�Q�A�K�A�O�A�M�A�(�A�VA�A��`Aհ!AՉ7A�ZA�7LA��A���A�XAӥ�A�{A�O�A�/A��A��A�
=A��AёhA�E�A��A�  A��A��A��/A���A���A���A�ƨAиRAЮAЋDA�`BAϬA�bNA�G�A��A��A���A�ĜAξwAήA΅A�Q�A� �A��AͼjAͬAͅA�n�A�C�A�33A�
=A�ƨA̅A�\)A�Q�A�;dA��HAˬA�n�A�XA�K�A�E�A�E�A�;dA�5?A�/A��A�VA�jA���A��mA���AɮAɝ�Aɗ�AɍPA�~�A�r�A�bNA�VA�M�A�/A�bA���A��A���AȸRAȅA�&�A�%A��A��Aǲ-AǓuA�~�A�|�A�v�A�ZA�=qA�"�A�bA���A��A��#AƲ-A�`BA�"�A��A�AŋDA�I�A�33A�"�A��A�%A���A��A��
Aİ!AăA�ZA�A�A�"�A�oA���A��TA���A�AÍPA�ffA�?}A�$�A���A��mA���A���A¼jA�v�A�S�A�M�A�7LA�+A�1A��RA��A���A���A���A���A���A���A��hA��\A��7A��A�p�A�bNA�S�A�K�A�G�A�E�A�=qA�5?A�+A�"�A�  A���A��^A�t�A�7LA�bA���A��A�ƨA�z�A�r�A�dZA�\)A�A�A�7LA�9XA�5?A��A��/A��A��A���A��PA�p�A�M�A���A��FA��!A��A���A���A���A��A�x�A�p�A�n�A�ffA�M�A�33A�-A��A�A��A��;A���A�ȴA��wA��-A��A��hA�n�A�E�A��A��A��HA��HA��HA���A��!A��PA�hsA�`BA�5?A�+A�&�A�$�A�"�A��A��A��A��A�
=A�A�A���A��A��TA��#A���A�ĜA���A��wA��9A���A���A��hA��+A�l�A�=qA���A��A��RA��A���A��uA��A�A�A�9XA�5?A�bA���A��!A��hA��A�n�A�bA�l�A��^A�JA��PA��A���A�?}A�/A�-A�+A�&�A��A� �A��A��`A���A��jA���A�t�A�ffA�7LA���A��#A��RA���A��hA��DA�~�A�jA�ZA�XA�Q�A�9XA�  A���A��FA���A�t�A�O�A�?}A�$�A��yA���A��FA���A��7A�E�A�33A� �A�
=A��A��;A���A��RA��\A��A�p�A�S�A�=qA�5?A� �A��A�VA�A��;A��RA���A�l�A�S�A�M�A�7LA�"�A�VA��A��#A���A�Q�A�"�A�bA���A��A��A��mA��;A��
A���A�ƨA��^A���A��A�dZA�33A�1A��/A���A�5?A��!A�E�A�{A���A���A��#A���A�A��jA��FA��!A��!A��A���A���A���A���A���A���A���A���A���A���A���A�v�A�=qA�{A��A��;A��RA���A��PA�ffA�A�|�A���A�r�A�oA��`A���A�ffA�C�A�(�A�VA���A�A��FA��DA�t�A�\)A�;dA�{A��A��A�+A�1A���A��9A���A���A��\A�dZA�VA��RA���A�z�A�I�A�%A��A��DA�XA�/A�JA��A�ȴA��uA�l�A�"�A��;A�ĜA��^A��A���A���A� �A���A�|�A�A�A��;A���A�^5A�O�A�=qA�+A��A���A��#A��A���A�ȴA��RA��FA��9A��!A���A�=qA��mA���A�A��RA��A���A���A���A���A���A���A��uA���A��PA�\)A��A��#A��RA���A�v�A�VA�5?A�$�A���A��!A�|�A�t�A�I�A��A��;A���A��PA��A�A��;A���A�Q�A�A���A�^5A���A���A�\)A�C�A��A��A��/A���A���A���A�ĜA�ĜA��-A���A��+A�`BA�;dA�"�A���A���A��-A��!A���A��DA�z�A�ZA�33A��A�JA���A��/A���A��\A��A��A��A��A�|�A�x�A�p�A�dZA�M�A��A��A�1'A��A�VA�  A��A��TA���A��-A��+A�hsA�K�A��A���A�/A�ƨA�\)A��
A���A���A�A���A�p�A�\)A�M�A�C�A�?}A�9XA�5?A�5?A�5?A�5?A�5?A�33A�33A��A��A�A��TA�ȴA��+A�n�A�E�A��A���A��\A�XA�"�A�A��RA���A��DA�z�A�XA�G�A�9XA�/A�&�A�"�A��A�oA�A��A���A��FA���A�n�A�5?A��A��/A��A���A��jA��A���A��\A��DA�~�A�ZA�VA�jA�7LA���A�XA��A&�A~ĜA~�A~�A~��A~��A~��A~�+A}�
A}�A|��A|�A|�HA|��A|�jA|v�A|1'A{�A{��A{O�Az��Az�Az-Ay�Ax~�Aw��Aw33AwAvĜAv�\AvE�Au��Au�-AuhsAu&�At�At�\AtjAtbNAt^5At^5AtI�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A֟�A֥�A֧�A֬A֩�A֩�A֮Aֲ-Aִ9Aְ!Aְ!A֮A֮Aְ!Aֲ-Aֲ-Aִ9Aִ9A֮A֮A֩�A֡�A֝�A�`BA�\)A�Q�A��TAԥ�A�7LA�33A���A�+A��mA��A�=qA�5?A�S�A���AɾwA�jA���A�1A�S�A�\)A���A��#A¶FA���A�x�A�oA�ȴA���A��#A�I�A���A��9A��A���A�XA�^5A��\A�v�A�\)A�/A�A�+A�G�A�&�A��uA���A��FA���A��A�?}A��-A�7LA��FA��A��DA��A���A�1A���A���A�1'A�M�A�(�A���A��^A��uA�\)A�1A���A��`A�7LA��^A�oA�1'A�x�A���A�bNA~{A|E�Ax�9Au|�As�wAp�Al1Aix�Af�Ae�Adn�AahsA]ƨAX��AVJAT~�AR-AM\)AH��AG�AFAC�hAA�FA?oA<r�A:�DA9�A7+A6�A3��A1S�A1A0jA.ZA-l�A,ĜA+�hA'A&1A%+A$A�A#"�A"�DA"bA!��AS�A�/A��AdZA��A��A��A�AA�A�^A;dA�;A`BAK�AVAM�A��A{AXA��A
�jA	?}A �A��AA;dA��A��AA��A��A^5A=qA�TA\)A��A��A��A~�A��A �u@��@���@��
@���@�dZ@�;d@���@���@�?}@��@���@���@�R@�-@���@�G�@�  @ꟾ@�I�@��@旍@�@���@��T@��@�9@ߍP@�o@ޟ�@�n�@ݡ�@��`@ۍP@��@ڧ�@�V@���@���@ם�@�ȴ@Չ7@ԣ�@��@Ѻ^@�7L@��`@Гu@�Q�@�1'@�1@��
@�o@��@�1@���@��T@��@��@�;d@���@�$�@š�@�x�@�&�@ě�@�A�@�1@��
@�ff@�@��T@���@�/@�j@��@�|�@���@�t�@�\)@��m@�C�@���@���@�~�@�~�@��+@���@��u@��/@�V@�?}@�hs@���@�ff@�@��@�/@���@�|�@��P@���@���@��
@��@���@���@��h@�/@�Ĝ@�z�@�b@��;@��y@��@�=q@�E�@��@��7@���@�A�@�(�@���@�ȴ@�ff@�M�@�=q@�$�@�@��@��#@���@��^@��-@���@���@��h@��h@���@���@�I�@�A�@�j@�r�@��@��D@�r�@�9X@�  @��@��@�E�@��@�p�@�/@���@�z�@�b@��F@�S�@���@�M�@��7@�/@���@�Z@�(�@��@���@�\)@�;d@��@��@��@��\@�E�@�$�@��@���@�G�@��@���@��
@�ƨ@��@��P@�K�@��y@��\@��+@��+@�n�@�M�@�=q@�$�@�{@�@���@�?}@��@��u@�z�@� �@�  @��@��;@�;d@�E�@���@��7@�/@���@�Ĝ@��9@�bN@�b@��
@��@�C�@��@�=q@���@��-@��7@�`B@�?}@���@�1'@��@�  @��@��;@���@�l�@�C�@�@�n�@�=q@�$�@�{@���@��^@��@��@��`@���@���@�1@��@��@�S�@�;d@��@���@�ff@��@��@��#@��-@�&�@��/@��9@��@�Q�@�9X@�9X@� �@�  @��m@�l�@�C�@��@��\@�V@��@�J@��@���@�X@�%@��/@�Ĝ@��D@�j@�I�@�9X@� �@��;@���@��@�dZ@�o@��@��H@�~�@��T@�?}@��@��`@�r�@�A�@�(�@��@�  @���@��@��m@��@�S�@�o@��y@��H@��!@�5?@��@��-@��7@�`B@�G�@���@���@�r�@�Z@�(�@��
@��@���@��@�K�@�o@���@��R@�n�@�5?@��@�J@�@��@�@�?}@�r�@|�@K�@
=@~ȴ@~��@~��@~��@~�+@~$�@~$�@~$�@~@}�T@}�h@}O�@}O�@}�@|�/@|�D@|9X@|1@{�
@{�F@{��@{dZ@{o@z��@z�\@z^5@z�@y�@y��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A֡�A֝�A֛�A֡�A֧�A֟�A֩�A֩�A֧�A֧�Aְ!A֩�A֩�A֮A֧�A֩�A֩�A֬A֮A֮Aִ9Aֲ-Aְ!AֶFAִ9A֬A֮Aֲ-A֩�A֬Aֲ-A֬A֬Aְ!A֬A֬Aֲ-A֮A֩�Aֲ-Aֲ-A֮A֮Aִ9Aִ9Aְ!Aְ!Aִ9Aִ9Aְ!Aִ9Aֲ-Aְ!Aִ9Aְ!Aֲ-AֶFAֲ-Aְ!AֶFAֶFAֲ-AָRAִ9Aֲ-AֶFAִ9A֮Aֲ-A֬A֩�Aְ!Aִ9A֮A֮Aְ!A֩�A֩�Aְ!A֮Aְ!A֬A֧�A֡�A֝�A֣�A֣�A֙�A֟�A֩�A֩�A֧�A֮A֬Aև+A�t�A�bNA�bNA�`BA�`BA�ZA�^5A�^5A�XA�bNA�`BA�ZA�XA�ZA�O�A�Q�A�K�A�O�A�M�A�(�A�VA�A��`Aհ!AՉ7A�ZA�7LA��A���A�XAӥ�A�{A�O�A�/A��A��A�
=A��AёhA�E�A��A�  A��A��A��/A���A���A���A�ƨAиRAЮAЋDA�`BAϬA�bNA�G�A��A��A���A�ĜAξwAήA΅A�Q�A� �A��AͼjAͬAͅA�n�A�C�A�33A�
=A�ƨA̅A�\)A�Q�A�;dA��HAˬA�n�A�XA�K�A�E�A�E�A�;dA�5?A�/A��A�VA�jA���A��mA���AɮAɝ�Aɗ�AɍPA�~�A�r�A�bNA�VA�M�A�/A�bA���A��A���AȸRAȅA�&�A�%A��A��Aǲ-AǓuA�~�A�|�A�v�A�ZA�=qA�"�A�bA���A��A��#AƲ-A�`BA�"�A��A�AŋDA�I�A�33A�"�A��A�%A���A��A��
Aİ!AăA�ZA�A�A�"�A�oA���A��TA���A�AÍPA�ffA�?}A�$�A���A��mA���A���A¼jA�v�A�S�A�M�A�7LA�+A�1A��RA��A���A���A���A���A���A���A��hA��\A��7A��A�p�A�bNA�S�A�K�A�G�A�E�A�=qA�5?A�+A�"�A�  A���A��^A�t�A�7LA�bA���A��A�ƨA�z�A�r�A�dZA�\)A�A�A�7LA�9XA�5?A��A��/A��A��A���A��PA�p�A�M�A���A��FA��!A��A���A���A���A��A�x�A�p�A�n�A�ffA�M�A�33A�-A��A�A��A��;A���A�ȴA��wA��-A��A��hA�n�A�E�A��A��A��HA��HA��HA���A��!A��PA�hsA�`BA�5?A�+A�&�A�$�A�"�A��A��A��A��A�
=A�A�A���A��A��TA��#A���A�ĜA���A��wA��9A���A���A��hA��+A�l�A�=qA���A��A��RA��A���A��uA��A�A�A�9XA�5?A�bA���A��!A��hA��A�n�A�bA�l�A��^A�JA��PA��A���A�?}A�/A�-A�+A�&�A��A� �A��A��`A���A��jA���A�t�A�ffA�7LA���A��#A��RA���A��hA��DA�~�A�jA�ZA�XA�Q�A�9XA�  A���A��FA���A�t�A�O�A�?}A�$�A��yA���A��FA���A��7A�E�A�33A� �A�
=A��A��;A���A��RA��\A��A�p�A�S�A�=qA�5?A� �A��A�VA�A��;A��RA���A�l�A�S�A�M�A�7LA�"�A�VA��A��#A���A�Q�A�"�A�bA���A��A��A��mA��;A��
A���A�ƨA��^A���A��A�dZA�33A�1A��/A���A�5?A��!A�E�A�{A���A���A��#A���A�A��jA��FA��!A��!A��A���A���A���A���A���A���A���A���A���A���A���A�v�A�=qA�{A��A��;A��RA���A��PA�ffA�A�|�A���A�r�A�oA��`A���A�ffA�C�A�(�A�VA���A�A��FA��DA�t�A�\)A�;dA�{A��A��A�+A�1A���A��9A���A���A��\A�dZA�VA��RA���A�z�A�I�A�%A��A��DA�XA�/A�JA��A�ȴA��uA�l�A�"�A��;A�ĜA��^A��A���A���A� �A���A�|�A�A�A��;A���A�^5A�O�A�=qA�+A��A���A��#A��A���A�ȴA��RA��FA��9A��!A���A�=qA��mA���A�A��RA��A���A���A���A���A���A���A��uA���A��PA�\)A��A��#A��RA���A�v�A�VA�5?A�$�A���A��!A�|�A�t�A�I�A��A��;A���A��PA��A�A��;A���A�Q�A�A���A�^5A���A���A�\)A�C�A��A��A��/A���A���A���A�ĜA�ĜA��-A���A��+A�`BA�;dA�"�A���A���A��-A��!A���A��DA�z�A�ZA�33A��A�JA���A��/A���A��\A��A��A��A��A�|�A�x�A�p�A�dZA�M�A��A��A�1'A��A�VA�  A��A��TA���A��-A��+A�hsA�K�A��A���A�/A�ƨA�\)A��
A���A���A�A���A�p�A�\)A�M�A�C�A�?}A�9XA�5?A�5?A�5?A�5?A�5?A�33A�33A��A��A�A��TA�ȴA��+A�n�A�E�A��A���A��\A�XA�"�A�A��RA���A��DA�z�A�XA�G�A�9XA�/A�&�A�"�A��A�oA�A��A���A��FA���A�n�A�5?A��A��/A��A���A��jA��A���A��\A��DA�~�A�ZA�VA�jA�7LA���A�XA��A&�A~ĜA~�A~�A~��A~��A~��A~�+A}�
A}�A|��A|�A|�HA|��A|�jA|v�A|1'A{�A{��A{O�Az��Az�Az-Ay�Ax~�Aw��Aw33AwAvĜAv�\AvE�Au��Au�-AuhsAu&�At�At�\AtjAtbNAt^5At^5AtI�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BpBo5Bp;Bo5BpBpBoiBo�BoiBp;Bo5BpBo�Bo�Bo�Bo�Bo5Bo�BpBoiBoiBoiBpBo�Bn�Bm)Bn/Bk�B]�Bl�Bv+B��B��B��B�tB�B�DB�B/OB7�BA�BL0BQ�Be�B.B�B�oB|�Bz�Bz�B�4B~�B��B�{B�GB��B�MB��B~]Bv�B[�BPHBM�BJ�BA B;�B7�B*eB�BPB��B��B�B�B�sBΥB� B��B��B�SBv`Bn/Bb�BZ�BIB4nB1B(BoB
��B
��B
ϫB
��B
��B
�B
cB
p�B
`�B
YKB
IB
:�B
#:B
SB
�B	��B	�BB	�B	��B	�xB	�B	�B	�B	qvB	]/B	B'B	-�B	$tB	B		7B�B�B�B��BϫBŢB�BB��B��B�B��B�@B�B��B��B��B�hB�.B��B��B�1B��B�%B��B��B�B{�B��B}VByrBv�Bp�Bn�Bm]BkQBn�Br�Bt�B|BzDB~�B�B��B~�B{�Bx8Bv�Bu%Bq�Bn/Bm�BncBo�Bm�Bl�Bl�BiBl"Bm�Bl�Bm�Bm)Bk�Bk�BjBh�Bg�BjKBi�Bd�Bd�Bc�Bd&BdZBe`Bd�Bb�Bb�Ba|Bd&B\)B[WB\�B_;Bc�BgBk�Bi�Bj�Bi�BlWBm�Bq�B�B�oB�B��B�B�_B��B��B�B�=B��B�OB�B��B�B�B� BɆBΥB�B�[B՛B��B��BچB��B� B�
B�B�iB�B��B��B��B�>B�xB�B�DB�B��B��B�.B��B	�B	�B	�B	�B	"4B	(�B	-�B	6B	9$B	@�B	B�B	M6B	Q�B	S�B	V9B	[#B	\�B	]dB	c B	g�B	m�B	u�B	xlB	yrB	zxB	.B	�	B	�PB	�PB	��B	�bB	�B	�oB	�B	��B	�1B	�=B	��B	��B	��B	�bB	��B	��B	��B	��B	�B	��B	��B	��B	��B	�OB	��B	��B	��B	�FB	�B	�FB	��B	�B	��B	�RB	��B	��B	�XB	��B	��B	��B	��B	�jB	��B	��B	�'B	��B	�9B	ƨB	��B	�B	�RB	�#B	�XB	��B	��B	��B	�jB	�HB	�}B	ѷB	ҽB	ӏB	ԕB	��B	�mB	�B	ںB	�#B	�WB	��B	ݘB	ޞB	��B	�B	��B	�B	��B	�B	�TB	�B	��B	�B	�`B	�2B	��B	�>B	�B	��B	��B	�B	��B	�B	��B	��B	��B	�]B	�B	�B	��B	�cB	�B	�B	�B	��B	�2B	�fB	�	B	�	B	��B	�8B	�xB	�PB	�B	�PB	��B	�VB	�"B	��B	��B	�(B	�(B	��B	��B	��B
B
B
AB
B
GB
�B
�B
YB
%B
�B
�B
�B
+B
_B
�B
fB
	�B

	B

=B

=B

�B
DB
xB
�B
�B
~B
�B
�B
"B
"B
�B
"B
�B
\B
�B
�B
4B
�B
�B
�B
B
:B
�B
@B
@B
�B
�B
@B
@B
{B
B
{B
{B
�B
�B
FB
{B
FB
�B
�B
B
MB
�B
B
�B
$B
YB
_B
�B
eB
eB
1B
�B
1B
�B
7B
	B
�B
�B
�B
CB
CB
B
�B
xB
�B
CB
xB
�B
�B
�B
�B
~B
B
�B
�B
 \B
 �B
!-B
!�B
"4B
!�B
"4B
"�B
#�B
$@B
$tB
%FB
%�B
%�B
%zB
%FB
%zB
'B
'�B
($B
'�B
'�B
($B
(XB
)_B
(�B
)_B
)�B
)�B
*0B
)�B
)�B
*eB
*eB
*eB
*�B
*�B
*�B
+B
+�B
,�B
,�B
,�B
-CB
-CB
-B
-B
-�B
-�B
-�B
-wB
-�B
.IB
.�B
/B
/�B
/�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BoiBqBpBm]Bm�Bp;Bp�Bo5Bp�Bo�Bn/BpoBp;Bo BpoBp�Bo�Bn�BpoBp�Bo BoiBp�Bn�Bo5Bq�Bo5Bo BqvBo�Bn�Bp;Bo�Bn�BpBp;Bn�Bo�BqvBo5Bo5BpoBp�Bo5Bo�Bp�BpBn�Bo Bp�Bn�Bo�BpoBncBpoBo�BncBo�BpoBn�Bo5BpoBn�BpBqBn/Bn�Bq�Bo�BpBqABo Bn�Bp�Bo5BncBo�Bp�Bn�Bn�Bo BqvBo5Bo�Bp;Bo Bo�Bp�Bm�Bo�BoiBo�Bm�Bm]Bx�BsBp�Bo Bn�Bn�Bp�Bo�BoiBo�Bn/Bm�BqBp;Bm�Bo�Bm�Bm)Bl�Bk�BtBn�BncBh�Bt�Bl�BkQBiyBg8BjB|�Bh�Bt�B[�B\�BXyBUgBVmBW�BiyBhsBl"Bp�BqBo�Bu%Bw�Bw�Bv�By	Bz�B}�B�;B��B�zB�CB�~B��B�LB�XB�B�B�RB�}B��B�9B�B��B��B�wB�}B�mBĜB�HB�aB�jBߤB�BB�ZB�oB��B��B�	B�B��B�>B��B��B�B��B��B0�B(�B+kB.}B2�B1[B1�B4�B6FB6FB8�B9XB9XB>�BAUB@�BA�BC�BEmBOBBL�BH�BL0BI�BM�BO�BMjBK�BL�BS&BR BWsBT�BU�BV�BZ�B`BBh�Bi�Bk�BrGBz�B� B~�B}"B{JB~(B}�B}�B�uB��B��B�YB��B��B��B�B��B��B��B��B��B��B��B�AB�BcB{B��B}�B}"B}�B}"B{B�oB~]B|�By�B{JBz�By�By�B|BzDBy>BzDB{�B~(ByrBzBy�BzDBy>BxlB{BzxBx�B|�B|�B{JB�B��B�uBcB{JB��B�B}�B~�B}VB�B|�Bz�B{B��B}�By�BzB.B��B��B��B��B~�B��B��B��B��B�uB�MB�oB��B��B�B��B�MB��B�{B�B�{B�{B��B��B��B��B�B��B�+B��B�1B��B��B�uB�B�MB��B��B�SB��B�GB�%B��B�{B�B��B��B�GB��B�B��B�B�AB�B�B�iB�4B�B�iB~�B�4B��B~(B}"B|B�AB��B�{B��BzxBt�Bv`By�BtTBv�BpoBo�Bv�Bw�Br�Bo�Bo�BrB�iB�=B�fBzBhsBiyBk�BU�BR�BQ�BO�BQBQBO�BN�BZ�BO�BS&BO�BOBMjBO�BQ�BL0BM�BI�BL0BJXBL0BNBK�BMBIBL0BXBT�BMjBN<BR�BL�BF�BK)BI�BGBFtBE�BEmBFtBA BAUB?�B?�B?HB>�B=<B@�B:�B:�B;�B;�B:�B>B:�B:*B9�B@B=B<�B@B8B6�B4�B2-B:^B1[B/BC�B2aB+6B(XB'�B%�B$B#:B#�B#B �B �B�B"�B�B�B�B1B_B�B�B�B.B�B��B�VB�B�]B�(B�xB��B�DB�B��B�DB�B��B�8B��B��B�`B��B��B�TB��B�JB�B�%B��B�TB��B��B��B�5B��B�lB�B�B�KB��B��B�pB�dB��B�B�EB�EB��B�?B҉B�&B֡B��B��B�B��BʌB�HBƨB�B�-B�aB�tBҽB��B��B�0B��B�wB�B��B��B��B��B�0B��B�6B��B�B��B��B��B��B�$B��B�_B��B��B�{B�B��B��B{B}VB|PBy	B|PBv�Bv+Bu�BuZBv�BtTBqBp;Bu%B�MBo5Bh>BgmBg8Bf�Bd�Bd�Bc�Bb�Ba�BbNBa�B`BBaHBk�Bh
B\�B\�BXBX�BR�BPBR BTaBWsBM�B?HBGEBNpBC�B<BI�BE�B4�B+�B?HB/OB7�B-�B/�B,=B�B �BSB 'B�BB�B�BbB�BVB B�B�B\B
�B1B�BfB;B
��B
��B
��B
��B
��B
��B
��B
��B
�|B
�/B
�B
�]B1'B
�B
�EB
خB
רB
��B
�B
�B
�WB
�B
�B
�mB
�B
��B
��B
�0B
�^B
�KB
��B
�B
��B
��B
�aB
ǮB
��B
�0B
�B
��B
�B
��B
��B
�hB
�=B
��B
��B
�B
�GB
�B
��B
��B
�GB
��B
��B
��B
�iB
�4B
~�B
�4B
�oB
��B
�YB
|�B
zB
x�B
�%B
w2B
zDB
l�B
p;B
oiB
h
B
h
B
e�B
e�B
b�B
a|B
`B
aB
]�B
_;B
]�B
]�B
[�B
\]B
Y�B
Y�B
_�B
\)B
XEB
MjB
L0B
L0B
L0B
H�B
I�B
H�B
E�B
C�B
GzB
K�B
Q�B
7�B
I�B
8B
8�B
:^B
)�B
%FB
#B
#B
"4B
 �B
!�B
8�B
~B
�B
�B
�B
$B
�B
qB
�B
�B
uB
�B
�B
�B
~B
~B
$B
1B	�rB	�`B	��B	��B	�vB	�B	�5B	��B	�B	�B	�B	�,B	�B	��B	��B	�B4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                 44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444BlWBk�Bl�Bk�BlWBlWBk�Bk�Bk�Bl�Bk�BlWBk�Bl"Bl"Bk�Bk�Bk�BlWBk�Bk�Bk�BlWBl"BkBiyBjBh
BZBiBr{B�=B�-B� B��B��B��B�B+�B3�B>BBH�BNBbB{~B�oB}�Bx�Bw1Bv�B|�B{B�@B�B�B�B��B}"Bz�BsBW�BL�BJ#BGEB=pB8B49B&�BB	�B��B��B�iB�fB��B��B�pB�0B��B��Br�BjB^�BV�BEmB0�B�BxB
��B
��B
�>B
��B
��B
��B
~\B
{�B
m(B
\�B
U�B
EmB
7KB
�B
�B
B	�B	ܒB	�QB	�B	��B	�kB	bB	}VB	m�B	YB	>wB	*0B	 �B	kB	�B��B�B��B�B��B��B��B��B�BB�^B��B��B�_B�@B�FB�:B��B�~B�	B��B��B��B�uB�:B~�B}VBxB.By�Bu�BsBl�Bj�Bi�Bg�BkBo BqABxlBv�Bz�B|B|�B{JBxBt�BsBquBm�BjBjBj�Bk�BjJBiBiDBe`BhrBi�BiDBi�BiyBh>Bg�BffBd�Bc�Bf�Bf2BaBaB`AB`vB`�Ba�BaB_B_B]�B`vBXyBW�BYB[�B_�BcTBg�Be�Bg8Be�Bh�Bi�Bn.B}VB}�BbB��B�SB��B��B�B�XB��B�0B��B�[B�B�^B�^B�pB��B��B�dBϫB��B�2B�2B��B�#B�pB�ZB��B�B��B�.B��B�MB��B��B�fB��B��B�B��B�~B��B	�B	B	B	�B	�B	$�B	)�B	2aB	5tB	=B	?B	I�B	M�B	PB	R�B	WsB	YB	Y�B	_pB	c�B	jB	rGB	t�B	u�B	v�B	{~B	�YB	��B	��B	�B	��B	�VB	��B	�hB	�B	��B	��B	�7B	�B	��B	��B	��B	�!B	�OB	�'B	�UB	�B	�LB	�*B	��B	��B	�B	��B	��B	��B	�aB	��B	��B	�gB	��B	��B	�B	�?B	��B	�B	�EB	�EB	�KB	��B	��B	�B	�wB	�B	��B	��B	�,B	�gB	ŢB	�sB	ƨB	�B	�B	�B	ɺB	̘B	��B	�B	�B	��B	��B	� B	ҽB	�mB	�
B	�sB	קB	�KB	��B	��B	�)B	�]B	�/B	�B	�5B	�B	ߤB	�B	�AB	��B	�B	�B	�B	�B	�fB	�2B	�2B	�lB	�B	��B	�JB	�DB	�DB	�B	��B	��B	�B	�B	��B	��B	�oB	�B	�B	�B	�YB	�YB	��B	�B	��B	��B	��B	��B	�>B	��B	�rB	�>B	��B	�xB	�xB	��B	��B	��B	�VB	�\B	��B	�bB	��B
  B
B
�B
uB
�B
�B
�B
{B
�B
B
�B
�B
YB
�B
�B
�B
�B
�B
	B
	7B
�B
	B

	B

rB

rB

�B

rB
B
�B
�B
B
�B
B
B
�B
VB
�B
�B
�B
�B
'B
�B
�B
�B
�B
bB
�B
�B
�B
4B
�B
�B
�B
4B
4B
hB
�B
B
nB
B
tB
�B
�B
LB
�B
�B
�B
LB
�B
B
�B
YB
$B
�B
*B
�B
�B
_B
�B
�B
�B
�B
�B
0B
0B
�B
�B
�B
kB
�B
B
�B
IB
}B
OB
�B
OB
�B
!B
 'B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
#nB
$@B
$tB
$@B
$@B
$tB
$�B
%�B
%FB
%�B
&LB
&LB
&�B
&LB
&LB
&�B
&�B
&�B
&�B
'B
&�B
'RB
'�B
(�B
(�B
)*B
)�B
)�B
)^B
)^B
)�B
)�B
*0B
)�B
)�B
*�B
+B
+kB
+�B
,<G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bk�Bm]BlWBi�BjBl�Bl�Bk�Bl�Bk�BjBl�Bl�BkPBl�Bl�Bl"BkBl�Bl�BkPBk�Bl�Bj�Bk�Bm�Bk�BkPBm�Bl"BkBl�Bl"BkBlWBl�BkBl"Bm�Bk�Bk�Bl�Bm(Bk�Bk�Bl�BlWBj�BkPBm(BkBl"Bl�Bj�Bl�Bk�Bj�Bk�Bl�BkBk�Bl�Bj�BlWBm]BjBj�Bn.Bk�BlWBm�BkPBj�Bl�Bk�Bj�Bk�Bl�Bj�BkBkPBm�Bk�Bk�Bl�BkPBl"Bm(BjBk�Bk�Bk�Bi�Bi�Bt�BoiBl�BkPBkBkBm(Bk�Bk�Bk�BjBi�Bm]Bl�Bi�Bk�BjBiyBiDBh>BpoBkBj�Bd�Bp�BiBg�Be�Bc�Bf�By>Bd�BqBW�BX�BT�BQ�BR�BS�Be�Bd�BhrBl�Bm]Bl"BquBs�Bs�Br�BuYBw1By�B}�B�+B��B��B��B��B��B��B�bB�[B��B��B�B��B�gB�?B�B��B��B��B��B̘BбBںB��BܒB�B�B�B�	B�YB�fB�1B��B�7B�7B�fB�>B�B-B%FB'�B*�B.�B-�B-�B1'B2�B2�B4�B5�B5�B:�B=�B<�B>B@NBA�BK�BIBE9BH�BF?BJ#BL/BI�BG�BH�BOvBNpBS�BQNBQ�BS&BV�B\�Bd�Bf2Bh>Bn�Bw1B|PB{JByrBw�BzxBzBy�B~�B}�B��B��B� B��B�:B�iB�4B~(B�:B��B~�B�4B�B~�B{�B{�Bw�B}�BzDByrBzByrBwfB}�Bz�By>Bu�Bw�Bw1Bu�Bv+BxlBv�Bu�Bv�BxBzxBu�Bv`Bv+Bv�Bu�Bt�Bw�Bv�Bt�By>Bx�Bw�B|B��B~�B{�Bw�B�:BbBzBz�By�B~\Bx�Bv�BwfB~�BzDBv+Bv`B{~B�4B.B�GB��Bz�B~�B~(B�:B~�B~�B��B}�B�4B~�B~\B�4B��B~(B�B�iB�B�B}�B.B� B.B{�B� B�{B��B��B~�B~(B~�B}VB��B�B�B��B~�B�B�uB��B�BbB�B�B�B��B�oB�:B~\B~�B~\B~\B|�B|�B}VB|�Bz�B|�B}"BzxByrBxlB~�B�:B��B�7Bv�Bp�Br�Bv+Bp�BsMBl�Bl"Br�BtBo5Bl"Bl"BncB|�B��B��Bv`Bd�Be�Bg�BR BOBN<BK�BMjBMjBL/BK)BV�BL/BOvBL/BK^BI�BL/BM�BH�BI�BF?BH�BF�BH�BJWBHKBIQBEmBH�BT`BQBI�BJ�BN�BIBB�BGyBF
BCaBB�BB&BA�BB�B=pB=�B<B<B;�B:�B9�B=B6�B7KB8B8B6�B:^B7B6zB6EB<jB9XB8�B<jB4mB2�B0�B.}B6�B-�B+kB@B.�B'�B$�B$@B!�B [B�B 'BUBBIB=B!B0B7B7B�B�BFBB�B~B �B�DB��B �B��B�xB��B��B��B�SB��B��B�`B��B�B�%B��B�B�MB�B�B�AB��B�fB�uB�5B�B�B�JB�DB�B��B��B��B�`B�B�/B�B��BٴB�B�gBԕBԕB�,BӏB��B�vB��B�/B�)B��B� B��B̘B��B�[B�}B��B��B�B�B�#B��B��B��B�mB�'B�?B��B��B��B�B��B��B�kB�7B�$B�B�B�tB��B��B��B�=B��B�kB�:B.BwfBy�Bx�BuYBx�BsBr{Bq�Bq�Br�Bp�Bm]Bl�BquB��Bk�Bd�Bc�Bc�BcBaGBaB_�B_B^B^�B^B\�B]�Bg�BdZBX�BX�BT`BT�BOBBLdBNpBP�BS�BJ#B;�BC�BJ�B?�B8RBF?BA�B0�B'�B;�B+�B3�B)�B,B(�B�B�B�BwBFB\BIB�B�B�B
�BPBBCB�B+B�B�B�B
��B
�DB
��B
�B
�+B
�>B
��B
�AB
�AB
��B
�B
��B
�B-wB
�mB
ԕB
��B
��B
�2B
��B
��B
קB
�B
��B
ҽB
�dB
�#B
�#B
ȀB
ǮB
ěB
�,B
�UB
��B
�B
��B
��B
�EB
��B
�[B
��B
�[B
��B
�=B
��B
��B
�GB
� B
�oB
�B
~\B
.B
�4B
�B
~�B
.B
|�B
|�B
|�B
{B
|�B
}�B
� B
��B
y>B
v`B
u%B
�uB
s�B
v�B
h�B
l�B
k�B
dZB
dZB
a�B
bB
^�B
]�B
\]B
]cB
ZB
[�B
Y�B
ZB
XB
X�B
V8B
VB
[�B
XyB
T�B
I�B
H�B
H�B
H�B
D�B
F
B
E9B
A�B
@B
C�B
G�B
NB
3�B
F?B
4mB
4�B
6�B
&B
!�B
UB
UB
�B
IB
OB
5?B
�B
�B
�B
B
tB
B
�B
B
�B
�B
�B

�B
CB
�B
�B
tB
�B	��B	�B	�AB	�5B	��B	�WB	�B	�DB	��B	��B	��B	�|B	��B	�/B	�#B	ܒ4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                 44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721225006                            20230721225006AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122500620230721225006  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500620230721225006QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500620230721225006QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               