CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:50:11Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  P�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  U�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  j\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  o�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ̀   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  (   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   !0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T '0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   '�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   '�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   '�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   '�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � '�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ($   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   (@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    (H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        (h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        (p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       (x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    (�Argo profile    3.1 1.2 19500101000000  20230721225011  20230721225011  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL              
  
AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�5��t�@�5��t�11  @�5��O�@@�5��O�@@2$�D�"@2$�D�"�d�B���1�d�B���111  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?�=q@�\@=p�@��\@��\@�G�@�G�A ��A��A   A*�HA>�RA`  A�Q�A�Q�A�Q�A��A�\)A�\)A�Q�A�Q�A��B  B  B�
B�B'�
B0  B7�
B@  BG�
BP  BX(�B_�
Bg�
Bo�Bw�
B�{B�{B��B��B�  B��B��B��B�{B�  B��B�  B�  B�{B��
B�  B�{B�{B�{B�{B�{B�(�B�(�B�{B�{B�  B��B��B�  B�(�B�(�B�  C 
=C
=C  C{C
=C	��C  C
=C  C��C  C  C��C��C�C�C 
=C"
=C#��C&  C(
=C*  C,  C-��C/��C1��C4  C6  C8  C:{C<
=C=��C@  CB�CD
=CF
=CH
=CJ
=CL
=CN  CO��CR  CT
=CV
=CX
=CY��C\  C]��C_��Cb{Cd
=Ce�Cg��Cj  Ck��Cm��Co�Cq��Cs�Cu��Cw��Cz  C|
=C~  C�C���C�
=C�C�  C�C�C�  C�  C�  C���C���C�  C���C�  C���C���C�  C�C�  C�C���C�  C���C���C�C�  C�  C�  C�  C�C���C���C�C�
=C�  C�  C���C���C���C�  C���C���C�  C�  C�  C�  C�C�  C���C���C�  C�  C�  C�  C�C���C�  C���C���C���C�  C�  C�  C�C�C�C�C���C���C�C�  C���C�  C���C���C�  C�  C�  C�C�C�  C�
=C�C���C�C�C���C�C�C�C�  C���C���C���C�  C���C���C���C���C�  C�
=C�  C���C���C�  C�C�C�C���C�  C�  C�  C�C�  C�  C�  C�  C���C�  C���C���C�  C�C���C�  C�  C�D �D ��D  D� D�D��D�D� D  D� D�D��DD��D  D}qD�qD}qD	  D	��D
  D
}qD
�qD� D  Dz�D��D}qD�D��D  D}qD��D}qD�D�D�qDz�D  D��D�qD� D  D}qD  D� D�qD}qD�qD� D�D� D  D� D�D��D�qD}qD��D��D�D��DD� D   D ��D!  D!}qD!�qD"��D"�qD#��D$  D$� D%�D%��D&  D&}qD'  D'�D(�D(��D)�D)}qD*�D*��D+D+�D,D,��D,�qD-z�D.  D.}qD/  D/��D0�D0� D0�qD1� D2  D2}qD3�D3}qD4  D4� D5�D5��D6�D6��D6�qD7}qD7�qD8}qD9�D9� D:�D:� D;  D;��D<�D<��D=  D=� D>�D>� D>��D?}qD@  D@}qDA  DA�DB  DB}qDC  DC}qDC�qDDz�DE  DE�DF�DF� DG  DG� DH  DH��DI�DI� DJ  DJz�DJ��DK� DL  DL� DM  DM� DN�DN� DN�qDO}qDO�qDP� DP�qDQ}qDQ�qDR� DR�qDS� DT  DT� DU  DU� DV  DV� DWDW�DX�DX��DY�DY�DZ�DZ� D[�D[��D\  D\� D]�D]� D^  D^� D_  D_� D`  D`}qD`�qDa}qDa�qDbz�Db�qDc� Dd  Dd� De  De� DfDf�Df�qDg}qDg�qDh}qDh�qDi}qDi�qDj��Dk  Dk}qDl  Dl� Dm  Dm}qDn�Dn��Do  Do��Dp  Dp}qDq�Dq��Dq��Dr� DsDs}qDt  Dt��Du�Du� Du�qDv� Dv�qDwz�Dw�qDx��Dx�qDy}qDz  Dz� D{�D{� D|�D|��D}  D}z�D}��D~}qD  D�D�HD�AHD�~�D��HD�  D�@ D�~�D���D�HD�AHD��HD�� D�  D�>�D�� D��HD���D�>�D�� D�� D���D�@ D��HD��HD�  D�AHD�� D�� D�  D�>�D�� D��HD�  D�@ D�� D���D�  D�B�D��HD�� D�  D�@ D�� D���D�  D�>�D��HD��HD��qD�=qD�� D�D���D�@ D���D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?\)?W
=?�\)?�Q�?�p�?�@�\@z�@0��@333@L��@\(�@s33@��\@��@�z�@�G�@�=q@��@��R@�ff@�\)@�(�@�G�@�{@�
=@��RAA
=qA�RA33AffA(�A   A#�
A(Q�A+�A0  A4z�A7
=A<(�AA�ADz�AH��AN{AS33AUA\(�A^�RAe�AhQ�Amp�Aq�Atz�Az=qA\)A���A��A�A�\)A��\A�(�A�{A���A��A���A��RA�Q�A�33A���A��RA���A��HA�z�A�
=A�Q�A�=qA���A�{A��A�=qA�z�A�A�Q�A��A��A�ffA���A��A���A�\)Aȣ�A˅A�{AϮAҏ\A��AָRA��A��
A�A��A�=qA��A�RA�G�A�A��A�A�=qA�A��RA�Q�A��\A��A��RB ��BB�HBQ�B�B�\B�
B��B	�B\)B(�Bp�B�HB�
B�B�RB\)B��B{B33B��B��B�HBz�BG�B�RB (�B ��B"�\B$  B%�B&=qB'�B)�B)�B+33B,��B-p�B.�RB0Q�B1G�B2ffB3�
B4��B5�B7\)B8  B9��B:ffB;�B=�B>{B?33B@��BABB�HBDQ�BE�BF�RBH  BH��BJ=qBK�BL��BM�BO33BP(�BQBR�HBS�BUG�BV�\BW�BX��BZ=qB[
=B\(�B]B^�RB_�
BaG�Bb{Bc�Bd��BeBg\)Bh��Bip�Bj�RBlQ�BmG�BnffBo�
Bp��Bq�Bs\)Btz�BuG�Bv�HBx  Bx��Bz{B{\)B|(�B}p�B~�RB�B�Q�B�
=B��B��B��\B�33B���B�(�B��HB�33B�B�z�B��HB�p�B�(�B�z�B�33B��
B�(�B���B��B��B��RB�p�B�B�z�B�33B���B�=qB�
=B��B�{B��RB�p�B��B�z�B�
=B��
B�Q�B���B��B�(�B��\B�33B��B�z�B��HB��B�ffB��HB�\)B�{B���B�33B��
B��\B��B���B�ffB���B�p�B�=qB���B�G�B�  B��RB��B��B�ffB�
=B�p�B�(�B��HB�G�B�  B��RB�33B�B��\B��B���B�ffB�
=B���B�(�B���B���B�{B���B���B�{B��RB�p�B�{B��\B�\)B�{B���B��B��
B��\B�
=B���B�Q�B�
=B�p�B�{B���B�G�B��
B\B�33BîB�ffB���BŅB�=qB���B�\)B�{BȸRB�33B�Bʏ\B�33BˮB�=qB���BͮB�(�BθRB�p�B�(�BиRB�33B��BҸRB��B��
Bԏ\B�
=BՅB�=qB���BׅB�  BظRB�p�B�  B�z�B��B��
B�z�B���Bݙ�B�Q�B�
=B�p�B�{B���B�33B��
B��B��B㙚B�Q�B�
=B噚B�  B�RB�\)B��
B�Q�B��B�B�=qB�RB�B�(�B�\B�33B��B�Q�B�
=B�B�{B�RB�p�B��B�ffB�33B�B�=qB��HB���B�  B���B�\)B��
B�=qB�
=B�B�Q�B���B�\)B�(�B���B��B��
B�z�B�
=B��C (�C z�C �C ��C\)C��C�HC(�C�C�
C{CQ�C��C��CG�C�CC�Cz�C�RC��CG�C��C�HC�Cp�C�
C
=CQ�C��C��C	G�C	�C	C
(�C
\)C
�C{C\)C�\C��CG�C�C��C33Cp�C�RC{CffC��C�C=qC��C�
C{CffCC  C=qC��C�C�C�C��C
=Cp�C�RC��CQ�C�C�C33C��C��C(�C�CC
=Cp�C�C��C\)C�\C�
C=qCz�C��C(�CffC�C{C\)C��C{CQ�C��C  CG�C�C��CG�C�C��CG�C�\C�
C 33C �\C �HC!�C!z�C!�HC"�C"ffC"�
C#{C#\)C#�RC${C$Q�C$��C%
=C%ffC%��C%��C&\)C&�C&��C'\)C'C({C(\)C(��C)
=C)p�C)�C*
=C*p�C*��C+{C+p�C+�C,(�C,�C,�C-G�C-�\C-��C.\)C.��C/
=C/p�C/��C0
=C0z�C0�HC133C1�C1��C233C2��C2��C333C3�\C3��C4Q�C4�\C4�HC5Q�C5�C5��C6G�C6�C7{C7\)C7�C8{C8z�C8�
C9�C9p�C9�
C:33C:�\C:�
C;(�C;��C;��C<33C<z�C<�C=G�C=��C=��C>=qC>�C?
=C?Q�C?��C@  C@ffC@�RCA  CAQ�CA�RCB{CB\)CB��CC  CCQ�CC��CD  CDG�CD�\CD��CEG�CE�CE�
CF33CF�\CF��CG{CGz�CG�
CH�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                               1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�=q@�\@=p�@��\@��\@�G�@�G�A ��A��A   A*�HA>�RA`  A�Q�A�Q�A�Q�A��A�\)A�\)A�Q�A�Q�A��B  B  B�
B�B'�
B0  B7�
B@  BG�
BP  BX(�B_�
Bg�
Bo�Bw�
B�{B�{B��B��B�  B��B��B��B�{B�  B��B�  B�  B�{B��
B�  B�{B�{B�{B�{B�{B�(�B�(�B�{B�{B�  B��B��B�  B�(�B�(�B�  C 
=C
=C  C{C
=C	��C  C
=C  C��C  C  C��C��C�C�C 
=C"
=C#��C&  C(
=C*  C,  C-��C/��C1��C4  C6  C8  C:{C<
=C=��C@  CB�CD
=CF
=CH
=CJ
=CL
=CN  CO��CR  CT
=CV
=CX
=CY��C\  C]��C_��Cb{Cd
=Ce�Cg��Cj  Ck��Cm��Co�Cq��Cs�Cu��Cw��Cz  C|
=C~  C�C���C�
=C�C�  C�C�C�  C�  C�  C���C���C�  C���C�  C���C���C�  C�C�  C�C���C�  C���C���C�C�  C�  C�  C�  C�C���C���C�C�
=C�  C�  C���C���C���C�  C���C���C�  C�  C�  C�  C�C�  C���C���C�  C�  C�  C�  C�C���C�  C���C���C���C�  C�  C�  C�C�C�C�C���C���C�C�  C���C�  C���C���C�  C�  C�  C�C�C�  C�
=C�C���C�C�C���C�C�C�C�  C���C���C���C�  C���C���C���C���C�  C�
=C�  C���C���C�  C�C�C�C���C�  C�  C�  C�C�  C�  C�  C�  C���C�  C���C���C�  C�C���C�  C�  C�D �D ��D  D� D�D��D�D� D  D� D�D��DD��D  D}qD�qD}qD	  D	��D
  D
}qD
�qD� D  Dz�D��D}qD�D��D  D}qD��D}qD�D�D�qDz�D  D��D�qD� D  D}qD  D� D�qD}qD�qD� D�D� D  D� D�D��D�qD}qD��D��D�D��DD� D   D ��D!  D!}qD!�qD"��D"�qD#��D$  D$� D%�D%��D&  D&}qD'  D'�D(�D(��D)�D)}qD*�D*��D+D+�D,D,��D,�qD-z�D.  D.}qD/  D/��D0�D0� D0�qD1� D2  D2}qD3�D3}qD4  D4� D5�D5��D6�D6��D6�qD7}qD7�qD8}qD9�D9� D:�D:� D;  D;��D<�D<��D=  D=� D>�D>� D>��D?}qD@  D@}qDA  DA�DB  DB}qDC  DC}qDC�qDDz�DE  DE�DF�DF� DG  DG� DH  DH��DI�DI� DJ  DJz�DJ��DK� DL  DL� DM  DM� DN�DN� DN�qDO}qDO�qDP� DP�qDQ}qDQ�qDR� DR�qDS� DT  DT� DU  DU� DV  DV� DWDW�DX�DX��DY�DY�DZ�DZ� D[�D[��D\  D\� D]�D]� D^  D^� D_  D_� D`  D`}qD`�qDa}qDa�qDbz�Db�qDc� Dd  Dd� De  De� DfDf�Df�qDg}qDg�qDh}qDh�qDi}qDi�qDj��Dk  Dk}qDl  Dl� Dm  Dm}qDn�Dn��Do  Do��Dp  Dp}qDq�Dq��Dq��Dr� DsDs}qDt  Dt��Du�Du� Du�qDv� Dv�qDwz�Dw�qDx��Dx�qDy}qDz  Dz� D{�D{� D|�D|��D}  D}z�D}��D~}qD  D�D�HD�AHD�~�D��HD�  D�@ D�~�D���D�HD�AHD��HD�� D�  D�>�D�� D��HD���D�>�D�� D�� D���D�@ D��HD��HD�  D�AHD�� D�� D�  D�>�D�� D��HD�  D�@ D�� D���D�  D�B�D��HD�� D�  D�@ D�� D���D�  D�>�D��HD��HD��qD�=qD�� D�D���D�@ D���D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?\)?W
=?�\)?�Q�?�p�?�@�\@z�@0��@333@L��@\(�@s33@��\@��@�z�@�G�@�=q@��@��R@�ff@�\)@�(�@�G�@�{@�
=@��RAA
=qA�RA33AffA(�A   A#�
A(Q�A+�A0  A4z�A7
=A<(�AA�ADz�AH��AN{AS33AUA\(�A^�RAe�AhQ�Amp�Aq�Atz�Az=qA\)A���A��A�A�\)A��\A�(�A�{A���A��A���A��RA�Q�A�33A���A��RA���A��HA�z�A�
=A�Q�A�=qA���A�{A��A�=qA�z�A�A�Q�A��A��A�ffA���A��A���A�\)Aȣ�A˅A�{AϮAҏ\A��AָRA��A��
A�A��A�=qA��A�RA�G�A�A��A�A�=qA�A��RA�Q�A��\A��A��RB ��BB�HBQ�B�B�\B�
B��B	�B\)B(�Bp�B�HB�
B�B�RB\)B��B{B33B��B��B�HBz�BG�B�RB (�B ��B"�\B$  B%�B&=qB'�B)�B)�B+33B,��B-p�B.�RB0Q�B1G�B2ffB3�
B4��B5�B7\)B8  B9��B:ffB;�B=�B>{B?33B@��BABB�HBDQ�BE�BF�RBH  BH��BJ=qBK�BL��BM�BO33BP(�BQBR�HBS�BUG�BV�\BW�BX��BZ=qB[
=B\(�B]B^�RB_�
BaG�Bb{Bc�Bd��BeBg\)Bh��Bip�Bj�RBlQ�BmG�BnffBo�
Bp��Bq�Bs\)Btz�BuG�Bv�HBx  Bx��Bz{B{\)B|(�B}p�B~�RB�B�Q�B�
=B��B��B��\B�33B���B�(�B��HB�33B�B�z�B��HB�p�B�(�B�z�B�33B��
B�(�B���B��B��B��RB�p�B�B�z�B�33B���B�=qB�
=B��B�{B��RB�p�B��B�z�B�
=B��
B�Q�B���B��B�(�B��\B�33B��B�z�B��HB��B�ffB��HB�\)B�{B���B�33B��
B��\B��B���B�ffB���B�p�B�=qB���B�G�B�  B��RB��B��B�ffB�
=B�p�B�(�B��HB�G�B�  B��RB�33B�B��\B��B���B�ffB�
=B���B�(�B���B���B�{B���B���B�{B��RB�p�B�{B��\B�\)B�{B���B��B��
B��\B�
=B���B�Q�B�
=B�p�B�{B���B�G�B��
B\B�33BîB�ffB���BŅB�=qB���B�\)B�{BȸRB�33B�Bʏ\B�33BˮB�=qB���BͮB�(�BθRB�p�B�(�BиRB�33B��BҸRB��B��
Bԏ\B�
=BՅB�=qB���BׅB�  BظRB�p�B�  B�z�B��B��
B�z�B���Bݙ�B�Q�B�
=B�p�B�{B���B�33B��
B��B��B㙚B�Q�B�
=B噚B�  B�RB�\)B��
B�Q�B��B�B�=qB�RB�B�(�B�\B�33B��B�Q�B�
=B�B�{B�RB�p�B��B�ffB�33B�B�=qB��HB���B�  B���B�\)B��
B�=qB�
=B�B�Q�B���B�\)B�(�B���B��B��
B�z�B�
=B��C (�C z�C �C ��C\)C��C�HC(�C�C�
C{CQ�C��C��CG�C�CC�Cz�C�RC��CG�C��C�HC�Cp�C�
C
=CQ�C��C��C	G�C	�C	C
(�C
\)C
�C{C\)C�\C��CG�C�C��C33Cp�C�RC{CffC��C�C=qC��C�
C{CffCC  C=qC��C�C�C�C��C
=Cp�C�RC��CQ�C�C�C33C��C��C(�C�CC
=Cp�C�C��C\)C�\C�
C=qCz�C��C(�CffC�C{C\)C��C{CQ�C��C  CG�C�C��CG�C�C��CG�C�\C�
C 33C �\C �HC!�C!z�C!�HC"�C"ffC"�
C#{C#\)C#�RC${C$Q�C$��C%
=C%ffC%��C%��C&\)C&�C&��C'\)C'C({C(\)C(��C)
=C)p�C)�C*
=C*p�C*��C+{C+p�C+�C,(�C,�C,�C-G�C-�\C-��C.\)C.��C/
=C/p�C/��C0
=C0z�C0�HC133C1�C1��C233C2��C2��C333C3�\C3��C4Q�C4�\C4�HC5Q�C5�C5��C6G�C6�C7{C7\)C7�C8{C8z�C8�
C9�C9p�C9�
C:33C:�\C:�
C;(�C;��C;��C<33C<z�C<�C=G�C=��C=��C>=qC>�C?
=C?Q�C?��C@  C@ffC@�RCA  CAQ�CA�RCB{CB\)CB��CC  CCQ�CC��CD  CDG�CD�\CD��CEG�CE�CE�
CF33CF�\CF��CG{CGz�CG�
CH�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                               1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�A�A�A�A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�?}A�A�A�A�A�A�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�E�A�E�A�G�A�G�A�G�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�E�A�7LA� �A���A�ĜA���AԋDA�I�A���A�A�ȴA��A��A��
AЋDAН�A���A�  A�Q�A�p�A��A�M�A��A̧�A�ȴA�-A�A��HA��`A�A�r�AǗ�A�S�A�9XA�t�A���A�^5A�A���A��hA��jA�n�A�ȴA�&�A�r�A�ĜA���A��9A�9XA�{A���A��A�r�A���A�M�A��#A���A�(�A��RA�+A�  A�z�A�9XA�ĜA�A�/A���A�O�A�1'A��A�l�A�9XA��mA�;dA�5?A�1A���A��A�+A��#A���A�O�A�/A���A��HA���A���A�G�A���A��`A�VA�hsA�wA~��A}|�A|bNAx��Aw�wAt��Aq��Ap  Ak�-Af��AbbNA_��A^VA\ĜAX�AV��AU`BAShsAQ�APAN��AMƨALI�AJ��AH��AF��AC�#AAA@�DA?A<��A9�mA7��A6~�A4bNA3�A3hsA2z�A1oA-|�A+��A*ffA(�!A&�A&�A&E�A%G�A#��A"bNA"bA!At�AA�AhsA�wA��A5?A��A�
A�7A�AbNA�uA��A
��A
A{Av�Ax�A��AZA �A��A��A�-A?}AoA Ĝ@�t�@�V@�O�@���@��@���@��7@�Z@�1@��
@��F@�|�@�5?@�t�@�"�@�o@��@��@�%@�ƨ@@�(�@�C�@�J@�A�@�t�@��H@��@���@�b@�@�"�@���@�Ĝ@�K�@���@�7L@�%@���@�z�@�@އ+@���@���@�`B@ڟ�@׍P@Ցh@�X@�5?@�X@�  @�ȴ@ԋD@��`@���@� �@Ӿw@�t�@�t�@�K�@�\)@�I�@��@�b@�~�@��@��@�"�@�ȴ@Χ�@�v�@�5?@�V@�v�@ͺ^@͑h@�1@�C�@��@ʰ!@ɲ-@�x�@�O�@��@�j@�b@ǝ�@�\)@�33@��H@��@�hs@��`@ă@��m@�+@�J@�`B@�1@��@��#@��#@��@���@��@��T@���@�X@��@�j@�C�@���@���@�^5@���@��@�ȴ@���@�ff@�{@���@�?}@��`@��9@�z�@��m@��w@��w@���@���@��@��y@�@���@��-@�x�@�G�@��@�Ĝ@��@�  @�t�@�dZ@�\)@�C�@��@���@��@���@�X@�bN@�b@�1@���@��m@��
@�ƨ@�t�@�\)@�33@��@�ff@�J@��-@���@�G�@��@�1'@��w@�t�@�l�@�;d@���@�V@��@���@���@�x�@�G�@���@���@��9@��@�z�@�  @��@��@�C�@��@��R@���@�V@�-@��@���@�p�@��@��/@�Z@��m@�S�@��@��!@���@���@�~�@�^5@�=q@�{@���@�x�@�V@��D@�z�@��
@��P@�dZ@�K�@�;d@�o@��@��!@�M�@�-@�$�@��T@��@���@�Z@��@�C�@��!@�v�@�5?@�J@�x�@��@��9@�A�@� �@� �@���@��@��@�$�@��#@���@�X@�%@���@��D@��@�I�@��@�33@�+@�"�@��@�o@���@���@�5?@�@��h@�7L@��@�V@�%@��@���@�Q�@�1@��m@��m@��m@�ƨ@��w@��P@�S�@��y@��+@���@���@��7@��7@�hs@�?}@��@��9@��u@�z�@�Z@�9X@��@��w@���@�l�@�C�@�33@�o@��y@��R@���@�~�@�^5@�=q@��T@��7@�?}@�%@�Ĝ@��u@�1'@�b@��@���@���@��@�S�@�"�@��y@���@�~�@�n�@�5?@�{@��-@��7@���@�Ĝ@�Ĝ@��9@��D@�(�@���@�"�@��H@��@��R@�V@�$�@��@��-@��h@��7@�`B@��@��@���@�Ĝ@��u@�Q�@�1'@��@�;@~��@~�@~�R@~v�@~{@}��@}?}@|�@|�D@{�@z��@zM�@y��@y�7@y%@x��@xbN@x �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�?}A�C�A�A�A�A�A�A�A�?}A�A�A�C�A�=qA�C�A�A�A�=qA�A�A�A�A�=qA�A�A�=qA�=qA�A�A�=qA�=qA�A�A�=qA�C�A�?}A�?}A�C�A�?}A�A�A�C�A�?}A�C�A�?}A�A�A�A�A�=qA�C�A�C�A�=qA�C�A�E�A�?}A�?}A�C�A�A�A�?}A�C�A�?}A�E�A�?}A�A�A�E�A�?}A�C�A�E�A�?}A�A�A�C�A�?}A�E�A�A�A�A�A�E�A�?}A�E�A�E�A�A�A�E�A�C�A�A�A�E�A�A�A�A�A�G�A�A�A�A�A�E�A�A�A�?}A�E�A�E�A�A�A�G�A�C�A�C�A�G�A�E�A�A�A�G�A�G�A�C�A�E�A�G�A�?}A�C�A�C�A�A�A�E�A�C�A�A�A�C�A�?}A�G�A�A�A�C�A�G�A�A�A�E�A�E�A�?}A�E�A�C�A�C�A�E�A�A�A�G�A�C�A�C�A�G�A�C�A�E�A�G�A�C�A�E�A�I�A�C�A�E�A�I�A�E�A�G�A�I�A�C�A�I�A�E�A�E�A�I�A�E�A�G�A�I�A�E�A�I�A�I�A�E�A�I�A�K�A�G�A�G�A�K�A�M�A�G�A�I�A�M�A�G�A�I�A�M�A�I�A�K�A�K�A�I�A�K�A�M�A�I�A�O�A�I�A�K�A�O�A�I�A�I�A�M�A�K�A�I�A�O�A�I�A�M�A�M�A�I�A�K�A�M�A�I�A�K�A�K�A�I�A�M�A�K�A�I�A�M�A�O�A�I�A�I�A�M�A�K�A�I�A�O�A�K�A�K�A�O�A�K�A�M�A�O�A�K�A�O�A�O�A�K�A�K�A�Q�A�M�A�K�A�Q�A�M�A�K�A�O�A�O�A�K�A�O�A�G�A�5?A�9XA�?}A�33A�1'A�?}A�7LA�5?A�=qA�33A�1'A�1'A�+A��A� �A��A�{A��A�bA�A�A���Aش9A�n�A�bNA��A�t�A�+A�ĜA�5?AՃA�|�A�M�A��HAԸRAԲ-AԲ-Aԥ�Aԝ�Aԙ�Aԕ�AԋDAԇ+A�v�A�v�A�n�A�\)A�K�A�=qA�7LA�7LA�33A�"�A�{A�%A�A��A�ȴAӍPA�^5A�Aҟ�A�z�A�ZA�M�A�$�A�%A��A���AѓuA�n�A�ZA�C�A�"�A�JA�VA�1A���A���A��yA��HA��`A���A���A���A��A��A��HA���A�Aв-AУ�AН�AБhAЁA�z�A�|�AЁAЇ+AН�AЧ�AХ�AЬAв-A���A��A���A��HAв-AЕ�A�~�A�O�A�"�A���A�ĜAϴ9Aϧ�Aϟ�AϮAϝ�A�C�A��A���AξwAΧ�A�p�A�dZA�^5A�K�A�=qA�7LA�1'A�$�A��A�bA���A��
A͸RA�hsA�+A�bA�A�A���A��#A���A���A���A���A���A�ȴA̼jA̬A̝�ÁA�v�A�bNA�oA�ĜA˛�A�n�A�hsA�Q�A�?}A�-A�+A�(�A��A�{A�VA�%A�  A���A�  A���A���A���A��A��mA��#A���Aʺ^Aʥ�AʓuA�=qAɛ�A�dZA�;dA�/A��A�bA�
=A�A�A��A���A�ȴAȺ^Aȗ�A�l�A�?}A�(�A�A��TA���AǺ^AǍPA�^5A�1'A�%Aư!A�z�A�dZA�=qA���Aũ�Ař�AŃA�ffA�G�A�VA���Aĥ�Aĕ�AċDA�~�A�p�A�^5A�Q�A�;dA�+A��A�%A��mA���AüjAé�Aá�AÅA�hsA�O�A�33A�%A��A��A¼jA�A�n�A�bNA�ZA�;dA�{A�A���A��A�ȴA�ĜA���A��^A��jA��^A�p�A�1A��#A��
A���A���A��9A���A���A��uA��DA�z�A�l�A�XA�K�A�"�A��/A�z�A�7LA�r�A��wA�ZA��wA�M�A�"�A�oA�
=A�  A��TA�ĜA��A�~�A�^5A�5?A�&�A��A��A�
=A��#A�~�A�n�A�A�A��A�A��A���A��A�Q�A�-A���A�`BA��\A��#A�x�A�ĜA��DA�5?A�33A�(�A�A��-A�-A���A�JA���A�|�A�n�A�E�A��A���A���A��DA��DA��DA��7A��DA��A�x�A�r�A�K�A�33A�E�A��A�  A��/A���A�dZA��A�bNA�9XA��A��A���A�v�A�oA���A��;A��hA�|�A��A�r�A�bNA�bNA�=qA��A�%A��A���A�r�A�O�A�1'A��A�A���A��HA���A��-A���A���A���A��A�hsA�/A�bA��A���A�1'A�ĜA��#A� �A��A�A���A�v�A�jA�bNA�\)A�Q�A�I�A�A�A�7LA�-A� �A��A���A��
A���A���A�5?A��A��
A��RA���A���A��A�dZA�C�A�"�A�VA�  A��yA��RA���A���A��PA��A�~�A�v�A�n�A�hsA�Q�A� �A���A���A�~�A�VA�{A��#A���A�hsA�E�A���A���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                               1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�A�A�A�A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�?}A�A�A�A�A�A�A�C�A�C�A�C�A�E�A�C�A�C�A�C�A�E�A�E�A�G�A�G�A�G�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�E�A�7LA� �A���A�ĜA���AԋDA�I�A���A�A�ȴA��A��A��
AЋDAН�A���A�  A�Q�A�p�A��A�M�A��A̧�A�ȴA�-A�A��HA��`A�A�r�AǗ�A�S�A�9XA�t�A���A�^5A�A���A��hA��jA�n�A�ȴA�&�A�r�A�ĜA���A��9A�9XA�{A���A��A�r�A���A�M�A��#A���A�(�A��RA�+A�  A�z�A�9XA�ĜA�A�/A���A�O�A�1'A��A�l�A�9XA��mA�;dA�5?A�1A���A��A�+A��#A���A�O�A�/A���A��HA���A���A�G�A���A��`A�VA�hsA�wA~��A}|�A|bNAx��Aw�wAt��Aq��Ap  Ak�-Af��AbbNA_��A^VA\ĜAX�AV��AU`BAShsAQ�APAN��AMƨALI�AJ��AH��AF��AC�#AAA@�DA?A<��A9�mA7��A6~�A4bNA3�A3hsA2z�A1oA-|�A+��A*ffA(�!A&�A&�A&E�A%G�A#��A"bNA"bA!At�AA�AhsA�wA��A5?A��A�
A�7A�AbNA�uA��A
��A
A{Av�Ax�A��AZA �A��A��A�-A?}AoA Ĝ@�t�@�V@�O�@���@��@���@��7@�Z@�1@��
@��F@�|�@�5?@�t�@�"�@�o@��@��@�%@�ƨ@@�(�@�C�@�J@�A�@�t�@��H@��@���@�b@�@�"�@���@�Ĝ@�K�@���@�7L@�%@���@�z�@�@އ+@���@���@�`B@ڟ�@׍P@Ցh@�X@�5?@�X@�  @�ȴ@ԋD@��`@���@� �@Ӿw@�t�@�t�@�K�@�\)@�I�@��@�b@�~�@��@��@�"�@�ȴ@Χ�@�v�@�5?@�V@�v�@ͺ^@͑h@�1@�C�@��@ʰ!@ɲ-@�x�@�O�@��@�j@�b@ǝ�@�\)@�33@��H@��@�hs@��`@ă@��m@�+@�J@�`B@�1@��@��#@��#@��@���@��@��T@���@�X@��@�j@�C�@���@���@�^5@���@��@�ȴ@���@�ff@�{@���@�?}@��`@��9@�z�@��m@��w@��w@���@���@��@��y@�@���@��-@�x�@�G�@��@�Ĝ@��@�  @�t�@�dZ@�\)@�C�@��@���@��@���@�X@�bN@�b@�1@���@��m@��
@�ƨ@�t�@�\)@�33@��@�ff@�J@��-@���@�G�@��@�1'@��w@�t�@�l�@�;d@���@�V@��@���@���@�x�@�G�@���@���@��9@��@�z�@�  @��@��@�C�@��@��R@���@�V@�-@��@���@�p�@��@��/@�Z@��m@�S�@��@��!@���@���@�~�@�^5@�=q@�{@���@�x�@�V@��D@�z�@��
@��P@�dZ@�K�@�;d@�o@��@��!@�M�@�-@�$�@��T@��@���@�Z@��@�C�@��!@�v�@�5?@�J@�x�@��@��9@�A�@� �@� �@���@��@��@�$�@��#@���@�X@�%@���@��D@��@�I�@��@�33@�+@�"�@��@�o@���@���@�5?@�@��h@�7L@��@�V@�%@��@���@�Q�@�1@��m@��m@��m@�ƨ@��w@��P@�S�@��y@��+@���@���@��7@��7@�hs@�?}@��@��9@��u@�z�@�Z@�9X@��@��w@���@�l�@�C�@�33@�o@��y@��R@���@�~�@�^5@�=q@��T@��7@�?}@�%@�Ĝ@��u@�1'@�b@��@���@���@��@�S�@�"�@��y@���@�~�@�n�@�5?@�{@��-@��7@���@�Ĝ@�Ĝ@��9@��D@�(�@���@�"�@��H@��@��R@�V@�$�@��@��-@��h@��7@�`B@��@��@���@�Ĝ@��u@�Q�@�1'@��@�;@~��@~�@~�R@~v�@~{@}��@}?}@|�@|�D@{�@z��@zM�@y��@y�7@y%@x��@xbN@x �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�?}A�C�A�A�A�A�A�A�A�?}A�A�A�C�A�=qA�C�A�A�A�=qA�A�A�A�A�=qA�A�A�=qA�=qA�A�A�=qA�=qA�A�A�=qA�C�A�?}A�?}A�C�A�?}A�A�A�C�A�?}A�C�A�?}A�A�A�A�A�=qA�C�A�C�A�=qA�C�A�E�A�?}A�?}A�C�A�A�A�?}A�C�A�?}A�E�A�?}A�A�A�E�A�?}A�C�A�E�A�?}A�A�A�C�A�?}A�E�A�A�A�A�A�E�A�?}A�E�A�E�A�A�A�E�A�C�A�A�A�E�A�A�A�A�A�G�A�A�A�A�A�E�A�A�A�?}A�E�A�E�A�A�A�G�A�C�A�C�A�G�A�E�A�A�A�G�A�G�A�C�A�E�A�G�A�?}A�C�A�C�A�A�A�E�A�C�A�A�A�C�A�?}A�G�A�A�A�C�A�G�A�A�A�E�A�E�A�?}A�E�A�C�A�C�A�E�A�A�A�G�A�C�A�C�A�G�A�C�A�E�A�G�A�C�A�E�A�I�A�C�A�E�A�I�A�E�A�G�A�I�A�C�A�I�A�E�A�E�A�I�A�E�A�G�A�I�A�E�A�I�A�I�A�E�A�I�A�K�A�G�A�G�A�K�A�M�A�G�A�I�A�M�A�G�A�I�A�M�A�I�A�K�A�K�A�I�A�K�A�M�A�I�A�O�A�I�A�K�A�O�A�I�A�I�A�M�A�K�A�I�A�O�A�I�A�M�A�M�A�I�A�K�A�M�A�I�A�K�A�K�A�I�A�M�A�K�A�I�A�M�A�O�A�I�A�I�A�M�A�K�A�I�A�O�A�K�A�K�A�O�A�K�A�M�A�O�A�K�A�O�A�O�A�K�A�K�A�Q�A�M�A�K�A�Q�A�M�A�K�A�O�A�O�A�K�A�O�A�G�A�5?A�9XA�?}A�33A�1'A�?}A�7LA�5?A�=qA�33A�1'A�1'A�+A��A� �A��A�{A��A�bA�A�A���Aش9A�n�A�bNA��A�t�A�+A�ĜA�5?AՃA�|�A�M�A��HAԸRAԲ-AԲ-Aԥ�Aԝ�Aԙ�Aԕ�AԋDAԇ+A�v�A�v�A�n�A�\)A�K�A�=qA�7LA�7LA�33A�"�A�{A�%A�A��A�ȴAӍPA�^5A�Aҟ�A�z�A�ZA�M�A�$�A�%A��A���AѓuA�n�A�ZA�C�A�"�A�JA�VA�1A���A���A��yA��HA��`A���A���A���A��A��A��HA���A�Aв-AУ�AН�AБhAЁA�z�A�|�AЁAЇ+AН�AЧ�AХ�AЬAв-A���A��A���A��HAв-AЕ�A�~�A�O�A�"�A���A�ĜAϴ9Aϧ�Aϟ�AϮAϝ�A�C�A��A���AξwAΧ�A�p�A�dZA�^5A�K�A�=qA�7LA�1'A�$�A��A�bA���A��
A͸RA�hsA�+A�bA�A�A���A��#A���A���A���A���A���A�ȴA̼jA̬A̝�ÁA�v�A�bNA�oA�ĜA˛�A�n�A�hsA�Q�A�?}A�-A�+A�(�A��A�{A�VA�%A�  A���A�  A���A���A���A��A��mA��#A���Aʺ^Aʥ�AʓuA�=qAɛ�A�dZA�;dA�/A��A�bA�
=A�A�A��A���A�ȴAȺ^Aȗ�A�l�A�?}A�(�A�A��TA���AǺ^AǍPA�^5A�1'A�%Aư!A�z�A�dZA�=qA���Aũ�Ař�AŃA�ffA�G�A�VA���Aĥ�Aĕ�AċDA�~�A�p�A�^5A�Q�A�;dA�+A��A�%A��mA���AüjAé�Aá�AÅA�hsA�O�A�33A�%A��A��A¼jA�A�n�A�bNA�ZA�;dA�{A�A���A��A�ȴA�ĜA���A��^A��jA��^A�p�A�1A��#A��
A���A���A��9A���A���A��uA��DA�z�A�l�A�XA�K�A�"�A��/A�z�A�7LA�r�A��wA�ZA��wA�M�A�"�A�oA�
=A�  A��TA�ĜA��A�~�A�^5A�5?A�&�A��A��A�
=A��#A�~�A�n�A�A�A��A�A��A���A��A�Q�A�-A���A�`BA��\A��#A�x�A�ĜA��DA�5?A�33A�(�A�A��-A�-A���A�JA���A�|�A�n�A�E�A��A���A���A��DA��DA��DA��7A��DA��A�x�A�r�A�K�A�33A�E�A��A�  A��/A���A�dZA��A�bNA�9XA��A��A���A�v�A�oA���A��;A��hA�|�A��A�r�A�bNA�bNA�=qA��A�%A��A���A�r�A�O�A�1'A��A�A���A��HA���A��-A���A���A���A��A�hsA�/A�bA��A���A�1'A�ĜA��#A� �A��A�A���A�v�A�jA�bNA�\)A�Q�A�I�A�A�A�7LA�-A� �A��A���A��
A���A���A�5?A��A��
A��RA���A���A��A�dZA�C�A�"�A�VA�  A��yA��RA���A���A��PA��A�~�A�v�A�n�A�hsA�Q�A� �A���A���A�~�A�VA�{A��#A���A�hsA�E�A���A���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                               1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��B
��B
�hB
�B
�'B
��B
�hB
��B
�9B
��B
��B
��B
��B
��B
��B
��B
��B
�hB
��B
��B
��B
�hB
�3B
��B
�hB
�hB
�3B
��B
�hB
�3B
�hB
�hB
��B
��B
��B
��B
��B
��B
�0B
�fB��B��B��BɺB�B�B�B�dB�B��B
=B!-BOBBT�BM�BN<BS&Bf�Bt�B{B��B�MB��B�nB��B��B��B��BݘB�B��B�WB��B��B��B��B�]B�NB�B�qB�B�}B��B��B\�BXyBI�B?�BDgB8�B0UB+B!�B7BB.B�B�B��B��B��BچB�[B�pB��B�jB�B��B�VB�Bz�BjBb�BG�B:�B/�B(�B�B
��B
ǮB
��B
�6B
��B
xlB
e,B
Z�B
TaB
J#B
C�B
@OB
49B
/�B
B
�B
�B	�B	��B	�'B	��B	��B	v+B	kB	`�B	S&B	AUB	:*B	1�B	&LB	�B	�B	�B	VB	�B�B�B�B��BҽBʌB�-B��B��B��B�@B��B�VB�xB�_B�B�PB�B�JB��B��B�MB�;B�GB�fB�rB��B�=B��B��B��B��B��B�4B�B{�BxBr�Bp;Bj�Be�B^5BYBX�BV9BT�BTaBU�BU2BT�B[WBaHBc Bd�BhsBrBr�Bv`BxBzxB{B|�B�B�;B�B��B�%B��B�uB��B�B��B��B�~B��B�@B��B��B��B�XB�RB�$B��B��B�B�[B�<B�?B��BںB�BںB��BܒB�2B� B�HB�B�B��B��B�B��B�B��B��B��B�VB	$B	%�B	%�B	(�B	,qB	4�B	6B	6FB	8B	FB	T�B	T,B	Q�B	Q�B	UgB	Y�B	[�B	`vB	d�B	e�B	h�B	p�B	s�B	sB	qB	m�B	qAB	s�B	v�B	v�B	v�B	zDB	|B	~�B	~�B	�B	�4B	�B	��B	�oB	�uB	��B	��B	�SB	�YB	�B	��B	�B	}"B	� B	�oB	�B	��B	�xB	�B	��B	��B	�:B	��B	�B	�$B	��B	�kB	��B	��B	�B	��B	�FB	��B	��B	��B	�0B	��B	��B	�B	�B	�B	�B	�aB	�9B	�B	��B	��B	�B	��B	��B	�$B	��B	��B	��B	��B	�6B	��B	�qB	��B	��B	��B	��B	�-B	B	��B	��B	��B	�-B	��B	ŢB	�B	�#B	��B	��B	ϫB	ѷB	��B	�2B	��B	�B	��B	��B	�)B	��B	ߤB	�HB	��B	�B	�B	�NB	�B	�B	�B	�B	�NB	�B	�B	�,B	��B	��B	�`B	��B	��B	�B	��B	�2B	�fB	�fB	��B	��B	�mB	�B	�WB	��B	�cB	�/B	�B	�B	��B	� B	�B	�B	�B	��B	�B	��B	�+B	�`B	��B	��B	��B	�2B	�fB	�B	�8B	�lB	��B	��B	�	B	��B	��B	�xB	�PB	�"B	��B	�(B	��B	�cB	��B	�.B
 4B
  B
  B
 �B
 iB
oB
�B
B
�B
B
�B
�B
�B
SB
�B
�B
_B
_B
+B
+B
+B
_B
�B
fB
fB
	lB
	�B
	�B

=B
DB
�B
B
�B
B
B
PB
�B
�B
�B
�B
�B
�B
�B
 B
hB
hB
hB
B
:B
@B
{B
{B
FB
�B
{B
�B
�B
�B
�B
B
�B
�B
�B
�B
$B
YB
�B
�B
�B
1B
eB
�B
�B
�B
CB
�B
�B
B
IB
IB
B
�B
B
CB
�B
�B
�B
�B
�B
�B
~B
�B
�B
�B
�B
!B
!B
�B
�B
�B
�B
 'B
 'B
 �B
!�B
!�B
!�B
!�B
!�B
"4B
"�B
#nB
#nB
$@B
$tB
$�B
%FB
%B
%B
&LB
'B
'�B
'�B
(�B
(XB
($B
(�B
(�B
)�B
*�B
*�B
+B
+�B
+kB
+6G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�ALJ�ALK^B
�?B
��B
�hB
��B
��B
�B
�9B
��B
�?B
��B
�hB
�B
�3B
�hB
�B
��B
�B
�nB
��B
�B
��B
�-B
��B
��B
��B
�9B
��B
��B
��B
�3B
��B
��B
�nB
�nB
��B
�nB
��B
�hB
�nB
�3B
�3B
�nB
��B
��B
�3B
�nB
��B
��B
��B
�B
��B
��B
��B
��B
��B
�B
�hB
��B
�nB
��B
�B
�9B
��B
��B
��B
�hB
��B
�-B
��B
�nB
�aB
�3B
�9B
�aB
�9B
�9B
�aB
��B
��B
��B
��B
�nB
��B
�B
�9B
�aB
��B
��B
��B
�3B
��B
��B
��B
��B
��B
��B
�B
�-B
�3B
�hB
��B
�9B
��B
�B
�hB
��B
�9B
��B
�aB
�9B
�-B
��B
�hB
��B
�nB
�-B
��B
��B
�-B
�9B
��B
��B
�9B
�B
�aB
�9B
�B
��B
�9B
��B
��B
�nB
�aB
��B
�B
�aB
�nB
��B
��B
�nB
��B
��B
��B
��B
�-B
��B
�9B
��B
�-B
��B
�hB
��B
�nB
��B
��B
��B
�3B
��B
�9B
�3B
�aB
�nB
��B
��B
�hB
��B
��B
�9B
�-B
�3B
�B
�-B
��B
�aB
��B
�nB
��B
��B
��B
�hB
�3B
�nB
�aB
�3B
�nB
��B
�aB
��B
�nB
��B
�nB
�B
�-B
�3B
�hB
��B
��B
�aB
�-B
��B
��B
��B
�B
��B
��B
�3B
��B
�-B
�3B
��B
��B
��B
��B
�-B
�?B
��B
�B
�B
�[B
ÖB
�B
�B
��B
�qB
ÖB
�'B
��B
�B
��B
��B
�B
�TB
ѷB
��B
چB
ںB
�dB
�PB7�B@�BZ�B�B��B�HB�tB�$B�B��B��B�B�B�B��B��B�UB� BÖB�'B�B�3B�B��BɆB�)B��BʌB��B��B�6B͟B�BҽB�EB�?B�B�B�2B��B�EB�?B�tB�'B��B�mB�RBɺBĜB�B��B˒B�RB�NB�B�mB��B�yB��B�sB��B�PB��B��B��B �B �BBYBYB	7BJB~BBB�BB'�B)*B+�B)�B2�BD�B^B]�B`vB`vBbB`�BX�BY�BQ�BF�BFBC�BD�BO�BX�BR�BS�BO�BM�BQNBK^BK^BN�BO�BM�BOvBR BS[BVBXBZ�B`BpoBhsBi�Bh>Bf�BjBw�Bv�Bu�Bw2BxBwfBw2BxB{B~]B�iB�4B�B�bB�+B�%B�iB~�B��B�uB�{B��B��B��B�lB�~B��B�B��B�+B�7B��B�4B�B��B�tB�tB��B��B�eB�*B��B�B�'B��B��B��B��B�wB��B��B��B� B��BĜB�XBȴBȴB�vB��B��B��B��B��B�TB��B��BٴB�
B��B�)B�,B��B�B��B�cB�AB�B�cB��B�B��B��B��B�)B��B�/B��B�B�B��B��B�DB�B�WB� B�iB�B��B�vB�B�B��B��B��B��B�8B�B��B�B�TB�B��B�;B�B�5B�iB��B��B��B��B�BںBںB�sB�2BҽB��B� B� B�HB�^B�pB՛B��BɆBܒBÖB��B�3B��B�*B��B��B�B�kB�wB�=B�'B��B�OB�IB�}B�=B�B�3B��B�B��B�nB��B��B��B�+B��B�"B��B�+B�BgmBpoBw�B\]BZQBTaBT�BVmBgmB_;Bd�Bf2BS�BI�BGzBR BK�BFBI�BD�BB'BB[BB[B@�B>BB?HB=<BD3Bx�BD3B8RB6�B8�B8�B5�BH�B?}B0�B49B.}B.�B=qB8�B'�B+�B+�B($BE�B'RB �B�B%�B$@B�B�B%�B�BB7B�B_B�B�BB�B�B B�BhB�B�B�BB{B�BPB �B
=B��B��B�`B��B�oB�/B�B�)B�)B�WB�B�B�KB�B�;B��B�2B�B�%B�B�5B��B��B�/B�/B��B��B�)BרBԕBچBیBҽBбB��BбB�vBϫB��B̘B�pB�NB��B�KB��B��BÖB�aB��B��B�^B��B��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                               4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  A���B
� B
��B
�UB
�wB
� B
��B
��B
��B
� B
� B
� B
��B
� B
��B
��B
� B
��B
��B
�OB
�OB
��B
��B
��B
��B
��B
��B
�OB
��B
��B
��B
��B
��B
�B
�OB
�OB
��B
��B
ȀB
�B�B�B�HB�
B�dB�jB�[BȴB�cB��B�B}BK�BP�BI�BJ�BOvBcBp�Bw�B�@B��B��B��B��B�0B�,B�)B��B��B�DB�B�JB�;B�B�.BحB͞B�gB��B�kB��B��B~�BX�BT�BF?B<6B@�B5?B,�B'RBOB�BbB~B	7B�]B�>B�>B�/B��BϫB��B�,B��B�RB�=B��BbBw1BffB_;BD3B7B,<B$�B
�.B
�<B
��B
�0B
��B
�IB
t�B
a|B
W
B
P�B
FsB
@B
<�B
0�B
+�B
_B
B
B	�B	�B	�wB	�*B	�MB	r{B	glB	]/B	OvB	=�B	6zB	.B	"�B	�B	*B	�B	
�B	GB�lB�iB��B�,B�B��B�}B��B��B�@B��B�B��B��B��B�bB��B�_B��B��B�B��B}�B�B��B��B�B��B��B}�B�4B�:B�4B|�B{�Bx7BtSBo Bl�Bg8BbNBZ�BU�BU2BR�BP�BP�BQ�BQ�BP�BW�B]�B_pBaGBd�BncBo5Br�BtSBv�BwfBx�B|B}�B�iB��B�uB��B��B�4B�nB��B�7B��B�!B��B�9B�B�LB��B��B�tB�*B��B�^B��B��BӏB�>B�
B�`B�
B�B��B�B�pBݘB��B��B�(B�>B��B�%B�B��B��B�1B��B	tB	!�B	"3B	%FB	(�B	1'B	2aB	2�B	4mB	B[B	QB	P|B	M�B	M�B	Q�B	VB	W�B	\�B	`�B	a�B	d�B	l�B	pB	oiB	m]B	jJB	m�B	pB	r�B	r�B	sB	v�B	xlB	{B	{B	|B	|�B	}VB	|�B	}�B	~�B	}�B	��B	��B	��B	�oB	� B	}VB	yrB	|PB	}�B	�iB	�B	��B	�eB	�B	��B	��B	�B	�\B	�tB	�LB	��B	�IB	�OB	�UB	��B	��B	�B	��B	�B	��B	��B	�0B	�dB	�dB	�dB	�dB	��B	��B	�[B	�9B	��B	�aB	��B	��B	�tB	�B	�B	�B	��B	��B	�#B	��B	�0B	��B	��B	�B	�}B	��B	�B	�HB	�B	�}B	�HB	��B	�gB	�sB	�B	�#B	��B	�B	�BB	тB	�&B	�gB	�2B	�>B	�yB	�#B	��B	ݘB	�5B	�iB	�iB	ޞB	�B	�B	�iB	��B	ޞB	��B	��B	�|B	�B	�B	�B	�B	�B	��B	�NB	�B	�B	�B	�B	�B	�B	��B	�B	�JB	�B	�B	��B	��B	�B	�PB	��B	��B	�]B	�;B	�oB	�;B	�{B	�B	�B	�B	�MB	�B	�B	�SB	�B	��B	�B	��B	�YB	�+B	�+B	��B	��B	�rB	��B	�xB	�DB	��B	�JB	�~B	��B	�PB	�PB	��B	��B	��B
 4B	�bB
 4B
 iB
:B
�B
B
�B
GB
MB
�B
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
eB
	�B
	kB
	kB
	�B

�B

�B

�B
B
IB
�B
CB
PB
�B
�B
�B
VB
�B
�B
�B
�B
�B
�B
�B
�B
B
:B
:B
nB
:B
�B
B
@B
tB
�B
�B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
eB
�B
�B
eB
�B
_B
�B
�B
*B
�B
�B
�B
*B
�B
�B
=B
�B
�B
qB
qB
�B
�B
�B
B
wB
wB
�B
B
�B
�B
OB
OB
�B
�B
�B
�B
 �B
 �B
 �B
!�B
!bB
!bB
"�B
#nB
$B
$@B
$�B
$�B
$tB
%B
%B
&B
&�B
'B
'RB
($B
'�B
'�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AL;�AL<�B
��B
��B
��B
�'B
�OB
�UB
��B
�B
��B
��B
��B
�[B
��B
��B
�[B
�B
�UB
��B
�IB
�UB
� B
�}B
�'B
�B
� B
��B
��B
��B
��B
��B
�'B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�OB
��B
��B
�B
�'B
��B
�[B
��B
�B
�'B
��B
��B
�[B
��B
�OB
��B
��B
�UB
��B
��B
��B
�B
��B
�'B
�}B
��B
��B
��B
��B
��B
��B
��B
��B
��B
� B
�'B
�OB
�B
��B
��B
�UB
��B
��B
�B
��B
�OB
��B
��B
��B
�B
��B
�B
��B
�UB
�}B
��B
��B
�B
��B
�IB
�UB
��B
�IB
��B
�B
��B
��B
�}B
��B
��B
�OB
��B
�}B
� B
� B
�}B
��B
�OB
��B
��B
�UB
��B
��B
�UB
��B
��B
� B
��B
��B
��B
��B
�UB
��B
��B
�OB
��B
��B
�OB
��B
��B
��B
�}B
��B
��B
�B
�}B
� B
��B
�IB
��B
��B
�IB
��B
��B
�B
��B
��B
��B
��B
�B
� B
��B
�IB
� B
��B
�}B
��B
�UB
�}B
��B
��B
�OB
��B
� B
�IB
� B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�B
��B
�UB
�}B
��B
��B
�B
��B
��B
�}B
��B
��B
��B
�UB
� B
�IB
��B
��B
�}B
��B
��B
�IB
��B
� B
�}B
��B
��B
�dB
�RB
��B
��B
�XB
�dB
�B
��B
��B
�wB
�B
�aB
�BB
�B
�WB
ΤB
�B
�B
��B
�
B
ٴB
��B4B=BV�B�UB�9B��B��B�tB�hB��B�?B�XB�XB�RB��B��B��B�pB��B�wB�[B��B�[B�B��B�yB�B��B�B�KBɆB��B�WB�BԕBӏB�`B�BтB�?BÕBB��B�wB�B��BŢB�
B��B�[B�?B��BŢB͞B�TBҽB�&B��B�2B��B��B��B�B��B�JB��B�"BoB�B�B�B�B�B	kBhB@B_B#�B%zB'�B&LB/OBA BZQBY�B\�B\�B^iB\�BU2BVBNBC,BB[B?�B@�BK�BT�BOBBO�BL/BI�BM�BG�BG�BJ�BK�BJ#BK�BNpBO�BRTBT`BV�B\]Bl�Bd�Be�Bd�BcBffBs�BsMBrGBs�BtSBs�Bs�BtSBwfBz�B|�B|�B�iB��B�{B�uB|�B{B}�B~�B�B.B.B��B��B��B�!B�hB�B�{B��B�CB��B�UB��B��B��B��B��B��B�zB�BB�^B�wB�#B�B��B��B��B�)B��B�B�pB�BB��BƨB�B�B��B�#B�EB�B�<B�BBΤB�B�B�B�ZB�EB�yB�|B�B��B�DB�B�B�cB�B�>B�B�B�B�B�yB�B�B�B�
B�lB�2B�2B�B�
B�B�PB�B�]B�GB��B��B�oB��B�GB�;B�(B�B��B�;B��B�B� B�(B�B��B�B�B��B�NB�B�B�iB�
B�
B��BтB�B�5B�pB�pB̘BǮB��B��B�?B��B��B��B�B��B�B�zB��B�B�nB��B��B��B�wB�B��B��B��B��B�dB��B��B�nB�B��B�B�B��B�{B��B�rB�B�{B�kBc�Bl�Bs�BX�BV�BP�BQNBR�Bc�B[�B`�Bb�BO�BF?BC�BNpBHBB[BF?B@�B>wB>�B>�B<�B:�B;�B9�B@�Bt�B@�B4�B2�B5?B4�B2-BD�B;�B-B0�B*�B+B9�B4�B#�B($B($B$tBB&B#�B�B=B!�B �B7B�B"3B=B_B�B�B�BBBnBB�BPBIB�B�B�B	�B_B�B�B	�BB�B�B�B�B�5B�B�B��B�yB�yB�B�lB�lB�B�fB�B�NB�B�cB�uB��BڅB�B�B�B�B�B�B�yB��B��B��B��B�B�B�<B�B��B��B�B��B��B͞B�#BěB�B�&B��B��B�&B�HB��B�#B�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                               4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721225011                            20230721225011AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122501120230721225011  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122501120230721225011QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122501120230721225011QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             