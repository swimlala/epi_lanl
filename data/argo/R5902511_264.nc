CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:50:08Z creation      
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
resolution        =���   axis      Z        h  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  SH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  Y$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  vh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` 1�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   2   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   >   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   Dd   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   Dl   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   Dt   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   D|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � D�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   E   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   E    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    E(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        EH   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        EP   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       EX   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    E`Argo profile    3.1 1.2 19500101000000  20230721225008  20230721225008  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�3<��"Q@�3<��"Q11  @�3<�W @�3<�W @2h�q��@2h�q���d�IQ����d�IQ���11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 BA  BA  FF  ?��@   @B�\@z�H@��R@�G�@�  A ��A�A!G�A+�A?\)A`��A�Q�A�Q�A�Q�A�\)A��A�Q�A�Q�A�  B   B�
B  B(�B   B(  B0  B8  B@  BG�BO�BW�B_�
BhQ�Bpz�Bx(�B�  B�  B��B��B��B�  B�  B��B��B�  B��B�  B�  B�  B�(�B�  B��
B�  B�{B��B�  B�  B�  B��B��B�  B�  B�{B�(�B�{B�{B��B��C  C  C��C��C
  C��C��C��C  C{C{C��C  C��C  C {C"  C#��C&  C'�C)��C,  C.  C/�C2  C4
=C6{C8
=C:
=C<  C>  C@  CA��CC��CF
=CH{CJ�CL{CN
=CP  CR  CT  CV{CX
=CZ  C\{C]��C_�Ca��Cc��Ce��Cg��Cj  Cl
=Cn
=Cp  Cq��Ct{Cv  Cw�
Cy�C{��C~  C�\C�  C�C�  C�C�  C�  C���C���C�  C���C���C���C���C�  C�  C���C�  C���C���C���C���C�  C�C�  C���C�  C���C�  C�  C���C���C���C���C�  C�  C���C���C�  C�
=C�C�C�C���C���C�C�C�C�  C���C�  C�  C�  C���C���C�C�
=C�  C�C���C���C���C���C���C�  C�C�  C�  C�C���C�  C���C�  C�  C�C���C���C���C�  C���C�  C���C���C���C�C�  C�C���C�C�  C�  C�  C���C�C�C�  C���C���C���C���C�  C�C�  C�  C�  C�  C�  C�  C���C���C�C�  C�  C���C���C���C��C���C�  C�C�  C�C�  C�  C�  C�  C���C�  D   D ��D�D}qD�qD}qD  D��D�D}qD�qD��D  D}qD  D� D�qD� D	�D	�D
�D
}qD
��D}qD  D��DD�D�qD}qD�D� D��D}qD  D� D�D� DD��D�qD}qD  D� D  D��D�D��DD�D�D� D  D� D  D}qD�D�D  D� D  D}qD�D}qD�qD }qD �qD!� D!��D"� D"�qD#� D$�D$��D%  D%}qD&  D&��D'D'��D(�D(� D(�qD)� D*  D*� D*�qD+}qD+�qD,}qD-  D-� D-�qD.}qD.�qD/}qD0  D0� D1�D1��D2  D2�D3�D3�D3�qD4xRD4�qD5��D6  D6� D7  D7� D7��D8z�D8��D9}qD9�qD:z�D:��D;z�D;�qD<��D=D=��D>  D>� D>�qD?z�D?�qD@� D@�qDAz�DA��DB}qDC  DCz�DD  DD� DE�DE��DF�DF}qDF�qDG��DH�DH}qDI  DI��DJ  DJ� DK�DK��DL�DL� DM  DM� DM��DNxRDO  DO��DO�qDPz�DP��DQ� DR  DR� DR�qDS}qDT  DT��DUDU��DV  DV}qDW  DW}qDX�DX�DY  DY� DZ�DZ}qDZ�qD[� D\  D\� D]�D]}qD^  D^��D_�D_� D`  D`� Da�Da� Da��Db}qDb�qDc��Dc�qDd� De  De� De�qDfz�Df�qDg��DhDh��Di  Di}qDi�qDj�Dk�Dk��Dl�Dl��Dm  Dm� Dn  Dn� Do�Do��Do�qDpz�Dp��Dq}qDr  Dr}qDs�Ds��Ds��Dtz�Dt�RDuz�Dv  Dv� Dw�Dw�Dx�Dx}qDx�qDy� Dz  Dz}qDz��D{}qD|�D|� D}  D}� D}�qD~� D  D��D�  D�@ D�~�D���D�HD�AHD�~�D���D�HD�AHD�� D���D���D�@ D�� D��qD���D�>�D�~�D�� D�HD�AHD���D��HD���D�>�D�� D���D���D�AHD��HD��HD�  D�>�D�� D��HD�  D�AHD�� D��HD���D�=qD�~�D�D���D�=qD��HD�� D��qD�@ D�� D�D��D�B�D���D�� D���D�@ D�~�D�� D�HD�@ D�� D��HD�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�>�D�� D���D���D�@ D�~�D���D���D�@ D�� D�� D�  D�@ D��HD�� D�  D�>�D�� D��HD�HD�@ D��HD�D�HD�@ D�� D�� D�  D�@ D��HD��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?.{?L��?k�?�z�?��R?���?�
=?�@��@
=@(��@=p�@E�@\(�@c�
@z�H@�G�@��@�z�@���@��
@���@�z�@���@\@���@У�@�p�@�\@���@�z�@�(�A�
AA
�HAp�A�
AffA�HA\)A!�A'�A)��A/\)A333A5A;�A>�RAA�AG
=AI��AN�RAR�\AUAZ�HA]p�Ac33Ae�Aj�HAn�RAq�Aw
=Ay��A\)A���A��\A�A�
=A�G�A�33A���A��A���A�33A�p�A�
=A���A�33A���A��RA���A�=qA���A�ffA�  A��\A��A�ffA�\)A�=qA��
A�p�A�Q�A���A��
A�A�\)A�=qAÅA�{AǮA��A�z�A�A�  A��A�(�A�ffA׮A��AۅA�A�Q�AᙚA��
A�{A�\)A��A�A�p�A�  A���A��HA�p�A�\)A��A��HA�A�\)B z�B�BffB�B��Bp�B�RB\)B��B	��B
=qB�BQ�BG�BffB
=Bz�BG�B�B33B  B��B=qB�RB  B��B��B�HB\)B��B��B=qB�B   B!�B"=qB"�HB#�
B$��B%p�B&�HB'�B(z�B)B*=qB+\)B,Q�B,��B.{B/33B/�B0��B2{B2�RB3�B4��B5p�B6�\B7�B8(�B9�B:=qB:�HB;�
B<��B=B>=qB?�B@(�BAG�BB=qBB�HBD  BD��BE��BF�RBG\)BHQ�BIp�BJ{BK
=BL  BLz�BM��BN�\BO33BPQ�BQp�BR{BS33BTQ�BT��BV{BW
=BW�BX��BYBZ�RB\  B\z�B]B^�HB_�B`z�Ba��Bb�RBc\)Bdz�Be��Bf{Bg�BhQ�Bi�Bj=qBk\)Bl  BmG�Bn=qBn�HBp  Bp��Bq��Br�HBs�BtQ�BuBv�RBw\)Bx��By�BzffB{�B|��B}��B~�\B�
B�(�B���B�33B���B�=qB���B�G�B���B�=qB�z�B��B�B�{B��\B��B�p�B��
B�z�B�
=B�G�B�B�ffB���B��B��B�  B���B���B�G�B��
B�Q�B��\B��B��B�  B�z�B��B�G�B�B�Q�B���B��B��B�(�B�ffB�
=B�p�B��
B�ffB���B�\)B�B�ffB��HB�33B��
B�=qB���B�G�B���B�{B��RB�
=B�p�B�{B�z�B��HB��B��
B�ffB���B�G�B��B�ffB��RB�G�B��
B�=qB��\B�G�B�B�(�B��\B�33B��B��B��\B�
=B�\)B�{B�z�B���B��B��B�Q�B��RB�\)B��
B�(�B��RB�G�B���B�(�B��RB�
=B��B�{B�ffB��HB�p�B��B�=qB���B�p�B�B�=qB��HB�\)B��B�=qB��HB�G�B��B�(�B���B�\)B��B�=qB��HB�33B�B�ffB���B�33B��B�ffB���B�p�B�  B�Q�B��HBÅB��B�z�B��Bř�B��BƏ\B��BǙ�B�  Bȣ�B�33BɮB�  BʸRB�G�B˙�B�(�B���B�33BͮB�Q�BθRB��B�B�Q�BУ�B�G�B�B�(�BҸRB�G�BӅB�(�BԸRB��BՅB�  BָRB��BׅB�(�Bأ�B�
=BٮB�(�Bڏ\B��B�B�=qBܣ�B�
=BݮB�Q�Bޣ�B�33B��
B�=qB�RB�G�B��B�Q�B���B�B�{B�ffB���B噚B�{B�ffB�
=B�B�{B��B�G�B陚B�(�B��HB�p�B�B�=qB�
=B�B��B��B�G�BB�=qB���B�B��B�z�B�G�B�B�(�B��HB�p�B��B�ffB��B�B�{B��HB�p�B��
B�Q�B��HB��B�(�B��\B��B�B�Q�B��RB�\)B��C (�C p�C ��C  C=qC��C�HC�Cz�C�C�CQ�C�\C��C(�Cz�C�RC{CffC��C��CQ�C�\C�HC33Cp�C�C{C\)C��C	  C	Q�C	�\C	�C
G�C
z�C
�
C=qC�CC33Cp�C�RC�CffC�C{C\)C��C  C\)C��C��CQ�C�\C�HCG�C�C��C33Cz�CC33Cz�C�RC{Cp�CC  C\)C�C��CG�C��C��C=qC�C�C=qCp�C�
C(�Cp�C�RC
=CffC�C��CG�C��C�C(�Cz�C�HC=qCz�C��C(�C�\C�
C(�C��C�
C �C �C �
C!{C!�C!�HC"�C"p�C"�
C#(�C#ffC#C$33C$z�C$�RC%{C%z�C%�RC&
=C&ffC&C'
=C'Q�C'�RC({C(Q�C(��C)
=C)\)C)��C)�HC*G�C*��C*�HC+33C+��C+��C,=qC,�C,�
C-=qC-��C-�HC.33C.�\C.�C/(�C/z�C/�HC0=qC0z�C0��C133C1�\C1��C2{C2z�C2�
C3{C3\)C3�RC4{C4p�C4�RC5  C5\)C5�RC6{C6\)C6��C6��C7\)C7�C7�C8=qC8��C8��C9=qC9z�C9�HC:=qC:�C:C;�C;z�C;��C<
=C<ffC<C={C=Q�C=��C=��C>Q�C>��C>�HC?(�C?z�C?�
C@33C@�C@CA
=CAffCACB{CBG�CB�CC
=CCQ�CC��CC��CDQ�CD��CD�CE33CEz�CE�CFG�CF�\CF��CG{CGp�CG�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333333333333333                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?��@   @B�\@z�H@��R@�G�@�  A ��A�A!G�A+�A?\)A`��A�Q�A�Q�A�Q�A�\)A��A�Q�A�Q�A�  B   B�
B  B(�B   B(  B0  B8  B@  BG�BO�BW�B_�
BhQ�Bpz�Bx(�B�  B�  B��B��B��B�  B�  B��B��B�  B��B�  B�  B�  B�(�B�  B��
B�  B�{B��B�  B�  B�  B��B��B�  B�  B�{B�(�B�{B�{B��B��C  C  C��C��C
  C��C��C��C  C{C{C��C  C��C  C {C"  C#��C&  C'�C)��C,  C.  C/�C2  C4
=C6{C8
=C:
=C<  C>  C@  CA��CC��CF
=CH{CJ�CL{CN
=CP  CR  CT  CV{CX
=CZ  C\{C]��C_�Ca��Cc��Ce��Cg��Cj  Cl
=Cn
=Cp  Cq��Ct{Cv  Cw�
Cy�C{��C~  C�\C�  C�C�  C�C�  C�  C���C���C�  C���C���C���C���C�  C�  C���C�  C���C���C���C���C�  C�C�  C���C�  C���C�  C�  C���C���C���C���C�  C�  C���C���C�  C�
=C�C�C�C���C���C�C�C�C�  C���C�  C�  C�  C���C���C�C�
=C�  C�C���C���C���C���C���C�  C�C�  C�  C�C���C�  C���C�  C�  C�C���C���C���C�  C���C�  C���C���C���C�C�  C�C���C�C�  C�  C�  C���C�C�C�  C���C���C���C���C�  C�C�  C�  C�  C�  C�  C�  C���C���C�C�  C�  C���C���C���C��C���C�  C�C�  C�C�  C�  C�  C�  C���C�  D   D ��D�D}qD�qD}qD  D��D�D}qD�qD��D  D}qD  D� D�qD� D	�D	�D
�D
}qD
��D}qD  D��DD�D�qD}qD�D� D��D}qD  D� D�D� DD��D�qD}qD  D� D  D��D�D��DD�D�D� D  D� D  D}qD�D�D  D� D  D}qD�D}qD�qD }qD �qD!� D!��D"� D"�qD#� D$�D$��D%  D%}qD&  D&��D'D'��D(�D(� D(�qD)� D*  D*� D*�qD+}qD+�qD,}qD-  D-� D-�qD.}qD.�qD/}qD0  D0� D1�D1��D2  D2�D3�D3�D3�qD4xRD4�qD5��D6  D6� D7  D7� D7��D8z�D8��D9}qD9�qD:z�D:��D;z�D;�qD<��D=D=��D>  D>� D>�qD?z�D?�qD@� D@�qDAz�DA��DB}qDC  DCz�DD  DD� DE�DE��DF�DF}qDF�qDG��DH�DH}qDI  DI��DJ  DJ� DK�DK��DL�DL� DM  DM� DM��DNxRDO  DO��DO�qDPz�DP��DQ� DR  DR� DR�qDS}qDT  DT��DUDU��DV  DV}qDW  DW}qDX�DX�DY  DY� DZ�DZ}qDZ�qD[� D\  D\� D]�D]}qD^  D^��D_�D_� D`  D`� Da�Da� Da��Db}qDb�qDc��Dc�qDd� De  De� De�qDfz�Df�qDg��DhDh��Di  Di}qDi�qDj�Dk�Dk��Dl�Dl��Dm  Dm� Dn  Dn� Do�Do��Do�qDpz�Dp��Dq}qDr  Dr}qDs�Ds��Ds��Dtz�Dt�RDuz�Dv  Dv� Dw�Dw�Dx�Dx}qDx�qDy� Dz  Dz}qDz��D{}qD|�D|� D}  D}� D}�qD~� D  D��D�  D�@ D�~�D���D�HD�AHD�~�D���D�HD�AHD�� D���D���D�@ D�� D��qD���D�>�D�~�D�� D�HD�AHD���D��HD���D�>�D�� D���D���D�AHD��HD��HD�  D�>�D�� D��HD�  D�AHD�� D��HD���D�=qD�~�D�D���D�=qD��HD�� D��qD�@ D�� D�D��D�B�D���D�� D���D�@ D�~�D�� D�HD�@ D�� D��HD�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�>�D�� D���D���D�@ D�~�D���D���D�@ D�� D�� D�  D�@ D��HD�� D�  D�>�D�� D��HD�HD�@ D��HD�D�HD�@ D�� D�� D�  D�@ D��HD��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?.{?L��?k�?�z�?��R?���?�
=?�@��@
=@(��@=p�@E�@\(�@c�
@z�H@�G�@��@�z�@���@��
@���@�z�@���@\@���@У�@�p�@�\@���@�z�@�(�A�
AA
�HAp�A�
AffA�HA\)A!�A'�A)��A/\)A333A5A;�A>�RAA�AG
=AI��AN�RAR�\AUAZ�HA]p�Ac33Ae�Aj�HAn�RAq�Aw
=Ay��A\)A���A��\A�A�
=A�G�A�33A���A��A���A�33A�p�A�
=A���A�33A���A��RA���A�=qA���A�ffA�  A��\A��A�ffA�\)A�=qA��
A�p�A�Q�A���A��
A�A�\)A�=qAÅA�{AǮA��A�z�A�A�  A��A�(�A�ffA׮A��AۅA�A�Q�AᙚA��
A�{A�\)A��A�A�p�A�  A���A��HA�p�A�\)A��A��HA�A�\)B z�B�BffB�B��Bp�B�RB\)B��B	��B
=qB�BQ�BG�BffB
=Bz�BG�B�B33B  B��B=qB�RB  B��B��B�HB\)B��B��B=qB�B   B!�B"=qB"�HB#�
B$��B%p�B&�HB'�B(z�B)B*=qB+\)B,Q�B,��B.{B/33B/�B0��B2{B2�RB3�B4��B5p�B6�\B7�B8(�B9�B:=qB:�HB;�
B<��B=B>=qB?�B@(�BAG�BB=qBB�HBD  BD��BE��BF�RBG\)BHQ�BIp�BJ{BK
=BL  BLz�BM��BN�\BO33BPQ�BQp�BR{BS33BTQ�BT��BV{BW
=BW�BX��BYBZ�RB\  B\z�B]B^�HB_�B`z�Ba��Bb�RBc\)Bdz�Be��Bf{Bg�BhQ�Bi�Bj=qBk\)Bl  BmG�Bn=qBn�HBp  Bp��Bq��Br�HBs�BtQ�BuBv�RBw\)Bx��By�BzffB{�B|��B}��B~�\B�
B�(�B���B�33B���B�=qB���B�G�B���B�=qB�z�B��B�B�{B��\B��B�p�B��
B�z�B�
=B�G�B�B�ffB���B��B��B�  B���B���B�G�B��
B�Q�B��\B��B��B�  B�z�B��B�G�B�B�Q�B���B��B��B�(�B�ffB�
=B�p�B��
B�ffB���B�\)B�B�ffB��HB�33B��
B�=qB���B�G�B���B�{B��RB�
=B�p�B�{B�z�B��HB��B��
B�ffB���B�G�B��B�ffB��RB�G�B��
B�=qB��\B�G�B�B�(�B��\B�33B��B��B��\B�
=B�\)B�{B�z�B���B��B��B�Q�B��RB�\)B��
B�(�B��RB�G�B���B�(�B��RB�
=B��B�{B�ffB��HB�p�B��B�=qB���B�p�B�B�=qB��HB�\)B��B�=qB��HB�G�B��B�(�B���B�\)B��B�=qB��HB�33B�B�ffB���B�33B��B�ffB���B�p�B�  B�Q�B��HBÅB��B�z�B��Bř�B��BƏ\B��BǙ�B�  Bȣ�B�33BɮB�  BʸRB�G�B˙�B�(�B���B�33BͮB�Q�BθRB��B�B�Q�BУ�B�G�B�B�(�BҸRB�G�BӅB�(�BԸRB��BՅB�  BָRB��BׅB�(�Bأ�B�
=BٮB�(�Bڏ\B��B�B�=qBܣ�B�
=BݮB�Q�Bޣ�B�33B��
B�=qB�RB�G�B��B�Q�B���B�B�{B�ffB���B噚B�{B�ffB�
=B�B�{B��B�G�B陚B�(�B��HB�p�B�B�=qB�
=B�B��B��B�G�BB�=qB���B�B��B�z�B�G�B�B�(�B��HB�p�B��B�ffB��B�B�{B��HB�p�B��
B�Q�B��HB��B�(�B��\B��B�B�Q�B��RB�\)B��C (�C p�C ��C  C=qC��C�HC�Cz�C�C�CQ�C�\C��C(�Cz�C�RC{CffC��C��CQ�C�\C�HC33Cp�C�C{C\)C��C	  C	Q�C	�\C	�C
G�C
z�C
�
C=qC�CC33Cp�C�RC�CffC�C{C\)C��C  C\)C��C��CQ�C�\C�HCG�C�C��C33Cz�CC33Cz�C�RC{Cp�CC  C\)C�C��CG�C��C��C=qC�C�C=qCp�C�
C(�Cp�C�RC
=CffC�C��CG�C��C�C(�Cz�C�HC=qCz�C��C(�C�\C�
C(�C��C�
C �C �C �
C!{C!�C!�HC"�C"p�C"�
C#(�C#ffC#C$33C$z�C$�RC%{C%z�C%�RC&
=C&ffC&C'
=C'Q�C'�RC({C(Q�C(��C)
=C)\)C)��C)�HC*G�C*��C*�HC+33C+��C+��C,=qC,�C,�
C-=qC-��C-�HC.33C.�\C.�C/(�C/z�C/�HC0=qC0z�C0��C133C1�\C1��C2{C2z�C2�
C3{C3\)C3�RC4{C4p�C4�RC5  C5\)C5�RC6{C6\)C6��C6��C7\)C7�C7�C8=qC8��C8��C9=qC9z�C9�HC:=qC:�C:C;�C;z�C;��C<
=C<ffC<C={C=Q�C=��C=��C>Q�C>��C>�HC?(�C?z�C?�
C@33C@�C@CA
=CAffCACB{CBG�CB�CC
=CCQ�CC��CC��CDQ�CD��CD�CE33CEz�CE�CFG�CF�\CF��CG{CGp�CG�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333333333333333                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aۗ�Aۗ�AہA�~�AہA�ffA�XA�dZA�bNA�^5A�G�A�C�A�1'A�A�A���A���A��A��A��A��A��yA��`A��`A��`A��TA��`A��TA��TA��`A��mA���A��A�VAؗ�Aס�A�?}A�&�A�&�A�&�A� �A�JA�1Aҟ�A�1AЛ�A��A΋DA��A�t�A�oAʃAɍPA�O�A�ĜAǉ7A�A�A�
=A�ȴA�O�A�"�A©�A���A���A���A�XA�+A���A��\A�M�A�%A��mA�XA���A�E�A�ĜA��!A�33A�ƨA� �A�dZA���A��A�dZA���A��HA�Q�A��A�C�A�JA���A�r�A�x�A�v�A��;A��A�S�A��/A�E�A�hsA�%A�(�A��!A�VA�-A�VA��FA�p�A�$�A��HA���A�{A��FA���A�bNA�ĜA�`BA���A��;A��RA��7A{�Aw�AvZAuS�Aq�hAo��An9XAl�uAk�7Ae�;Abn�A_|�A^z�A];dA[�AY��AW�
AUƨAR(�AL�9AK��AK��AJ�`AI�FAH�jAG
=AE"�AD�RAD�AB�RA@��A?�mA?7LA>��A>-A=|�A=&�A<A:bA8�A8r�A85?A6�9A2��A.��A,ȴA+�PA)�7A(-A&1'A#�A#%A!�A �A��A-AbNA�-A9XA��AA�A�jA-A/A1'AG�AA�9A�A��A�
AffA��At�A
  A	
=A5?Ax�A�A�
A+A��AK�A�AĜA�A bN@���@��@�hs@��D@�dZ@�r�@�Q�@��@�33@�@���@�$�@���@��H@���@�9@�x�@���@�9@���@�{@���@�1'@�l�@��T@�%@�@��m@��@�h@�G�@�9X@ް!@�V@��@�33@ڰ!@ڗ�@�n�@ّh@��@��/@�r�@�1@�C�@�-@��@���@�`B@���@Ѓ@��@Ͼw@�+@̴9@��;@��@��/@�hs@�O�@�&�@�j@���@�C�@�p�@�Z@�ƨ@�dZ@�\)@�+@���@ǶF@��@Ƨ�@�(�@��@�ȴ@§�@���@�Q�@���@��T@�hs@�j@�?}@�&�@�G�@�j@��;@���@�O�@���@�1'@���@�/@�/@��@�V@�p�@�p�@��^@���@�-@���@��@��-@��R@�v�@�E�@���@���@�/@��@�v�@�n�@��+@���@�n�@��@��7@�7L@�(�@�l�@��y@���@�n�@�~�@�^5@���@��@�@���@��@��/@�A�@�b@��w@���@���@��/@��j@���@�9X@��@��
@�@��!@���@�~�@��@��@�x�@�x�@�O�@���@��@�bN@��@��w@�C�@���@���@�n�@�^5@�=q@�J@��@���@��^@�p�@�/@��`@���@�(�@��w@�|�@�dZ@�"�@��@�M�@��@��-@�p�@�/@��@�%@��/@��D@�j@�I�@��m@�K�@��@�ȴ@���@�5?@��@���@���@���@��h@�hs@��@��@�r�@�1'@��;@��F@��P@�dZ@�@�=q@���@�O�@���@���@�r�@�Q�@��;@�t�@�33@��@��H@���@�5?@���@��#@�@��h@��@��j@��@�b@�1@��@��w@�o@��H@�ȴ@���@�~�@�E�@��^@�?}@��`@���@���@�Z@�(�@��@�t�@�dZ@��@�~�@�M�@�-@�$�@�J@���@���@�`B@��`@��j@��D@�bN@�b@��@�|�@�S�@�33@��@�ȴ@�M�@��7@�?}@�?}@�?}@�G�@��@��/@��u@�z�@�bN@�9X@���@���@�33@��@��y@��\@�ff@�=q@�5?@�$�@��#@���@���@�x�@��@��j@�I�@��@���@��;@���@�ƨ@���@�K�@�ȴ@���@�^5@�$�@�{@�{@���@��@���@���@�p�@�O�@�?}@�%@��@��@��@��`@��u@�Z@� �@���@��
@��@�|�@�dZ@�
=@���@�M�@�{@��@��^@��h@�O�@�&�@��@��9@�z�@�@\)@+@}�@}p�@}`B@}�@|j@{�@{@z�\@z=q@zJ@y%@xr�@w�@w|�@w�@w
=@v�+@v5?@u�@u�h@u`B@t��@t��@t9X@s�m@s�@r��@q��@qG�@p�9@p1'@ol�@oK�@n�R@nv�@nE�@m�T@m�h@m/@l��@l9X@k��@kƨ@k�@kC�@ko@j�@j�H@j�!@j�\@j=q@i��@i��@ihs@i�@hĜ@h�u@h�@h�@h�u@h�u@h�u@hbN@hbG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AۓuAە�Aە�A۟�Aە�Aۛ�Aۗ�Aۙ�Aە�AۑhAۏ\A�x�A�z�AۅAۅAۉ7A�t�AۅAۇ+AۅA�|�A�r�A�r�A�hsA�^5A�`BA�XA�XA�S�A�`BA�bNA�dZA�hsA�bNA�ffA�`BA�dZA�`BA�dZA�dZA�XA�VA�E�A�G�A�K�A�C�A�K�A�E�A�A�A�9XA�;dA�C�A�A�A�A�A�Q�A�?}A�/A�{A��A��A�A�1A�  A�A�A�A�1A�A�1A�1A�A�A���A���A�  A���A���A�  A���A���A���A���A�  A���A���A���A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��yA��A��A��A��A��A��yA��A��mA��mA��yA��mA��A��mA��A��mA��`A��yA��`A��A��yA��`A��mA��HA��mA��mA��TA��mA��TA��`A��mA��TA��mA��`A��TA��mA��TA��`A��mA��HA��yA��`A��`A��mA��HA��mA��TA��TA��mA��HA��TA��mA��HA��`A��TA��TA��mA��TA��`A��mA��HA��`A��`A��;A��TA��`A��;A��`A��`A��HA��HA��mA��HA��`A��mA��HA��TA��mA��TA��TA��mA��`A��TA��mA��TA��mA��mA��TA��mA��mA��`A��yA��`A��`A��A��mA��A��A��yA��A��A��A��mA��
AڬA�\)A�?}A��A���A���A�ĜAټjAٴ9A٧�Aٕ�AكA�z�A�n�A�\)A�G�A�=qA�+A��A��A�JA��Aذ!A؇+A�Q�A�M�A�A�A�/A�&�A��A�VA���A���A�Q�A�
=Aֲ-A�jA�XA�K�A�C�A�33A�1'A�&�A�+A�+A�&�A�(�A�+A�"�A�(�A�(�A�"�A�&�A�(�A�(�A�&�A�(�A�$�A�$�A�(�A�$�A�&�A�(�A�&�A�&�A�(�A�+A�&�A�&�A�+A�&�A�(�A�(�A��A� �A��A�oA�{A�  A���Aե�A��AԲ-AԍPA�l�A�\)A�ZA�M�A�=qA�(�A��A�1A���A��#AӾwAӡ�A�z�A�(�AґhA�ZA�S�A�M�A�=qA�5?A�-A�$�A��A�bA�VA�1A���A�A�S�A��A���Aд9AБhAЃA�n�A�Q�A���A�\)A�1'A�"�A��A�A��A��A��yA��A���A�AΡ�A�z�A�l�A�XA�?}A�5?A�VA�  A���A���A��A��`A���A���AͲ-AͮAͧ�A͕�A�|�A�dZA�5?A��A�A��;A̲-A�r�A�;dA��yAˏ\A�Q�A� �A��A��HA�ȴAʡ�Aʇ+A�VA�-A�bA���A���Aɥ�Aɟ�AɃA�bNA�E�A�$�A�AȁA�C�A�&�A��A�1A�%A�  A��A��HA���AǺ^AǮAǩ�Aǟ�AǙ�AǙ�AǗ�AǍPAǉ7AǋDAǃA�l�A�dZA�bNA�XA�;dA�5?A�1'A�(�A�&�A�&�A� �A��A��A�JA���A���A��;A���AƓuA�=qA���Aš�AŃA�7LA���A��A���Aĝ�A�~�A�XA�JA��
Aô9AÃA�ffA�\)A�=qA�JA��A��A���A�ĜAº^Aº^A¸RA¥�A�ADAA�v�A�S�A�ȴA���A��7A�x�A�t�A�ffA�VA��A�JA���A���A���A��+A�VA�(�A��A��^A���A���A��PA��A��A�~�A�dZA�E�A�;dA�?}A�-A�-A�/A�-A�&�A�+A�+A�$�A�$�A�$�A�bA�ȴA���A�hsA�=qA�A���A���A�t�A�^5A�=qA��A���A��A���A�^5A��yA��RA���A��\A�bNA�=qA�1A��`A��#A���A���A��hA�/A���A���A��A�XA�A�ȴA��A�-A�{A���A��A��mA��
A���A���A��+A�dZA� �A���A��DA�Q�A��A���A���A���A�ȴA�ĜA�ƨA��wA��^A��jA��^A��FA��!A��A���A��A�ZA�33A�(�A��A�bA�
=A�  A���A�ƨA�A��9A���A�x�A�M�A�5?A�bA�  A���A��yA���A��A�|�A�A�A�JA�ĜA��A���A��TA��;A���A�1'A���A�~�A��A�p�A�A�A���A�ZA���A���A��7A�ZA��A��9A�A�A�bNA��A���A�n�A�
=A���A���A��A��yA��
A�A��A��hA�\)A�$�A��A��A�{A�A���A���A�bNA�7LA��A��A�x�A��A���A��9A���A�bNA�+A�  A��A���A�G�A� �A���A��jA�G�A��A��9A�%A���A��A�=qA�%A��A�ƨA��RA��hA�hsA�33A��mA���A��+A�x�A�hsA�^5A�C�A�(�A�VA��
A�ĜA��RA��A���A��PA�r�A�A�A���A��+A�p�A�jA�dZA�`BA�S�A�I�A��A�
=A���A��/A���A��jA��A���A��+A�ZA�-A�%A��A���A���A��RA�v�A�-A���A��A�bNA�=qA�1A���A���A��A�`BA�7LA��A��A�{A�
=A���A��
A���A��-A��7A�v�A�;dA��A�r�A���A�t�A�`BA�E�A�9XA�/A�/A�-A�$�A��A��A�{A�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333333333333333                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aۗ�Aۗ�AہA�~�AہA�ffA�XA�dZA�bNA�^5A�G�A�C�A�1'A�A�A���A���A��A��A��A��A��yA��`A��`A��`A��TA��`A��TA��TA��`A��mA���A��A�VAؗ�Aס�A�?}A�&�A�&�A�&�A� �A�JA�1Aҟ�A�1AЛ�A��A΋DA��A�t�A�oAʃAɍPA�O�A�ĜAǉ7A�A�A�
=A�ȴA�O�A�"�A©�A���A���A���A�XA�+A���A��\A�M�A�%A��mA�XA���A�E�A�ĜA��!A�33A�ƨA� �A�dZA���A��A�dZA���A��HA�Q�A��A�C�A�JA���A�r�A�x�A�v�A��;A��A�S�A��/A�E�A�hsA�%A�(�A��!A�VA�-A�VA��FA�p�A�$�A��HA���A�{A��FA���A�bNA�ĜA�`BA���A��;A��RA��7A{�Aw�AvZAuS�Aq�hAo��An9XAl�uAk�7Ae�;Abn�A_|�A^z�A];dA[�AY��AW�
AUƨAR(�AL�9AK��AK��AJ�`AI�FAH�jAG
=AE"�AD�RAD�AB�RA@��A?�mA?7LA>��A>-A=|�A=&�A<A:bA8�A8r�A85?A6�9A2��A.��A,ȴA+�PA)�7A(-A&1'A#�A#%A!�A �A��A-AbNA�-A9XA��AA�A�jA-A/A1'AG�AA�9A�A��A�
AffA��At�A
  A	
=A5?Ax�A�A�
A+A��AK�A�AĜA�A bN@���@��@�hs@��D@�dZ@�r�@�Q�@��@�33@�@���@�$�@���@��H@���@�9@�x�@���@�9@���@�{@���@�1'@�l�@��T@�%@�@��m@��@�h@�G�@�9X@ް!@�V@��@�33@ڰ!@ڗ�@�n�@ّh@��@��/@�r�@�1@�C�@�-@��@���@�`B@���@Ѓ@��@Ͼw@�+@̴9@��;@��@��/@�hs@�O�@�&�@�j@���@�C�@�p�@�Z@�ƨ@�dZ@�\)@�+@���@ǶF@��@Ƨ�@�(�@��@�ȴ@§�@���@�Q�@���@��T@�hs@�j@�?}@�&�@�G�@�j@��;@���@�O�@���@�1'@���@�/@�/@��@�V@�p�@�p�@��^@���@�-@���@��@��-@��R@�v�@�E�@���@���@�/@��@�v�@�n�@��+@���@�n�@��@��7@�7L@�(�@�l�@��y@���@�n�@�~�@�^5@���@��@�@���@��@��/@�A�@�b@��w@���@���@��/@��j@���@�9X@��@��
@�@��!@���@�~�@��@��@�x�@�x�@�O�@���@��@�bN@��@��w@�C�@���@���@�n�@�^5@�=q@�J@��@���@��^@�p�@�/@��`@���@�(�@��w@�|�@�dZ@�"�@��@�M�@��@��-@�p�@�/@��@�%@��/@��D@�j@�I�@��m@�K�@��@�ȴ@���@�5?@��@���@���@���@��h@�hs@��@��@�r�@�1'@��;@��F@��P@�dZ@�@�=q@���@�O�@���@���@�r�@�Q�@��;@�t�@�33@��@��H@���@�5?@���@��#@�@��h@��@��j@��@�b@�1@��@��w@�o@��H@�ȴ@���@�~�@�E�@��^@�?}@��`@���@���@�Z@�(�@��@�t�@�dZ@��@�~�@�M�@�-@�$�@�J@���@���@�`B@��`@��j@��D@�bN@�b@��@�|�@�S�@�33@��@�ȴ@�M�@��7@�?}@�?}@�?}@�G�@��@��/@��u@�z�@�bN@�9X@���@���@�33@��@��y@��\@�ff@�=q@�5?@�$�@��#@���@���@�x�@��@��j@�I�@��@���@��;@���@�ƨ@���@�K�@�ȴ@���@�^5@�$�@�{@�{@���@��@���@���@�p�@�O�@�?}@�%@��@��@��@��`@��u@�Z@� �@���@��
@��@�|�@�dZ@�
=@���@�M�@�{@��@��^@��h@�O�@�&�@��@��9@�z�@�@\)@+@}�@}p�@}`B@}�@|j@{�@{@z�\@z=q@zJ@y%@xr�@w�@w|�@w�@w
=@v�+@v5?@u�@u�h@u`B@t��@t��@t9X@s�m@s�@r��@q��@qG�@p�9@p1'@ol�@oK�@n�R@nv�@nE�@m�T@m�h@m/@l��@l9X@k��@kƨ@k�@kC�@ko@j�@j�H@j�!@j�\@j=q@i��@i��@ihs@i�@hĜ@h�u@h�@h�@h�u@h�u@h�u@hbN@hbG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AۓuAە�Aە�A۟�Aە�Aۛ�Aۗ�Aۙ�Aە�AۑhAۏ\A�x�A�z�AۅAۅAۉ7A�t�AۅAۇ+AۅA�|�A�r�A�r�A�hsA�^5A�`BA�XA�XA�S�A�`BA�bNA�dZA�hsA�bNA�ffA�`BA�dZA�`BA�dZA�dZA�XA�VA�E�A�G�A�K�A�C�A�K�A�E�A�A�A�9XA�;dA�C�A�A�A�A�A�Q�A�?}A�/A�{A��A��A�A�1A�  A�A�A�A�1A�A�1A�1A�A�A���A���A�  A���A���A�  A���A���A���A���A�  A���A���A���A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��yA��A��A��A��A��A��yA��A��mA��mA��yA��mA��A��mA��A��mA��`A��yA��`A��A��yA��`A��mA��HA��mA��mA��TA��mA��TA��`A��mA��TA��mA��`A��TA��mA��TA��`A��mA��HA��yA��`A��`A��mA��HA��mA��TA��TA��mA��HA��TA��mA��HA��`A��TA��TA��mA��TA��`A��mA��HA��`A��`A��;A��TA��`A��;A��`A��`A��HA��HA��mA��HA��`A��mA��HA��TA��mA��TA��TA��mA��`A��TA��mA��TA��mA��mA��TA��mA��mA��`A��yA��`A��`A��A��mA��A��A��yA��A��A��A��mA��
AڬA�\)A�?}A��A���A���A�ĜAټjAٴ9A٧�Aٕ�AكA�z�A�n�A�\)A�G�A�=qA�+A��A��A�JA��Aذ!A؇+A�Q�A�M�A�A�A�/A�&�A��A�VA���A���A�Q�A�
=Aֲ-A�jA�XA�K�A�C�A�33A�1'A�&�A�+A�+A�&�A�(�A�+A�"�A�(�A�(�A�"�A�&�A�(�A�(�A�&�A�(�A�$�A�$�A�(�A�$�A�&�A�(�A�&�A�&�A�(�A�+A�&�A�&�A�+A�&�A�(�A�(�A��A� �A��A�oA�{A�  A���Aե�A��AԲ-AԍPA�l�A�\)A�ZA�M�A�=qA�(�A��A�1A���A��#AӾwAӡ�A�z�A�(�AґhA�ZA�S�A�M�A�=qA�5?A�-A�$�A��A�bA�VA�1A���A�A�S�A��A���Aд9AБhAЃA�n�A�Q�A���A�\)A�1'A�"�A��A�A��A��A��yA��A���A�AΡ�A�z�A�l�A�XA�?}A�5?A�VA�  A���A���A��A��`A���A���AͲ-AͮAͧ�A͕�A�|�A�dZA�5?A��A�A��;A̲-A�r�A�;dA��yAˏ\A�Q�A� �A��A��HA�ȴAʡ�Aʇ+A�VA�-A�bA���A���Aɥ�Aɟ�AɃA�bNA�E�A�$�A�AȁA�C�A�&�A��A�1A�%A�  A��A��HA���AǺ^AǮAǩ�Aǟ�AǙ�AǙ�AǗ�AǍPAǉ7AǋDAǃA�l�A�dZA�bNA�XA�;dA�5?A�1'A�(�A�&�A�&�A� �A��A��A�JA���A���A��;A���AƓuA�=qA���Aš�AŃA�7LA���A��A���Aĝ�A�~�A�XA�JA��
Aô9AÃA�ffA�\)A�=qA�JA��A��A���A�ĜAº^Aº^A¸RA¥�A�ADAA�v�A�S�A�ȴA���A��7A�x�A�t�A�ffA�VA��A�JA���A���A���A��+A�VA�(�A��A��^A���A���A��PA��A��A�~�A�dZA�E�A�;dA�?}A�-A�-A�/A�-A�&�A�+A�+A�$�A�$�A�$�A�bA�ȴA���A�hsA�=qA�A���A���A�t�A�^5A�=qA��A���A��A���A�^5A��yA��RA���A��\A�bNA�=qA�1A��`A��#A���A���A��hA�/A���A���A��A�XA�A�ȴA��A�-A�{A���A��A��mA��
A���A���A��+A�dZA� �A���A��DA�Q�A��A���A���A���A�ȴA�ĜA�ƨA��wA��^A��jA��^A��FA��!A��A���A��A�ZA�33A�(�A��A�bA�
=A�  A���A�ƨA�A��9A���A�x�A�M�A�5?A�bA�  A���A��yA���A��A�|�A�A�A�JA�ĜA��A���A��TA��;A���A�1'A���A�~�A��A�p�A�A�A���A�ZA���A���A��7A�ZA��A��9A�A�A�bNA��A���A�n�A�
=A���A���A��A��yA��
A�A��A��hA�\)A�$�A��A��A�{A�A���A���A�bNA�7LA��A��A�x�A��A���A��9A���A�bNA�+A�  A��A���A�G�A� �A���A��jA�G�A��A��9A�%A���A��A�=qA�%A��A�ƨA��RA��hA�hsA�33A��mA���A��+A�x�A�hsA�^5A�C�A�(�A�VA��
A�ĜA��RA��A���A��PA�r�A�A�A���A��+A�p�A�jA�dZA�`BA�S�A�I�A��A�
=A���A��/A���A��jA��A���A��+A�ZA�-A�%A��A���A���A��RA�v�A�-A���A��A�bNA�=qA�1A���A���A��A�`BA�7LA��A��A�{A�
=A���A��
A���A��-A��7A�v�A�;dA��A�r�A���A�t�A�`BA�E�A�9XA�/A�/A�-A�$�A��A��A�{A�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333333333333333                                                                                                                                11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
�PB
�PB
��B
�B
�~B
��B
��B
�~B
�"B
��B
�PB
�JB
�~B
�JB
�B
��B
�B
��B
��B
�xB
��B
��B
��B
�B
�JB
��B
��B
�B
��B
�0BIB6�B`vB��B�B��B�5B�B�B�B5tBn�B� B��B�!B��B�$B�B�RB�[B��B֡B��B��B�pB�B��B��B�lB�2B��B��B�PB�>B�+B��B�B�B�B�6B��B��B�-B�RB�B�VB��B��B�1B}"Bi�BK�B>�B%�B!�B�BuB�B�DB�B�sB�pB�RB�[B�9B��B�B��B��B}�Bv+BiyBT�BQ�BM6BH�BB'B<6B/�B"hBB
�(B
�
B
��B
��B
��B
iyB
P�B
2-B

	B	�B	�B	��B	�^B	�3B	�IB	��B	�1B	��B	tTB	Z�B	N�B	K�B	A�B	;�B	-�B	"�B	+�B	�B�JB��B�B�GB��B�fB�B�mB��B�B��B�
B��B�}B��B�B�mB�tB�B��B�[B�B��B�FB�B�\B�JB�B�SB��B��B�4B~�By�By�B|�Bx8BtBu�BqvBl"BiyBd�Bh�BkBjKBh>Bg8Bg8Bf�Bd�Bc�B`vB_;B`�B`vB_;BZ�BZ�BZ�BYKBY�BVmBV�BU�BWsB]/B^�B_pBf2Be�Bg8Bd�Bk�BuZBw2Bx8By>ByrB}VB�oB�iB�%B�bB��B~�B�iB�B��B�B�B�SB�lB�4B�_B�B�!B��B�B�:B�:B��B��B�OB�B�6B��B��B�[BŢB�tB�B��B�^B�BB��BԕB�mBںB�WB��B��B�B��B�fB��B	;B	�B	B	�B	�B	 4B	  B	 iB	GB		lB	~B	�B	B	~B	qB	_B	B	�B	B	�B	.B	C�B	D3B	C�B	B�B	MB	RTB	ZQB	ZB	ZB	W�B	V9B	WsB	X�B	^�B	gB	lWB	lWB	ffB	j�B	m)B	q�B	m�B	gB	h�B	h�B	h�B	t�B	wfB	zB	|B	��B	��B	�JB	��B	�$B	��B	�B	��B	�nB	��B	��B	��B	�zB	�B	�tB	��B	��B	��B	��B	�'B	��B	�LB	�RB	�RB	�XB	��B	�B	��B	��B	��B	�-B	��B	��B	�B	�zB	��B	��B	��B	��B	�jB	��B	�B	�wB	�OB	��B	��B	�[B	��B	ŢB	�KB	�XB	��B	��B	�B	�jB	�B	ϫB	�HB	ӏB	��B	�?B	�sB	��B	ٴB	�QB	ںB	ںB	��B	��B	�B	�B	� B	��B	��B	�B	�fB	�fB	�8B	�DB	��B	��B	�B	��B	�WB	�B	�]B	�cB	�B	�AB	�AB	�MB	�B	��B	��B	��B	��B	�2B	��B	�fB	��B	�lB	�>B	�	B	�	B	�DB	�B	��B	��B	�B	�PB	��B	��B	�VB	��B	��B	�]B	�]B	��B	��B	�.B	�.B
  B
 �B
 �B
oB
{B
�B
�B
{B
�B
�B
%B
_B
	7B
	lB
	lB
	�B

=B

�B
B
�B
�B
�B
�B
"B
"B
"B
�B
�B
�B
"B
�B
�B
�B
�B
bB
�B
�B
�B
�B
�B
4B
hB
�B
B
�B
�B
�B
B
�B
@B
@B
uB
FB
FB
�B
�B
�B
YB
1B
eB
�B
1B
�B
B
�B
�B
�B
�B
�B
CB
xB
�B
B
B
IB
�B
OB
�B
�B
 �B
 �B
 \B
 \B
 �B
 �B
 �B
 �B
 �B
 �B
!-B
 �B
 �B
!bB
!�B
!�B
"�B
"4B
#B
#nB
#nB
#�B
#�B
#�B
$tB
$tB
%�B
%FB
%zB
%FB
%zB
%�B
%zB
%�B
%zB
%zB
&B
%�B
%B
&�B
%�B
%zB
%B
&�B
&�B
&�B
'B
(XB
($B
(�B
(�B
)_B
)�B
*eB
,�B
-�B
-wB
-�B
-�B
.�B
/B
/�B
/�B
0!B
0UB
0�B
0�B
0�B
0�B
0�B
1�B
1�B
2�B
2-B
2�B
3�B
3hB
3hB
3�B
3hB
3hB
33B
3�B
3hB
3�B
3�B
4B
49B
4nB
5B
5tB
6B
6B
6zB
6zB
6�B
6�B
6�B
7B
7�B
7�B
8B
8�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�xB
��B
��B
��B
��B
�"B
��B
�(B
�VB
��B
��B
�DB
��B
�1B
�B
��B
�~B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�~B
��B
�~B
��B
��B
�PB
�B
��B
��B
�1B
��B
�xB
��B
�bB
��B
�JB
��B
�\B
��B
�"B
��B
��B
�	B
��B
�B
��B
�	B
��B
��B
��B
�"B
�xB
��B
�~B
��B
�PB
��B
�JB
��B
��B
�PB
��B
�B
��B
�PB
��B
�B
�JB
��B
��B
��B
�JB
�~B
�DB
��B
�rB
��B
�xB
�~B
��B
�B
�B
�xB
�DB
��B
�rB
�~B
�DB
�JB
��B
�DB
�B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�B
�B
�~B
�xB
�PB
�JB
�rB
��B
�~B
��B
�xB
�=B
�B
�DB
�JB
�B
��B
�JB
�B
�B
�~B
�DB
��B
��B
�xB
�~B
��B
�JB
��B
�xB
��B
�rB
�DB
��B
��B
�~B
��B
��B
�B
�rB
�B
��B
�B
��B
��B
�JB
�~B
��B
��B
�B
��B
�B
��B
�B
��B
�DB
�~B
�JB
�DB
��B
�PB
��B
�(B
�VB
�PB
�.B
��B
��B
�\B
�\B
��B
�\B
��B
��B
�:B
��B
��B
��B
��B
�.B
�hB
�oB
� B
�@B
�B
��B
�FB
��B
��B
�MB
�B
�SB
��B
��B
�CB
��B
�nB
�B
�:B
�nB
��B
��B
�HB
��B
��B;BB+B �B#B$B&B)*B*eB/�B.�B1�B5tB:^B;0B?HBC�BCaBE9BM6B]dBe`BqBn/Br�Bw�Bx�BzxB}VB��B��B�B��B�,B�NB�NB�B��B�"B�B�B�]B�B�iB�B��B�oB�/B�B�oB�5B�/B�B�iB�B�;B��B�5B��B�AB�iB��B�B�B��B�B�vB�B�vB�B�;B�B�;B�GB�MB�oB��B��BoBxB \B&B)_B,qB,B-CB.�B4nB4B33B5�B<6B?HBE9BK�B_;Bx�Bz�Bv+Bw2BzxB{�B|�B~�B}�BcB~�B}�B~]B�PB�FB��B��B��B��B��B�rB��B�=B�\B�\B�qB�=B��B��B��B��B��B��B��B��B�FB�tB��B��B��B�qB��B�LB�RB��B��B��B��B�=B�XB�*B�B��B��B��B�B�CB�'B�nB��B�RB�OB�B�0B��B��B�jB�HB��B�gBÖB��B�gB�B�B�jB�?BȴB�zBɺB�vB�jB�?B�B��B�mB�B�B՛BچB��B�)B��BܒB�WB��B��B�)B��BܒB��B��B��B�jB�;B�B�dB��B�pB�;B�pB�pB�5BޞB��B�B�jB�BݘB�B�|B�mB�B��B�DB��B�AB�oB��B�B��B�B�B�PB�B��B�B�+B��B��B�lB�lB��B��B��B�2B�`B�+B��B��B�fB��B��B��B�B��B��B��B�JB�VB�"BB��B��BB��B�(B��B�"B��B �B��B�DB�B��B�8B��B��B��B�2B��B��B��B��B��B��B��B�B��B��B�B��B�(B��B�%B�ZB�>B��B�|B��B� B�/B��B�B�B�WB�B�vB�&B�QB�B��B�|B�BںB�&B�NB�<B�B��BϫBŢB��B�0B�B��B�aB�6B�'B��B��B��B�B��B��B�OB�'B��B�B��B��B��B�_B��B��B��B��B��B��B�RB��B�B�B�B�tB�FB��B�FB�\B��B��B�xB��B��B��B�B�B�FB��B�YB��B��B��B��B�fB��B��B��B��B��B�YB�B��B�Bq�Bo�BpoB�IBf�Bb�Bw�BqB]/Bb�BiBS�BGzBH�BIRBG�BC�BI�BHB>BB6B1�B4nB'RB%B&LB%B(�B#:B$@B%�B'B�B�B�B�B�B �B�BqB+BFB{B'B�BB�B
	BB1B�B�B1BB��B�B iB�xB�B��B�B��B�yBݘB��B�mB՛B�gBרB֡B�B�]B��B�<B��B˒B˒B��BʌB͟B�B�zBƨB�-BÖB�mB�'BƨB�#B�*B��B��B��B��B��B��B��B�OB��B�B�B��B�6B��B�*B��B��B�$B��B�:B�qB��B��B�OB�B�B��B�VB� B��B�B��B��B��Bz�BwfBwfBzDBw�B|�Bu�BxBy	Bm]BkQBsBv`B�B[WBXBX�BXyBT�BS&BR�BS�BTaBS�BR�BP�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B
�	B
�=B
��B
��B
��B
�kB
��B
�7B
�7B
��B
�rB
�B
��B
��B
��B
��B
�eB
��B
�eB
�1B
�1B
��B
��B
��B
��B
�eB
��B
�B
�IB
�VB
�B
��B�B33B\�B�$B��B�B�B��B�]B�B1�BkB|PB�B�qB�'B�tB�RB��B��B�9B��B�>B�KB��B�WB�DB�AB��B�B��B��B��B��B�{B�MB��B��B��BɆB�B�B�}B��B�bB��B�@B�B��ByrBf2BHKB:�B!�B�B�B�BMB��B��B��B��BŢB��B��B�0B�nB��B�1By�Br{Be�BP�BNBI�BD�B>wB8�B+�B�B_B
�xB
�ZB
�B
�@B
�B
e�B
M5B
.}B
YB	��B	��B	�BB	ǮB	��B	��B	�B	��B	��B	p�B	W>B	J�B	HB	>BB	7�B	)�B	�B	'�B	B��B��B�SB�B�B�B��B�B�NB��B�>B�ZB�<B��B�B�gB��B��B�XB��B��B�^B�*B��B�nB��B��B�_B��B�@B� B|�B{Bv+Bu�Bx�Bt�BpoBrGBm�BhrBe�BaGBd�BglBf�Bd�Bc�Bc�Bb�BaGB_�B\�B[�B]/B\�B[�BV�BV�BV�BU�BVBR�BS&BR BS�BYB[#B[�Bb�Ba�Bc�BaBh>Bq�Bs�Bt�Bu�Bu�By�B}�B|�B�uB��B�B{B|�B{�B}"B~\B�iB��B��B��B��B�_B�qB��B�bB��B��B�-B��B��B�UB��B�B�B��B��B��B�gB�BǮB˒B�5B��BҽB�
BקB�B�/B��B�B�B�B��B��B�\B�.B�.B��B�PB��B��B	�B	�B		�B	nB	�B	�B	�B	VB	!B	\B	�B	*dB	@B	@�B	@B	?B	IQB	N�B	V�B	VmB	VmB	T,B	R�B	S�B	T�B	[#B	cTB	h�B	h�B	b�B	g8B	iyB	n.B	jJB	cTB	e,B	d�B	e,B	p�B	s�B	v`B	xlB	�B	��B	��B	��B	�tB	�B	�kB	�OB	��B	��B	�3B	�B	��B	�hB	��B	�-B	�B	�B	��B	�wB	�'B	��B	��B	��B	��B	�B	�mB	�B	� B	�B	�}B	��B	�-B	�aB	��B	�9B	��B	��B	�EB	��B	�)B	�^B	��B	��B	�B	�B	��B	�B	��B	ěB	ƨB	�KB	�B	�QB	ɺB	�^B	��B	̘B	��B	�&B	ӏB	��B	�&B	�B	֡B	�
B	�
B	�B	�EB	�]B	�B	�pB	�GB	�B	��B	�B	�B	�B	�B	�2B	�8B	��B	�>B	�B	��B	�B	�B	�WB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�MB	�B	��B	��B	��B	�YB	�YB	��B	�fB	��B	�1B	�lB	��B	�>B	�	B	��B	�B	��B	��B	��B	�DB	��B	�~B	�~B	�PB	�"B	�"B	��B	��B
  B	�.B	��B
 �B
@B
uB
�B
�B
�B
�B
%B
�B
+B
	kB
	B
	7B

=B

=B

rB

rB

rB

=B

=B

=B

rB
CB
CB
IB
B
�B
�B
B
B
B
�B
�B
�B
'B
VB
�B
�B
�B
\B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
LB
RB
B
�B
B
�B
*B
�B
�B
0B
eB
eB
�B
0B
�B
=B
=B
�B
�B
�B
�B
�B
B
�B
B
�B
B
}B
IB
IB
�B
�B
OB
�B
�B
UB
�B
�B
�B
 'B
�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"hB
"3B
!bB
#B
!�B
!�B
!bB
"�B
"�B
#B
#nB
$�B
$tB
%B
%FB
%�B
&LB
&�B
)*B
)�B
)�B
*0B
*0B
+6B
+kB
,B
,B
,qB
,�B
-B
-B
-B
,�B
-BB
-�B
-�B
.�B
.}B
/B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0 B
0 B
0UB
0�B
0�B
1[B
1�B
2aB
2aB
2�B
2�B
2�B
2�B
2�B
3gB
3�B
4B
4mB
5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�7B
��B
�B
��B
�B
�	B
�rB
��B
�xB
��B
��B
�=B
��B
�1B
��B
�kB
��B
��B
�B
��B
�	B
�'B
�	B
��B
�eB
�B
�	B
�B
�7B
��B
�B
��B
��B
�	B
��B
�=B
�1B
��B
�eB
��B
�CB
��B
��B
��B
�1B
��B
�1B
��B
�IB
��B
��B
�rB
�1B
�B
�YB
�CB
�hB
�CB
�YB
��B
�B
��B
�rB
��B
�1B
��B
�+B
��B
��B
��B
�=B
�B
��B
�7B
�_B
�1B
��B
��B
�kB
��B
�1B
�B
�+B
��B
��B
��B
�	B
��B
�7B
��B
��B
�7B
�_B
�kB
��B
��B
�7B
��B
��B
��B
��B
�1B
��B
�kB
�+B
�7B
�+B
�1B
�kB
�_B
��B
�B
�+B
�B
�eB
�_B
��B
��B
��B
��B
��B
�B
��B
�B
��B
��B
�eB
��B
��B
�eB
��B
��B
�_B
�_B
��B
��B
�7B
�+B
��B
��B
��B
��B
��B
��B
�7B
��B
��B
�B
��B
��B
��B
�+B
�kB
��B
�eB
�1B
�_B
�7B
�+B
��B
��B
�+B
�7B
�eB
�+B
�kB
�1B
�eB
�7B
��B
��B
��B
��B
��B
��B
�7B
�xB
��B
��B
�~B
�B
��B
��B
��B
�	B
��B
��B
��B
��B
��B
�IB
�!B
�!B
�~B
��B
��B
�PB
��B
�VB
�!B
��B
��B
�4B
��B
�bB
��B
�B
��B
��B
�B
��B
�bB
��B
��B
�FB
�6B
��B
�8B
��B
��BeB{BBUB [B"hB%zB&�B+�B+6B.B1�B6�B7�B;�B@B?�BA�BI�BY�Ba�Bm]BjBo BtBt�Bv�By�B~�B��B�aB�<B�|BޞBޞB��B�%B�rB�
B��B�B��B�B��B�B�B�B��B�B�B�B��B�B��B�B�"B�B�.B�B�B�(B�]B�WB�"B�cB��B��B��B�WB�B�cB�B�B�B�B�B��B��B�B�B"hB%�B(�B(XB)�B+B0�B0UB/�B1�B8�B;�BA�BG�B[�Bt�Bw1Br{Bs�Bv�BxBy>B{JBy�B{�B{BzBz�B��B��B��B�.B�%B�B�1B��B�B��B��B��B��B��B�CB��B��B�0B�B�CB�OB��B��B��B��B�B��B��B�B��B��B��B��B�@B�9B��B��B�zB�RB�FB��B��B�dB��B�wB��B��B��B��B�^B��B��B�NB��B��B��B��B��B�,B��B�aB�aBɺBB�B��B�
B��BɺBӏB�gB� BҽB�mB�TB��B��B�8B�yB�B��BקB�B�KB�yB�B��B�KB�EB�BںBۋB�QBٴB�B��BۋB��B��BڅB��B�)B�QBںB�cB��B�iB��B�B�lB�B�B�B�B�B�B��B�B�B��B��B�lB�B��B�{B�B��B��B��B��B�B�B�B�B�{B�%B�%B�B�GB�GB�B�B��B��B��B��B��B�rB_B�+B�B�bB��B�xB��B�rB��B�"B��B��B�SB�+B�B��B�>B�1B�B�B��B�B�B�B�B�AB��B�B�AB�iB�MB�xB��B�uB�B��B�AB��B�B�PB�B�DB�fB��B�B��B��B�vB֡B��B�B��B��B�
B�vB͞BʌB�mB�GB��B��B�BȀB�QB�B��B��B�wB��B�<B�<B�dB�B��B��B�wB��B�UB�0B�B�B��B��B�9B��B�@B�3B��B��B��B�bB�hB�hB��B��B��B��B��B��B�$B��B�B��B�$B�nB�\B��B�B��B�B�B�7B�+B��B�%B�%B��B�%B�B��BbB�B�SBm�Bk�Bl�B��Bb�B_;Bs�Bm]BYB^�Be`BPHBC�BD�BE�BC�B?�BF?BDgB:�B2aB.B0�B#�B!bB"�B!bB$�B�B �B"3B#nBCB*B*B�B7BIB0B�B{B�B�B#nB�B_BBYB	kB�BB��B�B�\B�+B��B��B��B��B�JB��B�#B��B��B�BҽB��BѷB��B��B�gBحB�NBʌB�KB��B��B�)B��B��B�^B��B��B�}B��B��B�wB��B�sB�zB��B� B�OB�B��B� B� B��B�B�dB�kB�B��B�B�zB�6B�-B�tB��B��B��B�0B�B��B�_B�nB�B��B�PB�1B�iB�B��B�%Bv�Bs�Bs�Bv�BtBx�BrGBtSBuYBi�Bg�BoiBr�B{�BW�BT`BU2BT�BQNBOvBOBBPBP�BO�BN�BM5444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721225008                            20230721225008AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122500820230721225008  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500820230721225008QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500820230721225008QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             