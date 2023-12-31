CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:50:02Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  R�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  X4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  t�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ڀ   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � |   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � (   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` )�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   *0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   00   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   60   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T <0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   <�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   <�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   <�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   <�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � <�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   =$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   =@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    =H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        =h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        =p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       =x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    =�Argo profile    3.1 1.2 19500101000000  20230721225002  20230721225002  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�)0'f�@�)0'f�11  @�)0[Ȁ@�)0[Ȁ@3"�},{�@3"�},{��d͟�vȴ�d͟�vȴ11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 BA  BA  FF  ?�  @�\@=p�@�  @�  @�  @�G�AG�A  A\)A+�A?\)A`  A���A��A�  A���A�Q�A�  A�\)A�
=B   B  B�
B  B   B(  B0  B7�
B?�
BH  BP  BX  B_�
Bg�
Bp  BxQ�B�(�B�  B��B��B�  B��B��
B�{B�(�B�  B�{B�(�B�  B�  B�  B��
B��
B��
B��B�  B�{B�  B��
B��B�(�B�  B��B�  B�  B�  B�{B�{C   C
=C��C��C  C	��C
=C  C  C  C  C  C  C
=C
=C
=C��C!�C$  C&
=C({C)��C+�HC.  C0
=C2
=C4
=C5��C8  C:  C<
=C>{C@  CB  CD
=CF  CH  CI��CL  CN
=CO��CQ�CS�CU�HCW�CY��C[�C]�C_��Ca�HCd  Cf  Cg�Ci�Ck�Cn  Cp
=Cr  Ct
=Cv  Cw��Cy�HC{�C~
=C�C�C�C�C�C�  C�  C���C�  C�  C�  C�
=C�  C���C���C�  C�  C�C�  C���C�  C���C�  C�C���C�  C���C���C���C�  C�  C���C���C�  C�  C�  C�C�C�C�
=C���C���C���C�  C�C�C�
=C�C���C�  C�  C�  C�  C�C�  C�  C�C�C���C���C���C���C�  C�  C���C�  C�  C���C���C�  C�C�  C�  C�C�  C�  C�C�C���C�  C�  C�  C�  C�  C�C���C�  C�
=C�
=C�  C���C�  C�
=C�C�C�C�  C���C���C���C�C�C�  C���C���C���C���C���C���C���C���C���C���C�  C���C�  C�  C���C���C�  C�  C���C�  C�  C�C�C�  C�  C���D � D�D� D  D}qD�qD��D  D� D  D}qD�D��D  D� D�qD� D	  D	� D
  D
}qD  D}qD�qD� D  D��D�D� D  D� D  D� D  D�D  Dz�D�qD� D�qDxRD��D� D�qDz�D�qD��D  D}qD�qD� D�D�D  D� D  D� D�D� D�qD� D�D� D   D � D �qD!}qD"  D"� D#�D#� D$�D$��D$�qD%z�D&  D&�D'D'� D(  D(� D(�qD)}qD*  D*� D+  D+��D,D,� D,�qD-}qD-�qD.��D/�D/��D0  D0� D0�qD1� D2  D2��D3�D3� D4�D4� D4�qD5� D5�qD6z�D6�qD7� D8  D8� D9  D9� D:�D:� D:�qD;}qD<  D<�D=�D=� D=��D>� D?  D?� D@  D@}qD@�qDA}qDA��DB}qDB�qDCz�DD  DD��DD��DE}qDF�DF��DG�DG� DG�qDH��DI  DIz�DI��DJ}qDK  DK��DL�DL� DM  DM��DN  DN}qDO  DO��DP�DP� DP�qDQ� DR  DR}qDS�DS� DS��DTz�DT�qDU� DV  DV�DW�DW}qDW�qDX� DY  DY��DZ�DZ}qD[  D[��D\  D\}qD\�qD]}qD^�D^��D_  D_��D_�qD`z�D`�qDa��Db  Db}qDb�qDcz�Dd  Dd� De�De}qDe�qDf� Dg  Dg��Dh�Dh�Di�Diz�Di��Djz�Dj�qDk}qDk��Dl� Dm  Dm� DnDn� Dn��Doz�Dp  Dp� Dq�Dq��Dr  Dr��Ds  Ds� Dt  Dt}qDt�qDu� Dv  Dv��Dw�Dw� Dx  Dx��Dy  Dy� Dz�Dz�D{�D{�D|D|��D}�D}��D~�D~��D  D}qD�  D�AHD�~�D�� D�HD�>�D�~�D�� D�  D�AHD��HD���D��qD�@ D�� D��qD��qD�>�D��HD�D�  D�>�D�~�D���D�  D�=qD�~�D���D�  D�B�D�� D���D�  D�AHD���D�� D��D�@ D�� D�� D�  D�>�D�~�D���D��qD�>�D�~�D�� D�HD�AHD���D�� D�  D�B�D��HD�� D�HD�AHD�� D��HD�HD�AHD�� D���D�  D�@ D���D�� D���D�>�D�~�D���D�  D�>�D�� D��HD��D�AHD�� D�� D�  D�>�D�� D��HD�  D�>�D�� D�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D�� D���D�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�B�D�l�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?\)?aG�?�\)?���?\?��@�@�@!G�@5@B�\@L��@c�
@xQ�@�  @���@�z�@��H@�G�@�{@�33@�(�@Ǯ@�{@�z�@�G�@�@�\)@��HAG�Az�A	��A\)A�AA(�A\)A#�
A)��A,(�A0��A6ffA:=qA=p�AC33AHQ�AJ�HAP��AS�
AW
=AZ�HA`��Ac33Ag�Al��Ap  As33Ay��A|��A�Q�A��\A��A�
=A�Q�A��\A�p�A��RA���A�33A�z�A��RA�G�A�=qA�z�A�
=A���A��A�z�A�ffA��A��A��A���A�\)A�G�A�=qA���A��RA��A��A�z�A�ffA��A���A�(�A�p�AǮA�=qA��
A�p�AϮA��A��HA�p�A׮A�G�A��HA���A�
=A�G�A�\A�z�A�
=A��A��A�(�A�ffA�  A�A�A�{A�Q�A���A�z�A��RB (�B ��BffB\)B  BG�B�\B33B(�B	p�B
�\B33B(�BG�BffB�HB(�Bp�B=qB�HB(�BG�B{B�RB  B�BB�HB  B��B��B�RB�B (�B!p�B"ffB#
=B$(�B%p�B&{B'
=B(Q�B)G�B*{B*�HB,(�B-G�B.{B.�HB/�
B1�B1�B2�\B4  B5�B5B6�\B7�
B8��B:{B:�HB<  B=G�B>{B>�HB@Q�BAp�BB{BC33BDz�BE��BF=qBG\)BH��BI��BJ=qBK�BL��BMp�BN�RBO�
BP��BQBR�\BS�
BT��BU��BV�\BW�
BX��BYBZ�\B[�
B\��B]B^�RB`(�Ba�BaBb�HBd(�BeG�Bf=qBg
=BhQ�Bi��Bj=qBk\)Bl��Bm��BnffBo�Bp��Bq��Br�\Bs�Bt��Bu�Bv�RBw�
By�Bz=qB{
=B|  B}G�B~=qB33B�(�B���B�G�B��B�(�B���B�p�B��
B�Q�B���B���B�{B�z�B��B��
B�=qB���B�G�B��B�Q�B��RB�p�B�  B�ffB��HB���B�{B�z�B�33B��
B�=qB���B�p�B��B�Q�B�
=B��B�{B�z�B��B�B�=qB���B�G�B�  B�z�B��HB�p�B�(�B��RB��B�B�z�B���B�p�B�  B���B�G�B�B�=qB���B��B��B�ffB�
=B��B�(�B���B�G�B��B�z�B��HB�\)B�  B��RB�33B���B�(�B��HB��B�{B��\B�
=B���B�=qB���B��B��B�z�B�33B��
B�Q�B��RB�p�B�{B��\B�
=B���B�=qB��HB�p�B��
B�ffB��B�B�=qB��RB�\)B�{B��RB��B��B�ffB��B��B�{B���B�\)B�{B�z�B���B�B�Q�B��HB�\)B�{BƸRB��BǮB�ffB�
=B�p�B�{B���B�33B�B�z�B�
=BͅB�=qBΣ�B�G�B�  BЏ\B�
=BхB�Q�B���B�\)B�  B���BՅB�{B֣�B�G�B�(�Bأ�B�33B�  Bڣ�B��B�  BܸRB�33B��
Bޣ�B��B߮B��\B��BᙚB�Q�B���B�B�(�B��B�p�B�{B��B��B��
B�z�B�
=B�B�=qB���B�\)B��B�RB�G�B�B�z�B�33BB�=qB��HB�B�{B�\B�G�B�  B�Q�B�
=B�B�=qB��RB��B�(�B���B�G�B��B���B�33B���B�ffB��B��B�(�B���B�G�C   C ffC ��C �CG�C��C�C(�Cz�C�
C�CQ�C�RC
=C=qC�C�
C33Cp�C�C  C\)C��C�HC(�C�\C�
C
=CQ�C�C�C	(�C	p�C	��C

=C
=qC
�C
�HC33Cz�C�RC  CQ�C��C�HC�C\)C�C{CQ�C�C�
C(�Cz�C�C��CQ�C��C�
C
=CQ�C�C  C33Cp�C�RC
=CQ�C��C�HC�C\)C��C  CQ�C�\CC
=C\)C�C��C=qC�C�RC��C33C�C�
C�C\)C�\C�
C(�C�C�RC��C33C�C�
C�CQ�C�\C�
C(�Cp�C��C�
C�Cz�C�RC��C(�Cp�C�RC 
=C Q�C ��C �C!(�C!ffC!��C!�HC"(�C"z�C"C#�C#ffC#��C#�HC$33C$�\C$�
C%�C%ffC%�C%��C&(�C&ffC&�C&��C'=qC'�\C'�HC(�C(\)C(��C(�
C){C)\)C)�C*  C*=qC*�\C*�HC+�C+\)C+��C+�HC,�C,p�C,�RC-  C-Q�C-��C-�HC.(�C.p�C.��C.�C/=qC/�C/��C0�C0ffC0�RC1  C1G�C1�\C1�
C2�C2\)C2�C2��C3G�C3�\C3�
C4{C4ffC4��C4�C533C5�C5��C6{C6ffC6�C7
=C7Q�C7��C7�C8=qC8�\C8�
C9{C9\)C9��C9��C:33C:�C:��C;�C;ffC;C<
=C<\)C<��C<��C==qC=�\C=�
C>(�C>p�C>�RC?  C?G�C?��C?�C@33C@�C@�
CA(�CA�CA�
CB(�CB�CB�
CC�CCp�CC�RCD
=CDG�CD��CD�CE=qCE�\CE�CFG�CF��CF�CG=qCG��CG�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333333                                                                                                 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @�\@=p�@�  @�  @�  @�G�AG�A  A\)A+�A?\)A`  A���A��A�  A���A�Q�A�  A�\)A�
=B   B  B�
B  B   B(  B0  B7�
B?�
BH  BP  BX  B_�
Bg�
Bp  BxQ�B�(�B�  B��B��B�  B��B��
B�{B�(�B�  B�{B�(�B�  B�  B�  B��
B��
B��
B��B�  B�{B�  B��
B��B�(�B�  B��B�  B�  B�  B�{B�{C   C
=C��C��C  C	��C
=C  C  C  C  C  C  C
=C
=C
=C��C!�C$  C&
=C({C)��C+�HC.  C0
=C2
=C4
=C5��C8  C:  C<
=C>{C@  CB  CD
=CF  CH  CI��CL  CN
=CO��CQ�CS�CU�HCW�CY��C[�C]�C_��Ca�HCd  Cf  Cg�Ci�Ck�Cn  Cp
=Cr  Ct
=Cv  Cw��Cy�HC{�C~
=C�C�C�C�C�C�  C�  C���C�  C�  C�  C�
=C�  C���C���C�  C�  C�C�  C���C�  C���C�  C�C���C�  C���C���C���C�  C�  C���C���C�  C�  C�  C�C�C�C�
=C���C���C���C�  C�C�C�
=C�C���C�  C�  C�  C�  C�C�  C�  C�C�C���C���C���C���C�  C�  C���C�  C�  C���C���C�  C�C�  C�  C�C�  C�  C�C�C���C�  C�  C�  C�  C�  C�C���C�  C�
=C�
=C�  C���C�  C�
=C�C�C�C�  C���C���C���C�C�C�  C���C���C���C���C���C���C���C���C���C���C�  C���C�  C�  C���C���C�  C�  C���C�  C�  C�C�C�  C�  C���D � D�D� D  D}qD�qD��D  D� D  D}qD�D��D  D� D�qD� D	  D	� D
  D
}qD  D}qD�qD� D  D��D�D� D  D� D  D� D  D�D  Dz�D�qD� D�qDxRD��D� D�qDz�D�qD��D  D}qD�qD� D�D�D  D� D  D� D�D� D�qD� D�D� D   D � D �qD!}qD"  D"� D#�D#� D$�D$��D$�qD%z�D&  D&�D'D'� D(  D(� D(�qD)}qD*  D*� D+  D+��D,D,� D,�qD-}qD-�qD.��D/�D/��D0  D0� D0�qD1� D2  D2��D3�D3� D4�D4� D4�qD5� D5�qD6z�D6�qD7� D8  D8� D9  D9� D:�D:� D:�qD;}qD<  D<�D=�D=� D=��D>� D?  D?� D@  D@}qD@�qDA}qDA��DB}qDB�qDCz�DD  DD��DD��DE}qDF�DF��DG�DG� DG�qDH��DI  DIz�DI��DJ}qDK  DK��DL�DL� DM  DM��DN  DN}qDO  DO��DP�DP� DP�qDQ� DR  DR}qDS�DS� DS��DTz�DT�qDU� DV  DV�DW�DW}qDW�qDX� DY  DY��DZ�DZ}qD[  D[��D\  D\}qD\�qD]}qD^�D^��D_  D_��D_�qD`z�D`�qDa��Db  Db}qDb�qDcz�Dd  Dd� De�De}qDe�qDf� Dg  Dg��Dh�Dh�Di�Diz�Di��Djz�Dj�qDk}qDk��Dl� Dm  Dm� DnDn� Dn��Doz�Dp  Dp� Dq�Dq��Dr  Dr��Ds  Ds� Dt  Dt}qDt�qDu� Dv  Dv��Dw�Dw� Dx  Dx��Dy  Dy� Dz�Dz�D{�D{�D|D|��D}�D}��D~�D~��D  D}qD�  D�AHD�~�D�� D�HD�>�D�~�D�� D�  D�AHD��HD���D��qD�@ D�� D��qD��qD�>�D��HD�D�  D�>�D�~�D���D�  D�=qD�~�D���D�  D�B�D�� D���D�  D�AHD���D�� D��D�@ D�� D�� D�  D�>�D�~�D���D��qD�>�D�~�D�� D�HD�AHD���D�� D�  D�B�D��HD�� D�HD�AHD�� D��HD�HD�AHD�� D���D�  D�@ D���D�� D���D�>�D�~�D���D�  D�>�D�� D��HD��D�AHD�� D�� D�  D�>�D�� D��HD�  D�>�D�� D�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D�� D���D�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�B�D�l�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?\)?aG�?�\)?���?\?��@�@�@!G�@5@B�\@L��@c�
@xQ�@�  @���@�z�@��H@�G�@�{@�33@�(�@Ǯ@�{@�z�@�G�@�@�\)@��HAG�Az�A	��A\)A�AA(�A\)A#�
A)��A,(�A0��A6ffA:=qA=p�AC33AHQ�AJ�HAP��AS�
AW
=AZ�HA`��Ac33Ag�Al��Ap  As33Ay��A|��A�Q�A��\A��A�
=A�Q�A��\A�p�A��RA���A�33A�z�A��RA�G�A�=qA�z�A�
=A���A��A�z�A�ffA��A��A��A���A�\)A�G�A�=qA���A��RA��A��A�z�A�ffA��A���A�(�A�p�AǮA�=qA��
A�p�AϮA��A��HA�p�A׮A�G�A��HA���A�
=A�G�A�\A�z�A�
=A��A��A�(�A�ffA�  A�A�A�{A�Q�A���A�z�A��RB (�B ��BffB\)B  BG�B�\B33B(�B	p�B
�\B33B(�BG�BffB�HB(�Bp�B=qB�HB(�BG�B{B�RB  B�BB�HB  B��B��B�RB�B (�B!p�B"ffB#
=B$(�B%p�B&{B'
=B(Q�B)G�B*{B*�HB,(�B-G�B.{B.�HB/�
B1�B1�B2�\B4  B5�B5B6�\B7�
B8��B:{B:�HB<  B=G�B>{B>�HB@Q�BAp�BB{BC33BDz�BE��BF=qBG\)BH��BI��BJ=qBK�BL��BMp�BN�RBO�
BP��BQBR�\BS�
BT��BU��BV�\BW�
BX��BYBZ�\B[�
B\��B]B^�RB`(�Ba�BaBb�HBd(�BeG�Bf=qBg
=BhQ�Bi��Bj=qBk\)Bl��Bm��BnffBo�Bp��Bq��Br�\Bs�Bt��Bu�Bv�RBw�
By�Bz=qB{
=B|  B}G�B~=qB33B�(�B���B�G�B��B�(�B���B�p�B��
B�Q�B���B���B�{B�z�B��B��
B�=qB���B�G�B��B�Q�B��RB�p�B�  B�ffB��HB���B�{B�z�B�33B��
B�=qB���B�p�B��B�Q�B�
=B��B�{B�z�B��B�B�=qB���B�G�B�  B�z�B��HB�p�B�(�B��RB��B�B�z�B���B�p�B�  B���B�G�B�B�=qB���B��B��B�ffB�
=B��B�(�B���B�G�B��B�z�B��HB�\)B�  B��RB�33B���B�(�B��HB��B�{B��\B�
=B���B�=qB���B��B��B�z�B�33B��
B�Q�B��RB�p�B�{B��\B�
=B���B�=qB��HB�p�B��
B�ffB��B�B�=qB��RB�\)B�{B��RB��B��B�ffB��B��B�{B���B�\)B�{B�z�B���B�B�Q�B��HB�\)B�{BƸRB��BǮB�ffB�
=B�p�B�{B���B�33B�B�z�B�
=BͅB�=qBΣ�B�G�B�  BЏ\B�
=BхB�Q�B���B�\)B�  B���BՅB�{B֣�B�G�B�(�Bأ�B�33B�  Bڣ�B��B�  BܸRB�33B��
Bޣ�B��B߮B��\B��BᙚB�Q�B���B�B�(�B��B�p�B�{B��B��B��
B�z�B�
=B�B�=qB���B�\)B��B�RB�G�B�B�z�B�33BB�=qB��HB�B�{B�\B�G�B�  B�Q�B�
=B�B�=qB��RB��B�(�B���B�G�B��B���B�33B���B�ffB��B��B�(�B���B�G�C   C ffC ��C �CG�C��C�C(�Cz�C�
C�CQ�C�RC
=C=qC�C�
C33Cp�C�C  C\)C��C�HC(�C�\C�
C
=CQ�C�C�C	(�C	p�C	��C

=C
=qC
�C
�HC33Cz�C�RC  CQ�C��C�HC�C\)C�C{CQ�C�C�
C(�Cz�C�C��CQ�C��C�
C
=CQ�C�C  C33Cp�C�RC
=CQ�C��C�HC�C\)C��C  CQ�C�\CC
=C\)C�C��C=qC�C�RC��C33C�C�
C�C\)C�\C�
C(�C�C�RC��C33C�C�
C�CQ�C�\C�
C(�Cp�C��C�
C�Cz�C�RC��C(�Cp�C�RC 
=C Q�C ��C �C!(�C!ffC!��C!�HC"(�C"z�C"C#�C#ffC#��C#�HC$33C$�\C$�
C%�C%ffC%�C%��C&(�C&ffC&�C&��C'=qC'�\C'�HC(�C(\)C(��C(�
C){C)\)C)�C*  C*=qC*�\C*�HC+�C+\)C+��C+�HC,�C,p�C,�RC-  C-Q�C-��C-�HC.(�C.p�C.��C.�C/=qC/�C/��C0�C0ffC0�RC1  C1G�C1�\C1�
C2�C2\)C2�C2��C3G�C3�\C3�
C4{C4ffC4��C4�C533C5�C5��C6{C6ffC6�C7
=C7Q�C7��C7�C8=qC8�\C8�
C9{C9\)C9��C9��C:33C:�C:��C;�C;ffC;C<
=C<\)C<��C<��C==qC=�\C=�
C>(�C>p�C>�RC?  C?G�C?��C?�C@33C@�C@�
CA(�CA�CA�
CB(�CB�CB�
CC�CCp�CC�RCD
=CDG�CD��CD�CE=qCE�\CE�CFG�CF��CF�CG=qCG��CG�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333333                                                                                                 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A��A���A���A���A���A���A���A��A��A��A��;A�^5A���A�9XA�ZA�5?A��A�ƨA͡�A�p�A̗�A��A�;dA�  A�n�A�G�A� �AȬAȓuAȃA�dZA�AǶFA�v�A�M�A�{A�"�A�O�A�z�AǋDAǓuA���A��AǙ�A�  A��AđhA�z�APA�ffA�v�A�O�A��A���A�dZA�A�ȴA�|�A��-A�JA���A���A�Q�A���A��9A�S�A�jA�r�A��A�7LA��#A�+A��A�$�A��FA��A�A���A�
=A���A�$�A�~�A�A��wA��+A�v�A���A��A���A���A��A�$�A�$�A�p�A��^A�{A�A{|�Aw��Aq�
Ap��Ak�PAg��Afz�Ad��Ac�Ac&�AbE�Aa�;A_�mA\ĜAZffAXI�AU�
AUC�AT1'AR�AO��AK�;AJn�AG��AGK�AGAF5?AD�A@~�A?�TA?S�A>�DA<�A9`BA5��A4bNA3�PA1��A0A�A.9XA+�
A*��A)ƨA&�/A%�A#XA"�\A!��A��A�A�;A|�A��A5?A��An�A��A�A��AZAbAt�A$�AdZA��AbNA|�AȴA�wA;dA
�`A
��A
VA	�A	?}AQ�A �A�-A`BAA��AQ�A�TAC�A��Ar�A�A|�AȴA �yA E�A �A 1A JA @��!@�  @���@�ȴ@���@�1@�ȴ@�hs@�b@�C�@�x�@��@�ƨ@��@�E�@�7@��@�\)@�V@噚@���@�b@�t�@�ȴ@�!@��@��u@�ƨ@�K�@�M�@ܼj@۶F@�33@�/@ם�@�o@�5?@Չ7@�V@�ƨ@ѩ�@��@�l�@�ȴ@�v�@Ͳ-@�bN@��@�1'@�C�@Ο�@ϝ�@�-@�33@�hs@��#@љ�@щ7@ёh@љ�@���@��m@��@�E�@�X@̛�@�b@��@���@˥�@�S�@���@�5?@��@��#@�@ɉ7@�G�@�%@�r�@���@�b@ǍP@��H@�V@��@�@ŉ7@�`B@�&�@���@�r�@öF@�dZ@�dZ@�+@�~�@���@�V@��/@���@��D@�  @�ƨ@���@�+@��H@��!@���@��+@�~�@�=q@�@�?}@��9@�Q�@�1@�t�@���@��T@��^@�7L@�Ĝ@���@���@��u@��D@��@�r�@��@��@�\)@�o@���@���@���@�x�@��/@��@��m@�t�@�;d@�o@���@��!@�V@�@��T@���@�7L@��@�V@��9@��@�A�@��@���@�K�@�+@��@���@��+@�E�@���@��7@��@��`@��u@��m@��P@�K�@�33@��y@���@�M�@��@�&�@�r�@�Z@�I�@��@��
@���@��@���@�O�@���@��D@���@��@�|�@��y@�{@���@�`B@�7L@��@��j@���@�Z@�  @�t�@��@�
=@��y@���@�ff@��@��h@�O�@��@���@���@� �@�ƨ@�t�@�l�@�\)@�;d@��@�o@���@���@��R@��!@��!@���@��+@�v�@�=q@�@�G�@�&�@��j@�9X@���@�ƨ@��@�t�@�|�@�K�@�@��R@�V@�-@�J@���@�V@�r�@�I�@� �@�  @��@�t�@���@��H@��@���@���@�V@���@��@��@�@��h@��@�X@�&�@��@��j@���@�z�@�(�@��@��w@�K�@�5?@���@�x�@�?}@��@�%@���@���@�j@�bN@�Z@� �@���@�ƨ@��@���@��P@��P@��@��P@��P@���@��!@�~�@��@���@�@���@�O�@�%@��@�z�@�  @��P@�S�@�+@�
=@��H@���@�~�@�^5@�@��7@�`B@�G�@�7L@�&�@��@��@� �@��@���@��@���@�|�@�C�@��@�ȴ@���@���@��+@�n�@��T@��@�7L@�%@��@��/@���@��9@�r�@�bN@�I�@�(�@�b@��@|�@l�@\)@~�@~v�@~$�@}`B@}�@|��@{ƨ@{S�@z�@z�!@z�\@zn�@z=q@z�@y��@xr�@w�@w�w@w�@w�P@wl�@wl�@wK�@v��@vv�@v$�@u�-@u?}@t�/@t��@tj@s��@s��@s"�@r��@rM�@r=q@r=q@r�@rJ@q��@q��@q��@qX@pĜ@pA�@p  @o��@o\)@o;d@o�@nE�@m�-@mO�@mV@mV@l�@l�/@l�D@l9X@k��@kƨ@k��@kS�@ko@j��@j-@i��@iX@i7L@i�@hĜ@hbN@g��@g\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A���A���A��A��A���A��A��A��A��A��A���A���A��A���A���A���A���A���A���A���A���A��A��A���A��A��A���A���A��A���A���A��A���A���A��A���A���A��A��A���A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A��A��A��A��A��yA��mA��HA��TA��;A��;A��HA��;A��#A��/A��#A��#A���AѼjAыDA�33A�/A� �A�
=A�A�  A��A���A���A�ȴAжFAа!AЩ�AЏ\A�t�A�E�A�?}A�-A�JA��A��#A϶FAϑhA�z�A�jA�A�A�&�A�$�A�oA���A�n�A�C�A�;dA��A�1A�A���A���A��A���A��A��mA��`A��`A��;A��#A���A���A�A�A�ĜA���Aͺ^Aͺ^A͸RAͰ!A͡�A͝�A͙�A͏\A͏\A͏\A̓A�|�A�|�A�z�A�t�A�t�A�hsA�K�A�oA��TA̬A̍PÁA�z�A�jA�XA�Q�A�G�A�/A�&�A��A�{A���A��yA��`A˶FA�hsA�C�A�-A� �A�1A��AʮAʋDA�VA��A�ĜAɮAə�Aɏ\Aɕ�AɓuA�|�A�n�A�hsA�S�A�O�A�S�A�S�A�O�A�M�A�Q�A�K�A�?}A�1'A�33A�5?A�/A�/A�33A�1'A� �A���A���A���AȰ!Aȴ9Aȩ�Aȥ�Aȟ�Aȝ�Aȟ�Aȝ�Aȕ�AȑhAȓuAȑhAȍPAȋDAȍPAȉ7AȁAȃAȅA�~�A�z�A�v�A�v�A�t�A�n�A�n�A�ffA�XA�K�A�?}A�9XA��A�  A���A���A��A��;A���A�ȴA���AǺ^Aǰ!Aǡ�AǛ�AǑhAǅAǁA�~�A�t�A�hsA�bNA�ffA�bNA�^5A�dZA�ffA�S�A�/A��A�JA�bA��A��A�{A�bA�bA�VA�JA�{A��A��A�/A�;dA�?}A�G�A�C�A�E�A�M�A�S�A�S�A�ZA�r�A�v�A�r�A�z�A�|�AǇ+AǃAǅAǉ7AǍPAǋDAǇ+Aǉ7AǍPAǋDAǋDAǏ\AǓuAǓuAǏ\AǗ�Aǡ�Aǣ�Aǰ!A�ĜA���A��mA���A�A�1A��A�&�A�$�A� �A��A�JA��yA���Aǰ!AǑhA�|�A�ffA�XA�E�A�5?A� �A�1A��TA�ƨAƶFAơ�AƁA�l�A�1'A��;Aš�A�^5A��A��HAġ�A�z�A�S�A�/A�VA��A�ĜAÝ�A�l�A�&�A�A��mA¾wA¥�A�AA�`BA�A�A��A�A��/A���A�n�A��
A�ffA�A�A���A��FA��A�`BA�%A��/A�`BA�-A���A�jA�ĜA���A�dZA�1'A��-A�oA���A�ȴA�hsA�/A���A��A���A���A�ƨA��-A���A�t�A�bNA�M�A�5?A�$�A��A�oA�1A�A���A��TA��
A���A���A�ƨA��wA��^A��!A���A��7A��7A�x�A�ffA�S�A�VA��mA��#A�ƨA��hA�dZA�VA�C�A�-A��A���A��A��;A���A���A��^A���A���A���A�\)A�9XA�?}A�?}A�A���A��A��FA���A��A�^5A�G�A��A�JA���A���A��jA��A��\A�O�A�  A��jA��RA��9A��A��A��-A��A���A�~�A�bNA�7LA�bA��;A�ȴA���A��uA�v�A�?}A�oA��A���A�z�A�n�A�l�A�dZA�ZA�K�A�?}A�7LA�+A��A�JA���A��`A��RA� �A��RA�1'A���A�I�A��A�jA��A��yA���A��-A���A�x�A���A���A�Q�A�1'A�VA���A�p�A�;dA���A��A�l�A�O�A�+A��A���A��RA�~�A�K�A�JA��jA��A�ZA�%A�S�A���A���A�bA��jA�`BA�+A�VA���A��yA��A�ȴA��A��\A�|�A�t�A�dZA�7LA�ĜA�G�A��wA�ffA��yA��!A��\A�\)A�-A���A��#A���A�=qA��HA�ĜA�A��wA��\A�oA��hA��`A��hA��hA�v�A�{A���A���A���A�ȴA��-A��\A�Q�A�5?A���A���A�jA�;dA��A��A�{A�bA�JA�1A�  A���A��A��mA��HA��/A���A�A��RA���A��hA�bNA���A��A�~�A�ƨA�S�A�&�A���A��`A���A���A��DA��
A�x�A�`BA�Q�A�?}A��A��A���A��A�hsA��A��hA��A�&�A�A�z�A�{A�A���A�?}A���A���A�7LA��A���A�A���A���A��A�r�A�ffA�S�A�-A�A���A���A��PA�z�A�ffA�O�A�1'A��A���A�ȴA��hA��yA�t�A�{A��\A�|�A�z�A�z�A�x�A�x�A�t�A�r�A�l�A�A�A��^A�ffA�dZA�$�A�1A��HA��uA�\)A�E�A�33A�
=A��wA���A�v�A�C�A�bA�mA�PAVA~A�A}S�A|�jA{�FAzjAyhsAx��Axr�AxM�Ax5?Aw��Av��At�AsXArE�Aql�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333333                                                                                                 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A��A��A���A���A���A���A���A���A��A��A��A��;A�^5A���A�9XA�ZA�5?A��A�ƨA͡�A�p�A̗�A��A�;dA�  A�n�A�G�A� �AȬAȓuAȃA�dZA�AǶFA�v�A�M�A�{A�"�A�O�A�z�AǋDAǓuA���A��AǙ�A�  A��AđhA�z�APA�ffA�v�A�O�A��A���A�dZA�A�ȴA�|�A��-A�JA���A���A�Q�A���A��9A�S�A�jA�r�A��A�7LA��#A�+A��A�$�A��FA��A�A���A�
=A���A�$�A�~�A�A��wA��+A�v�A���A��A���A���A��A�$�A�$�A�p�A��^A�{A�A{|�Aw��Aq�
Ap��Ak�PAg��Afz�Ad��Ac�Ac&�AbE�Aa�;A_�mA\ĜAZffAXI�AU�
AUC�AT1'AR�AO��AK�;AJn�AG��AGK�AGAF5?AD�A@~�A?�TA?S�A>�DA<�A9`BA5��A4bNA3�PA1��A0A�A.9XA+�
A*��A)ƨA&�/A%�A#XA"�\A!��A��A�A�;A|�A��A5?A��An�A��A�A��AZAbAt�A$�AdZA��AbNA|�AȴA�wA;dA
�`A
��A
VA	�A	?}AQ�A �A�-A`BAA��AQ�A�TAC�A��Ar�A�A|�AȴA �yA E�A �A 1A JA @��!@�  @���@�ȴ@���@�1@�ȴ@�hs@�b@�C�@�x�@��@�ƨ@��@�E�@�7@��@�\)@�V@噚@���@�b@�t�@�ȴ@�!@��@��u@�ƨ@�K�@�M�@ܼj@۶F@�33@�/@ם�@�o@�5?@Չ7@�V@�ƨ@ѩ�@��@�l�@�ȴ@�v�@Ͳ-@�bN@��@�1'@�C�@Ο�@ϝ�@�-@�33@�hs@��#@љ�@щ7@ёh@љ�@���@��m@��@�E�@�X@̛�@�b@��@���@˥�@�S�@���@�5?@��@��#@�@ɉ7@�G�@�%@�r�@���@�b@ǍP@��H@�V@��@�@ŉ7@�`B@�&�@���@�r�@öF@�dZ@�dZ@�+@�~�@���@�V@��/@���@��D@�  @�ƨ@���@�+@��H@��!@���@��+@�~�@�=q@�@�?}@��9@�Q�@�1@�t�@���@��T@��^@�7L@�Ĝ@���@���@��u@��D@��@�r�@��@��@�\)@�o@���@���@���@�x�@��/@��@��m@�t�@�;d@�o@���@��!@�V@�@��T@���@�7L@��@�V@��9@��@�A�@��@���@�K�@�+@��@���@��+@�E�@���@��7@��@��`@��u@��m@��P@�K�@�33@��y@���@�M�@��@�&�@�r�@�Z@�I�@��@��
@���@��@���@�O�@���@��D@���@��@�|�@��y@�{@���@�`B@�7L@��@��j@���@�Z@�  @�t�@��@�
=@��y@���@�ff@��@��h@�O�@��@���@���@� �@�ƨ@�t�@�l�@�\)@�;d@��@�o@���@���@��R@��!@��!@���@��+@�v�@�=q@�@�G�@�&�@��j@�9X@���@�ƨ@��@�t�@�|�@�K�@�@��R@�V@�-@�J@���@�V@�r�@�I�@� �@�  @��@�t�@���@��H@��@���@���@�V@���@��@��@�@��h@��@�X@�&�@��@��j@���@�z�@�(�@��@��w@�K�@�5?@���@�x�@�?}@��@�%@���@���@�j@�bN@�Z@� �@���@�ƨ@��@���@��P@��P@��@��P@��P@���@��!@�~�@��@���@�@���@�O�@�%@��@�z�@�  @��P@�S�@�+@�
=@��H@���@�~�@�^5@�@��7@�`B@�G�@�7L@�&�@��@��@� �@��@���@��@���@�|�@�C�@��@�ȴ@���@���@��+@�n�@��T@��@�7L@�%@��@��/@���@��9@�r�@�bN@�I�@�(�@�b@��@|�@l�@\)@~�@~v�@~$�@}`B@}�@|��@{ƨ@{S�@z�@z�!@z�\@zn�@z=q@z�@y��@xr�@w�@w�w@w�@w�P@wl�@wl�@wK�@v��@vv�@v$�@u�-@u?}@t�/@t��@tj@s��@s��@s"�@r��@rM�@r=q@r=q@r�@rJ@q��@q��@q��@qX@pĜ@pA�@p  @o��@o\)@o;d@o�@nE�@m�-@mO�@mV@mV@l�@l�/@l�D@l9X@k��@kƨ@k��@kS�@ko@j��@j-@i��@iX@i7L@i�@hĜ@hbN@g��@g\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A���A���A��A��A���A��A��A��A��A��A���A���A��A���A���A���A���A���A���A���A���A��A��A���A��A��A���A���A��A���A���A��A���A���A��A���A���A��A��A���A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A��A��A��A��A��yA��mA��HA��TA��;A��;A��HA��;A��#A��/A��#A��#A���AѼjAыDA�33A�/A� �A�
=A�A�  A��A���A���A�ȴAжFAа!AЩ�AЏ\A�t�A�E�A�?}A�-A�JA��A��#A϶FAϑhA�z�A�jA�A�A�&�A�$�A�oA���A�n�A�C�A�;dA��A�1A�A���A���A��A���A��A��mA��`A��`A��;A��#A���A���A�A�A�ĜA���Aͺ^Aͺ^A͸RAͰ!A͡�A͝�A͙�A͏\A͏\A͏\A̓A�|�A�|�A�z�A�t�A�t�A�hsA�K�A�oA��TA̬A̍PÁA�z�A�jA�XA�Q�A�G�A�/A�&�A��A�{A���A��yA��`A˶FA�hsA�C�A�-A� �A�1A��AʮAʋDA�VA��A�ĜAɮAə�Aɏ\Aɕ�AɓuA�|�A�n�A�hsA�S�A�O�A�S�A�S�A�O�A�M�A�Q�A�K�A�?}A�1'A�33A�5?A�/A�/A�33A�1'A� �A���A���A���AȰ!Aȴ9Aȩ�Aȥ�Aȟ�Aȝ�Aȟ�Aȝ�Aȕ�AȑhAȓuAȑhAȍPAȋDAȍPAȉ7AȁAȃAȅA�~�A�z�A�v�A�v�A�t�A�n�A�n�A�ffA�XA�K�A�?}A�9XA��A�  A���A���A��A��;A���A�ȴA���AǺ^Aǰ!Aǡ�AǛ�AǑhAǅAǁA�~�A�t�A�hsA�bNA�ffA�bNA�^5A�dZA�ffA�S�A�/A��A�JA�bA��A��A�{A�bA�bA�VA�JA�{A��A��A�/A�;dA�?}A�G�A�C�A�E�A�M�A�S�A�S�A�ZA�r�A�v�A�r�A�z�A�|�AǇ+AǃAǅAǉ7AǍPAǋDAǇ+Aǉ7AǍPAǋDAǋDAǏ\AǓuAǓuAǏ\AǗ�Aǡ�Aǣ�Aǰ!A�ĜA���A��mA���A�A�1A��A�&�A�$�A� �A��A�JA��yA���Aǰ!AǑhA�|�A�ffA�XA�E�A�5?A� �A�1A��TA�ƨAƶFAơ�AƁA�l�A�1'A��;Aš�A�^5A��A��HAġ�A�z�A�S�A�/A�VA��A�ĜAÝ�A�l�A�&�A�A��mA¾wA¥�A�AA�`BA�A�A��A�A��/A���A�n�A��
A�ffA�A�A���A��FA��A�`BA�%A��/A�`BA�-A���A�jA�ĜA���A�dZA�1'A��-A�oA���A�ȴA�hsA�/A���A��A���A���A�ƨA��-A���A�t�A�bNA�M�A�5?A�$�A��A�oA�1A�A���A��TA��
A���A���A�ƨA��wA��^A��!A���A��7A��7A�x�A�ffA�S�A�VA��mA��#A�ƨA��hA�dZA�VA�C�A�-A��A���A��A��;A���A���A��^A���A���A���A�\)A�9XA�?}A�?}A�A���A��A��FA���A��A�^5A�G�A��A�JA���A���A��jA��A��\A�O�A�  A��jA��RA��9A��A��A��-A��A���A�~�A�bNA�7LA�bA��;A�ȴA���A��uA�v�A�?}A�oA��A���A�z�A�n�A�l�A�dZA�ZA�K�A�?}A�7LA�+A��A�JA���A��`A��RA� �A��RA�1'A���A�I�A��A�jA��A��yA���A��-A���A�x�A���A���A�Q�A�1'A�VA���A�p�A�;dA���A��A�l�A�O�A�+A��A���A��RA�~�A�K�A�JA��jA��A�ZA�%A�S�A���A���A�bA��jA�`BA�+A�VA���A��yA��A�ȴA��A��\A�|�A�t�A�dZA�7LA�ĜA�G�A��wA�ffA��yA��!A��\A�\)A�-A���A��#A���A�=qA��HA�ĜA�A��wA��\A�oA��hA��`A��hA��hA�v�A�{A���A���A���A�ȴA��-A��\A�Q�A�5?A���A���A�jA�;dA��A��A�{A�bA�JA�1A�  A���A��A��mA��HA��/A���A�A��RA���A��hA�bNA���A��A�~�A�ƨA�S�A�&�A���A��`A���A���A��DA��
A�x�A�`BA�Q�A�?}A��A��A���A��A�hsA��A��hA��A�&�A�A�z�A�{A�A���A�?}A���A���A�7LA��A���A�A���A���A��A�r�A�ffA�S�A�-A�A���A���A��PA�z�A�ffA�O�A�1'A��A���A�ȴA��hA��yA�t�A�{A��\A�|�A�z�A�z�A�x�A�x�A�t�A�r�A�l�A�A�A��^A�ffA�dZA�$�A�1A��HA��uA�\)A�E�A�33A�
=A��wA���A�v�A�C�A�bA�mA�PAVA~A�A}S�A|�jA{�FAzjAyhsAx��Axr�AxM�Ax5?Aw��Av��At�AsXArE�Aql�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333333                                                                                                 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�BVB�B�B"B�B�B�B�B�B�B�BVB�B$@B/�B?HBZ�B]/B[WB[#BZQBe�BjKBt�B�uB�{B�_B�JB�:B�uB�FB��B��B��B��B��B�MB\B%FB0�B49B6BG�BlWBsBn�Bd&BV�BQBH�BPBb�By�B|�B�YB��B|B{JB{�B�B�uB�'B�hBںB�B�)B�B��B��B�QB�;B��B��B��B�MBhsB9�B,�B�B�WB��B��B��B�"B�BxBO�B2�B�B
�TB
ٴB
�NB
��B
�[B
�B
�=B
f2B
T�B
GB
'�B
�B	�vB	�fB	��B	��B	��B	��B	��B	�kB	�uB	� B	}"B	c�B	]/B	E�B	?}B	:�B	(�B	#�B	;B�>B�5B�yB��B�B�B��B��B��B�OB�^B��B�OB�:B��B��B��B��B�=B��B~]B��Bu�Bw�Br�Br|Bm�Bp�Bu�By>B}"B~�B�B��B�B��B��B��B�1B�rB��B��B�7B��B�B�+B�=B�rB�	B�	B��B�_B��B�B�B�B�B��B�B��B��B��B�B�B�JB��B�lB�1B��B�B�BcB~�B~(B{�Bq�Bp�Bo�BsBu�ByrB{�B}�B��B��B�FB�_B��B��B�UB�UB�3B�B�?B�zB�B�B�FB�?B��B��B�B��B�aB�3B�3BʌB�^B��B�0B�0B��B�vBбB��B��B� B�BچBܒB�B��B�iB	�B	_B	_B	#nB	7�B	=�B	EB	NpB	PB	P�B	T�B	S�B	T�B	UgB	YKB	\�B	_B	aB	a|B	a|B	e,B	f�B	h>B	iyB	iyB	jB	l�B	m�B	m�B	qB	u%B	v�B	x�B	z�B	{�B	|�B	|�B	}VB	}�B	~�B	cB	��B	�%B	��B	�B	�B	��B	��B	��B	��B	�"B	��B	��B	�@B	�FB	��B	�YB	��B	��B	�_B	��B	�eB	�=B	�~B	��B	�bB	��B	��B	��B	��B	�=B	�OB	��B	�'B	��B	��B	��B	��B	�9B	��B	�LB	�RB	��B	�*B	��B	�0B	�<B	��B	��B	��B	�?B	ƨB	ǮB	ȀB	��B	�XB	��B	˒B	��B	�BB	бB	��B	ԕB	�2B	�gB	�9B	�
B	�EB	�yB	�yB	��B	ٴB	�#B	ޞB	�B	�|B	�B	�B	�B	�B	� B	�B	�ZB	��B	�`B	�B	�>B	�B	�B	�B	�B	��B	��B	�B	� B	��B	�B	�B	�B	�B	�B	�B	�B	��B	�ZB	��B	��B	��B	�`B	��B	��B	�lB	�>B	��B	�>B	��B	�DB	�B	�B	��B	�"B	��B	�VB	��B	��B	��B	�]B	��B	��B	��B	��B	�.B	��B	�cB	��B	�.B	�cB	�cB	�cB
  B
 �B
;B
B
uB
B
GB
�B
�B
�B
�B
�B
�B
�B
+B
�B
+B
�B

=B

�B

rB

=B

rB
xB
~B
�B
"B
�B
�B
�B
�B
4B
�B
�B
�B
B
B
�B
B
@B
�B
�B
FB
�B
�B
SB
B
FB
{B
�B
B
�B
�B
�B
�B
�B
$B
YB
YB
�B
�B
$B
�B
�B
�B
�B
kB
=B
kB
�B
�B
�B
7B
7B
B
B
�B
�B
 'B
!-B
!bB
!�B
"4B
"�B
#B
#nB
#nB
$B
$�B
%�B
%zB
%�B
%�B
%�B
%zB
&�B
&LB
&B
&B
%�B
%�B
%�B
&�B
&LB
&�B
'RB
&�B
&�B
&B
'B
&�B
'�B
'�B
'�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
(XB
(�B
(�B
(�B
(�B
)�B
)�B
)_B
*0B
)�B
)�B
*�B
*�B
+B
+6B
+6B
+kB
+6B
+kB
+kB
,�B
,B
+�B
+�B
+�B
+�B
+�B
,=B
-�B
-�B
.IB
.}B
.�B
.�B
/OB
/�B
0!B
0!B
0UB
0�B
0�B
0�B
0UB
0�B
0�B
0�B
0�B
0�B
0�B
1'B
1[B
1[B
1[B
1�B
1�B
1[B
2�B
2�B
2�B
33B
2�B
2�B
33B
3hB
3�B
3�B
4B
49B
49B
4nB
4�B
5B
5?B
5tB
5tB
5�B
6FB
6�B
7�B
8G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�BBBB�BxB~B"BBxB�B"BJB�BB�B�BPB�B�B�B�B~B(B�B�BB(B�B�B�B"BPB�B�B�B�B�BPB�B�B�B�B\B�B"B(BB�B(B�BB\BVBB�B�B�B(B(B�B�B�BbB�B\B\B"BPB\BVB�B�B�B�B�B�B�B�BbBVB(B�B�B"B�B�BB�B�B�BIB0�B�BOB"�B!bB�B"4B#:B$tB!�B(XB%�B%�B*eB,B1�B.IB/�B2�B5?B4�B7�B>�B;�B<6BE9BB[BA BF?BQ�BY�BW?BVmB^�B]�B[�B]dB^�B]�B\]B_;B]�B\�BZ�B[�B^B[�BZ�B\]B[�BY�BZ�B[�BZ�BZB[�B]�B[#B[#B[WBZ�BYB\]BZQBX�BXyBY�BX�BYB\�Bf�Bc�BgmBf2Bd�Bc�Bf�Bh>Bf�Bh>BjKBiyBjBjBl�Bl�Bl"Bq�BpBtTBs�BrBwfB�B�B��B��B�B�+B~�B�iB��B~�BcB��B�%B�{B��B�%B��B��B�YB��B�SB�YB�=B��B�=B�lB�DB��B�1B��B��B�.B��B�oB�uB��B�:B��B��B�@B�:B�B�FB��B�@B��B�B��B��B�FB��B��B�@B�B��B��B�@B��B��B�{B�MB�B�1B��B��B�LB��B��B��B�B�[B��B��B��B��B��B��B�$B��B��B�0B�*B��B�HB�}B��B�aB�gBŢBȴB͟B�pBӏB�WB�B�B�B��B�PB�VB  B%BSBB�B�B�B!B#nB&B$@B$tB'�B'�B(�B,�B/�B0�B/�B.�B.�B4�B0UB49B2�B4B5tB4�B3hB4nB6zB5tB4�B5tB7B8RB8�B8�B;�B?�BH�BF�BW�BU�B\�B_pBlWBqBq�Br|BsMBt�Bs�Bu�Bs�Br�BrBo�Bo5BrBpBn/Bn�Bl"Bk�Bj�BiDBc�BjBcTB_pBZB[#BZB\�BQBS�BS�BS[BV9BR BQ�BT,BN�BI�BG�BJXBGzBE�BG�BI�BJ#BJ#BF�BK)BNpBJXBgmBT�BU2B`B^�B`�B`Bl�BhsB.Bv`Bu%B�fBx�Bv�Bv�Bu%B�xBy�Bv�B.B�oBzB�;B��B��B�VB�B�4B��B�AB�B~�B��B}�B{�B{�B|By�B}�B|�B|�BzxBz�B|PB|�B{�B|B~�BzDBzB|�B{B}VB�%B|PBy	B{�B�4B��B�rB��B��B��B��B��B�B�~B��B��B��B��B��B�B�eB�B��B�FB��B��B�dB��B�2B��B�yB��B�&B�B��B�B�B�8B�B��B� B�"B�WB��B�"B�KB�QB��B�B�B��B�ZB� B� B�B�cB� B�iB�B�B�8B�fB��B�;B�B�]B�dB��B�WB�yB�B�EB�EB�#B�HB�B�,B��BϫBƨBÖB�B�B�3B��B�B��B�IB�tB�-B��B�'B��B�kB��B��B��B�_B��B��B��B�{B��B�\B�fB��B��B�B}�Bu%B}"BzBh�Bg�BffBU�BOvBA�B=<B:�B8�B6B7B1'B1�B,B+B*�B,�B7�B%zB)*B�B7BGBMB�]B�B��B�B��B�B�lB�ZB�pB�jB��B��B��B�B�EBĜB�XBʌB՛B�B��B��B��B�CB�B�FB��B�RB��B�OB��B�(B�(B��B�\B��B��B��B�B�DB�lB�fB��B�B��B��B�B��B|�Bu�B��B�BY�BV�BTaBN�BMjBJXBV9BffB;�B9�B5tB3�B7�B/�B.IB-�B0�B�BA�B"�BB�B	BGB
�fB
�%B
�xB
��B
�B
�B
�HB
�dB
��B
�B
خB
�sB
��B
��B
��B
�B
�TB
֡B
�^B
�KB
�?B
�B
�'B
��B
�wB
��B
�jB
��B
�HB
�RB
��B
�-B
�IB
�kB
�eB
�+B
�B
��B
��B
�hB
�1B
�CB
�'B
��B
t�B
q�B
qB
u�B
l�B
f�B
c�B
g8B
cTB
\]B
^5B
]�B
X�B
OB
T,B
K�B
_pB
H�B
C�B
NB
HKB
EmB
,qB
#nB
$@B
xB
�B
8�B
?�B
�B
�B
_4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                 44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B	7B	7B	7B	7B	7B
�B
	B
=B
rB
=BB
�B
=BB
�BB
�B*B �B+�B;�BW
BYBW�BWsBV�BbBf�BqAB~�B�B��B��B��B��B��B�B�B�9B�#B�?B�B�B!�B,�B0�B2aBC�Bh�BoiBkB`vBS&BMjBD�BLdB_Bu�By	B��B|�BxlBw�Bx7B{�B��B�wB��B�
B�lB�yB��B�JB�/B֡BۋB�B��B�=B��Bd�B5�B)*BB�B�2B�B�B�rB�oBtSBK�B.�BB
�B
�B
͞B
�<B
��B
�hB
��B
b�B
P�B
CaB
#�B
B	��B	�B	� B	�?B	�B	��B	�B	��B	��B	�PB	yrB	`AB	YB	A�B	;�B	6�B	%B	 'B��B��B�B��B�NB�B��B�B�NB�BB��B��B�#B��B��B��B�7B��B��B��B�4Bz�B~(BrGBs�Bo5Bn�BjBl�BrGBu�ByrBz�B�oB�@BbB��B�GB��B��B��B�=B�B��B�GB�SB�{B��B��B�YB�YB��B��B��B�eB�_B�eB�eB�B�eB��B��B�	B�kB�eB��B�+B��B��B~(B}VB|B{�B{BzxBxBm�Bm(Bk�BoiBq�Bu�BxBzB~(B�%B��B��B�-B��B��B��B��B�UB��B��B�gB�gB��B��B��B�#B�^B�B��B��B��B��BǮB�BȀBȀB�KB��B�B�)B�)B�pB�gB��B��B�iB�8B�B	�B	�B	�B	�B	49B	:)B	AUB	J�B	LdB	MB	QB	PHB	QB	Q�B	U�B	YB	[WB	]cB	]�B	]�B	a|B	cB	d�B	e�B	e�B	f�B	iB	jB	jB	m]B	quB	sMB	u%B	v�B	x7B	x�B	x�B	y�B	zDB	{JB	{�B	.B	�uB	�GB	�SB	�eB	�1B	�1B	�	B	�=B	�rB	�B	�'B	��B	��B	��B	��B	�B	�FB	��B	��B	��B	��B	��B	��B	��B	��B	�3B	�@B	��B	��B	��B	�BB	�wB	��B	�IB	�OB	� B	��B	�-B	��B	��B	�?B	�zB	�B	��B	��B	�B	�B	�NB	B	��B	��B	��B	�9B	ƨB	�B	��B	�B	˒B	�B	�HB	��B	тB	ѷB	҉B	�ZB	ԕB	��B	��B	�8B	�B	�sB	��B	�]B	��B	�B	�iB	�B	�B	�pB	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�B	�JB	��B	�PB	�(B	��B	�cB	�B	��B	� B	��B	�oB	�AB	�B	�MB	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�lB	��B	�rB	�	B	��B	�DB	��B	��B	��B	��B	�B	�JB	�B	�~B	��B	��B	��B	�~B	��B	��B	��B	�PB	��B	��B	�VB	��B	�bB	��B
 4B
:B
B
 �B
�B
@B
GB
{B
�B
{B
�B
�B
+B
�B
�B
�B
�B
�B

	B

rB

�B

�B
�B
IB
�B
B
B
�B
VB
VB
�B
\B
�B
�B
�B
�B
�B
:B
�B
nB
�B
�B
4B
hB
B
@B
B
B
B
tB
�B
�B
@B
�B
tB
�B
B
FB
B
�B
�B
�B
B
LB
$B
�B
�B
RB
_B
B
�B
wB
}B
�B
B
�B
�B
UB
�B
�B
 [B
!-B
"3B
!�B
"3B
"3B
!�B
!�B
"�B
"�B
"hB
"hB
"3B
"3B
"3B
"�B
"�B
#B
#�B
#B
"�B
"hB
#nB
#9B
#�B
$B
#�B
#�B
#9B
#�B
$B
$B
$@B
$@B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&B
&LB
&�B
&�B
'RB
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(XB
($B
'�B
'�B
'�B
'�B
(�B
)�B
*0B
*�B
*�B
+6B
+6B
+�B
+�B
,qB
,qB
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-B
-wB
-�B
-�B
-�B
-�B
-�B
-�B
.�B
/B
/OB
/�B
/OB
/OB
/�B
/�B
/�B
0 B
0UB
0�B
0�B
0�B
1'B
1[B
1�B
1�B
1�B
1�B
2�B
2�B
3�B
4mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
	B	�BeBeB	kB	�B�B�B
rB	kB�B	7B
rB�B�B	kB
	B�B	�B	�B	7B
�BB�BxB
�B	B	kBxB	�B	7BCB
rB	�BB
�B	7BCBB	�BB�B	�BB�B	�B
rBxB	kB	�BxB
�B	kB�B
�B	kBBB	�BxBxBCB
=B
=B�B�B�B�B
rB	�B�B
�B	7BCBBB	�B
�B	�BCB�B
�BxBB
	B
rBB
	B	kB
=B
	B'B�B-BB�B�B�BB�B�B �BOB$�B!�B!�B&�B(XB-�B*�B+�B.�B1�B0�B4B:�B8B8�BA�B>�B=pBB�BN<BVBS�BR�BZ�BY�BXEBY�B[#BZBX�B[�BY�BX�BV�BXEBZQBW�BW
BX�BW�BVBW
BXEBW>BVmBW�BY�BWsBWsBW�BW
BU�BX�BV�BT�BT�BV8BU2BU�BYBcB`ABc�Bb�BaGB`Bb�Bd�BcBd�Bf�Be�BffBf�BiBiBhrBm�BlWBp�BpBncBs�B{�B}VB|�B�GB�iB�{Bz�B|�B}"Bz�B{�B�4B�uB�B�@B�uB�B�B��B��B��B��B��B��B��B��B��B��B��B�B�=B�~B�@B��B��B��B��B��B�.B��B��B�VB��B��B��B�'B�bB�.B��B��B�B��B��B�bB��B��B��B��B�4B��B��B�nB��B��B��B��B�B��B�B�RB��B�BB�'B��B��B�EB�KB�tB�EB��B��B�zB�B��B��B�6B��B��B��B�B��B��B��BקB��B��B��B�	B��B��B�PBuB�B_B	�B$B$BqB�B"hB �B �B#�B$@B$�B(�B+�B-B+�B+6B+6B1'B,�B0�B/B0UB1�B1'B/�B0�B2�B1�B0�B1�B3gB4�B5B5?B8B<BEBC,BT,BR BYKB[�Bh�Bm]Bn.Bn�Bo�BqABpBq�Bo�Bo5BncBl"Bk�BncBlWBjBj�BhrBh
BgBe�B_�Bf�B_�B[�BVmBWsBVmBYBMjBPBPHBO�BR�BNpBM�BP|BJ�BF
BC�BF�BC�BB&BC�BE�BFsBFsBB�BGyBJ�BF�Bc�BQBQ�B\]BZ�B\�B\]BiBd�B{~Br�BquB��Bt�Br�BsMBquB��Bv+BsB{~B}�Bv`B}�B�!B� B��B~\B|�B}�B~�B~\Bz�B}"BzBx7Bx7BxlBv+By�By>By>Bv�Bv�Bx�Bx�BxBxlB{JBv�Bv`Bx�BwfBy�B�uBx�BuYBx7B|�B�B��B�1B��B��B�$B�@B�\B��B�*B�@B�B��B�B�kB��B�RB�B��B�B��BȴB�BBтB�,B��B�5B�vB�B�B��B��B�B� B�B�PB�rB�B�DB�rB�B�B�.B��B��B�5B�B�PB�PB��B�B�PB�B��B��B�B�B�BۋB�]BحBٴB�KBקB��B�`BԕBԕB�sBݘB��B�|B�5B��B��B��B�mB�XB��B�6B�RB�B��B��B�}B�B�wB�CB��B��B�*B��B��B�%B��B�B�B�B��B��B�B��BbBzDBquByrBv`Bd�Bd%Bb�BQ�BK�B>BB9�B7KB4�B2aB3gB-wB.IB(XB'RB&�B)*B3�B!�B%zB�B�B��B �B��B��B��B�iB�B��B��B�B��BںB�NB�JB�B��BÕB��BƨB��B��B�dB�HB�BB�B��B�hB��B�'B��B�B��B�!B�xB�xB�B��B��B��B��B�kB��B��B��B�B�iB.B.B{�B�MBy>Bq�B��B}VBVBR�BP�BJ�BI�BF�BR�Bb�B8B5�B1�B/�B3�B,B*�B)�B,�B�B=�B�BnB	7BSB
��B
�B
�uB
��B
�B
�oB
�oB
ݘB
ٴB
�B
�mB
��B
��B
� B
�NB
�HB
��B
ΤB
��B
ǮB
ěB
B
�UB
�wB
�HB
��B
�0B
��B
��B
̘B
��B
�EB
�}B
��B
��B
��B
�{B
�nB
�B
�!B
��B
��B
��B
�wB
�B
p�B
n.B
m]B
rGB
iDB
b�B
`AB
c�B
_�B
X�B
Z�B
ZB
U2B
K^B
P|B
HKB
[�B
D�B
@B
JWB
D�B
A�B
(�B
�B
 �B
�B
�B
5?B
<6B
$B
�B
�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                 44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721225002                            20230721225002AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122500220230721225002  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500220230721225002QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500220230721225002QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             