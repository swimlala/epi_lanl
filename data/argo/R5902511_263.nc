CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:50:07Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  R�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       X�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       u�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o      �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` .   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   .h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   4h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   :h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T @h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   @�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   @�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   @�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   @�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � @�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   A\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   Ax   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    A�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        A�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        A�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       A�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    A�Argo profile    3.1 1.2 19500101000000  20230721225007  20230721225007  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�0�FZI@�0�FZI11  @�0�ww} @�0�ww} @2�]�U\@2�]�U\�d�<�ϕ��d�<�ϕ�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 BA  BA  FF  ?�\)@   @=p�@�  @�G�@\@�G�@��RA��A ��A,(�A>�RA`  A���A�  A��A��A�\)AϮA�\)A�\)B (�B  B�B�
B   B(  B/�
B7�B@  BH(�BO�
BW�B_�
Bh  Bp  Bw�
B�
B�  B�  B�  B�  B�{B�(�B�{B�(�B�(�B�(�B�{B��B�  B�{B�  B�{B�(�B�{B��B�  B�{B��
B��B�  B��
B�  B��B��B�  B�  B��B��C  C��C��C  C	��C{C  C��C��C�C  C{C
=C{C  C��C!��C$  C&  C(  C*
=C,
=C.  C0{C1��C4  C6
=C8  C:  C<
=C=�C@  CB{CD{CF  CH  CI�CK�CN
=CP{CR{CT
=CU��CW��CZ  C\  C^
=C`  Cb  Cd
=Ce��Ch
=Cj{Cl{Cn
=Cp  Cq��Cs��Cu��Cx
=Cz  C|  C~  C�  C�  C�C���C�  C�  C�  C�  C�  C�
=C���C���C�  C���C�  C�  C�  C�C�  C�C�C�C�  C���C�  C�C�  C���C���C���C�  C�
=C�C���C���C�C�
=C�  C�  C�C�  C�  C�C�C�C�  C���C���C�  C�C���C�  C�  C���C���C�  C���C�  C�  C�  C���C���C���C���C���C�  C�  C�  C�C�C���C�  C�  C�  C�  C���C���C���C���C�  C�  C�  C�C�C�C�  C���C�  C�  C���C���C���C���C���C�  C�C�  C���C�  C�C�  C�  C�C�  C�C�C���C�  C�C�C�  C���C�  C�  C�C�C�  C�  C�  C�C�C�  C�  C�  C���C�  C�C���D   D � D �qD� D�D� D�D�D�D� D  D� D  D��D�D� D�D��D	  D	}qD	��D
� D�D��D�D� D  D��DD�D  D� D  D��D�D� D�D� D  D��D�D��D�D� D��D}qD��Dz�D�qD� D  D� D�D��D�D��D  Dz�D  D��D�D}qD��D}qD�qD }qD ��D!}qD"�D"� D#  D#� D$�D$��D$��D%}qD%�qD&}qD'�D'��D(  D(��D)  D)� D*�D*� D+  D+}qD+�qD,��D-D-�D.D.� D.�qD/}qD0  D0}qD0�qD1��D2D2��D3  D3}qD4  D4��D5�D5}qD5�qD6� D7�D7�D8  D8}qD8�qD9}qD9�qD:��D;  D;� D<�D<� D<�qD=� D>�D>� D?  D?}qD?�qD@� DA  DA��DB  DB� DC�DC� DC�qDD��DE  DE}qDF  DF� DG  DG� DG�qDH� DI  DI� DI�qDJz�DJ��DKz�DK�qDL��DL�qDMz�DN  DN� DN�qDO}qDO�qDP� DQ  DQ}qDR  DR��DS�DS� DT�DT��DU  DU� DV�DV�DW�DW� DW��DXz�DY�DY� DY�qDZ� D[  D[��D\D\��D]�D]��D^  D^��D_�D_� D`�D`��Da�Da� Da��Dbz�Db�qDc��Dd�Dd� De  De��De�qDf}qDg�Dg� Dg�qDhz�Dh�qDi� Dj�Dj��Dk  Dk}qDl  Dl��Dm�Dm� Dn  Dn��Do�Do��Do�qDp� Dq  Dq}qDq�qDr}qDr�qDs}qDt�Dt��Du�Du}qDvDv�Dv�qDw}qDw��Dxz�Dy  Dy� Dz  Dz� D{�D{}qD{�RD|z�D|�qD}}qD~  D~� D  D� D�HD�B�D��HD�� D���D�AHD�� D���D���D�>�D�� D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D�~�D��qD�  D�@ D�}qD��qD���D�@ D���D���D��D�AHD�� D��qD��qD�<)D�~�D�� D���D�AHD��HD��HD��qD�>�D�� D��HD�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD��HD���D�  D�@ D�~�D��HD�HD�>�D�� D�� D���D�@ D�� D�� D�  D�AHD��HD�� D�  D�AHD�� D�� D���D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D���D��qD�>�D��HD�� D�  D�@ D�� D���D�  D�@ D��HD��HD�  D�>�D�� D��{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?B�\?aG�?�  ?���?�Q�?�(�?��H@�@�R@#�
@8Q�@E�@Q�@fff@n{@�  @�ff@��@�
=@��H@�G�@�=q@�\)@�@�p�@\@˅@�\)@�
=@�  @�\@�@��@�z�@�p�A33Az�A	��A(�A\)A�
AA��A{A   A%�A'
=A*�HA/\)A1�A5A:=qA<��A@��AE�AG
=AL��AP  AS33AX��A[�A`��Ac�
Ah��Al(�Ao\)Atz�AvffAz�HA~{A���A��HA��
A�ffA�\)A���A�33A�(�A�ffA��A�G�A��
A���A�
=A���A�=qA���A�{A��A�=qA��A�A��A�G�A��A�p�A�
=A���A��HA�z�A��RA���A�=qA�z�A�ffA��A\A�(�A�Aȣ�A��A�z�A�{AϮAҏ\A��
A�A�Q�A��A�z�A�{A߮A�=qA��
A�p�A�Q�A�G�A�A�A�
=A���A��
A��A�
=A���A��HA�p�A��B z�Bp�B�HB�Bz�B�B�RB�B��B	�B
�RB(�B��BB33B�
B��B=qB�HBQ�B��B=qB33B  BG�B{B�HBQ�B��B{B\)B�
B ��B"{B"�RB#�B$��B%p�B&�\B'�B(z�B)�B*ffB+\)B,  B-�B.ffB/
=B0  B1G�B1�B3
=B4(�B4��B5�B6�RB7�B8��B9B:ffB;�B<��B=G�B>=qB?�B@(�BA�BBffBC
=BC�
BE�BE�BF�\BG�
BH��BIG�BJffBK\)BL(�BL��BN=qBN�RBO�BP��BQBRffBS�
BT��BUG�BVffBW�BX  BYG�BZ=qBZ�HB\  B]�B]B^�RB`  B`��Ba��Bb�\Bc�
Bd��Bep�Bf�RBg�BhQ�Bi�BjffBk\)BlQ�Bl��Bn{Bn�HBo�Bp��Bq��Br=qBs
=BtQ�BuG�Bu�Bv�\Bw�Bx��ByG�Bz{B{33B|  B|��B}B~�HB�B�=qB��HB�33B���B�(�B��RB�
=B��B�{B���B��HB�G�B��
B�ffB���B�33B���B�Q�B��RB�
=B��B�=qB��RB��B��B�(�B���B���B�p�B�{B��\B��HB��B�{B�ffB��HB��B��
B�Q�B���B�\)B�B�Q�B��HB�p�B��
B�(�B��HB�\)B��B�=qB��HB�\)B��B�=qB��HB�33B��B�=qB���B�33B���B�=qB��HB�G�B��B�=qB���B�p�B�B�(�B���B�\)B�B�=qB���B�\)B�B�=qB��HB�\)B�B�=qB���B�p�B�B�Q�B���B��B��B�z�B��B�p�B�{B���B�
=B��B�=qB���B�
=B��B�Q�B���B�G�B��B�ffB���B��B��B�ffB��B���B��B���B��B��B�(�B��RB�33B��
B�z�B��HB�p�B�(�B���B�33B�  B�ffB�33B�B�=qB��BÙ�B�(�B��HB�\)B�  B���B�G�B��
Bȏ\B��Bə�B�z�B�
=B˅B�=qB���B�\)B�{B��HB�G�B��BУ�B�\)B��
B�z�B�G�B�B�Q�B���Bՙ�B�{B���Bי�B�{B؏\B�33B�  B�z�B�
=B��
B�ffB��HB݅B�Q�B��HB�p�B�{B��HB�B�  B��B�\)B�(�B�\B�33B�  B�RB�33B�B�z�B��B�B�(�B���B�B�{B��B�\)B�(�B�RB�33B��B�RB�33B�B�z�B�33B��B�ffB���B�B�ffB��HB��B�=qB���B�G�B��B���B�G�B�B�Q�B�
=B��B�Q�B���B�\)C {C ffC ��C �HC=qC��C��C{CffCC�CffC��C�C=qC�\C�C(�CffCC{C\)C��C�HC=qC��C�
C{Cp�C��C	
=C	Q�C	�C

=C
Q�C
�\C
�CQ�C�\C��C�Cz�C��C  CQ�C�RC  C=qCz�C�
C33Cp�C�RC��CQ�C��C��CG�C�CC
=CQ�C�RC
=CQ�C�C��C(�Cz�C�RC  CG�C��C  CG�C�\C�
C{Cp�C��C�CffC�C�C33C�C�
C(�Cz�C�RC��CQ�C��C  CG�C�\C��C{C\)C��C  CQ�C��C�HC(�CffC�C   C Q�C �C!  C!=qC!z�C!C"{C"p�C"C#
=C#G�C#�\C#�C$=qC$�C$��C%
=C%Q�C%��C%��C&=qC&z�C&C'{C'p�C'C(
=C(Q�C(�\C(�HC)=qC)�\C)�HC*�C*ffC*�RC+  C+Q�C+��C,  C,G�C,��C,�C-33C-p�C-C.  C.Q�C.�C.��C/Q�C/�C/��C0=qC0�C0��C1(�C1p�C1�RC2
=C2\)C2�RC3{C3ffC3�RC3��C4G�C4��C4�C533C5�C5�HC6G�C6�\C6�
C7�C7ffC7�RC8{C8p�C8��C9�C9ffC9�C:  C:G�C:��C;  C;Q�C;�C<  C<G�C<�\C<�HC=(�C=�C=�
C>(�C>�C>�
C?33C?�\C?�HC@(�C@p�C@�RCA{CAz�CA��CB(�CB�CB�
CC(�CCz�CC��CD�CDffCD��CD��CEG�CE�\CE�
CF�CFp�CF��CG�CGp�CG1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�\)@   @=p�@�  @�G�@\@�G�@��RA��A ��A,(�A>�RA`  A���A�  A��A��A�\)AϮA�\)A�\)B (�B  B�B�
B   B(  B/�
B7�B@  BH(�BO�
BW�B_�
Bh  Bp  Bw�
B�
B�  B�  B�  B�  B�{B�(�B�{B�(�B�(�B�(�B�{B��B�  B�{B�  B�{B�(�B�{B��B�  B�{B��
B��B�  B��
B�  B��B��B�  B�  B��B��C  C��C��C  C	��C{C  C��C��C�C  C{C
=C{C  C��C!��C$  C&  C(  C*
=C,
=C.  C0{C1��C4  C6
=C8  C:  C<
=C=�C@  CB{CD{CF  CH  CI�CK�CN
=CP{CR{CT
=CU��CW��CZ  C\  C^
=C`  Cb  Cd
=Ce��Ch
=Cj{Cl{Cn
=Cp  Cq��Cs��Cu��Cx
=Cz  C|  C~  C�  C�  C�C���C�  C�  C�  C�  C�  C�
=C���C���C�  C���C�  C�  C�  C�C�  C�C�C�C�  C���C�  C�C�  C���C���C���C�  C�
=C�C���C���C�C�
=C�  C�  C�C�  C�  C�C�C�C�  C���C���C�  C�C���C�  C�  C���C���C�  C���C�  C�  C�  C���C���C���C���C���C�  C�  C�  C�C�C���C�  C�  C�  C�  C���C���C���C���C�  C�  C�  C�C�C�C�  C���C�  C�  C���C���C���C���C���C�  C�C�  C���C�  C�C�  C�  C�C�  C�C�C���C�  C�C�C�  C���C�  C�  C�C�C�  C�  C�  C�C�C�  C�  C�  C���C�  C�C���D   D � D �qD� D�D� D�D�D�D� D  D� D  D��D�D� D�D��D	  D	}qD	��D
� D�D��D�D� D  D��DD�D  D� D  D��D�D� D�D� D  D��D�D��D�D� D��D}qD��Dz�D�qD� D  D� D�D��D�D��D  Dz�D  D��D�D}qD��D}qD�qD }qD ��D!}qD"�D"� D#  D#� D$�D$��D$��D%}qD%�qD&}qD'�D'��D(  D(��D)  D)� D*�D*� D+  D+}qD+�qD,��D-D-�D.D.� D.�qD/}qD0  D0}qD0�qD1��D2D2��D3  D3}qD4  D4��D5�D5}qD5�qD6� D7�D7�D8  D8}qD8�qD9}qD9�qD:��D;  D;� D<�D<� D<�qD=� D>�D>� D?  D?}qD?�qD@� DA  DA��DB  DB� DC�DC� DC�qDD��DE  DE}qDF  DF� DG  DG� DG�qDH� DI  DI� DI�qDJz�DJ��DKz�DK�qDL��DL�qDMz�DN  DN� DN�qDO}qDO�qDP� DQ  DQ}qDR  DR��DS�DS� DT�DT��DU  DU� DV�DV�DW�DW� DW��DXz�DY�DY� DY�qDZ� D[  D[��D\D\��D]�D]��D^  D^��D_�D_� D`�D`��Da�Da� Da��Dbz�Db�qDc��Dd�Dd� De  De��De�qDf}qDg�Dg� Dg�qDhz�Dh�qDi� Dj�Dj��Dk  Dk}qDl  Dl��Dm�Dm� Dn  Dn��Do�Do��Do�qDp� Dq  Dq}qDq�qDr}qDr�qDs}qDt�Dt��Du�Du}qDvDv�Dv�qDw}qDw��Dxz�Dy  Dy� Dz  Dz� D{�D{}qD{�RD|z�D|�qD}}qD~  D~� D  D� D�HD�B�D��HD�� D���D�AHD�� D���D���D�>�D�� D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D�~�D��qD�  D�@ D�}qD��qD���D�@ D���D���D��D�AHD�� D��qD��qD�<)D�~�D�� D���D�AHD��HD��HD��qD�>�D�� D��HD�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD��HD���D�  D�@ D�~�D��HD�HD�>�D�� D�� D���D�@ D�� D�� D�  D�AHD��HD�� D�  D�AHD�� D�� D���D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D���D��qD�>�D��HD�� D�  D�@ D�� D���D�  D�@ D��HD��HD�  D�>�D�� D��{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?B�\?aG�?�  ?���?�Q�?�(�?��H@�@�R@#�
@8Q�@E�@Q�@fff@n{@�  @�ff@��@�
=@��H@�G�@�=q@�\)@�@�p�@\@˅@�\)@�
=@�  @�\@�@��@�z�@�p�A33Az�A	��A(�A\)A�
AA��A{A   A%�A'
=A*�HA/\)A1�A5A:=qA<��A@��AE�AG
=AL��AP  AS33AX��A[�A`��Ac�
Ah��Al(�Ao\)Atz�AvffAz�HA~{A���A��HA��
A�ffA�\)A���A�33A�(�A�ffA��A�G�A��
A���A�
=A���A�=qA���A�{A��A�=qA��A�A��A�G�A��A�p�A�
=A���A��HA�z�A��RA���A�=qA�z�A�ffA��A\A�(�A�Aȣ�A��A�z�A�{AϮAҏ\A��
A�A�Q�A��A�z�A�{A߮A�=qA��
A�p�A�Q�A�G�A�A�A�
=A���A��
A��A�
=A���A��HA�p�A��B z�Bp�B�HB�Bz�B�B�RB�B��B	�B
�RB(�B��BB33B�
B��B=qB�HBQ�B��B=qB33B  BG�B{B�HBQ�B��B{B\)B�
B ��B"{B"�RB#�B$��B%p�B&�\B'�B(z�B)�B*ffB+\)B,  B-�B.ffB/
=B0  B1G�B1�B3
=B4(�B4��B5�B6�RB7�B8��B9B:ffB;�B<��B=G�B>=qB?�B@(�BA�BBffBC
=BC�
BE�BE�BF�\BG�
BH��BIG�BJffBK\)BL(�BL��BN=qBN�RBO�BP��BQBRffBS�
BT��BUG�BVffBW�BX  BYG�BZ=qBZ�HB\  B]�B]B^�RB`  B`��Ba��Bb�\Bc�
Bd��Bep�Bf�RBg�BhQ�Bi�BjffBk\)BlQ�Bl��Bn{Bn�HBo�Bp��Bq��Br=qBs
=BtQ�BuG�Bu�Bv�\Bw�Bx��ByG�Bz{B{33B|  B|��B}B~�HB�B�=qB��HB�33B���B�(�B��RB�
=B��B�{B���B��HB�G�B��
B�ffB���B�33B���B�Q�B��RB�
=B��B�=qB��RB��B��B�(�B���B���B�p�B�{B��\B��HB��B�{B�ffB��HB��B��
B�Q�B���B�\)B�B�Q�B��HB�p�B��
B�(�B��HB�\)B��B�=qB��HB�\)B��B�=qB��HB�33B��B�=qB���B�33B���B�=qB��HB�G�B��B�=qB���B�p�B�B�(�B���B�\)B�B�=qB���B�\)B�B�=qB��HB�\)B�B�=qB���B�p�B�B�Q�B���B��B��B�z�B��B�p�B�{B���B�
=B��B�=qB���B�
=B��B�Q�B���B�G�B��B�ffB���B��B��B�ffB��B���B��B���B��B��B�(�B��RB�33B��
B�z�B��HB�p�B�(�B���B�33B�  B�ffB�33B�B�=qB��BÙ�B�(�B��HB�\)B�  B���B�G�B��
Bȏ\B��Bə�B�z�B�
=B˅B�=qB���B�\)B�{B��HB�G�B��BУ�B�\)B��
B�z�B�G�B�B�Q�B���Bՙ�B�{B���Bי�B�{B؏\B�33B�  B�z�B�
=B��
B�ffB��HB݅B�Q�B��HB�p�B�{B��HB�B�  B��B�\)B�(�B�\B�33B�  B�RB�33B�B�z�B��B�B�(�B���B�B�{B��B�\)B�(�B�RB�33B��B�RB�33B�B�z�B�33B��B�ffB���B�B�ffB��HB��B�=qB���B�G�B��B���B�G�B�B�Q�B�
=B��B�Q�B���B�\)C {C ffC ��C �HC=qC��C��C{CffCC�CffC��C�C=qC�\C�C(�CffCC{C\)C��C�HC=qC��C�
C{Cp�C��C	
=C	Q�C	�C

=C
Q�C
�\C
�CQ�C�\C��C�Cz�C��C  CQ�C�RC  C=qCz�C�
C33Cp�C�RC��CQ�C��C��CG�C�CC
=CQ�C�RC
=CQ�C�C��C(�Cz�C�RC  CG�C��C  CG�C�\C�
C{Cp�C��C�CffC�C�C33C�C�
C(�Cz�C�RC��CQ�C��C  CG�C�\C��C{C\)C��C  CQ�C��C�HC(�CffC�C   C Q�C �C!  C!=qC!z�C!C"{C"p�C"C#
=C#G�C#�\C#�C$=qC$�C$��C%
=C%Q�C%��C%��C&=qC&z�C&C'{C'p�C'C(
=C(Q�C(�\C(�HC)=qC)�\C)�HC*�C*ffC*�RC+  C+Q�C+��C,  C,G�C,��C,�C-33C-p�C-C.  C.Q�C.�C.��C/Q�C/�C/��C0=qC0�C0��C1(�C1p�C1�RC2
=C2\)C2�RC3{C3ffC3�RC3��C4G�C4��C4�C533C5�C5�HC6G�C6�\C6�
C7�C7ffC7�RC8{C8p�C8��C9�C9ffC9�C:  C:G�C:��C;  C;Q�C;�C<  C<G�C<�\C<�HC=(�C=�C=�
C>(�C>�C>�
C?33C?�\C?�HC@(�C@p�C@�RCA{CAz�CA��CB(�CB�CB�
CC(�CCz�CC��CD�CDffCD��CD��CEG�CE�\CE�
CF�CFp�CF��CG�CGp�CG1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�ZA�ZA�VA�ZA�\)A�\)A�`BA�bNA�`BA�ZA�`BA�bNA�ffA�7LA׋DA�O�A�G�A�C�A�=qA�7LA�1'A�&�A��A�bA��A֑hAՁA�1'A��A��HA�hsA��HA�"�A��Aч+A�C�AΕ�A�ȴA�A���AɸRA��Aƛ�A�bNA�dZA�jA�bNA���A���A���A�"�A��A���A�jA��A�l�A��PA�?}A�jA���A��A�1A��!A�A��A���A�-A��A���A���A�G�A�G�A�A�Q�A��9A�&�A�JA���A�dZA���A��HA��A���A�p�A���A��A�`BA�ƨA���A���A�ƨA�Q�A��A��A���A���A�33A�9XA��^A~1'A|(�Ay&�AwoAu��AtJAo��Ak�hAhbAf��AfQ�Ae�#Ad��AbM�Aa�A_�;A_x�A_;dA]��AZ�9AZ{AW;dAU33AS33AQ�PAP5?AO��AO?}AK��AG��AF��AD�/AA33A>�A<5?A;��A:v�A9`BA7+A5?}A4  A2M�A1
=A/�
A.�A.ffA.  A,�9A+�-A+%A*�DA*A)�7A'dZA%`BA"�!A!�7A��A�A��AG�A�9A`BA�hA�A-AE�A��A%A33A(�A��AXA�/A�!An�AA`BA
�A	��A$�AO�A��A�PA�`A�A9XA ��@�hs@��@��@���@�Ĝ@��
@�+@�^5@�7L@�@���@���@�p�@�@�r�@��;@�P@�@�n�@ᙚ@�j@�1@�ff@���@۾w@�v�@�/@ش9@��@ו�@�C�@��@�"�@�l�@�33@�M�@Ցh@�p�@���@ѡ�@д9@�j@���@�^5@�@�7L@�Q�@ˍP@�@��@��@ɺ^@ɉ7@�?}@��/@�X@�&�@�V@�5?@�|�@���@�v�@�%@�9X@��
@�C�@�t�@�t�@�o@ư!@���@�M�@�^5@��@���@ũ�@���@�V@�p�@��@���@�$�@���@�`B@��@��D@��@�x�@��D@��F@�dZ@�9X@��u@���@�1@��y@���@���@��@���@�\)@�t�@�o@��y@���@�A�@�|�@�@�ȴ@��\@�n�@�~�@�ff@�5?@�-@�5?@�-@���@��#@��@���@�Q�@�  @��F@�K�@���@���@��+@�$�@���@��^@���@���@�&�@��/@���@�A�@�b@���@���@�C�@��@�-@�@��#@���@�X@���@�Ĝ@��j@��9@��D@�bN@�z�@�9X@�b@�1@��m@���@���@���@���@��@�C�@�"�@�@��\@��@��@��@���@�`B@��/@�Q�@�b@���@�ƨ@���@�S�@�"�@��@��+@�{@��#@��7@�O�@��/@���@��j@���@�z�@�A�@��@��@���@���@�^5@���@���@�p�@���@���@�bN@�(�@���@�"�@�ȴ@�-@���@���@��^@���@�X@�O�@�V@���@��@��/@�Ĝ@��@��@��@���@��@���@��+@�n�@�^5@���@�`B@�/@��@��`@���@�Q�@�1'@� �@�  @�ƨ@�K�@���@�^5@�-@�{@���@���@���@�&�@���@��j@�A�@��;@��w@��F@��@���@��P@�C�@�
=@��@���@�^5@�@���@�x�@�`B@�?}@���@��/@��u@�bN@�I�@�  @��@�;d@�o@��@��H@�V@�$�@���@�hs@�O�@�G�@�/@��@���@���@�Ĝ@��m@�;d@���@�v�@�V@�=q@�{@��@��-@�hs@�O�@�?}@�&�@���@���@�I�@� �@� �@�b@��w@�S�@��@�v�@�$�@��@���@��-@���@��h@�G�@��@���@�r�@� �@�  @��
@���@��@�33@��R@�v�@��@��@���@���@�p�@�O�@�/@�V@���@�z�@�1'@��@~E�@}@}�h@}p�@|��@|��@|��@|z�@|j@|Z@|I�@|9X@|9X@|�@{�F@{t�@{S�@{@z�\@z=q@zJ@y�^@yX@xr�@w�@w|�@v��@vV@vff@vV@v5?@v{@u�@u�T@u��@u@u��@up�@u?}@u�@t�/@t�j@t9X@sƨ@s��@sC�@r��@r=q@q�^@q��@q��@q��@q��@q��@qG�@q&�@q%@p��@pĜ@pr�@pA�@pb@o�@o�w@o|�@o\)@n�y@n��@n��@m@mp�@mO�@l��@l�D@lZ@l1@k��@k"�@j�!@j=q@i��@i�^@i��@ihs@i�@hr�@g�w@g
=@fff@e`B@d�j@dI�@d�@d�@d1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�\)A�ZA�`BA�XA�ZA�ZA�XA�XA�VA�XA�^5A�ZA�ZA�S�A�XA�S�A�Q�A�ZA�XA�XA�^5A�ZA�\)A�\)A�ZA�^5A�ZA�\)A�^5A�XA�`BA�^5A�\)A�`BA�bNA�^5A�ffA�bNA�bNA�dZA�`BA�bNA�dZA�\)A�bNA�\)A�XA�\)A�\)A�\)A�^5A�XA�^5A�`BA�`BA�jA�ffA�ZA�ffA�^5A�dZA�`BA�dZA�^5A�bNA�jA�jA�jA�dZA�ffA�ffA�bNA�ffA�`BA�\)A�`BA�bNA�l�A�dZA��A�=qA���A�ƨA��
A���A׺^A׍PA׋DA�XA�S�A�S�A�Q�A�M�A�S�A�O�A�M�A�S�A�O�A�I�A�K�A�M�A�G�A�I�A�I�A�E�A�G�A�E�A�C�A�G�A�C�A�E�A�C�A�?}A�E�A�=qA�=qA�A�A�;dA�?}A�;dA�9XA�=qA�;dA�9XA�=qA�7LA�9XA�=qA�7LA�9XA�;dA�5?A�33A�7LA�1'A�5?A�5?A�/A�/A�1'A�-A�+A�-A�&�A�$�A�(�A�"�A��A�"�A��A��A� �A��A��A��A��A��A�{A��A�{A�JA�bA�JA�%A�A���A�  A���A��A��A��mA��/A��
A���A�ƨAְ!A֥�A֑hAփA�jA�(�A���A��mA���AՇ+A�VA�E�A�=qA�9XA�9XA�33A�5?A�33A�/A�1'A�1'A�+A�+A�+A�&�A�$�A�"�A��A��A��A�{A�bA�
=A���A��A��`A��;A���A�ȴAԸRAԬAԥ�Aԟ�AԑhA�~�A�hsA�A�A�A���A��A��yA��TA��TA��/A��;A��HA���Aӣ�A�5?A� �A�oA�oA�oA�JA�JA�A�dZA�JA��mA���A�ȴA�ȴA�ƨA�ƨA�ƨA���AѺ^Aѩ�Aѝ�AёhA�z�A�O�A��A���AУ�A�x�A�9XA�+A�$�A��A�
=A�A���A��A��HA�A�=qA�bA�S�A��A��mA��`A��/A���A���A���A�ȴA���A̴9A̡�Ả7A�dZA�9XA��HA�bNA�S�A�5?A��A��A�bA���A���Aʰ!Aʝ�AʃA�t�A�bNA�33A�A��A��
Aɰ!A�hsA��A��;A�Aȣ�A�|�A�M�A��HAǁA�oA���A�ȴA�ȴA�ȴA�ĜA�ƨA�AƗ�A��AŋDA�p�A�hsA�dZA�^5A�^5A�XA�E�A�?}A�;dA��A���A�^5A���Aô9A�XA��A��;A���A�A�x�A�M�A�JA���A�hsA�=qA�A�5?A��A�bA�%A��jA�^5A��A��FA��A�l�A�dZA�XA�A�A�&�A�  A��
A��^A���A��A�z�A�Q�A��A��yA�ƨA��uA�7LA��A��uA�K�A�;dA�(�A��A��A�VA���A��HA���A�z�A�M�A���A�?}A��A���A���A���A��FA�|�A�9XA�{A��`A��FA�VA�I�A�G�A�=qA�33A�/A�"�A��A��A�oA�1A�  A���A��A�1'A�1A��A��TA���A���A�|�A�dZA�^5A�M�A�M�A�K�A�E�A�A�A�=qA�"�A���A���A��DA�M�A� �A���A��mA���A���A���A��DA��A�|�A�v�A�dZA�;dA��A��yA��-A�r�A�A�A�oA��yA��A���A���A�ƨA��^A��-A��9A��!A��PA��A���A�l�A��A�|�A��#A���A�n�A�9XA��A��RA�n�A�33A���A��/A���A�A��A���A�|�A�bNA�S�A�7LA�1A��A��A���A��A�O�A�ĜA�7LA���A�S�A�+A���A���A�v�A��HA��A���A�dZA�%A��A���A�hsA���A��A��7A�7LA��A��\A�Q�A��;A���A�x�A�&�A�
=A�ƨA��DA�=qA���A���A���A�x�A�\)A�A�A�A���A�VA��;A���A�ĜA���A��RA��9A��-A��9A��-A���A��A�bNA�A�A�JA��#A���A�O�A�{A�  A��A��A��A��A�{A�/A���A��PA�z�A�z�A�z�A�r�A�jA�dZA�^5A�XA�S�A�Q�A�M�A�(�A��HA���A�|�A�E�A�1A���A��A��`A��A�ȴA���A�\)A�33A��A�oA���A��`A���A���A��!A���A���A���A��A�K�A�bA�ƨA���A�+A��9A�hsA�K�A�bA�ĜA���A���A��A��A�x�A�hsA�S�A�=qA��A�bNA��A��A�hsA�S�A�I�A�7LA�33A�$�A��A��A�JA�XA�z�A�|�A�bNA�p�A�|�A��
A�r�A�9XA�(�A�(�A�+A�(�A���A�jA���A�I�A��A���A��RA��9A��9A���A�x�A�n�A�l�A�\)A�K�A�1'A��/A�Q�A��A���A�hsA�1'A���A�?}A���A���A���A�\)A�C�A�$�A��HA�/A���A��hA�M�A�$�A��A�oA�
=A�A���A��A��DA�p�A�VA�=qA�JA���A��!A��hA�bNA�I�A��A���A���A��A�O�A��`A��+A��A�p�A`BA~��A~bA}��A}XA}?}A}
=A|��A|(�A{�A{��A{|�A{G�AzVAy;dAx��AxbNAxAw`BAw"�Aw%AwVAwoAwAv��Avv�Au��Au�wAu��Au�7Au`BAu7LAuVAt�91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�\)A�ZA�ZA�VA�ZA�\)A�\)A�`BA�bNA�`BA�ZA�`BA�bNA�ffA�7LA׋DA�O�A�G�A�C�A�=qA�7LA�1'A�&�A��A�bA��A֑hAՁA�1'A��A��HA�hsA��HA�"�A��Aч+A�C�AΕ�A�ȴA�A���AɸRA��Aƛ�A�bNA�dZA�jA�bNA���A���A���A�"�A��A���A�jA��A�l�A��PA�?}A�jA���A��A�1A��!A�A��A���A�-A��A���A���A�G�A�G�A�A�Q�A��9A�&�A�JA���A�dZA���A��HA��A���A�p�A���A��A�`BA�ƨA���A���A�ƨA�Q�A��A��A���A���A�33A�9XA��^A~1'A|(�Ay&�AwoAu��AtJAo��Ak�hAhbAf��AfQ�Ae�#Ad��AbM�Aa�A_�;A_x�A_;dA]��AZ�9AZ{AW;dAU33AS33AQ�PAP5?AO��AO?}AK��AG��AF��AD�/AA33A>�A<5?A;��A:v�A9`BA7+A5?}A4  A2M�A1
=A/�
A.�A.ffA.  A,�9A+�-A+%A*�DA*A)�7A'dZA%`BA"�!A!�7A��A�A��AG�A�9A`BA�hA�A-AE�A��A%A33A(�A��AXA�/A�!An�AA`BA
�A	��A$�AO�A��A�PA�`A�A9XA ��@�hs@��@��@���@�Ĝ@��
@�+@�^5@�7L@�@���@���@�p�@�@�r�@��;@�P@�@�n�@ᙚ@�j@�1@�ff@���@۾w@�v�@�/@ش9@��@ו�@�C�@��@�"�@�l�@�33@�M�@Ցh@�p�@���@ѡ�@д9@�j@���@�^5@�@�7L@�Q�@ˍP@�@��@��@ɺ^@ɉ7@�?}@��/@�X@�&�@�V@�5?@�|�@���@�v�@�%@�9X@��
@�C�@�t�@�t�@�o@ư!@���@�M�@�^5@��@���@ũ�@���@�V@�p�@��@���@�$�@���@�`B@��@��D@��@�x�@��D@��F@�dZ@�9X@��u@���@�1@��y@���@���@��@���@�\)@�t�@�o@��y@���@�A�@�|�@�@�ȴ@��\@�n�@�~�@�ff@�5?@�-@�5?@�-@���@��#@��@���@�Q�@�  @��F@�K�@���@���@��+@�$�@���@��^@���@���@�&�@��/@���@�A�@�b@���@���@�C�@��@�-@�@��#@���@�X@���@�Ĝ@��j@��9@��D@�bN@�z�@�9X@�b@�1@��m@���@���@���@���@��@�C�@�"�@�@��\@��@��@��@���@�`B@��/@�Q�@�b@���@�ƨ@���@�S�@�"�@��@��+@�{@��#@��7@�O�@��/@���@��j@���@�z�@�A�@��@��@���@���@�^5@���@���@�p�@���@���@�bN@�(�@���@�"�@�ȴ@�-@���@���@��^@���@�X@�O�@�V@���@��@��/@�Ĝ@��@��@��@���@��@���@��+@�n�@�^5@���@�`B@�/@��@��`@���@�Q�@�1'@� �@�  @�ƨ@�K�@���@�^5@�-@�{@���@���@���@�&�@���@��j@�A�@��;@��w@��F@��@���@��P@�C�@�
=@��@���@�^5@�@���@�x�@�`B@�?}@���@��/@��u@�bN@�I�@�  @��@�;d@�o@��@��H@�V@�$�@���@�hs@�O�@�G�@�/@��@���@���@�Ĝ@��m@�;d@���@�v�@�V@�=q@�{@��@��-@�hs@�O�@�?}@�&�@���@���@�I�@� �@� �@�b@��w@�S�@��@�v�@�$�@��@���@��-@���@��h@�G�@��@���@�r�@� �@�  @��
@���@��@�33@��R@�v�@��@��@���@���@�p�@�O�@�/@�V@���@�z�@�1'@��@~E�@}@}�h@}p�@|��@|��@|��@|z�@|j@|Z@|I�@|9X@|9X@|�@{�F@{t�@{S�@{@z�\@z=q@zJ@y�^@yX@xr�@w�@w|�@v��@vV@vff@vV@v5?@v{@u�@u�T@u��@u@u��@up�@u?}@u�@t�/@t�j@t9X@sƨ@s��@sC�@r��@r=q@q�^@q��@q��@q��@q��@q��@qG�@q&�@q%@p��@pĜ@pr�@pA�@pb@o�@o�w@o|�@o\)@n�y@n��@n��@m@mp�@mO�@l��@l�D@lZ@l1@k��@k"�@j�!@j=q@i��@i�^@i��@ihs@i�@hr�@g�w@g
=@fff@e`B@d�j@dI�@d�@d�@d1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�\)A�ZA�`BA�XA�ZA�ZA�XA�XA�VA�XA�^5A�ZA�ZA�S�A�XA�S�A�Q�A�ZA�XA�XA�^5A�ZA�\)A�\)A�ZA�^5A�ZA�\)A�^5A�XA�`BA�^5A�\)A�`BA�bNA�^5A�ffA�bNA�bNA�dZA�`BA�bNA�dZA�\)A�bNA�\)A�XA�\)A�\)A�\)A�^5A�XA�^5A�`BA�`BA�jA�ffA�ZA�ffA�^5A�dZA�`BA�dZA�^5A�bNA�jA�jA�jA�dZA�ffA�ffA�bNA�ffA�`BA�\)A�`BA�bNA�l�A�dZA��A�=qA���A�ƨA��
A���A׺^A׍PA׋DA�XA�S�A�S�A�Q�A�M�A�S�A�O�A�M�A�S�A�O�A�I�A�K�A�M�A�G�A�I�A�I�A�E�A�G�A�E�A�C�A�G�A�C�A�E�A�C�A�?}A�E�A�=qA�=qA�A�A�;dA�?}A�;dA�9XA�=qA�;dA�9XA�=qA�7LA�9XA�=qA�7LA�9XA�;dA�5?A�33A�7LA�1'A�5?A�5?A�/A�/A�1'A�-A�+A�-A�&�A�$�A�(�A�"�A��A�"�A��A��A� �A��A��A��A��A��A�{A��A�{A�JA�bA�JA�%A�A���A�  A���A��A��A��mA��/A��
A���A�ƨAְ!A֥�A֑hAփA�jA�(�A���A��mA���AՇ+A�VA�E�A�=qA�9XA�9XA�33A�5?A�33A�/A�1'A�1'A�+A�+A�+A�&�A�$�A�"�A��A��A��A�{A�bA�
=A���A��A��`A��;A���A�ȴAԸRAԬAԥ�Aԟ�AԑhA�~�A�hsA�A�A�A���A��A��yA��TA��TA��/A��;A��HA���Aӣ�A�5?A� �A�oA�oA�oA�JA�JA�A�dZA�JA��mA���A�ȴA�ȴA�ƨA�ƨA�ƨA���AѺ^Aѩ�Aѝ�AёhA�z�A�O�A��A���AУ�A�x�A�9XA�+A�$�A��A�
=A�A���A��A��HA�A�=qA�bA�S�A��A��mA��`A��/A���A���A���A�ȴA���A̴9A̡�Ả7A�dZA�9XA��HA�bNA�S�A�5?A��A��A�bA���A���Aʰ!Aʝ�AʃA�t�A�bNA�33A�A��A��
Aɰ!A�hsA��A��;A�Aȣ�A�|�A�M�A��HAǁA�oA���A�ȴA�ȴA�ȴA�ĜA�ƨA�AƗ�A��AŋDA�p�A�hsA�dZA�^5A�^5A�XA�E�A�?}A�;dA��A���A�^5A���Aô9A�XA��A��;A���A�A�x�A�M�A�JA���A�hsA�=qA�A�5?A��A�bA�%A��jA�^5A��A��FA��A�l�A�dZA�XA�A�A�&�A�  A��
A��^A���A��A�z�A�Q�A��A��yA�ƨA��uA�7LA��A��uA�K�A�;dA�(�A��A��A�VA���A��HA���A�z�A�M�A���A�?}A��A���A���A���A��FA�|�A�9XA�{A��`A��FA�VA�I�A�G�A�=qA�33A�/A�"�A��A��A�oA�1A�  A���A��A�1'A�1A��A��TA���A���A�|�A�dZA�^5A�M�A�M�A�K�A�E�A�A�A�=qA�"�A���A���A��DA�M�A� �A���A��mA���A���A���A��DA��A�|�A�v�A�dZA�;dA��A��yA��-A�r�A�A�A�oA��yA��A���A���A�ƨA��^A��-A��9A��!A��PA��A���A�l�A��A�|�A��#A���A�n�A�9XA��A��RA�n�A�33A���A��/A���A�A��A���A�|�A�bNA�S�A�7LA�1A��A��A���A��A�O�A�ĜA�7LA���A�S�A�+A���A���A�v�A��HA��A���A�dZA�%A��A���A�hsA���A��A��7A�7LA��A��\A�Q�A��;A���A�x�A�&�A�
=A�ƨA��DA�=qA���A���A���A�x�A�\)A�A�A�A���A�VA��;A���A�ĜA���A��RA��9A��-A��9A��-A���A��A�bNA�A�A�JA��#A���A�O�A�{A�  A��A��A��A��A�{A�/A���A��PA�z�A�z�A�z�A�r�A�jA�dZA�^5A�XA�S�A�Q�A�M�A�(�A��HA���A�|�A�E�A�1A���A��A��`A��A�ȴA���A�\)A�33A��A�oA���A��`A���A���A��!A���A���A���A��A�K�A�bA�ƨA���A�+A��9A�hsA�K�A�bA�ĜA���A���A��A��A�x�A�hsA�S�A�=qA��A�bNA��A��A�hsA�S�A�I�A�7LA�33A�$�A��A��A�JA�XA�z�A�|�A�bNA�p�A�|�A��
A�r�A�9XA�(�A�(�A�+A�(�A���A�jA���A�I�A��A���A��RA��9A��9A���A�x�A�n�A�l�A�\)A�K�A�1'A��/A�Q�A��A���A�hsA�1'A���A�?}A���A���A���A�\)A�C�A�$�A��HA�/A���A��hA�M�A�$�A��A�oA�
=A�A���A��A��DA�p�A�VA�=qA�JA���A��!A��hA�bNA�I�A��A���A���A��A�O�A��`A��+A��A�p�A`BA~��A~bA}��A}XA}?}A}
=A|��A|(�A{�A{��A{|�A{G�AzVAy;dAx��AxbNAxAw`BAw"�Aw%AwVAwoAwAv��Avv�Au��Au�wAu��Au�7Au`BAu7LAuVAt�91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333333333333333333                                                                                                                 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BoiBo�Bo�BpBo5Bo�Bo�BoiBoiBo5Bn�Bo Bo Bn�Bo�Bl�BkQBkQBk�Bk�Bk�Bk�Bk�BkQBkQBk�Bm�B{�B�B��B��B��B�	B�UB�HB�dB��B��B�+BB*�B&�B,=B5?B8�BAUB2aB-wB3�B=qBZQBpoBx�Bu�Bl�BgBg8B_�BZQBY�BOvBL�BF?B<�B=qB*0BxB�BhB�JB�B҉B�B�B��B��B��Bt�Bz�Bx8Bw�Bf�B^�BS�BH�B%�B=BJB;B
�KB
��B
�UB
��B
��B
�B
u�B
dZB
[�B
M�B
@�B
$�B
bB
�B	�B	�DB	�BB	�jB	�B	��B	��B	��B	��B	�B	�{B	xB	r�B	k�B	h>B	iyB	OvB	J�B	@OB	1�B	$tB	�B	B	YB	SB	�B�.B��B�B�B�EB�[B�B��B��B��B�XB�zB�B�~B�1B��B�uB��B��B��B�(B�B��B��B��B�rB�B�iB��B~�B}�B{�By�Bx�Bv`BqBjBl"BrGBu%BgBe`BcTBcTB_;B_pB_pB_�B`BBa|BbNBc B_�B`vBa�B`vBc�B`�B_pBYBZ�BYKBW�BXyBYBXBT�B]�BYKBYB[�B`vBd&Bh>Bl�BqABv�Bz�B��B��B�4B��B��B�-B��B�kB�!B�nB��B�B�B�B��B�dB�}B��B҉B��B��BҽB҉B��B��B��B�yBݘB�|B�B�B�mB�B�B�B��B�]B	�B	B	fB	CB	�B	 \B	'RB	*�B	*eB	+�B	7�B	?�B	@OB	>wB	?B	GEB	I�B	H�B	HB	K�B	P�B	H�B	DgB	B�B	F?B	D�B	DgB	F?B	I�B	R�B	X�B	aB	d�B	c B	b�B	kB	p�B	u�B	w�B	v�B	y�B	��B	��B	��B	�hB	��B	��B	�+B	�_B	��B	�B	��B	�YB	��B	��B	��B	��B	�4B	�FB	�RB	�$B	�$B	��B	�0B	�qB	�=B	��B	�kB	��B	��B	�B	��B	�B	��B	��B	�?B	��B	�^B	��B	�6B	�B	��B	�jB	�qB	�qB	��B	�wB	��B	�}B	�OB	B	�3B	ĜB	��B	�B	��B	��B	�B	� B	��B	�[B	��B	��B	��B	��B	�B	�B	�)B	ܒB	ܒB	�dB	�/B	��B	ݘB	�vB	�B	� B	��B	�B	�B	�B	��B	��B	��B	�sB	�B	�B	�QB	�"B	��B	��B	�B	�5B	�B	��B	�B	�B	�B	�;B	�iB	��B	��B	�AB	��B	�TB	�ZB	��B	��B	�`B	�+B	�`B	��B	��B	�fB	�	B	�DB	�B	�DB	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�cB
  B
B	��B	��B
  B
 iB
;B
GB
B
�B
B
MB
�B
%B
�B
�B
�B
+B
_B
�B
�B
�B
	lB

�B
DB
B
DB
xB
xB
DB
JB
�B
�B
VB
�B
bB
4B
4B
4B
4B
:B
:B
B
�B
B
@B
FB
uB
�B
�B
�B
@B
�B
$B
YB
YB
$B
�B
$B
$B
YB
�B
�B
�B
�B
�B
�B
1B
1B
1B
B
7B
B
7B
7B
B
�B
�B
xB
B
�B
CB
�B
�B
IB
OB
B
B
B
�B
�B
 �B
 �B
!bB
"hB
"�B
#B
#nB
#�B
#�B
$@B
%�B
&LB
&�B
&LB
&�B
&�B
'�B
(XB
($B
($B
(�B
($B
($B
(�B
*�B
*0B
*�B
+B
+�B
,B
,B
,B
+�B
,B
,B
,qB
,�B
,�B
-�B
-�B
.B
.�B
.�B
.�B
.�B
.�B
/B
.�B
.B
.IB
-�B
.IB
/�B
1[B
33B
4B
4B
49B
4nB
4�B
5B
5?B
5�B
5tB
5�B
5�B
6zB
6FB
6�B
6�B
6�B
6zB
6FB
6�B
6�B
6�B
6�B
7B
7B
7B
7B
7B
7B
7B
7LB
7B
7B
6�B
6�B
6�B
6zB
6�B
7�B
8�B
8�B
:^B
;�B
<6B
<jB
<�B
=<B
=<B
=�B
=�B
>B
>B
=�B
>B
=�B
=�B
=�B
=B
<B
;dB
<6B
<�B
>B
>�B
>�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bm]Bo5Bp�Bn/Bp�BoiBo5BqvBo�Bo�BpoBo5Bq�Bo BqBo�BpBp�Bm�BpBo5Bm�Bp;Bo�BpBpBncBo�BpBo BqABn/BpoBqBn�Bm�Bq�Bn�BpBo�BoiBo Bo5BncBpoBm�Bn�Bo�Bl�Bo�Bn�Bn/BpoBncBm�Bn�Bo5Bp�BoiBncBp�Bn�Bo�Bn/Bn�Bm�Bm�BpoBo Bo5Bn�Bm�Bo�Bm�BoiBm�Bm)Bm�Bl"BncB~�Bf�BxlBg�BkQBgmBk�B{BqABjBl"Bj�Bk�Bl�Bi�Bj�Bk�Bi�Bj�Bl�Bk�BjKBlWBkQBj�BlWBjBk�Bk�BjKBl"BkBk�BlWBjBlWBl�BkBl�Bj�Bk�Bl"Bj�Bk�Bl"BjBlWBk�BjBl�Bk�Bj�BlWBl"Bj�Bl"BkQBjBl�Bk�Bj�Bk�Bl"BjBl"BlWBjKBl"BlWBjKBk�Bk�BjKBl"BkQBj�Bl"BjBl�BjBk�Bl�Bi�BkBlWBkBk�Bj�Bi�Bm)BkBk�Bl"Bk�Bi�Bl"Bm�Bl�Bk�Bm�BoiBs�Br�Bp�Bu�B}�B�SB.B��B.B~�B��B~�B.B� B~�B~�B�4B~�B~�B� B�4B~�B��B�oB�B�uB��B��B�MB�MB�oB��B��B�{B��B�B�B�{B�%B�B�xB�B��B��B�B��B�CB�7B��B��B�1B�xB�B�!B��B��B�-B�'B��B��B�3B��B�B��B��B�qB�jB��B�^B��B��B�XB��B��B��B��B�qB��B�jB��B�jB�?B�wB��B��B�gB��B� B�3B�-B�tB�B��B�]B�DB�ZB��B�%B�+B�`B�%B��B�+B��B�>B�	B�VB�B�B
rB~B($B&�B&�B&�B)_B/�B,=B*eB-CB)_B)�B+kB*�B%zB$�B%�B+kBB \B �B �B�B&�B5B6zB7�B7�B1�B/�B.�B0�B.}B.}B:�BJ�BCaB7B8�B8�B6�B6zB7B;0B7�B5?B>wBC�BGBH�B@�BE9B>�B0�B1'B2�B0�B0UB4�B0!B)�B)�B9�B,�B)_B'RB%�B6B:�B8�B2�B4�B1�B/�B2aB4nB5B9�B>�B@OBB�BB�BDgBI�BP�BS�BR BZBhsBdZBu�BpBm�BqBpoBncBo�Bq�BrBz�Br|Bv+B��Bz�By	B{B~�BpBr|Bu�Bs�Bn�BncBu�Bs�BiBg�Bh�BiDBgmBh�Bg�Be�Be�BgmBe,Bj�Bq�Bm�Bb�B`BB`vB`Be`Bc�B\]B[�B\)BZ�BYBY�BZBW
B^�BZ�BZ�B`BYBW�BV�BQNBP�BQ�BQ�BN<BL�BMjBJ�BL�BM�BM�BR�BI�BS�BIBI�BEmBB'B@OB>wB=B>wB<�B8�B7�B@BNB7�B<jB@�B7BAUB4nB$@B*eB1�B$@B,�B%B%B�BeB�B	B�B�BYB�B�B�BSB�B
=B
�B�B�BqB&�B��B�8B�B�%B�BoBB�,B��B�B��B֡B�B�BیBӏB��B�B��B��B�#BÖB��B�HB�tB�B�^B��B��B�}B�B�$B��B�B��B�hB�nB��B��B��B�B�MB��B�B�@B��B�{B�_B�(B�bB��B��B��B��B{Bo�Bo�Bn�Bo5BqB`�B��B��By>Bw�BuZBv�Bx�By	By�Bx�Bv�Bw2Bw�Bw�B��B}�Bu�Bm�Br|Bm�Bg�Bf�Bd&Bd�Bd�Bl�Bi�B`�B^�BZ�B[�BY�BZBV�BYBR�BOBO�BR�BPBP�BH�BI�BJXBJXB3�B*�B:�B(XB \BOBCB�B	B�BxB �B1B"�B!�BhB�B	7B	B%B�B�B�BoB
��B,=B
�B
�QBK�B
�)B
�pB{B
��B
��B
�dB
�#B
�KB
��B
�9B
�KB
��B
��B
�B
��B
�B
��B
�_B
�B
��B
�zB
�B
�*B
��B
�B
��B
��B
��B
��B
��B
�1B
��B
�YB
�1B
�uB
��B
�AB
x�B
u%B
~]B
��B
kB
k�B
x�B
e�B
f�B
e�B
c�B
_�B
c B
iDB
_;B
^�B
[�B
X�B
\]B
YKB
P�B
Q�B
Q�B
K�B
N�B
N�B
A B
>B
B'B
@OB
?B
<jB
D3B
:�B
)*B
#B
B
�B
B
�B
�B
�B
VB
xB
�B

=B
VB
PB
uB	�`B	��B	��B	�GB	�5B	�B	�B	��B	�B	�GB	�B	�B	�fB	� B	�&B	��B	یB	�H4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444Bk�Bl"Bl"BlWBk�Bk�Bk�Bk�Bk�Bk�BkBkPBkPBkBl"BiBg�Bg�Bg�Bh
Bg�Bg�Bg�Bg�Bg�Bg�BjBx7B{�B|�B� B�7B�YB��B��B��B�B�/B�{BVB&�B"�B(�B1�B5?B=�B.�B)�B0 B9�BV�Bl�Bu%BrBiDBcTBc�B[�BV�BVBK�BH�BB�B8�B9�B&�B�B:B�B��B�cB��B�dB�dB�'B�4B�Bp�Bv�Bt�Bs�BcB[#BPBE9B!�B�B�B
��B
�B
�/B
��B
�LB
�'B
�iB
rGB
`�B
W�B
I�B
<�B
!-B
�B
�B	�WB	�B	ܒB	ɺB	�XB	��B	�B	��B	��B	�kB	�B	tSB	o5B	h
B	d�B	e�B	K�B	GEB	<�B	.B	 �B	B	kB	�B	�B	�B�~B�B� B�cBԕB��B�^B��B��B�'B��B��B�eB��B��B�B��B��B��B�B�xB�kB��B�B�=B��B~\B|�B|�B{By�BxBv+Bt�Br�Bm]Bf�BhrBn�BquBcTBa�B_�B_�B[�B[�B[�B[�B\�B]�B^�B_pB[�B\�B^5B\�B`B]/B[�BU�BV�BU�BT,BT�BU�BT`BQNBY�BU�BU�BXEB\�B`vBd�BiDBm�BsBw1B� B��B��B��B��B�}B�B��B�qB��B�'B�gB�^B�aB�?BȴB��B�5B��B�B�B�B��B�B�NB�&B��B��B��B��B��B�B��B��B�WB�MB��B��B�bB	�B	�B	�B	�B	#�B	&�B	&�B	($B	49B	<B	<�B	:�B	;dB	C�B	F
B	E9B	DgB	HB	M5B	EB	@�B	?HB	B�B	A B	@�B	B�B	F?B	OB	T�B	]cB	aGB	_pB	_;B	glB	l�B	q�B	tB	r�B	u�B	~(B	�:B	��B	��B	��B	�B	�{B	��B	�.B	�\B	�:B	��B	�@B	�B	�LB	�B	��B	��B	��B	�tB	�tB	�B	��B	��B	��B	�$B	��B	�*B	��B	�^B	��B	�kB	�B	��B	��B	��B	��B	�KB	��B	�RB	�B	��B	��B	��B	��B	��B	�0B	��B	��B	��B	��B	��B	� B	�UB	�&B	�3B	�WB	�pB	�BB	ϫB	�HB	�NB	� B	�,B	��B	�mB	�yB	��B	��B	ٴB	�B	�KB	��B	��B	�iB	�pB	�AB	��B	��B	��B	�B	�%B	�%B	��B	�fB	�fB	�B	�rB	�DB	�JB	��B	�B	��B	�"B	��B	��B	�]B	�B	�B	�"B	�(B	�B	�B	�B	�B	��B	��B	�B	�{B	�B	��B	��B	�B	�YB	��B	�`B	��B	�`B	��B	��B	��B	��B	��B	�+B	�fB	�lB	�7B	�DB	��B	��B	�PB	�VB	�B	�B	�PB	��B	��B	��B	�bB
 4B
 iB
 �B
@B
uB
GB
B
GB
{B
�B
MB
�B
�B
�B
�B
�B
_B
�B
�B
�B
�B
�B
	7B

=B

�B
B
�B
�B
�B
�B
�B
�B
�B
\B
�B
\B
�B
�B
�B
'B
�B
'B
�B
:B
tB
�B
�B
tB
@B
tB
tB
�B
B
LB
LB
B
B
LB
�B
�B
�B
RB
�B
RB
�B
�B
RB
�B
�B
�B
_B
*B
�B
�B
0B
�B
�B
kB
kB
kB
B
CB
�B
B
�B
�B
�B
UB
�B
 'B
 'B
 �B
"3B
"�B
#9B
"�B
"�B
#B
$B
$�B
$tB
$tB
%B
$tB
$tB
%B
&�B
&�B
&�B
'RB
($B
(XB
(XB
(XB
($B
(XB
(XB
(�B
(�B
)*B
)�B
*0B
*dB
+B
+B
+B
+B
+6B
+kB
+B
*dB
*�B
*0B
*�B
+�B
-�B
/�B
0UB
0UB
0�B
0�B
1'B
1[B
1�B
1�B
1�B
1�B
2-B
2�B
2�B
2�B
2�B
33B
2�B
2�B
2�B
2�B
2�B
2�B
3gB
3gB
3gB
3gB
3gB
3gB
3gB
3�B
3gB
3gB
33B
2�B
33B
2�B
2�B
4B
5?B
5B
6�B
8B
8�B
8�B
9#B
9�B
9�B
:)B
:)B
:^B
:^B
:)B
:^B
:)B
9�B
:)B
9XB
8RB
7�B
8�B
9#B
:^B
:�B
;0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bi�Bk�Bl�BjBl�Bk�Bk�Bm�Bk�Bk�Bl�Bk�Bn.BkPBm]Bk�BlWBm(BjBlWBk�BjJBl�Bl"BlWBlWBj�Bl"BlWBkPBm�BjBl�Bm]Bj�BjJBn.BkBlWBl"Bk�BkPBk�Bj�Bl�Bi�BkBl"BiBk�BkBjBl�Bj�BjJBkBk�Bl�Bk�Bj�Bl�Bj�Bk�BjBj�Bi�Bi�Bl�BkPBk�BkBjJBk�Bi�Bk�BjJBiyBi�BhrBj�Bz�BcBt�Bd%Bg�Bc�Bh>BwfBm�BffBhrBg8Bh
Bh�Bf2Bg8Bh>Bf2Bg8Bh�Bg�Bf�Bh�Bg�BgBh�Bf�Bg�Bh>Bf�BhrBglBg�Bh�Bf�Bh�Bh�BglBiBg8Bh>BhrBgBh
BhrBffBh�Bg�BffBiBg�BgBh�BhrBg8BhrBg�Bf�Bh�Bh>BgBg�BhrBffBhrBh�Bf�BhrBh�Bf�Bh>Bh>Bf�BhrBg�Bg8BhrBffBh�BffBg�Bh�Bf2BglBh�BglBg�BgBf2BiyBglBh
BhrBg�Bf2BhrBjJBiDBh>BjJBk�Bp;Bo Bm(Bq�By�B��B{~B|�B{~B{B|�B{B{~B|PB{JBz�B|�B{JBz�B|PB|�B{JB~(B}�B|B~�B~�B.B��B��B}�B~�B�B�B�B�iB�oB�B�uB�eB��B�\B�B��B�RB��B��B��B��B�B��B��B�hB�qB��B� B�}B�wB�B�B��B�BB�UB�0B�#B��B��B�B��B�EB�KB��B�#B��B�B�KB��B�)B��B�B��BB��B��B�B��B�BB�pB��B�}B��B�cB�B��B��B�B�AB�uB�{B�B�uB�AB�{B�B��B�YB��B�.B�B�B�B$tB#B"�B#B%�B+�B(�B&�B)�B%�B&B'�B'B!�B �B!�B'�BkB�BBIBCB#B1[B2�B49B49B.B,B+B,�B*�B*�B7BGB?�B3gB4�B5?B33B2�B3gB7�B3�B1�B:�B?�BCaBEB=<BA�B:�B,�B-wB/OB,�B,�B1'B,qB%�B&B5�B(�B%�B#�B"3B2aB6�B5?B/OB0�B.B+�B.�B0�B1[B5�B:�B<�B?B>�B@�BE�BMBO�BNpBVmBd�B`�BrGBlWBjJBm]Bl�Bj�Bl"Bn.BncBv�Bn�Br{B�Bv�BuYBw�Bz�BlWBn�BrBp;Bj�Bj�BrBo�Be`Bd%Be,Be�Bc�Bd�Bd%Ba�BbBc�Ba|Bg8Bm�BjB_;B\�B\�B\]Ba�B_�BX�BW�BXyBW
BU�BV8BVmBSZB[#BV�BW
B\]BU�BT,BR�BM�BM5BN<BM�BJ�BIBI�BF�BH�BJ#BI�BN�BF
BO�BEmBE�BA�B>wB<�B:�B9XB:�B8�B5?B49B<jBJWB3�B8�B=<B3gB=�B0�B �B&�B-�B �B)*B!bB!bB$B�B$BYB�B�B�B4B�B@B�B�B�B+BCB�B�B#B��B�B�SB�uB�B��B�VB�|B�GB�cB�8B��B�mB��B��B��B�#B�mB�?B�3B�sB��B�6B��B��B�gB��B��B�B��B�kB�tB�9B�[B�<B��B��B�B��B�@B�hB��B�:B�hB��B�.B��B��B�xB��B�IB�GB��B�@BwfBk�Bk�Bj�Bk�Bm]B\�B~�B�7Bu�Bs�Bq�BsBt�BuYBv+Bt�Br�Bs�BtBtB}�BzBrGBi�Bn�BjBc�BcB`vBaB`�Bh�Bf2B\�BZ�BW
BXEBV8BVmBS&BUgBN�BK^BK�BOBBLdBM5BE9BF
BF�BF�B0 B&�B7KB$�B�B�B�BBYB:B�BB�B�B�B�B�B�BSBuBBB  B
��B
�B(�B
�mB
�BHB
�yB
��B
��B
�KB
�NB
ȴB
�sB
ěB
�3B
҉B
՛B
�8B
�0B
�jB
�OB
�XB
�B
��B
�kB
�B
��B
�nB
�zB
�9B
�RB
�B
��B
�B
�B
��B
��B
�OB
��B
��B
~�B
�:B
~�B
t�B
quB
z�B
�B
glB
g�B
u%B
bNB
cB
bB
_�B
[�B
_pB
e�B
[�B
Z�B
XEB
U2B
X�B
U�B
MB
NB
NB
HB
J�B
J�B
=pB
:^B
>wB
<�B
;dB
8�B
@�B
7KB
%zB
UB
kB
@B
bB
�B
FB
!B

�B
�B
B
�B
�B
	�B	��B	�B	�>B	�>B	�B	�B	�
B	��B	�B	��B	�B	��B	�TB	�B	�pB	�vB	�/B	��B	ݘ4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                 444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721225007                            20230721225007AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122500720230721225007  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500720230721225007QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122500720230721225007QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             