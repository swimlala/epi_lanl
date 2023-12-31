CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  D   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-11-26T02:38:25Z creation; 2022-02-04T23:30:06Z DMQC;      
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
_FillValue        G�O�        =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  W(   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�        ]�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�        ~X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        �x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � .�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�       58   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` OX   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   O�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   U�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   [�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T a�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   b   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   b   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   b   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   b$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � b,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   b�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   b�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    b�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        b�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        b�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       c    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    cArgo profile    3.1 1.2 19500101000000  20211126023825  20220204223520  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_190                 6810_008521_190                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @٥�?@٥�?11  @٥�r� �@٥�r� �@0E׈��@0E׈���dO#9����dO#9���11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@�G�@�G�@�  @޸R@�p�A  A   A+�A@  AaG�A�Q�A��A�Q�A�Q�A���AУ�A�  A�B   B  BQ�B(�B   B((�B0z�B8Q�B@(�BH(�BPQ�BXQ�B`(�Bh  Bp  Bx  B�{B��B��B��B�  B�{B�  B�{B�{B�(�B�(�B�(�B�  B�  B��B�{B�{B�  B�  B�  B�{B��B��B�  B�(�B�{B�{B�{B�{B�{B�{B�{C {C{C  C
=C
=C
  C��C�HC�C��C  C��C  C{C
=C  C��C!�C#�C&  C(  C*
=C,  C.  C/��C1��C4
=C5��C7�HC9��C<  C>  C@
=CB  CD
=CF  CG��CJ  CL  CN
=CP  CR  CT  CU��CX  CZ  C\  C]��C`{Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp  Cr
=Ct{Cv{Cx  Cy��C{�C~  C�  C�C�C�C�  C�  C���C�  C�C�C�  C�  C�  C�  C�  C�C�
=C���C���C���C�  C�C�C�  C�C�C�C�  C���C�  C�
=C�C���C���C���C�  C���C���C�  C�C�C�  C�  C�  C���C���C�  C�C�C�
=C�C�  C���C�C�C�  C���C�  C�C�
=C�C�C�C�  C�  C���C���C�  C�C�C�C�  C�  C���C���C���C�C�C�C�
=C���C���C�  C�  C�  C���C�  C�C�  C�  C�  C�C���C���C���C�  C�  C���C���C���C�  C�  C���C���C���C�C�  C���C���C���C���C���C�  C�C�  C�  C�C�C�
=C�  C���C�  C���C���C���C���C�  C�C���D }qD�D� D�D�D�D� D  D}qD�qD� D�D��D�D� D  D� D�qD	}qD
  D
� D
�qD� D�D� D�qD� D�D��D  D}qD�qD}qD  D}qD�D� D�D��D  D}qD�qD}qD�D�D  D}qD��D��D�D� D�qD� D�D��D�qD}qD�D� D�qD��D�D�D �D � D �qD!}qD"�D"�D"�qD#� D$�D$}qD$�qD%z�D%�qD&� D'�D'�D(�D(z�D(��D)� D*  D*� D+  D+��D,�D,}qD-  D-��D.  D.� D/  D/z�D0  D0� D1�D1��D1�qD2}qD3�D3��D4�D4� D4�qD5� D6  D6� D6�qD7}qD8�D8� D8�qD9� D9�qD:� D;�D;� D;�qD<� D=�D=��D>  D>}qD>�qD?}qD?��D@z�DA  DA��DB�DB��DCDC�DD�DD� DE  DE��DFDF� DG  DG��DH�DH��DI  DIz�DI�qDJ��DK  DK}qDL  DL� DL�qDM}qDN  DN� DN�qDO}qDP�DP��DQ  DQ��DRDR��DS�DS� DT�DT� DU  DU��DV  DV}qDW  DW��DXDX��DY�DY� DZ  DZ� D[  D[��D\D\�D]�D]� D^�D^��D^�qD_}qD`  D`}qD`�qDa}qDa��Dbz�Db�qDc}qDd  Dd��De  De� De�qDf}qDg�Dg��Dg�qDhz�Dh�qDi��Dj�Dj��DkDk��Dk�qDl}qDl�qDmz�Dm�qDn}qDo  Do}qDp  Dp� Dp�qDq� Dr  Dr��Ds�Ds� Dt  Dt� Du�Du� Dv  Dv� Dw�Dw��Dx�Dx��Dy�Dy�Dz  Dz� D{  D{� D|  D|}qD|��D}}qD~  D~��D�D��D�HD�>�D�� D��HD�  D�>�D�~�D��HD�  D�@ D�~�D�� D�HD�AHD�� D�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�HD�@ D�� D�� D�  D�>�D�~�D�� D���D�>�D�� D���D���D�@ D��HD�� D�  D�@ D�� D���D���D�AHD��HD���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?#�
?�  ?�{?���@�\@z�@+�@B�\@Tz�@n{@��\@�=q@�z�@�  @���@�33@��R@�ff@�@�p�@�=q@�
=A   A�A(�A��A
=A{A"�\A(Q�A/\)A3�
A:=qAAG�AEAK�AQ�AW
=A^{Aa�Ag�An{Aq�Ax��A~{A�G�A���A��RA���A��A�  A�=qA�p�A�Q�A��HA�ffA���A��A�
=A���A��
A�
=A���A�z�A��A���A�A��A��HA�AǮA˅A�{A�Q�AӅA�ffAأ�A�z�A޸RA���A�(�A�
=A�G�A�(�A�\)A�A�z�A�  A�=qA�z�B (�BG�B�\BQ�B��B�HBz�B	B
=B��B=qB\)B�B�\B�BG�B�HB  Bp�B33Bz�BB33B ��B"{B#�B%G�B&ffB'�B)G�B*�HB+�
B-B/
=B0Q�B2{B3�B4z�B6ffB7�B8��B:ffB<  B<��B>=qB?�
B@��BB{BC\)BD(�BE��BF�\BG\)BHz�BIBJ=qBK33BLz�BL��BMBN�HBO\)BP  BQ�BQ��BR=qBS33BS�
BTz�BUp�BUBV�HBW�BX  BY�BY��BZ=qB[\)B[�
B\Q�B]��B]B^�RB_�B`  B`��BaBb=qBc\)Bc�
Bdz�Be��Bf{Bf�RBg�
Bhz�Bh��Bi�Bj�HBk\)Bl  Bm�Bm��BnffBo�Bo�Bp��BqBr{Br�HBt  BtQ�Bu�Bv=qBv�HBw�Bx��ByG�By�B{
=B{�B|(�B}p�B~{B~�\B�B�{B�z�B�
=B�\)B��B�=qB�z�B��HB�p�B��B�(�B���B��HB�G�B��
B�{B��\B��B�\)B��B�Q�B��RB���B�\)B��B�=qB��\B��B�p�B�B�=qB��RB�
=B�\)B��B�=qB���B��B�p�B�B�ffB���B�
=B���B��
B�Q�B���B�
=B�p�B�{B�ffB���B�33B���B��B�z�B��HB��B���B�{B�ffB���B�G�B��B�  B��\B��HB�33B��B�(�B�ffB���B�\)B��B�  B��\B���B�33B�B�  B�Q�B���B�G�B��B��B�z�B��HB��B���B�{B�ffB��RB�\)B���B��B�z�B��RB�
=B��B�  B�Q�B���B�33B��B�B�=qB��HB�
=B�\)B�  B�Q�B��\B���B��B��
B�(�B��\B�
=B�G�B��B�=qB�z�B���B�\)B��
B�{B�ffB���B�\)B���B�(�B��\B���B�G�B��
B�{B�z�B���B�G�B��B�  B�z�B���B�G�B���B�(�B��\B���B�33B�B�{B�z�B��RB�\)B��B��B�ffB���B��B�\)B��B�Q�B�z�B���B�p�B��B�  B�z�B���B�G�BÅB�(�Bď\B��HB��BŮB�(�B�ffB���B�G�B�B�  B�Q�B���B�G�BɮB��
B�=qB���B�G�B˙�B��
B�=qB̸RB��B�\)B��
B�ffBθRB�
=Bϙ�B�  B�Q�BУ�B��BѮB��B�=qB���B�G�BӅB��
B�ffB���B�
=Bՙ�B�  B�Q�BָRB�G�Bי�B��B�z�B��HB��BمB�(�B�z�B���B�33B�B�(�B�ffB���B�p�B��B�(�Bޏ\B�33Bߙ�B�  B�Q�B��HB�\)BᙚB�  B�\B�
=B�G�B�B�Q�B�RB�
=B�\)B��B�ffB�\B�33B�B��B�=qB���B�G�B�B�  B�\B�RB�33B�B��B�Q�B��HB�33B�p�B��
B�ffB���B�
=B�\)B��B�ffB��\B��B�B��
B�(�B�RB�
=B�\)B�B�Q�B�\B��HB�\)B��
B�  B��\B�
=B�G�B�B�=qB���B���B�\)B��
B�{B�z�B��B�\)B�B�Q�B��\B���B���B�B�=qB���B��B�p�C 
=C (�C \)C ��C �
C  CG�Cz�C��C��C�CG�C��CC�C33Cz�C�\C��C{CG�Cp�C�C�C33CQ�C��C�C�CG�C��C�
C  C=qC�C��C�C33C�C�RC�HC	(�C	z�C	��C	�
C
33C
ffC
�\C
�
C�CQ�C�CC
=CG�Cp�C�RC  C=qCffC��C�HC(�C\)C�C�
C�CQ�C�C�
C(�C\)C�C�HC33CffC��C�C(�C\)C�C  C33CffC�C  C33CffC�C��CG�Cz�C��C��C33CffC�RC  CG�Cz�C�RC��CG�C�\C��C  C=qC��C�
C
=C=qC�\C��C  C=qC��C��C  CG�C��CC  CQ�C�\C�RC��CQ�Cz�C�C��CG�Cp�C��C�C 33C \)C ��C �HC!33C!\)C!��C!�HC"(�C"Q�C"�\C"�HC#{C#G�C#�C#�
C$�C$Q�C$�C$C%{C%Q�C%�C%C&{C&\)C&�C&C'  C'Q�C'�C'�RC(
=C(G�C(p�C(�C(��C)=qC)p�C)��C)��C*{C*\)C*z�C*�RC+  C+=qC+p�C+��C+�
C,�C,ffC,��C,�RC-
=C-Q�C-�C-�RC-�C.33C.�C.�RC.�C/(�C/p�C/�RC/��C0(�C0Q�C0��C0�C1{C1G�C1�C1�
C2{C2G�C2p�C2�C2��C3=qC3p�C3��C3��C4{C4\)C4�\C4�RC4��C5=qC5z�C5C5��C6(�C6ffC6�RC7  C7G�C7p�C7�C7�C8(�C8p�C8�RC9  C933C9ffC9�C:  C:=qC:�C:�C:�C;=qC;�\C;��C<  C<33C<z�C<��C={C=G�C=z�C=�RC>  C>G�C>�\C>�C>��C?G�C?�C?�RC?�C@33C@z�C@CA  CA(�CAp�CA�RCB
=CB33CBffCB�CB��CC=qCCp�CC��CC�HCD(�CDp�CD��CD�
CE
=CEQ�CE��CE�HCF�CFQ�CF�CFCG  CGQ�CG��CG�
CH
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                               11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @   @B�\@�G�@�G�@�  @޸R@�p�A  A   A+�A@  AaG�A�Q�A��A�Q�A�Q�A���AУ�A�  A�B   B  BQ�B(�B   B((�B0z�B8Q�B@(�BH(�BPQ�BXQ�B`(�Bh  Bp  Bx  B�{B��B��B��B�  B�{B�  B�{B�{B�(�B�(�B�(�B�  B�  B��B�{B�{B�  B�  B�  B�{B��B��B�  B�(�B�{B�{B�{B�{B�{B�{B�{C {C{C  C
=C
=C
  C��C�HC�C��C  C��C  C{C
=C  C��C!�C#�C&  C(  C*
=C,  C.  C/��C1��C4
=C5��C7�HC9��C<  C>  C@
=CB  CD
=CF  CG��CJ  CL  CN
=CP  CR  CT  CU��CX  CZ  C\  C]��C`{Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp  Cr
=Ct{Cv{Cx  Cy��C{�C~  C�  C�C�C�C�  C�  C���C�  C�C�C�  C�  C�  C�  C�  C�C�
=C���C���C���C�  C�C�C�  C�C�C�C�  C���C�  C�
=C�C���C���C���C�  C���C���C�  C�C�C�  C�  C�  C���C���C�  C�C�C�
=C�C�  C���C�C�C�  C���C�  C�C�
=C�C�C�C�  C�  C���C���C�  C�C�C�C�  C�  C���C���C���C�C�C�C�
=C���C���C�  C�  C�  C���C�  C�C�  C�  C�  C�C���C���C���C�  C�  C���C���C���C�  C�  C���C���C���C�C�  C���C���C���C���C���C�  C�C�  C�  C�C�C�
=C�  C���C�  C���C���C���C���C�  C�C���D }qD�D� D�D�D�D� D  D}qD�qD� D�D��D�D� D  D� D�qD	}qD
  D
� D
�qD� D�D� D�qD� D�D��D  D}qD�qD}qD  D}qD�D� D�D��D  D}qD�qD}qD�D�D  D}qD��D��D�D� D�qD� D�D��D�qD}qD�D� D�qD��D�D�D �D � D �qD!}qD"�D"�D"�qD#� D$�D$}qD$�qD%z�D%�qD&� D'�D'�D(�D(z�D(��D)� D*  D*� D+  D+��D,�D,}qD-  D-��D.  D.� D/  D/z�D0  D0� D1�D1��D1�qD2}qD3�D3��D4�D4� D4�qD5� D6  D6� D6�qD7}qD8�D8� D8�qD9� D9�qD:� D;�D;� D;�qD<� D=�D=��D>  D>}qD>�qD?}qD?��D@z�DA  DA��DB�DB��DCDC�DD�DD� DE  DE��DFDF� DG  DG��DH�DH��DI  DIz�DI�qDJ��DK  DK}qDL  DL� DL�qDM}qDN  DN� DN�qDO}qDP�DP��DQ  DQ��DRDR��DS�DS� DT�DT� DU  DU��DV  DV}qDW  DW��DXDX��DY�DY� DZ  DZ� D[  D[��D\D\�D]�D]� D^�D^��D^�qD_}qD`  D`}qD`�qDa}qDa��Dbz�Db�qDc}qDd  Dd��De  De� De�qDf}qDg�Dg��Dg�qDhz�Dh�qDi��Dj�Dj��DkDk��Dk�qDl}qDl�qDmz�Dm�qDn}qDo  Do}qDp  Dp� Dp�qDq� Dr  Dr��Ds�Ds� Dt  Dt� Du�Du� Dv  Dv� Dw�Dw��Dx�Dx��Dy�Dy�Dz  Dz� D{  D{� D|  D|}qD|��D}}qD~  D~��D�D��D�HD�>�D�� D��HD�  D�>�D�~�D��HD�  D�@ D�~�D�� D�HD�AHD�� D�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�HD�@ D�� D�� D�  D�>�D�~�D�� D���D�>�D�� D���D���D�@ D��HD�� D�  D�@ D�� D���D���D�AHD��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?#�
?�  ?�{?���@�\@z�@+�@B�\@Tz�@n{@��\@�=q@�z�@�  @���@�33@��R@�ff@�@�p�@�=q@�
=A   A�A(�A��A
=A{A"�\A(Q�A/\)A3�
A:=qAAG�AEAK�AQ�AW
=A^{Aa�Ag�An{Aq�Ax��A~{A�G�A���A��RA���A��A�  A�=qA�p�A�Q�A��HA�ffA���A��A�
=A���A��
A�
=A���A�z�A��A���A�A��A��HA�AǮA˅A�{A�Q�AӅA�ffAأ�A�z�A޸RA���A�(�A�
=A�G�A�(�A�\)A�A�z�A�  A�=qA�z�B (�BG�B�\BQ�B��B�HBz�B	B
=B��B=qB\)B�B�\B�BG�B�HB  Bp�B33Bz�BB33B ��B"{B#�B%G�B&ffB'�B)G�B*�HB+�
B-B/
=B0Q�B2{B3�B4z�B6ffB7�B8��B:ffB<  B<��B>=qB?�
B@��BB{BC\)BD(�BE��BF�\BG\)BHz�BIBJ=qBK33BLz�BL��BMBN�HBO\)BP  BQ�BQ��BR=qBS33BS�
BTz�BUp�BUBV�HBW�BX  BY�BY��BZ=qB[\)B[�
B\Q�B]��B]B^�RB_�B`  B`��BaBb=qBc\)Bc�
Bdz�Be��Bf{Bf�RBg�
Bhz�Bh��Bi�Bj�HBk\)Bl  Bm�Bm��BnffBo�Bo�Bp��BqBr{Br�HBt  BtQ�Bu�Bv=qBv�HBw�Bx��ByG�By�B{
=B{�B|(�B}p�B~{B~�\B�B�{B�z�B�
=B�\)B��B�=qB�z�B��HB�p�B��B�(�B���B��HB�G�B��
B�{B��\B��B�\)B��B�Q�B��RB���B�\)B��B�=qB��\B��B�p�B�B�=qB��RB�
=B�\)B��B�=qB���B��B�p�B�B�ffB���B�
=B���B��
B�Q�B���B�
=B�p�B�{B�ffB���B�33B���B��B�z�B��HB��B���B�{B�ffB���B�G�B��B�  B��\B��HB�33B��B�(�B�ffB���B�\)B��B�  B��\B���B�33B�B�  B�Q�B���B�G�B��B��B�z�B��HB��B���B�{B�ffB��RB�\)B���B��B�z�B��RB�
=B��B�  B�Q�B���B�33B��B�B�=qB��HB�
=B�\)B�  B�Q�B��\B���B��B��
B�(�B��\B�
=B�G�B��B�=qB�z�B���B�\)B��
B�{B�ffB���B�\)B���B�(�B��\B���B�G�B��
B�{B�z�B���B�G�B��B�  B�z�B���B�G�B���B�(�B��\B���B�33B�B�{B�z�B��RB�\)B��B��B�ffB���B��B�\)B��B�Q�B�z�B���B�p�B��B�  B�z�B���B�G�BÅB�(�Bď\B��HB��BŮB�(�B�ffB���B�G�B�B�  B�Q�B���B�G�BɮB��
B�=qB���B�G�B˙�B��
B�=qB̸RB��B�\)B��
B�ffBθRB�
=Bϙ�B�  B�Q�BУ�B��BѮB��B�=qB���B�G�BӅB��
B�ffB���B�
=Bՙ�B�  B�Q�BָRB�G�Bי�B��B�z�B��HB��BمB�(�B�z�B���B�33B�B�(�B�ffB���B�p�B��B�(�Bޏ\B�33Bߙ�B�  B�Q�B��HB�\)BᙚB�  B�\B�
=B�G�B�B�Q�B�RB�
=B�\)B��B�ffB�\B�33B�B��B�=qB���B�G�B�B�  B�\B�RB�33B�B��B�Q�B��HB�33B�p�B��
B�ffB���B�
=B�\)B��B�ffB��\B��B�B��
B�(�B�RB�
=B�\)B�B�Q�B�\B��HB�\)B��
B�  B��\B�
=B�G�B�B�=qB���B���B�\)B��
B�{B�z�B��B�\)B�B�Q�B��\B���B���B�B�=qB���B��B�p�C 
=C (�C \)C ��C �
C  CG�Cz�C��C��C�CG�C��CC�C33Cz�C�\C��C{CG�Cp�C�C�C33CQ�C��C�C�CG�C��C�
C  C=qC�C��C�C33C�C�RC�HC	(�C	z�C	��C	�
C
33C
ffC
�\C
�
C�CQ�C�CC
=CG�Cp�C�RC  C=qCffC��C�HC(�C\)C�C�
C�CQ�C�C�
C(�C\)C�C�HC33CffC��C�C(�C\)C�C  C33CffC�C  C33CffC�C��CG�Cz�C��C��C33CffC�RC  CG�Cz�C�RC��CG�C�\C��C  C=qC��C�
C
=C=qC�\C��C  C=qC��C��C  CG�C��CC  CQ�C�\C�RC��CQ�Cz�C�C��CG�Cp�C��C�C 33C \)C ��C �HC!33C!\)C!��C!�HC"(�C"Q�C"�\C"�HC#{C#G�C#�C#�
C$�C$Q�C$�C$C%{C%Q�C%�C%C&{C&\)C&�C&C'  C'Q�C'�C'�RC(
=C(G�C(p�C(�C(��C)=qC)p�C)��C)��C*{C*\)C*z�C*�RC+  C+=qC+p�C+��C+�
C,�C,ffC,��C,�RC-
=C-Q�C-�C-�RC-�C.33C.�C.�RC.�C/(�C/p�C/�RC/��C0(�C0Q�C0��C0�C1{C1G�C1�C1�
C2{C2G�C2p�C2�C2��C3=qC3p�C3��C3��C4{C4\)C4�\C4�RC4��C5=qC5z�C5C5��C6(�C6ffC6�RC7  C7G�C7p�C7�C7�C8(�C8p�C8�RC9  C933C9ffC9�C:  C:=qC:�C:�C:�C;=qC;�\C;��C<  C<33C<z�C<��C={C=G�C=z�C=�RC>  C>G�C>�\C>�C>��C?G�C?�C?�RC?�C@33C@z�C@CA  CA(�CAp�CA�RCB
=CB33CBffCB�CB��CC=qCCp�CC��CC�HCD(�CDp�CD��CD�
CE
=CEQ�CE��CE�HCF�CFQ�CF�CFCG  CGQ�CG��CG�
CH
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                               11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�K�A�I�A�M�A�K�A�=qA�=qA�E�A�G�A�C�A�?}A�?}A�1'A�9XA�1'A�(�A�A���A���A��A��A��`A��
AԺ^AԍPA�jA�XA�XA�ZA�ZA�ZA�ZA�VA�S�A�7LA�{A���A��A�p�A�5?A�JA�VA���A��TAҡ�A�K�A��A���A��`A͸RA̕�A�=qA�I�A� �AǇ+AĮA��A��`A��A��#A�E�A��/A���A�?}A��jA���A�bNA���A�%A�I�A���A��\A���A��mA�JA���A�x�A�ȴA�t�A���A��A��A�JA��A�JA���A�G�A�ƨA��A�p�A�bA���A��hA�E�A��mA��A�bA��\A�+A�VA�M�A��HA�33A��A�A�G�A��7A�A�n�A�ĜA�^5A~1'Aw��As�Ap��Ao
=Ai�
Ae�Act�AaG�A^�yA\��A[`BAYO�AV�+AT�!AR�`AQVAO7LANbALz�AJ-AG\)ADA�AA�FA?C�A=33A;��A9��A8Q�A7"�A4�RA3�wA2��A2JA0��A/�A.  A,5?A)�A(�RA'�hA&1'A%S�A$��A$1'A#�-A#G�A"�A"9XA ~�A�jA��An�A"�A��A1'A�A�AVAVAr�AA��A|�AXA��A�A�A  AA�wAt�A�A
v�A	��A	/A�/A�Ap�Ap�A/AA�A�hA�\A�A��Al�A
=A~�A  A�
A��AbNAr�A��A��A%A�A �A�wA��AdZA ��A (�@�o@���@���@��!@���@��D@��R@�E�@�5?@���@�x�@��@�1'@�@땁@ꟾ@��@�/@�I�@�1'@���@��@��
@�7@�w@��#@���@���@�dZ@���@ޟ�@��@��#@ݡ�@�V@���@�1@�|�@ڧ�@ڰ!@�ȴ@���@�ȴ@ڇ+@�=q@�@���@��@�-@��`@���@��@�^5@�M�@��@�7L@�;d@�^5@�x�@�X@ѩ�@�J@�ff@ҧ�@�@�&�@��@�?}@�hs@ёh@ёh@���@��y@�n�@�V@ͺ^@��
@˾w@˾w@ˮ@ʧ�@���@ǅ@���@őh@��`@�Z@�1@î@î@Õ�@�\)@��@��@���@�ff@��7@�bN@� �@��@�
=@���@��-@�`B@��@��j@��9@�r�@�Q�@�Z@�Q�@��w@�ȴ@�V@�-@�$�@�J@�/@��`@���@��F@�l�@�C�@��@�-@���@���@��7@�p�@�X@�/@�V@��@�1@�ƨ@�S�@�+@��y@��R@��+@�^5@���@��^@�x�@�O�@�/@�V@���@�j@�A�@��m@���@�|�@�\)@�+@��!@�^5@�-@��^@�O�@�r�@�+@��R@���@���@�v�@�E�@��T@��7@�X@��@�%@��j@��D@�9X@���@�"�@�
=@��@��!@���@���@��j@��j@��@��D@�9X@��
@�dZ@�@�n�@�{@���@��@��@���@��h@�p�@�X@���@�(�@���@�t�@�@���@�v�@�5?@�@���@���@���@��@�hs@�G�@�7L@��@�%@���@��/@��/@�I�@��@��F@��@�\)@�"�@��@��!@�M�@�@��#@��7@�G�@�V@���@�I�@�b@���@��
@���@�K�@��H@���@��\@�v�@��@��^@�/@���@��@���@���@��@�Q�@��@��
@��F@�|�@�dZ@��@���@���@���@��+@�ff@�E�@�$�@��#@���@�p�@��@�z�@�A�@� �@�1@��m@��F@���@�33@��@���@��R@���@��+@�v�@�n�@�^5@�V@�@��@�G�@�%@��`@�Ĝ@�z�@��P@�C�@�o@�@���@��y@���@��!@��+@�M�@��@���@��@�G�@��@�%@��@���@�Z@�I�@�I�@�I�@�I�@�  @��w@���@�l�@�@��y@���@���@�-@���@���@��/@�Q�@�1@��P@�dZ@���@�~�@�ff@�M�@�$�@�J@�@�/@���@���@��`@��/@���@�Ĝ@��u@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�M�A�K�A�I�A�E�A�O�A�K�A�K�A�O�A�M�A�O�A�Q�A�C�A�G�A�;dA�7LA�7LA�?}A�9XA�G�A�E�A�E�A�M�A�A�A�E�A�E�A�A�A�A�A�?}A�?}A�C�A�?}A�5?A�/A�9XA�/A�+A�+A�5?A�7LA�9XA�?}A�?}A�7LA�;dA�7LA�+A�/A�$�A�$�A�+A�"�A�$�A�/A��A�1A�  A���A���A�A���A���A�A���A���A���A���A���A���A���A���A��A���A��A��A��A��A��yA��A��A��yA��A��yA��`A��mA��yA��TA��HA��`A��HA��/A��/A���A���A�ȴA���AԸRAԺ^AԶFAԲ-AԴ9Aԛ�AԑhAԃA�~�A�v�A�x�A�n�A�dZA�dZA�bNA�ZA�XA�XA�\)A�XA�VA�\)A�XA�VA�ZA�ZA�XA�\)A�XA�XA�\)A�ZA�VA�\)A�XA�XA�ZA�^5A�ZA�ZA�\)A�XA�ZA�\)A�ZA�^5A�\)A�VA�XA�\)A�XA�ZA�ZA�S�A�XA�ZA�XA�VA�ZA�S�A�S�A�XA�VA�VA�XA�Q�A�XA�VA�O�A�S�A�O�A�O�A�S�A�K�A�K�A�K�A�;dA�?}A�?}A�33A�/A�1'A�+A�&�A��A��A��A��A�oA��A�{A�bA�oA�bA�
=A�A�A���A���A�A���A���A���A���A���A���A��A��A���A��A��A��TA�ƨAӶFAӶFAӮAӥ�Aә�AӓuAӋDA�|�A�n�A�`BA�S�A�E�A�C�A�G�A�A�A�;dA�;dA�5?A�5?A�5?A�+A�"�A��A�VA�VA�oA�VA�JA�bA�bA�%A�%A�1A�
=A�
=A�{A�oA�bA�oA�oA�bA�1A�1A�%A�A�A���A���A���A��A��A��A��A��yA��A��TA��mA��A��mA��`A��yA���A���A�ĜA���AҶFAҬAҧ�Aқ�AґhA҃A�r�A�hsA�dZA�\)A�VA�S�A�A�A�9XA�9XA�=qA�7LA�=qA�-A��A��A�oA�A���A��A���Aѩ�A�r�A�`BA�S�A�G�A�&�A��HAЕ�A�hsA�C�A�33A�$�A��yAϝ�A�E�A�VA��/Aδ9A΍PA�`BA�K�A�7LA�9XA�33A��A���A��yAʹ9A�r�A�^5A�E�A�5?A�-A�+A���A̼jḀ�A̗�A̅A�S�A��A��A���Aˏ\A�n�A�dZA�C�A�5?A��A�
=A���A��mAʙ�A�VA�+Aɕ�A�/A��#A�ȴAȼjAȥ�AȅA�l�A�XA�C�A�1'A� �A�{A�
=A�A�A���A���A��yA���AǮAǗ�AǅA�~�A�jA�\)A�G�A�1'A���A�^5A���A�VA�l�A���A��mA�AÑhA�hsA�\)A�C�A�=qA�5?A�+A��A�1A���A��A��TA�ƨA¥�AA�|�A�n�A�A�A��A���A�oA���A�t�A�ffA�XA�M�A�C�A��A�t�A��^A��A�hsA�;dA��A�1A���A��yA��;A�ƨA��FA���A���A��7A�C�A���A���A�C�A���A��`A��9A��PA��A�|�A�ffA�;dA�$�A��A���A��TA���A�M�A��/A��\A�E�A�{A���A��
A���A���A��DA�t�A�bNA�`BA�XA�M�A�G�A�A�A�9XA�/A�"�A�oA���A��
A���A���A���A��jA���A���A�|�A�|�A�~�A�x�A�jA�+A��TA��wA���A��hA��A�z�A�~�A�x�A�t�A�p�A�t�A�p�A�`BA�VA�?}A�(�A��A�JA�  A��A��mA��;A���A��9A���A�n�A�=qA�+A�{A���A�hsA��
A���A�G�A��/A���A�r�A���A��A�I�A�=qA�33A��A�  A��HA��FA��A�G�A�"�A�  A��RA�I�A�7LA��A�A��yA��#A�ĜA���A��+A�|�A�v�A�t�A�hsA�VA�?}A�(�A��A�1A���A��;A���A���A���A���A�jA�A�A�
=A���A���A�v�A�Q�A�;dA�&�A��A�oA�
=A�1A�A��A��TA��TA��TA��A��
A��A���A�A��^A��9A���A���A��DA��+A�t�A�C�A�-A��A�  A��yA��
A���A���A���A��DA�p�A�C�A�  A��
A���A��A��A���A�{A�jA��A���A�ĜA��FA��hA�z�A�K�A�5?A�-A�&�A��A�{A�
=A���A��A��`A���A�ĜA��^A���A��A�ffA�M�A�1'A�&�A��A�oA�
=A���A��A���A�VA��A��mA���A���A���A�A���A�~�A�t�A�S�A��yA��jA��9A���A��+A�%A���A��wA��A��A��A���A���A���A��A�ffA�`BA�K�A�oA��A���A��-A�bNA��A��9A�p�A�A�A�/A�$�A�JA���A���A��A��`A��;A��A���A���A���A��wA���A�v�A�9XA�{A���A��mA���A��A�1'A���A���A���A�p�A�O�A�-A��A�VA�1A�A���A��HA���A��7A��A�  A��A�ȴA�n�A�9XA�bA���A��A���A��-A���A�x�A�&�A��
A�~�A�^5A�%A�`BA���A���A��A�z�A�t�A�^5A�C�A�-A��A�A���A��`A���A���A�S�A��A���A��/A���A���A��
A���A�|�A�E�A�1'A�&�A��9A��-A���A�Q�A�jA��TA���A�p�A�7LA��A��A�ƨA��A��DA�dZA�7LA��A�
=A��yA��/A�ȴA���A��hA�p�A�M�A�A�A�7LA�33A�"�A�{A�JA�%A���A��yA���A�A���A��\A��A�l�A�S�A�9XA�"�A�JA��A���A���A�bNA�&�A�A���A��A�XA�=qA�-A��A�oA�%A���A���A��A��TA��
A���A�ĜA��-A�p�A�1A��wA�I�A�&�A��A���A�l�A�hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                               11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�K�A�I�A�M�A�K�A�=qA�=qA�E�A�G�A�C�A�?}A�?}A�1'A�9XA�1'A�(�A�A���A���A��A��A��`A��
AԺ^AԍPA�jA�XA�XA�ZA�ZA�ZA�ZA�VA�S�A�7LA�{A���A��A�p�A�5?A�JA�VA���A��TAҡ�A�K�A��A���A��`A͸RA̕�A�=qA�I�A� �AǇ+AĮA��A��`A��A��#A�E�A��/A���A�?}A��jA���A�bNA���A�%A�I�A���A��\A���A��mA�JA���A�x�A�ȴA�t�A���A��A��A�JA��A�JA���A�G�A�ƨA��A�p�A�bA���A��hA�E�A��mA��A�bA��\A�+A�VA�M�A��HA�33A��A�A�G�A��7A�A�n�A�ĜA�^5A~1'Aw��As�Ap��Ao
=Ai�
Ae�Act�AaG�A^�yA\��A[`BAYO�AV�+AT�!AR�`AQVAO7LANbALz�AJ-AG\)ADA�AA�FA?C�A=33A;��A9��A8Q�A7"�A4�RA3�wA2��A2JA0��A/�A.  A,5?A)�A(�RA'�hA&1'A%S�A$��A$1'A#�-A#G�A"�A"9XA ~�A�jA��An�A"�A��A1'A�A�AVAVAr�AA��A|�AXA��A�A�A  AA�wAt�A�A
v�A	��A	/A�/A�Ap�Ap�A/AA�A�hA�\A�A��Al�A
=A~�A  A�
A��AbNAr�A��A��A%A�A �A�wA��AdZA ��A (�@�o@���@���@��!@���@��D@��R@�E�@�5?@���@�x�@��@�1'@�@땁@ꟾ@��@�/@�I�@�1'@���@��@��
@�7@�w@��#@���@���@�dZ@���@ޟ�@��@��#@ݡ�@�V@���@�1@�|�@ڧ�@ڰ!@�ȴ@���@�ȴ@ڇ+@�=q@�@���@��@�-@��`@���@��@�^5@�M�@��@�7L@�;d@�^5@�x�@�X@ѩ�@�J@�ff@ҧ�@�@�&�@��@�?}@�hs@ёh@ёh@���@��y@�n�@�V@ͺ^@��
@˾w@˾w@ˮ@ʧ�@���@ǅ@���@őh@��`@�Z@�1@î@î@Õ�@�\)@��@��@���@�ff@��7@�bN@� �@��@�
=@���@��-@�`B@��@��j@��9@�r�@�Q�@�Z@�Q�@��w@�ȴ@�V@�-@�$�@�J@�/@��`@���@��F@�l�@�C�@��@�-@���@���@��7@�p�@�X@�/@�V@��@�1@�ƨ@�S�@�+@��y@��R@��+@�^5@���@��^@�x�@�O�@�/@�V@���@�j@�A�@��m@���@�|�@�\)@�+@��!@�^5@�-@��^@�O�@�r�@�+@��R@���@���@�v�@�E�@��T@��7@�X@��@�%@��j@��D@�9X@���@�"�@�
=@��@��!@���@���@��j@��j@��@��D@�9X@��
@�dZ@�@�n�@�{@���@��@��@���@��h@�p�@�X@���@�(�@���@�t�@�@���@�v�@�5?@�@���@���@���@��@�hs@�G�@�7L@��@�%@���@��/@��/@�I�@��@��F@��@�\)@�"�@��@��!@�M�@�@��#@��7@�G�@�V@���@�I�@�b@���@��
@���@�K�@��H@���@��\@�v�@��@��^@�/@���@��@���@���@��@�Q�@��@��
@��F@�|�@�dZ@��@���@���@���@��+@�ff@�E�@�$�@��#@���@�p�@��@�z�@�A�@� �@�1@��m@��F@���@�33@��@���@��R@���@��+@�v�@�n�@�^5@�V@�@��@�G�@�%@��`@�Ĝ@�z�@��P@�C�@�o@�@���@��y@���@��!@��+@�M�@��@���@��@�G�@��@�%@��@���@�Z@�I�@�I�@�I�@�I�@�  @��w@���@�l�@�@��y@���@���@�-@���@���@��/@�Q�@�1@��P@�dZ@���@�~�@�ff@�M�@�$�@�J@�@�/@���@���@��`@��/@���@�Ĝ@��u@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�M�A�K�A�I�A�E�A�O�A�K�A�K�A�O�A�M�A�O�A�Q�A�C�A�G�A�;dA�7LA�7LA�?}A�9XA�G�A�E�A�E�A�M�A�A�A�E�A�E�A�A�A�A�A�?}A�?}A�C�A�?}A�5?A�/A�9XA�/A�+A�+A�5?A�7LA�9XA�?}A�?}A�7LA�;dA�7LA�+A�/A�$�A�$�A�+A�"�A�$�A�/A��A�1A�  A���A���A�A���A���A�A���A���A���A���A���A���A���A���A��A���A��A��A��A��A��yA��A��A��yA��A��yA��`A��mA��yA��TA��HA��`A��HA��/A��/A���A���A�ȴA���AԸRAԺ^AԶFAԲ-AԴ9Aԛ�AԑhAԃA�~�A�v�A�x�A�n�A�dZA�dZA�bNA�ZA�XA�XA�\)A�XA�VA�\)A�XA�VA�ZA�ZA�XA�\)A�XA�XA�\)A�ZA�VA�\)A�XA�XA�ZA�^5A�ZA�ZA�\)A�XA�ZA�\)A�ZA�^5A�\)A�VA�XA�\)A�XA�ZA�ZA�S�A�XA�ZA�XA�VA�ZA�S�A�S�A�XA�VA�VA�XA�Q�A�XA�VA�O�A�S�A�O�A�O�A�S�A�K�A�K�A�K�A�;dA�?}A�?}A�33A�/A�1'A�+A�&�A��A��A��A��A�oA��A�{A�bA�oA�bA�
=A�A�A���A���A�A���A���A���A���A���A���A��A��A���A��A��A��TA�ƨAӶFAӶFAӮAӥ�Aә�AӓuAӋDA�|�A�n�A�`BA�S�A�E�A�C�A�G�A�A�A�;dA�;dA�5?A�5?A�5?A�+A�"�A��A�VA�VA�oA�VA�JA�bA�bA�%A�%A�1A�
=A�
=A�{A�oA�bA�oA�oA�bA�1A�1A�%A�A�A���A���A���A��A��A��A��A��yA��A��TA��mA��A��mA��`A��yA���A���A�ĜA���AҶFAҬAҧ�Aқ�AґhA҃A�r�A�hsA�dZA�\)A�VA�S�A�A�A�9XA�9XA�=qA�7LA�=qA�-A��A��A�oA�A���A��A���Aѩ�A�r�A�`BA�S�A�G�A�&�A��HAЕ�A�hsA�C�A�33A�$�A��yAϝ�A�E�A�VA��/Aδ9A΍PA�`BA�K�A�7LA�9XA�33A��A���A��yAʹ9A�r�A�^5A�E�A�5?A�-A�+A���A̼jḀ�A̗�A̅A�S�A��A��A���Aˏ\A�n�A�dZA�C�A�5?A��A�
=A���A��mAʙ�A�VA�+Aɕ�A�/A��#A�ȴAȼjAȥ�AȅA�l�A�XA�C�A�1'A� �A�{A�
=A�A�A���A���A��yA���AǮAǗ�AǅA�~�A�jA�\)A�G�A�1'A���A�^5A���A�VA�l�A���A��mA�AÑhA�hsA�\)A�C�A�=qA�5?A�+A��A�1A���A��A��TA�ƨA¥�AA�|�A�n�A�A�A��A���A�oA���A�t�A�ffA�XA�M�A�C�A��A�t�A��^A��A�hsA�;dA��A�1A���A��yA��;A�ƨA��FA���A���A��7A�C�A���A���A�C�A���A��`A��9A��PA��A�|�A�ffA�;dA�$�A��A���A��TA���A�M�A��/A��\A�E�A�{A���A��
A���A���A��DA�t�A�bNA�`BA�XA�M�A�G�A�A�A�9XA�/A�"�A�oA���A��
A���A���A���A��jA���A���A�|�A�|�A�~�A�x�A�jA�+A��TA��wA���A��hA��A�z�A�~�A�x�A�t�A�p�A�t�A�p�A�`BA�VA�?}A�(�A��A�JA�  A��A��mA��;A���A��9A���A�n�A�=qA�+A�{A���A�hsA��
A���A�G�A��/A���A�r�A���A��A�I�A�=qA�33A��A�  A��HA��FA��A�G�A�"�A�  A��RA�I�A�7LA��A�A��yA��#A�ĜA���A��+A�|�A�v�A�t�A�hsA�VA�?}A�(�A��A�1A���A��;A���A���A���A���A�jA�A�A�
=A���A���A�v�A�Q�A�;dA�&�A��A�oA�
=A�1A�A��A��TA��TA��TA��A��
A��A���A�A��^A��9A���A���A��DA��+A�t�A�C�A�-A��A�  A��yA��
A���A���A���A��DA�p�A�C�A�  A��
A���A��A��A���A�{A�jA��A���A�ĜA��FA��hA�z�A�K�A�5?A�-A�&�A��A�{A�
=A���A��A��`A���A�ĜA��^A���A��A�ffA�M�A�1'A�&�A��A�oA�
=A���A��A���A�VA��A��mA���A���A���A�A���A�~�A�t�A�S�A��yA��jA��9A���A��+A�%A���A��wA��A��A��A���A���A���A��A�ffA�`BA�K�A�oA��A���A��-A�bNA��A��9A�p�A�A�A�/A�$�A�JA���A���A��A��`A��;A��A���A���A���A��wA���A�v�A�9XA�{A���A��mA���A��A�1'A���A���A���A�p�A�O�A�-A��A�VA�1A�A���A��HA���A��7A��A�  A��A�ȴA�n�A�9XA�bA���A��A���A��-A���A�x�A�&�A��
A�~�A�^5A�%A�`BA���A���A��A�z�A�t�A�^5A�C�A�-A��A�A���A��`A���A���A�S�A��A���A��/A���A���A��
A���A�|�A�E�A�1'A�&�A��9A��-A���A�Q�A�jA��TA���A�p�A�7LA��A��A�ƨA��A��DA�dZA�7LA��A�
=A��yA��/A�ȴA���A��hA�p�A�M�A�A�A�7LA�33A�"�A�{A�JA�%A���A��yA���A�A���A��\A��A�l�A�S�A�9XA�"�A�JA��A���A���A�bNA�&�A�A���A��A�XA�=qA�-A��A�oA�%A���A���A��A��TA��
A���A�ĜA��-A�p�A�1A��wA�I�A�&�A��A���A�l�A�hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                               11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��By	Bx�Bw�Bx8By	BwfBx8Bw�Bw�Bx8BxlBxBw�Bx�BwfBy	BxBxlBx�Bx�BzB}"B��B��B�B��B�+B�_B��B�1B�eB�7B��B��B�tB�kB��B�eB�zB��B��B�B��B��B��B�nB�kB�B��B�+B��B�Bv�BiyB�B�B��B�B��B�BBuB�B{B�B�BB�B�xB��B��B�BBBFB{B@B�B�B�B�B�B�B�B�B��B�B��B�EB��B��B��B��B�	Bu%B`�B>B
��B
�
B
ݘB
��B
�B
��B
�UB
�B
��B
�~B
��B
}�B
ncB
c B
MjB
5?B
OB
\B	��B	�NB	՛B	�tB	�RB	�=B	��B	��B	�xB	��B	z�B	s�B	h>B	a�B	X�B	Q�B	B�B	;�B	/�B	"�B	�B	eB	:B	(B	
�B	�B	MB	AB��B�B�B��B�B�B��B��B�B�TB�ZB� B�&B�B�TB�&B�B�>B�B��B��B�B�`B��B��B�8B��B	oB	 �B��B�B�B�B�>B�(B�B��B��B��B�(B�cB��B	 iB��B	B	%B	�B	
�B	PB	:B	�B	�B	"4B	#�B	&LB	$B	&LB	(�B	?�B	M�B	\]B	d&B	r|B	v`B	}VB	|�B	}�B	�B	��B	��B	��B	��B	��B	�B	~(B	|�B	�GB	�B	�B	��B	��B	|B	p;B	o5B	ncB	sMB	t�B	s�B	r|B	rGB	v`B	�MB	��B	�MB	��B	{�B	u�B	t�B	u�B	wfB	{B	}"B	��B	�%B	�+B	��B	�xB	�VB	��B	�_B	��B	�B	�=B	�IB	��B	��B	�B	��B	�B	�B	��B	�!B	��B	�hB	�B	��B	��B	�RB	��B	��B	��B	��B	��B	ÖB	�B	�B	��B	ʌB	͟B	��B	҉B	��B	�B	�2B	�[B	��B	��B	�B	�<B	͟B	�<B	ѷB	��B	�aB	�2B	�B	خB	�EB	�B	چB	�]B	�B	�,B	�mB	�B	�QB	��B	��B	�]B	�B	�B	��B	��B	�B	�B	��B	�`B	��B	�DB
  B
oB
�B
�B
�B
B
JB
~B
JB
(B
�B
�B
.B
\B
�B
 B
�B
�B
oB
�B
B
@B
uB
�B
B
�B
�B
�B
�B
CB
�B
B
IB
OB
�B
�B
VB
!B
!B
�B
 'B
 'B
 �B
!bB
!bB
!bB
!�B
"hB
"�B
"�B
#�B
$B
&LB
(�B
)�B
)�B
)�B
)�B
)�B
*eB
*�B
+6B
+�B
+�B
,B
,qB
-B
.IB
.}B
.�B
.}B
.}B
0�B
33B
33B
33B
33B
3hB
4B
4�B
5?B
5�B
6�B
7LB
7�B
7�B
7�B
7�B
8B
8RB
8B
8�B
:^B
:�B
:�B
;0B
;�B
;�B
<6B
<jB
<�B
<�B
<�B
<�B
=B
=<B
=B
=<B
<�B
<�B
<�B
<B
>B
>B
>�B
?B
?HB
?�B
@B
@B
A B
AUB
A B
B'B
B'B
B'B
CaB
C-B
C�B
CaB
C�B
C�B
DgB
E9B
EmB
EB
E9B
EmB
F?B
GzB
G�B
G�B
HB
HKB
HB
H�B
H�B
I�B
I�B
J#B
J#B
J�B
J�B
K�B
K�B
L0B
L0B
LdB
L�B
M6B
MjB
MjB
N<B
N�B
N�B
N�B
OB
OB
OvB
OvB
PB
PB
O�B
PB
PHB
PHB
PHB
PB
PHB
O�B
P�B
QB
QNB
QNB
QNB
QNB
Q�B
S[B
S[B
S�B
S�B
S�B
S�B
S�B
T,B
T�B
T�B
U2B
U�B
VmB
VmB
V�B
V�B
V�B
W�B
W�B
W�B
WsB
W�B
WsB
XEB
XEB
XyB
X�B
YKB
X�B
YB
YB
Z�B
Z�B
[�B
^B
^jB
^�B
^�B
_B
`�B
aB
aB
aHB
a|B
a|B
b�B
b�B
b�B
b�B
b�B
bNB
bNB
bB
a�B
bB
b�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BxBy>BxlBzxBy�Bu%By	BwfBwfBx8BxBwfBy	B{By>By�Bx8BwfBy>Bv�Bx�Bx8BuZB{�BwfBu�BzxBx�Bv`By�By>Bw2Bz�Bu�Bs�BzxBy�Bx�BxlBw�By>Bw2BwfBzBx�By>By�Bv�BxlBx8Bv�BxBx�Bt�B}�BwfBz�Bx8Bx�Bu�By	Bw�Bu�By>By	BwfByrBv�By>BxBxBy�Bw�BxByrBx�BxBzBwfBx�By�Bx8By>BzxBy�BxlBz�B{ByrBzxB{�B{JB~�B�;B�;B��B��B��B��B�1B�_B�PB��B��B�(B�hB�.B�B�B�FB�{B�SB��B��B��B��B�1B��B��B��B��B��B�1B��B��B�1B��B�+B��B��B�eB��B��B��B�eB��B�_B�B��B��B�kB��B�eB��B�eB��B�B�1B��B��B��B�1B��B�	B��B��B�	B��B��B�kB��B��B��B�B�	B��B�=B�=B��B�CB�CB��B�'B��B�~B�-B��B��B��B��B�B�B��B�tB��B��B��B�zB�tB��B�XB�eB��B�kB�kB�0B��B��B��B��B��B��B��B�qB�B�=B�=B��B�UB��B�6B�=B�B��B��B�CB��B�kB�B�RB�$B��B��B��B��B�zB��B��B�nB��B�FB�B��B��B�bB��B�B��B��B��B�4B�'B��B��B�B�LB�RB��B��B�RB�B�B��B��B�B�B�B�B��B��B��B��B�_B��B�*B�eB�*B��B�0B�eB��B��B��B��B�kB��B�*B�kB��B��B��B�RB��B��B�zB�B��B�:B��B��B�hB��B�FB�tB�tB�nB�:B��B�hB��B��B�4B�~B��B�qB��B��B��B�	B��B�B�B�B��B��B��B��B�VB�~B�VB��B�_B��B��B��B��B��B�xB�eB�_B�+B�B��B�{B��B�SB��B�{B�MB��B��B�B�{B�B��B�4B��B��B��B�B�B�xB�B��B��B�~B��B�JB�MB�{B��B�YB��B|�B�By>Bw2Bs�Bt�Bs�Bo�Bq�BqBs�BoiBo�Bm]BhsBe�BffBe�BcTB`vBk�BxBu%B~�B�SB��B��B�SB�eB�7B��B�IB�=B�	B��B��B��B��B�	B��B�\B�-B��B��B�bB��B�XB�BΥB��B�B�HBбBѷB��B�`B�]B�DB��B��B�JB�JB��B�rB�8B�B�JB�	B�rB�fB�"B{BMB
=B�B�B{B_B�B�B{B�BDB+B�B	�B	lB�BB�BYB�B�B4B@B�B�BBB{B�B�BhBoB�BhBoB@B�B@B_B�BhBBSB�B{B�BBBB�BB	B7B$B�BB�BB�B�B{B�B�B�B�B�B�B�B�BFBMBuB:BuB�BYB�BB�B"B1B�BBYBB�B�cB�B	7B��B��B��B�B��B��B�lB�JB��B��B��B��B�B��B�JB�BB�.B��B��BB��B��B��B��B�cBBuB�BB�B�B�B�B�B�B�B�B�B�B7B+B�B�B�BeB�B�BB�B{B+B�BFB@B�B{B�B�B{BFB�B�BFB�B�BFB7BuB�B{B�B�BhB BB�BB$BeB�B_B{B)�B#B+6B)�B�B{B\BuBoB�BFB�B�B\B�B(B�B�BPB�BPB�BJB�B�B�BB�B
�BDB�B�B_BDB{B�B"B	�B�B�BoBB�B+B��BB B�.B�>B�BoB�B�PB�8B�8B��B��B�ZB��B��B�`B�%B�B�B�`B�B��B�B��B�%B��B�QB��B� BߤB�B�;B�5B�/BںB�WB��B�WB��BרB�KB�WB��B��B�[B�TBбB�NB�&B�aB˒BɆB�dB�B��B�UB��B�<B�B�dB�$B��B��BǮB�qB�OB��B�UB�}B�eB��B�$B��B�B��B�B�$B�nB��B��B��B�:B��B��B�B~(B}�Bz�B|�B{By	BwfBs�Bq�BpoBp�BwfBqABiyBc�BcTB[WB_BuZBhsBI�BH�B5�B1�BM�BlWB8�B�B0!B�B
��B
��B
�>B
�ZB
�MB
�B
��B
�5B
�"B
�B
�mB
�ZB
�B
�TB
��B
��B
��B
�NB
��B
�]B
�QB
ںB
�WB
�B
��B
��B
�?B
ںB
��B
֡B
�B
��B
�B
��B
�}B
�NB
̘B
�XB
��B
��B
��B
�^B
�B
�B
�'B
ǮB
��B
�tB
�3B
�-B
�-B
��B
�!B
��B
�wB
�IB
��B
�kB
��B
��B
��B
��B
�=B
�XB
��B
�:B
�=B
�MB
�:4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                               44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                               44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021112602382520211126023825IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021120523314120211205233141QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021120523314120211205233141QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365520220126093655IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295720220204232957IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295720220204232957IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295720220204232957IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                