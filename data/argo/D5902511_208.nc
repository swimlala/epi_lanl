CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-04-16T03:09:13Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 <  U�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  \,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 <  u   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  {P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 <  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 <  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҁ   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 < P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 
�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 < #t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � )�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` B�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   B�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   H�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   N�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T T�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   UL   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   UT   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   U\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   Ud   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � Ul   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   U�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   V   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    V   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        V0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        V8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       V@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    VHArgo profile    3.1 1.2 19500101000000  20220416030913  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_208                 6810_008521_208                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @���"��@���"��11  @���PH�@���PH�@0T��#y@0T��#y�d��y���d��y��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @@  @�  @�  @�  @޸R@��RA  A ��A,��A?\)A^�RA�Q�A�Q�A�  A��A��A�Q�A�Q�A�Q�B   B�
B  B  B   B'�
B/�B7�
B@(�BH  BP(�BXQ�B`(�Bh(�Bo�
Bx  B�  B�  B�  B��B�  B�  B�{B�{B�{B��
B��B��B�  B�{B�  B�  B�{B��B��
B�  B�{B�{B�  B�{B�  B�(�B�(�B�  B�  B�  B�  B�  C 
=C  C
=C��C�C	��C
=C��C
=C
=C
=C
=C{C{C  C  C�C!��C${C&{C({C*  C+��C.  C/��C2
=C4
=C6
=C8  C:  C<  C>  C@  CB
=CD
=CF  CH  CI��CL
=CN
=CO��CQ��CT
=CV
=CX
=CZ  C\  C]��C_��Ca�Cc��Ce��Ch
=Cj
=Cl
=Cn  Cp  Cr  Cs��Cu��Cw��Cy��C|  C~
=C��C�  C�  C���C�C�  C�C�
=C�C�  C���C�  C���C�  C�C���C���C���C���C�  C�C���C���C�C���C�C�  C���C�  C�C�  C���C�  C�\C�  C���C�
=C�C���C�  C���C�C�C���C���C�  C���C���C�  C�  C�C�
=C�C�  C���C�C�C�
=C�
=C�
=C�\C�C�  C�  C���C�C�C�  C�C�
=C�C�  C�  C�  C�  C���C�  C�  C�  C�  C���C���C���C���C�  C���C�  C�  C���C���C�  C�C�C�C�C�C�  C�C�  C���C�  C���C���C�  C���C���C�  C�  C���C�  C���C���C���C�  C�C�C�  C�C�C�  C�C�  C���C�  C�C�
=C�C���C���D � D  D� D�qD}qD�qD� D�D�D  D}qD  D� D�D��D�D��D	�D	}qD	�qD
z�D
��D}qD��DxRD�qD��D�D� D  D� D  D� D�qD��D  D}qD�D��D�D�D�D� D  D� D  D}qD  D� D  D��D  D� D�D}qD��D}qD  D}qD�qD��D�D� D �D ��D!�D!��D!�qD"}qD#  D#� D#�qD$z�D%  D%�D&�D&��D'  D'}qD'��D(}qD(�qD)� D*  D*� D+�D+� D,  D,� D-  D-}qD.  D.�D/  D/� D/�qD0}qD0�qD1}qD2  D2� D3  D3� D4�D4��D5  D5}qD6�D6��D7D7��D7�qD8}qD9�D9��D:  D:}qD:�qD;��D<�D<��D=  D=� D>�D>��D?  D?� D@  D@}qD@�qDA� DA�qDBz�DB�qDC� DD�DD��DD�qDE� DF�DF}qDG  DG��DG�qDH}qDI�DI� DJ  DJ��DK  DK}qDL  DL� DM�DM� DN  DN}qDN�qDOz�DO�qDP}qDQ  DQ}qDR  DR��DS  DS�DTDT� DT�qDU}qDV�DV��DW  DW� DX�DX}qDX�qDY� DZ  DZ��D[�D[��D[�qD\z�D]  D]}qD^  D^��D_�D_��D`�D`��Da  Da��Db�Db� Db�qDc}qDd  Dd��De�De}qDe�qDf��Dg  Dg}qDg�qDhz�Dh�qDi�DjDj�Dk�Dk� Dk�qDl}qDl��Dm}qDn  Dn� Do�Do� Do�qDp}qDp�qDq� Dq�qDr}qDr��Ds� Dt�Dt}qDt�qDu� Dv  Dv}qDv�qDw��Dx  Dxz�Dx��Dy}qDz  Dz� D{  D{��D|  D|��D}�D}��D~  D~� D  D� D�  D�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�HD�AHD�� D���D�  D�@ D�� D���D��qD�>�D�� D��HD�  D�>�D�� D�D�  D�@ D��HD�� D�  D�AHD�� D�� D�HD�@ D�� D��HD�  D�@ D�� D�� D�HD�@ D�� D���D���D�>�D�� D��HD�HD�@ D���D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?L��?��?�z�?�33?�
=?�@�\@
=@(�@(��@:�H@G�@Q�@aG�@s33@u@�G�@��@�{@�z�@�(�@��\@�ff@�{@�@���@�G�@���@�{@�33@�p�@�\@�ff@��@�
=@��HA ��A�AQ�A
�HA\)A�
AffAQ�A{A!G�A#33A&ffA+�A.{A0��A5A9��A;�A@  AE�AG�AJ�HAP  AR�\AU�AZ�HA]p�A_\)Adz�Ag�Aj=qAn�RAs33AvffAx��A~{A���A��A�z�A�{A�  A�G�A�33A�A�
=A���A��A�p�A�ffA���A�33A�(�A�{A���A��HA�(�A�A�Q�A�=qA��A�A�  A���A��HA�p�A��A���A��\A��A��A�G�A\A�p�A�\)Aȣ�A�33A��AθRA�  Aҏ\A���AָRA׮A�=qA�z�A�{A߮A�=qA�(�A��A�A��A�A��A�
=A�A�33A�z�A��RA�G�A��HA�(�A�ffB Q�B ��B{B\)B  B��B{B33B�
B��B
{B
�RB�B��BB=qB�B��BG�B{B33Bz�B�B�B33BQ�B��B�B
=B�
Bz�BB�HB�B Q�B!��B"�\B#33B$(�B%��B&=qB'
=B((�B)G�B)�B*�HB,(�B-�B-B.�HB0(�B0��B1��B3
=B4  B4��B5p�B6�RB7�B8Q�B9G�B:�\B;33B<  B=G�B>ffB?
=B?�
BA�BB=qBC
=BC�BD��BE�BF�RBG�
BH��BIBJffBK�BL��BMp�BN�\BO�BPz�BQ�BRffBS�BT  BU�BVffBW33BW�
BX��BZ{B[
=B[�
B\z�B]��B^�HB_�B`Q�BaG�BbffBc33Bd  Bd��Bf{Bg
=Bh  Bh��Bi��Bj�HBk�BlQ�Bm��Bn�RBo\)Bp(�Bq��BrffBs
=Bt(�BuG�Bu�Bv�RBw�
Bx��Byp�Bz=qB{\)B|z�B|��B}�B
=B�  B�=qB���B�33B�B�{B�z�B���B�\)B�B�{B�z�B�
=B���B��B�=qB��RB�33B��B��B�ffB���B�\)B���B�{B���B�
=B�G�B�B�ffB��RB�
=B���B�(�B�ffB���B�\)B�B�{B�z�B�
=B�p�B��B�  B�z�B�
=B�\)B��B�{B���B���B�33B�B�Q�B��\B��HB�G�B�B�{B�z�B���B�p�B�B�{B���B��B��B��
B�=qB���B�G�B��B��B�z�B��HB�33B���B�(�B��\B��HB�G�B��B�(�B��\B���B�33B�B�Q�B���B���B�p�B�  B�z�B��HB�33B��
B�Q�B���B�
=B���B�{B��\B��HB�\)B��B�ffB��RB��B��B�{B���B���B�\)B��
B�z�B��HB�G�B���B�(�B���B�33B��B��B��\B��B���B��B�Q�B��HB��B��
B�=qB��RB�\)B��
B�=qB���B��B��B�=qB���B�
=B�p�B�  B��\B�
=B��B�  B�Q�B��RB�G�B��
B�Q�B£�B�33B�B�=qBď\B���Bř�B�(�BƏ\B��HB�p�B�{Bȏ\B���B�p�B�  Bʣ�B�
=B�p�B�{Ḅ�B�33B�B�(�BΣ�B�
=Bϙ�B�=qB���B�G�BѮB�(�B���B�\)B��
B�=qBԸRB�\)B��B�Q�B���B�\)B�  B�z�B��HB�G�B��B�z�B���B�\)B�B�ffB�
=B݅B�  B�ffB��HB߅B�(�B��B�
=BᙚB�=qB���B�33B�B�=qB��HB�\)B�B�Q�B���B�p�B��
B�Q�B��HB�p�B�  B�ffB�RB�G�B��B�Q�B��B��B��B�Q�B�RB��BB�=qB��B�
=B�B�(�B��B�
=B�B�  B��RB�33B���B�  B��\B��B��B��B�z�B�
=B���B�  B�Q�B��HB�p�B��B�Q�B���B�\)B��B�Q�B��RB�G�B��
C 
=C Q�C ��C ��C �C�CffC�C�HC
=CG�C��C�
C
=C=qC�C��C��C(�Cp�C�RC�HC
=C=qC�CC  C(�CffC�C�C{CQ�C��C�
C
=C33CffC�RC	  C	33C	\)C	��C	�C
(�C
Q�C
�C
�
C(�C\)C�\CC
=C\)C��C�HC
=C=qC�CC{CQ�Cz�C�RC{C\)C��C��C{Cp�C�C�
C�Cp�CC  C=qCp�CC{C\)C��C�
C�Cp�C�RC��C33Cp�CC
=C\)C��C�
C�C\)C��C�CG�C�\CC  C=qC�C�HC(�Cp�C�RC��C=qCz�C�RC
=C\)C�C��C33Cp�C�RC{CffC�RC��C=qCz�CC 
=C ffC �C!
=C!Q�C!�\C!�
C"(�C"p�C"C#�C#z�C#��C${C$\)C$��C$�C%=qC%��C%��C&G�C&�\C&��C'{C'ffC'C(�C(p�C(�C)  C)=qC)��C)�C*33C*�C*�HC+33C+z�C+��C,{C,ffC,�RC-
=C-ffC-�RC.{C.p�C.��C/{C/\)C/�C0  C0ffC0�RC1�C1z�C1��C2�C2p�C2C3{C3p�C3�
C433C4��C4��C5G�C5��C5��C6Q�C6��C7  C7Q�C7�C8{C8�C8�HC9=qC9�\C9�HC:=qC:��C:�C;G�C;�C<{C<z�C<�
C=33C=�\C=�C>Q�C>��C>��C?Q�C?�C@  C@\)C@CA�CA�CA�HCB=qCB��CC  CCffCCCD(�CD�CD�HCE=qCE��CE�CFG�CF��CG  CG\)CG�CH{CHz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                            11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?��@   @@  @�  @�  @�  @޸R@��RA  A ��A,��A?\)A^�RA�Q�A�Q�A�  A��A��A�Q�A�Q�A�Q�B   B�
B  B  B   B'�
B/�B7�
B@(�BH  BP(�BXQ�B`(�Bh(�Bo�
Bx  B�  B�  B�  B��B�  B�  B�{B�{B�{B��
B��B��B�  B�{B�  B�  B�{B��B��
B�  B�{B�{B�  B�{B�  B�(�B�(�B�  B�  B�  B�  B�  C 
=C  C
=C��C�C	��C
=C��C
=C
=C
=C
=C{C{C  C  C�C!��C${C&{C({C*  C+��C.  C/��C2
=C4
=C6
=C8  C:  C<  C>  C@  CB
=CD
=CF  CH  CI��CL
=CN
=CO��CQ��CT
=CV
=CX
=CZ  C\  C]��C_��Ca�Cc��Ce��Ch
=Cj
=Cl
=Cn  Cp  Cr  Cs��Cu��Cw��Cy��C|  C~
=C��C�  C�  C���C�C�  C�C�
=C�C�  C���C�  C���C�  C�C���C���C���C���C�  C�C���C���C�C���C�C�  C���C�  C�C�  C���C�  C�\C�  C���C�
=C�C���C�  C���C�C�C���C���C�  C���C���C�  C�  C�C�
=C�C�  C���C�C�C�
=C�
=C�
=C�\C�C�  C�  C���C�C�C�  C�C�
=C�C�  C�  C�  C�  C���C�  C�  C�  C�  C���C���C���C���C�  C���C�  C�  C���C���C�  C�C�C�C�C�C�  C�C�  C���C�  C���C���C�  C���C���C�  C�  C���C�  C���C���C���C�  C�C�C�  C�C�C�  C�C�  C���C�  C�C�
=C�C���C���D � D  D� D�qD}qD�qD� D�D�D  D}qD  D� D�D��D�D��D	�D	}qD	�qD
z�D
��D}qD��DxRD�qD��D�D� D  D� D  D� D�qD��D  D}qD�D��D�D�D�D� D  D� D  D}qD  D� D  D��D  D� D�D}qD��D}qD  D}qD�qD��D�D� D �D ��D!�D!��D!�qD"}qD#  D#� D#�qD$z�D%  D%�D&�D&��D'  D'}qD'��D(}qD(�qD)� D*  D*� D+�D+� D,  D,� D-  D-}qD.  D.�D/  D/� D/�qD0}qD0�qD1}qD2  D2� D3  D3� D4�D4��D5  D5}qD6�D6��D7D7��D7�qD8}qD9�D9��D:  D:}qD:�qD;��D<�D<��D=  D=� D>�D>��D?  D?� D@  D@}qD@�qDA� DA�qDBz�DB�qDC� DD�DD��DD�qDE� DF�DF}qDG  DG��DG�qDH}qDI�DI� DJ  DJ��DK  DK}qDL  DL� DM�DM� DN  DN}qDN�qDOz�DO�qDP}qDQ  DQ}qDR  DR��DS  DS�DTDT� DT�qDU}qDV�DV��DW  DW� DX�DX}qDX�qDY� DZ  DZ��D[�D[��D[�qD\z�D]  D]}qD^  D^��D_�D_��D`�D`��Da  Da��Db�Db� Db�qDc}qDd  Dd��De�De}qDe�qDf��Dg  Dg}qDg�qDhz�Dh�qDi�DjDj�Dk�Dk� Dk�qDl}qDl��Dm}qDn  Dn� Do�Do� Do�qDp}qDp�qDq� Dq�qDr}qDr��Ds� Dt�Dt}qDt�qDu� Dv  Dv}qDv�qDw��Dx  Dxz�Dx��Dy}qDz  Dz� D{  D{��D|  D|��D}�D}��D~  D~� D  D� D�  D�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ D��HD�� D���D�>�D�� D��HD�HD�AHD�� D���D�  D�@ D�� D���D��qD�>�D�� D��HD�  D�>�D�� D�D�  D�@ D��HD�� D�  D�AHD�� D�� D�HD�@ D�� D��HD�  D�@ D�� D�� D�HD�@ D�� D���D���D�>�D�� D��HD�HD�@ D���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�G�?L��?��?�z�?�33?�
=?�@�\@
=@(�@(��@:�H@G�@Q�@aG�@s33@u@�G�@��@�{@�z�@�(�@��\@�ff@�{@�@���@�G�@���@�{@�33@�p�@�\@�ff@��@�
=@��HA ��A�AQ�A
�HA\)A�
AffAQ�A{A!G�A#33A&ffA+�A.{A0��A5A9��A;�A@  AE�AG�AJ�HAP  AR�\AU�AZ�HA]p�A_\)Adz�Ag�Aj=qAn�RAs33AvffAx��A~{A���A��A�z�A�{A�  A�G�A�33A�A�
=A���A��A�p�A�ffA���A�33A�(�A�{A���A��HA�(�A�A�Q�A�=qA��A�A�  A���A��HA�p�A��A���A��\A��A��A�G�A\A�p�A�\)Aȣ�A�33A��AθRA�  Aҏ\A���AָRA׮A�=qA�z�A�{A߮A�=qA�(�A��A�A��A�A��A�
=A�A�33A�z�A��RA�G�A��HA�(�A�ffB Q�B ��B{B\)B  B��B{B33B�
B��B
{B
�RB�B��BB=qB�B��BG�B{B33Bz�B�B�B33BQ�B��B�B
=B�
Bz�BB�HB�B Q�B!��B"�\B#33B$(�B%��B&=qB'
=B((�B)G�B)�B*�HB,(�B-�B-B.�HB0(�B0��B1��B3
=B4  B4��B5p�B6�RB7�B8Q�B9G�B:�\B;33B<  B=G�B>ffB?
=B?�
BA�BB=qBC
=BC�BD��BE�BF�RBG�
BH��BIBJffBK�BL��BMp�BN�\BO�BPz�BQ�BRffBS�BT  BU�BVffBW33BW�
BX��BZ{B[
=B[�
B\z�B]��B^�HB_�B`Q�BaG�BbffBc33Bd  Bd��Bf{Bg
=Bh  Bh��Bi��Bj�HBk�BlQ�Bm��Bn�RBo\)Bp(�Bq��BrffBs
=Bt(�BuG�Bu�Bv�RBw�
Bx��Byp�Bz=qB{\)B|z�B|��B}�B
=B�  B�=qB���B�33B�B�{B�z�B���B�\)B�B�{B�z�B�
=B���B��B�=qB��RB�33B��B��B�ffB���B�\)B���B�{B���B�
=B�G�B�B�ffB��RB�
=B���B�(�B�ffB���B�\)B�B�{B�z�B�
=B�p�B��B�  B�z�B�
=B�\)B��B�{B���B���B�33B�B�Q�B��\B��HB�G�B�B�{B�z�B���B�p�B�B�{B���B��B��B��
B�=qB���B�G�B��B��B�z�B��HB�33B���B�(�B��\B��HB�G�B��B�(�B��\B���B�33B�B�Q�B���B���B�p�B�  B�z�B��HB�33B��
B�Q�B���B�
=B���B�{B��\B��HB�\)B��B�ffB��RB��B��B�{B���B���B�\)B��
B�z�B��HB�G�B���B�(�B���B�33B��B��B��\B��B���B��B�Q�B��HB��B��
B�=qB��RB�\)B��
B�=qB���B��B��B�=qB���B�
=B�p�B�  B��\B�
=B��B�  B�Q�B��RB�G�B��
B�Q�B£�B�33B�B�=qBď\B���Bř�B�(�BƏ\B��HB�p�B�{Bȏ\B���B�p�B�  Bʣ�B�
=B�p�B�{Ḅ�B�33B�B�(�BΣ�B�
=Bϙ�B�=qB���B�G�BѮB�(�B���B�\)B��
B�=qBԸRB�\)B��B�Q�B���B�\)B�  B�z�B��HB�G�B��B�z�B���B�\)B�B�ffB�
=B݅B�  B�ffB��HB߅B�(�B��B�
=BᙚB�=qB���B�33B�B�=qB��HB�\)B�B�Q�B���B�p�B��
B�Q�B��HB�p�B�  B�ffB�RB�G�B��B�Q�B��B��B��B�Q�B�RB��BB�=qB��B�
=B�B�(�B��B�
=B�B�  B��RB�33B���B�  B��\B��B��B��B�z�B�
=B���B�  B�Q�B��HB�p�B��B�Q�B���B�\)B��B�Q�B��RB�G�B��
C 
=C Q�C ��C ��C �C�CffC�C�HC
=CG�C��C�
C
=C=qC�C��C��C(�Cp�C�RC�HC
=C=qC�CC  C(�CffC�C�C{CQ�C��C�
C
=C33CffC�RC	  C	33C	\)C	��C	�C
(�C
Q�C
�C
�
C(�C\)C�\CC
=C\)C��C�HC
=C=qC�CC{CQ�Cz�C�RC{C\)C��C��C{Cp�C�C�
C�Cp�CC  C=qCp�CC{C\)C��C�
C�Cp�C�RC��C33Cp�CC
=C\)C��C�
C�C\)C��C�CG�C�\CC  C=qC�C�HC(�Cp�C�RC��C=qCz�C�RC
=C\)C�C��C33Cp�C�RC{CffC�RC��C=qCz�CC 
=C ffC �C!
=C!Q�C!�\C!�
C"(�C"p�C"C#�C#z�C#��C${C$\)C$��C$�C%=qC%��C%��C&G�C&�\C&��C'{C'ffC'C(�C(p�C(�C)  C)=qC)��C)�C*33C*�C*�HC+33C+z�C+��C,{C,ffC,�RC-
=C-ffC-�RC.{C.p�C.��C/{C/\)C/�C0  C0ffC0�RC1�C1z�C1��C2�C2p�C2C3{C3p�C3�
C433C4��C4��C5G�C5��C5��C6Q�C6��C7  C7Q�C7�C8{C8�C8�HC9=qC9�\C9�HC:=qC:��C:�C;G�C;�C<{C<z�C<�
C=33C=�\C=�C>Q�C>��C>��C?Q�C?�C@  C@\)C@CA�CA�CA�HCB=qCB��CC  CCffCCCD(�CD�CD�HCE=qCE��CE�CFG�CF��CG  CG\)CG�CH{CHz�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                            11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�33A�/A�(�A�33A�(�A� �A�{A��A��HAҲ-Aң�A҇+A�Q�A� �A���A��#A���A���AѼjAѺ^AѸRAѶFAѰ!AѬAѬAѩ�Aѧ�Aѥ�Aѡ�Aѝ�Aѕ�Aя\AуA�C�AмjA�|�A�bNA�E�A�ȴAϑhA�^5A�(�A�1A��A��/A�1'A�VA�XA�A�A��yA�l�A��A�ƨAʰ!A�I�Aə�A�JA���A�A�A�|�A�1A�ZA�(�A�ZA�hsA�S�A���A�|�A��hA���A���A��-A��\A�JA��TA���A��RA�"�A�
=A��A��-A�C�A�z�A�?}A���A��A�"�A�5?A���A��A��TA�%A�33A�+A���A��9A�{A��yA��A��mA��9A�r�A�n�A�l�A�~�A���A�1A��A{O�Aw�As�TAq��Ao��AnAlA�AkG�AjJAhbNAe�TAeG�Ad�Ab�A`�jA\�AW��AS��AR�uAQANv�AK�#AJQ�AHjAF�AEO�AC��AB  A@r�A<=qA7A6��A4�A1��A.-A,VA)�;A);dA(��A'|�A&{A%?}A#��A"�\A!hsA �jAt�AQ�A��A��A\)A�TAA\)A�A�A�Al�A1'A�AdZAI�Al�A�+A��AG�AĜAA
5?A	�AbA�A�^A�A~�A�FA|�A ��@��@���@��T@��7@�b@��\@�dZ@���@���@�X@���A�A�AVA�A1'A�A�-A��A��A�hAx�Ax�At�AO�A+A%A�A�\A�A��Al�A �/@��;@�33@��@�V@�hs@���@��@��@�  @�ȴ@��T@���@��T@�J@�-@�@��/@��y@�{@��^@�9@�33@��H@��@���@���@陚@�G�@�%@�@��;@���@���@��
@�l�@�+@�
=@�~�@��@��@�p�@�@���@ݩ�@�V@���@��;@ڸR@�E�@�X@�A�@ץ�@�C�@���@��@�@��@�j@�A�@���@ӕ�@�
=@ҟ�@�^5@��T@�&�@�Ĝ@�1'@��@ϕ�@�+@�-@�@ͺ^@���@��T@��T@���@͡�@�p�@�O�@̬@˾w@�\)@�@�n�@��@�K�@��H@Ɨ�@��#@őh@��@ģ�@�j@�9X@���@ÍP@�;d@��@¸R@§�@\@��#@�?}@��D@�C�@��y@�ȴ@�$�@��h@�?}@�V@��`@���@���@��@��w@��@���@�5?@��^@�`B@�%@��9@���@��F@���@�S�@�
=@�5?@���@�`B@�O�@��@��j@���@��@�A�@�1@�dZ@��R@�M�@�5?@�5?@���@��h@��7@�hs@��@�r�@�(�@�o@�ff@�@���@�hs@���@��@�z�@���@��@�"�@��H@�{@�@��h@�hs@�7L@�V@�z�@��@���@�ƨ@���@�|�@�S�@�@�n�@�5?@�=q@��@��-@�`B@��@�Ĝ@�1'@��F@���@�l�@�;d@�@��y@��H@���@��+@�v�@�v�@�v�@�$�@��-@��7@��@�x�@�x�@�`B@�G�@�?}@��@��/@��j@��@���@� �@��@�S�@��@�^5@�5?@�J@��T@��^@�p�@��@��j@�r�@�j@�9X@�  @�ƨ@�\)@�33@���@��@���@��@���@���@�O�@���@��u@�I�@��@���@�|�@�t�@�dZ@�+@��@���@�ff@�E�@�$�@�J@��@���@�x�@�`B@�O�@�/@�V@���@��@�Q�@��@��!@��\@��+@�v�@��T@�p�@�G�@��@��j@��@�dZ@�
=@��!@��\@�n�@�V@�E�@�5?@���@��-@���@�hs@�%@���@��9@��u@�j@�1'@�1@��
@�t�@�^5@��@���@�%@��/@�z�@�A�@�1'@� �@�b@��
@�
=@���@�5?@�p�@��@�ƨ@���@�l�@�C�@�"�@��H@���@���@��T@���@�?}@���@���@���@�z�@�Z@�I�@�A�@�1'@�  @~��@~5?@}��@|�j@|�@{��@{33@z~�@z�@y�^@y��@y��@yG�@x��@x�@xbN@xA�@w��@w\)@w�@v�@v�+@v@u��@t��@t�DG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�A�A�-A�+A�33A�33A�1'A�/A�/A�-A�&�A�$�A�+A�(�A�1'A�7LA�5?A�1'A�7LA�-A�$�A�-A�-A�+A�(�A�-A�(�A�(�A�VA�VA�oA��A��A�JA�A���A��yA��A��A���A���A�A��#AҸRAҶFAҴ9AҶFAҲ-Aҡ�Aҧ�Aҟ�Aҝ�Aқ�Aҗ�AғuA҅A�|�A�t�A�jA�^5A�VA�VA�ZA�\)A�VA�ZA�O�A�5?A�5?A�33A�/A�(�A�(�A�+A�$�A�bA�1A�%A�A�A�%A���A�  A���A��A��mA��yA��yA��HA���A���A��
A���A���A���A���A���A���A���A���A�ĜA�ƨA�ƨA�AѾwA���A���AѾwAѺ^AѾwA���AѺ^AѼjAѾwAѺ^AѸRAѼjAѾwAѼjAѸRAѼjAѾwAѺ^AѸRAѼjAѼjAѶFAѺ^AѼjAѸRAѶFAѸRAѼjAѼjAѴ9AѶFAѸRAѶFAѲ-AѴ9AѸRAѲ-AѲ-AѶFAѲ-AѰ!AѲ-AѲ-AѮAѮAѰ!AѮAѩ�AѮAѬAѩ�AѮAѮAѩ�Aѩ�Aѧ�AѮAѩ�Aѧ�AѮAѮAѧ�AѬAѮAѬAѧ�AѬAѮAѩ�Aѥ�Aѩ�Aѩ�Aѥ�Aѥ�Aѩ�Aѥ�Aѥ�Aѧ�Aѩ�Aѥ�Aѧ�Aѧ�Aѧ�Aѣ�Aѥ�Aѧ�Aѣ�Aѡ�Aѥ�Aѥ�Aѣ�Aџ�Aѣ�Aѥ�Aџ�Aџ�Aѣ�Aѝ�Aѝ�Aѡ�Aѡ�Aџ�Aљ�Aћ�Aѝ�Aљ�Aѕ�Aї�Aї�AѓuAѕ�Aї�AѓuAёhAѓuAѓuAэPAя\AѓuAэPAыDAэPAя\Aщ7Aч+AэPAщ7A�~�A�|�A�~�A�~�A�z�A�l�A�dZA�ZA�K�A�=qA�5?A�$�A��A��A�VA��TA�ƨAЮAС�AЗ�AЕ�AБhAЉ7AЇ+AЁA�x�A�t�A�z�A�x�A�p�A�n�A�l�A�hsA�`BA�^5A�`BA�\)A�\)A�`BA�`BA�^5A�XA�Q�A�=qA�5?A�33A�7LA�7LA�33A��A��AϺ^AϮAϩ�Aϥ�Aϥ�Aϥ�Aϡ�Aϙ�Aϗ�Aϗ�Aϕ�AϏ\AϏ\AϑhAϋDA�|�A�x�A�x�A�p�A�hsA�ffA�dZA�S�A�;dA�33A�5?A�/A�+A�-A�+A�&�A�&�A�&�A� �A��A��A��A��A�{A�JA�
=A�A��A���A���A���A���A��A��A�
=A�
=A���A��A��#A���A�O�A�(�A�%A���A��A���A�ȴAͩ�A͏\ÁA�O�A�I�A�7LA�5?A�+A�$�A��A��A��A�&�A�$�A� �A��A��A��A�
=A���A��
A�ƨA̸RÁA�\)A�Q�A�=qA�"�A��A��A��A��A�/A�G�A�Q�A�S�A�O�A�M�A�G�A�?}A��A�bA�%A���A��mA�ĜAˬA˛�A˓uAˋDA�~�A�x�A�r�A�ffA�XA�K�A�5?A�JA�VA�VA�
=A��A�ȴA���Aʥ�A�|�AʃAʁAʙ�A��A�%A�A�%A�%A���Aʺ^Aʲ-Aʟ�Aʙ�Aʗ�Aʗ�AʍPAʁA�dZA�A�A�A�A�A�A�;dA�33A�33A�33A���A��`A�AɑhA�^5A�/A��A�{A�{A��A��A��A�{A���A��A��mA��HA��HA��
A���A���AȶFAȧ�Aț�Aȇ+A�jA�O�A�$�A�VA�
=A���A���AǼjAǟ�AǋDA�z�A�ZA�(�A��Aƺ^AƅA�VA� �A���A���A�G�A��A���Aĩ�AąA�hsA�O�A��AöFAÉ7A�hsA�Q�A�1'A�$�A�
=A��A��HA���A°!AA�XA�G�A�/A��A��mA�ȴA���A�p�A�VA�G�A�?}A�9XA�;dA�9XA��A��`A��hA�1'A�bA��-A�dZA�C�A�33A��A�{A���A��9A�E�A�&�A��A�VA�VA�A���A��mA�$�A�?}A��DA�dZA�XA�1'A��wA�;dA���A��A���A�~�A�bNA�G�A�JA�ĜA��hA�VA��#A�|�A�`BA�VA�C�A�"�A��TA�\)A���A��A�9XA��/A���A�A���A���A��hA��7A�z�A�ffA�S�A�=qA��A��A��!A���A�ffA��^A�+A�ĜA�7LA��A��yA��A�O�A��A�VA�ĜA�t�A�=qA���A��jA��A��A���A�I�A���A��A��jA��hA��A�n�A�oA�Q�A���A��A�x�A�$�A��FA�;dA�O�A��`A���A�t�A�Q�A��A��/A��A�x�A�VA�M�A�E�A�$�A�A��
A���A�O�A��A���A���A�VA�{A��A���A�l�A��mA��A���A��A�t�A�?}A��yA��A�v�A�M�A��A���A��mA��A���A�A���A��+A�S�A�"�A��A��!A��A�`BA�7LA���A��;A�ȴA��9A���A��+A���A�VA�+A�A��HA��9A���A��A�bNA�
=A��;A���A�C�A�(�A�{A��#A�~�A�XA�S�A�S�A�O�A�A�A���A��
A��A���A��uA��\A��7A��A��A�~�A�p�A�
=A�jA�
=A��A��yA��mA��`A��HA��;A���A��!A�+A�%A��yA�ĜA���A�t�A�\)A�E�A�1'A��A���A��A��9A�t�A�-A��yA�n�A���A�hsA�/A��FA�5?A��A���A��mA���A��-A��A�G�A�9XA�1'A�(�A��A���A���A�7LA���A���A��-A���A���A��\A��7A��A�t�A�hsA�E�A�VA��A�ĜA�VA��A��#A���A���A�t�A�{A��wA���A�t�A�M�A�(�A��HA��!A��+A�Q�A�+A�A�A���A�hsA��A��yA���A���A��7A�l�A�;dA��A���A���A���A��+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                            11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�33A�/A�(�A�33A�(�A� �A�{A��A��HAҲ-Aң�A҇+A�Q�A� �A���A��#A���A���AѼjAѺ^AѸRAѶFAѰ!AѬAѬAѩ�Aѧ�Aѥ�Aѡ�Aѝ�Aѕ�Aя\AуA�C�AмjA�|�A�bNA�E�A�ȴAϑhA�^5A�(�A�1A��A��/A�1'A�VA�XA�A�A��yA�l�A��A�ƨAʰ!A�I�Aə�A�JA���A�A�A�|�A�1A�ZA�(�A�ZA�hsA�S�A���A�|�A��hA���A���A��-A��\A�JA��TA���A��RA�"�A�
=A��A��-A�C�A�z�A�?}A���A��A�"�A�5?A���A��A��TA�%A�33A�+A���A��9A�{A��yA��A��mA��9A�r�A�n�A�l�A�~�A���A�1A��A{O�Aw�As�TAq��Ao��AnAlA�AkG�AjJAhbNAe�TAeG�Ad�Ab�A`�jA\�AW��AS��AR�uAQANv�AK�#AJQ�AHjAF�AEO�AC��AB  A@r�A<=qA7A6��A4�A1��A.-A,VA)�;A);dA(��A'|�A&{A%?}A#��A"�\A!hsA �jAt�AQ�A��A��A\)A�TAA\)A�A�A�Al�A1'A�AdZAI�Al�A�+A��AG�AĜAA
5?A	�AbA�A�^A�A~�A�FA|�A ��@��@���@��T@��7@�b@��\@�dZ@���@���@�X@���A�A�AVA�A1'A�A�-A��A��A�hAx�Ax�At�AO�A+A%A�A�\A�A��Al�A �/@��;@�33@��@�V@�hs@���@��@��@�  @�ȴ@��T@���@��T@�J@�-@�@��/@��y@�{@��^@�9@�33@��H@��@���@���@陚@�G�@�%@�@��;@���@���@��
@�l�@�+@�
=@�~�@��@��@�p�@�@���@ݩ�@�V@���@��;@ڸR@�E�@�X@�A�@ץ�@�C�@���@��@�@��@�j@�A�@���@ӕ�@�
=@ҟ�@�^5@��T@�&�@�Ĝ@�1'@��@ϕ�@�+@�-@�@ͺ^@���@��T@��T@���@͡�@�p�@�O�@̬@˾w@�\)@�@�n�@��@�K�@��H@Ɨ�@��#@őh@��@ģ�@�j@�9X@���@ÍP@�;d@��@¸R@§�@\@��#@�?}@��D@�C�@��y@�ȴ@�$�@��h@�?}@�V@��`@���@���@��@��w@��@���@�5?@��^@�`B@�%@��9@���@��F@���@�S�@�
=@�5?@���@�`B@�O�@��@��j@���@��@�A�@�1@�dZ@��R@�M�@�5?@�5?@���@��h@��7@�hs@��@�r�@�(�@�o@�ff@�@���@�hs@���@��@�z�@���@��@�"�@��H@�{@�@��h@�hs@�7L@�V@�z�@��@���@�ƨ@���@�|�@�S�@�@�n�@�5?@�=q@��@��-@�`B@��@�Ĝ@�1'@��F@���@�l�@�;d@�@��y@��H@���@��+@�v�@�v�@�v�@�$�@��-@��7@��@�x�@�x�@�`B@�G�@�?}@��@��/@��j@��@���@� �@��@�S�@��@�^5@�5?@�J@��T@��^@�p�@��@��j@�r�@�j@�9X@�  @�ƨ@�\)@�33@���@��@���@��@���@���@�O�@���@��u@�I�@��@���@�|�@�t�@�dZ@�+@��@���@�ff@�E�@�$�@�J@��@���@�x�@�`B@�O�@�/@�V@���@��@�Q�@��@��!@��\@��+@�v�@��T@�p�@�G�@��@��j@��@�dZ@�
=@��!@��\@�n�@�V@�E�@�5?@���@��-@���@�hs@�%@���@��9@��u@�j@�1'@�1@��
@�t�@�^5@��@���@�%@��/@�z�@�A�@�1'@� �@�b@��
@�
=@���@�5?@�p�@��@�ƨ@���@�l�@�C�@�"�@��H@���@���@��T@���@�?}@���@���@���@�z�@�Z@�I�@�A�@�1'@�  @~��@~5?@}��@|�j@|�@{��@{33@z~�@z�@y�^@y��@y��@yG�@x��@x�@xbN@xA�@w��@w\)@w�@v�@v�+@v@u��@t��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�A�A�-A�+A�33A�33A�1'A�/A�/A�-A�&�A�$�A�+A�(�A�1'A�7LA�5?A�1'A�7LA�-A�$�A�-A�-A�+A�(�A�-A�(�A�(�A�VA�VA�oA��A��A�JA�A���A��yA��A��A���A���A�A��#AҸRAҶFAҴ9AҶFAҲ-Aҡ�Aҧ�Aҟ�Aҝ�Aқ�Aҗ�AғuA҅A�|�A�t�A�jA�^5A�VA�VA�ZA�\)A�VA�ZA�O�A�5?A�5?A�33A�/A�(�A�(�A�+A�$�A�bA�1A�%A�A�A�%A���A�  A���A��A��mA��yA��yA��HA���A���A��
A���A���A���A���A���A���A���A���A�ĜA�ƨA�ƨA�AѾwA���A���AѾwAѺ^AѾwA���AѺ^AѼjAѾwAѺ^AѸRAѼjAѾwAѼjAѸRAѼjAѾwAѺ^AѸRAѼjAѼjAѶFAѺ^AѼjAѸRAѶFAѸRAѼjAѼjAѴ9AѶFAѸRAѶFAѲ-AѴ9AѸRAѲ-AѲ-AѶFAѲ-AѰ!AѲ-AѲ-AѮAѮAѰ!AѮAѩ�AѮAѬAѩ�AѮAѮAѩ�Aѩ�Aѧ�AѮAѩ�Aѧ�AѮAѮAѧ�AѬAѮAѬAѧ�AѬAѮAѩ�Aѥ�Aѩ�Aѩ�Aѥ�Aѥ�Aѩ�Aѥ�Aѥ�Aѧ�Aѩ�Aѥ�Aѧ�Aѧ�Aѧ�Aѣ�Aѥ�Aѧ�Aѣ�Aѡ�Aѥ�Aѥ�Aѣ�Aџ�Aѣ�Aѥ�Aџ�Aџ�Aѣ�Aѝ�Aѝ�Aѡ�Aѡ�Aџ�Aљ�Aћ�Aѝ�Aљ�Aѕ�Aї�Aї�AѓuAѕ�Aї�AѓuAёhAѓuAѓuAэPAя\AѓuAэPAыDAэPAя\Aщ7Aч+AэPAщ7A�~�A�|�A�~�A�~�A�z�A�l�A�dZA�ZA�K�A�=qA�5?A�$�A��A��A�VA��TA�ƨAЮAС�AЗ�AЕ�AБhAЉ7AЇ+AЁA�x�A�t�A�z�A�x�A�p�A�n�A�l�A�hsA�`BA�^5A�`BA�\)A�\)A�`BA�`BA�^5A�XA�Q�A�=qA�5?A�33A�7LA�7LA�33A��A��AϺ^AϮAϩ�Aϥ�Aϥ�Aϥ�Aϡ�Aϙ�Aϗ�Aϗ�Aϕ�AϏ\AϏ\AϑhAϋDA�|�A�x�A�x�A�p�A�hsA�ffA�dZA�S�A�;dA�33A�5?A�/A�+A�-A�+A�&�A�&�A�&�A� �A��A��A��A��A�{A�JA�
=A�A��A���A���A���A���A��A��A�
=A�
=A���A��A��#A���A�O�A�(�A�%A���A��A���A�ȴAͩ�A͏\ÁA�O�A�I�A�7LA�5?A�+A�$�A��A��A��A�&�A�$�A� �A��A��A��A�
=A���A��
A�ƨA̸RÁA�\)A�Q�A�=qA�"�A��A��A��A��A�/A�G�A�Q�A�S�A�O�A�M�A�G�A�?}A��A�bA�%A���A��mA�ĜAˬA˛�A˓uAˋDA�~�A�x�A�r�A�ffA�XA�K�A�5?A�JA�VA�VA�
=A��A�ȴA���Aʥ�A�|�AʃAʁAʙ�A��A�%A�A�%A�%A���Aʺ^Aʲ-Aʟ�Aʙ�Aʗ�Aʗ�AʍPAʁA�dZA�A�A�A�A�A�A�;dA�33A�33A�33A���A��`A�AɑhA�^5A�/A��A�{A�{A��A��A��A�{A���A��A��mA��HA��HA��
A���A���AȶFAȧ�Aț�Aȇ+A�jA�O�A�$�A�VA�
=A���A���AǼjAǟ�AǋDA�z�A�ZA�(�A��Aƺ^AƅA�VA� �A���A���A�G�A��A���Aĩ�AąA�hsA�O�A��AöFAÉ7A�hsA�Q�A�1'A�$�A�
=A��A��HA���A°!AA�XA�G�A�/A��A��mA�ȴA���A�p�A�VA�G�A�?}A�9XA�;dA�9XA��A��`A��hA�1'A�bA��-A�dZA�C�A�33A��A�{A���A��9A�E�A�&�A��A�VA�VA�A���A��mA�$�A�?}A��DA�dZA�XA�1'A��wA�;dA���A��A���A�~�A�bNA�G�A�JA�ĜA��hA�VA��#A�|�A�`BA�VA�C�A�"�A��TA�\)A���A��A�9XA��/A���A�A���A���A��hA��7A�z�A�ffA�S�A�=qA��A��A��!A���A�ffA��^A�+A�ĜA�7LA��A��yA��A�O�A��A�VA�ĜA�t�A�=qA���A��jA��A��A���A�I�A���A��A��jA��hA��A�n�A�oA�Q�A���A��A�x�A�$�A��FA�;dA�O�A��`A���A�t�A�Q�A��A��/A��A�x�A�VA�M�A�E�A�$�A�A��
A���A�O�A��A���A���A�VA�{A��A���A�l�A��mA��A���A��A�t�A�?}A��yA��A�v�A�M�A��A���A��mA��A���A�A���A��+A�S�A�"�A��A��!A��A�`BA�7LA���A��;A�ȴA��9A���A��+A���A�VA�+A�A��HA��9A���A��A�bNA�
=A��;A���A�C�A�(�A�{A��#A�~�A�XA�S�A�S�A�O�A�A�A���A��
A��A���A��uA��\A��7A��A��A�~�A�p�A�
=A�jA�
=A��A��yA��mA��`A��HA��;A���A��!A�+A�%A��yA�ĜA���A�t�A�\)A�E�A�1'A��A���A��A��9A�t�A�-A��yA�n�A���A�hsA�/A��FA�5?A��A���A��mA���A��-A��A�G�A�9XA�1'A�(�A��A���A���A�7LA���A���A��-A���A���A��\A��7A��A�t�A�hsA�E�A�VA��A�ĜA�VA��A��#A���A���A�t�A�{A��wA���A�t�A�M�A�(�A��HA��!A��+A�Q�A�+A�A�A���A�hsA��A��yA���A���A��7A�l�A�;dA��A���A���A���A��+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                            11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
B
�B
MB
MB
MB
B
�B
MB
B
GB
�B
GB
�B
�B
�B
uB
AB
AB
uB
AB
AB
B
AB
B
�B
B
�B
�B
oB
;B
 �B
  B	�.B	��B	�PB	��B	��B	��B
B
 'B
F�B
��B
خBFBz�BjBiBz�B��B�hB��B��B��B�dB�pB�&B�B�iB�B�B�PBDBhB�BqB%B(�B/�B9�B:�BB[B@�B@�B<jB=B=qB0�B
=B�B�B�HB� B�B�B�hB��B��B��B�B�PB}�B{BkBe�BK)B3hB'RB�B	lBfB
�5B
�EB
��B
�B
��B
�~B
�@B
� B
R�B
K)B
+B
"�B
{B
�B
  B	�	B	�MB	��B	�B	��B	��B	ȴB	��B	�*B	�$B	tB	h
B	b�B	YB	K)B	B�B	9�B	1'B	,�B	)_B	OB	�B	B�B��B�AB�yB�B՛B��B��BȀB�0B��B�TB�vB�)B�yBԕB��B��B�ZB��B��B��B�B�"B��B��B�B��B��B�lB	B	 4B	uB	�B	�B	_B	YB	B	 B	fB��B��B�B�B�BB�5B�B�jBܒB��B��BҽB��B�B	B	FB	*eB	2�B	9XB	jKB	��B	��B	��B	�qB	��B	��B	��B	�^B	�^B	��B	�<B	�}B	��B	��B	��B	B	��B	�-B	��B	��B	�3B	��B	�3B	�}B	��B	��B	��B	�^B	��B	��B	�B	��B	�6B	�BB	��B	�[B	�[B	ÖB	�B	��B	�-B	��B	�HB	��B	��B	��B	��B	��B	�gB	ĜB	�?B	�B	�XB	̘B	��B	�pB	��B	��B	��B	�B	�B	�B	��B	�aB	՛B	՛B	�2B	רB	�B	�EB	��B	֡B	��B	רB	ٴB	�WB	�)B	ݘB	�B	�;B	�B	��B	�B	�B	�B	��B	��B	��B	�TB	��B	�TB	�ZB	�&B	��B	�B	�8B	�>B	�sB	�sB	�yB	�DB	�DB	��B	��B	�KB	��B	�DB	�B	�B	��B	�)B	�]B	��B	� B	�;B	�oB	�oB	�AB	��B	�|B	��B	�B	��B	�|B	�TB	��B	�`B	��B	��B	��B	��B	��B	��B	�(B	��B	�]B	��B	�(B
 4B
 �B
 �B
;B
AB
�B
�B
SB
�B
�B
+B
_B
�B

	B

	B
	�B
	lB
	�B
	�B
	7B
	lB

	B

�B
B
�B
JB
B
�B
(B
(B
�B
�B
�B
�B
�B
�B
4B
�B
�B
�B
�B
@B
�B
B
FB
{B
�B
�B
$B
YB
�B
�B
�B
1B
�B
�B
7B
7B
�B
B
�B
=B
qB
qB
B
�B
IB
~B
OB
�B
 'B
�B
 \B
 'B
 �B
 �B
!�B
"�B
#nB
#nB
#nB
#:B
$@B
%zB
%B
$�B
$�B
$tB
$@B
$@B
#�B
$@B
$@B
%B
%FB
%zB
&LB
&�B
'B
'�B
(�B
(�B
)*B
)�B
)�B
*�B
+B
,=B
+�B
+�B
,=B
,B
,B
,�B
,qB
-B
,qB
-B
.}B
.IB
/�B
/�B
0UB
0UB
0�B
1'B
1�B
1�B
1�B
1�B
2aB
2�B
2�B
3hB
3�B
3�B
3�B
49B
4�B
4�B
5B
5B
5B
5B
5�B
5�B
5�B
8RB
9$B
9$B
8�B
8�B
:�B
:�B
:�B
:�B
:�B
<�B
<6B
=<B
=<B
=qB
=qB
>B
=�B
>B
>BB
>wB
>B
?B
>�B
?B
?B
?B
?HB
?HB
?B
>�B
>�B
B�B
A B
B'B
B�B
CaB
C�B
CaB
C-B
B�B
B�B
CaB
D3B
D3B
C�B
C�B
E�B
GB
F�B
F�B
G�B
I�B
J�B
J�B
JXB
L0B
LdB
N<B
M�B
N<B
NpB
N�B
N�B
N�B
N�B
N�B
N�B
OvB
OvB
OvB
O�B
PHB
PHB
P�B
QNB
QNB
Q�B
Q�B
Q�B
R B
R�B
R�B
S&B
S&B
S�B
S�B
TaB
TaB
T�B
UgB
U�B
V�B
W?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
MB
1B
YB
{B
�B
B
B	�"B
B
�B
{B
B
{B
�B
�B
�B
YB
�B
�B
�B
AB
B
SB
�B
{B
�B
�B
�B
�B
�B
oB
�B
YB

�B	�.B
�B
GB
�B
�B
�B	��B
{B
B
GB
uB
oB
�B
B
�B
GB
B
B
�B	��B
�B
�B
GB
�B
B
{B
�B
B
B
{B	��B
�B
YB
�B
�B
B
B
oB
 �B
MB
�B
B
AB
B
uB
;B
�B
�B
�B
AB
�B
oB
�B
�B
�B
oB
B
B
GB
oB
;B
B
�B
�B
AB
GB
B
oB
B
GB
�B
B
B
{B
�B
oB
GB
�B
oB
�B
GB
AB
;B
�B
{B
uB
oB
B
B
oB
;B
{B
�B
B
uB
GB
�B
B
B
�B
AB
;B
�B
�B
�B
 �B
�B
B
B
B
�B
�B
;B
�B
uB
B
�B
GB
;B
�B
B
�B
B
�B
�B
GB
B
uB
�B
 �B
B
�B
oB
 �B
B
�B
B
B
�B
�B
;B
oB
uB
uB
 �B
uB
uB
�B
 �B
�B
�B
oB
B
B
�B
 �B
oB
uB
;B
 iB
�B
�B
 �B
 4B
AB
�B
 4B
�B
AB
 �B
 4B
 �B
GB
B	��B
oB
B
 4B
 4B
;B
 �B	��B
 �B
B
 4B	�.B
 �B
  B	�.B
 �B
B	�.B	��B
 �B
 �B	�]B	��B
  B	��B	��B	��B	��B
oB	��B	�"B	�.B	��B	��B	��B	�JB	�PB	�(B
 iB	��B	�VB	��B	�B	��B	�B	�JB	�B	��B	��B	�JB	��B	�B	��B	�JB	��B	�B	��B	��B	�xB	��B	��B	�B	�JB	�PB	��B	��B	�.B
 4B
B	��B	�.B
 4B
�B
�B
_B
SB
�B
+B
�B
YB
eB
B
�B
B
�B
�B
!-B
 \B
#:B
(�B
+�B
+B
-�B
1[B
2aB
7�B
F?B
l�B
�B
�@B
��B
��B
��B
��B
��B
�4B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B�B�BeBB�B!-B1�BE�B_�BjBp;B|B�hB~(B{�Bw�BwfB��BxBuZBsBv`Bo�Bi�Bn�BiDBiyBf�Bh
BiBg8Bd�Bg�Bh>Bh�BhsBgBi�BlWBp;Br�BtTB��By�By�B|PB|PB|B{JB�iB��B�YB��B��B�:B��B��B��B��B��B�nB��B��B��B�4B�'B��B�B��B��B�YB�+B��B��B��B��B�bB��B�xB��B�B��B�xB�FB�VB��B�B��B��B��B�)B��B�6B��B�QB�B�]B�B�|B�|B�`B�B��B�HB�)B��B��B�)B�]BںB�;B��B� B�B��B�8B�B�B�TB�B��B�KB��B�vB� B��B� B��B�AB�cB�cB�iB�5B�B�;B�B�B��B�;B��B�;B�B�B�)B�B�B��B��B�B�B��B��B��B�lB�.B�B�B�BfB�B+BYBhB�B�B�B.BB B�BoB4B�B�B�B�B�B�B+BqB�B~B"4B�BkB	BB�B�BqB 'B+6B#�B"hB1�B+�B%�B#nB%FB"�B(�B1'B-wB)�B(XB&LB#:B#nB$tB#:BU�BH�B<B5?B0�B3�BFtBB'BA B8RB5tB6�B6�B5?B:�B9�B8�B=qBFtB9�B8�B5�B5B7B9�BQ�BQ�BV9BVmBE�B>�B=<BA B>wB=B;dB;�B=<B;dB;�B8RB;�B=qBQNB@�BEmBF�BDgBG�B0�B5?B:*BGzB1�B0�B@�B?�B:�B?B<�B;dBH�B=qBFB>BBT,B6zB8B0UB/�BA�BIB3�B*eB%�B"�B*�BbB%FB�B:B\B�B
	BBB�B�VB�rB��B��B�>B�"B��B��B�B�+B�cB�8B�/B�B�
B�|B�B�;B�yB�KB�[B�B�vB��B֡B҉B��B��B�#BȴBƨBƨBƨB��BɺB�?B��B��B�B�B��B��B��B��B�B��B��B�XB��B��B��B��B�B�bB�B��B�4B��B�bB��B��B��B�=B��B��B��B��B�"B�\B�~B�.B��B�DB��B�1B�fB�+B�YB��B��B�oB��B��B�iB��B}�B}�B|PB{B�B{�B� By�Br�By>Bl"Bs�Bm�Bi�Bm�BbBg8Bh�Bb�Bl"B]�B^Bj�B\�BR�BEmBW�BP�B9$B5?B5�B1�B0UB5�B/�B&�B&B$B$@B'�B)�B/�B�B�B�B~BB	lB	�B	lB�B�BB�B
��B
�.B)�BMB
��B
�B
�&B
�&B
�B
�B
�mB
՛B
�[B
��B
ѷB
ɆB
��B
ɆB
�B
�OB
�B
�^B
�^B
��B
�B
��B
�B
�6B
��B
��B
��B
��B
��B
��B
�e4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                            44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                            44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022041603091320220416030913IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022041709010920220417090109QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022041709010920220417090109QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194420230210131944IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                