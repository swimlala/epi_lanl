CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-04-04T06:19:17Z creation; 2023-07-20T16:49:53Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  T�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  Z�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  rx   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  xh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ř   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ˈ   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �H   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` 6h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   6�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   <�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   B�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T H�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   I   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   I$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   I,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   I4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � I<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   I�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   I�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    I�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        J    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        J   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       J   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    JArgo profile    3.1 1.2 19500101000000  20190404061917  20230720164953  5905789 5905789 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7823_008763_003                 7823_008763_003                 2C  2C  DD  SOLO_II                         SOLO_II                         8763                            8763                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @سΉ"S@سΉ"S11  @سζ�}V@سζ�}V@6��k�@6��k��d��4� �d��4� 11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q?��H@@  @�G�@�  @��R@�G�A ��A  A   A,(�A@  A`  A�Q�A�  A�Q�A�  A��A�Q�A�  A���A��BQ�B(�B�
B   B(  B/�
B8  B@(�BH(�BO�BW�
B`(�Bh  BpQ�Bx  B�
B�  B��B�  B�  B�{B�  B��
B��
B�{B��B�(�B�  B�(�B��B�=qB��B�(�B�B�=qB��
B�{B׮B�  B��B�{B��
B��B�(�B�B��
B�(�B��C�C
=C
=C�C	�HC  C�C(�C�HC  C(�C�C  C{C�HC �C!�C#��C%�C(�C)��C+��C-��C/��C2  C4{C6(�C8  C9�
C;�HC>  C@{CB{CD�CF�CH33CJ33CL{CN
=CP(�CR(�CT{CV
=CX(�CZ(�C\(�C^�C`�Cb
=Cd
=Cf  Cg�Ci��Cl  Cn�Cp  Cq�Ct  Cv{Cx  Cy�HC|
=C~(�C�C�  C��C�C�C�
=C�{C�C�  C�  C�C�{C���C��fC��C���C�C�
=C�\C�C��C��C��C���C��C��C�C�
=C�\C�
=C�
=C�\C�\C�\C�{C�C��C��C���C�C�  C�C�
=C�
=C�  C��C��C�  C��C�
=C�C�  C�C��C�  C���C��C���C���C�
=C�\C�\C�\C��C��C���C�  C���C���C�\C�C��C��HC��fC��C���C���C���C�  C�C���C��C���C�  C�  C��C��C��C�  C�
=C�
=C�C�  C���C���C�  C�C�
=C�C��C�  C�{C�
=C���C���C�C�{C�\C�\C�\C�{C�
=C�C�  C���C���C��C�C�
=C���C��C���C�  C���C�C�
=C�{C�C���D � D�D��D�D��D�D}qD�RD}qD�D�=D  D}qD�D� DD�=D	  D	xRD
  D
��D
�qDu�D��D� D�D�=D�Ds3D�RD��D�D� D��D� D�D�D
=D��D  D�D�D� D��D� DD��DD� D�RD}qD�D��D�D� D�D��D�D� D�D��D  D}qD��D xRD!  D!��D!��D"xRD#  D#�=D$D$��D%
=D%� D%�RD&}qD'  D'}qD(  D(��D)�D)z�D)��D*� D+�D+z�D+�RD,� D-�D-� D-�qD.� D/�D/��D/�qD0u�D0��D1}qD1�RD2}qD3�D3}qD3��D4}qD5  D5� D6�D6� D6�RD7z�D7�qD8��D9D9��D:  D:��D;  D;� D;��D<z�D=  D=xRD=�RD>� D?  D?�D@�D@�DA�DA��DB  DB� DB�qDCxRDD  DD��DEDE��DF�DF� DG  DG��DH  DHz�DI�DI��DJ�DJ��DK�DK� DK��DL� DM�DM��DM�qDN� DO  DO��DPDP� DQ  DQ� DQ��DR}qDS�DSz�DS�qDT��DT�qDU��DV  DV� DW�DWz�DX  DX��DX��DY� DZ�DZ}qDZ�RD[� D\�D\z�D\�qD]��D^�D^��D_
=D_��D_�qD`��Da�Da� Db  Db� Db�qDc}qDc�qDd� Dd��De� Df  Df��Dg�Dg� Dh�Dh}qDh�qDi� Dj�Dj��Dk  Dk�Dk�qDlz�DmDm}qDm�qDn��Dn�RDo� Dp  Dp� Dq  Dq� Dr
=Dr��Ds�Ds��Dt  Dt��Du�DuxRDu��Dv��Dv�qDwxRDx�Dx��Dy  Dy��Dz  Dz� Dz�qD{z�D|D|��D}  D}��D}��D~z�DD��D��D�@ D�� D�D�HD�B�D�� D���D��D�AHD�� D�� D���D�B�D�~�D�� D�  D�@ D�� D�� D��D�@ D���D�D��D�C�D���D��HD�  D�@ D�~�D���D���D�@ D��HD�� D���D�>�D�}qD��qD�  D�ED���D�� D��qD�AHD�~�D���D��D�C�D�~�D���D�HD�>�D�� D���D���D�AHD�}qD���D��D�B�D�� D�� D��)D�@ D���D�� D��qD�AHD���D���D��qD�>�D�� D��)D���D�@ D�� D��HD�HD�@ D��HD�D���D�=qD�~�D��HD�  D�>�D�� D���D���D�@ D�� D��qD�  D�C�D�~�D��qD�HD�B�D�~�D��qD�  D�C�D���D�D���D�<)D�~�D�� D�  D�@ D�� D�D�HD�=qD�� D�D�  D�=qD�}qD��HD��D�>�D�~�D���D�HD�>�D�� D��HD�  D�@ D�� D�� D��qD�>�D��HD��HD���D�<)D�� D�D�HD�>�D�� D��HD���D�AHD��HD��)D��qD�@ D�}qD�� D�HD�=qD�~�D�D�  D�@ D��HD���D���D�@ D�� D��HD�  D�>�D�� D��HD��)D�>�D��HD�� D���D�@ D��HD���D���D�@ D��HD��HD�  D�>�D�|)D���D��D�>�D��HD���D�  D�>�D��HD���D�  D�=qD�~�D�� D���D�>�D��HD��HD��qD�@ D��HD�� D��qD�<)D�}qD�� D�  D�>�D�}qD�� D��D�B�D��HD�� D���D�@ D�}qD���D�D�C�D���D�D�  D�=qD�}qD��qD��)D�AHD���D�� D�HD�B�D�}qD��)D�  D�C�D�j=?8Q�?B�\?aG�?�=q?���?\?�G�@   @��@#�
@333@=p�@Q�@^�R@n{@}p�@���@�\)@�
=@�  @���@���@�
=@�G�@�=q@��@�(�@��
@�@�z�@���AG�A�A	��A��AG�A�A��A��A\)A#�
A'�A+�A/\)A2�\A7
=A:=qA>�RAAG�AE�AHQ�AMp�AQ�AW
=AX��A\��Ab�\Ag
=Aj�HAn�RAs33AxQ�A}p�A���A��A�A��A��A��
A�{A�Q�A��\A��A�  A�=qA���A��A���A�(�A�ffA���A��A�p�A��A�=qA�z�A�
=A���A��
A�ffA���AÅA�Aȣ�A�33A�{A�Q�A��A�z�AָRA���A�33A�p�A�  A��A�z�A�  A�\A���A�
=A�G�A�33A�p�A�\)A���A�33A�(�A�{A��B ��B�B
=BQ�B��B
=BQ�B	p�B
�\B�Bz�B��B�\B�B��BB�RB�B��B��BffB\)BQ�BG�B=qB33BQ�BG�BffB�B z�B!��B"�\B#�B$z�B%��B&�\B'�B(z�B)p�B*ffB+\)B,Q�B,��B-�B/
=B0  B1�B2{B333B4Q�B5G�B6=qB733B8Q�B9p�B:�RB<  B=�B>ffB?�B@��BABC
=BDz�BE��BFffBG�BHz�BI��BJffBK\)BL(�BMp�BN=qBO
=BPQ�BQp�BR�RBT(�BUG�BVffBX  BX��BZffB[�B\��B]�B^�RB_�B`z�Ba��BbffBc\)Bdz�BeBf�RBh  BiG�BjffBk�
Bl��Bm�Bo�Bp��Br{Bs33Bt  Bu�Bv{Bw33Bw�
Bx��ByBz�RB|  B}G�B~�\B�  B��\B�33B�B�Q�B���B��B�Q�B�
=B���B�{B���B�33B��
B�ffB���B��B�(�B���B�33B��B�=qB���B�G�B��B�z�B���B���B�(�B��RB�G�B�  B��RB�G�B�  B���B��B�(�B��RB�\)B��
B�Q�B�
=B���B�(�B��\B���B�p�B�  B���B�33B��
B�z�B��B��
B��\B�G�B�{B��RB�\)B��
B�Q�B��HB�\)B��
B�z�B�
=B�B�ffB��B�B���B�\)B��B�ffB���B��B�B�Q�B���B���B�ffB�
=B���B�=qB�
=B��
B�z�B���B��B�{B��\B��B��B�Q�B���B��B�Q�B���B���B��\B��B��B�Q�B��HB�p�B��
B�(�B���B�\)B�(�B��HB�B�z�B��BŮB�(�BƸRB��BǙ�B�Q�B���BɮB�z�B�G�B��B�z�B��HB��B͙�B�=qB��HBυB�=qB��HBѮBҏ\B��B�B�ffB�
=BՅB�  B�z�B��HBׅB�=qB���B��
Bڣ�B�33BۮB�(�B�ffB�
=BݮB�ffB�
=Bߙ�B�z�B��B��B�z�B��B㙚B�=qB��HB�p�B��
B�Q�B��HB癚B�ffB��B��B��B��B�B�  B�Q�B�
=B��
B��B�B�{B��B��B�B�ffB���B�G�B�  B��RB��B�z�B���B�p�B�{B�ffB��HB���B�ffB�33B�{B���B��B��B�{B���B�p�C 
=C \)C ��C(�C��C�
C{CQ�C�\C�
C(�C�C�CQ�CC{Cp�CC
=CQ�C��C�C(�Cz�C�
CQ�C�RC	  C	=qC	�\C	�HC
  C
\)C
�RC
=CffC��C33C��C�HC�CffC�RC��C=qC�\C�CQ�C��C33C�CC�C�C�C�HC=qC��C
=Cp�C�C��C=qCp�C��C(�C�\C�CG�C�RC�C
=C\)C�RC{Cp�C�C=qCz�C��C{C\)C��C��CQ�C�RC33C�CC
=CQ�C�C��C�C�C�CffC�C   C Q�C �\C C!{C!p�C!C"�C"��C#  C#=qC#ffC#�C${C$�C$�C%=qC%�C%C%��C&G�C&�C'
=C'ffC'�
C(33C(�C(C)  C)(�C)��C)��C*ffC*C+
=C+G�C+�C+�RC,�C,�C-  C-Q�C-��C-�C-�C.Q�C.��C/(�C/ffC/��C/�C033C0��C1  C1p�C1C1��C2{C2\)C2C3�C3��C3�HC4�C4\)C4��C4�C5G�C5�C633C6p�C6��C6��C7�C7ffC7C8�C8��C9  C9=qC9p�C9��C:  C:\)C:��C;=qC;�C;�RC;��C<33C<��C=�C=�C=C>
=C>33C>�\C>�C?Q�C?��C@�C@Q�C@z�C@CA33CA��CB{CBG�CB�CBCC
=CCffCC��CD=qCD�CD�CD�
CE=qCE�RCF{CFQ�CF�\CF��CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                          ?�=q?��H@@  @�G�@�  @��R@�G�A ��A  A   A,(�A@  A`  A�Q�A�  A�Q�A�  A��A�Q�A�  A���A��BQ�B(�B�
B   B(  B/�
B8  B@(�BH(�BO�BW�
B`(�Bh  BpQ�Bx  B�
B�  B��B�  B�  B�{B�  B��
B��
B�{B��B�(�B�  B�(�B��B�=qB��B�(�B�B�=qB��
B�{B׮B�  B��B�{B��
B��B�(�B�B��
B�(�B��C�C
=C
=C�C	�HC  C�C(�C�HC  C(�C�C  C{C�HC �C!�C#��C%�C(�C)��C+��C-��C/��C2  C4{C6(�C8  C9�
C;�HC>  C@{CB{CD�CF�CH33CJ33CL{CN
=CP(�CR(�CT{CV
=CX(�CZ(�C\(�C^�C`�Cb
=Cd
=Cf  Cg�Ci��Cl  Cn�Cp  Cq�Ct  Cv{Cx  Cy�HC|
=C~(�C�C�  C��C�C�C�
=C�{C�C�  C�  C�C�{C���C��fC��C���C�C�
=C�\C�C��C��C��C���C��C��C�C�
=C�\C�
=C�
=C�\C�\C�\C�{C�C��C��C���C�C�  C�C�
=C�
=C�  C��C��C�  C��C�
=C�C�  C�C��C�  C���C��C���C���C�
=C�\C�\C�\C��C��C���C�  C���C���C�\C�C��C��HC��fC��C���C���C���C�  C�C���C��C���C�  C�  C��C��C��C�  C�
=C�
=C�C�  C���C���C�  C�C�
=C�C��C�  C�{C�
=C���C���C�C�{C�\C�\C�\C�{C�
=C�C�  C���C���C��C�C�
=C���C��C���C�  C���C�C�
=C�{C�C���D � D�D��D�D��D�D}qD�RD}qD�D�=D  D}qD�D� DD�=D	  D	xRD
  D
��D
�qDu�D��D� D�D�=D�Ds3D�RD��D�D� D��D� D�D�D
=D��D  D�D�D� D��D� DD��DD� D�RD}qD�D��D�D� D�D��D�D� D�D��D  D}qD��D xRD!  D!��D!��D"xRD#  D#�=D$D$��D%
=D%� D%�RD&}qD'  D'}qD(  D(��D)�D)z�D)��D*� D+�D+z�D+�RD,� D-�D-� D-�qD.� D/�D/��D/�qD0u�D0��D1}qD1�RD2}qD3�D3}qD3��D4}qD5  D5� D6�D6� D6�RD7z�D7�qD8��D9D9��D:  D:��D;  D;� D;��D<z�D=  D=xRD=�RD>� D?  D?�D@�D@�DA�DA��DB  DB� DB�qDCxRDD  DD��DEDE��DF�DF� DG  DG��DH  DHz�DI�DI��DJ�DJ��DK�DK� DK��DL� DM�DM��DM�qDN� DO  DO��DPDP� DQ  DQ� DQ��DR}qDS�DSz�DS�qDT��DT�qDU��DV  DV� DW�DWz�DX  DX��DX��DY� DZ�DZ}qDZ�RD[� D\�D\z�D\�qD]��D^�D^��D_
=D_��D_�qD`��Da�Da� Db  Db� Db�qDc}qDc�qDd� Dd��De� Df  Df��Dg�Dg� Dh�Dh}qDh�qDi� Dj�Dj��Dk  Dk�Dk�qDlz�DmDm}qDm�qDn��Dn�RDo� Dp  Dp� Dq  Dq� Dr
=Dr��Ds�Ds��Dt  Dt��Du�DuxRDu��Dv��Dv�qDwxRDx�Dx��Dy  Dy��Dz  Dz� Dz�qD{z�D|D|��D}  D}��D}��D~z�DD��D��D�@ D�� D�D�HD�B�D�� D���D��D�AHD�� D�� D���D�B�D�~�D�� D�  D�@ D�� D�� D��D�@ D���D�D��D�C�D���D��HD�  D�@ D�~�D���D���D�@ D��HD�� D���D�>�D�}qD��qD�  D�ED���D�� D��qD�AHD�~�D���D��D�C�D�~�D���D�HD�>�D�� D���D���D�AHD�}qD���D��D�B�D�� D�� D��)D�@ D���D�� D��qD�AHD���D���D��qD�>�D�� D��)D���D�@ D�� D��HD�HD�@ D��HD�D���D�=qD�~�D��HD�  D�>�D�� D���D���D�@ D�� D��qD�  D�C�D�~�D��qD�HD�B�D�~�D��qD�  D�C�D���D�D���D�<)D�~�D�� D�  D�@ D�� D�D�HD�=qD�� D�D�  D�=qD�}qD��HD��D�>�D�~�D���D�HD�>�D�� D��HD�  D�@ D�� D�� D��qD�>�D��HD��HD���D�<)D�� D�D�HD�>�D�� D��HD���D�AHD��HD��)D��qD�@ D�}qD�� D�HD�=qD�~�D�D�  D�@ D��HD���D���D�@ D�� D��HD�  D�>�D�� D��HD��)D�>�D��HD�� D���D�@ D��HD���D���D�@ D��HD��HD�  D�>�D�|)D���D��D�>�D��HD���D�  D�>�D��HD���D�  D�=qD�~�D�� D���D�>�D��HD��HD��qD�@ D��HD�� D��qD�<)D�}qD�� D�  D�>�D�}qD�� D��D�B�D��HD�� D���D�@ D�}qD���D�D�C�D���D�D�  D�=qD�}qD��qD��)D�AHD���D�� D�HD�B�D�}qD��)D�  D�C�G�O�?8Q�?B�\?aG�?�=q?���?\?�G�@   @��@#�
@333@=p�@Q�@^�R@n{@}p�@���@�\)@�
=@�  @���@���@�
=@�G�@�=q@��@�(�@��
@�@�z�@���AG�A�A	��A��AG�A�A��A��A\)A#�
A'�A+�A/\)A2�\A7
=A:=qA>�RAAG�AE�AHQ�AMp�AQ�AW
=AX��A\��Ab�\Ag
=Aj�HAn�RAs33AxQ�A}p�A���A��A�A��A��A��
A�{A�Q�A��\A��A�  A�=qA���A��A���A�(�A�ffA���A��A�p�A��A�=qA�z�A�
=A���A��
A�ffA���AÅA�Aȣ�A�33A�{A�Q�A��A�z�AָRA���A�33A�p�A�  A��A�z�A�  A�\A���A�
=A�G�A�33A�p�A�\)A���A�33A�(�A�{A��B ��B�B
=BQ�B��B
=BQ�B	p�B
�\B�Bz�B��B�\B�B��BB�RB�B��B��BffB\)BQ�BG�B=qB33BQ�BG�BffB�B z�B!��B"�\B#�B$z�B%��B&�\B'�B(z�B)p�B*ffB+\)B,Q�B,��B-�B/
=B0  B1�B2{B333B4Q�B5G�B6=qB733B8Q�B9p�B:�RB<  B=�B>ffB?�B@��BABC
=BDz�BE��BFffBG�BHz�BI��BJffBK\)BL(�BMp�BN=qBO
=BPQ�BQp�BR�RBT(�BUG�BVffBX  BX��BZffB[�B\��B]�B^�RB_�B`z�Ba��BbffBc\)Bdz�BeBf�RBh  BiG�BjffBk�
Bl��Bm�Bo�Bp��Br{Bs33Bt  Bu�Bv{Bw33Bw�
Bx��ByBz�RB|  B}G�B~�\B�  B��\B�33B�B�Q�B���B��B�Q�B�
=B���B�{B���B�33B��
B�ffB���B��B�(�B���B�33B��B�=qB���B�G�B��B�z�B���B���B�(�B��RB�G�B�  B��RB�G�B�  B���B��B�(�B��RB�\)B��
B�Q�B�
=B���B�(�B��\B���B�p�B�  B���B�33B��
B�z�B��B��
B��\B�G�B�{B��RB�\)B��
B�Q�B��HB�\)B��
B�z�B�
=B�B�ffB��B�B���B�\)B��B�ffB���B��B�B�Q�B���B���B�ffB�
=B���B�=qB�
=B��
B�z�B���B��B�{B��\B��B��B�Q�B���B��B�Q�B���B���B��\B��B��B�Q�B��HB�p�B��
B�(�B���B�\)B�(�B��HB�B�z�B��BŮB�(�BƸRB��BǙ�B�Q�B���BɮB�z�B�G�B��B�z�B��HB��B͙�B�=qB��HBυB�=qB��HBѮBҏ\B��B�B�ffB�
=BՅB�  B�z�B��HBׅB�=qB���B��
Bڣ�B�33BۮB�(�B�ffB�
=BݮB�ffB�
=Bߙ�B�z�B��B��B�z�B��B㙚B�=qB��HB�p�B��
B�Q�B��HB癚B�ffB��B��B��B��B�B�  B�Q�B�
=B��
B��B�B�{B��B��B�B�ffB���B�G�B�  B��RB��B�z�B���B�p�B�{B�ffB��HB���B�ffB�33B�{B���B��B��B�{B���B�p�C 
=C \)C ��C(�C��C�
C{CQ�C�\C�
C(�C�C�CQ�CC{Cp�CC
=CQ�C��C�C(�Cz�C�
CQ�C�RC	  C	=qC	�\C	�HC
  C
\)C
�RC
=CffC��C33C��C�HC�CffC�RC��C=qC�\C�CQ�C��C33C�CC�C�C�C�HC=qC��C
=Cp�C�C��C=qCp�C��C(�C�\C�CG�C�RC�C
=C\)C�RC{Cp�C�C=qCz�C��C{C\)C��C��CQ�C�RC33C�CC
=CQ�C�C��C�C�C�CffC�C   C Q�C �\C C!{C!p�C!C"�C"��C#  C#=qC#ffC#�C${C$�C$�C%=qC%�C%C%��C&G�C&�C'
=C'ffC'�
C(33C(�C(C)  C)(�C)��C)��C*ffC*C+
=C+G�C+�C+�RC,�C,�C-  C-Q�C-��C-�C-�C.Q�C.��C/(�C/ffC/��C/�C033C0��C1  C1p�C1C1��C2{C2\)C2C3�C3��C3�HC4�C4\)C4��C4�C5G�C5�C633C6p�C6��C6��C7�C7ffC7C8�C8��C9  C9=qC9p�C9��C:  C:\)C:��C;=qC;�C;�RC;��C<33C<��C=�C=�C=C>
=C>33C>�\C>�C?Q�C?��C@�C@Q�C@z�C@CA33CA��CB{CBG�CB�CBCC
=CCffCC��CD=qCD�CD�CD�
CE=qCE�RCF{CFQ�CF�\CF��CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                          @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��HA��;A��
A��FA���A��+A��PA���A���A�bNA�/A���A�x�A�ZA���A��yA��HA��#A��9A���A�dZA��A���A�ĜA���A�-A�%A�A�ZA�1A��A��;A���A��hA�n�A�;dA���A�n�A�ZA�?}A�7LA���A�l�A�A���A��-A���A�hsA�C�A�7LA� �A��A�A���A��mA���A���A� �A��;A���A�VA��HA��\A�A��A�K�A���A���A��mA��A�E�A���A��+A�G�A�VA���A��wA��^A��RA��uA�ffA��A��A���A���A�-A�K�A��!A�ĜA�  A�ȴA��FA�?}A�I�A�1A�%A�VA�O�A�r�A��9A�;dA�(�A���A�-A�^5A�XA���A��
A� �A�bA��^A� �A��wA���A��A���A��A��#A�1'A��FA���A�XA�7LA�
=A��#A�;dA�&�A�;dAoA}t�Ay��AvM�Au�At�yAq?}Ao�AlbNAi
=Af(�Ad��A`��A^�yA]G�A[�
AZ�`AY�#AW�;AU|�AS"�ARr�AR �AQ
=AP�jAP1'ANAL��AKAJ �AH��AHjAF�`AE�;AD�HAB�/A@ZA>^5A:��A9�A7x�A6ĜA6bA61A3x�A2JA1&�A0I�A09XA/�A/�PA/"�A.-A-�A,ĜA*�A)oA'��A&�uA%�wA%�A$�A#A"��A!��A!K�A z�A
=A�DA�#AhsA�AffA�+A��AZA�-A/A=qA��A�AĜA�+AJA%A��A��AĜAz�A�;AE�A�+AhsA
=qA	K�A��Av�Ax�AJA��AM�A��A��Az�A��A �`@��;@�@�ƨ@��@��@�K�@�|�@�G�@�Z@�\)@�C�@��@���@�@��@�@��@�X@�7L@���@�r�@�`B@��@��/@��@�M�@��;@�+@��y@�^5@��@ա�@�7L@�Q�@���@�;d@щ7@Ѓ@Ϯ@��@ͺ^@��@�j@��@˝�@���@ɲ-@ɉ7@ɡ�@�@ɉ7@��@�%@��@ȴ9@�1@�ƨ@ǅ@���@�$�@���@��T@��#@�@�@�z�@���@�M�@�x�@��@�%@���@�bN@� �@�b@��;@���@��@���@�Z@�K�@�&�@�K�@�~�@�{@��@��^@�&�@�dZ@�V@���@�@���@��7@�p�@��@�Ĝ@�ƨ@���@���@���@�{@�{@���@���@�O�@���@�S�@��H@�^5@���@�7L@�z�@�ƨ@�o@��!@�~�@�{@��^@��@���@�hs@��@��9@�9X@��m@��
@��
@��@�\)@���@�|�@�l�@�+@���@���@�&�@��@�1@��@��;@���@�K�@�+@�C�@�"�@��H@��@��@��R@�^5@�J@���@�O�@���@��@�Ĝ@��@�Q�@�1'@��@���@��w@��@�\)@��y@�@�Z@�9X@��@�1@�ƨ@�;d@��y@��@���@��R@��+@��\@���@�v�@�ff@�E�@�=q@�-@���@���@�bN@� �@�1@�  @��m@�ƨ@���@�dZ@�C�@��@���@��@���@�^5@�G�@�r�@�A�@�1@���@���@�S�@�o@�ff@���@�`B@�X@�?}@�/@�&�@�&�@�G�@�?}@��@�(�@��w@�K�@�
=@��+@�hs@���@��T@���@��@�`B@�G�@��@���@��9@���@��j@���@��`@�%@�V@�/@�/@��9@�Q�@�  @��@��@�t�@�33@��\@�V@��T@�hs@�O�@�/@��/@���@�j@�b@���@�S�@��@���@�^5@���@��@��#@���@���@�?}@��@���@���@�I�@�  @+@~ȴ@~@}V@|�/@{�
@{33@{"�@{"�@{@z�H@z�!@zn�@y�^@x��@x�u@xbN@xA�@x  @w\)@w;d@w;d@w;d@w+@w�@v�R@vE�@u�@u��@u��@u`B@u�@tj@sƨ@s�F@s��@sdZ@r��@q�@q��@qG�@p�`@pr�@p  @o�@o|�@o\)@n�y@nv�@n$�@m�@m��@m@mp�@m�@l�j@l1@kdZ@j�H@jn�@ihs@h�@hbN@hA�@g�@g+@g�@fv�@fV@f5?@e�@e�T@e�T@e�T@e�T@e�-@e�@e?}@d�@dz�@dZ@d9X@d1@b�!@b^5@bM�@b-@a��@`��@`�`@`�9@`��@` �@_��@_��@_\)@_
=@^��@^ff@^5?@^{@^@]@]��@]�@]�@\�D@\�@[��@[�m@[ƨ@[��@[C�@["�@Z�!@Z=q@Y�#@Y��@Y&�@XĜ@X1'@W�@W\)@V�y@Vv�@V5?@U�@U��@Up�@T�@T�@Tj@T9X@St�@Rn�@RJ@RJ@Q�#@Q�7@Q7L@P�u@P �@O�;@O��@O;d@O�@Nȴ@MV@L(�@L1@L1@K�F@K��@K��@K��@Kt�@KC�@Ko@J�H@J�!@J�!@J��@J�\@J-@I�#@I��@I�7@I�@H�`@H�`@H�u@HbN@G��@G|�@F��@F�R@Fv�@F{@E@E�h@E?}@EV@D�j@D��@Dz�@D9X@C�
@C"�@A��@A��@A��@A��@A��@@Ĝ@@A�@@b@@  @?�;@?|�@?\)@?K�@?;d@?
=@>��@>�R@>E�@=�@=�h@=O�@<�/@<�D@<(�@;�F@;�@;S�@;33@;33@:��@:=q@:J@9��@9X@9�@9%@8��@8��@8�9@8bN@8  @7�@7�w@7��@7\)@7�@6��@6ff@5�@5�T@5@5O�@5V@4�@4z�@49X@3�m@3��@3dZ@3dZ@3dZA��TA��TA��`A��HA��TA��TA��HA��HA��HA��HA��;A��/A��HA��`A��TA���A���A��TA���A���A��-A���A���A���A���A���A���A�v�A�x�A��7A��DA��DA��hA���A��uA���A���A���A���A���A���A��DA�ffA�Q�A�7LA�=qA�?}A�33A�C�A�&�A��A��A�  A���A��
A��A�t�A�v�A�jA�hsA�dZA�jA�r�A�r�A��PA��DA��A��hA��A��hA�I�A�=qA�+A���A���A���A���A���A���A��A��yA��yA��TA��TA��yA��A��mA��`A��TA��;A��/A��;A��HA��;A��HA��;A��;A��
A��
A���A���A�ȴA��^A��RA���A���A��A���A���A���A���A��hA���A���A��\A��DA�`BA�ffA�`BA�E�A�`BA�S�A�1'A�+A��A��A�oA�oA�bA�%A�  A���A���A���A���A��A��`A��HA��/A���A���A���A��9A��A���A���A���A���A���A���A���A���A�~�A�`BA�S�A�C�A�(�A��A�{A�VA�  A�  A�A��A��A���A��A�+A�{A��A��mA��;A��
A��FA���A���A��\A��+A�v�A�`BA�K�A�G�A�1'A��A��A�bA�1A�A�A�  A���A���A���A��A��A��A��yA��mA��mA��`A��;A��;A��/A��/A��#A���A��A���A���A���A���A���A���A���A���A��uA��uA��\A��DA��+A��A�z�A�r�A�l�A�dZA�`BA�`BA�bNA�Q�A�K�A�C�A�G�A�(�A���A��#A��^A��-A���A���A���A��DA��A�x�A�p�A�ffA�bNA�`BA�^5A�\)A�\)A�\)A�ZA�XA�S�A�K�A�G�A�C�A�?}A�;dA�9XA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�/A�&�A��A�%A��A��^A���A���A���A�r�A�G�A�?}A�7LA�5?A�33A�-A��A��HA��/A��/A��#A��A��
A���A���A�ĜA��jA��FA��FA��9A��-A��!A��!A���A���A���A���A���A���A���A���A�|�A�hsA�^5A�Q�A�K�A�G�A�E�A�E�A�E�A�C�A�A�A�=qA�=qA�;dA�;dA�9XA�7LA�33A�+A� �A��A� �A� �A� �A� �A� �A��A��A��A��A�bA�JA�1A�%A�A�A���A���A���A�  A���A���A���A���A��A��A��A��`A��HA��;A��/A��/A��A��
A���A���A���A�ȴA�A��^A���A��hA�l�A�bNA�\)A�Q�A�&�A�
=A���A��A��`A��TA��HA��;A��/A��/A��#A���A��9A���A���A��DA�z�A�bNA�ZA�ZA�XA�VA�Q�A�A�A�+A�JA��
A�A��wA��jA��^A��FA��9A��A���A�S�A�-A��A�bA�
=A�A���A��A��yA��HA�ĜA���A���A��hA��A�hsA�ZA�O�A�;dA�5?A�1'A�(�A��A�A��A��;A��
A���A�ȴA��FA���A��uA�|�A�ffA�XA�(�A�A��`A�ƨA�~�A�Q�A�A�A�&�A��A�oA�A��A���A���A�?}A�%A��A���A��jA��-A���A���A��uA��hA��\A��\A��DA��A�z�A�t�A�jA�\)A�A�A�1'A�-A�+A�&�A�$�A��A�VA��A��`A��;A��
A���A���A���A�ȴA�ȴA�ƨA�ĜA��wA��^A��RA��RA��RA��^A��^A��^A��^A��^A��RA��RA��RA��FA��RA��^A��RA��A���A��uA��A�v�A�l�A�jA�jA�hsA�dZA�\)A�XA�O�A�I�A�/A��A��A�ȴA��^A���A�~�A�ffA�S�A� �A��A�JA���A��A��/A�A��RA���A���A���A��hA��+A�v�A�\)A�1'A�oA��A��#A���A�`BA�1'A��A���A��A��A��`A��FA���A��\A�n�A�7LA�oA�  A��mA���A�VA�+A�+A�bA���A��yA��;A���A���A�ƨA�A�ĜA�A���A���A��jA��^A��-A��!A���A��uA�n�A�-A��A��A��A�JA��HA���A�hsA�VA�A��A�\)A�=qA��A�VA��A���A��A�dZA�M�A�5?A��A���A���A�n�A�dZA�^5A�VA�C�A�/A���A��9A�^5A���A���A�A���A�9XA��9A�7LA���A�G�A�bA���A��`A���A��7A�jA�ffA�I�A�A�A�?}A�;dA�(�A�/A�1'A�33A�5?A�33A� �A�oA�%A�1A�%A�A���A��A��`A�ĜA��7A�Q�A��A��`A��A�/A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                          A��TA��HA��;A��
A��FA���A��+A��PA���A���A�bNA�/A���A�x�A�ZA���A��yA��HA��#A��9A���A�dZA��A���A�ĜA���A�-A�%A�A�ZA�1A��A��;A���A��hA�n�A�;dA���A�n�A�ZA�?}A�7LA���A�l�A�A���A��-A���A�hsA�C�A�7LA� �A��A�A���A��mA���A���A� �A��;A���A�VA��HA��\A�A��A�K�A���A���A��mA��A�E�A���A��+A�G�A�VA���A��wA��^A��RA��uA�ffA��A��A���A���A�-A�K�A��!A�ĜA�  A�ȴA��FA�?}A�I�A�1A�%A�VA�O�A�r�A��9A�;dA�(�A���A�-A�^5A�XA���A��
A� �A�bA��^A� �A��wA���A��A���A��A��#A�1'A��FA���A�XA�7LA�
=A��#A�;dA�&�A�;dAoA}t�Ay��AvM�Au�At�yAq?}Ao�AlbNAi
=Af(�Ad��A`��A^�yA]G�A[�
AZ�`AY�#AW�;AU|�AS"�ARr�AR �AQ
=AP�jAP1'ANAL��AKAJ �AH��AHjAF�`AE�;AD�HAB�/A@ZA>^5A:��A9�A7x�A6ĜA6bA61A3x�A2JA1&�A0I�A09XA/�A/�PA/"�A.-A-�A,ĜA*�A)oA'��A&�uA%�wA%�A$�A#A"��A!��A!K�A z�A
=A�DA�#AhsA�AffA�+A��AZA�-A/A=qA��A�AĜA�+AJA%A��A��AĜAz�A�;AE�A�+AhsA
=qA	K�A��Av�Ax�AJA��AM�A��A��Az�A��A �`@��;@�@�ƨ@��@��@�K�@�|�@�G�@�Z@�\)@�C�@��@���@�@��@�@��@�X@�7L@���@�r�@�`B@��@��/@��@�M�@��;@�+@��y@�^5@��@ա�@�7L@�Q�@���@�;d@щ7@Ѓ@Ϯ@��@ͺ^@��@�j@��@˝�@���@ɲ-@ɉ7@ɡ�@�@ɉ7@��@�%@��@ȴ9@�1@�ƨ@ǅ@���@�$�@���@��T@��#@�@�@�z�@���@�M�@�x�@��@�%@���@�bN@� �@�b@��;@���@��@���@�Z@�K�@�&�@�K�@�~�@�{@��@��^@�&�@�dZ@�V@���@�@���@��7@�p�@��@�Ĝ@�ƨ@���@���@���@�{@�{@���@���@�O�@���@�S�@��H@�^5@���@�7L@�z�@�ƨ@�o@��!@�~�@�{@��^@��@���@�hs@��@��9@�9X@��m@��
@��
@��@�\)@���@�|�@�l�@�+@���@���@�&�@��@�1@��@��;@���@�K�@�+@�C�@�"�@��H@��@��@��R@�^5@�J@���@�O�@���@��@�Ĝ@��@�Q�@�1'@��@���@��w@��@�\)@��y@�@�Z@�9X@��@�1@�ƨ@�;d@��y@��@���@��R@��+@��\@���@�v�@�ff@�E�@�=q@�-@���@���@�bN@� �@�1@�  @��m@�ƨ@���@�dZ@�C�@��@���@��@���@�^5@�G�@�r�@�A�@�1@���@���@�S�@�o@�ff@���@�`B@�X@�?}@�/@�&�@�&�@�G�@�?}@��@�(�@��w@�K�@�
=@��+@�hs@���@��T@���@��@�`B@�G�@��@���@��9@���@��j@���@��`@�%@�V@�/@�/@��9@�Q�@�  @��@��@�t�@�33@��\@�V@��T@�hs@�O�@�/@��/@���@�j@�b@���@�S�@��@���@�^5@���@��@��#@���@���@�?}@��@���@���@�I�@�  @+@~ȴ@~@}V@|�/@{�
@{33@{"�@{"�@{@z�H@z�!@zn�@y�^@x��@x�u@xbN@xA�@x  @w\)@w;d@w;d@w;d@w+@w�@v�R@vE�@u�@u��@u��@u`B@u�@tj@sƨ@s�F@s��@sdZ@r��@q�@q��@qG�@p�`@pr�@p  @o�@o|�@o\)@n�y@nv�@n$�@m�@m��@m@mp�@m�@l�j@l1@kdZ@j�H@jn�@ihs@h�@hbN@hA�@g�@g+@g�@fv�@fV@f5?@e�@e�T@e�T@e�T@e�T@e�-@e�@e?}@d�@dz�@dZ@d9X@d1@b�!@b^5@bM�@b-@a��@`��@`�`@`�9@`��@` �@_��@_��@_\)@_
=@^��@^ff@^5?@^{@^@]@]��@]�@]�@\�D@\�@[��@[�m@[ƨ@[��@[C�@["�@Z�!@Z=q@Y�#@Y��@Y&�@XĜ@X1'@W�@W\)@V�y@Vv�@V5?@U�@U��@Up�@T�@T�@Tj@T9X@St�@Rn�@RJ@RJ@Q�#@Q�7@Q7L@P�u@P �@O�;@O��@O;d@O�@Nȴ@MV@L(�@L1@L1@K�F@K��@K��@K��@Kt�@KC�@Ko@J�H@J�!@J�!@J��@J�\@J-@I�#@I��@I�7@I�@H�`@H�`@H�u@HbN@G��@G|�@F��@F�R@Fv�@F{@E@E�h@E?}@EV@D�j@D��@Dz�@D9X@C�
@C"�@A��@A��@A��@A��@A��@@Ĝ@@A�@@b@@  @?�;@?|�@?\)@?K�@?;d@?
=@>��@>�R@>E�@=�@=�h@=O�@<�/@<�D@<(�@;�F@;�@;S�@;33@;33@:��@:=q@:J@9��@9X@9�@9%@8��@8��@8�9@8bN@8  @7�@7�w@7��@7\)@7�@6��@6ff@5�@5�T@5@5O�@5V@4�@4z�@49X@3�m@3��@3dZ@3dZG�O�A��TA��TA��`A��HA��TA��TA��HA��HA��HA��HA��;A��/A��HA��`A��TA���A���A��TA���A���A��-A���A���A���A���A���A���A�v�A�x�A��7A��DA��DA��hA���A��uA���A���A���A���A���A���A��DA�ffA�Q�A�7LA�=qA�?}A�33A�C�A�&�A��A��A�  A���A��
A��A�t�A�v�A�jA�hsA�dZA�jA�r�A�r�A��PA��DA��A��hA��A��hA�I�A�=qA�+A���A���A���A���A���A���A��A��yA��yA��TA��TA��yA��A��mA��`A��TA��;A��/A��;A��HA��;A��HA��;A��;A��
A��
A���A���A�ȴA��^A��RA���A���A��A���A���A���A���A��hA���A���A��\A��DA�`BA�ffA�`BA�E�A�`BA�S�A�1'A�+A��A��A�oA�oA�bA�%A�  A���A���A���A���A��A��`A��HA��/A���A���A���A��9A��A���A���A���A���A���A���A���A���A�~�A�`BA�S�A�C�A�(�A��A�{A�VA�  A�  A�A��A��A���A��A�+A�{A��A��mA��;A��
A��FA���A���A��\A��+A�v�A�`BA�K�A�G�A�1'A��A��A�bA�1A�A�A�  A���A���A���A��A��A��A��yA��mA��mA��`A��;A��;A��/A��/A��#A���A��A���A���A���A���A���A���A���A���A��uA��uA��\A��DA��+A��A�z�A�r�A�l�A�dZA�`BA�`BA�bNA�Q�A�K�A�C�A�G�A�(�A���A��#A��^A��-A���A���A���A��DA��A�x�A�p�A�ffA�bNA�`BA�^5A�\)A�\)A�\)A�ZA�XA�S�A�K�A�G�A�C�A�?}A�;dA�9XA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�/A�&�A��A�%A��A��^A���A���A���A�r�A�G�A�?}A�7LA�5?A�33A�-A��A��HA��/A��/A��#A��A��
A���A���A�ĜA��jA��FA��FA��9A��-A��!A��!A���A���A���A���A���A���A���A���A�|�A�hsA�^5A�Q�A�K�A�G�A�E�A�E�A�E�A�C�A�A�A�=qA�=qA�;dA�;dA�9XA�7LA�33A�+A� �A��A� �A� �A� �A� �A� �A��A��A��A��A�bA�JA�1A�%A�A�A���A���A���A�  A���A���A���A���A��A��A��A��`A��HA��;A��/A��/A��A��
A���A���A���A�ȴA�A��^A���A��hA�l�A�bNA�\)A�Q�A�&�A�
=A���A��A��`A��TA��HA��;A��/A��/A��#A���A��9A���A���A��DA�z�A�bNA�ZA�ZA�XA�VA�Q�A�A�A�+A�JA��
A�A��wA��jA��^A��FA��9A��A���A�S�A�-A��A�bA�
=A�A���A��A��yA��HA�ĜA���A���A��hA��A�hsA�ZA�O�A�;dA�5?A�1'A�(�A��A�A��A��;A��
A���A�ȴA��FA���A��uA�|�A�ffA�XA�(�A�A��`A�ƨA�~�A�Q�A�A�A�&�A��A�oA�A��A���A���A�?}A�%A��A���A��jA��-A���A���A��uA��hA��\A��\A��DA��A�z�A�t�A�jA�\)A�A�A�1'A�-A�+A�&�A�$�A��A�VA��A��`A��;A��
A���A���A���A�ȴA�ȴA�ƨA�ĜA��wA��^A��RA��RA��RA��^A��^A��^A��^A��^A��RA��RA��RA��FA��RA��^A��RA��A���A��uA��A�v�A�l�A�jA�jA�hsA�dZA�\)A�XA�O�A�I�A�/A��A��A�ȴA��^A���A�~�A�ffA�S�A� �A��A�JA���A��A��/A�A��RA���A���A���A��hA��+A�v�A�\)A�1'A�oA��A��#A���A�`BA�1'A��A���A��A��A��`A��FA���A��\A�n�A�7LA�oA�  A��mA���A�VA�+A�+A�bA���A��yA��;A���A���A�ƨA�A�ĜA�A���A���A��jA��^A��-A��!A���A��uA�n�A�-A��A��A��A�JA��HA���A�hsA�VA�A��A�\)A�=qA��A�VA��A���A��A�dZA�M�A�5?A��A���A���A�n�A�dZA�^5A�VA�C�A�/A���A��9A�^5A���A���A�A���A�9XA��9A�7LA���A�G�A�bA���A��`A���A��7A�jA�ffA�I�A�A�A�?}A�;dA�(�A�/A�1'A�33A�5?A�33A� �A�oA�%A�1A�%A�A���A��A��`A�ĜA��7A�Q�A��A��`A��A�/A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                          ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BP}BP}BO�BO�BR�BPBQBP�BO�BQ�BR�BP�BYBY�B^5BiyBjBiDBh�BiDBh�BkBlWBl�Bm�Bn�Bu�B�;B��B�LB�B��B�B�!B��B�B�3B�B��B��B��B��B�FB�B��B��B�aB�[B�nB�hB�hB�hB��B��B�aB��B�-B��B��B��B��B��B��B�LB�!B�=B�SB�FB�B��B��B��B��B�YB�1B�B��B��B��B��B�eB��B�B��B�{B�(B�JB��B}"Bu%Bg8Bd�BgmBc BX�B@�B1�B �BYB�B�vB�9B�&B��B�^B��B��B��B��B�VBt�BV�BN�BFtB>�B2�B*�BMBVB
��B
�
B
�?B
уB
��B
�^B
ǮB
��B
�zB
��B
u�B
iyB
UgB
7�B
0�B
*�B
B
GB	�B	�B	�tB	B	��B	��B	��B	��B	�B	�4B	q�B	iyB	ZB	R�B	R�B	HKB	DgB	B�B	3�B	%�B	!�B	=B	$B	�B	_B	 �B��B�B�]B�B��B��B��B��B��B��B��B�~B�\B�xB�IB��B�XB��B�@B��B��B��B�%B|�By�By	Bu�Bv�BwfB�MB�;B�AB��B~]B~�B|B{�By�B{�Bv`Bx�Bw�BtTBu�Bs�BrBpoBo Bn/Bn�Bk�BjBl�Bl�Bk�Bm�Bo5BiBiBf�Be�Bb�Ba|Bf2Bb�B`�BdZBbB_�B^�B_;B_;B]�B\�B[#BZQBYKBY�BP�BT,BPBNpBRTBT�BW�BX�BZBT�BR�BQ�BQNBP�BR BYBW
BVBYB]�B`�B_�B_�B`�B`�B`BBa�Bc�Bc�Bf2Bk�Bm)BqBp;BsMBv�B|B}�B}�B�GB�fB�=B�~B��B�hB��B��B��B�eB��B�'B��B��B��B��B�^B��B�^B��B�0B��B��B��B�wB�HB��B��B��B�3B�mB�B�^B��BǮB�B�KB�B�mB��B��BĜBƨB��B��B��BǮB�zBƨBƨBǮB�EB�0BϫB�TB��B�dB�B�B��B�,B��B�AB�B�>B�cB	�B	�B	�B	B	\B	�B	YB	B	OB	#�B	%FB	'B	)�B	.B	.B	-�B	-�B	-wB	.�B	6FB	=�B	@�B	B�B	GB	HB	I�B	J�B	N�B	P}B	S�B	VB	VmB	W�B	ZQB	[WB	Z�B	]�B	b�B	d�B	h
B	jKB	k�B	m�B	m�B	ncB	rB	t�B	v+B	v�B	v�B	v�B	v`B	uZB	t�B	u�B	y>B	|PB	|�B	|�B	}"B	~�B	�uB	�MB	��B	��B	��B	��B	��B	��B	�B	�JB	�B	��B	�DB	��B	�.B	�:B	�{B	�B	�MB	��B	�7B	�CB	��B	��B	��B	��B	�nB	�B	�B	�kB	�OB	�UB	��B	�3B	�9B	��B	��B	��B	�zB	�B	��B	�dB	��B	�B	�6B	��B	�'B	ÖB	�EB	�EB	�tB	�9B	��B	�B	̘B	ΥB	�BB	ϫB	��B	�TB	�B	�NB	� B	��B	��B	՛B	�B	֡B	��B	ٴB	ݘB	�BB	�pB	�BB	�B	�NB	�NB	�B	�HB	��B	�B	�B	�B	��B	��B	��B	�`B	�2B	�8B	��B	��B	�B	�B	��B	�B	��B	��B	�]B	�iB	�iB	�iB	�iB	��B	�B	�oB	�B	�AB	�AB	�B	��B	��B	�B	��B	��B	��B	�%B	�+B	�lB	��B	��B	��B	�B	��B	��B	�PB	�B	�B	�PB	�B	��B	��B	��B	��B	��B	��B	�]B
 4B
 iB
 �B
B
B
MB
�B
SB
%B
�B
�B
1B
�B
�B
	B
	�B

=B

�B

�B
B
B
DB
�B
B
�B
PB
�B
"B
�B
bB
�B
�B
�B
�B
:B
FB
B
{B
B
B
B
B
B
MB
MB
�B
$B
$B
$B
YB
YB
�B
�B
�B
�B
1B
�B
eB
kB
=B
�B
�B
�B
B
B
B
B
B
IB
B
IB
IB
IB
OB
�B
�B
VB
�B
�B
 'B
 �B
 �B
!bB
!�B
"4B
"4B
#:B
#nB
$@B
$@B
$tB
%FB
%B
%zB
%�B
%�B
&B
&B
&B
&�B
&�B
'�B
(�B
(�B
(�B
)*B
)�B
)�B
*�B
+B
,B
,qB
,qB
,=B
,qB
-�B
-CB
-B
-B
-�B
.}B
/B
/�B
/�B
/�B
0!B
0�B
1'B
0�B
1'B
0�B
1�B
2-B
2aB
2�B
33B
33B
2�B
3hB
3hB
49B
49B
5B
5B
5�B
5�B
6FB
6FB
6�B
6�B
7LB
7LB
7LB
7�B
7�B
8�B
:^B
:*B
:*B
9�B
9�B
;�B
<6B
<jB
<�B
=B
=qB
=qB
=qB
=qB
=�B
=�B
>wB
?B
?B
?�B
?�B
@�B
@�B
A�B
B[B
B[B
B�B
B�B
B�B
C�B
C�B
C�B
DgB
DgB
D�B
D�B
EB
EB
EB
E�B
E�B
E�B
FB
FB
F?B
FtB
GEB
GEB
GzB
GEB
GzB
G�B
G�B
H�B
H�B
IB
I�B
I�B
I�B
J#B
JXBQBQBPBQ�BPBP�BO�BPHBP�BPBP�BO�BM�BM�BPHBPHBOBK�BY�BS�BM�BT,BOBBO�BQ�BM�BN�BQNBO�BL�BPHBP}BQ�BOBBP�BPHBO�BRTBQNBPHBP}BV9BU�BP�BR�BN�BT�BQ�BM�BWsB]dBR BOBV�BK^B_;BVB[�B_�B^jB[WB[�BY�BZ�BYKBX�BY�BQ�B[�BT�Bo B^5Be,Bh�Bi�Bg�BiDBiBh�Bk�BjBjKBkBkQBkBj�BkQBh�Bi�Bh
BiyBiyBh�Bh�Bh>Bh>Bh
BjKBh�BkBi�BiBlWBi�BdZBm�BffBi�Bg�Be�Bn�BffBd�Bl�Bj�BhsBiyBi�Bh
BkBi�Bk�Br�BkQBm�Bk�Bl"BkQBk�Bm)Bl�BlWBl�BlWBl"BlWBm�Bm�Bl�Bm�Bm�Bn/Bn�Bp�Bm�Bm�Bn/Bm�Bm)Bo Bn/BncBr�Bs�Bs�BtTBv�Bt�BuZBw�BxBxByrB{�ByrB|�BzB�PB��B�7B�\B��B��B�B��B�FB�B�@B�RB�RB��B��B��B�tB��B�LB�LB�B��B�B��B��B��B�$B��B��B��B�*B��B�B�qB�kB�B�B�qB��B��B��B�'B��B�!B��B�OB�OB��B��B�}B�B�}B�IB��B��B�wB��B�wB��B�=B��B�[B�!B�3B��B�$B�B�XB��B��B��B��B�B��B��B�B��B�B��B�B��B��B��B��B��B�B��B��B�B�LB��B�zB�B�B��B��B��B�tB��B�?B��B�zB��B��B��B�B�zB�hB��B�hB�B��B��B�[B�'B�[B��B��B�?B��B��B��B��B�[B��B��B��B��B��B��B��B��B��B��B��B��B�[B�[B�'B��B��B��B��B��B��B�?B��B��B�B�hB�hB�3B��B�3B��B��B��B�3B��B�B��B��B�B�3B��B��B�aB��B�aB��B�aB�3B�B��B�B��B��B�B�B�hB�aB�-B�-B��B��B��B�3B�hB�B�nB�aB��B��B��B��B��B��B�aB��B��B��B��B��B�hB�tB��B�OB��B��B��B�UB�OB�OB��B��B�B�CB��B�=B��B�OB�}B�!B��B�UB��B�qB�kB��B��B�B�=B��B�OB�=B��B�zB�zB��B��B�tB��B�B��B��B��B�\B��B��B�!B��B�7B��B�IB��B�1B�kB��B�CB�$B�$B�B��B��B�B�{B�YB�SB�B��B��B�@B��B�:B�{B�B�uB�:B��B�B�hB�B�CB��B��B�B��B��B�+B��B�	B��B��B��B�qB��B�eB��B��B�+B��B��B��B�$B�SB��B�$B�_B��B�1B��B�eB�_B�YB�+B��B�+B��B�IB�CB��B��B�kB�7B�B��B�_B�_B��B�+B��B��B��B��B��B�_B��B��B��B��B��B��B�1B��B�+B��B�kB�eB�eB�YB��B��B�YB��B��B�B�B�uB�MB��B��B�+B��B�B�FB��B�$B�YB��B�eB�B�B�B�FB�B��B��B�4B�(B��B�\B�"B��B��B�"B��B�lB��B��B�B��B��B��B|PB{�B.B��B|�B{BzDBy	Bw2BrGBtB{Bw�Bo�Bg�BkQBf�BiBf2Bg�Bd&Bg8Bd�Bd&BdZBe�Bf2Bg�Bg�Bh�Bf�Bh
BgBffBgBa|B]/B[WB\�B`BB]�BXyBb�BL�BK)BLdBA�BB�B>�B?�B>wB9�B7�B.B49B3hB%FB-�B&�B"�B \BVB~B�BOB#�B�BB%B�B�BSB �B�B��B�TB��B�B��B�B��B��B֡B��B��B��B�B��B�HB�TB��B�TB�B�<B҉B�B�B�6B��B�B��B�6B�jB��BרB�B�'B�gB��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                          BP�BP�BP[BQBSfBP�BP�BPyBP�BTUBT�BTDBY�BZ�B`dBi�BjDBiyBi�Bi�Bj1Bl�Bm.BnBn�Bq9BwB��B��B�B��B�B�DB��B��B��B�iB�iB�?B�{B�B�GB�{B��B�(B�NB��B��B�FB��B��B��B�MB�B��B��B��B�WB�wB�gB�AB�DB��B�fB�5B��B�CB��B��B�B��B��B��B��B��B�jB�4B��B��B�lB��B�^B��B�lB��B�zB��B��B�FBy�Bh�Be�BjwBh�B`BF�B6�B(B"�BIB��B��BԓB�cB�xB�%B�	B��B��B�'B}`BZ�BQ�BM�BD�B9�B1�B�B�B4B
�B
�mB
�hB
��B
�
B
̦B
�MB
��B
��B
|DB
t�B
_B
9�B
4�B
4�B
�B
�B	��B	�B	�B	�&B	�B	��B	�GB	�UB	�LB	��B	yVB	p�B	\�B	TUB	VUB	I�B	F�B	I�B	8�B	+gB	$�B	�B	mB	�B	#B	�B	 �B�B��B��B��B��B�{B�8B��B�KB��B��B�/B��B��B�1B��B��B��B��B�SB��B��B�SB|�B{aBv�Bz�B{[B��B�cB�jB�B��B��B}�B}2B|�B��Bz0B}XBzBvoByBv=Bs�Bq�BpBpBr2Bm�Bm�BpCBm�Bn�BsjBu1Bm)BmBi�BgcBd�Be`BkBdzBe�Bh�Bc{B`�B`�Bb�Bb�Ba�BaXB`�B^�B^�B`�BUBU�BQ�BN�BS�BX+BZ�B\B^�BWvBSuBRBQ�BR'BW{BZZBWmBW�B\�BbBb1B`IB`�Ba�BaXBa7BchBd�BeBi!BmtBn�Br�BrHBt�Bw�B}	B~�B�B�.B��B�&B�dB�B�3B��B��B��B��B�B��B��B��B�cB��B��B��B��B�fB�B�B�LBB��B��B��B�GB�1BıB�VB��B�jB��B��B�+BˠBȡB�CB�hB�KB��B�B��BɪB�bB��BǹB��B�\B�uB�BBϸBϱB�,B��B�gB�BB�uB��B�B�EB�)B�KB��B	 /B	B	=B	
/B	�B	�B	�B	�B	�B	�B	$KB	&B	'�B	*�B	.�B	.AB	-�B	.B	.*B	/�B	5~B	>B	A~B	D B	H�B	I=B	J�B	K�B	OAB	PDB	T B	V�B	V�B	W�B	Z�B	[�B	Z�B	]�B	b�B	eJB	h�B	k'B	lTB	n<B	m�B	n�B	r�B	uXB	vrB	wB	v�B	wB	v�B	u�B	u�B	w�B	{�B	|�B	|�B	}&B	}�B	�B	�B	�uB	��B	��B	��B	��B	�B	��B	�>B	��B	�3B	�!B	�uB	��B	��B	��B	��B	�6B	��B	��B	��B	��B	�B	�NB	�
B	��B	��B	�B	�MB	��B	��B	��B	�kB	��B	��B	�GB	�B	�B	�[B	�<B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�"B	�B	�`B	��B	��B	�B	��B	��B	��B	�'B	��B	�[B	ѓB	�BB	��B	ԢB	�B	��B	֕B	ױB	��B	ވB	��B	�B	�jB	�0B	�BB	��B	�EB	��B	��B	��B	�B	��B	�B	�8B	�wB	�B	�B	��B	�B	�DB	��B	�uB	��B	��B	��B	�B	�4B	�B	�B	��B	�UB	�mB	��B	��B	��B	�$B	�B	�B	�B	�B	��B	��B	��B	��B	�{B	��B	�:B	�!B	��B	�B	�`B	�BB	�B	�VB	�%B	�4B	�mB	��B	�\B	��B	��B	��B	�B	�DB	�B
 �B
 �B
 �B
MB
�B
�B
4B
�B
�B
oB
;B
�B
	B
�B
	rB

JB

�B

�B

�B
,B
eB
�B
B
�B
�B
�B
B
!B
jB
�B
�B
5B
LB
�B
�B
lB
>B
�B
+B
B
B
#B
LB
�B
�B
{B
WB
JB
QB
�B
�B
�B
�B
�B
aB
�B
�B
�B
�B
�B
�B
�B
)B
kB
zB
ZB
IB
;B
cB
WB
oB
yB
�B
�B
\B
�B
oB
�B
�B
 zB
 �B
!B
!�B
!�B
"pB
"�B
#�B
$B
$�B
$�B
$�B
%�B
%ZB
%�B
&B
& B
&�B
&_B
&bB
&�B
'MB
(yB
)WB
)B
)-B
)�B
)�B
*8B
+?B
+PB
,NB
,�B
,�B
,�B
."B
.�B
-mB
-B
-aB
-�B
.�B
/2B
/�B
/�B
0%B
0WB
0�B
1-B
1B
1BB
1\B
1�B
2FB
2�B
39B
3eB
3AB
3SB
3�B
3�B
4�B
4�B
5RB
5TB
6	B
67B
6|B
6�B
6�B
7B
7qB
7tB
7�B
7�B
8vB
:$B
:�B
:1B
:2B
:B
:�B
<FB
<lB
<�B
<�B
=lB
=�B
=�B
=�B
=�B
=�B
>"B
>�B
?lB
?yB
?�B
@'B
AB
AQB
B,B
B�B
B�B
B�B
CB
CkB
DB
D	B
DrB
D�B
D�B
D�B
D�B
EB
E/B
E[B
FB
E�B
FB
F4B
FQB
F�B
F�B
G�B
G�B
G�B
GrB
G�B
H*B
HLB
H�B
H�B
IyB
I�B
JB
I�B
J(G�O�BQBQBPBQ�BPBP�BO�BPHBP�BPBP�BO�BM�BM�BPHBPHBOBK�BY�BS�BM�BT,BOBBO�BQ�BM�BN�BQNBO�BL�BPHBP}BQ�BOBBP�BPHBO�BRTBQNBPHBP}BV9BU�BP�BR�BN�BT�BQ�BM�BWsB]dBR BOBV�BK^B_;BVB[�B_�B^jB[WB[�BY�BZ�BYKBX�BY�BQ�B[�BT�Bo B^5Be,Bh�Bi�Bg�BiDBiBh�Bk�BjBjKBkBkQBkBj�BkQBh�Bi�Bh
BiyBiyBh�Bh�Bh>Bh>Bh
BjKBh�BkBi�BiBlWBi�BdZBm�BffBi�Bg�Be�Bn�BffBd�Bl�Bj�BhsBiyBi�Bh
BkBi�Bk�Br�BkQBm�Bk�Bl"BkQBk�Bm)Bl�BlWBl�BlWBl"BlWBm�Bm�Bl�Bm�Bm�Bn/Bn�Bp�Bm�Bm�Bn/Bm�Bm)Bo Bn/BncBr�Bs�Bs�BtTBv�Bt�BuZBw�BxBxByrB{�ByrB|�BzB�PB��B�7B�\B��B��B�B��B�FB�B�@B�RB�RB��B��B��B�tB��B�LB�LB�B��B�B��B��B��B�$B��B��B��B�*B��B�B�qB�kB�B�B�qB��B��B��B�'B��B�!B��B�OB�OB��B��B�}B�B�}B�IB��B��B�wB��B�wB��B�=B��B�[B�!B�3B��B�$B�B�XB��B��B��B��B�B��B��B�B��B�B��B�B��B��B��B��B��B�B��B��B�B�LB��B�zB�B�B��B��B��B�tB��B�?B��B�zB��B��B��B�B�zB�hB��B�hB�B��B��B�[B�'B�[B��B��B�?B��B��B��B��B�[B��B��B��B��B��B��B��B��B��B��B��B��B�[B�[B�'B��B��B��B��B��B��B�?B��B��B�B�hB�hB�3B��B�3B��B��B��B�3B��B�B��B��B�B�3B��B��B�aB��B�aB��B�aB�3B�B��B�B��B��B�B�B�hB�aB�-B�-B��B��B��B�3B�hB�B�nB�aB��B��B��B��B��B��B�aB��B��B��B��B��B�hB�tB��B�OB��B��B��B�UB�OB�OB��B��B�B�CB��B�=B��B�OB�}B�!B��B�UB��B�qB�kB��B��B�B�=B��B�OB�=B��B�zB�zB��B��B�tB��B�B��B��B��B�\B��B��B�!B��B�7B��B�IB��B�1B�kB��B�CB�$B�$B�B��B��B�B�{B�YB�SB�B��B��B�@B��B�:B�{B�B�uB�:B��B�B�hB�B�CB��B��B�B��B��B�+B��B�	B��B��B��B�qB��B�eB��B��B�+B��B��B��B�$B�SB��B�$B�_B��B�1B��B�eB�_B�YB�+B��B�+B��B�IB�CB��B��B�kB�7B�B��B�_B�_B��B�+B��B��B��B��B��B�_B��B��B��B��B��B��B�1B��B�+B��B�kB�eB�eB�YB��B��B�YB��B��B�B�B�uB�MB��B��B�+B��B�B�FB��B�$B�YB��B�eB�B�B�B�FB�B��B��B�4B�(B��B�\B�"B��B��B�"B��B�lB��B��B�B��B��B��B|PB{�B.B��B|�B{BzDBy	Bw2BrGBtB{Bw�Bo�Bg�BkQBf�BiBf2Bg�Bd&Bg8Bd�Bd&BdZBe�Bf2Bg�Bg�Bh�Bf�Bh
BgBffBgBa|B]/B[WB\�B`BB]�BXyBb�BL�BK)BLdBA�BB�B>�B?�B>wB9�B7�B.B49B3hB%FB-�B&�B"�B \BVB~B�BOB#�B�BB%B�B�BSB �B�B��B�TB��B�B��B�B��B��B֡B��B��B��B�B��B�HB�TB��B�TB�B�<B҉B�B�B�6B��B�B��B�6B�jB��BרB�B�'B�gB��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<CW<+Բ<#�
<#�
<#�
<#�
<3!q<#�
<#�
<#�
<#�
<��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<6�<* �<#�
<#�
<#�
<#�
<#�
<#�
<Wdh<P�#<@l�<#�
<A�v<,�=<#�
<#�
<4�p<#�
<6�	<=:�<#�
<#�
<W{3<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<KX�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202307201649482023072016494820230720164948202307201649482023072016494820230720164948SI  SI  ARFMARFM                                                                                                                                                2019040406191720190404061917IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019062403004220190624030042QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019062403004220190624030042QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020060109060620200601090606IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023072016494920230720164949IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023072016494920230720164949IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023072016494920230720164949IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                