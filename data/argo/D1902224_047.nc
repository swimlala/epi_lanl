CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       $Woods Hole Oceanographic Institution   source        
Argo float     history       92021-03-01T20:16:14Z creation; 2023-02-16T18:13:52Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         gPRIMARY | https://orcid.org/0000-0001-6630-1293 | Sachiko Yoshida, Woods Hole Oceanographic Institution       @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7`   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7p   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7t   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7x   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  88   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  8�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9    DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9$   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9(   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9h   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9p   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9t   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  9�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :4   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :<   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :L   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :P   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :`   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :p   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        <�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    <�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    <�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    <�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  <�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʰ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ҕ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � A   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210301201614  20230216131352  1902224 1902224 US ARGO PROJECT                                                 US ARGO PROJECT                                                 BRECK OWENS, STEVEN JAYNE, P.E. ROBBINS                         BRECK OWENS, STEVEN JAYNE, P.E. ROBBINS                         PRES            TEMP            PSAL            PRES            TEMP            PSAL               /   /AA  AOAO7994                            7994                            2C  2C  DD  S2A                             S2A                             7533                            7533                            SBE602 15Aug17 ARM V2.4         SBE602 15Aug17 ARM V2.4         854 854 @�"m����@�"m����11  @�"m�}>�@�"m�}>��DG�7܇�DG�7܇@D�+J@D�+J11  GPS     GPS     Primary sampling: averaged [nominal 2 dbar binned data sampled at 0.5 Hz from a SBE41CP]                                                                                                                                                                        Near-surface sampling: discrete, pumped [data sampled at 1.0Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?k�?��@@  @��\@�  @�p�@޸RA   A  A#33A@  A`  A\)A��A�  A�  A�Q�A�  A�  A�  B   B�B  B  B�B(  B0Q�B8Q�B@  BG�
BP(�BX(�B`(�Bh  Bo�
Bw�
B�  B�  B�  B�  B��B��B��B��B�{B�(�B�  B��B�  B�  B��B�{B��B��
B��B��B��B�  B�{B�  B��B�  B��B��B��
B��
B�  B�{C 
=C  C  C  C��C	��C  C
=C
=C
=C
=C
=C��C��C  C  C 
=C"
=C#��C&  C(  C)��C+��C.  C0  C2
=C4
=C6  C7��C9��C<  C=��C?��CB  CD  CE��CG��CI��CL  CN  CP  CQ��CS�CU��CX
=CZ  C\  C^
=C`  Cb  Cd  Cf  Cg��Ci��Cl
=Cn
=Cp
=Cr  Cs��Cu��Cx  Cy��C|  C~  C�  C���C���C�  C���C���C�C�  C���C���C���C���C���C�C���C���C�C�C�  C���C�  C�  C�  C�  C���C���C���C���C���C�  C�  C���C���C���C�  C�  C�  C�C���C���C�  C�  C�C�  C���C���C�  C�  C�  C�  C�  C�  C���C���C�  C���C���C�  C���C�  C�C�  C�  C�
=C�  C���C���C�  C���C���C�  C���C���C�C�C���C���C�C�C���C���C���C���C�C�C�  C�  C�  C�  C�  C�C���C�  C�C�  C�  C�  C���C���C�  C�C�  C���C���C���C���C�  C�C���C���C�  C�  C���C�  C�  C�  C�  C���C���C���C���C���C���C�C�C�  C�  C�  D �D � D �qD}qD  D� D�D� D��D� D  D� D  D� D�qD� D�qD}qD�qD	z�D
  D
� D
�qD� D�D� D  D� D�qD� D�D}qD  D� D  D� D  D}qD�qD� DD��D�qD}qD  D� D�qDz�D��Dz�D�qD� D�D� D  D� D�D��D�qD}qD  D��D  D}qD�qD z�D!  D!� D!�qD"}qD"�qD#� D$�D$��D%�D%� D&  D&}qD&�qD'��D(�D(� D(�qD)}qD*  D*� D*�qD+z�D+��D,}qD,�qD-}qD.  D.� D/�D/� D/��D0z�D0��D1� D2D2� D2�qD3}qD3��D4}qD5  D5� D5�qD6� D7  D7}qD8  D8� D9  D9� D:�D:� D:��D;}qD<�D<}qD<��D=z�D=�qD>��D?�D?� D?��D@z�D@�qDA��DB�DB}qDC  DC� DC�qDD� DE  DE� DF�DF�DG  DG}qDG�qDH� DI  DI� DI�qDJ� DK  DK� DK�qDL� DM  DM� DN  DN}qDN�qDO}qDO�qDP� DQDQ��DR  DR}qDR�qDS}qDS�qDTz�DT�qDU� DV  DV� DV�qDW}qDX  DX� DY  DY}qDY�qDZ� D[D[��D[�qD\� D]�D]� D^�D^� D^��D_� D`  D`� Da�Da� Db  Db�Dc�Dc� Dd  Dd� De  De� Df�Df��DgDg� Dh  Dh� Di  Di� Dj  Dj��Dk  Dk� Dk�qDl}qDl�qDm��Dn�Dn� Do  Do��Dp  Dp� Dq�Dq� Dq�qDr}qDs  Ds��Dt  Dt� Du  Du� Du��Dv}qDw  Dw}qDx  Dx� Dy  Dy� Dz  Dz��D{�D{� D|  D|� D|�qD}��D~�D~}qD~��D� D�  D�AHD�� D�� D�  D�AHD�� D�� D�  D�@ D�~�D�� D�  D�AHD��HD��HD�  D�@ D�� D��HD�  D�@ D��HD��HD�HD�AHD�� D��HD�HD�@ D�� D�� D���D�@ D�� D�� D�HD�AHD��HD��HD�  D�AHD�� D��HD���D�>�D�� D���D���D�>�D�~�D�� D���D�@ D��HD���D�  D�B�D��HD�� D�HD�AHD�� D�� D�  D�AHD�~�D�� D�  D�@ D��HD�� D���D�>�D�� D�� D���D�@ D�~�D���D�  D�@ D�~�D���D�  D�>�D�}qD���D���D�>�D��HD�� D�  D�@ D�~�D���D�  D�AHD�� D���D�  D�AHD��HD�� D���D�@ D��HD�D�  D�=qD�� D��HD�HD�@ D��HD�� D���D�@ D��HD�� D���D�>�D�� D��HD�HD�@ D��HD�D�HD�@ D�� D��HD�  D�@ D��HD�� D�  D�>�D�� D��HD��D�AHD�~�D���D�  D�@ D�� D�� D�HD�AHD��HD��HD�HD�>�D�� D��HD�HD�@ D�� D�� D���D�>�D�~�D���D�  D�B�D���D�� D�  D�AHD��HD�� D�  D�>�D�}qD���D�  D�@ D�~�D���D��qD�>�D�� D���D���D�AHD��HD���D���D�AHD�~�D���D�  D�AHD��HD�� D�  D�>�D�}qD��qD���D�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D�� D�HD�AHD��HD�� D���D�@ D�� D��HD�  D�@ D�� D��HD�  D�>�D�~�D���D�HD�B�D�� D���D���D�@ D�� D�� D�  D�>�D�� D��HD�HD�@ D�~�D���D���D�>�D�� D��HD�  D�@ D�~�D�� D�  D�>�D�~�D��HD�HD�AHD�~�D���D�  D�>�D�}qD¾�D�  D�@ DÀ D��HD�HD�AHD�~�Dľ�D���D�>�D�~�D�� D�  D�@ D�~�Dƾ�D�  D�>�Dǀ D�� D�  D�@ DȀ DȾ�D�  D�AHDɀ D�� D�  D�@ Dʀ Dʾ�D�  D�AHDˀ D˾�D���D�=qD�}qD̾�D�  D�AHD́HD��HD�HD�@ D�~�D�� D��D�@ Dπ DϾ�D�  D�@ D�~�DнqD��qD�>�Dр D��HD�  D�@ DҀ DҾ�D���D�AHDӀ DӾ�D�  D�@ D�~�D�� D��qD�@ DՁHD�� D�  D�@ D�~�D�� D�  D�>�D׀ D׾�D���D�=qD؀ D��HD�  D�>�D�~�D�� D��qD�>�Dڂ�D�� D�  D�AHDۀ D۾�D���D�@ D�~�DܽqD�  D�AHD݀ Dݾ�D�  D�@ Dހ D޾�D���D�>�D�~�D�� D�HD�@ D�� D��HD�  D�>�D� D�� D���D�@ D� D�� D�  D�>�D�~�D�� D�  D�@ D� D侸D���D�=qD�~�D�� D�  D�AHD� D澸D���D�@ D�}qD�qD���D�>�D� D�� D���D�>�D� D龸D�HD�@ D�~�D�qD��qD�@ D�HD�� D���D�>�D�~�D�� D�  D�>�D�~�D�� D���D�>�D� D��HD�HD�@ D� D�� D�  D�@ D�� D�� D���D�>�D�~�D�D�  D�>�D�~�D�D���D�@ D� D�D���D�>�D�~�D�� D�HD�AHD��HD�� D���D�>�D�� D��HD�  D�>�D�~�D���D�  D�AHD��HD��HD�  D�@ D�~�D�� D�  D�>�D�� D�� D�D�*=?\)?.{?W
=?u?�=q?�z�?���?\?���?�
=?�@   @�@
=q@z�@!G�@(��@+�@333@=p�@J=q@O\)@W
=@^�R@h��@s33@xQ�@}p�@��
@���@���@�\)@�z�@��H@��R@�G�@��@��@���@��@�@�(�@�  @��
@�ff@˅@У�@�z�@�
=@ٙ�@�  @��@�@�=q@�{@�33@�
=@���@�p�A ��A�
A�AffAQ�A�A{A�RA��A�\A�A�AQ�A=qA��A\)A!�A#33A%�A(Q�A*=qA+�A-p�A0��A2�\A4z�A6ffA8Q�A;�A>�RA@  AA�ADz�AG
=AH��AJ=qAL��AO\)AQ�AS�
AUAXQ�A[�A]p�A^{A`��Ac33AeAhQ�Ai��Ak�An{Ap��Ar�\Atz�AvffAy��A|(�A|��A\)A���A�=qA��HA��A���A�ffA��A�  A���A�=qA��A���A�p�A�ffA�  A�G�A���A��\A�(�A�p�A�ffA�\)A�Q�A��A��HA��
A�z�A�{A�\)A�  A���A�=qA��A�(�A��A�{A��A�Q�A���A��A��A�z�A�p�A�ffA�\)A���A��A��\A��
A��A�{A��RA�  A�G�A��\A��A�(�A�A��RA��A�Q�A���A�33A��
A���A�A�\)A�Q�A���A��A˅A�z�A�p�A�{A�\)AУ�Aљ�A�=qAӅA��A�{AָRA׮A�G�Aڏ\A�33A��
A��A޸RA߮A�Q�A�G�A��HA��
A���A�p�A�RA�  A�G�A陚A��HA�z�A�p�A�{A�
=A��A�A�=qA�33A�z�A�{A�
=A��A���A�=qA��A�(�A��A�ffA��B Q�B ��BG�B�B�\B�HB\)B  B��B��Bp�B{B�RB33B�B(�B��B	p�B	B
=qB
�HB�B  Bz�B��BB=qB�\B
=B�
BQ�B��BG�BB�RB
=B\)B  B��BG�B��B{B�RB\)B�
B(�B��Bp�B{BffB�HB\)B(�B��B��Bp�B{B�HB33B�B (�B ��B!p�B!B"=qB"�HB#�B$(�B$z�B$��B%B&ffB&�RB'33B'�B(z�B(��B)G�B)B*ffB+33B+�B,  B,z�B-�B-B.=qB.�\B/33B0  B0z�B0��B1G�B1�B2�\B333B3\)B4  B4��B5G�B5B6{B6�\B7\)B8  B8Q�B8��B9G�B:{B:�\B:�HB;\)B<  B<��B=�B=p�B=�B>�\B?\)B?�B@(�B@��BAp�BA�BBffBB�RBC\)BD  BDz�BD��BEG�BE�BF�RBG
=BG\)BH  BH��BIG�BI��BJ{BJ�RBK\)BK�
BL(�BL��BMG�BN{BNffBN�RBO\)BO�
BP��BP��BQG�BQ�BR�\BS33BS\)BT  BTz�BUG�BU��BU�BV�\BW33BW�BX  BXz�BY�BY�BZ=qBZ�\B[33B\  B\z�B\��B]G�B]�B^�\B_
=B_\)B`  B`��BaG�Ba��Bb{Bb�HBc�Bc�
BdQ�Be�BeBf=qBf�\Bg33Bg�
Bhz�Bh��Bip�Bj=qBj�RBk
=Bk�BlQ�Bl��Bmp�BmBnffBo
=Bo�Bp  Bp��BqG�BqBr{Br�RBs�Bt  BtQ�Bt��Bu��Bv=qBv�RBw33Bw�Bxz�Bx��ByG�By�Bz�RB{33B{�B|(�B|��B}p�B}�B~=qB~�HB�B�{B�=qB��\B��HB�33B�\)B���B�  B�Q�B��\B��RB�
=B�\)B��B��
B�{B�z�B���B�
=B�33B��B��B�(�B�Q�B��\B���B�G�B��B��B��B�Q�B���B���B�
=B�\)B�B�  B�(�B�z�B���B��B�G�B��B��B�=qB�ffB���B��HB�G�B���B�B�  B�Q�B���B��HB�
=B�\)B�B�  B�=qB�z�B���B��B�\)B��B��
B�=qB��\B��RB���B�G�B���B��
B�  B�Q�B���B�
=B�33B�\)B��B�{B�Q�B�z�B��RB��B�p�B���B��
B�(�B�z�B��RB��HB�33B���B��
B�  B�=qB���B��HB��B�\)B��B�  B�(�B�ffB���B��B�G�B��B��
B�=qB�ffB��\B���B�G�B��B��B�  B�ffB���B���B��B��B�B��B�=qB��\B���B���B�G�B��B��
B�{B�ffB��RB��HB��B��B��
B�  B�=qB��\B��HB�
=B�G�B��B��B�(�B�ffB���B�
=B�33B��B��
B�(�B�Q�B��\B���B�G�B�\)B�B�{B�Q�B��\B��HB�G�B�p�B��B�  B�ffB��\B���B��B��B�B�  B�Q�B���B��HB��B��B�B��B�Q�B���B��HB��B��B�B�  B�=qB��\B���B��B�\)B��B�{B�Q�B�z�B���B�33B�p�B���B��B�Q�B��\B���B�
=B�\)B��B��
B�(�B�z�B��HB�
=B�G�B���B�  B�=qB�ffB��RB��B�\)B���B��
B�=qB��\B��RB�
=B�\)B��B��B�{B��\B���B�
=B�G�B��B�  B�(�B�ffB¸RB��B�\)BÅB��
B�=qB�z�Bģ�B���B�\)Bř�B�B�(�B�z�Bƣ�B���B�G�BǙ�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                ?k�?��@@  @��\@�  @�p�@޸RA   A  A#33A@  A`  A\)A��A�  A�  A�Q�A�  A�  A�  B   B�B  B  B�B(  B0Q�B8Q�B@  BG�
BP(�BX(�B`(�Bh  Bo�
Bw�
B�  B�  B�  B�  B��B��B��B��B�{B�(�B�  B��B�  B�  B��B�{B��B��
B��B��B��B�  B�{B�  B��B�  B��B��B��
B��
B�  B�{C 
=C  C  C  C��C	��C  C
=C
=C
=C
=C
=C��C��C  C  C 
=C"
=C#��C&  C(  C)��C+��C.  C0  C2
=C4
=C6  C7��C9��C<  C=��C?��CB  CD  CE��CG��CI��CL  CN  CP  CQ��CS�CU��CX
=CZ  C\  C^
=C`  Cb  Cd  Cf  Cg��Ci��Cl
=Cn
=Cp
=Cr  Cs��Cu��Cx  Cy��C|  C~  C�  C���C���C�  C���C���C�C�  C���C���C���C���C���C�C���C���C�C�C�  C���C�  C�  C�  C�  C���C���C���C���C���C�  C�  C���C���C���C�  C�  C�  C�C���C���C�  C�  C�C�  C���C���C�  C�  C�  C�  C�  C�  C���C���C�  C���C���C�  C���C�  C�C�  C�  C�
=C�  C���C���C�  C���C���C�  C���C���C�C�C���C���C�C�C���C���C���C���C�C�C�  C�  C�  C�  C�  C�C���C�  C�C�  C�  C�  C���C���C�  C�C�  C���C���C���C���C�  C�C���C���C�  C�  C���C�  C�  C�  C�  C���C���C���C���C���C���C�C�C�  C�  C�  D �D � D �qD}qD  D� D�D� D��D� D  D� D  D� D�qD� D�qD}qD�qD	z�D
  D
� D
�qD� D�D� D  D� D�qD� D�D}qD  D� D  D� D  D}qD�qD� DD��D�qD}qD  D� D�qDz�D��Dz�D�qD� D�D� D  D� D�D��D�qD}qD  D��D  D}qD�qD z�D!  D!� D!�qD"}qD"�qD#� D$�D$��D%�D%� D&  D&}qD&�qD'��D(�D(� D(�qD)}qD*  D*� D*�qD+z�D+��D,}qD,�qD-}qD.  D.� D/�D/� D/��D0z�D0��D1� D2D2� D2�qD3}qD3��D4}qD5  D5� D5�qD6� D7  D7}qD8  D8� D9  D9� D:�D:� D:��D;}qD<�D<}qD<��D=z�D=�qD>��D?�D?� D?��D@z�D@�qDA��DB�DB}qDC  DC� DC�qDD� DE  DE� DF�DF�DG  DG}qDG�qDH� DI  DI� DI�qDJ� DK  DK� DK�qDL� DM  DM� DN  DN}qDN�qDO}qDO�qDP� DQDQ��DR  DR}qDR�qDS}qDS�qDTz�DT�qDU� DV  DV� DV�qDW}qDX  DX� DY  DY}qDY�qDZ� D[D[��D[�qD\� D]�D]� D^�D^� D^��D_� D`  D`� Da�Da� Db  Db�Dc�Dc� Dd  Dd� De  De� Df�Df��DgDg� Dh  Dh� Di  Di� Dj  Dj��Dk  Dk� Dk�qDl}qDl�qDm��Dn�Dn� Do  Do��Dp  Dp� Dq�Dq� Dq�qDr}qDs  Ds��Dt  Dt� Du  Du� Du��Dv}qDw  Dw}qDx  Dx� Dy  Dy� Dz  Dz��D{�D{� D|  D|� D|�qD}��D~�D~}qD~��D� D�  D�AHD�� D�� D�  D�AHD�� D�� D�  D�@ D�~�D�� D�  D�AHD��HD��HD�  D�@ D�� D��HD�  D�@ D��HD��HD�HD�AHD�� D��HD�HD�@ D�� D�� D���D�@ D�� D�� D�HD�AHD��HD��HD�  D�AHD�� D��HD���D�>�D�� D���D���D�>�D�~�D�� D���D�@ D��HD���D�  D�B�D��HD�� D�HD�AHD�� D�� D�  D�AHD�~�D�� D�  D�@ D��HD�� D���D�>�D�� D�� D���D�@ D�~�D���D�  D�@ D�~�D���D�  D�>�D�}qD���D���D�>�D��HD�� D�  D�@ D�~�D���D�  D�AHD�� D���D�  D�AHD��HD�� D���D�@ D��HD�D�  D�=qD�� D��HD�HD�@ D��HD�� D���D�@ D��HD�� D���D�>�D�� D��HD�HD�@ D��HD�D�HD�@ D�� D��HD�  D�@ D��HD�� D�  D�>�D�� D��HD��D�AHD�~�D���D�  D�@ D�� D�� D�HD�AHD��HD��HD�HD�>�D�� D��HD�HD�@ D�� D�� D���D�>�D�~�D���D�  D�B�D���D�� D�  D�AHD��HD�� D�  D�>�D�}qD���D�  D�@ D�~�D���D��qD�>�D�� D���D���D�AHD��HD���D���D�AHD�~�D���D�  D�AHD��HD�� D�  D�>�D�}qD��qD���D�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D�� D�HD�AHD��HD�� D���D�@ D�� D��HD�  D�@ D�� D��HD�  D�>�D�~�D���D�HD�B�D�� D���D���D�@ D�� D�� D�  D�>�D�� D��HD�HD�@ D�~�D���D���D�>�D�� D��HD�  D�@ D�~�D�� D�  D�>�D�~�D��HD�HD�AHD�~�D���D�  D�>�D�}qD¾�D�  D�@ DÀ D��HD�HD�AHD�~�Dľ�D���D�>�D�~�D�� D�  D�@ D�~�Dƾ�D�  D�>�Dǀ D�� D�  D�@ DȀ DȾ�D�  D�AHDɀ D�� D�  D�@ Dʀ Dʾ�D�  D�AHDˀ D˾�D���D�=qD�}qD̾�D�  D�AHD́HD��HD�HD�@ D�~�D�� D��D�@ Dπ DϾ�D�  D�@ D�~�DнqD��qD�>�Dр D��HD�  D�@ DҀ DҾ�D���D�AHDӀ DӾ�D�  D�@ D�~�D�� D��qD�@ DՁHD�� D�  D�@ D�~�D�� D�  D�>�D׀ D׾�D���D�=qD؀ D��HD�  D�>�D�~�D�� D��qD�>�Dڂ�D�� D�  D�AHDۀ D۾�D���D�@ D�~�DܽqD�  D�AHD݀ Dݾ�D�  D�@ Dހ D޾�D���D�>�D�~�D�� D�HD�@ D�� D��HD�  D�>�D� D�� D���D�@ D� D�� D�  D�>�D�~�D�� D�  D�@ D� D侸D���D�=qD�~�D�� D�  D�AHD� D澸D���D�@ D�}qD�qD���D�>�D� D�� D���D�>�D� D龸D�HD�@ D�~�D�qD��qD�@ D�HD�� D���D�>�D�~�D�� D�  D�>�D�~�D�� D���D�>�D� D��HD�HD�@ D� D�� D�  D�@ D�� D�� D���D�>�D�~�D�D�  D�>�D�~�D�D���D�@ D� D�D���D�>�D�~�D�� D�HD�AHD��HD�� D���D�>�D�� D��HD�  D�>�D�~�D���D�  D�AHD��HD��HD�  D�@ D�~�D�� D�  D�>�D�� D�� D�D�*=?\)?.{?W
=?u?�=q?�z�?���?\?���?�
=?�@   @�@
=q@z�@!G�@(��@+�@333@=p�@J=q@O\)@W
=@^�R@h��@s33@xQ�@}p�@��
@���@���@�\)@�z�@��H@��R@�G�@��@��@���@��@�@�(�@�  @��
@�ff@˅@У�@�z�@�
=@ٙ�@�  @��@�@�=q@�{@�33@�
=@���@�p�A ��A�
A�AffAQ�A�A{A�RA��A�\A�A�AQ�A=qA��A\)A!�A#33A%�A(Q�A*=qA+�A-p�A0��A2�\A4z�A6ffA8Q�A;�A>�RA@  AA�ADz�AG
=AH��AJ=qAL��AO\)AQ�AS�
AUAXQ�A[�A]p�A^{A`��Ac33AeAhQ�Ai��Ak�An{Ap��Ar�\Atz�AvffAy��A|(�A|��A\)A���A�=qA��HA��A���A�ffA��A�  A���A�=qA��A���A�p�A�ffA�  A�G�A���A��\A�(�A�p�A�ffA�\)A�Q�A��A��HA��
A�z�A�{A�\)A�  A���A�=qA��A�(�A��A�{A��A�Q�A���A��A��A�z�A�p�A�ffA�\)A���A��A��\A��
A��A�{A��RA�  A�G�A��\A��A�(�A�A��RA��A�Q�A���A�33A��
A���A�A�\)A�Q�A���A��A˅A�z�A�p�A�{A�\)AУ�Aљ�A�=qAӅA��A�{AָRA׮A�G�Aڏ\A�33A��
A��A޸RA߮A�Q�A�G�A��HA��
A���A�p�A�RA�  A�G�A陚A��HA�z�A�p�A�{A�
=A��A�A�=qA�33A�z�A�{A�
=A��A���A�=qA��A�(�A��A�ffA��B Q�B ��BG�B�B�\B�HB\)B  B��B��Bp�B{B�RB33B�B(�B��B	p�B	B
=qB
�HB�B  Bz�B��BB=qB�\B
=B�
BQ�B��BG�BB�RB
=B\)B  B��BG�B��B{B�RB\)B�
B(�B��Bp�B{BffB�HB\)B(�B��B��Bp�B{B�HB33B�B (�B ��B!p�B!B"=qB"�HB#�B$(�B$z�B$��B%B&ffB&�RB'33B'�B(z�B(��B)G�B)B*ffB+33B+�B,  B,z�B-�B-B.=qB.�\B/33B0  B0z�B0��B1G�B1�B2�\B333B3\)B4  B4��B5G�B5B6{B6�\B7\)B8  B8Q�B8��B9G�B:{B:�\B:�HB;\)B<  B<��B=�B=p�B=�B>�\B?\)B?�B@(�B@��BAp�BA�BBffBB�RBC\)BD  BDz�BD��BEG�BE�BF�RBG
=BG\)BH  BH��BIG�BI��BJ{BJ�RBK\)BK�
BL(�BL��BMG�BN{BNffBN�RBO\)BO�
BP��BP��BQG�BQ�BR�\BS33BS\)BT  BTz�BUG�BU��BU�BV�\BW33BW�BX  BXz�BY�BY�BZ=qBZ�\B[33B\  B\z�B\��B]G�B]�B^�\B_
=B_\)B`  B`��BaG�Ba��Bb{Bb�HBc�Bc�
BdQ�Be�BeBf=qBf�\Bg33Bg�
Bhz�Bh��Bip�Bj=qBj�RBk
=Bk�BlQ�Bl��Bmp�BmBnffBo
=Bo�Bp  Bp��BqG�BqBr{Br�RBs�Bt  BtQ�Bt��Bu��Bv=qBv�RBw33Bw�Bxz�Bx��ByG�By�Bz�RB{33B{�B|(�B|��B}p�B}�B~=qB~�HB�B�{B�=qB��\B��HB�33B�\)B���B�  B�Q�B��\B��RB�
=B�\)B��B��
B�{B�z�B���B�
=B�33B��B��B�(�B�Q�B��\B���B�G�B��B��B��B�Q�B���B���B�
=B�\)B�B�  B�(�B�z�B���B��B�G�B��B��B�=qB�ffB���B��HB�G�B���B�B�  B�Q�B���B��HB�
=B�\)B�B�  B�=qB�z�B���B��B�\)B��B��
B�=qB��\B��RB���B�G�B���B��
B�  B�Q�B���B�
=B�33B�\)B��B�{B�Q�B�z�B��RB��B�p�B���B��
B�(�B�z�B��RB��HB�33B���B��
B�  B�=qB���B��HB��B�\)B��B�  B�(�B�ffB���B��B�G�B��B��
B�=qB�ffB��\B���B�G�B��B��B�  B�ffB���B���B��B��B�B��B�=qB��\B���B���B�G�B��B��
B�{B�ffB��RB��HB��B��B��
B�  B�=qB��\B��HB�
=B�G�B��B��B�(�B�ffB���B�
=B�33B��B��
B�(�B�Q�B��\B���B�G�B�\)B�B�{B�Q�B��\B��HB�G�B�p�B��B�  B�ffB��\B���B��B��B�B�  B�Q�B���B��HB��B��B�B��B�Q�B���B��HB��B��B�B�  B�=qB��\B���B��B�\)B��B�{B�Q�B�z�B���B�33B�p�B���B��B�Q�B��\B���B�
=B�\)B��B��
B�(�B�z�B��HB�
=B�G�B���B�  B�=qB�ffB��RB��B�\)B���B��
B�=qB��\B��RB�
=B�\)B��B��B�{B��\B���B�
=B�G�B��B�  B�(�B�ffB¸RB��B�\)BÅB��
B�=qB�z�Bģ�B���B�\)Bř�B�B�(�B�z�Bƣ�B���B�G�BǙ�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\A�+A�A�A�+A�A�A�A~�A�Az�AE�AA�A=qA$�A��A�A�A�mA�mA�#A��A��A��AƨAA�^A�A�A��A�7Ap�AO�A33A33A&�A�A�A�/A�/A��A��A�`A�A��AȴA�9A��A�uA�\AffAA�A-AbA1A�A�#A�#A�-AA�mA��A�^A��A?}A��A�-AE�AA
=A7LAz�A��AoAĜA��A"�A�;A	�^AĜA	/AbAbNAM�A?}AoA�-A�uAƨA �y@��@��@�33@���@�{@��@��@�@�@޸R@ߍP@�n�@�9X@���@��@��;@��@�z�@�O�@�I�@�~�@�O�@�O�@�R@�G�@�n�@��@�w@��@�X@�@���@�t�@�33@�33@�+@�;d@���@���@���@�5?@�j@�/@�G�@�u@�@���@���@�@��@���@��y@�hs@�7L@�r�@��@�v�@��@�%@�l�@���@��@�9@�b@��@㕁@��@�h@���@�Ĝ@�hs@蛦@���@�hs@��#@��^@���@�h@��@�V@���@�=q@�~�@�!@�!@��@�\)@�@��@�@�w@�F@���@�C�@��@�^5@�v�@�+@�!@柾@�\@�p�@���@�  @�ff@�%@�K�@��@և+@��m@��@�n�@���@�(�@�X@�z�@��y@ޗ�@�$�@���@��@ܼj@���@�=q@�^5@�^5@�n�@ڏ\@��@�dZ@�\)@ە�@�|�@ڗ�@��@�\)@�z�@��;@�(�@��/@��@�V@љ�@�E�@�=q@ѡ�@���@��#@�z�@˅@���@�=q@���@�G�@�r�@ƸR@ċD@�A�@� �@� �@� �@��@���@�t�@��@�5?@���@��@�ff@���@�I�@��@�M�@��-@��@�\)@�~�@�Ĝ@�S�@���@�`B@���@��!@�/@���@���@�bN@��F@��H@�\)@��R@�ff@�$�@���@���@���@�M�@��@��-@��7@�`B@�?}@�Ĝ@�9X@�b@�K�@��+@��T@���@���@�j@��w@�S�@��@���@�@�@���@��@�Ĝ@�9X@��y@��/@��\@�S�@�Z@��9@�b@�|�@���@��@�V@��\@�E�@�ff@���@�v�@��@���@���@�r�@�E�@�x�@�X@���@�%@��^@��R@���@��h@�/@��@��9@��@�|�@�;d@�+@�\)@�|�@�\)@�dZ@��@��#@�`B@�?}@���@�Ĝ@���@���@��@�j@�j@�bN@�I�@��@�ƨ@��
@�  @�(�@�A�@�9X@�b@���@�|�@�dZ@�5?@�p�@�7L@�p�@�hs@���@�P@~�+@�@~��@~ff@~V@~E�@~��@��@��@
=@~�+@}�-@|�j@{��@{"�@{S�@{dZ@{33@{"�@z^5@y�^@y�@w�@w\)@w
=@vV@up�@u?}@t�@r�@q7L@n�@m?}@m?}@l�@l�/@m�h@m�T@m�h@l(�@k��@k�@kt�@kdZ@j�@j�@i�^@ix�@l�@m�h@m�T@o
=@n�@lI�@j~�@k��@k"�@j��@ihs@hbN@i�@fv�@dj@c33@b��@b��@b~�@a�^@ahs@aX@aG�@a7L@a�@`��@`Ĝ@`bN@_\)@_�@`r�@a�7@a��@a�@b�@b=q@c@a�@a7L@a�@ax�@`�u@_�@^��@^v�@^V@^5?@^{@]�T@]��@]�h@]p�@]O�@\�@[@ZJ@Y��@Yx�@Yhs@Y&�@X��@X1'@W�@Xb@X  @X  @W�@Xb@Xb@Xb@X  @W�;@W�;@W�;@W��@W��@W��@W�w@W�P@W��@W�@W��@W|�@W|�@W|�@W|�@W\)@W
=@V��@V�y@V�R@V��@Vff@VV@VE�@V$�@U�@U�-@UO�@UV@U�@T�@T(�@S��@SS�@SS�@SS�@S"�@Rn�@R-@Q�#@Q��@Q�7@Qx�@QX@Q&�@P��@P�@P�u@PbN@PA�@P�@P �@N5?@L�@K�
@KdZ@Ko@J��@J=q@I�@I�@I�^@Ix�@IG�@I7L@I&�@I�@I&�@I&�@H��@HĜ@H��@H�u@H�@H�u@H��@H��@H�@HQ�@HA�@HA�@HA�@HA�@HQ�@HA�@HA�@HA�@H  @G�@G�;@G��@G��@F��@Fv�@FV@FE�@FE�@FE�@Gl�@H�@Ix�@J�\@JM�@JJ@I�@I��@Ihs@I7L@I&�@I�@H�`@HA�@H �@G�@G��@G|�@F�R@E�-@E�@E�@E�@E�@E�@EV@D��@D�D@Dj@DI�@D1@Co@B�H@B��@B�\@B^5@B=q@B=q@B�@BJ@BJ@A��@A�@A��@A�7@Ax�@Ax�@Ax�@Ahs@AG�@AX@AX@A7L@@��@A�@A%@@Ĝ@@�u@@r�@@�@@  @?�w@?;d@?�@>��@>�y@>�R@>��@>��@>��@>�+@>v�@>ff@>E�@>5?@>$�@=�@=��@=�-@=p�@=?}@<�/@<�@<z�@<z�@<j@<j@<j@<I�@<9X@<9X@<9X@<(�@<1@;��@<1@;��@;�m@;�
@;ƨ@;�F@;��@;��@;��@;��@;��@;�@;t�@;C�@;o@;@:�H@:�!@:��@:�\@:�\@:n�@:M�@:=q@9��@9��@9x�@9X@97L@9&�@9%@8��@8�`@8�`@8��@8�9@8�u@8�@8�@8�@8bN@8Q�@8Q�@8Q�@8r�@8r�@81'@81'@8A�@8A�@81'@81'@8  @8  @7�@7�;@7��@7��@7�@7��@7l�@7l�@7\)@7\)@7\)@7K�@7;d@7
=@6��@6ff@6V@6E�@6@5�@5@5��@5O�@5V@4�@4�/@4�@4��@4��@4��@4j@49X@4�@41@3�F@3C�@3"�@3@2��@2�\@2�\@2~�@2n�@2n�@2^5@2M�@2=q@2=q@2=q@2M�@2=q@2-@2-@2-@2�@2J@2J@1�@1�#@1�#@1�#@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�#@1��@1�^@1�^@1�^@1��@1��@1��@1��@1�#@1�#@1�#@1�#@1�#@1��@1��@1��@1�^@1��@1��@1�7@1x�@1x�@1X@17L@1&�@1�@1%@1%@0��@0�`@0�`@0Ĝ@0�9@0�9@0�9@0�9@0��@0�@0bN@0bN@0bN@0bN@0Q�@0Q�@0Q�@0Q�@0Q�@0Q�@0A�@01'@0b@0  @/�@/�;@/��@/�@/�P@/\)@/;d@/+@/+@/�@/�@/�@/�@/
=@/
=@.��@.�y@.�R@.��@.��@.��@.��@.��@.��@.��@.�+@.�+@.v�@.E�@.5?@.$�@.$�@.{@.@.@.@.@.@-�@-�@-�@-�@-�T@-��@-��@-@-@-@-@-�h@-�@-�@-�@-p�@-p�@-O�@-�@-V@,�@,�@,�D@,z�@,j@,j@,Z@,I�@,9X@,9X@,(�@,�@+��@+�m@+�
@+�F@+�F@+��@+�@+t�@+t�@+t�@+S�@+S�@+C�@+33@+"�@+@*�@*�H@*��@*��@*��@*��@*��@*��@*��@*�!@*�\@*�\@*�\@*n�@*^5@*M�@*-@*-@*J@)��@)��@)��@)��@)x�@)X@)X@)X@)X@)G�@)7L@)&�@)%@(��@(��@(�`@(��@(Ĝ@(��@(��@(��@(�@(r�@(Q�@(1'@(b@(  @(  @(  @(  A�uA�uA�\A�DA�\A�uA�DA~�A�A�DA�DA�+A�+A�DA�\A�DA�+A�+A�DA�A~�A~�A�A�A�A~�A~�A�A�A�A�A�+A�A�A�+A�+A�DA�+A�A�A�+A�A~�A~�A�A�A~�A~�A~�A�A~�A�A�A�+A�DA�A�+A�DA�DA�DA�Av�Az�Az�Az�Az�Az�A�A�A~�A~�A~�Az�Az�Av�Az�A�+A�DA�DA�A�A�DA�DAz�Av�A~�Az�Ar�A~�A�A�A~�A~�Av�Az�Ar�A^5AI�AE�AE�AE�AA�AA�AE�AE�AE�AA�AA�AE�AE�AE�AA�AE�AE�AE�A=qA=qAA�AE�AE�AA�AA�AE�AA�A=qA=qA=qAA�AA�A9XA=qAA�AA�A=qA=qAA�AA�A=qA9XA5?A5?A1'A-A(�A1'A-A$�A$�A �A(�A$�A$�A �A�A�AbAbA�A{A1A��A��A��A��A��A�A��A��A�A�A��A��A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�mA�TA�TA�mA�mA�mA�TA�TA�A�mA�TA�TA�TA�mA�mA�TA�mA�mA�mA�TA�TA�mA�mA�TA�TA�mA�mA�TA�TA�A�A�mA�mA�mA�A�A�TA�
A�;A�#A��A��A��A�
A�
A�
A�
A�
A�
A��A��A��A��A��A��A��A�
A�
A��A��A�
A�
A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��AƨAƨA��AƨAƨAAƨAƨAƨAAAƨAƨAAAAƨAAAAAAA�wA�wA�wA�wA�wA�^AAA�wA�FA�FA�FA�FA�-A�-A�-A�-A�-A�-A�A�A�A��A��A��A�A�A�A��A��A��A�A�A��A��A�A�-A�A��A��A�-A�A�A��A�A�-A�-A�A��A��A�-A�A��A��A��A��A��A��A��A��A��A�hA�hA�hA��A�hA�7A�7A�PA�PA�7A�7A�7A�PA�7A�A�A�A|�Ax�At�AhsAl�At�Al�Ap�At�At�Ap�Al�Al�Al�Al�AdZA`BA\)A`BA\)AXAS�AXAXAS�AO�AK�AK�AC�A;dA;dA;dA7LA/A33A33A33A33A/A33A33A/A33A33A7LA33A33A33A33A7LA33A33A7LA7LA/A33A7LA7LA33A33A33A7LA/A/A/A33A"�A�A�A�A�A�A"�A+A+A/A/A"�A�A�A�A�A�A�A�A�A�AVA%AA��A��AAAA�A�A�/A�A�HA�HA�HA�HA�HA�`A�yA�HA�A�A�/A�/A�A�A�HA�`A�HA�/A�/A�HA�HA�/A�/A�HA�HA�/A��A��A�A�A�A�A�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�A�`A�HA�`A�yA�yA�`A�`A�yA�yA�yA�`A�`A�`A�`A�`A�HA�`A�`A�/A��A�A�/A��A��A�A�HA�/A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�A��A��A��A��A��A��A��A��A��A��A��AĜAĜAȴAȴAĜAĜA��A�jA�RA�!A�9A�!A�!A�A�!A�!A�9A�9A�RA�9A��A��A��A��A��A��A��A��A�uA��A��A��A�uA�uA��A��A��A��A��A��A��A��A��A��A��A��A�+A~�Az�A�+A�\A�\A�uA��A�uA�\A�\A�uA�uA�+A�+A�DA�DA�+A�+Az�AffA^5AbNAjAjA^5AZAZAZA^5AbNAffAbNAZAQ�AVAQ�AE�AE�A=qA5?A1'A-A-A1'A5?A-A(�A-A1'A-A-A-A1'A1'A-A1'A1'A-A$�A �A �A�A�A�A�A�A{AJAAA  A  AA  A  AA1A1A1AJG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                A�\A�+A�A�A�+A�A�A�A~�A�Az�AE�AA�A=qA$�A��A�A�A�mA�mA�#A��A��A��AƨAA�^A�A�A��A�7Ap�AO�A33A33A&�A�A�A�/A�/A��A��A�`A�A��AȴA�9A��A�uA�\AffAA�A-AbA1A�A�#A�#A�-AA�mA��A�^A��A?}A��A�-AE�AA
=A7LAz�A��AoAĜA��A"�A�;A	�^AĜA	/AbAbNAM�A?}AoA�-A�uAƨA �y@��@��@�33@���@�{@��@��@�@�@޸R@ߍP@�n�@�9X@���@��@��;@��@�z�@�O�@�I�@�~�@�O�@�O�@�R@�G�@�n�@��@�w@��@�X@�@���@�t�@�33@�33@�+@�;d@���@���@���@�5?@�j@�/@�G�@�u@�@���@���@�@��@���@��y@�hs@�7L@�r�@��@�v�@��@�%@�l�@���@��@�9@�b@��@㕁@��@�h@���@�Ĝ@�hs@蛦@���@�hs@��#@��^@���@�h@��@�V@���@�=q@�~�@�!@�!@��@�\)@�@��@�@�w@�F@���@�C�@��@�^5@�v�@�+@�!@柾@�\@�p�@���@�  @�ff@�%@�K�@��@և+@��m@��@�n�@���@�(�@�X@�z�@��y@ޗ�@�$�@���@��@ܼj@���@�=q@�^5@�^5@�n�@ڏ\@��@�dZ@�\)@ە�@�|�@ڗ�@��@�\)@�z�@��;@�(�@��/@��@�V@љ�@�E�@�=q@ѡ�@���@��#@�z�@˅@���@�=q@���@�G�@�r�@ƸR@ċD@�A�@� �@� �@� �@��@���@�t�@��@�5?@���@��@�ff@���@�I�@��@�M�@��-@��@�\)@�~�@�Ĝ@�S�@���@�`B@���@��!@�/@���@���@�bN@��F@��H@�\)@��R@�ff@�$�@���@���@���@�M�@��@��-@��7@�`B@�?}@�Ĝ@�9X@�b@�K�@��+@��T@���@���@�j@��w@�S�@��@���@�@�@���@��@�Ĝ@�9X@��y@��/@��\@�S�@�Z@��9@�b@�|�@���@��@�V@��\@�E�@�ff@���@�v�@��@���@���@�r�@�E�@�x�@�X@���@�%@��^@��R@���@��h@�/@��@��9@��@�|�@�;d@�+@�\)@�|�@�\)@�dZ@��@��#@�`B@�?}@���@�Ĝ@���@���@��@�j@�j@�bN@�I�@��@�ƨ@��
@�  @�(�@�A�@�9X@�b@���@�|�@�dZ@�5?@�p�@�7L@�p�@�hs@���@�P@~�+@�@~��@~ff@~V@~E�@~��@��@��@
=@~�+@}�-@|�j@{��@{"�@{S�@{dZ@{33@{"�@z^5@y�^@y�@w�@w\)@w
=@vV@up�@u?}@t�@r�@q7L@n�@m?}@m?}@l�@l�/@m�h@m�T@m�h@l(�@k��@k�@kt�@kdZ@j�@j�@i�^@ix�@l�@m�h@m�T@o
=@n�@lI�@j~�@k��@k"�@j��@ihs@hbN@i�@fv�@dj@c33@b��@b��@b~�@a�^@ahs@aX@aG�@a7L@a�@`��@`Ĝ@`bN@_\)@_�@`r�@a�7@a��@a�@b�@b=q@c@a�@a7L@a�@ax�@`�u@_�@^��@^v�@^V@^5?@^{@]�T@]��@]�h@]p�@]O�@\�@[@ZJ@Y��@Yx�@Yhs@Y&�@X��@X1'@W�@Xb@X  @X  @W�@Xb@Xb@Xb@X  @W�;@W�;@W�;@W��@W��@W��@W�w@W�P@W��@W�@W��@W|�@W|�@W|�@W|�@W\)@W
=@V��@V�y@V�R@V��@Vff@VV@VE�@V$�@U�@U�-@UO�@UV@U�@T�@T(�@S��@SS�@SS�@SS�@S"�@Rn�@R-@Q�#@Q��@Q�7@Qx�@QX@Q&�@P��@P�@P�u@PbN@PA�@P�@P �@N5?@L�@K�
@KdZ@Ko@J��@J=q@I�@I�@I�^@Ix�@IG�@I7L@I&�@I�@I&�@I&�@H��@HĜ@H��@H�u@H�@H�u@H��@H��@H�@HQ�@HA�@HA�@HA�@HA�@HQ�@HA�@HA�@HA�@H  @G�@G�;@G��@G��@F��@Fv�@FV@FE�@FE�@FE�@Gl�@H�@Ix�@J�\@JM�@JJ@I�@I��@Ihs@I7L@I&�@I�@H�`@HA�@H �@G�@G��@G|�@F�R@E�-@E�@E�@E�@E�@E�@EV@D��@D�D@Dj@DI�@D1@Co@B�H@B��@B�\@B^5@B=q@B=q@B�@BJ@BJ@A��@A�@A��@A�7@Ax�@Ax�@Ax�@Ahs@AG�@AX@AX@A7L@@��@A�@A%@@Ĝ@@�u@@r�@@�@@  @?�w@?;d@?�@>��@>�y@>�R@>��@>��@>��@>�+@>v�@>ff@>E�@>5?@>$�@=�@=��@=�-@=p�@=?}@<�/@<�@<z�@<z�@<j@<j@<j@<I�@<9X@<9X@<9X@<(�@<1@;��@<1@;��@;�m@;�
@;ƨ@;�F@;��@;��@;��@;��@;��@;�@;t�@;C�@;o@;@:�H@:�!@:��@:�\@:�\@:n�@:M�@:=q@9��@9��@9x�@9X@97L@9&�@9%@8��@8�`@8�`@8��@8�9@8�u@8�@8�@8�@8bN@8Q�@8Q�@8Q�@8r�@8r�@81'@81'@8A�@8A�@81'@81'@8  @8  @7�@7�;@7��@7��@7�@7��@7l�@7l�@7\)@7\)@7\)@7K�@7;d@7
=@6��@6ff@6V@6E�@6@5�@5@5��@5O�@5V@4�@4�/@4�@4��@4��@4��@4j@49X@4�@41@3�F@3C�@3"�@3@2��@2�\@2�\@2~�@2n�@2n�@2^5@2M�@2=q@2=q@2=q@2M�@2=q@2-@2-@2-@2�@2J@2J@1�@1�#@1�#@1�#@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�#@1��@1�^@1�^@1�^@1��@1��@1��@1��@1�#@1�#@1�#@1�#@1�#@1��@1��@1��@1�^@1��@1��@1�7@1x�@1x�@1X@17L@1&�@1�@1%@1%@0��@0�`@0�`@0Ĝ@0�9@0�9@0�9@0�9@0��@0�@0bN@0bN@0bN@0bN@0Q�@0Q�@0Q�@0Q�@0Q�@0Q�@0A�@01'@0b@0  @/�@/�;@/��@/�@/�P@/\)@/;d@/+@/+@/�@/�@/�@/�@/
=@/
=@.��@.�y@.�R@.��@.��@.��@.��@.��@.��@.��@.�+@.�+@.v�@.E�@.5?@.$�@.$�@.{@.@.@.@.@.@-�@-�@-�@-�@-�T@-��@-��@-@-@-@-@-�h@-�@-�@-�@-p�@-p�@-O�@-�@-V@,�@,�@,�D@,z�@,j@,j@,Z@,I�@,9X@,9X@,(�@,�@+��@+�m@+�
@+�F@+�F@+��@+�@+t�@+t�@+t�@+S�@+S�@+C�@+33@+"�@+@*�@*�H@*��@*��@*��@*��@*��@*��@*��@*�!@*�\@*�\@*�\@*n�@*^5@*M�@*-@*-@*J@)��@)��@)��@)��@)x�@)X@)X@)X@)X@)G�@)7L@)&�@)%@(��@(��@(�`@(��@(Ĝ@(��@(��@(��@(�@(r�@(Q�@(1'@(b@(  @(  @(  @(  A�uA�uA�\A�DA�\A�uA�DA~�A�A�DA�DA�+A�+A�DA�\A�DA�+A�+A�DA�A~�A~�A�A�A�A~�A~�A�A�A�A�A�+A�A�A�+A�+A�DA�+A�A�A�+A�A~�A~�A�A�A~�A~�A~�A�A~�A�A�A�+A�DA�A�+A�DA�DA�DA�Av�Az�Az�Az�Az�Az�A�A�A~�A~�A~�Az�Az�Av�Az�A�+A�DA�DA�A�A�DA�DAz�Av�A~�Az�Ar�A~�A�A�A~�A~�Av�Az�Ar�A^5AI�AE�AE�AE�AA�AA�AE�AE�AE�AA�AA�AE�AE�AE�AA�AE�AE�AE�A=qA=qAA�AE�AE�AA�AA�AE�AA�A=qA=qA=qAA�AA�A9XA=qAA�AA�A=qA=qAA�AA�A=qA9XA5?A5?A1'A-A(�A1'A-A$�A$�A �A(�A$�A$�A �A�A�AbAbA�A{A1A��A��A��A��A��A�A��A��A�A�A��A��A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�mA�TA�TA�mA�mA�mA�TA�TA�A�mA�TA�TA�TA�mA�mA�TA�mA�mA�mA�TA�TA�mA�mA�TA�TA�mA�mA�TA�TA�A�A�mA�mA�mA�A�A�TA�
A�;A�#A��A��A��A�
A�
A�
A�
A�
A�
A��A��A��A��A��A��A��A�
A�
A��A��A�
A�
A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��AƨAƨA��AƨAƨAAƨAƨAƨAAAƨAƨAAAAƨAAAAAAA�wA�wA�wA�wA�wA�^AAA�wA�FA�FA�FA�FA�-A�-A�-A�-A�-A�-A�A�A�A��A��A��A�A�A�A��A��A��A�A�A��A��A�A�-A�A��A��A�-A�A�A��A�A�-A�-A�A��A��A�-A�A��A��A��A��A��A��A��A��A��A�hA�hA�hA��A�hA�7A�7A�PA�PA�7A�7A�7A�PA�7A�A�A�A|�Ax�At�AhsAl�At�Al�Ap�At�At�Ap�Al�Al�Al�Al�AdZA`BA\)A`BA\)AXAS�AXAXAS�AO�AK�AK�AC�A;dA;dA;dA7LA/A33A33A33A33A/A33A33A/A33A33A7LA33A33A33A33A7LA33A33A7LA7LA/A33A7LA7LA33A33A33A7LA/A/A/A33A"�A�A�A�A�A�A"�A+A+A/A/A"�A�A�A�A�A�A�A�A�A�AVA%AA��A��AAAA�A�A�/A�A�HA�HA�HA�HA�HA�`A�yA�HA�A�A�/A�/A�A�A�HA�`A�HA�/A�/A�HA�HA�/A�/A�HA�HA�/A��A��A�A�A�A�A�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�A�`A�HA�`A�yA�yA�`A�`A�yA�yA�yA�`A�`A�`A�`A�`A�HA�`A�`A�/A��A�A�/A��A��A�A�HA�/A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�A��A��A��A��A��A��A��A��A��A��A��AĜAĜAȴAȴAĜAĜA��A�jA�RA�!A�9A�!A�!A�A�!A�!A�9A�9A�RA�9A��A��A��A��A��A��A��A��A�uA��A��A��A�uA�uA��A��A��A��A��A��A��A��A��A��A��A��A�+A~�Az�A�+A�\A�\A�uA��A�uA�\A�\A�uA�uA�+A�+A�DA�DA�+A�+Az�AffA^5AbNAjAjA^5AZAZAZA^5AbNAffAbNAZAQ�AVAQ�AE�AE�A=qA5?A1'A-A-A1'A5?A-A(�A-A1'A-A-A-A1'A1'A-A1'A1'A-A$�A �A �A�A�A�A�A�A{AJAAA  A  AA  A  AA1A1A1AJG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�fB�`B�fB�`B�`B�fB�fB�fB�`B�`B�fB�fB�fB�fB�fB�mB�fB�fB�fB�fB�fB�fB�fB�fB�`B�`B�`B�ZB�ZB�ZB�TB�NB�HB�BB�BB�;B�;B�5B�/B�/B�)B�)B�/B�/B�)B�)B�#B�#B�#B�B�B�B�
B�B�B�B�B�
B�)B�HB�BPB$�B;dBS�Bl�B�PB�B�dB�;B�)B�B�/B�/B�yB��B		7B	�B	�B	uB	�B	�B	�B	/B	DB	%B	\B	$�B	oB	B�mB��B�B�B��B�!B�Be`BXB[#BcTBaHBK�BA�BD�BD�BI�BN�BW
BjB�B�{B�BȴB�/B�`B�BÖB	B	DB	�B	"�B	 �B	�B	�B	 �B	!�B	2-B	N�B	O�B	J�B	@�B	E�B	G�B	H�B	Q�B	W
B	[#B	]/B	\)B	YB	R�B	L�B	L�B	J�B	F�B	C�B	A�B	>wB	8RB	5?B	33B	5?B	5?B	1'B	49B	K�B	^5B	^5B	^5B	e`B	iyB	q�B	�oB	��B	��B	��B	��B	��B	��B	�3B	�^B	�}B	��B	��B	ÖB	�qB	�3B	�dB	ĜB	ĜB	ĜB	ŢB	ǮB	�RB	�XB	�dB	�dB	�jB	�jB	�dB	�XB	�RB	�RB	�9B	�B	��B	�\B	|�B	~�B	��B	�{B	�\B	�JB	��B	��B	�wB	�qB	�dB	�?B	�9B	�?B	�B	��B	��B	��B	��B	�B	�?B	�RB	�dB	�qB	�jB	�^B	��B	��B	�uB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�hB	�\B	�VB	�VB	�PB	�DB	�=B	�%B	�B	�B	�B	�%B	�%B	�B	�B	�B	�B	� B	{�B	x�B	v�B	s�B	n�B	l�B	jB	hsB	dZB	cTB	^5B	YB	W
B	R�B	Q�B	M�B	J�B	I�B	I�B	I�B	J�B	H�B	B�B	>wB	>wB	>wB	?}B	=qB	=qB	9XB	8RB	7LB	7LB	6FB	6FB	5?B	5?B	49B	33B	1'B	0!B	/B	.B	-B	,B	+B	-B	.B	.B	/B	.B	.B	-B	,B	(�B	#�B	�B	�B	#�B	(�B	(�B	(�B	%�B	!�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	hB	bB	bB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	'�B	(�B	&�B	$�B	$�B	%�B	&�B	'�B	)�B	+B	,B	.B	/B	0!B	1'B	33B	49B	7LB	9XB	:^B	;dB	>wB	@�B	?}B	?}B	>wB	;dB	;dB	=qB	>wB	>wB	B�B	A�B	F�B	F�B	F�B	G�B	G�B	I�B	P�B	T�B	T�B	S�B	T�B	S�B	S�B	T�B	YB	[#B	[#B	[#B	[#B	ZB	ZB	ZB	YB	YB	ZB	ZB	[#B	[#B	XB	W
B	T�B	R�B	S�B	S�B	S�B	VB	YB	]/B	]/B	\)B	\)B	\)B	\)B	[#B	[#B	\)B	[#B	dZB	jB	m�B	o�B	r�B	q�B	l�B	o�B	r�B	r�B	t�B	p�B	s�B	r�B	n�B	k�B	jB	k�B	l�B	p�B	q�B	r�B	s�B	v�B	y�B	z�B	|�B	|�B	}�B	~�B	�B	�1B	�7B	�=B	�DB	�DB	�\B	�oB	�hB	�hB	�uB	�{B	�oB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�LB	�LB	�LB	�LB	�RB	�jB	�jB	�jB	�wB	��B	��B	��B	B	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�#B	�#B	�)B	�;B	�NB	�NB	�NB	�NB	�ZB	�ZB	�mB	�sB	�yB	�`B	�ZB	�ZB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
+B
	7B

=B

=B
DB
DB
PB
\B
bB
hB
oB
{B
�B
�B
 �B
%�B
'�B
'�B
'�B
)�B
,B
,B
-B
-B
-B
0!B
0!B
1'B
1'B
2-B
49B
7LB
9XB
;dB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
J�B
K�B
M�B
N�B
P�B
Q�B
Q�B
R�B
S�B
VB
ZB
[#B
\)B
\)B
_;B
aHB
dZB
ffB
gmB
hsB
iyB
k�B
l�B
n�B
o�B
p�B
r�B
s�B
t�B
v�B
w�B
z�B
|�B
}�B
}�B
}�B
~�B
~�B
�B
�B
�B
�B
�%B
�1B
�=B
�=B
�DB
�DB
�JB
�PB
�VB
�VB
�\B
�\B
�\B
�bB
�bB
�hB
�uB
�{B
�{B
�{B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�!B
�-B
�3B
�3B
�?B
�?B
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�RB
�^B
�dB
�dB
�dB
�qB
�wB
�wB
�}B
��B
��B
��B
��B
B
B
B
B
ÖB
ĜB
ŢB
ŢB
ǮB
ɺB
ɺB
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
��B
��B
��B
�B
�B
�B
�B
�B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�)B
�/B
�5B
�5B
�5B
�5B
�5B
�;B
�;B
�;B
�BB
�HB
�HB
�NB
�TB
�TB
�fB
�mB
�mB
�sB
�sB
�yB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
��B
��B
��B
��B
��B
��B
��B
��B  B  B  B  B  B  BBBBBBBBBBBBBBBBBBBB%B%B%B%B%B+B+B1B1B1B1B1B	7B
=BDBDBPBPBVBVB\B\B\BbBbBbBbBhBoBoBoBoBuBuB{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B!�B!�B!�B"�B"�B"�B"�B#�B#�B$�B%�B%�B%�B%�B%�B�fB�`B�fB�fB�fB�`B�mB�fB�`B�ZB�fB�fB�fB�`B�ZB�fB�mB�`B�`B�fB�mB�fB�`B�`B�`B�fB�`B�ZB�`B�fB�`B�`B�`B�fB�`B�`B�`B�`B�mB�`B�ZB�fB�fB�fB�`B�`B�fB�mB�fB�`B�`B�`B�fB�fB�`B�fB�fB�fB�ZB�`B�sB�fB�`B�`B�`B�fB�fB�ZB�`B�`B�fB�fB�fB�fB�fB�`B�TB�TB�`B�mB�fB�ZB�`B�sB�fB�`B�fB�fB�TB�`B�fB�`B�`B�mB�ZB�fB�yB�yB�fB�`B�`B�fB�fB�`B�`B�`B�fB�fB�`B�`B�`B�fB�fB�`B�`B�mB�`B�`B�`B�`B�fB�fB�fB�`B�fB�fB�fB�`B�`B�fB�fB�`B�`B�fB�fB�`B�`B�fB�mB�fB�fB�fB�fB�mB�fB�fB�mB�mB�mB�TB�fB�mB�mB�mB�mB�mB�sB�ZB�fB�sB�B�fB�fB�fB�mB�mB�fB�fB�mB�mB�mB�fB�fB�sB�mB�fB�fB�mB�fB�`B�fB�mB�mB�fB�fB�fB�mB�fB�fB�mB�mB�mB�fB�fB�mB�mB�fB�sB�mB�fB�mB�fB�fB�sB�fB�`B�fB�mB�mB�fB�fB�fB�mB�fB�fB�fB�mB�mB�fB�fB�mB�mB�fB�fB�fB�fB�`B�fB�fB�mB�fB�fB�fB�mB�yB�ZB�sB�mB�fB�fB�fB�`B�fB�fB�fB�fB�fB�fB�`B�`B�fB�mB�fB�`B�`B�fB�fB�fB�`B�fB�fB�fB�`B�`B�fB�fB�fB�fB�fB�fB�fB�`B�fB�fB�fB�fB�`B�fB�fB�fB�`B�fB�fB�fB�`B�`B�fB�fB�fB�`B�`B�fB�fB�`B�`B�`B�mB�fB�`B�`B�fB�fB�`B�`B�`B�fB�fB�`B�`B�`B�fB�`B�`B�`B�fB�fB�`B�ZB�`B�`B�fB�`B�ZB�`B�fB�ZB�ZB�`B�fB�fB�`B�`B�ZB�`B�`B�ZB�ZB�ZB�`B�`B�`B�`B�ZB�`B�ZB�ZB�ZB�`B�`B�ZB�TB�ZB�`B�`B�ZB�ZB�ZB�`B�`B�TB�ZB�`B�`B�ZB�ZB�ZB�ZB�`B�ZB�TB�ZB�`B�ZB�ZB�fB�ZB�ZB�ZB�ZB�NB�ZB�ZB�TB�TB�TB�`B�TB�TB�TB�TB�TB�TB�TB�TB�TB�ZB�NB�ZB�NB�ZB�ZB�HB�HB�TB�TB�NB�NB�NB�TB�NB�HB�NB�TB�TB�NB�HB�HB�NB�NB�HB�HB�HB�NB�HB�HB�NB�NB�BB�HB�HB�HB�HB�BB�;B�BB�HB�;B�HB�BB�BB�BB�;B�BB�HB�BB�BB�BB�BB�BB�;B�BB�HB�BB�;B�BB�BB�HB�BB�;B�BB�HB�;B�BB�TB�5B�BB�;B�;B�5B�/B�5B�;B�/B�BB�TB�;B�5B�BB�;B�;B�5B�5B�BB�BB�NB�;B�;B�5B�/B�)B�;B�BB�TB�#B�BB�/B�/B�)B�/B�/B�/B�/B�/B�5B�5B�5B�)B�)B�/B�/B�)B�#B�/B�5B�/B�)B�)B�/B�/B�/B�)B�/B�5B�/B�)B�)B�)B�/B�/B�/B�)B�/B�/B�)B�)B�/B�/B�)B�)B�)B�/B�/B�)B�)B�/B�/B�/B�)B�)B�/B�/B�)B�)B�)B�/B�/B�)B�B�/B�5B�/B�)B�/B�/B�/B�/B�/B�5B�/B�)B�/B�/B�5B�/B�)B�5B�BB�#B�/B�5B�/B�)B�#B�)B�5B�/B�#B�)B�/B�/B�)B�)B�/B�/B�/B�)B�)B�/B�#B�#B�/B�/B�)B�#B�/B�/B�#B�)B�)B�/B�)B�)B�)B�#B�#B�)B�)B�/B�#B�)B�/B�#B�#B�#B�#B�#B�B�#B�)B�#B�)B�5B�5B�#B�#B�#B�)B�B�#B�#B�#B�B�B�)B�#B�B�#B�#B�#B�#B�#B�#B�#B�B�#B�#B�#B�;B�B�B�B�B�#B�B�B�#B�#B�)B�B�B�/B�#B�B�B�B�#B�/B�B�B�#B�B�B�#B�B�B�B�B�B�B�B�B�B�B�B�B�
B�#B�B�
B�
B�B�B�B�B�B�B�B�B�B�
B�
B�
B�B�
B�B�
B�B�B�B�
B�
B�
B�B�B�
B�B�B��B�B�B��B�B��B��B��B��B�
B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBɺBɺBȴBȴBƨBŢBĜBĜBĜBĜBŢB��B��B�5B��BuB+BC�BYBy�B��B�-B�B��B��B��B��B�
B�fB��B	oB	JB	B	JB	
=B		7B	$�B��B�B��B	�B		7B��B��B�fB�BB�B��B�!Bx�BXBE�BH�BS�BXB=qB1'B49B33B7LB;dB@�BT�Bl�B|�B�oB�3BɺB�BɺB��B�B��B	PB	hB	\B	VB	VB	VB	JB	�B	<jB	A�B	;dB	.B	33B	6FB	5?B	?}B	D�B	I�B	K�B	K�B	I�B	C�B	;dB	<jB	:^B	6FB	2-B	1'B	/B	(�B	$�B	!�B	$�B	$�B	�B	�B	6FB	L�B	L�B	K�B	S�B	VB	[#B	� B	�=B	�JB	�\B	�hB	�\B	�bB	��B	��B	�B	�!B	�'B	�?B	�B	��B	��B	�-B	�3B	�-B	�?B	�qB	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	iyB	iyB	�B	�B	}�B	x�B	~�B	�{B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�hB	�B	{�B	~�B	�%B	�+B	�+B	�=B	�oB	�uB	��B	�JB	�7B	�B	�B	}�B	|�B	|�B	|�B	{�B	{�B	t�B	s�B	s�B	s�B	t�B	t�B	s�B	s�B	r�B	p�B	p�B	k�B	hsB	gmB	dZB	]/B	\)B	ZB	XB	S�B	S�B	N�B	H�B	F�B	B�B	C�B	>wB	9XB	8RB	8RB	9XB	:^B	<jB	2-B	-B	-B	-B	/B	-B	.B	'�B	&�B	%�B	%�B	$�B	$�B	#�B	#�B	#�B	"�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	1B	DB	hB	�B	�B	�B	�B	hB	oB	%B	B	%B	
=B	JB	DB	
=B	PB	DB	B	  B��B��B��B	B	1B	VB		7B		7B	1B		7B	+B	%B	%B	1B	DB	hB	{B	�B	�B	�B	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	$�B	'�B	(�B	)�B	-B	/B	.B	/B	.B	)�B	(�B	,B	-B	.B	1'B	/B	5?B	5?B	5?B	6FB	5?B	7LB	?}B	C�B	C�B	B�B	C�B	B�B	B�B	C�B	G�B	I�B	I�B	I�B	I�B	H�B	H�B	H�B	G�B	G�B	H�B	H�B	J�B	J�B	F�B	F�B	D�B	A�B	B�B	B�B	A�B	C�B	G�B	L�B	K�B	J�B	J�B	J�B	J�B	I�B	J�B	J�B	G�B	P�B	XB	[#B	^5B	bNB	aHB	ZB	^5B	aHB	bNB	cTB	^5B	dZB	bNB	^5B	ZB	YB	ZB	[#B	_;B	`BB	aHB	bNB	e`B	hsB	iyB	k�B	k�B	l�B	l�B	o�B	u�B	v�B	w�B	x�B	x�B	}�B	�B	� B	~�B	�B	�B	�B	� B	�B	�+B	�1B	�=B	�DB	�PB	�VB	�\B	�VB	�bB	�hB	�hB	�hB	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�FB	�LB	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�dB	�jB	�jB	�qB	�}B	��B	��B	��B	��B	��B	B	ŢB	ŢB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�BB	�NB	�NB	�HB	�TB	�ZB	�ZB	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B

=B
VB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
%�B
'�B
)�B
+B
,B
-B
-B
.B
.B
.B
.B
/B
0!B
0!B
0!B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
9XB
:^B
<jB
<jB
?}B
@�B
@�B
A�B
A�B
D�B
H�B
I�B
J�B
J�B
M�B
O�B
R�B
T�B
VB
W
B
XB
ZB
[#B
]/B
^5B
_;B
aHB
bNB
cTB
e`B
ffB
iyB
k�B
k�B
l�B
l�B
l�B
m�B
o�B
o�B
p�B
r�B
t�B
v�B
w�B
x�B
y�B
y�B
z�B
{�B
|�B
|�B
|�B
|�B
}�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�7B
�=B
�JB
�PB
�PB
�VB
�VB
�VB
�PB
�VB
�\B
�\B
�bB
�\B
�\B
�hB
�oB
�hB
�hB
�oB
�{B
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
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�!B
�B
�!B
�!B
�!B
�!B
�'B
�-B
�3B
�9B
�9B
�FB
�RB
�RB
�XB
�^B
�^B
�dB
�dB
�dB
�dB
�jB
�qB
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�wB
��B
��B
B
B
B
B
B
B
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ƨB
ǮB
ǮB
ǮB
ǮB
ǮB
ȴB
ɺB
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
��B
��B
��B
��B
��B
��B
��B
�B
�B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�)B
�/B
�/B
�5B
�5B
�5B
�5B
�5B
�5B
�;B
�HB
�HB
�NB
�TB
�TB
�`B
�fB
�mB
�sB
�sB
�sB
�yB
�yB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
��B  BB  B  BBBBBBBBBBBBBBB%B%B%B%B%B%B+B+B+B1B1B1B	7B	7B
=B	7BDBDBJBJBJBJBPBPBPBPBPB\BVBVBVB\B\BbBbBbBhBbBhBoBuBuBuBuBuBuB��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B�B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B�
B��B��B��B��B��B��B��B��B��B�B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�B��B��B�B�B�B��B��B�B�B�B�B�B�
B��B��B�
B�B��B��B��B�B�B��B��B�B�B�B��B��B�
B�B��B��B�B��B��B��B�B�B��B��B��B�B��B��B�B�B�B��B��B�B�B��B�
B�B��B�B��B��B�
B��B��B��B�B�B��B��B��B�B��B��B��B�B�B��B��B�B�B��B��B��B��B��B��B��B�B��B��B��B�B�B��B�
B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺB��B��B��B��BɺB��B��B��BɺB��B��B��B��B��B��B��B��B��B��B��BɺBɺB��B��B��BɺB��B��BɺB��B��B��B��B��B��BɺBɺB��B��B��BɺB��B��BɺBɺBɺBɺBɺBȴBɺB��BɺB��B��B��BɺBɺBɺB��BȴBɺBɺBɺBȴBȴB��BɺBȴBɺBɺBɺBɺBɺBɺBɺBȴBɺBɺBɺB��BǮBȴBƨBȴBɺBǮBȴBɺBɺB��BȴBȴB��BɺBǮBǮBȴBɺB��BǮBȴBɺBƨBǮBɺBȴBǮBǮBƨBǮBȴBǮBǮBȴBǮBƨBǮBŢBɺBƨBŢBŢBƨBĜBĜBƨBƨBĜBĜBƨBƨBŢBŢBŢBƨBŢBĜBŢBǮBƨBĜBŢBŢBŢBĜBĜBŢBǮBĜBÖBĜBĜBBĜBÖBÖBBÖBŢBĜG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                <�{<��<�@<�=<��<�8<�3<�=<�3<�	><�<�U<�[<��<��<��<�a<�Q<�2<�	0<��<�2<�L<��<�Q<��<��<�8<�	[<�[<��<��<��<�D<�	"<��<��<�	�<�6<�t<�2<�	j<��<�t<��<�
�<�2<�Q<�a<��<��<�+<��<��<��<�
�<�H<��<�	�<��<�-�<�<�z<�;�<�;�<��H<�w�<�2<�'�<�A<�0�<�>�<�y�<�5�<��<�
(<���<�#�<�]�<�5<���<�$b<�-�<���<�=�<���<���<�c<��<�k<�<��<�2�<�.V<�I�<�g�<��<���<�$�<�<Y<��<��[<���<�`�<���<�t<�(�<�e8<�ͦ<�6<�h�<�T<�P<���<�^�<�L�<�<�<�2�<�k�<�2<� A<�<�z<�:<�Q<��<�c<���<�o<��D<�-!<�:l<�	><�/�<�[ <�'<�?�<�	�<�%?<��(<��c<��><�/<�F�<��5<�8(<�3<�|L<��<��<�a<��<�5�<�x�<�=!<�1<���<�,P<�D<�!�<�1T<�ш<��<�7j<�{<�{<�W<�*e<�	�<�I�<��<��<��<�8<�;�<�<<�=�<���<�?�<�	[<�H<�a<�I<���<��<�	"<��<�
(<��<�	�<�{�<�!�<�~<�	�<��~<��<�?s<��A<���<��<��<���<�0�<���<���<�Jn<��<�!<��<�D<�#<�^�<�+�<��<�2<��<�+<���<��<�L<��<�
d<��z<�;<�y�<�\<�1�<��<�5�<�	[<�
�<�& <�0�<�1<�?<�Cn<��*<��N<�m+<�M<�&k<�p<�'�<�`<�L4<���<�x<�	�<�3<�6<�Q<�x<�<�%?<�D <���<��<��7<�7�<���<��<�.�<�5�<��<���<�h�<�Ge<��F<�C�<��`<�:l<���<��<�T<��<��<�=�<�l<��<�BY<��<�N<�*<�4�<��u<��Z<��<�<�[<�
�<�C<� A<�)t<��<�I�<�I�<�8(<�4<�9H<�,P<�;�<�p<��<�L<�U<�2<��<�}u<���<�0�<��B<���<�)�<�3n<�yj<��<�7<�3<�;0<�A�<��#<��y<��<�
<�%?<�"�<�!�<�[<��<��s<��.<�UX<�+<��<�	><�;�<�oX<��<��f<��<�[<�4<�F�<�x<��<��<�+<�	�<�	�<�1<�+X<�k�<�"�<�C<�4<��<�g<��<�	><�	<�6<�H<�	j<�<�<��<�
�<��<�	"<�a<��<��<��<�[<��W<�F�<�W<�G<�g<�-J<��7<�#@<��<�<�	><�g<�8<��<�)�<��<��<��<�x<�!�<�-�<�5<��<�[<��<�m<�<��<�w<�-J<�x<��<�<�/<�
O<�2<�pp<�*<���<�M�<�m<�
�<�8<�H<�
�<�G<�<�<�<��<�a<�t<�<�{<�.�<�t<��W<�Ji<�Q<�*�<�
�<���<�Tg<�B<�<�c<�A<��<��<���<���<�4$<��<�L<��<�<�
�<�U<�H<�a<��<�t<�	y<��<�%�<�	L<�:
<�(�<�
<�
(<�	0<�	<�x<�(�<��<��<�
�<��<��<��<�x<�g<��<��<�	<�[<�	�<��<��<��<�Z%<�#@<��<�	><�{<�
<�
�<�x<�
<��<�L<�1<�U<�m<�1<�2<�Q<��<�1<�1<�D<�2<�2<�t<�	�<�g<�H<�g<��<�8<�3<�1<��<�C<�U<�a<�	"<��<�	><�Q<�a<��<�	j<�	�<��<�	�<�@<�	�<��<��<�
(<�2<�3<�	�<�<�
(<��<�
(<�H<�g<��<�	[<�
�<�
�<�D<�	�<�m<�	<�q<�q�<�J�<�p<�<�C<��<��<�
�<�2<�	L<�
�<�	0<�L<�=<�H<�U<�2<�
�<�t<��<�H<��<�m<�Q<�1<��<�	><�L<�1<�1<�1<�[<�L<�6<�3<�
<�H<�H<�a<�	y<�<��<��<�U<�2<�D<�,�<�)t<�'Q<�(9<�	�<�
<��<��<��<��<�a<�[<�	�<�/<��<�	j<��<��<��<�'Q<��<�2<�4<�1<�4<�Q<�
x<�
<��<��<�
�<� A<�	�<��<�	�<�	L<��<�3<��<�Q<�2<�[<�U<��<�
(<�D<�1<�1<�D<��<�H<�1<��<�	�<�m<�L<�	�<�	j<��<�4<�4<�
<��<��<�	<�H<�	j<�Q<�@<�1<�U<�U<�H<��<�L<�Q<�	j<��<��<�
x<�	0<��<�	0<�	<�1<�L<�4<�1<��<�=<�1<�1<�Q<��<�H<�g<�[<�L<�a<�L<�Q<�H<�6<�2<�1<�a<�L<�g<�	j<�	0<�@<��<�	j<�@<�[<�1<��<��<�t<�
x<�	L<�+<��<��<�m<��<�[<�U<�1<�g<��<��<�H<�1<�2<��<�@<�1<�1<��<�1<�
<�4<�:<�=<�U<�1<��<�3<�[<�m<�@<�1<��<�@<�	y<�1<�a<�8<�1<�U<�m<�	L<�<�	�<�Q<�a<�
x<�Q<�	0<��<�[<�
<��<�Q<�	L<�@<�2<�2<�	y<�	y<��<�U<�+<��<��<��<�
<<�	L<�1<�[<�[<�2<�[<�g<�[<�3<�2<�H<�g<�@<�1<�4<�Q<�[<�3<��<�L<�1<�1<�H<�2<�2<�1<�2<�1<�1<�3<�2<�3<�3<�3<�1<�Q<�H<�[<�1<�1<�Q<�6<�3<��<�Q<�4<�1<�1<�1<�L<�2<�3<�m<��<�1<�a<�L<�1<�	<��<�Q<�a<�H<�6<�L<�H<�1<��<�U<�1<�1<�1<�g<��<��<�3<�2<�2<�[<�4<�1<�3<�1<�2<�[<�Q<��<�Q<�U<�@<�[<��<��<�	0<��<�D<�4<�L<�2<�2<�1<�L<�1<�Q<�[<�	[<�U<�1<�Q<�1<�1<�1<�2<�=<�1<�Q<�	j<�U<�D<�1<�L<�@<�2<�3<�1<�3<�H<�2<�2<�2<�Q<�U<�1<�H<�1<�1<�6<�	<�[<�1<�1<�Q<�1<��<�	L<�U<��<�
x<��<�m<�D<�:<�D<�L<�m<�2<�Q<�U<��<�Q<�U<��<�3<�t<��<�[<�3<�2<��<�1<�H<�a<�a<��<��<�H<�Q<�6<�D<�1<�2<�1<�1<�Q<��<�1<�3<��<�L<�[<��<�1<��<�g<�	L<�1<��<�	[<��<�8<�2<�1<�L<�H<�a<��<�Q<�2<�g<�m<�U<��<�1<�3<��<�m<��<��<��<�D<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1<�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                  PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                          None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment;OW: r =0.9995(+/-0.0006), vertically averaged dS =-0.017(+/-0.021), breaks: 1, -- < T < 3, 1000 < P < --,  Map Scales:[x=2,1; y=1,0.5].                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW: r =0.9995(+/-0.0006), vertically averaged dS =-0.017(+/-0.021), breaks: 1, -- < T < 3, 1000 < P < --,  Map Scales:[x=2,1; y=1,0.5].                                                                                                                         SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT.; Significant salinity drift, OW fit adopted: fit for cycles 0 to 83.  The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                 SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                No thermal mass adjustment on non-primary profiles.; Significant salinity drift, OW fit adopted: fit for cycles 0 to 83.  The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                            202302160000002023021600000020230216000000202302160000002023021600000020230216000000AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030120161420210301201614QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030120161420210301201614QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               WHOIWHOIARSQARSQWHQCWHQCV0.5V0.5                                                                                                                                2021040500000020210405000000QC  QC                                  G�O�G�O�G�O�G�O�G�O�G�O�                                WHOIWHOIARSQARSQCTM CTM V1.0V1.0                                                                                                                                2022112100000020221121000000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                WHOIWHOIARCAARCAOWC OWC V2.0V2.0ARGO_for_DMQC_2022V03; CTD_for_DMQC_2021V02                     ARGO_for_DMQC_2022V03; CTD_for_DMQC_2021V02                     2023021600000020230216000000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                