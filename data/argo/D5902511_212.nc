CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-05-16T13:09:55Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  R�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  X8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  m�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  sh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �H   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p 8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` !h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   !�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   '�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   -�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T 3�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   4$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   4,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   44   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 4<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   4�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   4�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    4�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        5    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        5   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       5   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    5Argo profile    3.1 1.2 19500101000000  20220516130955  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_212                 6810_008521_212                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��]n�Y@��]n�Y11  @��]����@��]����@0I�Y��}@0I�Y��}�d�CA�0�d�CA�011  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�@B�\@�G�@��R@�p�@�p�A   A  A   A,��A@  A`  A�  A�  A�  A�  A�Q�A�Q�A�Q�A�Q�B Q�BQ�BQ�B(�B (�B((�B/�
B7�
B@  BH  BP(�BX(�B`  Bg�Bo�
Bx(�B�{B��B��
B�  B�  B��B�  B�  B�(�B�  B�{B�{B�{B�  B��B�  B�{B��
B��
B�  B�(�B�{B�  B�  B�  B��B��B�{B�{B�{B�  B�  C 
=C�C  C  C  C
  C
=C
=C  C��C  C�C
=C  C
=C
=C   C!�C#�C%��C(  C*
=C,  C-��C0�C2{C4{C6{C8  C:  C<  C>
=C@  CB  CD
=CF  CH
=CI��CL
=CN{CP  CR  CT
=CV  CX  CY��C\  C^
=C`  Cb  Cd  Cf
=Ch  Ci��Cl  Cm��Co��Cq��Cs�Cu��Cx  Cz
=C|
=C~  C�C�  C���C���C�  C�  C���C�  C�  C���C���C�  C�  C�  C�C���C�  C�  C���C���C�  C�C�C�  C���C���C�  C�C�C�  C�  C�C�  C���C���C�  C�  C���C�  C�  C�  C���C���C�  C�C�
=C�C�  C�  C�C�
=C�C�  C�  C���C���C�  C�  C�  C�C�C�  C�  C�  C���C�  C�C�  C�C�
=C�C�  C���C�C�C�  C�  C�  C�C�C�  C�  C�C�C�
=C�
=C�  C���C���C�  C�  C���C���C�  C���C�  C�  C���C�  C�  C�  C�  C���C���C�C�C���C���C�C�  C�  C�  C���C�  C���C�  C�C�  C�C�  C�  C�
=C�C���C���C�  C�  C���D   D �D  Dz�D  D��D  D� D  D}qD  D� D  Dz�D��D� D�D��D	  D	� D
�D
� D
�qD� D  D��D  Dz�D  D�DD��D�D� D  D}qD��D� D�D�D�D�D�D��D  D� D�D�D�Dz�D  D��D  D� D�qD��D�D��D�D� D  D� D  D� D   D � D!�D!�D"  D"� D#  D#� D$�D$��D%�D%��D&D&� D&�qD'��D(�D(}qD)  D)��D)�qD*z�D*��D+� D,�D,��D-�D-� D-��D.� D.�qD/z�D/��D0}qD1  D1��D2  D2� D3  D3}qD4  D4� D5  D5��D6  D6}qD7�D7� D8  D8��D9  D9}qD9�qD:� D;�D;� D<�D<�D=�D=�D>  D>� D?  D?��D@D@� DA  DA��DBDB��DC�DC��DD�DD� DD�qDE��DFDF��DG  DG� DH�DH��DI  DI��DJDJ��DK  DK� DL�DL��DM  DM}qDN  DN�DODO� DO�qDP}qDP��DQz�DR  DR�DSDS� DS�RDTxRDT�RDU� DVDV��DV�qDW��DX  DXz�DX�qDY}qDZ  DZ� D[�D[��D[�qD\}qD]�D]��D]�qD^� D_  D_}qD`  D`z�D`��Da}qDa�qDbz�Db�qDc� DdDd�Dd�qDe}qDf  Df��Dg�Dg�Dh�Dh� Di�Di� Di�qDj��Dk�Dk}qDk��Dl}qDm  Dmz�Dm�qDn}qDn��Do}qDo�qDp� Dq  Dq}qDr  Dr��Ds  Ds� Dt  Dt� Du  Du��Dv�Dv}qDw  Dw� Dx  Dx��Dy  Dy� Dz�Dz� Dz�qD{}qD|  D|}qD|�qD}}qD~  D~}qD~�qD}qD�  D�B�D�� D�� D�HD�@ D�~�D���D��qD�=qD�~�D�� D���D�>�D��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D��HD�  D�AHD��HD�� D�HD�@ D�~�D�� D�HD�AHD��HD��=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��?8Q�?k�?�{?�p�?�@\)@(�@0��@E�@Tz�@h��@�  @��@��@��R@��
@��@��H@\@У�@�Q�@��
@���@�A ��Az�A��A  A33AQ�A�RA!�A'�A,��A0��A6ffA<(�A?\)ADz�AJ=qAN�RAR�\AX��A\(�A`��Ag
=Aj=qAp  Atz�Aw�A~{A�G�A�33A�p�A�Q�A�=qA�z�A�  A���A��
A�
=A�Q�A��A�{A��A�=qA�p�A�
=A��A�z�A�ffA��A�(�A�{A���A�(�A�p�A�Q�A�33A���A�\)Aʏ\A��AθRAљ�A�z�A�{Aأ�A��
A�A�Q�A�33A���A�  A�\A�z�A�RA��A�(�A�{A�G�A��
A�p�B Q�B��B�RB�BG�B�RB�B��B
=qB33Bz�B�B33B  BG�B�HB�
B��B�\B�B��B{B
=B(�B��B�HB�B ��B"ffB#�B$z�B%�B&�HB'�
B)G�B*�RB+�B,��B.{B/�B0z�B1p�B2�HB4(�B5�B6=qB7�
B8��B9�B;
=B<��B=��B>�\B@(�BAG�BB{BC�BD��BE�BF�HBHz�BI��BJ�\BK�
BM�BNffBO\)BP��BR=qBS33BT(�BUp�BW
=BW�
BX��BZ�\B[�B\z�B^=qB_33B`(�BaG�Bb�RBd(�Bd��Be�Bg�Bh��Bi��Bj�RBl(�BmG�Bn{Bo�Bp��Bq�Br�HBtQ�Bu��Bv�\Bw�By�Bz{B{
=B|z�B}�B~�HB�
B���B�G�B�B�ffB��B��B�(�B��HB���B�{B��\B�G�B��B��\B�
=B��B�z�B���B��B�(�B��HB�\)B��B���B�\)B��
B�Q�B���B�B�ffB��HB�p�B�=qB��RB�33B��B��RB�G�B�B�ffB��B�B�Q�B��HB�p�B�=qB��HB�G�B��B���B�G�B��B�Q�B�
=B�B�=qB��RB�\)B�{B��RB�33B�B��\B�G�B��B�=qB�
=B�B�=qB���B��B�=qB���B�G�B��B��RB�\)B��
B�z�B�G�B��
B�Q�B�
=B�B�Q�B��HB�p�B�{B���B��B�  B��\B�G�B�  B���B�33B��B�Q�B��B��B�(�B���B��B�(�B��\B�33B�  B�ffB��HBÙ�B�Q�B���B�G�B�  BƸRB�G�B�B�Q�B���BɮB�Q�B���B�G�B�  B̸RB�33B�B�=qB���Bϙ�B�=qBиRB�G�B�  BҸRB�\)B��B�ffB�
=B�B�ffB��HB�p�B�=qB���B�\)B��
B�z�B�33B�B�=qB���B݅B�(�B޸RB�33B�B�z�B�
=B�B�  B�RB�\)B��B�ffB���B�B�=qB���B�\)B�(�B�RB�33B��B��B��B�B�z�B�
=B홚B�Q�B���B�B�  B�RB�p�B��
B�\B�G�B�B�=qB���B��B�(�B��RB��B��B�ffB�33B��B�ffB��HB��B�=qB��HB�G�B�B��\B�
=B�p�C {C ffC �C �C(�C�\C�
C{CQ�C��C  CG�Cz�C�
C(�C�CC  CG�C��C�C(�C\)C�RC
=CQ�C�\C��C�Cp�CC	  C	G�C	��C
  C
=qC
�C
�HC=qCz�CC(�Cz�C�RC  CQ�C��C��C33C�\C�C=qC�C��C{Cz�C��C(�CffC�C��CQ�C�C  CG�C�\C�HCG�C��C�
C(�C�C�C=qC�C�
C�Cp�C��C(�Cp�CC
=CQ�C��C
=C\)C�RC�C33C�\C�HC33C�\C�
C{CQ�C�C  C\)C�RC��C=qC��C�C G�C ��C �C!=qC!z�C!��C"�C"z�C"�
C#(�C#z�C#C$
=C$\)C$�C%{C%p�C%C&
=C&Q�C&��C'  C'Q�C'�RC(
=C(Q�C(��C(�C)Q�C)�C*  C*G�C*�\C*�C+G�C+��C,  C,\)C,�C-  C-Q�C-��C-�C.G�C.��C.��C/\)C/�C0  C0Q�C0�\C0�
C1(�C1z�C1�
C2(�C2ffC2��C2�HC3(�C3p�C3C4  C4=qC4p�C4�C4�HC5
=C5G�C5�\C5C5��C6(�C6\)C6�C6��C6��C7
=C7=qC7p�C7��C7�
C8
=C833C8Q�C8z�C8�RC8�C9(�C9Q�C9p�C9��C9��C:
=C:33C:\)C:�C:�C:�HC;�C;Q�C;�C;�C;�
C<  C<(�C<Q�C<�\C<C<��C=(�C=\)C=�C=�C=��C>  C>G�C>p�C>�C>�
C?  C?�C?Q�C?z�C?�RC?��C@�C@=qC@ffC@�C@�RC@�CA(�CA\)CA�CA��CACA�CB�CBG�CBz�CB�RCB�HCC�CCG�CC\)CC�\CC��CC��CD33CD\)CD�\CD�CD�HCE
=CE=qCE�CE�RCE�HCF
=CF33CFffCF�CF�HCG{CGG�CGffCG�\CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                               111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@�@B�\@�G�@��R@�p�@�p�A   A  A   A,��A@  A`  A�  A�  A�  A�  A�Q�A�Q�A�Q�A�Q�B Q�BQ�BQ�B(�B (�B((�B/�
B7�
B@  BH  BP(�BX(�B`  Bg�Bo�
Bx(�B�{B��B��
B�  B�  B��B�  B�  B�(�B�  B�{B�{B�{B�  B��B�  B�{B��
B��
B�  B�(�B�{B�  B�  B�  B��B��B�{B�{B�{B�  B�  C 
=C�C  C  C  C
  C
=C
=C  C��C  C�C
=C  C
=C
=C   C!�C#�C%��C(  C*
=C,  C-��C0�C2{C4{C6{C8  C:  C<  C>
=C@  CB  CD
=CF  CH
=CI��CL
=CN{CP  CR  CT
=CV  CX  CY��C\  C^
=C`  Cb  Cd  Cf
=Ch  Ci��Cl  Cm��Co��Cq��Cs�Cu��Cx  Cz
=C|
=C~  C�C�  C���C���C�  C�  C���C�  C�  C���C���C�  C�  C�  C�C���C�  C�  C���C���C�  C�C�C�  C���C���C�  C�C�C�  C�  C�C�  C���C���C�  C�  C���C�  C�  C�  C���C���C�  C�C�
=C�C�  C�  C�C�
=C�C�  C�  C���C���C�  C�  C�  C�C�C�  C�  C�  C���C�  C�C�  C�C�
=C�C�  C���C�C�C�  C�  C�  C�C�C�  C�  C�C�C�
=C�
=C�  C���C���C�  C�  C���C���C�  C���C�  C�  C���C�  C�  C�  C�  C���C���C�C�C���C���C�C�  C�  C�  C���C�  C���C�  C�C�  C�C�  C�  C�
=C�C���C���C�  C�  C���D   D �D  Dz�D  D��D  D� D  D}qD  D� D  Dz�D��D� D�D��D	  D	� D
�D
� D
�qD� D  D��D  Dz�D  D�DD��D�D� D  D}qD��D� D�D�D�D�D�D��D  D� D�D�D�Dz�D  D��D  D� D�qD��D�D��D�D� D  D� D  D� D   D � D!�D!�D"  D"� D#  D#� D$�D$��D%�D%��D&D&� D&�qD'��D(�D(}qD)  D)��D)�qD*z�D*��D+� D,�D,��D-�D-� D-��D.� D.�qD/z�D/��D0}qD1  D1��D2  D2� D3  D3}qD4  D4� D5  D5��D6  D6}qD7�D7� D8  D8��D9  D9}qD9�qD:� D;�D;� D<�D<�D=�D=�D>  D>� D?  D?��D@D@� DA  DA��DBDB��DC�DC��DD�DD� DD�qDE��DFDF��DG  DG� DH�DH��DI  DI��DJDJ��DK  DK� DL�DL��DM  DM}qDN  DN�DODO� DO�qDP}qDP��DQz�DR  DR�DSDS� DS�RDTxRDT�RDU� DVDV��DV�qDW��DX  DXz�DX�qDY}qDZ  DZ� D[�D[��D[�qD\}qD]�D]��D]�qD^� D_  D_}qD`  D`z�D`��Da}qDa�qDbz�Db�qDc� DdDd�Dd�qDe}qDf  Df��Dg�Dg�Dh�Dh� Di�Di� Di�qDj��Dk�Dk}qDk��Dl}qDm  Dmz�Dm�qDn}qDn��Do}qDo�qDp� Dq  Dq}qDr  Dr��Ds  Ds� Dt  Dt� Du  Du��Dv�Dv}qDw  Dw� Dx  Dx��Dy  Dy� Dz�Dz� Dz�qD{}qD|  D|}qD|�qD}}qD~  D~}qD~�qD}qD�  D�B�D�� D�� D�HD�@ D�~�D���D��qD�=qD�~�D�� D���D�>�D��HD��HD�  D�@ D�� D�� D�  D�>�D�~�D��HD�  D�AHD��HD�� D�HD�@ D�~�D�� D�HD�AHD��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?��?8Q�?k�?�{?�p�?�@\)@(�@0��@E�@Tz�@h��@�  @��@��@��R@��
@��@��H@\@У�@�Q�@��
@���@�A ��Az�A��A  A33AQ�A�RA!�A'�A,��A0��A6ffA<(�A?\)ADz�AJ=qAN�RAR�\AX��A\(�A`��Ag
=Aj=qAp  Atz�Aw�A~{A�G�A�33A�p�A�Q�A�=qA�z�A�  A���A��
A�
=A�Q�A��A�{A��A�=qA�p�A�
=A��A�z�A�ffA��A�(�A�{A���A�(�A�p�A�Q�A�33A���A�\)Aʏ\A��AθRAљ�A�z�A�{Aأ�A��
A�A�Q�A�33A���A�  A�\A�z�A�RA��A�(�A�{A�G�A��
A�p�B Q�B��B�RB�BG�B�RB�B��B
=qB33Bz�B�B33B  BG�B�HB�
B��B�\B�B��B{B
=B(�B��B�HB�B ��B"ffB#�B$z�B%�B&�HB'�
B)G�B*�RB+�B,��B.{B/�B0z�B1p�B2�HB4(�B5�B6=qB7�
B8��B9�B;
=B<��B=��B>�\B@(�BAG�BB{BC�BD��BE�BF�HBHz�BI��BJ�\BK�
BM�BNffBO\)BP��BR=qBS33BT(�BUp�BW
=BW�
BX��BZ�\B[�B\z�B^=qB_33B`(�BaG�Bb�RBd(�Bd��Be�Bg�Bh��Bi��Bj�RBl(�BmG�Bn{Bo�Bp��Bq�Br�HBtQ�Bu��Bv�\Bw�By�Bz{B{
=B|z�B}�B~�HB�
B���B�G�B�B�ffB��B��B�(�B��HB���B�{B��\B�G�B��B��\B�
=B��B�z�B���B��B�(�B��HB�\)B��B���B�\)B��
B�Q�B���B�B�ffB��HB�p�B�=qB��RB�33B��B��RB�G�B�B�ffB��B�B�Q�B��HB�p�B�=qB��HB�G�B��B���B�G�B��B�Q�B�
=B�B�=qB��RB�\)B�{B��RB�33B�B��\B�G�B��B�=qB�
=B�B�=qB���B��B�=qB���B�G�B��B��RB�\)B��
B�z�B�G�B��
B�Q�B�
=B�B�Q�B��HB�p�B�{B���B��B�  B��\B�G�B�  B���B�33B��B�Q�B��B��B�(�B���B��B�(�B��\B�33B�  B�ffB��HBÙ�B�Q�B���B�G�B�  BƸRB�G�B�B�Q�B���BɮB�Q�B���B�G�B�  B̸RB�33B�B�=qB���Bϙ�B�=qBиRB�G�B�  BҸRB�\)B��B�ffB�
=B�B�ffB��HB�p�B�=qB���B�\)B��
B�z�B�33B�B�=qB���B݅B�(�B޸RB�33B�B�z�B�
=B�B�  B�RB�\)B��B�ffB���B�B�=qB���B�\)B�(�B�RB�33B��B��B��B�B�z�B�
=B홚B�Q�B���B�B�  B�RB�p�B��
B�\B�G�B�B�=qB���B��B�(�B��RB��B��B�ffB�33B��B�ffB��HB��B�=qB��HB�G�B�B��\B�
=B�p�C {C ffC �C �C(�C�\C�
C{CQ�C��C  CG�Cz�C�
C(�C�CC  CG�C��C�C(�C\)C�RC
=CQ�C�\C��C�Cp�CC	  C	G�C	��C
  C
=qC
�C
�HC=qCz�CC(�Cz�C�RC  CQ�C��C��C33C�\C�C=qC�C��C{Cz�C��C(�CffC�C��CQ�C�C  CG�C�\C�HCG�C��C�
C(�C�C�C=qC�C�
C�Cp�C��C(�Cp�CC
=CQ�C��C
=C\)C�RC�C33C�\C�HC33C�\C�
C{CQ�C�C  C\)C�RC��C=qC��C�C G�C ��C �C!=qC!z�C!��C"�C"z�C"�
C#(�C#z�C#C$
=C$\)C$�C%{C%p�C%C&
=C&Q�C&��C'  C'Q�C'�RC(
=C(Q�C(��C(�C)Q�C)�C*  C*G�C*�\C*�C+G�C+��C,  C,\)C,�C-  C-Q�C-��C-�C.G�C.��C.��C/\)C/�C0  C0Q�C0�\C0�
C1(�C1z�C1�
C2(�C2ffC2��C2�HC3(�C3p�C3C4  C4=qC4p�C4�C4�HC5
=C5G�C5�\C5C5��C6(�C6\)C6�C6��C6��C7
=C7=qC7p�C7��C7�
C8
=C833C8Q�C8z�C8�RC8�C9(�C9Q�C9p�C9��C9��C:
=C:33C:\)C:�C:�C:�HC;�C;Q�C;�C;�C;�
C<  C<(�C<Q�C<�\C<C<��C=(�C=\)C=�C=�C=��C>  C>G�C>p�C>�C>�
C?  C?�C?Q�C?z�C?�RC?��C@�C@=qC@ffC@�C@�RC@�CA(�CA\)CA�CA��CACA�CB�CBG�CBz�CB�RCB�HCC�CCG�CC\)CC�\CC��CC��CD33CD\)CD�\CD�CD�HCE
=CE=qCE�CE�RCE�HCF
=CF33CFffCF�CF�HCG{CGG�CGffCG�\CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                               111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A��A��A���A��A��A��A��A��A��A��A��A��A��mA���A�VA��A��A��A�{A�{A�oA�bA�VA�VA�VA�
=A�1A�1A�1A�%A�A�A�  A���A���A���A���A���A��A��A��A��A��A��A��A��A��yA��TA���Aԇ+A�"�A��A���AӸRA�=qA�ĜA�5?A�|�AȰ!A��
Aǲ-A�jAĮA��A��A��`A��7A��A�/A�^5A���A�t�A�A���A��A�I�A�A�A��wA��uA��\A���A���A�%A���A���A���A���A���A�5?A��TA�~�A�C�A��
A�;dA��!A�A���A�l�A�-A�|�A��HA�9XA�(�A�JA���A��A�~�A}ƨA|�A{oAy�-Ax��AvA�Ar��Ao�Ak�AiVAf��AcdZAa"�A`jA_��A]�
AZ�9AX��AXA�AV�HAS�
AP��AMAK�
AJ=qAI��AH��AG�-AE+ABbNA@��A?t�A>��A=��A;"�A:Q�A9��A9;dA8�DA7�#A5�A3�A1�A1C�A/��A/�7A/&�A.9XA,��A*��A(ȴA'�
A'dZA&��A&{A%oA"1'A�AƨA��A=qAK�AA�A�RAA7LA�9A��A�A��A&�An�AS�Ax�A��AVA�7AĜA�\A^5A�TAA�A?}A"�A
��A
ffA	�
A�AM�AJA�hA�A��AZA{A�yA1'A�AK�AI�A �j@�|�@�ff@�33@�V@�1'@�\)@��@���@��+@�@�O�@�"�@�&�@�1'@�S�@��@�v�@�@띲@��@�w@��@�@�@�S�@��@�R@�^@��@�n�@� �@��;@߾w@ߍP@�+@�{@�?}@��@ۅ@۶F@�  @�K�@�o@�;d@ڧ�@�p�@�7L@�Q�@ו�@�"�@�V@׮@�r�@�t�@��#@��`@�z�@�I�@�33@�-@���@�b@�o@�o@�C�@��@���@���@Η�@θR@�ff@�5?@�@�O�@�A�@�A�@�dZ@ɺ^@��@ȼj@ȴ9@�r�@�ƨ@�S�@���@���@�^5@�hs@ř�@őh@�/@��@��@� �@���@Ý�@î@�t�@�;d@���@���@�ȴ@�@�5?@��T@��^@�7L@��@��@�=q@�ff@§�@���@�n�@�/@�A�@��@�K�@��+@��#@�hs@���@�ȴ@���@�n�@�-@�X@��@�  @���@�t�@�;d@���@���@�G�@���@��9@���@�p�@�&�@���@���@�
=@��@��R@��\@�n�@��@�?}@�  @�S�@���@���@���@��+@�v�@�$�@���@���@��@��@��@�=q@�{@��7@��`@�bN@�1@��@�o@���@���@�-@���@���@�G�@���@��j@�z�@�A�@���@��F@���@��@�C�@��R@�=q@��@��T@�@���@���@��7@��@�/@���@�I�@���@��
@��@��@�S�@�33@�"�@��@�M�@��-@�G�@���@��u@�j@�bN@�Z@�A�@�b@��m@��P@�dZ@�33@���@���@���@�/@��@�r�@�bN@�9X@��
@���@�dZ@�33@�+@�"�@�^5@�J@��-@�x�@���@�j@�bN@�9X@��
@�ƨ@��F@��@�|�@�C�@��!@�n�@�n�@�ff@�M�@�J@���@���@�7L@��u@��@�S�@�@��+@�{@�@�J@���@�/@��/@��9@�j@�(�@�b@��@��
@��@�dZ@�33@�@��@�ȴ@��+@�V@�J@���@�G�@���@���@��9@��@�1'@�1@��w@�C�@��H@�ȴ@�v�@�$�@�J@�@��T@��#@��#@���@��-@��h@��@�Ĝ@�Q�@�1'@�1@��m@��@�S�@�ȴ@�5?@���@��h@��@�G�@�V@�%@���@���@��@�j@��@�1@�@�P@
=@~ȴ@~ff@~$�@}�T@}��@}�@}/@|9X@{dZ@z��@zM�@z-@y�#@yhs@yXG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A���A��A��A���A��A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A���A��A���A��A��A��A��A��A��A��A��A��A��A��`A��`A��yA��HA��mA��mA��;A��/A���A���Aա�AՓuAՅAՃA�dZA�-A� �A�"�A��A� �A�"�A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A�oA�oA��A��A�oA�{A��A�oA�{A��A�oA��A��A�bA�{A��A�oA�bA�{A�oA�VA�oA�oA�JA�VA�oA�VA�JA�bA�oA�JA�VA�bA�JA�JA�bA�bA�JA�VA�oA�VA�JA�bA�JA�VA�bA�JA�
=A�VA�JA�1A�
=A�JA�
=A�%A�
=A�1A�A�1A�
=A�1A�%A�1A�
=A�1A�%A�
=A�JA�%A�%A�
=A�
=A�%A�%A�
=A�1A�A�%A�1A�A�A�1A�A�  A�%A�A�  A�A�%A�A�  A�A�%A�A�  A�  A�A���A���A�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A��A���A���A��A��A���A���A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��A��A��A��mA��yA��A��A��mA��mA��yA��mA��TA��;A��TA��TA��/A��
A��A��
A�ȴA�ĜAԺ^AԶFAԧ�AԓuA�~�A�`BA�C�A�7LA�1'A�+A��A�{A�oA�VA�A���A��`A��HA��HA��TA��;A��#A���A���A���A�ȴA���A���A�AӾwAӮAӮAӧ�Aә�AӅA�l�A�O�A��AҴ9A�r�A�-A��;A�Q�A���A���AμjA�%Aͺ^A͋DA�hsA��A̝�A�bA˗�A�?}A�1Aʺ^A�+AɅA�{A���A��;A���Aȣ�A�~�A�&�A�%A��A�ƨA�ĜA�ƨA���A���A�ƨA�ƨAǶFAǮAǣ�Aǟ�AǙ�AǋDAǅA�t�A�jA�XA�1'A��HA�/A�jA��;A���A��A�|�A�A��RA�Q�A��A���A�ZA�oA��A�A��A�^5A�A�A�7LA�/A�{A���A��A���A��A���A���A��uA��7A��A��A�|�A��A��^A��^A��FA��A��\A�C�A��`A���A�ZA�A���A���A��A��+A��A�r�A�`BA�VA�E�A��;A�-A��RA�9XA���A�1A�E�A�z�A��DA���A�+A�ȴA���A��^A��9A���A�p�A�;dA���A��jA�G�A��A�A�A��/A���A�l�A�9XA�
=A���A�VA��/A�JA���A�&�A�A�XA���A�hsA�{A���A���A��DA��A�x�A�ffA�C�A���A��jA�9XA��A���A��!A�VA�(�A�bA���A���A��PA�C�A�1A��yA���A��-A��7A�t�A�^5A�O�A�?}A�oA�ZA�"�A��;A�ȴA��^A��FA��A��A��A�t�A�p�A�I�A�
=A�JA��mA�ȴA��!A���A�r�A���A�?}A���A���A�r�A�O�A�oA���A���A���A�;dA��A�ȴA��DA�p�A�dZA�-A�$�A�JA��A��HA��;A���A���A��-A��!A���A��hA��A�jA�VA�5?A�(�A�"�A��A�1A�JA�A���A��A���A�x�A�oA�ĜA��A�33A�"�A���A��A���A�;dA��A���A�1'A�K�A���A���A�|�A�t�A�^5A�+A��/A�p�A�  A��wA�;dA���A�C�A�-A�=qA���A��hA��A��A���A�A���A�ĜA�|�A�\)A�K�A�(�A�VA���A���A���A��A��#A��-A�|�A�/A��A�A���A��7A�t�A�Q�A�C�A�5?A�5?A�7LA�=qA�=qA�?}A�;dA�;dA�7LA�/A�+A�&�A�$�A��A�bA���A���A�ƨA���A��DA��A�l�A�VA�-A� �A��A�JA�%A�  A���A���A��A��TA���A��wA���A��hA��A�|�A�hsA�\)A�S�A�C�A�-A��A���A��/A�ȴA��FA���A��hA��A�x�A�ZA��mA��HA���A��A�x�A�/A�?}A���A�1'A��A��A���A��uA�~�A�ffA�M�A�5?A� �A��A���A�ĜA��+A�\)A�G�A�33A��A�bA�
=A�%A�1A�
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                               111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A���A��A��A��A��A��A��A��A��A��A��mA���A�VA��A��A��A�{A�{A�oA�bA�VA�VA�VA�
=A�1A�1A�1A�%A�A�A�  A���A���A���A���A���A��A��A��A��A��A��A��A��A��yA��TA���Aԇ+A�"�A��A���AӸRA�=qA�ĜA�5?A�|�AȰ!A��
Aǲ-A�jAĮA��A��A��`A��7A��A�/A�^5A���A�t�A�A���A��A�I�A�A�A��wA��uA��\A���A���A�%A���A���A���A���A���A�5?A��TA�~�A�C�A��
A�;dA��!A�A���A�l�A�-A�|�A��HA�9XA�(�A�JA���A��A�~�A}ƨA|�A{oAy�-Ax��AvA�Ar��Ao�Ak�AiVAf��AcdZAa"�A`jA_��A]�
AZ�9AX��AXA�AV�HAS�
AP��AMAK�
AJ=qAI��AH��AG�-AE+ABbNA@��A?t�A>��A=��A;"�A:Q�A9��A9;dA8�DA7�#A5�A3�A1�A1C�A/��A/�7A/&�A.9XA,��A*��A(ȴA'�
A'dZA&��A&{A%oA"1'A�AƨA��A=qAK�AA�A�RAA7LA�9A��A�A��A&�An�AS�Ax�A��AVA�7AĜA�\A^5A�TAA�A?}A"�A
��A
ffA	�
A�AM�AJA�hA�A��AZA{A�yA1'A�AK�AI�A �j@�|�@�ff@�33@�V@�1'@�\)@��@���@��+@�@�O�@�"�@�&�@�1'@�S�@��@�v�@�@띲@��@�w@��@�@�@�S�@��@�R@�^@��@�n�@� �@��;@߾w@ߍP@�+@�{@�?}@��@ۅ@۶F@�  @�K�@�o@�;d@ڧ�@�p�@�7L@�Q�@ו�@�"�@�V@׮@�r�@�t�@��#@��`@�z�@�I�@�33@�-@���@�b@�o@�o@�C�@��@���@���@Η�@θR@�ff@�5?@�@�O�@�A�@�A�@�dZ@ɺ^@��@ȼj@ȴ9@�r�@�ƨ@�S�@���@���@�^5@�hs@ř�@őh@�/@��@��@� �@���@Ý�@î@�t�@�;d@���@���@�ȴ@�@�5?@��T@��^@�7L@��@��@�=q@�ff@§�@���@�n�@�/@�A�@��@�K�@��+@��#@�hs@���@�ȴ@���@�n�@�-@�X@��@�  @���@�t�@�;d@���@���@�G�@���@��9@���@�p�@�&�@���@���@�
=@��@��R@��\@�n�@��@�?}@�  @�S�@���@���@���@��+@�v�@�$�@���@���@��@��@��@�=q@�{@��7@��`@�bN@�1@��@�o@���@���@�-@���@���@�G�@���@��j@�z�@�A�@���@��F@���@��@�C�@��R@�=q@��@��T@�@���@���@��7@��@�/@���@�I�@���@��
@��@��@�S�@�33@�"�@��@�M�@��-@�G�@���@��u@�j@�bN@�Z@�A�@�b@��m@��P@�dZ@�33@���@���@���@�/@��@�r�@�bN@�9X@��
@���@�dZ@�33@�+@�"�@�^5@�J@��-@�x�@���@�j@�bN@�9X@��
@�ƨ@��F@��@�|�@�C�@��!@�n�@�n�@�ff@�M�@�J@���@���@�7L@��u@��@�S�@�@��+@�{@�@�J@���@�/@��/@��9@�j@�(�@�b@��@��
@��@�dZ@�33@�@��@�ȴ@��+@�V@�J@���@�G�@���@���@��9@��@�1'@�1@��w@�C�@��H@�ȴ@�v�@�$�@�J@�@��T@��#@��#@���@��-@��h@��@�Ĝ@�Q�@�1'@�1@��m@��@�S�@�ȴ@�5?@���@��h@��@�G�@�V@�%@���@���@��@�j@��@�1@�@�P@
=@~ȴ@~ff@~$�@}�T@}��@}�@}/@|9X@{dZ@z��@zM�@z-@y�#@yhsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A���A��A��A���A��A���A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A���A��A���A��A��A��A��A��A��A��A��A��A��A��`A��`A��yA��HA��mA��mA��;A��/A���A���Aա�AՓuAՅAՃA�dZA�-A� �A�"�A��A� �A�"�A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A�oA�oA��A��A�oA�{A��A�oA�{A��A�oA��A��A�bA�{A��A�oA�bA�{A�oA�VA�oA�oA�JA�VA�oA�VA�JA�bA�oA�JA�VA�bA�JA�JA�bA�bA�JA�VA�oA�VA�JA�bA�JA�VA�bA�JA�
=A�VA�JA�1A�
=A�JA�
=A�%A�
=A�1A�A�1A�
=A�1A�%A�1A�
=A�1A�%A�
=A�JA�%A�%A�
=A�
=A�%A�%A�
=A�1A�A�%A�1A�A�A�1A�A�  A�%A�A�  A�A�%A�A�  A�A�%A�A�  A�  A�A���A���A�A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A��A���A���A��A��A���A���A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��A��A��A��mA��yA��A��A��mA��mA��yA��mA��TA��;A��TA��TA��/A��
A��A��
A�ȴA�ĜAԺ^AԶFAԧ�AԓuA�~�A�`BA�C�A�7LA�1'A�+A��A�{A�oA�VA�A���A��`A��HA��HA��TA��;A��#A���A���A���A�ȴA���A���A�AӾwAӮAӮAӧ�Aә�AӅA�l�A�O�A��AҴ9A�r�A�-A��;A�Q�A���A���AμjA�%Aͺ^A͋DA�hsA��A̝�A�bA˗�A�?}A�1Aʺ^A�+AɅA�{A���A��;A���Aȣ�A�~�A�&�A�%A��A�ƨA�ĜA�ƨA���A���A�ƨA�ƨAǶFAǮAǣ�Aǟ�AǙ�AǋDAǅA�t�A�jA�XA�1'A��HA�/A�jA��;A���A��A�|�A�A��RA�Q�A��A���A�ZA�oA��A�A��A�^5A�A�A�7LA�/A�{A���A��A���A��A���A���A��uA��7A��A��A�|�A��A��^A��^A��FA��A��\A�C�A��`A���A�ZA�A���A���A��A��+A��A�r�A�`BA�VA�E�A��;A�-A��RA�9XA���A�1A�E�A�z�A��DA���A�+A�ȴA���A��^A��9A���A�p�A�;dA���A��jA�G�A��A�A�A��/A���A�l�A�9XA�
=A���A�VA��/A�JA���A�&�A�A�XA���A�hsA�{A���A���A��DA��A�x�A�ffA�C�A���A��jA�9XA��A���A��!A�VA�(�A�bA���A���A��PA�C�A�1A��yA���A��-A��7A�t�A�^5A�O�A�?}A�oA�ZA�"�A��;A�ȴA��^A��FA��A��A��A�t�A�p�A�I�A�
=A�JA��mA�ȴA��!A���A�r�A���A�?}A���A���A�r�A�O�A�oA���A���A���A�;dA��A�ȴA��DA�p�A�dZA�-A�$�A�JA��A��HA��;A���A���A��-A��!A���A��hA��A�jA�VA�5?A�(�A�"�A��A�1A�JA�A���A��A���A�x�A�oA�ĜA��A�33A�"�A���A��A���A�;dA��A���A�1'A�K�A���A���A�|�A�t�A�^5A�+A��/A�p�A�  A��wA�;dA���A�C�A�-A�=qA���A��hA��A��A���A�A���A�ĜA�|�A�\)A�K�A�(�A�VA���A���A���A��A��#A��-A�|�A�/A��A�A���A��7A�t�A�Q�A�C�A�5?A�5?A�7LA�=qA�=qA�?}A�;dA�;dA�7LA�/A�+A�&�A�$�A��A�bA���A���A�ƨA���A��DA��A�l�A�VA�-A� �A��A�JA�%A�  A���A���A��A��TA���A��wA���A��hA��A�|�A�hsA�\)A�S�A�C�A�-A��A���A��/A�ȴA��FA���A��hA��A�x�A�ZA��mA��HA���A��A�x�A�/A�?}A���A�1'A��A��A���A��uA�~�A�ffA�M�A�5?A� �A��A���A�ĜA��+A�\)A�G�A�33A��A�bA�
=A�%A�1A�
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                               111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��BkB�BkB=B�BkBkB�B�B7BBkBkBB�B�B�B�B�B�B�B�BxBxB�B�B�BBBBB�BB�BB�BIB�B�B~BBBIB�BIB�BBxB�B+B�B
�B�BB
�"B
rBHBM6B}"B��B�bB��B�[B�B��BݘB�`B��B��B��B�B�B�BB1BJB�JB�GB�B�EB�2B�6B�LB��B��B~]BtBqBl�Bh
B_�BPB3�BkB
�VB
ԕB
��B
��B
�hB
��B
�B
�VB
�B
s�B
\�B
Z�B
U�B
EB
9$B
3hB
.IB
%�B
�B
	�B	�DB	�B	��B	��B	��B	�wB	�XB	��B	��B	�xB	�B	z�B	t�B	iDB	XEB	H�B	=B	6�B	1[B	/B	'RB	$@B	7B	MB	�B	�B	�B	�B	�B	hB	4B	bB	 B	�B	�B	�B	.B	�B	�B	.B	.B	
�B	�B	
rB	:B	�B	$B	%�B	%B	�B	�B	uB�B�sB�vBںB՛B�B�HBбB�aB�yB��B�?B�yB�]B�B�BB�TB�B��B��B�WB�5B�5B��B�GB�B��B��B��B	�B	�B	�B	�B		�B	�B	�B	�B	B	{B	�B	�B	�B	~B	B	�B	IB	 'B	"�B	%FB	(�B	+B	,qB	,=B	)�B	0�B	1�B	1�B	.B	.IB	.�B	4�B	7�B	9XB	D�B	IB	G�B	L0B	M�B	N<B	M�B	OvB	OvB	S[B	K�B	K�B	MB	QB	R�B	VB	[�B	a�B	d�B	f�B	kQB	m)B	o�B	q�B	r|B	oiB	o5B	sB	p�B	s�B	x8B	~�B	�B	�	B	��B	��B	cB	cB	�;B	.B	�iB	�B	�_B	�7B	�B	�SB	��B	�$B	��B	��B	�7B	�7B	�~B	��B	�B	�:B	�FB	�LB	�RB	��B	�*B	��B	��B	�'B	�hB	��B	�tB	�0B	��B	��B	��B	��B	�B	ǮB	��B	�KB	�B	��B	�RB	�XB	�^B	�dB	��B	��B	�jB	��B	�HB	бB	��B	�vB	��B	��B	�
B	�B	�B	�>B	�B	�sB	��B	�B	�mB	��B	� B	�NB	�ZB	�`B	��B	�B	��B	�)B	��B	�B	�/B	��B	�B	��B	�B	�|B	�B
 �B
oB
�B
�B
�B
MB
B
�B
�B
YB
�B
	�B

	B

rB

rB

rB

rB
B

�B

�B
�B
(B
�B
B
B
�B
�B
�B
B
�B
MB
�B
�B
�B
�B
�B
�B
�B
�B
_B
�B
�B
1B
eB
eB
�B
xB
B
B
�B
�B
B
B
�B
B
�B
�B
 �B
 �B
 �B
 �B
!bB
!bB
!-B
!-B
!-B
"�B
#B
$@B
$�B
%zB
%�B
&B
&B
&�B
&�B
&�B
($B
'�B
($B
($B
'RB
)�B
)_B
)*B
(�B
(�B
)_B
)�B
*0B
*�B
+6B
+�B
,B
/B
.�B
/OB
/OB
0�B
0�B
0!B
0�B
1�B
1�B
1�B
1�B
2aB
2-B
49B
4B
4B
3�B
4B
4nB
4�B
5B
5�B
6�B
8RB
8RB
9$B
:*B
:*B
9�B
9�B
9�B
9�B
9$B
9XB
9�B
9XB
9�B
9�B
9�B
:*B
:�B
:�B
;0B
:�B
:�B
;�B
<jB
=B
=<B
=�B
=�B
>B
>B
>BB
>wB
>wB
>�B
?B
?�B
?�B
@�B
@�B
@�B
@�B
AUB
AUB
AUB
AUB
AUB
A�B
A�B
B�B
B�B
C-B
CaB
CaB
C�B
DgB
E9B
GB
G�B
HB
HB
H�B
H�B
H�B
H�B
H�B
H�B
J#B
J#B
JXB
J#B
J�B
J�B
J�B
K)B
K^B
K�B
K�B
K^B
K�B
MB
NB
OBB
OvB
OvB
PB
PHB
PHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B	BkB�BqBqB+B�B�B�B=BkB�B�B�BkBBkBBqBB�BkB�B	BeB=B�B�B�B�B�BqB�B�BqB7B�B�B�B�BkB�B_BkB�B�BCBeB�B=BCBeB7B$�B%FBxBB#:B�B �B	BBCBqB�BB�B�B�BB~B�BxB�B�BqB~BIBBIB�BCBCB�B�BCBOBB�B�BxBB�B�B�BB�B�B�B~BCBBOB�BqB�BIBCB�BIB�BqBIB�B�BB~B�B�BB~B�B~B�BBIB�BBB�BBB�B~B�B�B�BB�BB�B�BBBBBB�B�BCB�B�B�BB�BOBxBxBBB�B~B�BCB�B�BIBCBxB�B�BBxBB~B�B�B~BB�B�BBCB~BBCB�B~B�BCBxB~B�B�BBB�B�B�B�BIBCB�B�B�B~BB�BBBOB�BB�B~BBxBBBCBBOB�BCB�BOBIB�BBBB�B~B�BBCBIBB�BCBxB�B�B�BBOBIB�BCB�BB�BCBB�B�BxBxBOB�BB�BOB�B�BCBxBOB~BxBB�B~BxBxBIB~B�BqB�BCB�BqB�BqB=B�B�B7BeBB B�B�B�BVBPBPB�B�BxBB	�B�BYB�B�BYBYBB{BMBuBBBuB
��B
�VB
��B
�VB
��B
�xB�B
��B
�	B
��B
��B
��B5tB�B$B@OB>wBH�BL0BN�BT�BF?B?}B.�B/�B=�BT,Bp�B}VBx�Bx8Bz�By�B� B�SB��B��B��B��B��B�4B��B��B��B��B� B�uB��B��B��B��B��B�@B�:B�B��B��B�#BҽB��B�B�B�|B�B�BޞB��B�WB��B�?BٴB�HBݘB�]B��B��B�B��B��B��B�]B�5B��B�B�fB�2B�B��BYB�AB�]B�B�|B�B��B�B�B�TB�%B�B�B��B�;B��B��B��B�B�BVB�BGBbB�B�B�B�B�B~B#nB"B1B�B�B�B\B
	BB�B�B0UB$BuB�BB�B"B 'B�BCB*0BkB!�BeB�B�B�BbB�B��B��B��B�xB��B�xB�B�|B%B��B�B�B��B�B��B�`B�
B��B�iB�BںB��B�#B��B֡BӏB�TB�&B��B��B��B�UB�HB�}B�jB��B�<B��B�B�B�gB�B�3B��B��B�3B�!B�9B��B�RB��B�tB��B��B�B��B�PB�zB��B�B�4B�lBzDBv+B� Bm)BzBs�BuZBr|Br|BrGBu�BncBy	BncBp;Bm)BjKBqBj�Bk�Bi�Bf�Bf2Bg8Be,Bh�BhsBf�Bp;Be�B_�BU�BV9BP�BRTBU�BZ�BD3BF�BIBX�B4nB*�B$�B!-B �B"�B 'B%FBFBFB:BB
	B \BB
��B
�pB
�B
��B
�B
��B
�aB
ɆB
�vB
�B
�tB
��B
�mB
��B
�B
�6B
�qB
��B
�HB
��B
��B
��B
�'B
��B
�OB
�wB
��B
�CB
��B
�B
��B
�}B
��B
��B
�hB
�hB
��B
��B
��B
�nB
��B
�B
��B
�tB
�LB
��B
�B
�B
�XB
�B
��B
�B
��B
��B
��B
��B
�hB
�B
�tB
��B
�B
��B
�B
�B
��B
��B
��B
�:B
�!B
��B
�B
��B
�OB
�VB
�B
��B
�_B
��B
�B
�oB
��B
��B
�+B
��B
��B
�DB
�GB
�.B
�B
zxB
t�B
zDB
m]B
e`B
h>B
aHB
d�B
d�B
a�B
`�B
Y�B
[�B
_�B
`�B
]dB
XEB
ZB
Z�B
W?B
V�B
W�B
XEB
W�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                               444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                               444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022051613095520220516130955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022051719011520220517190115QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022051719011520220517190115QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194520230210131945IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                