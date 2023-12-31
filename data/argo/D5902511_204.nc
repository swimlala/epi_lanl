CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-03-16T17:09:39Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�     p  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Qx   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     p  V�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   k   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     p  p    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     p  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     p  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                       SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                       SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                        SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T &    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   &T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   &\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   &d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   &l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � &t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   &�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   '   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    '   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        '8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        '@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       'H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    'PArgo profile    3.1 1.2 19500101000000  20220316170939  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_204                 6810_008521_204                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��*��Q�@��*��Q�11  @��+C,�@��+C,�@0sC,�z@0sC,�z�d��䣃'�d��䣃'11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @B�\@}p�@��R@�  @�  A ��A  A   A*�HA@  A`  A�  A��A��A��A�  A�Q�A�  A�B   BQ�B�
B  B (�B((�B0(�B8  B@(�BHQ�BP(�BX  B`  Bh  Bo�
Bw�
B�  B�{B�(�B�  B��B�  B�  B��B�  B�(�B�{B�{B�  B�  B�{B�{B�{B��B��
B�  B��B��B�  B�  B��B��B��B��B��B�  B�{B�  B��C  C
=C
=C  C	��C��C  C�C  C{C
=C  C  C��C  C 
=C!��C$  C&�C(
=C*  C,{C.  C0  C2  C4  C6{C8
=C:{C<
=C=��C@  CA��CC�CE��CH
=CJ  CL  CM��CP  CR  CT
=CV
=CX  CY��C[��C]��C`  Cb  Cd  Cf  Ch  Cj
=Cl  Cn  Cp
=Cr
=Ct{Cv
=Cx  Cy�C{��C~  C�C�  C�  C�C�  C�  C���C�  C�  C�  C�  C���C���C�
=C�C���C�  C�C�  C���C�C���C���C���C�  C�  C�  C���C���C���C�  C�C�  C���C�  C�  C���C���C�  C���C�  C���C���C���C���C�  C�  C�  C�C�C�  C�  C�
=C�C���C�  C�C�
=C�C�  C�C�C�C�C�C�  C���C�  C�  C�  C�C�C�  C�  C�C���C���C�  C���C�  C�C�C���C���C���C�  C�  C���C���C���C�  C�C�  C���C���C�  C�C���C���C�C�C���C�C�
=C�
=C�
=C�C�
=C�C���C�  C�  C�  C�C�C�C�C�C�  C���C���C���C�  C�  C�  C�  C�
=C�D   D ��DD� D�qD}qD�qD� D�qD� D  D�D�D}qD  D}qD�D��D	  D	}qD	�qD
� D  D��D  D}qD�D� D  D}qD  D� D  D� D�qD� D�qDz�D�RD� D�D}qD  D�D�D� D  D� D  D}qD��Dz�D  D�D�D�D�D� D�qD}qD�qD}qD�qD}qD   D ��D!�D!� D!�qD"}qD#�D#�D$  D$}qD$�qD%}qD%�qD&��D'�D'��D(D(��D)�D)��D*�D*��D+  D+}qD+�qD,}qD-  D-}qD.  D.� D/  D/��D0�D0��D1  D1}qD2  D2� D3  D3� D4  D4� D5D5�D5�qD6}qD6�qD7� D8  D8}qD9  D9� D9�qD:}qD;  D;� D<  D<��D=D=� D=�qD>� D?  D?��D@D@��D@�qDA� DA�qDB� DC�DC}qDC�qDD}qDE  DE}qDE��DFz�DF�qDG��DHDH� DH�qDI}qDJ�DJ� DK  DK}qDL  DL��DMDM��DN  DN� DO  DO��DO�qDP}qDQ�DQ� DR  DR� DS  DS� DT  DT� DU  DU��DV�DV� DV�qDW}qDX�DX� DY�DY��DY��DZ}qD[  D[� D[�qD\}qD\�qD]� D^  D^�D_D_�D`�D`��D`�qDaxRDb  Db� Db��Dc� Dd  Dd��De  De� DfDf��Df�qDg� Dh�Dh}qDi  Di� Di�qDj��Dk  Dk}qDl�Dl� Dl�qDm��Dn�Dn}qDo  Do�Dp�Dp� Dq�Dq� Dq�qDrz�Dr�RDs� DtDt� Dt�qDu� Dv  Dv}qDv�qDw� Dx�Dx� Dy  Dy��Dz  Dz� D{  D{� D|�D|��D}  D}� D~�D~��D  D� D�qD�@ D���D��HD���D�@ D�� D�� D�HD�AHD���D��HD�  D�>�D�~�D���D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D���D���D�=qD�� D��HD�  D�@ D�� D�� D�HD�AHD�~�D���D�HD�!HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?�?W
=?��?�Q�?�G�@   @�R@5@B�\@^�R@u@��\@���@��R@��@���@�  @���@�@�  @�=q@�Q�A�\A
=Ap�A33AQ�A�RA%�A)��A0  A7
=A;�AAG�AG�AL(�AQG�AXQ�A\��Aa�Ah��An{As�
A{�A�  A��\A�{A���A��HA�ffA���A��
A�
=A�=qA�(�A��A��HA���A�  A�33A��A�  A��A�A�Q�A��
A�A���A�(�A�ffA�G�A�z�AθRA�G�A���A�
=A��A�p�A�  A�\A�{A��A�33A�RA���A�A�
=A�=qA�(�A�
=BG�B�\B�Bp�B�HB(�B	��B\)Bz�B{B�
B��BffB(�Bp�B�RB��B�B
=B��B{B\)B!G�B"{B#�B%G�B&ffB'�B)p�B*�\B+�
B-p�B.�HB0  B1B2�RB4(�B5B7
=B8(�B9p�B;33B<��B=��B?
=B@��BA�BB�HBDz�BF{BG33BHz�BJ=qBK33BL��BN=qBO\)BP��BR=qBS\)BT��BV=qBW�
BX��BZ=qB[�
B]�B^{B_�BaG�BbffBc�Bd��BfffBg�Bh��Bj=qBk�Bl��Bn=qBo�Bp��Br{Bs�Bt��Bu�Bw
=Bx��By�Bz�RB|��B}�B~�HB�(�B��HB�\)B�(�B��HB�\)B�{B��HB�\)B�  B���B�p�B��B��\B�p�B��B�z�B�G�B��
B�Q�B��B�B�=qB��HB��B�=qB��RB��B�(�B���B�\)B�(�B��\B��B��B��\B���B��B�z�B��HB��B�Q�B���B�p�B�=qB��RB�G�B�{B���B��B��B��\B�
=B���B�Q�B���B�\)B�{B���B�\)B��
B��\B�G�B�B�=qB��B��B�(�B��HB���B�  B��RB�p�B��B�ffB��B��B�(�B���B���B�(�B���B�\)B�{B���B��B��B���B�
=B��B�z�B�
=B��B�Q�B�
=B��B�(�B���B��B�  B��HB�p�B��B���B�\)B��B\B�\)B�  B�z�B�G�B�  B�z�B��B�  Bȏ\B��B��Bʏ\B�
=B�B̏\B��BͮB�z�B�G�B�B�Q�B��B��B�ffB��B��
B�z�B���B��
B֏\B��B׮B؏\B��BٮB�z�B�G�B�B�ffB�33B�  B�z�B�33B�  B��\B��B�  B�RB�33B�{B��HB�p�B�{B���B癚B�=qB�
=B��
B�\B��B��
B�RB�p�B�  B���B�B�Q�B���B�B��B�33B��B��HB��B�(�B�
=B��B�Q�B��B�  B��\B�33B�{B���B�\)B�{B���B��C �C z�C �C=qC��C  Cp�C�RC{Cz�C�CG�C��C��CffC��C�Cp�C�CQ�C��C��Cp�CC	�C	�\C	�HC
=qC
��C
=Cp�CC�Cz�C�CG�C��C��CffCC
=Cz�C�C33C�C  C\)C��C{C�C�
C33C�C  CQ�C�RC(�Cz�C��CG�C�C��CQ�CC(�Cp�C�HCG�C�C
=C\)C�C�C�\C�
C33C��C
=C\)C�C�C�C��C33C��C��C G�C �C!{C!ffC!�RC"�C"z�C"�HC#�C#�\C#��C$\)C$�C$��C%ffC%��C&�C&p�C&��C'33C'��C'��C(=qC(�C){C)\)C)�C*{C*z�C*�
C+�C+p�C+�HC,=qC,�C,��C-33C-�\C-�HC.�C.z�C.�HC/(�C/p�C/��C033C0�C0C1{C1z�C1��C2{C2\)C2C3{C3Q�C3��C4  C4Q�C4�C4��C5(�C5z�C5�RC5��C6G�C6��C6��C7{C7\)C7��C7��C8
=C8\)C8�\C8�C8��C9�C9=qC9p�C9�C9�
C9�C:{C:G�C:\)C:p�C:��C:��C:�C:��C;�C;G�C;Q�C;z�C;�C;�C;��C<  C<{C<�C<Q�C<ffC<p�C<��C<C<C<�
C=
=C=�C=(�C=Q�C=p�C=z�C=��C=��C=�C=��C>
=C>=qC>\)C>\)C>�C>�C>��C>�
C?
=C?33C?=qC?Q�C?�C?�C?��C?�
C@  C@(�C@33C@\)C@�\C@��C@C@��CA  CA�CAQ�CAz�CA��CA�CA��CB  CB�CB33CBffCB�\CB��CB�RCB�CC{CC(�CCG�CCp�CC��CC��CC�CD  CD(�CDQ�CD�CD��CD��CE  CE�CE=qCE\)CE�\CECE�
CF  CF33CFp�CF��CF��CF�HCG{CG=qCG�CG��CGCG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?u@   @B�\@}p�@��R@�  @�  A ��A  A   A*�HA@  A`  A�  A��A��A��A�  A�Q�A�  A�B   BQ�B�
B  B (�B((�B0(�B8  B@(�BHQ�BP(�BX  B`  Bh  Bo�
Bw�
B�  B�{B�(�B�  B��B�  B�  B��B�  B�(�B�{B�{B�  B�  B�{B�{B�{B��B��
B�  B��B��B�  B�  B��B��B��B��B��B�  B�{B�  B��C  C
=C
=C  C	��C��C  C�C  C{C
=C  C  C��C  C 
=C!��C$  C&�C(
=C*  C,{C.  C0  C2  C4  C6{C8
=C:{C<
=C=��C@  CA��CC�CE��CH
=CJ  CL  CM��CP  CR  CT
=CV
=CX  CY��C[��C]��C`  Cb  Cd  Cf  Ch  Cj
=Cl  Cn  Cp
=Cr
=Ct{Cv
=Cx  Cy�C{��C~  C�C�  C�  C�C�  C�  C���C�  C�  C�  C�  C���C���C�
=C�C���C�  C�C�  C���C�C���C���C���C�  C�  C�  C���C���C���C�  C�C�  C���C�  C�  C���C���C�  C���C�  C���C���C���C���C�  C�  C�  C�C�C�  C�  C�
=C�C���C�  C�C�
=C�C�  C�C�C�C�C�C�  C���C�  C�  C�  C�C�C�  C�  C�C���C���C�  C���C�  C�C�C���C���C���C�  C�  C���C���C���C�  C�C�  C���C���C�  C�C���C���C�C�C���C�C�
=C�
=C�
=C�C�
=C�C���C�  C�  C�  C�C�C�C�C�C�  C���C���C���C�  C�  C�  C�  C�
=C�D   D ��DD� D�qD}qD�qD� D�qD� D  D�D�D}qD  D}qD�D��D	  D	}qD	�qD
� D  D��D  D}qD�D� D  D}qD  D� D  D� D�qD� D�qDz�D�RD� D�D}qD  D�D�D� D  D� D  D}qD��Dz�D  D�D�D�D�D� D�qD}qD�qD}qD�qD}qD   D ��D!�D!� D!�qD"}qD#�D#�D$  D$}qD$�qD%}qD%�qD&��D'�D'��D(D(��D)�D)��D*�D*��D+  D+}qD+�qD,}qD-  D-}qD.  D.� D/  D/��D0�D0��D1  D1}qD2  D2� D3  D3� D4  D4� D5D5�D5�qD6}qD6�qD7� D8  D8}qD9  D9� D9�qD:}qD;  D;� D<  D<��D=D=� D=�qD>� D?  D?��D@D@��D@�qDA� DA�qDB� DC�DC}qDC�qDD}qDE  DE}qDE��DFz�DF�qDG��DHDH� DH�qDI}qDJ�DJ� DK  DK}qDL  DL��DMDM��DN  DN� DO  DO��DO�qDP}qDQ�DQ� DR  DR� DS  DS� DT  DT� DU  DU��DV�DV� DV�qDW}qDX�DX� DY�DY��DY��DZ}qD[  D[� D[�qD\}qD\�qD]� D^  D^�D_D_�D`�D`��D`�qDaxRDb  Db� Db��Dc� Dd  Dd��De  De� DfDf��Df�qDg� Dh�Dh}qDi  Di� Di�qDj��Dk  Dk}qDl�Dl� Dl�qDm��Dn�Dn}qDo  Do�Dp�Dp� Dq�Dq� Dq�qDrz�Dr�RDs� DtDt� Dt�qDu� Dv  Dv}qDv�qDw� Dx�Dx� Dy  Dy��Dz  Dz� D{  D{� D|�D|��D}  D}� D~�D~��D  D� D�qD�@ D���D��HD���D�@ D�� D�� D�HD�AHD���D��HD�  D�>�D�~�D���D�  D�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D���D���D�=qD�� D��HD�  D�@ D�� D�� D�HD�AHD�~�D���D�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?�?W
=?��?�Q�?�G�@   @�R@5@B�\@^�R@u@��\@���@��R@��@���@�  @���@�@�  @�=q@�Q�A�\A
=Ap�A33AQ�A�RA%�A)��A0  A7
=A;�AAG�AG�AL(�AQG�AXQ�A\��Aa�Ah��An{As�
A{�A�  A��\A�{A���A��HA�ffA���A��
A�
=A�=qA�(�A��A��HA���A�  A�33A��A�  A��A�A�Q�A��
A�A���A�(�A�ffA�G�A�z�AθRA�G�A���A�
=A��A�p�A�  A�\A�{A��A�33A�RA���A�A�
=A�=qA�(�A�
=BG�B�\B�Bp�B�HB(�B	��B\)Bz�B{B�
B��BffB(�Bp�B�RB��B�B
=B��B{B\)B!G�B"{B#�B%G�B&ffB'�B)p�B*�\B+�
B-p�B.�HB0  B1B2�RB4(�B5B7
=B8(�B9p�B;33B<��B=��B?
=B@��BA�BB�HBDz�BF{BG33BHz�BJ=qBK33BL��BN=qBO\)BP��BR=qBS\)BT��BV=qBW�
BX��BZ=qB[�
B]�B^{B_�BaG�BbffBc�Bd��BfffBg�Bh��Bj=qBk�Bl��Bn=qBo�Bp��Br{Bs�Bt��Bu�Bw
=Bx��By�Bz�RB|��B}�B~�HB�(�B��HB�\)B�(�B��HB�\)B�{B��HB�\)B�  B���B�p�B��B��\B�p�B��B�z�B�G�B��
B�Q�B��B�B�=qB��HB��B�=qB��RB��B�(�B���B�\)B�(�B��\B��B��B��\B���B��B�z�B��HB��B�Q�B���B�p�B�=qB��RB�G�B�{B���B��B��B��\B�
=B���B�Q�B���B�\)B�{B���B�\)B��
B��\B�G�B�B�=qB��B��B�(�B��HB���B�  B��RB�p�B��B�ffB��B��B�(�B���B���B�(�B���B�\)B�{B���B��B��B���B�
=B��B�z�B�
=B��B�Q�B�
=B��B�(�B���B��B�  B��HB�p�B��B���B�\)B��B\B�\)B�  B�z�B�G�B�  B�z�B��B�  Bȏ\B��B��Bʏ\B�
=B�B̏\B��BͮB�z�B�G�B�B�Q�B��B��B�ffB��B��
B�z�B���B��
B֏\B��B׮B؏\B��BٮB�z�B�G�B�B�ffB�33B�  B�z�B�33B�  B��\B��B�  B�RB�33B�{B��HB�p�B�{B���B癚B�=qB�
=B��
B�\B��B��
B�RB�p�B�  B���B�B�Q�B���B�B��B�33B��B��HB��B�(�B�
=B��B�Q�B��B�  B��\B�33B�{B���B�\)B�{B���B��C �C z�C �C=qC��C  Cp�C�RC{Cz�C�CG�C��C��CffC��C�Cp�C�CQ�C��C��Cp�CC	�C	�\C	�HC
=qC
��C
=Cp�CC�Cz�C�CG�C��C��CffCC
=Cz�C�C33C�C  C\)C��C{C�C�
C33C�C  CQ�C�RC(�Cz�C��CG�C�C��CQ�CC(�Cp�C�HCG�C�C
=C\)C�C�C�\C�
C33C��C
=C\)C�C�C�C��C33C��C��C G�C �C!{C!ffC!�RC"�C"z�C"�HC#�C#�\C#��C$\)C$�C$��C%ffC%��C&�C&p�C&��C'33C'��C'��C(=qC(�C){C)\)C)�C*{C*z�C*�
C+�C+p�C+�HC,=qC,�C,��C-33C-�\C-�HC.�C.z�C.�HC/(�C/p�C/��C033C0�C0C1{C1z�C1��C2{C2\)C2C3{C3Q�C3��C4  C4Q�C4�C4��C5(�C5z�C5�RC5��C6G�C6��C6��C7{C7\)C7��C7��C8
=C8\)C8�\C8�C8��C9�C9=qC9p�C9�C9�
C9�C:{C:G�C:\)C:p�C:��C:��C:�C:��C;�C;G�C;Q�C;z�C;�C;�C;��C<  C<{C<�C<Q�C<ffC<p�C<��C<C<C<�
C=
=C=�C=(�C=Q�C=p�C=z�C=��C=��C=�C=��C>
=C>=qC>\)C>\)C>�C>�C>��C>�
C?
=C?33C?=qC?Q�C?�C?�C?��C?�
C@  C@(�C@33C@\)C@�\C@��C@C@��CA  CA�CAQ�CAz�CA��CA�CA��CB  CB�CB33CBffCB�\CB��CB�RCB�CC{CC(�CCG�CCp�CC��CC��CC�CD  CD(�CDQ�CD�CD��CD��CE  CE�CE=qCE\)CE�\CECE�
CF  CF33CFp�CF��CF��CF�HCG{CG=qCG�CG��CGCG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�?}A�E�A�?}A�M�A�S�A�S�A�O�A�M�A�M�A�K�A�G�A�K�A�O�A�S�A�S�A�VA�VA�XA�VA�ZA�^5A�\)A�ZA�M�A�M�A�K�A�M�A�K�A�M�A�M�A�I�A�G�A�?}A�+A�+A�+A�-A�-A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�-A�-A�+A�+A�+A�+A�(�A�(�A�(�A�+A�(�A�+A�(�A�&�A�(�A�&�A�&�A�&�A�"�A��A�%A���A��/A���A˺^AˍPA�p�A�l�A�l�A�l�A�l�A�l�A�jA�hsA�dZA�\)A�VA�K�A�C�A�-A��A���A���AʓuA�Aȝ�A��;A�VA�v�A���A���A���A��uA��-A���A���A���A�O�A���A���A�S�A�~�A�&�A�9XA�5?A���A��wA�ƨA��DA�ĜA�ĜA�z�A�ĜA��!A��A�&�A�9XA���A�z�A���A��
A��A�\)A{�Aux�Aq%An-Ah�AfjAd�!AaA[��AX��AW
=AU�AT��AT5?ARĜAR�AQ/AP�yAPbAM��AJ�AGhsAE�AC/AA/A>~�A< �A:-A8JA3��A1�wA/l�A-?}A,�uA+�A)G�A%�A$�A$z�A#�-A"��A!&�AhsA33A�yA�!A��A��A"�AO�A��A�jA�+Ap�A"�A�A�!Ax�AXA�#A��A�A�HA�A�A1'A\)A��A��A
5?A��AĜA�A	
=A
�`A\)A`BA
�HA
Q�A
VA
ZA
-A	ƨA	�^A	|�AA�Ax�AK�A%A�9AbNA�A+AĜA{A7LA�HA��AQ�A�Ax�A7LAVA �9A   @�33@��\@��@���@��@�C�@�@���@�~�@��@��@�X@�&�@���@���@�@�I�@�
=@��T@��@��@�1'@��m@���@�b@��m@�"�@���@�x�@�X@���@�@��
@�C�@��@�M�@�J@���@�?}@�@�\)@�V@�G�@�z�@�;d@��@�7L@��/@�Q�@��@�S�@ް!@��@�V@�Ĝ@�Q�@�33@�^5@�@�/@�  @�\)@֟�@�$�@���@�1'@�+@�E�@с@�hs@��@д9@��@�J@̼j@�A�@�(�@��;@�|�@���@�n�@���@�x�@�V@ȓu@��
@��@�v�@��T@�&�@��@Ĭ@Å@�
=@¸R@��@���@�x�@���@�z�@�(�@���@��;@���@�K�@���@��@��T@�hs@��`@�z�@�I�@� �@�+@���@���@�5?@��@�A�@�b@�b@���@�ȴ@�V@�?}@���@��@�C�@�n�@��T@���@��@�7L@���@�r�@�(�@�  @��
@���@�dZ@��@��@���@�n�@�V@�{@���@�x�@��j@��@��@��@�z�@�j@��F@�;d@��H@���@�M�@�$�@���@�X@�&�@�&�@�V@���@��/@��j@�z�@�b@��w@���@��P@��@�l�@�@���@�J@���@�X@�7L@��@���@�j@��@��@��;@��w@�t�@�K�@�
=@��@��!@�^5@�{@��@���@���@�Q�@��w@�|�@�+@�
=@��@���@�V@��@��^@�p�@�/@���@�Ĝ@��D@�1'@���@�dZ@�"�@��H@���@���@�M�@��@�G�@�Q�@�1@��
@���@�l�@�33@���@�~�@�-@��^@���@�p�@��@��/@�Q�@�1@��;@��w@��@�o@��!@�E�@�@��-@�?}@�%@�Ĝ@�z�@�9X@�b@��;@��@���@���@��@�"�@�ȴ@���@���@�v�@�M�@�=q@��@��7@�X@�?}@�/@�%@��`@���@��u@�z�@�  @��@�S�@��!@�~�@�ff@�M�@�=q@��@��^@��^@���@�`B@�X@��@��`@�j@�  @���@��@��;@��
@��w@���@��P@�t�@�@���@�=q@�@��#@���@�hs@�`B@�?}@���@�z�@�9X@�(�@���@��@�C�@��@��!@�5?@�@���@��@�?}@���@�r�@�9XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�?}A�?}A�?}A�9XA�E�A�A�A�A�A�O�A�?}A�;dA�A�A�M�A�Q�A�Q�A�VA�Q�A�Q�A�XA�Q�A�Q�A�O�A�M�A�M�A�O�A�M�A�K�A�O�A�I�A�I�A�K�A�G�A�K�A�K�A�I�A�K�A�Q�A�K�A�K�A�Q�A�M�A�M�A�VA�Q�A�Q�A�VA�S�A�Q�A�XA�S�A�Q�A�S�A�XA�Q�A�VA�VA�Q�A�VA�XA�S�A�XA�ZA�S�A�VA�ZA�S�A�VA�ZA�VA�VA�ZA�VA�VA�ZA�VA�Q�A�XA�S�A�XA�^5A�\)A�XA�^5A�ZA�ZA�`BA�^5A�\)A�`BA�`BA�\)A�ZA�^5A�\)A�ZA�^5A�`BA�ZA�XA�ZA�S�A�Q�A�M�A�K�A�I�A�O�A�K�A�K�A�O�A�M�A�M�A�O�A�K�A�K�A�O�A�I�A�K�A�O�A�M�A�K�A�O�A�K�A�I�A�K�A�I�A�I�A�O�A�K�A�K�A�O�A�O�A�M�A�K�A�O�A�O�A�K�A�K�A�O�A�O�A�K�A�O�A�O�A�I�A�E�A�G�A�E�A�E�A�I�A�C�A�E�A�G�A�E�A�C�A�E�A�?}A�?}A�5?A�33A�+A�(�A�+A�/A�+A�(�A�+A�-A�+A�(�A�-A�-A�(�A�-A�-A�(�A�+A�/A�-A�+A�(�A�/A�/A�+A�/A�/A�+A�-A�-A�(�A�-A�-A�(�A�+A�-A�(�A�(�A�-A�+A�(�A�(�A�-A�(�A�(�A�+A�+A�(�A�-A�-A�(�A�+A�-A�-A�(�A�-A�-A�(�A�+A�-A�(�A�&�A�+A�+A�&�A�+A�-A�&�A�(�A�-A�(�A�+A�-A�(�A�(�A�-A�+A�&�A�+A�-A�(�A�&�A�-A�-A�(�A�+A�-A�-A�(�A�+A�/A�(�A�(�A�/A�+A�(�A�/A�-A�(�A�+A�/A�-A�(�A�/A�-A�(�A�-A�/A�+A�(�A�+A�/A�-A�(�A�+A�-A�(�A�(�A�-A�(�A�&�A�+A�-A�(�A�(�A�-A�+A�(�A�-A�+A�&�A�(�A�+A�&�A�&�A�-A�(�A�&�A�+A�+A�(�A�(�A�-A�+A�&�A�+A�-A�&�A�(�A�-A�&�A�&�A�+A�-A�(�A�&�A�+A�-A�&�A�(�A�-A�(�A�&�A�+A�-A�+A�(�A�-A�+A�&�A�(�A�+A�&�A�$�A�+A�+A�$�A�&�A�+A�&�A�$�A�+A�(�A�$�A�(�A�+A�(�A�$�A�+A�&�A�$�A�&�A�(�A�&�A�$�A�$�A�(�A�(�A�$�A�$�A�(�A�&�A�$�A�&�A�(�A�"�A� �A�"�A��A��A��A��A�oA�bA�bA�
=A�%A�1A�%A�A�  A�A�A���A��A��A��TA��;A��/A��#A��
A���A���A���A���A�ƨA�ĜA�ƨA�A˺^AˮA˧�Aˡ�A˓uAˇ+A˅A�~�A�z�A�t�A�p�A�l�A�jA�n�A�n�A�l�A�jA�jA�n�A�n�A�jA�jA�n�A�n�A�jA�jA�n�A�l�A�jA�n�A�l�A�hsA�n�A�n�A�jA�jA�n�A�l�A�jA�l�A�n�A�l�A�hsA�l�A�l�A�hsA�hsA�jA�jA�ffA�hsA�hsA�hsA�ffA�bNA�^5A�`BA�`BA�\)A�ZA�ZA�\)A�XA�VA�VA�VA�O�A�O�A�O�A�I�A�E�A�G�A�K�A�I�A�C�A�A�A�?}A�;dA�5?A�1'A�+A�+A�(�A�"�A� �A� �A��A�VA�1A�%A�1A���A���A��A��A��;A���A�A�ĜA�AʾwAʺ^AʮAʡ�AʅA�G�A�7LA��mA���Aɴ9AɑhA�dZA�E�A�-A�%AȲ-A�=qAǉ7A��`A�ffA���Aţ�Aŉ7A�C�A�Ać+A�-A��A��A���AÙ�A� �A���A�oA�I�A�{A��A��A��#A��A�jA�XA�E�A��A��PA���A���A�XA�&�A�bA��A�ĜA��DA�z�A�x�A�^5A�S�A�M�A�1'A��A�{A�{A��A���A�ȴA��FA��!A��A��-A���A���A���A��+A�l�A�bNA�ZA�M�A�A�A�33A�"�A��A�bA��A�n�A�A�ƨA�XA�33A�%A���A��^A���A���A��uA�x�A�-A�|�A��A���A���A��PA�dZA�?}A��A��A���A�z�A�7LA���A���A���A���A��FA��\A�XA�E�A�$�A�JA���A��mA��A�ƨA��-A��A���A���A���A��hA�bNA��A���A���A�r�A��A���A��hA�;dA��
A��A�S�A�=qA��A��TA���A��A�=qA�&�A�A�ƨA�z�A�Q�A���A��PA�=qA��A���A��wA�~�A�hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A�E�A�?}A�M�A�S�A�S�A�O�A�M�A�M�A�K�A�G�A�K�A�O�A�S�A�S�A�VA�VA�XA�VA�ZA�^5A�\)A�ZA�M�A�M�A�K�A�M�A�K�A�M�A�M�A�I�A�G�A�?}A�+A�+A�+A�-A�-A�+A�+A�+A�+A�+A�+A�+A�+A�+A�+A�-A�-A�+A�+A�+A�+A�(�A�(�A�(�A�+A�(�A�+A�(�A�&�A�(�A�&�A�&�A�&�A�"�A��A�%A���A��/A���A˺^AˍPA�p�A�l�A�l�A�l�A�l�A�l�A�jA�hsA�dZA�\)A�VA�K�A�C�A�-A��A���A���AʓuA�Aȝ�A��;A�VA�v�A���A���A���A��uA��-A���A���A���A�O�A���A���A�S�A�~�A�&�A�9XA�5?A���A��wA�ƨA��DA�ĜA�ĜA�z�A�ĜA��!A��A�&�A�9XA���A�z�A���A��
A��A�\)A{�Aux�Aq%An-Ah�AfjAd�!AaA[��AX��AW
=AU�AT��AT5?ARĜAR�AQ/AP�yAPbAM��AJ�AGhsAE�AC/AA/A>~�A< �A:-A8JA3��A1�wA/l�A-?}A,�uA+�A)G�A%�A$�A$z�A#�-A"��A!&�AhsA33A�yA�!A��A��A"�AO�A��A�jA�+Ap�A"�A�A�!Ax�AXA�#A��A�A�HA�A�A1'A\)A��A��A
5?A��AĜA�A	
=A
�`A\)A`BA
�HA
Q�A
VA
ZA
-A	ƨA	�^A	|�AA�Ax�AK�A%A�9AbNA�A+AĜA{A7LA�HA��AQ�A�Ax�A7LAVA �9A   @�33@��\@��@���@��@�C�@�@���@�~�@��@��@�X@�&�@���@���@�@�I�@�
=@��T@��@��@�1'@��m@���@�b@��m@�"�@���@�x�@�X@���@�@��
@�C�@��@�M�@�J@���@�?}@�@�\)@�V@�G�@�z�@�;d@��@�7L@��/@�Q�@��@�S�@ް!@��@�V@�Ĝ@�Q�@�33@�^5@�@�/@�  @�\)@֟�@�$�@���@�1'@�+@�E�@с@�hs@��@д9@��@�J@̼j@�A�@�(�@��;@�|�@���@�n�@���@�x�@�V@ȓu@��
@��@�v�@��T@�&�@��@Ĭ@Å@�
=@¸R@��@���@�x�@���@�z�@�(�@���@��;@���@�K�@���@��@��T@�hs@��`@�z�@�I�@� �@�+@���@���@�5?@��@�A�@�b@�b@���@�ȴ@�V@�?}@���@��@�C�@�n�@��T@���@��@�7L@���@�r�@�(�@�  @��
@���@�dZ@��@��@���@�n�@�V@�{@���@�x�@��j@��@��@��@�z�@�j@��F@�;d@��H@���@�M�@�$�@���@�X@�&�@�&�@�V@���@��/@��j@�z�@�b@��w@���@��P@��@�l�@�@���@�J@���@�X@�7L@��@���@�j@��@��@��;@��w@�t�@�K�@�
=@��@��!@�^5@�{@��@���@���@�Q�@��w@�|�@�+@�
=@��@���@�V@��@��^@�p�@�/@���@�Ĝ@��D@�1'@���@�dZ@�"�@��H@���@���@�M�@��@�G�@�Q�@�1@��
@���@�l�@�33@���@�~�@�-@��^@���@�p�@��@��/@�Q�@�1@��;@��w@��@�o@��!@�E�@�@��-@�?}@�%@�Ĝ@�z�@�9X@�b@��;@��@���@���@��@�"�@�ȴ@���@���@�v�@�M�@�=q@��@��7@�X@�?}@�/@�%@��`@���@��u@�z�@�  @��@�S�@��!@�~�@�ff@�M�@�=q@��@��^@��^@���@�`B@�X@��@��`@�j@�  @���@��@��;@��
@��w@���@��P@�t�@�@���@�=q@�@��#@���@�hs@�`B@�?}@���@�z�@�9X@�(�@���@��@�C�@��@��!@�5?@�@���@��@�?}@���@�r�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�?}A�?}A�?}A�9XA�E�A�A�A�A�A�O�A�?}A�;dA�A�A�M�A�Q�A�Q�A�VA�Q�A�Q�A�XA�Q�A�Q�A�O�A�M�A�M�A�O�A�M�A�K�A�O�A�I�A�I�A�K�A�G�A�K�A�K�A�I�A�K�A�Q�A�K�A�K�A�Q�A�M�A�M�A�VA�Q�A�Q�A�VA�S�A�Q�A�XA�S�A�Q�A�S�A�XA�Q�A�VA�VA�Q�A�VA�XA�S�A�XA�ZA�S�A�VA�ZA�S�A�VA�ZA�VA�VA�ZA�VA�VA�ZA�VA�Q�A�XA�S�A�XA�^5A�\)A�XA�^5A�ZA�ZA�`BA�^5A�\)A�`BA�`BA�\)A�ZA�^5A�\)A�ZA�^5A�`BA�ZA�XA�ZA�S�A�Q�A�M�A�K�A�I�A�O�A�K�A�K�A�O�A�M�A�M�A�O�A�K�A�K�A�O�A�I�A�K�A�O�A�M�A�K�A�O�A�K�A�I�A�K�A�I�A�I�A�O�A�K�A�K�A�O�A�O�A�M�A�K�A�O�A�O�A�K�A�K�A�O�A�O�A�K�A�O�A�O�A�I�A�E�A�G�A�E�A�E�A�I�A�C�A�E�A�G�A�E�A�C�A�E�A�?}A�?}A�5?A�33A�+A�(�A�+A�/A�+A�(�A�+A�-A�+A�(�A�-A�-A�(�A�-A�-A�(�A�+A�/A�-A�+A�(�A�/A�/A�+A�/A�/A�+A�-A�-A�(�A�-A�-A�(�A�+A�-A�(�A�(�A�-A�+A�(�A�(�A�-A�(�A�(�A�+A�+A�(�A�-A�-A�(�A�+A�-A�-A�(�A�-A�-A�(�A�+A�-A�(�A�&�A�+A�+A�&�A�+A�-A�&�A�(�A�-A�(�A�+A�-A�(�A�(�A�-A�+A�&�A�+A�-A�(�A�&�A�-A�-A�(�A�+A�-A�-A�(�A�+A�/A�(�A�(�A�/A�+A�(�A�/A�-A�(�A�+A�/A�-A�(�A�/A�-A�(�A�-A�/A�+A�(�A�+A�/A�-A�(�A�+A�-A�(�A�(�A�-A�(�A�&�A�+A�-A�(�A�(�A�-A�+A�(�A�-A�+A�&�A�(�A�+A�&�A�&�A�-A�(�A�&�A�+A�+A�(�A�(�A�-A�+A�&�A�+A�-A�&�A�(�A�-A�&�A�&�A�+A�-A�(�A�&�A�+A�-A�&�A�(�A�-A�(�A�&�A�+A�-A�+A�(�A�-A�+A�&�A�(�A�+A�&�A�$�A�+A�+A�$�A�&�A�+A�&�A�$�A�+A�(�A�$�A�(�A�+A�(�A�$�A�+A�&�A�$�A�&�A�(�A�&�A�$�A�$�A�(�A�(�A�$�A�$�A�(�A�&�A�$�A�&�A�(�A�"�A� �A�"�A��A��A��A��A�oA�bA�bA�
=A�%A�1A�%A�A�  A�A�A���A��A��A��TA��;A��/A��#A��
A���A���A���A���A�ƨA�ĜA�ƨA�A˺^AˮA˧�Aˡ�A˓uAˇ+A˅A�~�A�z�A�t�A�p�A�l�A�jA�n�A�n�A�l�A�jA�jA�n�A�n�A�jA�jA�n�A�n�A�jA�jA�n�A�l�A�jA�n�A�l�A�hsA�n�A�n�A�jA�jA�n�A�l�A�jA�l�A�n�A�l�A�hsA�l�A�l�A�hsA�hsA�jA�jA�ffA�hsA�hsA�hsA�ffA�bNA�^5A�`BA�`BA�\)A�ZA�ZA�\)A�XA�VA�VA�VA�O�A�O�A�O�A�I�A�E�A�G�A�K�A�I�A�C�A�A�A�?}A�;dA�5?A�1'A�+A�+A�(�A�"�A� �A� �A��A�VA�1A�%A�1A���A���A��A��A��;A���A�A�ĜA�AʾwAʺ^AʮAʡ�AʅA�G�A�7LA��mA���Aɴ9AɑhA�dZA�E�A�-A�%AȲ-A�=qAǉ7A��`A�ffA���Aţ�Aŉ7A�C�A�Ać+A�-A��A��A���AÙ�A� �A���A�oA�I�A�{A��A��A��#A��A�jA�XA�E�A��A��PA���A���A�XA�&�A�bA��A�ĜA��DA�z�A�x�A�^5A�S�A�M�A�1'A��A�{A�{A��A���A�ȴA��FA��!A��A��-A���A���A���A��+A�l�A�bNA�ZA�M�A�A�A�33A�"�A��A�bA��A�n�A�A�ƨA�XA�33A�%A���A��^A���A���A��uA�x�A�-A�|�A��A���A���A��PA�dZA�?}A��A��A���A�z�A�7LA���A���A���A���A��FA��\A�XA�E�A�$�A�JA���A��mA��A�ƨA��-A��A���A���A���A��hA�bNA��A���A���A�r�A��A���A��hA�;dA��
A��A�S�A�=qA��A��TA���A��A�=qA�&�A�A�ƨA�z�A�Q�A���A��PA�=qA��A���A��wA�~�A�hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                           111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��BPBPBVBB�BBPB�BPB�B"B�BB�BB�B�B�B�B�BPBB�B�BJBJBBBBJBB�BJB�BxB�BxBxBxBDBxBxBxBDBDBDBDBxBxB�B�BxBBxB�BxBxBxB�BDBDBxBDBB
�BB
=B	lB�B�BB �B��B�B�8B��B�fB��B�fB�fB�2B��B�fB�fB�fB�fB��B�2B��B�%B�B�B�fB�VB+B"B:B1B!�B$B-�B=�B>wBC�BJ#BD3BE�B<6B6�B2�B-CB,qB!�B!B�BJB�B�JB�B��B�B��B��B��B�;BiyBM�BkB
�NB
�:B
��B
K)B
!�B
�B	��B	��B	��B	��B	��B	��B	rGB	iDB	bB	\�B	YKB	N<B	JXB	E�B	@�B	=�B	5B	&�B		B	~B	�B		lB	�B	�B	
=B	
�B	�B�VB��B��B�	B��B�>B�	B��B�B�B�|B��B��B	�B	�B	eB	!bB	5tB	FB	W
B	Y�B	iDB	M�B	C�B	IB	T,B	[�B	XEB	W?B	cTB	c�B	\�B	PHB	;�B	/�B	*�B	$@B	~B	�B	 \B	 �B	$@B	'�B	?}B	��B	��B	�VB	��B	��B	��B	��B	��B	�B	��B	�nB	�B	�B	��B	�B	�?B	�nB	��B	�$B	��B	�^B	��B	��B	��B	�jB	��B	��B	��B	�B	�UB	�9B	�mB	�?B	�EB	�EB	˒B	̘B	̘B	̘B	�6B	ϫB	ҽB	��B	چB	��B	�B	�&B	�B	��B	�B	�}B	уB	҉B	�B	��B	�QB	ܒB	��B	�)B	�#B	��B	�jB	��B	ߤB	�B	�pB	�B	�B	�B	��B	ߤB	�vB	ߤB	ݘB	�QB	ٴB	�5B	�pB	ߤB	�|B	�B	��B	��B	�|B	�B	��B	�TB	�B	�B	�B	�
B	�>B	�sB	��B	�B	�QB	�DB	�KB	��B	��B	�DB	�B	��B	�B	�B	�]B	��B	�]B	�]B	�B	�;B	�GB	�GB	��B	�B	�B	�B	�MB	�TB	��B	�ZB	�`B	�DB	��B	��B	�DB	�B	�>B	�JB	�PB	�VB	�(B	�cB
 4B
B
B
B
�B
;B
�B
B
�B
�B
�B
_B
	lB
DB
�B
VB
�B
�B
�B
�B
"B
"B
"B
B
B
�B
PB
PB
�B
B
�B
�B
VB
(B
�B
�B
�B
:B
B
@B
@B
uB
�B
B
�B
B
�B
+B
�B
�B
�B
�B
7B
�B
qB
xB
B
B
�B
~B
�B
�B
VB
VB
VB
VB
VB
 �B
 \B
 'B
 \B
 'B
 �B
 �B
 �B
!-B
"4B
"�B
#B
#nB
$tB
$�B
%zB
%�B
%zB
&B
&LB
&LB
&�B
&�B
'B
&�B
'RB
'�B
(�B
(XB
)_B
*0B
*�B
+�B
+�B
,B
,�B
.IB
.}B
/B
/OB
/�B
/�B
/B
/�B
0�B
1�B
2-B
1�B
2aB
2-B
2-B
2�B
2aB
33B
2�B
2-B
2aB
2-B
2aB
2�B
2�B
3�B
3�B
3�B
3hB
3hB
4nB
5?B
6FB
7B
8�B
8RB
7�B
9$B
9�B
9�B
:�B
;dB
;�B
<6B
<jB
<jB
=<B
=�B
>BB
>wB
>BB
>B
>wB
?�B
?�B
?}B
?HB
?�B
?�B
?}B
@B
?�B
@B
?�B
?�B
@B
@OB
A B
AUB
A B
B[B
A�B
B[B
B[B
B'B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D3B
D3B
EB
E9B
E9B
E�B
E�B
E�B
E�B
E�B
E�B
EmB
FB
FB
F�B
F�B
FtB
F�B
F�B
F�B
FtB
FtB
F�B
GzB
GzB
HKB
HKB
H�B
IB
IRB
IB
H�B
I�B
I�B
JXB
K^B
MB
M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B
	B�B�B�B"B�BPB	7B�BBxB�B�BxBVB�BPB"B�BB�B"B�B�B"B�B"B�B�B"BVBJB�BVB~BPB"B�BPB�B�B�B�B�B�BVB�B�B�B~B~B"B�B�B�BBxB"BBB�BPB�BPBB�B�B�BxBPB�BB�B�BDB~BVBJB�B�BJB�B�BJB~B�B�BJB�B"B�BB�BPBxB�BPB
�B�B�BDBJB~BBB�B�B�B�BDB�BJBB�BPB
�B�B�B~B�BB
�B�B�BBDB�BPBxB
�BPB�BDB�B�BBDB�BJBB~BBB�BB
�B�B�B�BBB"BxB�B~BDB
rBB�BxB
�BxB~BBxB�BDBB�B�B
rBDBB�B
=BDB�B
�B
�B~BBB~BxB
rBJBB
�BBB
rB
�BJBB
rBxBJB
�BxBJB
�B
rBJB�B
rBB~B
�B
rBJBxB
=BBB
=BBBB
rBJBxB
=BJBxB
=B�BJB
=B
�BJBB
=B�BJB
�B
=BJBxB
rB
rB�BxB
=BJB~B
rBDB~B
rB
�BJB�B
rBxB�B
�BxB�BDB
�B�B�BxB
rBDB�B�B
�BJB�B
=B�BJB
�B
=BJB�B
=BDB~B
�B
�B~B�B
rB�BJB
=B�B�B
�B
�BBB
=BDBJBB
�B~B�B
rBJB�BDB
=B�B~BDB
�B~B�B
�BxB~B
�B
rBDBJB
�BxB~BB
�B�B~B
�B
�B�BxB
	BDBJB
rBDB~BB
=BDBJB
	BBJBB	�B
rB�B�B
	B
=B�BxB	�B
rB�BB	lBxB�B	B
rB
�B	B	�B
�B
rB�B�B	B�BYB�B�B�BB�B�BB{BBB iBuB{B;B�.B��B �B 4B��B��B�.B �B��B��B�VB��B�rB�>B�rB�B��B�8B�lB�2B��B��B�8B�8B��B�2B�B�8B��B��B�8B�B�`B��B�B��B�2B��B��B�+B�B�B�`B��B�lB�fB��B��B��B��B��B�B�8B��B��B��B��B��B�`B��B�8B��B�2B�+B��B��B��B��B�fB�8B��B��B�8B��B��B�2B��B�fB��B��B��B��B��B�`B��B��B��B��B�`B��B�2B�%B�2B��B��B��B��B�ZB��B��B�B��B�8B�B��B�GB�B�MB�B�B��B��B�ZB�B��B�fB�`B�fB�B�+B��B��BB1B�B�B
�BYBGB�BJB:B�B�B�B�B�B�B�B iB7LB�B~BB�B4B�B�B�BSBOB%FB�B($B!B�B$tBVB)�B �B!�B!-B!B�B�B&�B#B"hB'�B)�B$�B&�B#�B#:B!bB"hB"hB#�B$tB&�B#�B"�B$B%B$B!�B"�B!-B$�B8RB+�B33B6zB-CB5�B1�B.}B/�B-CB,B.B9$BK�B8RB6FB9XB=B8RB7�B:�B?�B8�B:�BA�BK�BPHB<B:^B@�BC�B=qB=�B?HB?HB?�B>BB>BB?HB?HB;�B<B<B:�B:�BA�BC�BB[B?}BA BG�BE9BA�BNpBF?BM�BC�BA�BD3BH�BA�BJ�BK�BFtBIRBR�BK�BIBO�BR�BHBD�BF?BB[BG�BA�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                           444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                           444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022031617093920220316170939IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022031722313920220317223139QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022031722313920220317223139QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194420230210131944IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                